open Batteries;;
open Lwt;;

module OCR = GCloudTextRecognition
module Parse = GCloudNaturalLanguageSyntax

module type DICTIONARY = sig
  type t
  type page

  val perform : string -> t Lwt.t
  val pages : t -> page list
  val name : page -> string
  val reading : page -> string
  val rendering : page -> string
end

module Dictionary : DICTIONARY = JishoDictionary

module PD = PersonalDictionary

type ocrdata = string
type segmentdata = Parse.Segment.sentence list
type id = int

type entry_state =
  { id : id
  ; filename : string
  ; filepath : string
  ; created : float
  ; imgdata : string Lwt.t
  ; ocrdata : ocrdata Lwt.t
  ; bufferdata : string ref
  ; segmentdata : segmentdata option ref
  }

module Dict = BatSplay.Map(struct
  type t = int
  let compare = Int.compare
end)

type ui_state =
{ entries : entry_state Dict.t ref
; dictionary : PD.t option ref
}

let state =
  { entries = ref Dict.empty
  ; dictionary = ref @@ None
  }

let id =
  let counter = ref 0 in
  (fun () -> counter := !counter + 1 ; !counter)

let find_entry id = Dict.find id !(state.entries)

let load_directory dir =
  let files = BatSys.readdir dir
    |> BatArray.to_list
    |> BatList.filter (fun s -> List.exists (BatString.ends_with s)
      [ ".jpg"
      ; ".jpeg"
      ; ".bmp"
      ; ".gif"
      ; ".png"
      ]) in
  let entries = List.map (fun f ->
    let filepath = (String.concat (Filename.dir_sep) [dir ; f]) in
    let created = (BatUnix.stat filepath).st_mtime in
    let imgdata =
      try%lwt
        Lwt_io.with_file ~mode:Lwt_io.Input filepath Lwt_io.read
      with | e -> Log.log_trace e `error (String.concat "" ["Error loading file: " ; filepath]) ; raise e
    in
    let bufferdata = ref "" in
    let ocrdata =
      try%lwt
        let%lwt imgdata = imgdata in
        let%lwt result = OCR.perform (CurlLwtGCloud.APIKey !(Preferences.gcloud_apikey)) imgdata in
        let result = String.trim result in
        bufferdata := result;
        return result
      with | e -> Log.log_trace e `error "Error during OCR fetch"; raise e
    in
    { id = id ()
    ; filename = f
    ; filepath
    ; created
    ; imgdata = imgdata
    ; ocrdata = ocrdata
    ; bufferdata = bufferdata
    ; segmentdata = ref None
    }
  ) files in
  let entries = List.fast_sort (fun a b -> BatFloat.compare a.created b.created) entries in
  state.entries := Dict.of_list @@ List.map (fun e -> (e.id, e)) entries;
  entries

let fetch_ocr (entry : entry_state) = Lwt_main.run entry.ocrdata

let set_buffer (entry : entry_state) s =
  entry.bufferdata := s;
  entry.segmentdata := None

type segment = Parse.Segment.t
let fetch_segment (entry : entry_state) =
  match !(entry.segmentdata) with
  | None ->
    let result = Lwt_main.run @@
      try%lwt
        Parse.perform (CurlLwtGCloud.APIKey !(Preferences.gcloud_apikey)) !(entry.bufferdata)
      with | e -> Log.log_trace e `error "Error during natural language segmentation"; raise e
    in
    let result = List.map Parse.Segment.clean_segment_space result in
    entry.segmentdata := Some result;
    result
  | Some result -> result

let dict_lookup text =
  let open Dictionary in
  let result = Lwt_main.run @@
    try%lwt
      perform text
    with | e -> Log.log_trace e `error
      (String.concat "" ["Error during dictionary lookup for: " ; text]); raise e
  in
  pages result

let external_dictionaries page =
  let name = Dictionary.name page |> Curl.escape in
  let reading = Dictionary.reading page |> Curl.escape in
  [ "OJAD", String.concat "" ["http://www.gavo.t.u-tokyo.ac.jp/ojad/search/index/word:" ; reading]
  ; "Weblio", String.concat "" ["https://www.weblio.jp/content/" ; name]
  ; "Kotobank", String.concat "" ["https://kotobank.jp/word/" ; name]
  ]

let pronounce_lookup text =
  let open Dictionary in
  let result = Lwt_main.run @@
    try%lwt
      perform text
    with | e -> Log.log_trace e `error
      (String.concat "" ["Error during pronounciation lookup for: " ; text]); raise e
  in
  match pages result with
  | [] -> None
  | page :: _ -> Some (reading page)

let set_dictionary pd =
  state.dictionary := Some pd

let reload_dictionary path =
  let pd =
    if BatSys.file_exists path
    then PD.load path
    else
      let pd = PD.new_dictionary path in
      PD.save pd;
      pd
  in
  state.dictionary := Some pd;
  pd

let load_dictionary () =
  match !(state.dictionary) with
  | None -> reload_dictionary !(Preferences.dictionary_path)
  | Some dict -> dict

let add_to_dictionary ~name ~reading ~meaning ~image =
  let dict = load_dictionary () in
  let image =
    match image with
    | None -> []
    | Some image -> [Lwt_main.run image.imgdata]
  in
  let _, dict = PD.add_entry dict
    ({ name = name
    ; pronounciation = reading
    ; render = meaning
    ; context = { images = image }
    } : PD.entry)
  in
  state.dictionary := Some dict;
  ()

let save_dictionary () =
  let pd = load_dictionary () in
  PD.save pd
