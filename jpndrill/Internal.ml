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
; dictionary : PD.t ref
}

let load_dictionary () =
  let path = !(Preferences.dictionary_path) in
  if BatSys.file_exists path
  then PD.load path
  else
    let pd = PD.new_dictionary path in
    PD.save pd;
    pd

let state =
  { entries = ref Dict.empty
  ; dictionary = ref @@ load_dictionary ()
  }

let id =
  let counter = ref 0 in
  (fun () -> counter := !counter + 1 ; !counter)

let find_entry id = Dict.find id !(state.entries)

let load_directory dir =
  let files = BatSys.readdir dir
    |> BatArray.to_list
    |> BatList.filter (fun s -> List.exists (BatString.ends_with s) [".jpg"]) in
  let entries = List.map (fun f ->
    let filepath = (String.concat (Filename.dir_sep) [dir ; f]) in
    let created = (BatUnix.stat filepath).st_ctime in
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
  state.entries := Dict.of_list @@ List.map (fun e -> (e.id, e)) entries;
  entries |> List.fast_sort (fun a b -> BatFloat.compare a.created b.created)

let fetch_ocr (entry : entry_state) = Lwt_main.run entry.ocrdata

let set_buffer (entry : entry_state) s =
  entry.bufferdata := s;
  entry.segmentdata := None

let fetch_segment (entry : entry_state) =
  match !(entry.segmentdata) with
  | None ->
    let result = Lwt_main.run @@
      try%lwt
        Parse.perform (CurlLwtGCloud.APIKey !(Preferences.gcloud_apikey)) !(entry.bufferdata)
      with | e -> Log.log_trace e `error "Error during natural language segmentation"; raise e
    in
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

let external_dictionaries text =
  let text = Curl.escape text in
  [ "Weblio", String.concat "" ["https://www.weblio.jp/content/" ; text]
  ; "Kotobank", String.concat "" ["https://kotobank.jp/word/" ; text]
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

let add_to_dictionary page =
  let _, dict = PD.add_entry !(state.dictionary)
    ({ name = Dictionary.name page
    ; pronounciation = Some (Dictionary.reading page)
    ; render = Some (Dictionary.rendering page)
    ; context = { images = [] }
    } : PD.entry)
  in
  state.dictionary := dict;
  PD.save dict;
  ()
