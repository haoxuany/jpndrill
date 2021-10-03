open Batteries;;
open Lwt;;

module OCR = GCloudTextRecognition
module Parse = GCloudNaturalLanguageSyntax
module Lookup = JishoLookup

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
}

let state = ref { entries = ref Dict.empty }

let id =
  let counter = ref 0 in
  (fun () -> counter := !counter + 1 ; !counter)

let find_entry id = Dict.find id !((!state).entries)

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
  state := { entries = ref @@ Dict.of_list @@ List.map (fun e -> (e.id, e)) entries };
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
  let result = Lwt_main.run @@
    try%lwt
      Lookup.perform text
    with | e -> Log.log_trace e `error
      (String.concat "" ["Error during dictionary lookup for: " ; text]); raise e
  in
  let open Lookup in
  List.map (fun ({ slug ; japanese ; senses ; _}) ->
    slug,
    String.concat "\n" @@
    List.interleave "" @@
    (String.concat "/"
      (List.map (fun { word ; reading } ->
        match word with
        | None -> reading
        | Some word -> String.concat "" [word ; " (" ; reading ; ") "]
      ) japanese
      )
    ) ::
    (List.mapi (fun index { english_definitions ; parts_of_speech } ->
      String.concat ""
      [ Int.to_string (index + 1)
      ; ". "
      ; String.concat "/" english_definitions
      ; " ("
      ; String.concat "," parts_of_speech
      ; ")"
      ]
      ) senses
    )
  ) result.data

let pronounce_lookup text =
  let result = Lwt_main.run @@
    try%lwt
      Lookup.perform text
    with | e -> Log.log_trace e `error
      (String.concat "" ["Error during pronounciation lookup for: " ; text]); raise e
  in
  let open Lookup in
  match result.data with
  | { japanese = ({ reading ; _ } :: _) ; _ } :: _ -> Some reading
  | _ -> None
