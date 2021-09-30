open Batteries;;
open Lwt;;

module OCR = GCloudTextRecognition
module Parse = GCloudNaturalLanguageSyntax
module Lookup = JishoLookup

let read_dir_files dir =
  BatSys.readdir dir
    |> BatArray.to_list
    |> BatList.filter (fun s -> List.exists (BatString.ends_with s) [".jpg"])

type ocrdata = string
type segmentdata = Parse.Segment.sentence list
type id = int

type entry_state =
  { id : id
  ; filename : string
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
  let files = read_dir_files dir in
  let entries = List.map (fun f ->
    let imgdata = Lwt_io.with_file ~mode:Lwt_io.Input f Lwt_io.read in
    let bufferdata = ref "" in
    let ocrdata =
      let%lwt imgdata = imgdata in
      let%lwt result = OCR.perform (CurlLwtGCloud.APIKey !(Preferences.gcloud_apikey)) imgdata in
      bufferdata := result;
      return result
    in
    { id = id ()
    ; filename = f
    ; imgdata = imgdata
    ; ocrdata = ocrdata
    ; bufferdata = bufferdata
    ; segmentdata = ref None
    }
  ) files in
  state := { entries = ref @@ Dict.of_list @@ List.map (fun e -> (e.id, e)) entries };
  entries

let fetch_ocr (entry : entry_state) = Lwt_main.run entry.ocrdata

let set_buffer (entry : entry_state) s =
  entry.bufferdata := s;
  entry.segmentdata := None

let fetch_segment (entry : entry_state) =
  match !(entry.segmentdata) with
  | None ->
    let result = Lwt_main.run
      (Parse.perform (CurlLwtGCloud.APIKey !(Preferences.gcloud_apikey)) !(entry.bufferdata)) in
    entry.segmentdata := Some result;
    result
  | Some result -> result

let dict_lookup text =
  let result = Lwt_main.run (Lookup.perform text) in
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
