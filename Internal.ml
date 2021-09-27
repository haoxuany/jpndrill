open Batteries;;

module OCR = GCloudTextRecognition
module Parse = GCloudNaturalLanguageSyntax
module Lookup = JishoLookup

let read_dir_files dir =
  BatSys.readdir dir
    |> BatArray.to_list
    |> BatList.filter (fun s -> List.exists (BatString.ends_with s) [".jpg"])

type 'a request =
| OK of 'a
| Error of exn

let map_request f r =
  match r with
  | OK a -> f a
  | Error exn -> Error exn

type 'a staged = unit -> 'a request

type ocrdata = string
type segmentdata = Parse.Segment.sentence list
type id = int

type entry_state =
  { id : id
  ; filename : string
  ; imgdata : string staged ref
  ; ocrdata : ocrdata request option ref
  ; bufferdata : string ref
  ; segmentdata : segmentdata request option ref
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
  let wrap_exn f = fun () -> try OK (f ()) with | e -> Error e in
  let entries = List.map (fun f ->
    { id = id ()
    ; filename = f
    ; imgdata = ref @@ wrap_exn (fun () -> f |> BatFile.open_in |> BatIO.read_all)
    ; ocrdata = ref None
    ; bufferdata = ref ""
    ; segmentdata = ref None
    }
  ) files in
  state := { entries = ref @@ Dict.of_list @@ List.map (fun e -> (e.id, e)) entries };
  entries

let fetch_ocr (entry : entry_state) =
  match !(entry.ocrdata) with
  | Some (OK _) -> ()
  | Some (Error _) | None ->
      let request = map_request
      (fun imgdata ->
        entry.imgdata := (fun () -> OK imgdata);
        try
          let ocr = GCloudTextRecognition.ocr imgdata in
          let result = ocr () in
          entry.bufferdata := result;
          OK result
        with | e -> Error e
      )
      ((!(entry.imgdata))()) in
      entry.ocrdata := Some request

let set_buffer (entry : entry_state) s =
  entry.bufferdata := s;
  entry.segmentdata := None

let fetch_segment (entry : entry_state) =
  match !(entry.segmentdata) with
  | Some (OK _) -> ()
  | _ ->
    entry.segmentdata :=
      Some (try
        let parse = Parse.parse !(entry.bufferdata) in
        let result = parse () in
        OK result
      with | e -> Error e)

(* dumb, fix this *)
let dict_lookup text =
  let result = Lookup.lookup text () in
  result.data
    |> List.hd
    |> (fun d -> d.japanese)
    |> List.hd
    |> (fun d -> d.reading)
