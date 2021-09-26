open Batteries;;

module OCR = GCloudTextRecognition
module Parse = GCloudNaturalLanguageSyntax

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
type segmentdata = Parse.Segment.t
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
  let fetch () =
    let request = map_request
    (fun imgdata ->
      entry.imgdata := (fun () -> OK imgdata);
      try
        let ocr = GCloudTextRecognition.ocr imgdata in
        let result = ocr () in
        OK result
      with | e -> Error e
    )
    ((!(entry.imgdata))()) in
    entry.ocrdata := Some request
  in
  match !(entry.ocrdata) with
  | Some (OK _) -> ()
  | Some (Error _) | None -> fetch ()
