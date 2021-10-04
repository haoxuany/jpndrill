open Batteries;;

module Map = BatMap.Int

type context_internal =
  { images : string Lwt.t list (* contains data *)
  }

type entry_internal =
  { creation : int
  ; name : string
  ; pronounciation : string option
  ; render : string option
  ; context : context_internal
  }

type handle = int

type data =
  { entry : entry_internal Lwt.t
  ; index : handle
  }

type t =
  { filepath : string
  ; data : data Map.t
  }

let new_dictionary filepath =
  { filepath
  ; data = Map.empty
  }

type context =
  { images : string list
  }

type entry =
  { name : string
  ; pronounciation : string option
  ; render : string option
  ; context : context
  }

let add_entry ( { filepath ; data } : t )
  ( { name ; pronounciation ; render ; context } : entry) =
  let max = Map.max_binding_opt data in
  let max =
    match max with
    | None -> 0
    | Some (i, _) -> i + 1
  in
  let entry =
    { creation = BatUnix.time () |> Float.to_int
    ; name ; pronounciation ; render
    ; context = { images = List.map Lwt.return context.images } } in
  let entry = { entry = Lwt.return entry ; index = max } in
  max, { filepath ; data = Map.add max entry data }

let find ( { data ; _ } : t) handle = Map.find handle data

let entry data =
  Lwt_main.run @@
  let%lwt entry = data.entry in
  let { name ; pronounciation ; render ; context = { images } ; _ } : entry_internal = entry in
  let images = Lwt_list.map_s (fun image -> image) images in
  let%lwt images = images in
  Lwt.return
    { name
    ; pronounciation
    ; render
    ; context = { images }
    }

module Serialize = struct
  open Zip

  type context_json =
    { images : string list (* names *)
    } [@@deriving yojson, show] [@@yojson.allow_extra_fields]

  type entry_json =
    { creation : int
    ; name : string
    ; pronounciation : string option
    ; render : string option
    ; context : context_json
    } [@@deriving yojson, show] [@@yojson.allow_extra_fields]

  let img i = String.concat "" [ "images/" ; Int.to_string i ; ".jpg"] (* a bit hacky to assume jpeg *)

  let save ( { data ; filepath } : t ) =
    let file = open_out filepath in
    let img_count = ref 0 in
    Map.iter
    (fun _ ({ entry ; index ; } : data) ->
      let filename = (Int.to_string index) ^ ".json" in
      let ({ creation ; name ; pronounciation ; render ; context = { images } } : entry_internal) =
        Lwt_main.run entry in
      let images = List.map
        (fun image ->
          let image = Lwt_main.run image in
          let idx = !img_count in
          img_count := !img_count + 1;
          let filename = img idx in
          add_entry image file filename;
          filename
        ) images in
      let json =
        { creation
        ; name
        ; pronounciation
        ; render
        ; context = { images = images }
        }
        |> yojson_of_entry_json
      in
      add_entry (Yojson.Safe.to_string json) file ~level:0 filename
    ) data;
    close_out file

  let load filepath =
    let file = open_in filepath in
    let entries = entries file in
    (* (1* how does the hash table magic even work? *1) *)
    (* let module Table = BatHashtbl in *)
    (* let table = Table.create 10 in *)
    (* List.iter (fun entry -> Table.add table entry.filename entry) entries; *)
    let data =
      List.filter_map
      (fun entry ->
        if String.ends_with entry.filename ".json"
        then
          let ({ creation ; name ; pronounciation ; render ; context = { images } } : entry_json) =
            read_entry file entry
            |> Yojson.Safe.from_string
            |> entry_json_of_yojson
          in
          let data =
            (* could be done better, maybe *)
            { entry = Lwt.return
                ({ creation
                ; name
                ; pronounciation
                ; render
                ; context = ({ images = List.map
                  (fun name -> Lwt.return (find_entry file name |> read_entry file)) images }
                  : context_internal)
                } : entry_internal)
            ; index = String.split entry.filename ~by:"."
              |> (fun (index, _) -> Int.of_string index)
            }
          in Some data
        else None)
      entries
    in
    let result =
      { filepath
      ; data = List.fold_left (fun map data -> Map.add data.index data map) Map.empty data
      }
    in
    close_in file;
    result
end

let save = Serialize.save

let load = Serialize.load
