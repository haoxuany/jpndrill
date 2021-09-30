open Batteries;;
open Signatures;;

type field =
  { key : string
  ; value : (module VALUE)
  }

type section =
  { header : string
  ; fields : field list
  }

type t = section list

let serialize (sections : t) =
  let serialize_field ({key ; value} : field) =
    let module Value = (val value) in
    String.concat "" [key ; "=" ; Value.serialize (Value.get Value.reference)]
  in
  String.concat "\n" (
    List.map (fun { header ; fields } ->
      String.concat "\n"
      ((String.concat "" ["[" ; header ; "]"]) :: (List.map serialize_field fields))
    ) sections
  )

let load (sections : t) s =
  (* very dumb way of reading things, but it does work and is simple *)
  let lines = String.split_on_char '\n' s
    |> List.map String.trim
    |> List.filter (fun s -> not (String.is_empty s || String.starts_with s ";")) in
  let parsed = List.fold_left
  (fun result line ->
    if String.starts_with line "[" then
      (String.filter (fun c -> not (List.exists (Char.equal c) ['[' ; ']'])) line, []) :: result
    else
      let key, value = String.split line ~by:"=" in
      let entry = (String.trim key, String.trim value) in
      match result with
      | [] -> [("unsectioned", [entry])]
      | (section, values) :: rest -> (section, entry :: values) :: rest
  )
  [] lines in
  (* very dumb and slow, but we can hash table this later *)
  List.iter (fun (name, fields) ->
    match List.find_opt (fun s -> s.header = name) sections with
    | None -> ()
    | Some { fields = section_fields ; _ } ->
        List.iter (fun (key, value) ->
          match List.find_opt (fun s -> s.key = key) section_fields with
          | None -> ()
          | Some { value = v ; _ } ->
              let module Value = (val v) in
              Value.set Value.reference (Value.deserialize value)
        ) fields
  ) parsed

module R = Commonref
