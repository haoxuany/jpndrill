open Batteries;;
open JishoLookup;;

type t = jisho_response

type page = jisho_data

let perform = perform

let pages ({ data ; _ } : t) = data

let name ({ slug ; japanese ; _ } : page) =
  match japanese with
  | { word ; reading } :: _ ->
      begin match word with
      | None -> reading
      | Some word -> word
      end
  | _ -> slug

let reading ({ slug ; japanese ; _ } : page) =
  match japanese with
  | { reading ; _ } :: _ -> reading
  | _ -> slug

let rendering ({ slug ; japanese ; senses ; _ } : page) =
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
