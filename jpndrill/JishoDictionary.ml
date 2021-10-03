open Batteries;;
open JishoLookup;;

type t = jisho_response

type page = jisho_data

let perform = perform

let pages ({ data ; _ } : t) = data

let name ({ slug ; japanese ; _ } : page) =
  match japanese with
  | { word ; reading } :: _ ->
      begin match word, reading with
      | Some word, _ -> word
      | None, Some reading -> reading
      | None, None -> slug
      end
  | _ -> slug

let reading ({ slug ; japanese ; _ } : page) =
  match japanese with
  | { reading ; word } :: _ ->
      begin match reading, word with
      | Some reading, _ -> reading
      | None, Some word -> word
      | None, None -> slug
      end
  | _ -> slug

let rendering ({ slug ; japanese ; senses ; _ } : page) =
  String.concat "\n" @@
  List.interleave "" @@
  (String.concat "/"
    (List.map (fun { word ; reading } ->
      match word, reading with
      | None, Some reading -> reading
      | Some word, None -> word
      | Some word, Some reading -> String.concat "" [word ; " (" ; reading ; ") "]
      | None, None -> slug
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
