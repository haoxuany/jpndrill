open Batteries;;
open Lwt;;
open CurlLwt;;
open CurlLwtGCloud;;

module C = Curl

type text_span =
  { content : string
  ; beginOffset : int
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type sentence =
  { text : text_span
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type tag =
  | Unknown [@name "UNKNOWN"]
  | Adjective [@name "ADJ"]
  | Adposition [@name "ADP"]
  | Adverb [@name "ADV"]
  | Conjuction [@name "CONJ"]
  | Determiner [@name "DET"]
  | Noun [@name "NOUN"]
  | Number [@name "NUM"]
  | Pronoun [@name "PRON"]
  | Particle [@name "PRT"]
  | Punctuation [@name "PUNCT"]
  | Verb [@name "VERB"]
  | Other [@name "X"]
  | Affix [@name "AFFIX"]
  [@@deriving of_yojson, show]

type part_of_speech =
  { tag : string
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type token =
  { text : text_span
  ; partOfSpeech : part_of_speech
  ; lemma : string
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type response =
  { sentences : sentence list
  ; tokens : token list
  ; language : string
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

module Segment = struct
  type info =
    { tag : tag
    ; lemma : string
    } [@@deriving show]
  type t =
    { text : string
    ; info : info option
    } [@@deriving show]
  type sentence = t list [@@deriving show]

  let empty text =
    { text ; info = None }

  let rec clean_segment_space (segs : sentence) =
    match segs with
    | [] -> []
    | ({ info = info1 ; text = text1 } as seg1) ::
      (({ info = info2 ; text = text2 } :: rest) as tail) ->
        begin
          match info1, info2 with
          | Some _, Some _ -> seg1 :: (empty " ") :: clean_segment_space tail
          | None, None ->
              clean_segment_space @@
              { text = String.concat "" [text1 ; text2] ; info = None}
              :: rest
          | _, _ -> seg1 :: clean_segment_space tail
        end
    | [_] -> segs
end

module S = Segment

let perform auth text =
  let%lwt handle = init () in
  let%lwt handle = add_gcloud_auth
    "https://language.googleapis.com/v1/documents:analyzeSyntax" auth handle
  in
  C.set_post handle true ;
  let%lwt request =
    (`Assoc
    [ ("encodingType", `String "UTF8")
    ; ("document",
        `Assoc
        [ ("type", `String "PLAIN_TEXT" )
        ; ("content", `String text)
        ]
      )
    ])
    |> Yojson.Safe.to_string
    |> return
  in
  C.set_postfields handle request;
  perform handle
  >>= handle_gcloud_error
  >>= fun json ->
      let open Yojson.Safe.Util in
      let module SplayDict = BatSplay.Map(struct
        type t = int
        let compare = Int.compare
      end) in
      let { sentences ; tokens } = response_of_yojson json in
      let tokens =
        tokens
        |> List.map (fun tok -> (tok.text.beginOffset, tok))
        |> SplayDict.of_list
      in
      sentences
        |> List.map (fun ({ text = { content ; beginOffset = offset }} : sentence) ->
            (* offsets are assumed absolute to content *)
            let unparsed from til : S.t =
              let len = til - from in
              let from = from - offset in
              S.empty (String.sub content from len)
            in
            let length = offset + String.length content in
            let rec split offset lastoffset =
              if offset >= length then
                if lastoffset < length then
                  [unparsed lastoffset length]
                else []
              else
              match SplayDict.find_opt offset tokens with
                | Some { text = { content = text ; _ } ; partOfSpeech = { tag } ; lemma } ->
                    let newalign = offset + (String.length text) in
                    let rest = split newalign newalign in
                    let rest =
                      if lastoffset < offset then (unparsed lastoffset offset) :: rest else rest in
                    { text
                    ; info = Some
                      { tag =
                        String.concat "" ["[\"" ; tag ; "\"]"]
                        |> Yojson.Safe.from_string
                        |> tag_of_yojson
                      ; lemma
                      }
                    } :: rest
                | None -> split (offset + 1) lastoffset
            in
            split offset offset
          )
          |> return
