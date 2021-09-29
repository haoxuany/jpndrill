open Batteries;;
open Lwt;;
open CurlLwt;;
open CurlLwtGCloud;;

module C = Curl

module Segment = struct
  type meta =
    { tag : string
    }
  type t =
    { text : string
    ; meta : meta option
    }
  type sentence = t list

  let to_string ({ text ; meta } : t) =
    String.concat "" [text ; "(" ; (match meta with None -> "N/A" | Some { tag } -> tag) ; ")" ]
end

let perform text =
  let%lwt handle = init () in
  let%lwt handle = add_gcloud_auth
    "https://language.googleapis.com/v1/documents:analyzeSyntax"
    (* dumb, needs to be fixed *)
    (APIKey (String.trim (BatIO.read_all (BatFile.open_in "/Users/haoxuany/projects/apikey"))))
    handle
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
      let tokens =
        json |> member "tokens" |> to_list
        |> List.map (fun json ->
            let text = json |> member "text" in
            ( text |> member "beginOffset" |> to_int
            , { text = text |> member "content" |> to_string
              ; meta = Some { tag = json |> member "partOfSpeech" |> member "tag" |> to_string } }
            ) : Yojson.Safe.t -> (int * Segment.t))
        |> SplayDict.of_list
      in
      json |> member "sentences" |> to_list
        |> List.map (fun json ->
            let json = member "text" json in
            let content = member "content" json |> to_string in
            let offset = member "beginOffset" json |> to_int in
            let length = String.length content in
            let unparsed from til : Segment.t =
              { text = String.sub content from (til - from)
              ; meta = None
              }
            in
            let rec split offset lastoffset =
              if offset >= length then
                if lastoffset < length then
                  [unparsed lastoffset length]
                else []
              else
              match SplayDict.find_opt offset tokens with
                | Some seg ->
                    let newalign = offset + (String.length seg.text) in
                    let rest = split newalign newalign  in
                    let rest =
                      if lastoffset < offset then (unparsed lastoffset offset) :: rest else rest in
                    seg :: rest
                | None -> split (offset + 1) lastoffset
            in
            split offset offset
          )
          |> return
