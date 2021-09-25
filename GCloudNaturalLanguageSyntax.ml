open Batteries;;

module C = Curl
module CB = CurlBoot
module J = Yojson.Basic

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

let parse text =
  let handle = C.init () in
    CurlBoot.gauth handle
      ({ url = "https://language.googleapis.com/v1/documents:analyzeSyntax"
      (* dumb, needs to be fixed *)
      ; auth = CurlBoot.APIKey (String.trim (BatIO.read_all (BatFile.open_in "/Users/haoxuany/projects/apikey")))
      }) ;
    C.set_post handle true ;
    (`Assoc
    [ ("encodingType", `String "UTF8")
    ; ("document",
        `Assoc
        [ ("type", `String "PLAIN_TEXT" )
        ; ("content", `String text)
        ]
      )
    ])
    |> J.to_string
    |> C.set_postfields handle ;
    fun () ->
      let open J.Util in
      let module SplayDict = BatSplay.Map(struct
        type t = int
        let compare = Int.compare
      end) in
      let result = CurlBoot.perform_g handle in
      let tokens =
        result |> member "tokens" |> to_list
        |> List.map (fun json ->
            let text = json |> member "text" in
            ( text |> member "beginOffset" |> to_int
            , { text = text |> member "content" |> to_string
              ; meta = Some { tag = json |> member "partOfSpeech" |> member "tag" |> to_string } }
            ) : J.t -> (int * Segment.t))
        |> SplayDict.of_list
      in
      result |> member "sentences" |> to_list
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
