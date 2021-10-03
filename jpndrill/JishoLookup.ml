open Batteries;;
open Lwt;;
open CurlLwt;;

module C = Curl

type jisho_meta =
  { status : int
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type jisho_japanese =
  { word : string option [@yojson.option]
  ; reading : string
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type jisho_senses =
  { english_definitions : string list
  ; parts_of_speech : string list
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type jisho_data =
  { slug : string
  ; is_common : bool option [@yojson.option]
  ; tags : string list
  ; japanese : jisho_japanese list
  ; senses : jisho_senses list
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type jisho_response =
  { meta : jisho_meta
  ; data : jisho_data list
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

let perform word =
  let%lwt handle = init () in
  C.set_url handle (String.concat ""
    [ "https://jisho.org/api/v1/search/words"
    ; "?"
    ; "keyword="
    ; C.escape word]
  );
  C.set_httpheader handle [
    "Content-Type: application/json; charset=utf-8"
  ];
  let%lwt result = perform handle in
  result
  |> Yojson.Safe.from_string
  |> jisho_response_of_yojson
  |> return
