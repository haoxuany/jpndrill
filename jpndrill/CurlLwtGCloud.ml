open Batteries;;
open Lwt;;
open CurlLwt;;

module C = Curl

type auth_method = APIKey of string

let add_gcloud_auth url auth handle =
  match auth with
  | APIKey key ->
      C.set_url handle (String.concat "" [url ; "?" ; "key=" ; key])
  ;
  (* set_verbose handle true ; *)
  C.set_httpheader handle [
    "Content-Type: application/json; charset=utf-8"
  ];
  return handle

type gcloud_error_details =
  { ty : string [@key "@type"]
  ; reason : string
  ; domain : string
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type gcloud_error_field =
  { code : int
  ; message : string
  ; status : string
  ; details : gcloud_error_details
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

type gcloud_error =
  { error : gcloud_error_field
  } [@@deriving of_yojson, show] [@@yojson.allow_extra_fields]

exception GCloudError of int * string * string

let handle_gcloud_error result =
  let json = Yojson.Safe.from_string result in
  try
    let { error = { code ; message ; status ; _ } } =
      gcloud_error_of_yojson json in
    fail (GCloudError (code , message , status))
  with | _ -> return json
