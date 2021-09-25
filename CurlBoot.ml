open Batteries;;
open Curl;;

(* This can definitely be cleaned up to use multithreading *)
(* also ideally we abstract over the auth stuff to an abstract type *)

let init () =
  let () = global_init CURLINIT_GLOBALALL in
  ()

type auth_method = APIKey of string
type auth = { url : string ; auth : auth_method }

let gauth handle {url ; auth} =
  match auth with
  | APIKey key ->
      set_url handle (String.concat "" [url ; "?" ; "key=" ; key])
  ;
  (* set_verbose handle true ; *)
  set_httpheader handle [
    "Content-Type: application/json; charset=utf-8"
  ]

exception CurlError of (curlCode * int * string)
exception GCloudError of (int * string)

let perform handle =
  let output = BatBuffer.create 100 in
  set_writefunction handle
    (fun s ->
      BatBuffer.add_string output s ;
      String.length s ) ;
  let result =
    try
      perform handle ; BatBuffer.contents output
    with
      | CurlException issue -> cleanup handle ; raise (CurlError issue)
  in
  cleanup handle ;
  result

let perform_g handle =
  let open Yojson.Basic in
  let open Util in
  let result = from_string (perform handle) in
  match result |> member "error" |> to_option
    (fun json -> (member "code" json |> to_int, member "message" json |> to_string))
    with
  | Some error -> raise (GCloudError error)
  | None -> result
