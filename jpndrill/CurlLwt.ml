open Batteries;;
open Lwt;;

module C = Curl

let init () = C.init () |> return

exception CurlException of string

let perform (handle : C.t) =
  let output = BatBuffer.create 100 in
  C.set_writefunction handle
    (fun s ->
      BatBuffer.add_string output s ;
      String.length s ) ;
  let%lwt code = Curl_lwt.perform handle in
  C.cleanup handle;
  match code with
  | C.CURLE_OK -> return (BatBuffer.contents output)
  | _ -> fail (CurlException (C.strerror code))
