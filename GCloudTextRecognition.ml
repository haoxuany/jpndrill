open Batteries;;
open Lwt;;
open CurlLwt;;
open CurlLwtGCloud;;

module C = Curl

let perform data =
  let%lwt handle = init () in
  let%lwt handle = add_gcloud_auth
    "https://vision.googleapis.com/v1/images:annotate"
    (* dumb, needs to be fixed *)
    (APIKey (String.trim (BatIO.read_all (BatFile.open_in "/Users/haoxuany/projects/apikey"))))
    handle
  in
  C.set_post handle true ;
  let%lwt request =
    Base64.str_encode data
    |> fun content ->
        (`Assoc [
          ("requests",
            `List [
              `Assoc [
                ("image", `Assoc [ ("content", `String content) ])
              ; ("features", `List [ `Assoc [("type", `String "TEXT_DETECTION")] ])
              ]])
        ])
    |> Yojson.Safe.to_string
    |> return
  in
  C.set_postfields handle request;
  perform handle
  >>= handle_gcloud_error
  >>= (fun json ->
      let open Yojson.Safe.Util in
      json
      |> member "responses" |> to_list |> List.hd
      |> member "fullTextAnnotation" |> member "text" |> to_string
      |> return)
