open Batteries;;

module C = Curl
module CB = CurlBoot
module J = Yojson.Basic

let ocr data =
  let handle = C.init () in
    CurlBoot.gauth handle
      ({ url = "https://vision.googleapis.com/v1/images:annotate"
      (* dumb, needs to be fixed *)
      ; auth = CurlBoot.APIKey (String.trim (BatIO.read_all (BatFile.open_in "/Users/haoxuany/projects/apikey")))
      }) ;
    C.set_post handle true ;
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
    |> J.to_string
    |> C.set_postfields handle ;
    fun () ->
      let open J.Util in
      CurlBoot.perform_g handle
        |> member "responses" |> to_list |> List.hd
        |> member "fullTextAnnotation" |> member "text" |> to_string
