open Batteries;;

(* functorize this when I have time *)
module OCR = GCloudTextRecognition
module Parse = GCloudNaturalLanguageSyntax

module Main = struct

  let ui () =
    CurlBoot.init ();
    let _ = UI.init () in
    UI.run ()

  let () = ui ()
end
