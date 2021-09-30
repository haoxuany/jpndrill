open Batteries;;

(* functorize this when I have time *)
module OCR = GCloudTextRecognition
module Parse = GCloudNaturalLanguageSyntax
module Lookup = JishoLookup

module Main = struct

  let ui () =
    let () =
      let open Curl in
      global_init CURLINIT_GLOBALALL
    in
    Preferences.load ();
    let _ = UI.init () in
    UI.run ()

  let () = ui ()
end
