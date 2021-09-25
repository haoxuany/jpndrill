open Batteries;;

(* functorize this when I have time *)
module OCR = GCloudTextRecognition
module Parse = GCloudNaturalLanguageSyntax

module Main = struct
  let run file =
    try
      CurlBoot.init () ;
      let comp = OCR.ocr (BatIO.read_all (BatFile.open_in file)) in
      let text = comp () in
      (* let _ = file in *)
      (* let text = "I am a groot. I take my groot." in *)
      let comp = Parse.parse text in
      let text = comp () in
      let show = String.join "\n"
        (List.map (fun l -> String.join "/"
        (List.map Parse.Segment.to_string l)) text) in
      print_string show
    with
    | CurlBoot.GCloudError (_code, s) -> print_string s

  let ui () =
    let _ = UI.init () in
    UI.run ()

  let () = ui ()
end
