open Batteries;;

module Dictionary = PersonalDictionary

module Main = struct

  let ui () =
    let () =
      let open Curl in
      global_init CURLINIT_GLOBALALL
    in
    Preferences.load ();
    let _ = UI.init () in
    let _ = UI.run () in
    Preferences.save ();
    ()

  let () = ui ()
end
