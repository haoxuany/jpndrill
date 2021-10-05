open Batteries;;

module Dictionary = PersonalDictionary

module Main = struct

  let ui () =
    let () =
      let open Curl in
      global_init CURLINIT_GLOBALALL
    in
    Preferences.load ();
    let _locale = GtkMain.Main.init () in
    let _ = UI.init () in
    let _ = GMain.Main.main () in
    Preferences.save ();
    ()

  let () = ui ()
end
