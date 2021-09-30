open Batteries;;

module I = Inifile;;

(* Window stuff *)
let window_width = ref 600
let window_height = ref 400

(* Google Cloud stuff *)
let gcloud_apikey : string option ref = ref None

let settings_spec : I.t =
  [ { header = "window"
    ; fields =
      []
      (* [ { key = "width" *)
      (*   ; value = module (I.R.StringRefReference( *)
      (*     struct *)
      (*       type t = string *)
      (*       val reference = window_width *)
      (*     end *)
      (*   ) : ) *)
      (*   } *)
      (* ] *)
    }
  ]
