open Batteries;;

module I = Inifile;;
module R = I.R;;

(* Window stuff *)
let window_width = ref 600
let window_height = ref 400

(* Google Cloud stuff *)
let gcloud_apikey : string ref = ref ""

let settings_spec : I.t =
  [ { header = "window"
    ; fields =
      [ { key = "width"
        ; value =
            let module V = R.IntRefReference(struct let value = window_width end) in
            (module V)
        }
      ; { key = "height"
        ; value =
            let module V = R.IntRefReference(struct let value = window_height end) in
            (module V)
        }
      ]
    }
  ; { header = "gcloud"
    ; fields =
      [ { key = "key"
        ; value =
            let module V = R.StringRefReference(struct let value = gcloud_apikey end) in
            (module V)
        }
      ]
    }
  ]

let load () =
  (BatFile.open_in "settings.ini")
  |> BatIO.read_all
  |> I.load settings_spec