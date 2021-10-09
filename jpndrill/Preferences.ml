open Batteries;;

module I = Inifile;;
module R = I.R;;

(* Window stuff *)
let window_width = ref 600
let window_height = ref 400

(* TODO: write a generic option serializer later *)
let text_font = ref ""
let dict_font = ref ""

let dictionary_width = ref 500
let dictionary_height = ref 500

let preserve_aspect_ratio = ref true

(* Google Cloud stuff *)
let gcloud_apikey : string ref = ref ""

(* Paths *)
let dictionary_path : string ref = ref "./dictionary.dict"

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
      ; { key = "preserve_aspect_ration"
        ; value =
            let module V = R.BoolRefReference(struct let value = preserve_aspect_ratio end) in
            (module V)
        }
      ; { key = "text_font"
        ; value =
            let module V = R.StringRefReference(struct let value = text_font end) in
            (module V)
        }
      ; { key = "dict_font"
        ; value =
            let module V = R.StringRefReference(struct let value = dict_font end) in
            (module V)
        }
      ; { key = "dict_width"
        ; value =
            let module V = R.IntRefReference(struct let value = dictionary_width end) in
            (module V)
        }
      ; { key = "dict_height"
        ; value =
            let module V = R.IntRefReference(struct let value = dictionary_height end) in
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
  ; { header = "paths"
    ; fields =
      [ { key = "personal_dictionary"
        ; value =
            let module V = R.StringRefReference(struct let value = dictionary_path end) in
            (module V)
        }
      ]
    }
  ]

let settings_name = "settings.ini"

let load () =
  if BatSys.file_exists settings_name
  then
    BatFile.with_file_in settings_name
    (I.load settings_spec % BatIO.read_all)
  else ()

let save () =
  BatFile.with_file_out settings_name
  (fun file -> BatIO.write_line file (I.serialize settings_spec))
