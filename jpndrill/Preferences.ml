open Batteries;;

module I = Inifile;;
module R = I.R;;

(* Window stuff *)
let window_width = ref 600
let window_height = ref 400

(* TODO: write a generic option serializer later *)
let text_font = ref ""
let dict_font = ref ""

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
    let file = BatFile.open_in "settings.ini" in
    BatIO.read_all file
    |> I.load settings_spec;
    BatIO.close_in file
  else ()

let save () =
  let file = BatFile.open_out "settings.ini" in
  BatIO.write_string file (I.serialize settings_spec);
  BatIO.close_out file
