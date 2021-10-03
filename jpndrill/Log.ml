open Batteries;;

module L = BatLog.Easy

let () = L.level := `info

let flush () = flush !(L.output)

let push_log, hook_new_log =
  let hooks = ref [] in
  let msgs = ref [] in
  (fun level msg ->
    msgs := msg :: !msgs;
    List.iter (fun hook -> hook level msg) !hooks;
    ()
  ),
  (fun hook -> hooks := hook :: !hooks)

let log level msg = L.log level msg ; push_log level msg ; flush ()

let logf level form = log level (BatPrintf.sprintf form)

let log_trace e level msg =
  log level @@
  String.concat "\n"
  [ BatPrintexc.to_string e
  ; "at trace:"
  ; BatPrintexc.get_backtrace ()
  ; msg
  ]

let register_printer = BatPrintexc.register_printer

let () = register_printer
  (fun exn ->
    match exn with
    | CurlLwt.CurlException msg -> Some (String.concat " " ["Curl Exception:" ; msg])
    | CurlLwtGCloud.GCloudError (code, message, status) ->
        Some (String.concat " "
          ["Google Cloud Error:"
          ; Int.to_string code
          ; "with msg:"
          ; message
          ; "with status:"
          ; status
          ])
    | Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (exn, json) ->
        Some (String.concat "\n"
          [ "JSON Parsing Error:"
          ; BatPrintexc.to_string exn
          ; "with json:"
          ; Yojson.Safe.pretty_to_string json
          ])
    | _ -> None)
