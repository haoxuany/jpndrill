open GMain;;

let init () =
  let _locale = GtkMain.Main.init () in
  (* TODO functorize this *)
  (* window *)
  let width = 600 in
  let height = 500 in
  let window =
    GWindow.window
    ~title:"JPN Drill"
    ~width:width
    ~height:height
    ~position:`CENTER
    ()
  in
  let _ = window#connect#destroy ~callback:Main.quit in

  let wlayout = GPack.vbox ~packing:window#add () in
  wlayout#set_halign `FILL ;
  let grid = GPack.grid ~col_spacings:8 ~row_spacings:8 ~packing:wlayout#pack () in
  grid#set_expand true;

  (* statusbar *)
  let statusbar = GMisc.statusbar ~packing:wlayout#pack ~show:true () in
  let (set_status, status_ready) =
    let status = statusbar#new_context ~name:"Status" in
    let count = ref 0 in
    let remove_all () = for _i = 1 to !count do status#pop () done; count := 0 in
    let push = fun s -> remove_all (); status#push s |> ignore in
    push, (fun () -> push "Ready")
  in
  status_ready ();

  (* top buttons *)
  let buttonbox = GPack.hbox () in
  buttonbox#set_halign `FILL;
  grid#attach ~left:0 ~top:0 ~width:2 buttonbox#coerce;
  (* open button *)
  let openbutton = GButton.button ~label:"Open" ~packing:buttonbox#pack () in
  (* action button *)
  let actionbutton = GButton.button ~label:"Action" ~packing:buttonbox#pack () in
  (* settings button *)
  let settingsbutton = GButton.button ~label:"Settings" ~packing:buttonbox#pack () in

  (* image display *)
  let img = GMisc.image () in
  grid#attach ~left:0 ~top:1 ~width:2 img#coerce;
  img#set_halign `START;
  img#set_hexpand true;
  img#set_vexpand true;

  (* list *)
  let columns = new GTree.column_list in
  let idcol = columns#add Gobject.Data.int in
  let labelcol = columns#add Gobject.Data.string in
  columns#lock () ;

  let liststore = GTree.list_store columns in
  let list = GTree.view ~model:liststore ~width:100 ~height:100 () in
  GTree.view_column ~renderer:(GTree.cell_renderer_text [], ["text", labelcol]) ()
  |> list#append_column
  |> ignore;
  grid#attach ~left:0 ~top:2 ~width:2 list#coerce;
  let selection = list#selection in

  (* textview *)
  let buffer_edit = GText.buffer ~text:"Select a file from the filelist to get started" () in
  let buffer_result = GText.buffer () in
  let textview = GText.view ~buffer:buffer_edit () in
  textview#set_monospace true;
  textview#set_halign `FILL;
  textview#set_justification `CENTER;
  grid#attach ~left:1 ~top:0 ~width:1 ~height:4 textview#coerce;

  (* action reactions *)
  (* open button clicked *)
  let _ = openbutton#connect#clicked ~callback:(fun () ->
    let chooser = GWindow.file_chooser_dialog
      ~action:`SELECT_FOLDER ~modal:true ~title:"Select your directory of images or cards"
      () in
    chooser#add_button_stock `CANCEL `CANCEL;
    chooser#add_select_button_stock `OPEN `OPEN;
    let selected =
      match chooser#run () with
      | `DELETE_EVENT | `CANCEL -> None
      | `OPEN -> List.nth_opt chooser#get_filenames 0
    in
    chooser#destroy () ;
    (match selected with
    | None -> ()
    | Some selected ->
        Internal.load_directory selected |>
        BatList.iter (fun (entry : Internal.entry_state) ->
          let iter = liststore#append () in
          liststore#set ~row:iter ~column:labelcol entry.filename;
          liststore#set ~row:iter ~column:idcol entry.id
        )
    )
  ) in

  (* settings button clicked *)
  let _ = settingsbutton#connect#clicked ~callback:(fun () ->
    let window = GWindow.window
      ~title:"Settings" ~modal:true
      ~position:`CENTER
      () in
    let wlayout = GPack.vbox ~packing:window#add () in
    wlayout#set_halign `FILL ;
    (* font settings *)
    let font = GMisc.font_selection ~packing:wlayout#pack ~show:true () in
    let text = buffer_edit#get_text () |> BatString.trim in
    font#set_preview_text text;

    window#show ();
    ()
  ) in

  let on_selected f =
    match selection#get_selected_rows with
    | [] -> ()
    | entry :: _ ->
        let iter = liststore#get_iter entry in
        let id = liststore#get ~row:iter ~column:idcol in
        let entry = Internal.find_entry id in
        f entry
  in

  (* list selected *)
  let _ = selection#connect#changed ~callback:(fun () ->
    on_selected (fun entry ->
      img#set_file entry.filename;
      Internal.fetch_ocr entry;
      let text =
        match !(entry.ocrdata) with
        | None | Some (Error _) -> "Some Error Happened"
        | Some (OK s) -> s
      in
      buffer_edit#set_text text
    )
  ) in

  (* buffer modified *)
  let _ = buffer_edit#connect#changed ~callback:(fun () ->
    on_selected (fun entry ->
      Internal.set_buffer entry (buffer_edit#get_text ()))
  ) in

  (* action clicked *)
  let _ = actionbutton#connect#clicked ~callback:(fun () ->
    on_selected (fun entry ->
      Internal.fetch_segment entry;
      let text =
        match !(entry.segmentdata) with
        | None | Some (Error _) -> "Some Error Happened"
        | Some (OK s) ->
            BatString.join "\n" @@
            List.map
            (fun sentence -> BatString.join "/" @@
              List.map GCloudNaturalLanguageSyntax.Segment.to_string sentence ) s
      in
      buffer_result#set_text text;
      textview#set_buffer buffer_result
    )
  ) in

  window#show ();
  window

let run () =
  Main.main ()
