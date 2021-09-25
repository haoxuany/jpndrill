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
    ()
  in
  let _ = window#connect#destroy ~callback:Main.quit in

  let wlayout = GPack.hbox ~packing:window#add () in
  wlayout#set_halign `FILL ;
  let grid = GPack.grid ~col_spacings:8 ~row_spacings:8 ~packing:wlayout#pack () in
  grid#set_expand true;


  (* top buttons *)
  let buttonbox = GPack.hbox () in
  buttonbox#set_halign `FILL;
  grid#attach ~left:0 ~top:0 ~width:2 buttonbox#coerce;
  (* open button *)
  let openbutton = GButton.button ~label:"Open" ~packing:buttonbox#pack () in

  (* image display *)
  let img = GMisc.image () in
  grid#attach ~left:0 ~top:1 ~width:2 img#coerce;
  img#set_halign `START;
  img#set_hexpand true;
  img#set_vexpand true;

  (* list *)
  let columns = new GTree.column_list in
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
  let buffer = GText.buffer ~text:"hello" () in
  let textview = GText.view ~buffer:buffer ~editable:false () in
  grid#attach ~left:1 ~top:0 ~width:1 ~height:4 textview#coerce;
  textview#set_halign `FILL;
  textview#set_events [`POINTER_MOTION];
  (* (GtkBase.Widget.cast textview#as_widget) *)
  let _ = textview#connect#move_cursor ~callback:(fun _ _ ~extend:_ -> print_string "moved") in

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
        BatSys.chdir selected;
        let files = BatSys.readdir "."
          |> BatArray.filter (fun s -> List.exists (BatString.ends_with s) [".jpg"]) in
        BatArray.iter (fun s ->
          let iter = liststore#append () in
          liststore#set ~row:iter ~column:labelcol s
        ) files
    )
  ) in

  (* list selected *)
  let _ = selection#connect#changed ~callback:(fun () ->
    match selection#get_selected_rows with
    | [] -> ()
    | file :: _ ->
        let iter = liststore#get_iter file in
        let filename = liststore#get ~row:iter ~column:labelcol in
        img#set_file filename;
        let ocr = GCloudTextRecognition.ocr (BatIO.read_all (BatFile.open_in filename)) in
        buffer#set_text (ocr ());
        ()
  ) in

  window#show ();
  window

let run () =
  Main.main ()
