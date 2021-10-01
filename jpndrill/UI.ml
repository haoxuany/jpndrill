open Batteries;;
open GMain;;

module P = Preferences

let init () =
  let _locale = GtkMain.Main.init () in
  (* window *)
  let window =
    GWindow.window
    ~title:"JPN Drill"
    ~position:`CENTER
    ~resizable:true
    ()
  in
  window#resize ~width:(!P.window_width) ~height:(!P.window_height);
  let _ = window#connect#destroy ~callback:Main.quit in
  let _ = window#misc#connect#size_allocate
    ~callback:(fun rect ->
      P.window_width := rect.width;
      P.window_height := rect.height;
      ()
    )
  in

  let layout_window = GPack.vbox ~packing:window#add () in

  let layout_split = GPack.paned `HORIZONTAL ~packing:layout_window#pack ~show:true () in
  layout_split#set_expand true;

  let frame ~label ~packing f =
    let frame = GBin.frame ~label ~packing ~show:true () in
    frame#set_expand true;
    f ~packing:frame#add
  in

  (* statusbar *)
  let statusbar = GMisc.statusbar ~packing:layout_window#pack ~show:true () in
  let (set_status, status_ready) =
    let status = statusbar#new_context ~name:"Status" in
    let count = ref 0 in
    let remove_all () = for _i = 1 to !count do status#pop () done; count := 0 in
    let push = fun s -> remove_all (); status#push s |> ignore in
    push, (fun () -> push "Ready")
  in
  status_ready ();

  (* Left UI *)
  let layout_left = GPack.vbox ~packing:layout_split#pack1 () in

  (* top buttons *)
  let buttonbox = GPack.hbox ~packing:layout_left#pack () in
  (* open button *)
  let openbutton = GButton.button ~label:"Open" ~packing:buttonbox#pack () in
  (* action button *)
  let actionbutton = GButton.button ~label:"Action" ~packing:buttonbox#pack () in
  (* settings button *)
  let settingsbutton = GButton.button ~label:"Settings" ~packing:buttonbox#pack () in

  let layout_left = GPack.paned `VERTICAL ~packing:layout_left#pack ~show:true () in

  (* image display *)
  let load_img =
    let pixbuf = ref None in
    let img_inner = GBin.scrolled_window () in
    let img = frame ~label:"Image" ~packing:layout_left#pack1
      (fun ~packing ->
        packing img_inner#coerce;
        GMisc.image ~packing:img_inner#add ()) in
    let rescale_img rect =
      match !pixbuf with
      | None -> ()
      | Some pixbuf ->
        let ({ width ; height } : Gtk.rectangle) = rect in
        let result = GdkPixbuf.create ~width ~height () in
        GdkPixbuf.scale ~dest:result ~width ~height pixbuf;
        img#misc#set_size_request ~width:(GdkPixbuf.get_width pixbuf) ~height:(GdkPixbuf.get_height pixbuf) ();
        img_inner#misc#set_size_request ~width:(GdkPixbuf.get_width pixbuf) ~height:(GdkPixbuf.get_height pixbuf) ();
        img#set_pixbuf result;
        ()
    in
    let load_img filename =
      let buf = GdkPixbuf.from_file filename in
      pixbuf := Some buf;
      rescale_img img_inner#misc#allocation
    in
    let _ = img_inner#misc#connect#size_allocate ~callback:rescale_img in
    load_img
  in

  (* list *)
  let columns = new GTree.column_list in
  let idcol = columns#add Gobject.Data.int in
  let labelcol = columns#add Gobject.Data.string in
  columns#lock () ;

  let liststore = GTree.list_store columns in
  let list = frame ~label:"Files" ~packing:layout_left#pack2
    (fun ~packing -> GTree.view ~model:liststore ~width:100 ~height:100 ~packing ()) in
  GTree.view_column ~renderer:(GTree.cell_renderer_text [], ["text", labelcol]) ()
  |> list#append_column
  |> ignore;

  let selection = list#selection in

  (* Right UI *)
  let layout_right = GPack.paned `VERTICAL ~packing:layout_split#pack2 ~show:true () in

  (* main text *)
  let buffer_edit = GText.buffer
    ~text:"Select a file from the filelist to get started"
    () in
  let tag_table = GText.tag_table () in
  let buffer_result = GText.buffer ~tag_table () in
  let textview = frame ~label:"Text" ~packing:layout_right#pack1
    (fun ~packing -> GText.view ~buffer:buffer_edit ~packing ()) in
  textview#set_monospace true;
  textview#set_justification `CENTER;

  (* tags *)
  let tag name prop =
    let tag = GText.tag ~name () in
    tag#set_properties prop;
    tag
  in
  let seg_item = tag "Tag" [`UNDERLINE_SET true ; `UNDERLINE `SINGLE] in
  tag_table#add seg_item#as_tag;

  (* lookup *)
  let layout_lookup = frame ~label:"Lookup" ~packing:layout_right#pack2
    (fun ~packing -> GPack.vbox ~packing ()) in

  let add_page, clear_pages =
    let notebook = GPack.notebook ~scrollable:true ~packing:layout_lookup#pack () in
    notebook#set_expand true;
    let pages = ref [] in
    let add_page title content =
      let scroll = GBin.scrolled_window () in
      scroll#set_expand true;
      let view = GText.view ~wrap_mode:`WORD ~packing:scroll#add () in
      view#set_expand true;
      view#buffer#set_text content;
      let label = GMisc.label ~text:title () in
      let idx = notebook#append_page ~tab_label:label#coerce scroll#coerce in
      pages := idx :: !pages;
      idx
    in
    let clear_pages () =
      List.iter (fun i -> notebook#remove_page i) !pages
    in
    add_page, clear_pages
  in

  let layout_search = GPack.hbox ~packing:layout_lookup#pack () in
  let search = GEdit.entry ~packing:layout_search#pack () in

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
        List.iter (fun (entry : Internal.entry_state) ->
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
    (* font settings *)
    let font = GMisc.font_selection ~packing:wlayout#pack ~show:true () in
    let text = buffer_edit#get_text () |> String.trim in
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
      let text =
        match !(entry.bufferdata) with
        | "" ->
            let text = Internal.fetch_ocr entry in
            Internal.set_buffer entry text;
            text
        | text -> text
      in
      load_img entry.filepath;
      buffer_edit#set_text text;
      textview#set_buffer buffer_edit;
      textview#set_editable true
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
      Internal.fetch_segment entry
      |>
        List.iter (fun sentence ->
          sentence |>
          GCloudNaturalLanguageSyntax.Segment.clean_segment_space |>
          List.map
          (fun ({text ; info} : GCloudNaturalLanguageSyntax.Segment.t) ->
            buffer_result#insert ~tags:(
              match info with
              | None -> []
              | Some _ -> [seg_item]
            ) text
          ) |> ignore
        );
      textview#set_buffer buffer_result;
      textview#set_editable false
    )
  ) in

  (* tag reactions *)
  let _ = seg_item#connect#event ~callback:(
    fun ~origin event it ->
      let iter = new GText.iter it in
      let start = iter#backward_to_tag_toggle (Some seg_item) in
      let stop = iter#forward_to_tag_toggle (Some seg_item) in
      let text = start#get_text ~stop in
      match GdkEvent.get_type event with
      | `BUTTON_PRESS ->
          search#set_text text;
          let info = Internal.dict_lookup text in
          let head, body = List.hd info in
          clear_pages ();
          add_page head body;
          true
      | `MOTION_NOTIFY ->
          (* we'd need a caching way of doing this if we ever want to, otherwise we'd hit
             a request overload. *)
          (* begin match Internal.pronounce_lookup text with *)
          (* | None -> textview#set_has_tooltip false *)
          (* | Some result -> *)
          (*     textview#set_has_tooltip true; *)
          (*     textview#set_tooltip_text result *)
          (* end; *)
          false
      | _ -> false
  ) in

  window#show ();
  window

let run () =
  Main.main ()
