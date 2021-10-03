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

  let resize_pixbuf pixbuf to_width to_height =
    (* TODO: check resize mode here *)
    let from_width = GdkPixbuf.get_width pixbuf in
    let from_height = GdkPixbuf.get_height pixbuf in
    let width, height =
      (* preserve aspect ratio case *)
      let result_height = from_height * to_width / from_width in
      if result_height <= to_height then to_width, result_height
      else (from_width * to_height / from_height), to_height
    in
    let result = GdkPixbuf.create ~width ~height () in
    GdkPixbuf.scale ~dest:result ~width ~height pixbuf;
    result
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
  let (set_status, status_ready) =
    let statusbar = GMisc.statusbar ~packing:layout_window#pack ~show:true () in
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
  let openbutton, actionbutton, prevbutton, nextbutton, settingsbutton =
    let toolbar = GButton.toolbar
      ~orientation:`HORIZONTAL
      ~style:`ICONS
      ~show:true
      ~packing:layout_left#pack
      ()
    in
    let add item = toolbar#insert ~pos:(- 1) item; item in
    let open GButton in
    let openbutton = add @@ tool_button ~label:"Open" ~stock:`OPEN () in
    let prevbutton = add @@ tool_button ~label:"Previous" ~stock:`GO_UP () in
    let nextbutton = add @@ tool_button ~label:"Next" ~stock:`GO_DOWN () in
    let actionbutton = add @@ tool_button ~label:"Action" ~stock:`CONVERT () in
    let settingsbutton = add @@ tool_button ~label:"Settings" ~stock:`PREFERENCES () in
    openbutton, actionbutton, prevbutton, nextbutton, settingsbutton
  in

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
        img#set_pixbuf @@ resize_pixbuf pixbuf width height;
        ()
    in
    let load_img filename =
      let buf = GdkPixbuf.from_file filename in
      pixbuf := Some buf;
      let pixbuf_width = GdkPixbuf.get_width buf in
      let pixbuf_height = GdkPixbuf.get_height buf in
      img_inner#misc#set_size_request ~width:pixbuf_width ~height:pixbuf_height ();
      img#misc#set_size_request ~width:pixbuf_width ~height:pixbuf_height ();
      rescale_img img_inner#misc#allocation
    in
    let _ = img_inner#misc#connect#size_allocate ~callback:rescale_img in
    load_img
  in

  (* list *)
  let add_file_entry, on_selection_changed, select_next, select_prev, with_selection =
    let columns = new GTree.column_list in
    let idcol = columns#add Gobject.Data.int in
    let labelcol = columns#add Gobject.Data.string in
    let imgcol = columns#add Gobject.Data.gobject in
    columns#lock () ;

    let liststore = GTree.list_store columns in
    let list = frame ~label:"Files" ~packing:layout_left#pack2
      (fun ~packing -> GTree.view ~model:liststore ~width:100 ~height:100 ~packing ()) in
    list#set_headers_visible false;
    List.iter (fun col -> ignore (list#append_column col))
    [ GTree.view_column ~renderer:(GTree.cell_renderer_text [`XALIGN 0.0], ["text", labelcol]) ()
    ; GTree.view_column ~renderer:(GTree.cell_renderer_pixbuf [`XALIGN 1.0], ["pixbuf", imgcol]) ()
    ];
    let selection = list#selection in
    let with_selection f =
      match selection#get_selected_rows with
      | [] -> ()
      | entry :: _ ->
          let iter = liststore#get_iter entry in
          let id = liststore#get ~row:iter ~column:idcol in
          let entry = Internal.find_entry id in
          f entry
    in
    let on_selection_changed callback =
      selection#connect#changed
      ~callback:(fun () -> with_selection callback) in
    let add ({ filename ; id ; filepath ; _ } : Internal.entry_state) =
      let row = liststore#append () in
      (* The caml compiler ignores my ascription here, even if this is eta expanded. Bug in value restriction? *)
      (* let set : ('a GTree.column) -> 'a -> unit = *)
      (*   (fun column value -> liststore#set ~row ~column value) *)
      (* in *)
      liststore#set ~row ~column:labelcol filename;
      liststore#set ~row ~column:idcol id;
      let pixbuf =
        try
          let pixbuf = GdkPixbuf.from_file filepath in
          resize_pixbuf pixbuf 100 50
        with | _e -> list#misc#render_icon ~size:`LARGE_TOOLBAR `DIALOG_ERROR
      in
      liststore#set ~row ~column:imgcol pixbuf;
      ()
    in
    let select f =
      match selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
          match f path with
          | true -> selection#select_path path
          | false -> ()
    in
    let select_next () = select (fun path -> GtkTree.TreePath.next path; true) in
    let select_prev () = select GtkTree.TreePath.prev in
    add, on_selection_changed, select_next, select_prev, with_selection
  in

  (* Right UI *)
  let layout_right = GPack.paned `VERTICAL ~packing:layout_split#pack2 ~show:true () in

  (* main text *)
  let textview = frame ~label:"Text" ~packing:layout_right#pack1
    (fun ~packing ->
      let textview = GText.view ~packing () in
      textview#set_monospace true;
      textview#set_justification `CENTER;
      let () =
        match !(P.text_font) with
        | "" -> ()
        | font -> textview#misc#modify_font_by_name font
      in
      textview
    )
  in
  let tag_table = GText.tag_table () in
  let use_edit_buffer, use_result_buffer, if_buffer =
    let buffer_edit = GText.buffer
      ~text:"Select a file from the filelist to get started"
      () in
    let buffer_result = GText.buffer ~tag_table () in
    let current_buffer_edit = ref true in
    let use_edit f =
      f buffer_edit;
      textview#set_buffer buffer_edit;
      textview#set_editable true;
      current_buffer_edit := true;
      ()
    in
    let use_result f =
      f buffer_result;
      textview#set_buffer buffer_result;
      textview#set_editable false;
      current_buffer_edit := false;
      ()
    in
    (* edit buffer modified *)
    let _ = buffer_edit#connect#changed ~callback:(fun () ->
      with_selection (fun entry ->
        Internal.set_buffer entry (buffer_edit#get_text ()))
    ) in
    let if_buffer ~edit ~result =
      match !current_buffer_edit with
      | true -> edit buffer_edit
      | false -> result buffer_result
    in
    use_edit, use_result, if_buffer
  in

  use_edit_buffer (fun _ -> ());

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

  let add_page, clear_pages, set_dictionary_font =
    let notebook = GPack.notebook ~scrollable:true ~packing:layout_lookup#pack () in
    notebook#set_expand true;
    let pages = ref [] in
    let views = ref [] in
    let set_font font view =
      match font with
      | "" -> ()
      | font -> view#misc#modify_font_by_name font
    in
    let add_page title content =
      let scroll = GBin.scrolled_window () in
      scroll#set_expand true;
      let view = GText.view ~wrap_mode:`WORD ~packing:scroll#add () in
      view#set_expand true;
      view#buffer#set_text content;
      set_font !(P.dict_font) view;
      views := view :: (!views);
      let label = GMisc.label ~text:title () in
      let idx = notebook#append_page ~tab_label:label#coerce scroll#coerce in
      pages := idx :: !pages;
      idx
    in
    let clear_pages () =
      List.iter (fun i -> notebook#remove_page i) !pages;
      pages := [];
      views := []
    in
    let set_font font = List.iter (set_font font) !views in
    add_page, clear_pages, set_font
  in

  let layout_search = GPack.hbox ~packing:layout_lookup#pack () in
  let search = GEdit.entry ~packing:layout_search#pack () in
  let search_button =
    let img = GMisc.image () in
    img#set_icon_size `SMALL_TOOLBAR;
    img#set_stock `FIND;
    let button = GButton.button ~packing:layout_search#pack () in
    button#set_image img#coerce;
    button
  in

  let lookup_text text =
    search#set_text text;
    let info = Internal.dict_lookup text in
    clear_pages ();
    List.iter (fun (head, body) -> ignore (add_page head body)) info;
    ()
  in

  (* action reactions *)
  (* open button clicked *)
  let _ = openbutton#connect#clicked ~callback:(fun () ->
    let chooser = GWindow.file_chooser_dialog
      ~action:`SELECT_FOLDER ~modal:true ~title:"Select your directory of images"
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
    | Some selected -> Internal.load_directory selected |> List.iter add_file_entry
    )
  ) in

  (* prev/next buttons *)
  let _ = prevbutton#connect#clicked ~callback:select_prev in
  let _ = nextbutton#connect#clicked ~callback:select_next in

  (* settings button clicked *)
  let _ = settingsbutton#connect#clicked ~callback:(fun () ->
    let window = GWindow.window
      ~title:"Settings" ~modal:true
      ~position:`CENTER
      () in
    let wlayout = GPack.vbox ~spacing:8 ~packing:window#add () in
    let grid = GPack.grid ~packing:wlayout#add
      ~row_spacings:8 ~col_spacings:8 ~border_width:16 () in
    let next =
      let row = ref 0 in
      fun () -> let r = !row in row := !row + 1; r
    in
    let add text item =
      let label = GMisc.label ~text () in
      let top = next () in
      grid#attach ~left:0 ~top label#coerce;
      grid#attach ~left:1 ~top item#coerce;
      item
    in

    (* api key settings *)
    let apikey = add "GCloud API Key" @@ GEdit.entry ~text:(!(P.gcloud_apikey)) () in
    let _ = apikey#connect#changed ~callback:(fun () ->
      P.gcloud_apikey :=  apikey#text
    ) in

    (* font settings *)
    let font_for label title font callback =
      let font_button =
        add label @@ GButton.font_button ~title ()
      in
      let () =
        match font with
        | "" -> ()
        | font -> font_button#set_font_name font
      in
      let _ = font_button#connect#font_set ~callback:(fun () ->
        let font = font_button#font_name in
        callback font
      ) in
      ()
    in

    let () = font_for "Text Font" "Select font for text input"
      !(P.text_font)
      (fun name -> P.text_font := name; textview#misc#modify_font_by_name name)
    in

    let () = font_for "Dictionary Font" "Select font for dictionary display"
      !(P.dict_font)
      (fun name -> P.dict_font := name; set_dictionary_font name)
    in

    window#show ();
    ()
  ) in

  (* list selected *)
  let _ = on_selection_changed @@
    fun entry ->
      let text =
        match !(entry.bufferdata) with
        | "" ->
            let text = Internal.fetch_ocr entry in
            Internal.set_buffer entry text;
            text
        | text -> text
      in
      load_img entry.filepath;
      use_edit_buffer (fun buffer -> buffer#set_text text)
  in

  (* action clicked *)
  let _ = actionbutton#connect#clicked ~callback:(fun () ->
    with_selection (fun entry ->
      if_buffer
      ~edit:(fun _ ->
        use_result_buffer (fun buffer ->
          buffer#set_text "";
          Internal.fetch_segment entry
          |>
            List.iter (fun sentence ->
              sentence |>
              GCloudNaturalLanguageSyntax.Segment.clean_segment_space |>
              List.map
              (fun ({text ; info} : GCloudNaturalLanguageSyntax.Segment.t) ->
                buffer#insert ~tags:(
                  match info with
                  | None -> []
                  | Some _ -> [seg_item]
                ) text
              ) |> ignore
            )
        )
      )
      ~result:(fun _ -> use_edit_buffer (fun _ -> ()))
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
      | `BUTTON_RELEASE ->
          if_buffer ~edit:(fun _ -> ())
          ~result:(fun buffer ->
            (* nope, don't handle this here but in the selection case instead *)
            if buffer#has_selection then ()
            else lookup_text text;
            ());
          false
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

  (* text selected *)
  let _ = textview#event#connect#button_release ~callback:(
    fun (button : GdkEvent.Button.t) ->
      match GdkEvent.Button.button button with
      | 1 -> (* left mouse button *)
        if_buffer ~edit:(fun _ -> ())
        ~result:(fun buffer ->
          if buffer#has_selection then
            let from, til = buffer#selection_bounds in
            let text =
              from#get_text ~stop:til
              |> String.filter (not % Char.is_whitespace)
            in
            lookup_text text
          else ();
          ());
        false
      | _ -> false
  ) in

  (* search button clicked *)
  let perform_search () =
    match search#text with
    | "" -> ()
    | text -> lookup_text text
  in
  let _ = search_button#connect#clicked ~callback:perform_search in
  let _ = search#event#connect#key_release
    ~callback:(fun key ->
      if (GdkEvent.Key.keyval key) = GdkKeysyms._Return then
        perform_search ()
      else ();
      false
    )
  in

  window#show ();
  window

let run () =
  Main.main ()
