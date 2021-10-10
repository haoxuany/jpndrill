open Batteries;;

module P = Preferences
module Dictionary = Internal.Dictionary

let init () =
  (* window *)
  let window =
    GWindow.window
    ~title:"JPN Drill"
    ~position:`CENTER
    ~resizable:true
    ()
  in
  window#resize ~width:(!P.window_width) ~height:(!P.window_height);
  let _ = window#connect#destroy ~callback:GMain.Main.quit in
  let _ = window#misc#connect#size_allocate
    ~callback:(fun rect ->
      P.window_width := rect.width;
      P.window_height := rect.height;
      ()
    )
  in

  let resize_pixbuf pixbuf to_width to_height =
    let from_width = GdkPixbuf.get_width pixbuf in
    let from_height = GdkPixbuf.get_height pixbuf in
    let width, height =
      if !(P.preserve_aspect_ratio) then
        let result_height = from_height * to_width / from_width in
        if result_height <= to_height then to_width, result_height
        else (from_width * to_height / from_height), to_height
      else
        to_width, to_height
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
  let () = Log.hook_new_log (fun level msg -> set_status "Error raised, see logs") in

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
      set_status "Loading Image";
      let buf = GdkPixbuf.from_file filename in
      pixbuf := Some buf;
      rescale_img img_inner#misc#allocation;
      set_status "Image Loaded"
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
      (fun ~packing ->
        let scroll = GBin.scrolled_window ~packing () in
        GTree.view ~model:liststore ~width:100 ~height:100 ~packing:scroll#add ()) in
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
      let scroll = GBin.scrolled_window ~packing () in
      let textview = GText.view ~packing:scroll#add () in
      textview#set_monospace true;
      textview#set_justification `CENTER;
      textview#set_vexpand false;
      let () =
        match !(P.text_font) with
        | "" -> ()
        | font -> textview#misc#modify_font_by_name font
      in
      textview
    )
  in
  let tag_table = GText.tag_table () in
  let center () =
    let buffer = textview#buffer in
    let start, til = buffer#start_iter, buffer#end_iter in
    let start, til = (textview#get_iter_location start, textview#get_iter_location til) in
    let module R = Gdk.Rectangle in
    let top, bottom = (R.y start, (R.y til) + (R.height til)) in
    let height = R.height textview#visible_rect in
    let spacing = (height - (bottom - top)) / 2 in
    let spacing = Int.max spacing 20 in
    textview#set_top_margin spacing;
    textview#set_bottom_margin 20;
    ()
  in
  let _ = textview#misc#connect#size_allocate ~callback:(fun _ -> center ()) in
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
      center ();
      ()
    in
    let use_result f =
      f buffer_result;
      textview#set_buffer buffer_result;
      textview#set_editable false;
      current_buffer_edit := false;
      center ();
      ()
    in
    (* edit buffer modified *)
    let _ = buffer_edit#connect#changed ~callback:(fun () ->
      with_selection (fun entry ->
        Internal.set_buffer entry (buffer_edit#get_text ()));
      center ();
      ()
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
    (fun ~packing -> GPack.vbox ~packing ~height:60 ()) in

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
    let add_page page =
      let name = Internal.Dictionary.name page in
      let render = Internal.Dictionary.rendering page in
      let scroll = GBin.scrolled_window () in
      scroll#set_expand true;
      let view = GText.view ~wrap_mode:`WORD ~packing:scroll#add () in
      view#set_expand true;
      let buffer = view#buffer in
      buffer#set_text render;
      buffer#insert "\n\nOther Dictionaries:";
      List.iter (fun (sitename, url) ->
        buffer#insert "\n";
        let last = buffer#end_iter in
        let anchor = buffer#create_child_anchor last in
        view#add_child_at_anchor
        (GButton.link_button
          ~label:(String.concat "" ["Search " ; sitename ; " for \"" ; name ; "\""])
          url ())#coerce
        anchor;
        ()
      ) (Internal.external_dictionaries page);
      buffer#insert "\n\n";
      let () =
        let frame = GBin.expander
          ~label:"Add to Personal Dictionary:"
          ~border_width:4
          ()
        in
        frame#set_hexpand true;
        frame#set_expanded true;
        let box = GPack.vbox ~spacing:8 ~border_width:8 ~packing:frame#add () in
        let reading = GButton.check_button ~label:"Pronounciation"
          ~active:true
          ~packing:box#pack ()
        in
        let meaning = GButton.check_button ~label:"Meaning"
          ~active:true
          ~packing:box#pack ()
        in
        let image = GButton.check_button ~label:"Attach Image"
          ~active:true
          ~packing:box#pack ()
        in
        let button = GButton.button ~label:"Add" ~packing:box#pack () in
        let _ = button#connect#clicked ~callback:(
          fun () ->
            with_selection (fun entry ->
              let pull_checked checkbox f =
                match checkbox#active with
                | true -> Some (f ())
                | false -> None
              in
              Internal.add_to_dictionary
                ~name:(Dictionary.name page)
                ~reading:(pull_checked reading (fun () -> Dictionary.reading page))
                ~meaning:(pull_checked meaning (fun () -> Dictionary.rendering page))
                ~image:(pull_checked image (fun () -> entry))
            )
        ) in
        let last = buffer#end_iter in
        let anchor = buffer#create_child_anchor last in
        view#add_child_at_anchor frame#coerce anchor
      in
      set_font !(P.dict_font) view;
      views := view :: (!views);
      let label = GMisc.label ~text:name () in
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
  search#set_hexpand true;
  let stock_button stock =
    let img = GMisc.image () in
    img#set_icon_size `SMALL_TOOLBAR;
    img#set_stock stock;
    let button = GButton.button ~packing:layout_search#pack () in
    button#set_image img#coerce;
    button
  in
  let search_button = stock_button `FIND in
  let dictionary_button = stock_button `PAGE_SETUP in

  let lookup_text text =
    set_status "Looking up text";
    search#set_text text;
    let info = Internal.dict_lookup text in
    clear_pages ();
    List.iter (fun page -> ignore (add_page page)) info;
    set_status "Lookup finished";
    ()
  in

  (* action reactions *)
  (* actions *)
  (* TODO: turn this into a toolbar one day *)
  (* let manager = GAction.ui_manager () in *)
  (* let group = GAction.action_group ~name:"Main Actions" () in *)
  (* let () = *)
  (*   let open GAction in *)
  (*   add_actions group *)
  (*   [ add_action "open" ~label:"Open Directory" ~accel:"<Control>f" *)
  (*   ; add_action "prev" ~label:"Previous" ~accel:"<Control>k" *)
  (*   ; add_action "next" ~label:"Next" ~accel:"<Control>j" *)
  (*   ; add_action "perform" ~label:"Segment/Undo" ~accel:"<Control>l" *)
  (*   ]; *)
  (*   manager#insert_action_group group 0; *)
  (*   () *)
  (* in *)
  (* let accel = manager#get_accel_group in *)
  (* window#add_accel_group accel; *)

  (* open button clicked *)
  let _ = openbutton#connect#clicked ~callback:(fun () ->
    match !(P.gcloud_apikey) with
    | "" ->
        let msg =
          GWindow.message_dialog ~buttons:GWindow.Buttons.ok ~message_type:`ERROR
          ~parent:window ~modal:true ~title:"Error"
          ~position:`CENTER
          ~message:"No Google Cloud API Key entered, so all requests will fail. Enter it in Settings first." ()
        in
        ignore (msg#run ());
        msg#destroy ()
    | _ ->
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
      | Some selected ->
          set_status "Loading file list";
          Internal.load_directory selected |> List.iter add_file_entry;
          set_status "File list loaded";
          ()
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

    let aspect_ratio = add "Preserve Aspect Ratio" @@ GButton.check_button () in
    aspect_ratio#set_active !(P.preserve_aspect_ratio);
    let _ = aspect_ratio#connect#toggled ~callback:(fun () ->
      P.preserve_aspect_ratio := aspect_ratio#active
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

    (* the file chooser dialog button signals are not bound correctly in lablgtk, as
       such if we want this we need to fix lablgtk to deal with file-set instead of file-activated
    *)
    let _ = add "Dictionary" @@
      let button = GButton.button () in
      let reset_text () = button#set_label
        begin
        match !(P.dictionary_path) with
        | "" -> "None Selected"
        | path -> path
        end
      in
      reset_text ();
      let _ = button#connect#clicked ~callback:(fun () ->
        let chooser = GWindow.file_chooser_dialog
          ~action:`SAVE
          ~parent:window
          ~title:"Select or create a dictionary file"
          ~modal:true
          ()
        in
        chooser#set_create_folders true;
        begin
        match !(P.dictionary_path) with
        | "" -> ()
        | path -> chooser#set_filename path |> ignore
        end;
        chooser#set_filter @@ GFile.filter ~patterns:["*.dict" ; "*.zip"] ();
        chooser#set_do_overwrite_confirmation false;
        chooser#set_select_multiple false;
        chooser#add_button_stock `CANCEL `CANCEL;
        chooser#add_select_button_stock `OPEN `OPEN;
        let selected =
          match chooser#run () with
          | `DELETE_EVENT | `CANCEL -> None
          | `OPEN -> List.nth_opt chooser#get_filenames 0
        in
        chooser#destroy ();
        (match selected with
        | None -> ()
        | Some filename ->
            ignore (Internal.reload_dictionary filename);
            P.dictionary_path := filename;
            reset_text ();
            ()
        )
      ) in
      button
    in

    let error = add "Error Log" @@ GButton.button ~label:"Show" () in
    (* error window *)
    let show_error =
      let log = ref [] in
      let buffer = GText.buffer () in
      let () = Log.hook_new_log (fun level msg ->
        log := msg :: !log ;
        buffer#set_text (String.concat "\n" !log)
        )
      in
      fun () ->
        let window = GWindow.window ~title:"Error Log" ~modal:true () in
        let box = GPack.vbox ~border_width:8 ~packing:window#add () in
        let _view = GText.view ~buffer ~width:200 ~height:200 ~packing:box#add () in
        window#show ()
    in
    let _ = error#connect#clicked ~callback:show_error in

    window#show ();
    ()
  ) in

  (* list selected *)
  let _ = on_selection_changed @@
    fun entry ->
      let text =
        match !(entry.bufferdata) with
        | "" ->
            set_status "Fetching OCR";
            let text = Internal.fetch_ocr entry in
            Internal.set_buffer entry text;
            set_status "OCR fetched";
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
          set_status "Segmenting";
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
            );
          set_status "Segmentation completed";
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
  let _ = dictionary_button#connect#clicked ~callback:(fun () ->
    let window = DictUI.init
      (Internal.load_dictionary ())
      Internal.set_dictionary
    in
    DictUI.run window
  ) in

  window#show ();
  window
