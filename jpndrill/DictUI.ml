open Batteries;;

module P = Preferences
module Dictionary = PersonalDictionary

type t =
  { window : GWindow.window
  ; dictionary : Dictionary.t
  }

let init dictionary =
  let window =
    GWindow.window
    ~title:"Personal Dictionary"
    ~modal:true
    ~position:`CENTER
    ~resizable:true
    ()
  in
  window#resize ~width:!(P.dictionary_width) ~height:!(P.dictionary_height);
  let _ = window#misc#connect#size_allocate
    ~callback:(fun rect ->
      P.dictionary_width := rect.width;
      P.dictionary_height := rect.height;
      ()
    )
  in
  let layout_split =
    let layout = GPack.vbox ~packing:window#add () in
    let pane = GPack.paned `HORIZONTAL ~packing:layout#pack ~show:true () in
    pane#set_expand true;
    pane
  in
  let font () =
    match !(P.dict_font) with
    | "" -> None
    | font -> Some font
  in

  let textview =
    let scroll = GBin.scrolled_window ~packing:layout_split#pack2 () in
    let textview = GText.view ~packing:scroll#add () in
    textview#buffer#set_text "Select an entry to display";
    textview#set_monospace true;
    textview#set_expand true;
    textview#set_wrap_mode `WORD;
    let () =
      match font () with
      | None -> ()
      | Some font -> textview#misc#modify_font_by_name font
    in
    textview
  in

  let columns = new GTree.column_list in
  let idcol = columns#add Gobject.Data.int in
  let labelcol = columns#add Gobject.Data.string in
  columns#lock () ;

  let liststore = GTree.list_store columns in
  let list =
    let scroll = GBin.scrolled_window ~packing:layout_split#pack1 () in
    GTree.view ~model:liststore ~width:100 ~height:100 ~packing:scroll#add ()
  in
  list#set_headers_visible false;
  list#set_expand true;
  let properties =
    let result = [`XALIGN 0.0] in
    match font () with
    | None -> result
    | Some font -> (`FONT font) :: result
  in
  List.iter (fun col -> ignore (list#append_column col))
  [ GTree.view_column ~renderer:(GTree.cell_renderer_text properties, ["text", labelcol]) ()
  ];
  let selection = list#selection in

  let with_selection f =
    match selection#get_selected_rows with
    | [] -> ()
    | entry :: _ ->
        let iter = liststore#get_iter entry in
        let id = liststore#get ~row:iter ~column:idcol in
        let entry = Dictionary.Map.find id (dictionary : Dictionary.t).data
          |> Dictionary.entry in
        f entry
  in

  let add handle data =
    let row = liststore#append () in
    liststore#set ~row ~column:idcol handle;
    liststore#set ~row ~column:labelcol ((Dictionary.entry data).name);
    ()
  in
  let _ = selection#connect#changed ~callback:(fun () ->
    with_selection (fun (entry : Dictionary.entry) ->
      let buffer = textview#buffer in
      buffer#set_text "";
      begin
        match entry.pronounciation with
        | None -> ()
        | Some reading ->
            buffer#insert "Pronouciation: ";
            buffer#insert reading;
            buffer#insert "\n\n"
      end;
      begin
        match entry.render with
        | None -> ()
        | Some render ->
            buffer#insert render;
            buffer#insert "\n\n"
      end;
      begin
        match entry.context.images with
        | [] -> ()
        | images ->
            buffer#insert "Context:";
            List.iter (fun image ->
              buffer#insert "\n";
              try
                let file =
                  BatFile.with_temporary_out ~suffix:"jpg"
                  (fun stream filename ->
                    BatIO.write_line stream image;
                    filename
                  ) in
                let last = buffer#end_iter in
                let pixbuf = GdkPixbuf.from_file file in
                buffer#insert_pixbuf ~iter:last ~pixbuf
              with | _ -> buffer#insert "Error rendering image";
              ()
            ) images
      end;
      ()
    )
  ) in

  Dictionary.Map.iter add (dictionary : Dictionary.t).data;

  { window ; dictionary }

let run ({ window ; dictionary } : t) =
  window#show ()
