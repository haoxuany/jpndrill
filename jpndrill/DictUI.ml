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
  let layout_split =
    let layout = GPack.vbox ~packing:window#add () in
    let pane = GPack.paned `HORIZONTAL ~packing:layout#pack ~show:true () in
    pane#set_expand true;
    pane
  in

  let textview =
    let textview = GText.view ~packing:layout_split#pack2 () in
    textview#buffer#set_text "test";
    textview#set_monospace true;
    textview#set_justification `CENTER;
    textview#set_expand true;
    let () =
      match !(P.dict_font) with
      | "" -> ()
      | font -> textview#misc#modify_font_by_name font
    in
    textview
  in

  let columns = new GTree.column_list in
  let idcol = columns#add Gobject.Data.int in
  let labelcol = columns#add Gobject.Data.string in
  columns#lock () ;

  let liststore = GTree.list_store columns in
  let list = GTree.view ~model:liststore ~width:100 ~height:100 ~packing:layout_split#pack1 () in
  list#set_headers_visible false;
  list#set_expand true;
  List.iter (fun col -> ignore (list#append_column col))
  [ GTree.view_column ~renderer:(GTree.cell_renderer_text [`XALIGN 0.0], ["text", labelcol]) ()
  ];

  let add handle data =
    let row = liststore#append () in
    liststore#set ~row ~column:idcol handle;
    liststore#set ~row ~column:labelcol ((Dictionary.entry data).name);
    ()
  in

  PersonalDictionary.Map.iter add (dictionary : Dictionary.t).data;

  { window ; dictionary }

let run ({ window ; dictionary } : t) =
  window#show ()
