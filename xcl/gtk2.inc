(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
(*
  ATENCAO:

  Aqui sao adicionadas funcoes que nao constam no package gtk2 que
  acopanha o FPC. Sempre que atualizado isso no package gtk2 do
  FPC, remover daqui. (judison)

*)

// GLib

procedure g_set_application_name(application_name: pgchar); cdecl; external gliblib;

function g_closure_new_simple(sizeof_closure:guint; data:gpointer):PGClosure; cdecl; external gobjectlib;

// GDK

function gdk_event_new(_type: TGdkEventType): PGdkEvent;  cdecl; external gdklib;
procedure gdk_draw_pixbuf(drawable: PGdkDrawable; gc:PGdkGC; src:PGdkPixbuf;
            xsrc:gint; ysrc:gint; xdest:gint; ydest:gint;
            width:gint; height:gint; dither:TGdkRgbDither;
            x_dither:gint; y_dither:gint); cdecl; external gdklib;

//==
function gtk_file_chooser_error_quark: TGQuark; cdecl; external gtklib;
function gtk_file_system_error_quark: TGQuark; cdecl; external gtklib;
function gtk_icon_theme_error_quark: TGQuark; cdecl; external gtklib;

// GtkMessageDialog

procedure gtk_message_dialog_format_secondary_text(dialog: pointer; msg: pgchar); cdecl; external gtklib;

// GtkAccelerator

function gtk_accelerator_get_label(accelerator_key: guint; accelerator_mods: longint): PGChar; cdecl; external gtklib;

// GtkImage (Since 2.6)

procedure gtk_image_set_from_icon_name(image: pointer; icon_name: pgchar; size: longint); cdecl; external gtklib;

// GtkIconTheme (Since 2.4)

function gtk_icon_theme_get_default(): pointer; cdecl; external gtklib;
function gtk_icon_theme_load_icon(icon_theme: pointer; icon_name: pgchar; size: gint; flags: longint; error: ppgerror): pointer; cdecl; external gtklib;
function gtk_icon_theme_has_icon(icon_theme: pointer; icon_name: pgchar): gboolean; cdecl; external gtklib;
procedure gtk_icon_theme_append_search_path(icon_theme: pointer; path: pgchar); cdecl; external gtklib;
procedure gtk_icon_theme_prepend_search_path(icon_theme: pointer; path: pgchar); cdecl; external gtklib;

// GtkAboutDialog (Since 2.6)

type
  PGtkAboutDialog = pointer;

function gtk_about_dialog_new:PGtkWidget;cdecl;external gtklib;
function gtk_about_dialog_get_name(about:PGtkAboutDialog):Pgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_name(about:PGtkAboutDialog; name:Pgchar);cdecl;external gtklib;
function gtk_about_dialog_get_version(about:PGtkAboutDialog):Pgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_version(about:PGtkAboutDialog; version:Pgchar);cdecl;external gtklib;
function gtk_about_dialog_get_copyright(about:PGtkAboutDialog):Pgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_copyright(about:PGtkAboutDialog; copyright:Pgchar);cdecl;external gtklib;
function gtk_about_dialog_get_comments(about:PGtkAboutDialog):Pgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_comments(about:PGtkAboutDialog; comments:Pgchar);cdecl;external gtklib;
function gtk_about_dialog_get_license(about:PGtkAboutDialog):Pgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_license(about:PGtkAboutDialog; license:Pgchar);cdecl;external gtklib;
//function gtk_about_dialog_get_wrap_license(about:PGtkAboutDialog):gboolean;cdecl;external gtklib;
//procedure gtk_about_dialog_set_wrap_license(about:PGtkAboutDialog; wrap_license:gboolean);cdecl;external gtklib;
function gtk_about_dialog_get_website(about:PGtkAboutDialog):Pgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_website(about:PGtkAboutDialog; website:Pgchar);cdecl;external gtklib;
function gtk_about_dialog_get_website_label(about:PGtkAboutDialog):Pgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_website_label(about:PGtkAboutDialog; website_label:Pgchar);cdecl;external gtklib;
function gtk_about_dialog_get_authors(about:PGtkAboutDialog):PPgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_authors(about:PGtkAboutDialog; authors:PPgchar);cdecl;external gtklib;
function gtk_about_dialog_get_documenters(about:PGtkAboutDialog):PPgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_documenters(about:PGtkAboutDialog; documenters:PPgchar);cdecl;external gtklib;
function gtk_about_dialog_get_artists(about:PGtkAboutDialog):PPgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_artists(about:PGtkAboutDialog; artists:PPgchar);cdecl;external gtklib;
function gtk_about_dialog_get_translator_credits(about:PGtkAboutDialog):Pgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_translator_credits(about:PGtkAboutDialog; translator_credits:Pgchar);cdecl;external gtklib;
function gtk_about_dialog_get_logo(about:PGtkAboutDialog):PGdkPixbuf;cdecl;external gtklib;
procedure gtk_about_dialog_set_logo(about:PGtkAboutDialog; logo:PGdkPixbuf);cdecl;external gtklib;
function gtk_about_dialog_get_logo_icon_name(about:PGtkAboutDialog):Pgchar;cdecl;external gtklib;
procedure gtk_about_dialog_set_logo_icon_name(about:PGtkAboutDialog; icon_name:Pgchar);cdecl;external gtklib;
type
  TGtkAboutDialogActivateLinkFunc = procedure (about:PGtkAboutDialog; link:Pgchar; data:gpointer);cdecl;
function gtk_about_dialog_set_email_hook(func:TGtkAboutDialogActivateLinkFunc; data:gpointer; destroy:TGDestroyNotify):TGtkAboutDialogActivateLinkFunc;cdecl;external gtklib;
function gtk_about_dialog_set_url_hook(func:TGtkAboutDialogActivateLinkFunc; data:gpointer; destroy:TGDestroyNotify):TGtkAboutDialogActivateLinkFunc;cdecl;external gtklib;

// GtkLabel

function gtk_label_get_ellipsize  (_label: pointer): longint; cdecl; external gtklib;
procedure gtk_label_set_ellipsize (_label: pointer; mode: longint); cdecl; external gtklib;

// GtkProgressBar

function gtk_progress_bar_get_ellipsize (pbar: pointer): longint; cdecl; external gtklib;
procedure gtk_progress_bar_set_ellipsize(pbar: pointer; mode: longint); cdecl; external gtklib;

// GtkColorButton (Since 2.4)

function  gtk_color_button_new(): pointer; cdecl; external gtklib;
function  gtk_color_button_new_with_color(color: PGdkColor): pointer; cdecl; external gtklib;
procedure gtk_color_button_set_color(color_button: pointer; color: PGdkColor); cdecl; external gtklib;
procedure gtk_color_button_get_color(color_button: pointer; color: PGdkColor); cdecl; external gtklib;
procedure gtk_color_button_set_alpha(color_button: pointer; alpha: guint16); cdecl; external gtklib;
function  gtk_color_button_get_alpha(color_button: pointer): guint16; cdecl; external gtklib;
procedure gtk_color_button_set_use_alpha(color_button: pointer; use_alpha: gboolean); cdecl; external gtklib;
function  gtk_color_button_get_use_alpha(color_button: pointer): gboolean; cdecl; external gtklib;
procedure gtk_color_button_set_title(color_button: pointer; title: pgchar); cdecl; external gtklib;
function  gtk_color_button_get_title(color_button: pointer): pgchar; cdecl; external gtklib;

// GtkFontButton (Since 2.4)

function gtk_font_button_new: pointer; cdecl; external gtklib;
function gtk_font_button_get_title(font_button: pointer): Pgchar; cdecl; external gtklib;
procedure gtk_font_button_set_title(font_button: pointer; title: Pgchar); cdecl; external gtklib;
function gtk_font_button_get_use_font(font_button: pointer): gboolean; cdecl; external gtklib;
procedure gtk_font_button_set_use_font(font_button: pointer; use_font: gboolean); cdecl; external gtklib;
function gtk_font_button_get_use_size(font_button: pointer): gboolean; cdecl; external gtklib;
procedure gtk_font_button_set_use_size(font_button: pointer; use_size: gboolean); cdecl; external gtklib;
function gtk_font_button_get_font_name(font_button: pointer): Pgchar; cdecl; external gtklib;
function gtk_font_button_set_font_name(font_button: pointer; fontname: Pgchar): gboolean; cdecl; external gtklib;
function gtk_font_button_get_show_style(font_button: pointer): gboolean; cdecl; external gtklib;
procedure gtk_font_button_set_show_style(font_button: pointer; show_style: gboolean); cdecl; external gtklib;
function gtk_font_button_get_show_size(font_button: pointer): gboolean; cdecl; external gtklib;
procedure gtk_font_button_set_show_size(font_button: pointer; show_size: gboolean); cdecl; external gtklib;

// GtkMisc (Wrong declaration)
procedure gtk_misc_get_padding(misc:PGtkMisc; xpad: Pgint; ypad: Pgint); cdecl; external gtklib;
procedure gtk_misc_get_alignment(misc:PGtkMisc; xalign:Pgfloat; yalign:Pgfloat); cdecl; external gtklib;

// ??

function gtk_notebook_get_n_pages(a: pointer): integer; cdecl; external gtklib;


// GtkTreeView (Since 2.2)

procedure gtk_tree_view_expand_to_path(tree_view: pointer; path: PGtkTreePath); cdecl; external gtklib;

// GtkCellLayout (Since 2.4)

(*
void        ( * GtkCellLayoutDataFunc)        (GtkCellLayout *cell_layout,
                                             GtkCellRenderer *cell,
                                             GtkTreeModel *tree_model,
                                             GtkTreeIter *iter,
                                             gpointer data);
*)
procedure gtk_cell_layout_pack_start(cell_layout: pointer; cell: pointer; expand: gboolean); cdecl; external gtklib;
procedure gtk_cell_layout_pack_end(cell_layout: pointer; cell: pointer; expand: gboolean); cdecl; external gtklib;
procedure gtk_cell_layout_reorder(cell_layout: pointer; cell: pointer; position: gint); cdecl; external gtklib;
procedure gtk_cell_layout_clear(cell_layout: pointer); cdecl; external gtklib;
procedure gtk_cell_layout_set_attributes(cell_layout: pointer; cell: pointer; const args: array of const); cdecl; external gtklib;
procedure gtk_cell_layout_add_attribute(cell_layout: pointer; cell: pointer; attribute: PGChar; column: gint); cdecl; external gtklib;
(*
procedure gtk_cell_layout_set_cell_data_func
                                            (cell_layout: pointer,
                                             GtkCellRenderer *cell,
                                             GtkCellLayoutDataFunc func,
                                             gpointer func_data,
                                             GDestroyNotify destroy);
*)
procedure gtk_cell_layout_clear_attributes(cell_layout: pointer; cell: pointer); cdecl; external gtklib;

// GtkComboBoxEntry (Since 2.4)

function gtk_combo_box_entry_new(): pointer; cdecl; external gtklib;
procedure gtk_combo_box_entry_set_text_column(entry_box: pointer; text_column: gint); cdecl; external gtklib;

// GtkComboBox (Since 2.4 w/ some Since 2.6)

function gtk_combo_box_new(): pointer; cdecl; external gtklib;
function gtk_combo_box_new_with_model(model: pointer): pointer; cdecl; external gtklib;
function gtk_combo_box_get_wrap_width(combo_box: pointer): gint; cdecl; external gtklib;
procedure gtk_combo_box_set_wrap_width(combo_box: pointer; width: gint); cdecl; external gtklib;
function gtk_combo_box_get_row_span_column(combo_box: pointer): gint; cdecl; external gtklib;
procedure gtk_combo_box_set_row_span_column(combo_box: pointer; row_span: gint); cdecl; external gtklib;
function gtk_combo_box_get_column_span_column(combo_box: pointer): gint; cdecl; external gtklib;
procedure gtk_combo_box_set_column_span_column(combo_box: pointer; column_span: gint); cdecl; external gtklib;
function gtk_combo_box_get_active(combo_box: pointer): gint; cdecl; external gtklib;
procedure gtk_combo_box_set_active(combo_box: pointer; index_: gint); cdecl; external gtklib;
function gtk_combo_box_get_active_iter(combo_box: pointer; iter: pointer): gboolean; cdecl; external gtklib;
procedure gtk_combo_box_set_active_iter(combo_box: pointer; iter: pointer); cdecl; external gtklib;
function gtk_combo_box_get_model(combo_box: pointer): pointer; cdecl; external gtklib;
procedure gtk_combo_box_set_model(combo_box: pointer; model: pointer); cdecl; external gtklib;
(*
GtkWidget*  gtk_combo_box_new_text          (void);
void        gtk_combo_box_append_text       (combo_box: pointer,const gchar *text);
void        gtk_combo_box_insert_text       (combo_box: pointer,gint position,const gchar *text);
void        gtk_combo_box_prepend_text      (combo_box: pointer,const gchar *text);
void        gtk_combo_box_remove_text       (combo_box: pointer,gint position);
*)
function gtk_combo_box_get_active_text(combo_box: pointer): PGChar; cdecl; external gtklib;

procedure gtk_combo_box_popup(combo_box: pointer); cdecl; external gtklib;
procedure gtk_combo_box_popdown(combo_box: pointer); cdecl; external gtklib;
function gtk_combo_box_get_popup_accessible(combo_box: pointer): pointer; cdecl; external gtklib;
(*
GtkTreeViewRowSeparatorFunc gtk_combo_box_get_row_separator_func(combo_box: pointer);
void        gtk_combo_box_set_row_separator_func(combo_box: pointer,GtkTreeViewRowSeparatorFunc func,gpointer data,GtkDestroyNotify destroy);
*)
procedure gtk_combo_box_set_add_tearoffs(combo_box: pointer; add_tearoffs: gboolean); cdecl; external gtklib;
function gtk_combo_box_get_add_tearoffs(combo_box: pointer): gboolean; cdecl; external gtklib;
procedure gtk_combo_box_set_focus_on_click(combo: pointer; focus_on_click: gboolean); cdecl; external gtklib;
function gtk_combo_box_get_focus_on_click(combo: pointer): gboolean; cdecl; external gtklib;

// GtkListStore (Since 2.2) ?

procedure gtk_list_store_swap(store: pointer; a: pointer; b: pointer); cdecl; external gtklib;
procedure gtk_list_store_move_before(store: pointer; iter: pointer; position: pointer); cdecl; external gtklib;
procedure gtk_list_store_move_after(store: pointer; iter: pointer; position: pointer); cdecl; external gtklib;


// GtkExpander (Since 2.4)

function gtk_expander_new(alabel: PGChar): pointer; cdecl; external gtklib;
function gtk_expander_new_with_mnemonic(alabel:PGChar): pointer; cdecl; external gtklib;
procedure gtk_expander_set_expanded (expander: pointer; aexpanded: gboolean); cdecl; external gtklib;
function gtk_expander_get_expanded (expander: pointer): gboolean; cdecl; external gtklib;
procedure gtk_expander_set_spacing (expander: pointer; aspacing: gint); cdecl; external gtklib;
function gtk_expander_get_spacing (expander: pointer): gint; cdecl; external gtklib;
procedure gtk_expander_set_label (expander: pointer; alabel: PGChar); cdecl; external gtklib;
function gtk_expander_get_label (expander: pointer): PGChar; cdecl; external gtklib;
procedure gtk_expander_set_use_underline (expander: pointer; ause_underline: gboolean); cdecl; external gtklib;
function gtk_expander_get_use_underline (expander: pointer): gboolean; cdecl; external gtklib;
procedure gtk_expander_set_use_markup (expander: pointer; ause_markup: gboolean); cdecl; external gtklib;
function gtk_expander_get_use_markup (expander: pointer): gboolean; cdecl; external gtklib;
procedure gtk_expander_set_label_widget (expander: pointer; alabel_widget: pointer); cdecl; external gtklib;
function gtk_expander_get_label_widget (expander: pointer): pointer; cdecl; external gtklib;

// GtkButton.FocusOnClick (Since 2.4)

procedure gtk_button_set_focus_on_click (button: pointer; afocusonclick: gboolean); cdecl; external gtklib;
function gtk_button_get_focus_on_click (button: pointer): gboolean; cdecl; external gtklib;

// GtkFileChooser (Since 2.4)
function gtk_file_chooser_dialog_new (atitle: PGChar; aparent: pointer; aaction: integer; afirst_button_text: pgchar; const args: array of const): pointer; cdecl; external gtklib;
function gtk_file_chooser_dialog_new (atitle: PGChar; aparent: pointer; aaction: integer; afirst_button_text: pgchar): pointer; cdecl; external gtklib;
function gtk_file_chooser_widget_new (aaction: integer): pointer; cdecl; external gtklib;

procedure gtk_file_chooser_set_action (achooser: pointer; aaction: longint); cdecl; external gtklib;
function gtk_file_chooser_get_action (achooser: pointer): longint; cdecl; external gtklib;
procedure gtk_file_chooser_set_local_only (achooser: pointer; alocal_only: gboolean); cdecl; external gtklib;
function gtk_file_chooser_get_local_only (achooser: pointer): gboolean; cdecl; external gtklib;
procedure gtk_file_chooser_set_select_multiple (achooser: pointer; aselect_multiple: gboolean); cdecl; external gtklib;
function gtk_file_chooser_get_select_multiple (achooser: pointer): gboolean; cdecl; external gtklib;
function gtk_file_chooser_set_current_folder (achooser: pointer; afilename: PGChar): gboolean; cdecl; external gtklib;
function gtk_file_chooser_get_current_folder (achooser: pointer): PGChar; cdecl; external gtklib;
function gtk_file_chooser_get_uri (achooser: pointer): PGChar; cdecl; external gtklib;
function gtk_file_chooser_set_uri (achooser: pointer; auri: PChar): gboolean; cdecl; external gtklib;
function gtk_file_chooser_set_current_folder_uri(achooser: pointer; auri: PGChar): gboolean; cdecl; external gtklib;
function gtk_file_chooser_get_current_folder_uri(achooser: pointer): PGChar; cdecl; external gtklib;
function gtk_file_chooser_get_filename (achooser: pointer): PGChar; cdecl; external gtklib;
function gtk_file_chooser_set_filename (achooser: pointer;  afilename: PChar): gboolean; cdecl; external gtklib;
// GtkFileChooser (Since 2.6)
procedure  gtk_file_chooser_set_show_hidden (achooser: pointer; ashow_hidden: gboolean); cdecl; external gtklib;
function gtk_file_chooser_get_show_hidden (achooser: pointer): gboolean; cdecl; external gtklib;


// GtkToolBar
procedure gtk_toolbar_insert(atoolbar: pointer; aitem: pointer; apos: gint); cdecl; external gtklib;
procedure gtk_toolbar_set_show_arrow(atoolbar: pointer; ashow_arrow: gboolean); cdecl; external gtklib;
function gtk_toolbar_get_show_arrow(atoolbar: pointer): gboolean; cdecl; external gtklib;

// GtkToolItem (Since 2.4)
function gtk_tool_item_new: pointer; cdecl; external gtklib;
procedure gtk_tool_item_set_homogeneous(atool_item: pointer; ahomogeneous: gboolean); cdecl; external gtklib;
function gtk_tool_item_get_homogeneous(atool_item: pointer): gboolean; cdecl; external gtklib;
procedure gtk_tool_item_set_expand(atool_item: pointer; aexpand: gboolean); cdecl; external gtklib;
function gtk_tool_item_get_expand(atool_item: pointer): gboolean; cdecl; external gtklib;
procedure gtk_tool_item_set_visible_horizontal(atool_item: pointer; avisible_horizontal: gboolean); cdecl; external gtklib;
function gtk_tool_item_get_visible_horizontal(atool_item: pointer): gboolean; cdecl; external gtklib;
procedure gtk_tool_item_set_visible_vertical(atool_item: pointer; avisible_vertical: gboolean); cdecl; external gtklib;
function gtk_tool_item_get_visible_vertical(atool_item: pointer): gboolean; cdecl; external gtklib;
procedure gtk_tool_item_set_is_important(atool_item: pointer; ais_important: gboolean); cdecl; external gtklib;
function gtk_tool_item_get_is_important(atool_item: pointer): gboolean; cdecl; external gtklib;
function gtk_tool_item_get_orientation(atool_item: pointer): longint; cdecl; external gtklib;
function gtk_tool_item_get_toolbar_style(atool_item: pointer): longint; cdecl; external gtklib;

//GtkSeparatorToolItem (Since 2.4)
function gtk_separator_tool_item_new: pointer; cdecl; external gtklib;
procedure gtk_separator_tool_item_set_draw(aitem: pointer; adraw: gboolean); cdecl; external gtklib;
function gtk_separator_tool_item_get_draw(aitem: pointer): gboolean; cdecl; external gtklib;

//GtkToolButton (Since 2.4)
function gtk_tool_button_new(aicon_widget: pointer; const alabel: PGChar): pointer; cdecl; external gtklib;
procedure gtk_tool_button_set_label(abutton: pointer; const alabel: PGChar); cdecl; external gtklib;
function gtk_tool_button_get_label(abutton: pointer): PGChar; cdecl; external gtklib;
procedure gtk_tool_button_set_use_underline(abutton: pointer; ause_underline: gboolean); cdecl; external gtklib;
function gtk_tool_button_get_use_underline(abutton: pointer): gboolean; cdecl; external gtklib;
procedure gtk_tool_button_set_stock_id(abutton: pointer; const astock_id: PGChar); cdecl; external gtklib;
function gtk_tool_button_get_stock_id(abutton: pointer): PGChar; cdecl; external gtklib;


//================================== bug in FPC's gtktreestore.inc

function gtk_tree_store_iter_is_valid(tree_store:PGtkTreeStore; iter:PGtkTreeIter):gboolean; cdecl; external gtklib;
procedure gtk_tree_store_reorder(tree_store:PGtkTreeStore; parent:PGtkTreeIter; new_order:pgint); cdecl; external gtklib;
procedure gtk_tree_store_swap(tree_store:PGtkTreeStore; a:PGtkTreeIter; b:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_store_move_before(tree_store:PGtkTreeStore; iter:PGtkTreeIter; position:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_store_move_after(tree_store:PGtkTreeStore; iter:PGtkTreeIter; position:PGtkTreeIter); cdecl; external gtklib;
