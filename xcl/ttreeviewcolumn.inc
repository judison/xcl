(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{** INTERFACE *************************************}{$IFDEF INC_READ_INTERFACE}

  TTreeViewColumns = class(TCollection)
  private
    FOwner: TTreeView;
    function GetItem(AIdx: Integer): TTreeViewColumn;
    procedure SetItem(AIdx: Integer; AValue: TTreeViewColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TTreeView);
    destructor Destroy; override;
    //--
    function Add: TTreeViewColumn;
    property Items[Idx: Integer]: TTreeViewColumn read GetItem write SetItem; default;
  published
  end;

  TTreeViewColumn = class(TCollectionItem)
  private
    FTreeView: TTreeView;
    FTextColumn: Integer;
    //-- ColumnOrder property
    function GetColumnOrder: Integer;
    procedure SetColumnOrder(AValue: Integer);
    function IsColumnOrderStored: Boolean;
    //-- ExpanderColumn Property
    function GetExpanderColumn: Boolean;
    procedure SetExpanderColumn(AValue: Boolean);
    function IsExpanderColumnStored: Boolean;
    //-- Properties Gets'n'Sets
    function GetClickable: Boolean;
    procedure SetClickable(AValue: Boolean);
    //function GetExpand: Boolean;
    //procedure SetExpand(AValue: Boolean);
    function GetFixedWidth: Integer;
    procedure SetFixedWidth(AValue: Integer);
    function GetMaxWidth: Integer;
    procedure SetMaxWidth(AValue: Integer);
    function GetMinWidth: Integer;
    procedure SetMinWidth(AValue: Integer);
    function GetReorderable: Boolean;
    procedure SetReorderable(AValue: Boolean);
    function GetResizable: Boolean;
    procedure SetResizable(AValue: Boolean);
    function GetSizing: TTreeViewColumnSizing;
    procedure SetSizing(AValue: TTreeViewColumnSizing);
    function GetSpacing: Integer;
    procedure SetSpacing(AValue: Integer);
    function GetTitle: String;
    procedure SetTitle(AValue: String);
    procedure SetTextColumn(AValue: Integer);
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function GetWidth: Integer;
    //-- Event Sets
    //procedure SetOnClicked(AValue: TNotifyEvent);
  protected
    Handle: Pointer;
    FRenderer: Pointer;
    //-- TCollectionItem
    function GetDisplayName: String; override;
    procedure SetIndex(AValue: Integer); override;
    //==
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    //--
    property Width: Integer read GetWidth;
  published
    property ColumnOrder: Integer read GetColumnOrder write SetColumnOrder stored IsColumnOrderStored;
    property Clickable: Boolean read GetClickable write SetClickable default True;
    //property Expand: Boolean read GetExpand write SetExpand default False;
    property ExpanderColumn: Boolean read GetExpanderColumn write SetExpanderColumn stored IsExpanderColumnStored;
    property FixedWidth: Integer read GetFixedWidth write SetFixedWidth default 0;
    property MaxWidth: Integer read GetMaxWidth write SetMaxWidth default -1;
    property MinWidth: Integer read GetMinWidth write SetMinWidth default -1;
    property Reorderable: Boolean read GetReorderable write SetReorderable default False;
    property Resizable: Boolean read GetResizable write SetResizable default False;
    property Sizing: TTreeViewColumnSizing read GetSizing write SetSizing default tvcsGrowOnly;
    property Spacing: Integer read GetSpacing write SetSpacing;
    property TextColumn: Integer read FTextColumn write SetTextColumn stored True;
    property Title: String read GetTitle write SetTitle;
    property Visible: Boolean read GetVisible write SetVisible default True;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TTreeViewColumns }

constructor TTreeViewColumns.Create(AOwner: TTreeView);
begin
  inherited Create(TTreeViewColumn);
  FOwner := AOwner;
end;

destructor TTreeViewColumns.Destroy;
begin
  inherited;
end;

function TTreeViewColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TTreeViewColumns.GetItem(AIdx: Integer): TTreeViewColumn;
begin
  Result := TTreeViewColumn(inherited GetItem(AIdx));
end;

procedure TTreeViewColumns.SetItem(AIdx: Integer; AValue: TTreeViewColumn);
begin
  inherited SetItem(AIdx, AValue);
end;

function TTreeViewColumns.Add: TTreeViewColumn;
var
  New: TTreeViewColumn;
begin
  New := TTreeViewColumn.Create(Self);
  Result := New;
end;

{ TTreeViewColumn }

constructor TTreeViewColumn.Create(ACollection: TCollection);
begin
  inherited;
  FTextColumn := 0;
  //==
  FTreeView := TTreeViewColumns(ACollection).FOwner;
  CreateHandle;
  gtk_tree_view_append_column(FTreeView.Handle, Handle);
  g_object_ref(Handle);
end;

destructor TTreeViewColumn.Destroy;
begin
  DestroyHandle;
  inherited;
end;

procedure TTreeViewColumn.CreateHandle;
begin
  FRenderer := gtk_cell_renderer_text_new();
  g_object_ref(FRenderer);
  //--
//  Handle := gtk_tree_view_column_new();
//  gtk_tree_view_column_set_attributes(Handle, FRenderer, ['text', FTextColumn, nil]);
  Handle := gtk_tree_view_column_new_with_attributes('', FRenderer, ['text', FTextColumn, nil]);
end;

procedure TTreeViewColumn.DestroyHandle;
begin
  gtk_tree_view_remove_column(FTreeView.Handle, Handle);
  g_object_unref(Handle);
  g_object_unref(FRenderer);
end;

function TTreeViewColumn.GetDisplayName: string;
begin
  Result := Title;
end;

procedure TTreeViewColumn.SetIndex(AValue: Integer);
begin
  if Index = ColumnOrder then
    ColumnOrder := AValue;
  inherited SetIndex(AValue);
end;

function TTreeViewColumn.GetColumnOrder: Integer;
var
  List: Pointer;
begin
  List := gtk_tree_view_get_columns(FTreeView.Handle);
  try
    Result := g_list_index(List, Handle);
  finally
    g_list_free(List);
  end;
end;

procedure TTreeViewColumn.SetColumnOrder(AValue: Integer);
var
  BefCol: Pointer; // pointer to column before
begin
  if AValue = 0 then
    BefCol := nil
  else
    BefCol := gtk_tree_view_get_column(FTreeView.Handle, AValue-1);
  gtk_tree_view_move_column_after(FTreeView.Handle, Handle, BefCol);
end;

function TTreeViewColumn.IsColumnOrderStored: Boolean;
begin
  Result := ColumnOrder <> Index;
end;

function TTreeViewColumn.GetClickable: Boolean;
begin
  Result := gtk_tree_view_column_get_clickable(Handle);
end;

function TTreeViewColumn.GetExpanderColumn: Boolean;
begin
  Result := gtk_tree_view_get_expander_column(FTreeView.Handle) = Handle;
end;

procedure TTreeViewColumn.SetExpanderColumn(AValue: Boolean);
begin
  if AValue then
    gtk_tree_view_set_expander_column(FTreeView.Handle, Handle)
  else
    gtk_tree_view_set_expander_column(FTreeView.Handle, nil);
end;

function TTreeViewColumn.IsExpanderColumnStored: Boolean;
begin
  Result := ExpanderColumn and (ColumnOrder <> 0);
end;

procedure TTreeViewColumn.SetClickable(AValue: Boolean);
begin
  gtk_tree_view_column_set_clickable(Handle, AValue);
end;

//function TTreeViewColumn.GetExpand: Boolean;
//begin
//  Result := gtk_tree_view_column_get_expand(Handle);
//end;

//procedure TTreeViewColumn.SetExpand(AValue: Boolean);
//begin
//  gtk_tree_view_column_set_expand(Handle, AValue);
//end;

function TTreeViewColumn.GetFixedWidth: Integer;
begin
  Result := gtk_tree_view_column_get_fixed_width(Handle);
end;

procedure TTreeViewColumn.SetFixedWidth(AValue: Integer);
begin
  gtk_tree_view_column_set_fixed_width(Handle, AValue);
end;

function TTreeViewColumn.GetMaxWidth: Integer;
begin
  Result := gtk_tree_view_column_get_max_width(Handle);
end;

procedure TTreeViewColumn.SetMaxWidth(AValue: Integer);
begin
  gtk_tree_view_column_set_max_width(Handle, AValue);
end;

function TTreeViewColumn.GetMinWidth: Integer;
begin
  Result := gtk_tree_view_column_get_min_width(Handle);
end;

procedure TTreeViewColumn.SetMinWidth(AValue: Integer);
begin
  gtk_tree_view_column_set_min_width(Handle, AValue);
end;

function TTreeViewColumn.GetReorderable: Boolean;
begin
  Result := gtk_tree_view_column_get_reorderable(Handle);
end;

procedure TTreeViewColumn.SetReorderable(AValue: Boolean);
begin
  gtk_tree_view_column_set_reorderable(Handle, AValue);
end;

function TTreeViewColumn.GetResizable: Boolean;
begin
  Result := gtk_tree_view_column_get_resizable(Handle);
end;

procedure TTreeViewColumn.SetResizable(AValue: Boolean);
begin
  gtk_tree_view_column_set_resizable(Handle, AValue);
end;

function TTreeViewColumn.GetSizing: TTreeViewColumnSizing;
begin
  Result := TTreeViewColumnSizing(gtk_tree_view_column_get_sizing(Handle));
end;

procedure TTreeViewColumn.SetSizing(AValue: TTreeViewColumnSizing);
begin
  gtk_tree_view_column_set_sizing(Handle, TGtkTreeViewColumnSizing(AValue));
end;

function TTreeViewColumn.GetSpacing: Integer;
begin
  Result := gtk_tree_view_column_get_spacing(Handle);
end;

procedure TTreeViewColumn.SetSpacing(AValue: Integer);
begin
  gtk_tree_view_column_set_spacing(Handle, AValue);
end;

function TTreeViewColumn.GetTitle: String;
begin
  Result := gtk_tree_view_column_get_title(Handle);
end;

procedure TTreeViewColumn.SetTitle(AValue: String);
begin
  gtk_tree_view_column_set_title(Handle, PChar(AValue));
  Changed(True);
end;

procedure TTreeViewColumn.SetTextColumn(AValue: Integer);
begin
  if AValue <> FTextColumn then
  begin
    FTextColumn := AValue;
    gtk_tree_view_column_set_attributes(Handle, FRenderer, ['text', FTextColumn, nil]);
  end;
end;

function TTreeViewColumn.GetVisible: Boolean;
begin
  Result := gtk_tree_view_column_get_visible(Handle);
end;

procedure TTreeViewColumn.SetVisible(AValue: Boolean);
begin
  gtk_tree_view_column_set_visible(Handle, AValue);
end;

function TTreeViewColumn.GetWidth: Integer;
begin
  Result := gtk_tree_view_column_get_width(Handle);
end;

{$ENDIF}
