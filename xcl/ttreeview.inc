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

  TTreeView = class(TControl)
  private
    FModel: TTreeModel;
    FColumns: TTreeViewColumns;
    FOnSelectionChanged: TNotifyEvent;
    FOnRowActivated: TRowActivatedEvent;
    procedure SetColumns(AValue: TTreeViewColumns);
    function GetHeadersVisible: Boolean;
    procedure SetHeadersVisible(AValue: Boolean);
    function GetRulesHint: Boolean;
    procedure SetRulesHint(AValue: Boolean);
    //== Model
    procedure SetModel(AValue: TTreeModel);
    //== Selection
    function GetSelectionMode: TSelectionMode;
    procedure SetSelectionMode(AValue: TSelectionMode);
  protected
    FSelection: Pointer;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure ConnectSignals; override;
    procedure DoChanged(var Msg: TGtkSig); message 'GTK:changed';
    procedure DoRowActivated(var Msg: TGtkSig); message 'GTK:row-activated';
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ColumnsAutosize;
    procedure ExpandAll;
    procedure CollapseAll;
    procedure ExpandTo(const AIter: TTreeIter);
    //== Selection
    procedure SelectIter(const AIter: TTreeIter);
    function IterIsSelected(const AIter: TTreeIter): Boolean;
    function GetSelected(var AIter: TTreeIter): Boolean;
  published
    property Columns: TTreeViewColumns read FColumns write SetColumns;
    property HeadersVisible: Boolean read GetHeadersVisible write SetHeadersVisible default True;
    property RulesHint: Boolean read GetRulesHint write SetRulesHint default False;
    property SelectionMode: TSelectionMode read GetSelectionMode write SetSelectionMode;
    property Model: TTreeModel read FModel write SetModel;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnRowActivated: TRowActivatedEvent read FOnRowActivated write FOnRowActivated;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TTreeView }

constructor TTreeView.Create(AOwner: TComponent); 
begin
  inherited;
  FModel := nil;
  FColumns := TTreeViewColumns.Create(Self);
end;

destructor TTreeView.Destroy;
begin
  FColumns.Free;
  inherited;
end;

procedure TTreeView.CreateHandle;
begin
  Handle := gtk_tree_view_new();
  FSelection := gtk_tree_view_get_selection(Handle);
  g_object_ref(FSelection);
  g_object_set_qdata(FSelection, QTObject, Self);
end;

procedure TTreeView.DestroyHandle;
begin
  g_object_unref(FSelection);
  inherited;
end;

procedure TTreeView.ConnectSignals;
begin
  inherited;
  XCL.ConnectGtkSignal(FSelection, 'changed');
  XCL.ConnectGtkSignal(Handle, 'row-activated');
end;

procedure TTreeView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FModel) and (Operation = opRemove) then
  begin
    FModel := nil;
    gtk_tree_view_set_model(Handle, nil);
  end;
  inherited;
end;

procedure TTreeView.SetColumns(AValue: TTreeViewColumns);
begin
  FColumns.Assign(AValue);
end;

function TTreeView.GetHeadersVisible: Boolean;
begin
  Result := gtk_tree_view_get_headers_visible(Handle);
end;

procedure TTreeView.SetHeadersVisible(AValue: Boolean);
begin
  gtk_tree_view_set_headers_visible(Handle, AValue);
end;

function TTreeView.GetRulesHint: Boolean;
begin
  Result := gtk_tree_view_get_rules_hint(Handle);
end;

procedure TTreeView.SetRulesHint(AValue: Boolean);
begin
  gtk_tree_view_set_rules_hint(Handle, AValue);
end;

function TTreeView.GetSelectionMode: TSelectionMode;
begin
  Result := TSelectionMode(gtk_tree_selection_get_mode(FSelection));
end;

procedure TTreeView.SetSelectionMode(AValue: TSelectionMode);
begin
  gtk_tree_selection_set_mode(FSelection, Ord(AValue));
end;

procedure TTreeView.SelectIter(const AIter: TTreeIter);
begin
  gtk_tree_selection_select_iter(FSelection, @AIter);
end;

function TTreeView.IterIsSelected(const AIter: TTreeIter): Boolean;
begin
  Result := gtk_tree_selection_iter_is_selected(FSelection, @AIter);
end;

function TTreeView.GetSelected(var AIter: TTreeIter): Boolean;
begin
  Result := gtk_tree_selection_get_selected(FSelection, nil, @AIter);
end;

procedure TTreeView.ColumnsAutosize;
begin
  gtk_tree_view_columns_autosize(Handle);
end;

procedure TTreeView.ExpandAll;
begin
  gtk_tree_view_expand_all(Handle);
end;

procedure TTreeView.CollapseAll;
begin
  gtk_tree_view_collapse_all(Handle);
end;

procedure TTreeView.ExpandTo(const AIter: TTreeIter);
var
  P: PGtkTreePath;
begin
  P := gtk_tree_model_get_path(FModel.Handle, @AIter);
  gtk_tree_view_expand_to_path(Handle, P);
  gtk_tree_path_free(P);
end;

procedure TTreeView.SetModel(AValue: TTreeModel);
begin
  if AValue <> FModel then
  begin
    if Assigned(FModel) then
      FModel.RemoveFreeNotification(Self);
    FModel := AValue;
    if Assigned(FModel) then
    begin
      gtk_tree_view_set_model(Handle, FModel.Handle);
      FModel.FreeNotification(Self);
    end
    else
      gtk_tree_view_set_model(Handle, nil);
  end;
end;

procedure TTreeView.DoChanged(var Msg: TGtkSig);
begin
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

// P1: path: PGtkTreePath
// P2: column: PGtkTreeViewColumn
procedure TTreeView.DoRowActivated(var Msg: TGtkSig);
var
  Iter: TTreeIter;
  Column: TObject;
begin
  if Assigned(FModel) and Assigned(FOnRowActivated) then
    if gtk_tree_model_get_iter(FModel.Handle, @Iter, Msg.P1) then
    begin
      Column := ObjectByHandle(Msg.P2);
      if Column is TTreeViewColumn then
        FOnRowActivated(Self, Iter, TTreeViewColumn(Column))
      else
        FOnRowActivated(Self, Iter, nil);
    end;
end;

{$ENDIF}
