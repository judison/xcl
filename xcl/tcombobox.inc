(*
   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{$IFDEF INC_READ_INTERFACE}
//** INTERFACE
//****************************************************************************

  TComboBox = class(TControl)
  private
    {$DEFINE INTF_PRIVATE}{$I icelllayout.inc}{$UNDEF INTF_PRIVATE}
    FRenderer: Pointer;
    FOnChanged: TNotifyEvent;
    //== Properties
    function GetWrapWidth: Integer;
    procedure SetWrapWidth(AValue: Integer);
    function GetRowSpanColumn: Integer;
    procedure SetRowSpanColumn(AValue: Integer);
    function GetColumnSpanColumn: Integer;
    procedure SetColumnSpanColumn(AValue: Integer);
    function GetActive: Integer;
    procedure SetActive(AValue: Integer);
    function GetHasFrame: Boolean;
    procedure SetHasFrame(AValue: Boolean);
  protected
    {$DEFINE INTF_PROTECTED}{$I icelllayout.inc}{$UNDEF INTF_PROTECTED}
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure ConnectSignals; override;
    procedure DoChanged(var Msg: TGtkSig); message 'GTK:changed';
    //== Model
    FModel: TTreeModel;
    procedure SetModel(AValue: TTreeModel); virtual;
    //== TextColumn
    FTextColumn: Integer;
    procedure SetTextColumn(AValue: Integer); virtual;
    property HasFrame: Boolean read GetHasFrame write SetHasFrame default True;
  public
    {$DEFINE INTF_PUBLIC}{$I icelllayout.inc}{$UNDEF INTF_PUBLIC}
    constructor Create(AOwner: TComponent); override;
    //--
    procedure SetActiveIter(const AIter: TTreeIter);
    function GetActiveIter(var AIter: TTreeIter): Boolean;
    //--
    procedure Popup;
    procedure Popdown;
    //== Properties
    property Active: Integer read GetActive write SetActive;
  published
    {$DEFINE INTF_PUBLISHED}{$I icelllayout.inc}{$UNDEF INTF_PUBLISHED}
    property Model: TTreeModel read FModel write SetModel;
    property WrapWidth: Integer read GetWrapWidth write SetWrapWidth default 0;
    property RowSpanColumn: Integer read GetRowSpanColumn write SetRowSpanColumn default -1;
    property ColumnSpanColumn: Integer read GetColumnSpanColumn write SetColumnSpanColumn default -1;
    property TextColumn: Integer read FTextColumn write SetTextColumn default 0;
    //==
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TComboBoxEntry = class(TComboBox)
  private
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure SetTextColumn(AValue: Integer); override;
    function GetActiveText: String;
    procedure SetModel(AValue: TTreeModel); override;
  public
    property ActiveText: String read GetActiveText;
  published
    property HasFrame;
  end;


{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TComboBox }

{$DEFINE INTF_IMPLEMENTATION}
  {$define the_class := TComboBox }
    {$I icelllayout.inc}
  {$undef the_class}
{$UNDEF INTF_IMPLEMENTATION}

constructor TComboBox.Create(AOwner: TComponent);
begin
  FModel := nil;
  FTextColumn := 0;
  inherited;
end;

procedure TComboBox.CreateHandle;
begin
  Handle := gtk_combo_box_new;
  FRenderer := gtk_cell_renderer_text_new();
  g_object_ref(FRenderer);
  CellLayoutPackStart(FRenderer, True);
  CellLayoutAddAttribute(FRenderer, 'text', FTextColumn);
end;

procedure TComboBox.DestroyHandle;
begin
  if FRenderer <> nil then
    g_object_unref(FRenderer);
  inherited;
end;

procedure TComboBox.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('changed');
end;

procedure TComboBox.DoChanged(var Msg: TGtkSig);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TComboBox.SetModel(AValue: TTreeModel);
begin
  if AValue <> FModel then
  begin
    FModel := AValue;
    gtk_combo_box_set_model(Handle, FModel.Handle);
  end;
end;

function TComboBox.GetWrapWidth: Integer;
begin
  Result := gtk_combo_box_get_wrap_width(Handle);
end;

procedure TComboBox.SetWrapWidth(AValue: Integer);
begin
  gtk_combo_box_set_wrap_width(Handle, AValue);
end;

function TComboBox.GetRowSpanColumn: Integer;
begin
  Result := gtk_combo_box_get_row_span_column(Handle);
end;

procedure TComboBox.SetRowSpanColumn(AValue: Integer);
begin
  gtk_combo_box_set_row_span_column(Handle, AValue);
end;

function TComboBox.GetColumnSpanColumn: Integer;
begin
  Result := gtk_combo_box_get_column_span_column(Handle);
end;

procedure TComboBox.SetColumnSpanColumn(AValue: Integer);
begin
  gtk_combo_box_set_column_span_column(Handle, AValue);
end;

function TComboBox.GetActive: Integer;
begin
  Result := gtk_combo_box_get_active(Handle);
end;

procedure TComboBox.SetActive(AValue: Integer);
begin
  gtk_combo_box_set_active(Handle, AValue);
end;

function TComboBox.GetHasFrame: Boolean;
begin
  //Result := gtk_combo_box_get_has_frame(Handle);
  {$WARNINGS OFF}
  g_object_get(Handle, 'has-frame', [@Result, 0]);
  {$WARNINGS ON}
end;

procedure TComboBox.SetHasFrame(AValue: Boolean);
begin
  //gtk_combo_box_set_has_frame(Handle, AValue);
  {$WARNINGS OFF}
  g_object_set(Handle, 'has-frame', [AValue, 0]);
  {$WARNINGS ON}
end;


procedure TComboBox.SetActiveIter(const AIter: TTreeIter);
begin
  gtk_combo_box_set_active_iter(Handle, @AIter);
end;

function TComboBox.GetActiveIter(var AIter: TTreeIter): Boolean;
begin
  Result := gtk_combo_box_get_active_iter(Handle, @AIter);
end;

procedure TComboBox.Popup;
begin
  gtk_combo_box_popup(Handle);
end;

procedure TComboBox.Popdown;
begin
  gtk_combo_box_popdown(Handle);
end;

procedure TComboBox.SetTextColumn(AValue: Integer);
begin
  if AValue <> FTextColumn then
  begin
    FTextColumn := AValue;
    CellLayoutAddAttribute(FRenderer, 'text', FTextColumn);
  end;
end;

{ TComboBoxEntry }

procedure TComboBoxEntry.CreateHandle;
begin
  Handle := gtk_combo_box_entry_new();
  FRenderer := nil;
end;

procedure TComboBoxEntry.DestroyHandle;
begin
  inherited;
end;

procedure TComboBoxEntry.SetTextColumn(AValue: Integer);
begin
  inherited;
  if FModel <> nil then
    gtk_combo_box_entry_set_text_column(Handle, FTextColumn);
end;

procedure TComboBoxEntry.SetModel(AValue: TTreeModel);
begin
  inherited;
  if FModel <> nil then
    gtk_combo_box_entry_set_text_column(Handle, FTextColumn);
end;

function TComboBoxEntry.GetActiveText: String;
var
  R: PChar;
begin
  R := gtk_combo_box_get_active_text(Handle);
  Result := R;
  g_free(R);
end;

{$ENDIF}
