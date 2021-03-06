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

  TControl = class(TComponent)
  private
    FParent: TControl;
    FFixedX, FFixedY: Integer; // remover?
    FPopupMenu: TMenu;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnFocusIn: TNotifyEvent;
    FOnFocusOut: TNotifyEvent;
    //***********************
    function GetFontDesc: string;
    procedure SetFontDesc(AValue: string);
    function IsFontDescStored: Boolean;
    //
    function GetToolTip: string;
    procedure SetToolTip(AValue: string);
    //--
    procedure SetPopupMenu(AValue: TMenu);
    //--

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function GetSensitive: Boolean;
    procedure SetSensitive(AValue: Boolean);

    function GetState: TWidgetState;

    function GetSizeRequest(AIdx: Integer): Integer;
    procedure SetSizeRequest(AIdx: Integer; AValue: Integer);
    // for TFixed Children
    procedure SetFixedX(AValue: Integer);
    procedure SetFixedY(AValue: Integer);
    function IsFixedXYStored: Boolean;
    // for TBox Children
    function GetBoxExpand: Boolean;
    function GetBoxFill: Boolean;
    procedure SetBoxExpand(AValue: Boolean);
    procedure SetBoxFill(AValue: Boolean);
    function GetBoxPadding: Cardinal;
    procedure SetBoxPadding(AValue: Cardinal);
    function GetBoxPosition: Integer;
    procedure SetBoxPosition(AValue: Integer);
    // for TPaned Children
    function GetPanedResize: Boolean;
    function GetPanedShrink: Boolean;
    procedure SetPanedResize(AValue: Boolean);
    procedure SetPanedShrink(AValue: Boolean);
    function IsPanedResizeStored: Boolean;
    function IsPanedShrinkStored: Boolean;
    // for TTable Children
    function GetTableBottomAttach: Word;
    procedure SetTableBottomAttach(AValue: Word);
    function GetTableLeftAttach: Word;
    procedure SetTableLeftAttach(AValue: Word);
    function GetTableRightAttach: Word;
    procedure SetTableRightAttach(AValue: Word);
    function GetTableTopAttach: Word;
    procedure SetTableTopAttach(AValue: Word);
    function GetTableXPadding: Word;
    procedure SetTableXPadding(AValue: Word);
    function GetTableYPadding: Word;
    procedure SetTableYPadding(AValue: Word);
    function IsTableAttachStored: Boolean;
    function GetTableXOptions: TAttachOptions;
    procedure SetTableXOptions(AValue: TAttachOptions);
    function GetTableYOptions: TAttachOptions;
    procedure SetTableYOptions(AValue: TAttachOptions);
  protected
    // moved here....
    Handle: Pointer;
    // Properties
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
    //
    procedure SetParent(ANewParent: TControl); virtual;
    //
    procedure AddControl(AControl: TControl); virtual;
    procedure RemoveControl(AControl: TControl); virtual;
    function GetControlByIndex(AIndex: Integer): TControl; virtual;
    function GetControlCount: Integer; virtual;
    //
    procedure SetName(const Value: TComponentName); override;
    procedure ReadState(Reader: TReader); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    //
    procedure ShowCtrls; virtual;
    procedure HideCtrls; virtual;
    procedure ShowMe; virtual;
    procedure HideMe; virtual;
    //
    procedure ConnectSignals; virtual;
    //
    procedure ConnectGtkSignal(const AName: String);
    procedure DisconnectGtkSignal(const AName: String);
    // Aux das property
    function IsBoxExpandStored: Boolean; virtual;
    // Signals DO's
    procedure DoPopupMenu(var Msg: TGtkSig); message 'GTK:popup-menu'; virtual;
    procedure DoShow(var Msg: TGtkSig); message 'GTK:show'; virtual;
    procedure DoHide(var Msg: TGtkSig); message 'GTK:hide'; virtual;
    procedure DoRealize(var Msg: TGtkSig); message 'GTK:realize'; virtual;
    procedure DoUnrealize(var Msg: TGtkSig); message 'GTK:unrealize'; virtual;
    procedure DoExpose(var Msg: TGtkSigEvent); message 'GTK:button-press-event'; virtual;
    procedure DoButtonPress(var Msg: TGtkSigEvent); message 'GTK:expose-event'; virtual;
    procedure DoFocusIn(var Msg: TGtkSigEvent); message 'GTK:focus-in-event'; virtual;
    procedure DoFocusOut(var Msg: TGtkSigEvent); message 'GTK:focus-out-event'; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    //--
    function GetHandle: Pointer;
    //
    function HasParent: Boolean; override;
    //-
    function GetForm: TForm;
    //--
    procedure GrabFocus;
    function IsFocus: Boolean;
    //--
    procedure Show; virtual;
    procedure Hide; virtual;
    //==
    procedure ShowPopupMenu(AButton: Integer = 0);
    //==
    procedure SendMessage(var AMsg: TXCLMsg); inline;
    procedure SendMessage(AName: String);
    //==
    function DesignShowProp(AName: ShortString): Boolean; virtual;
    // Properties
    property Controls[index: Integer]: TControl read GetControlByIndex;
    property ControlCount: Integer read GetControlCount;
    property Parent: TControl read FParent write SetParent;
    property State: TWidgetState read GetState;
    //====
    property FontDesc: string read GetFontDesc write SetFontDesc stored IsFontDescStored;
  published
    property PopupMenu: TMenu read FPopupMenu write SetPopupMenu;
    property ToolTip: String read GetToolTip write SetToolTip;
    property Sensitive: Boolean read GetSensitive write SetSensitive default True;
    property Visible: Boolean read GetVisible write SetVisible default True;
    property WidthRequest: Integer index 0 read GetSizeRequest write SetSizeRequest default -1;
    property HeightRequest: Integer index 1 read GetSizeRequest write SetSizeRequest default -1;
    //** TFixed Children **************
    property FixedX: Integer read FFixedX write SetFixedX stored IsFixedXYStored;
    property FixedY: Integer read FFixedY write SetFixedY stored IsFixedXYStored;
    //** TBox Children ****************
    property BoxExpand: Boolean read GetBoxExpand write SetBoxExpand stored IsBoxExpandStored;
    property BoxFill: Boolean read GetBoxFill write SetBoxFill default True;
    property BoxPadding: Cardinal read GetBoxPadding write SetBoxPadding default 0;
    property BoxPosition: Integer read GetBoxPosition write SetBoxPosition stored False;
    //** TPaned Children **************
    property PanedResize: Boolean read GetPanedResize write SetPanedResize stored IsPanedResizeStored;
    property PanedShrink: Boolean read GetPanedShrink write SetPanedShrink stored IsPanedShrinkStored;
    //** TTable Children **************
    property TableBottomAttach: Word read GetTableLeftAttach write SetTableBottomAttach stored IsTableAttachStored;
    property TableLeftAttach: Word read GetTableLeftAttach write SetTableLeftAttach stored IsTableAttachStored;
    property TableRightAttach: Word read GetTableRightAttach write SetTableRightAttach stored IsTableAttachStored;
    property TableTopAttach: Word read GetTableTopAttach write SetTableTopAttach stored IsTableAttachStored;
    property TableXPadding: Word read GetTableXPadding write SetTableXPadding default 0;
    property TableYPadding: Word read GetTableYPadding write SetTableYPadding default 0;
    property TableXOptions: TAttachOptions read GetTableXOptions write SetTableXOptions; //default
    property TableYOptions: TAttachOptions read GetTableYOptions write SetTableYOptions; //default
    //** Events ***********************
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnFocusIn: TNotifyEvent read FOnFocusIn write FOnFocusIn;
    property OnFocusOut: TNotifyEvent read FOnFocusOut write FOnFocusOut;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

function ObjectByHandle(AHandle: Pointer): TObject;
begin
  Result := TObject(g_object_get_qdata(AHandle, QTObject));
end;

function ControlByHandle(AHandle: Pointer): TControl;
begin
  Result := TControl(g_object_get_qdata(AHandle, QTObject));
end;

{ TControl }

var
  DefaultFontDesc: String = '';

constructor TControl.Create(AOwner: TComponent);
begin
  inherited;
  FPopupMenu := nil;
  Handle := nil;
  CreateHandle;

  //--
  if DefaultFontDesc = '' then
    DefaultFontDesc := FontDesc;
  //--
  g_object_ref(Handle);
  //-- Set Data TObject
  if Handle <> nil then
    g_object_set_qdata(Handle, QTObject, Self);
  //-- Conecta signals
  ConnectSignals;

  // tava antes do g_object_ref
  //-- Visible = True
  if not (Self is TForm) then
    Show;
end;

destructor TControl.Destroy;
var
  Aux: TControl;
begin
  Parent := nil;

  while ControlCount > 0 do
  begin
    Aux := Controls[0];
    RemoveControl(Aux);
    Aux.Free;
  end;

  // g_object_unref(handle) ?
  DestroyHandle;
  inherited;
end;

procedure TControl.CreateHandle;
begin
  raise Exception.CreateFmt('%s is an abstract class.', [ClassName]);
end;

procedure TControl.DestroyHandle;
begin
  gtk_widget_destroy(Handle);
end;

function TControl.GetHandle: Pointer;
begin
  Result := Handle;
end;

procedure TControl.ReadState(Reader: TReader);
begin
  if Reader.Parent is TControl then
    Parent := TControl(Reader.Parent);
  inherited ReadState(Reader);
end;

function TControl.HasParent: Boolean;
begin
  Result := FParent <> nil;
end;

function TControl.GetForm: TForm;
var
  C: TControl;
begin
  C := Self;
  while (C <> nil) and (not (C is TForm)) do
  begin
    C := C.Parent;
  end;
  if C is TForm then
    Result := TForm(C)
  else
    Result := nil;
end;

procedure TControl.SendMessage(var AMsg: TXCLMsg); inline;
var
  Ev: PGdkEventClient;
begin
  Ev := PGdkEventClient(gdk_event_new(GDK_CLIENT_EVENT));
  Ev^.window := PGtkWidget(Handle)^.window;
  Ev^.message_type := gdk_atom_intern('_XCL_MESSAGE', False);
  Ev^.data_format := 32;
  Ev^.data.l[0] := AddXCLMsg(AMsg);
  gdk_event_put(PGdkEvent(Ev));
  gdk_event_free(PGdkEvent(Ev));
end;

procedure TControl.SendMessage(AName: String);
var
  Msg: TXCLMsg;
begin
  Msg.Name := AName;
  SendMessage(Msg);
end;

procedure TControl.GrabFocus;
begin
  gtk_widget_grab_focus(Handle);
end;

function TControl.IsFocus: Boolean;
begin
  Result := gtk_widget_is_focus(Handle);
end;

procedure TControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent is TControl) and (TControl(AComponent).Parent = Self) and (Operation = opRemove) then
    RemoveControl(TControl(AComponent));
  if (AComponent = FPopupMenu) and (Operation = opRemove) then
    PopupMenu := nil;
  inherited;
end;

procedure TControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I].Owner = Root then
      Proc(Controls[I]);
end;

procedure TControl.SetParent(ANewParent: TControl);
begin
  if FParent <> ANewParent then
  begin
    // Remove no Old
    if FParent <> nil then
      FParent.RemoveControl(Self);
    // Add no New
    if ANewParent <> nil then
      ANewParent.AddControl(Self)
  end;
end;

procedure TControl.AddControl(AControl: TControl);
begin
  raise Exception.CreateFmt('%s(%s) can''t hold controls.', [ClassName, Name]);
end;

procedure TControl.RemoveControl(AControl: TControl);
begin
  raise Exception.CreateFmt('%s(%s) can''t hold controls.', [ClassName, Name]);
end;

function TControl.GetControlByIndex(AIndex: Integer): TControl;
begin
  Result := nil;
  raise Exception.CreateFmt('Item index out of bounds. (%d)', [AIndex]);
end;

function TControl.GetControlCount: Integer;
begin
  Result := 0;
end;

procedure TControl.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  gtk_widget_set_name(Handle, PChar(Name));
end;

//-----------------------------------------------------------------------

procedure TControl.ShowCtrls;
begin
  ShowMe;
  gtk_widget_show_all(Handle);
end;

procedure TControl.HideCtrls;
begin
  HideMe;
  gtk_widget_hide_all(Handle);
end;

procedure TControl.ShowMe;
begin
  gtk_widget_show(Handle);
end;

procedure TControl.HideMe;
begin
  gtk_widget_hide(Handle);
end;

procedure TControl.Show;
begin
  ShowMe;
end;

procedure TControl.Hide;
begin
  HideMe;
end;

procedure TControl.ShowPopupMenu(AButton: Integer = 0);
begin
  if Assigned(FPopupMenu) then
    FPopupMenu.Popup(AButton);
end;

procedure TControl.SetPopupMenu(AValue: TMenu);
begin
  if AValue <> FPopupMenu then
  begin
    if Assigned(FPopupMenu) then
      FPopupMenu.RemoveFreeNotification(Self);
    //--
    FPopupMenu := AValue;
    //--
    if Assigned(FPopupMenu) then
      FPopupMenu.FreeNotification(Self);
  end;
end;

function TControl.GetToolTip: string;
var
  D: PGtkToolTipsData;
begin
  D := gtk_tooltips_data_get(Handle);
  if D <> nil then
    Result := D^.tip_text
  else
    Result := '';
end;

procedure TControl.SetToolTip(AValue: string);
begin
  if AValue <> '' then
    gtk_tooltips_set_tip(Application.FToolTips, Handle, PChar(AValue), nil)
  else
    gtk_tooltips_set_tip(Application.FToolTips, Handle, nil, nil)
end;

function TControl.GetSensitive: Boolean;
begin
  Result := (PGtkObject(Handle)^.Flags and GTK_SENSITIVE) <> 0;
end;

procedure TControl.SetSensitive(AValue: Boolean);
begin
  gtk_widget_set_sensitive(Handle, AValue);
end;

function TControl.GetVisible: Boolean;
begin
  Result := gtk_widget_visible(Handle);
end;

procedure TControl.SetVisible(AValue: Boolean);
begin
  if AValue then
    Show
  else
    Hide;
end;

procedure TControl.ConnectGtkSignal(const AName: String);
begin
  XCL.ConnectGtkSignal(Handle, PChar(AName));
end;

procedure TControl.DisconnectGtkSignal(const AName: String);
begin
  XCL.DisconnectGtkSignal(Handle, PChar(AName));
end;

procedure TControl.ConnectSignals;
begin
  ConnectGtkSignal('show');
  ConnectGtkSignal('hide');
  ConnectGtkSignal('button-press-event');
  ConnectGtkSignal('focus-in-event');
  ConnectGtkSignal('focus-out-event');
  ConnectGtkSignal('client-event');
end;

function TControl.IsFixedXYStored: Boolean;
begin
  Result := (Parent is TFixed);
end;

procedure TControl.SetFixedX(AValue: Integer);
begin
  if (Parent is TFixed) then
    TFixed(Parent).MoveChild(Self, AValue, FFixedY)
  else
    raise Exception.Create('You can set position only inside of a TFixed container.');
end;

procedure TControl.SetFixedY(AValue: Integer);
begin
  if (Parent is TFixed) then
    TFixed(Parent).MoveChild(Self, FFixedX, AValue)
  else
    raise Exception.Create('You can set position only inside of a TFixed container.');
end;

procedure TControl.DoPopupMenu(var Msg: TGtkSig);
begin
  if Assigned(FPopupMenu) then
    FPopupMenu.Popup;
end;

procedure TControl.DoShow(var Msg: TGtkSig);
begin
  ShowPopupMenu;
end;

procedure TControl.DoHide(var Msg: TGtkSig);
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TControl.DoRealize(var Msg: TGtkSig);
begin
  // nothing yet
end;

procedure TControl.DoUnrealize(var Msg: TGtkSig);
begin
  // nothing yet
end;

procedure TControl.DoExpose(var Msg: TGtkSigEvent);
begin
  // nothing yet
end;

procedure TControl.DoButtonPress(var Msg: TGtkSigEvent);
var
  Ev: PGdkEventButton;
begin
  Ev := PGdkEventButton(Msg.Event);
  if (Ev^.button = 3) and (Ev^._type = GDK_BUTTON_PRESS) then // It isn't always GDK_BUTTON_PRESS???
    ShowPopupMenu(3);
end;

procedure TControl.DoFocusIn(var Msg: TGtkSigEvent);
begin
  if Assigned(FOnFocusIn) then
    FOnFocusIn(Self);
end;

procedure TControl.DoFocusOut(var Msg: TGtkSigEvent);
begin
  if Assigned(FOnFocusOut) then
    FOnFocusOut(Self);
end;

function TControl.GetFontDesc: string;
var
  PC: PPangoContext;
  FD: PPangoFontDescription;
begin
  PC := gtk_widget_get_pango_context(Handle);
  FD := pango_context_get_font_description(PC);
  Result := pango_font_description_to_string(FD);
end;

procedure TControl.SetFontDesc(AValue: string);
var
  FD: PPangoFontDescription;
begin
  FD := pango_font_description_from_string(PChar(AValue));
  gtk_widget_modify_font(Handle, FD);
  pango_font_description_free(FD);
end;

function TControl.GetState: TWidgetState;
begin
  Result := TWidgetState(PGtkWidget(Handle)^.state);
end;

function TControl.GetBoxExpand: Boolean;
begin
  if Parent is TBox then
    gtk_container_child_get(Parent.Handle, Handle, 'expand', [@Result, 0])
  else
    Result := False;
end;

function TControl.GetSizeRequest(AIdx: Integer): Integer;
var
  SW, SH: Integer;
begin
  gtk_widget_get_size_request(Handle, @SW, @SH);
  if AIdx = 0 then
    Result := SW
  else
    Result := SH;
end;

procedure TControl.SetSizeRequest(AIdx: Integer; AValue: Integer);
var
  SW, SH: Integer;
begin
  gtk_widget_get_size_request(Handle, @SW, @SH);
  if AIdx = 0 then
    gtk_widget_set_size_request(Handle, AValue, SH)
  else
    gtk_widget_set_size_request(Handle, SW, AValue);
end;

function TControl.GetBoxFill: Boolean;
begin
  if Parent is TBox then
    gtk_container_child_get(Parent.Handle, Handle, 'fill', [@Result, 0])
  else
    Result := True;
end;

function TControl.GetBoxPadding: Cardinal;
begin
  if Parent is TBox then
    gtk_container_child_get(Parent.Handle, Handle, 'padding', [@Result, 0])
  else
    Result := 0;
end;

function TControl.GetBoxPosition: Integer;
begin
  if Parent is TBox then
    gtk_container_child_get(Parent.Handle, Handle, 'position', [@Result, 0])
  else
    Result := 0;
end;

procedure TControl.SetBoxExpand(AValue: Boolean);
begin
  if Parent is TBox then
    gtk_container_child_set(Parent.Handle, Handle, 'expand', [gboolean(AValue), 0])
  else
    raise Exception.Create('BoxExpand can be set only in a TBox Child.');
end;

procedure TControl.SetBoxFill(AValue: Boolean);
begin
  if Parent is TBox then
    gtk_container_child_set(Parent.Handle, Handle, 'fill', [gboolean(AValue), 0])
  else
    raise Exception.Create('BoxFill can be set only in a TBox Child.');
end;

procedure TControl.SetBoxPadding(AValue: Cardinal);
begin
  if Parent is TBox then
    gtk_container_child_set(Parent.Handle, Handle, 'padding', [AValue, 0])
  else
    raise Exception.Create('BoxPadding can be set only in a TBox Child.');
end;

procedure TControl.SetBoxPosition(AValue: Integer);
begin
  if Parent is TBox then
    TBox(Parent).ReorderControl(Self, AValue)
  else
    raise Exception.Create('BoxPosition can be set only in a TBox Child.');
end;

function TControl.IsBoxExpandStored: Boolean;
begin
  Result := (Parent is TBox);
end;

function TControl.IsFontDescStored: Boolean;
begin
  Result := FontDesc <> DefaultFontDesc;
end;

function TControl.DesignShowProp(AName: ShortString): Boolean;
begin
  if AName = 'FixedPosition' then
    Result := False
  else if AName = 'FixedX' then
    Result := Parent is TFixed
  else if AName = 'FixedY' then
    Result := Parent is TFixed
  else if (AName = 'BoxExpand') or (AName = 'BoxFill') or (AName = 'BoxPadding') then
    Result := Parent is TBox
  else if (AName = 'PanedResize') or (AName = 'PanedShrink') then
    Result := Parent is TPaned
  else if (AName = 'TableBottomAttach') or (AName = 'TableLeftAttach') or (AName = 'TableRightAttach') or (AName = 'TableTopAttach') or (AName = 'TableXPadding') or (AName = 'TableYPadding') or (AName = 'TableXOptions') or (AName = 'TableYOptions') then
    Result := Parent is TTable
  else
    Result := True;
end;

function TControl.GetPanedResize: Boolean;
begin
  if Parent is TPaned then
    gtk_container_child_get(Parent.Handle, Handle, 'resize', [@Result, 0])
  else
    Result := False;
end;

function TControl.GetPanedShrink: Boolean;
begin
  if Parent is TPaned then
    gtk_container_child_get(Parent.Handle, Handle, 'shrink', [@Result, 0])
  else
    Result := False;
end;

procedure TControl.SetPanedResize(AValue: Boolean);
begin
  if Parent is TPaned then
    gtk_container_child_set(Parent.Handle, Handle, 'resize', [gboolean(AValue), 0])
  else
    raise Exception.Create('PanedResize can be set only in a TPaned Child.');
end;

procedure TControl.SetPanedShrink(AValue: Boolean);
begin
  if Parent is TPaned then
    gtk_container_child_set(Parent.Handle, Handle, 'shrink', [gboolean(AValue), 0])
  else
    raise Exception.Create('PanedShrink can be set only in a TPaned Child.');
end;

function TControl.IsPanedResizeStored: Boolean;
begin
  Result := Parent is TPaned;
end;

function TControl.IsPanedShrinkStored: Boolean;
begin
  Result := Parent is TPaned;
end;

function TControl.GetTableBottomAttach: Word;
begin
  if Parent is TTable then
    gtk_container_child_get(Parent.Handle, Handle, 'bottom-attach', [@Result, 0])
  else
    Result := 0;
end;

procedure TControl.SetTableBottomAttach(AValue: Word);
begin
  if Parent is TTable then
    gtk_container_child_set(Parent.Handle, Handle, 'bottom-attach', [AValue, 0])
  else
    raise Exception.Create('TableBottomAttach can be set only in a TTable Child.');
end;

function TControl.GetTableLeftAttach: Word;
begin
  if Parent is TTable then
    gtk_container_child_get(Parent.Handle, Handle, 'left-attach', [@Result, 0])
  else
    Result := 0;
end;

procedure TControl.SetTableLeftAttach(AValue: Word);
begin
  if Parent is TTable then
    gtk_container_child_set(Parent.Handle, Handle, 'left-attach', [AValue, 0])
  else
    raise Exception.Create('TableLeftAttach can be set only in a TTable Child.');
end;

function TControl.GetTableRightAttach: Word;
begin
  if Parent is TTable then
    gtk_container_child_get(Parent.Handle, Handle, 'right-attach', [@Result, 0])
  else
    Result := 0;
end;

procedure TControl.SetTableRightAttach(AValue: Word);
begin
  if Parent is TTable then
    gtk_container_child_set(Parent.Handle, Handle, 'right-attach', [AValue, 0])
  else
    raise Exception.Create('TableRightAttach can be set only in a TTable Child.');
end;

function TControl.GetTableTopAttach: Word;
begin
  if Parent is TTable then
    gtk_container_child_get(Parent.Handle, Handle, 'top-attach', [@Result, 0])
  else
    Result := 0;
end;

procedure TControl.SetTableTopAttach(AValue: Word);
begin
  if Parent is TTable then
    gtk_container_child_set(Parent.Handle, Handle, 'top-attach', [AValue, 0])
  else
    raise Exception.Create('TableTopAttach can be set only in a TTable Child.');
end;

function TControl.GetTableXPadding: Word;
begin
  if Parent is TTable then
    gtk_container_child_get(Parent.Handle, Handle, 'x-padding', [@Result, 0])
  else
    Result := 0;
end;

procedure TControl.SetTableXPadding(AValue: Word);
begin
  if Parent is TTable then
    gtk_container_child_set(Parent.Handle, Handle, 'x-padding', [AValue, 0])
  else
    raise Exception.Create('TableXPadding can be set only in a TTable Child.');
end;

function TControl.GetTableYPadding: Word;
begin
  if Parent is TTable then
    gtk_container_child_get(Parent.Handle, Handle, 'y-padding', [@Result, 0])
  else
    Result := 0;
end;

procedure TControl.SetTableYPadding(AValue: Word);
begin
  if Parent is TTable then
    gtk_container_child_set(Parent.Handle, Handle, 'y-padding', [AValue, 0])
  else
    raise Exception.Create('TableYPadding can be set only in a TTable Child.');
end;

function TControl.IsTableAttachStored: Boolean;
begin
  Result := Parent is TTable;
end;

function TControl.GetTableXOptions: TAttachOptions;
begin
  if Parent is TTable then
    gtk_container_child_get(Parent.Handle, Handle, 'x-options', [@Result, 0])
  else
    Result := [];
end;

procedure TControl.SetTableXOptions(AValue: TAttachOptions);
begin
  if Parent is TTable then
  begin
    gtk_container_child_set(Parent.Handle, Handle, 'x-options', [Integer(AValue), 0]);
    gtk_container_resize_children(Parent.Handle);
    gtk_widget_child_notify(Handle, 'x-options');
  end
  else
    raise Exception.Create('TableXOptions can be set only in a TTable Child.');
end;

function TControl.GetTableYOptions: TAttachOptions;
begin
  if Parent is TTable then
    gtk_container_child_get(Parent.Handle, Handle, 'y-options', [@Result, 0])
  else
    Result := [];
end;

procedure TControl.SetTableYOptions(AValue: TAttachOptions);
begin
  if Parent is TTable then
  begin
    gtk_container_child_set(Parent.Handle, Handle, 'y-options', [Integer(AValue), 0]);
    gtk_container_resize_children(Parent.Handle);
    gtk_widget_child_notify(Handle, 'y-options');
  end
  else
    raise Exception.Create('TableYOptions can be set only in a TTable Child.');
end;

{$ENDIF}
