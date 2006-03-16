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

  IActionClient = interface
    procedure DoActionChanged(Sender: TCustomAction);
  end;

  TCustomAction = class(TComponent)
  private
    FCategory: string;
    FActionList: TActionList;
    //--
    FClientList: TList;
    //--
    FOnExecute: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
    FAccelClosure: Pointer;
    FAccelerator: String;
    FCaption: String;
    FTooltip: String;
    FIconName: String;
    FSensitive: Boolean;
    FVisible: Boolean;
    function GetIndex: Integer;
    procedure SetCategory(const Value: string);
    procedure SetIndex(Value: Integer);
    procedure SetActionList(AActionList: TActionList);
    //--
    procedure SetAccelerator(const AValue: String);
    procedure SetCaption(const AValue: String);
    procedure SetTooltip(const AValue: String);
    procedure SetIconName(const AValue: String);
    procedure SetSensitive(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
  protected
    function IsCaptionStored: Boolean; virtual;
    function IsIconNameStored: Boolean; virtual;
    //--
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;
    //--
    procedure AssignTo(Dest: TPersistent); override;
    //--
    procedure DoExecute; virtual;
    procedure DoUpdate; virtual;
    //--
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //--
    procedure AddClient(AClient: IActionClient);
    procedure RemoveClient(AClient: IActionClient);
    //--
    procedure Execute;
    procedure Update;
    //--
    function GetAccelLabel: String;
    //--
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    property ActionList: TActionList read FActionList write SetActionList;
    property Index: Integer read GetIndex write SetIndex stored False;
  public
    property Accelerator: String read FAccelerator write SetAccelerator;
    property Caption: String read FCaption write SetCaption stored IsCaptionStored;
    property Tooltip: String read FTooltip write SetTooltip;
    property IconName: String read FIconName write SetIconName stored IsIconNameStored;
    property Sensitive: Boolean read FSensitive write SetSensitive default True;
    property Visible: Boolean read FVisible write SetVisible default True;
    //--
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  published
    property Category: String read FCategory write SetCategory;
  end;

  TAction = class(TCustomAction)
  published
    property Accelerator;
    property Caption;
    property Tooltip;
    property IconName;
    property Sensitive;
    property Visible;
    //--
    property OnExecute;
    property OnUpdate;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TCustomAction }

constructor TCustomAction.Create(AOwner: TComponent);
begin
  inherited;
  FAccelerator := '';
  FAccelClosure := nil;
  FSensitive := True;
  FVisible := True;
  FClientList := TList.Create;
  //--
  Application.AddAction(Self);
end;

destructor TCustomAction.Destroy;
begin
  FClientList.Free;
  FClientList := nil;

  Application.RemoveAction(Self);

  if ActionList <> nil then
    ActionList.RemoveAction(Self);
  inherited;
end;

procedure TCustomAction.AddClient(AClient: IActionClient);
begin
  FClientList.Add(AClient);
end;

procedure TCustomAction.RemoveClient(AClient: IActionClient);
begin
  FClientList.Remove(AClient);
end;

procedure TCustomAction.Changed;
var
  I: Integer;
begin
  for I := 0 to FClientList.Count -1 do
    IActionClient(FClientList[I]).DoActionChanged(Self);
end;

function TCustomAction.GetIndex: Integer;
begin
  if ActionList <> nil then
    Result := ActionList.FActions.IndexOf(Self)
  else
    Result := -1;
end;

function TCustomAction.GetParentComponent: TComponent;
begin
  if ActionList <> nil then
    Result := ActionList
  else
    Result := inherited GetParentComponent;
end;

function TCustomAction.HasParent: Boolean;
begin
  if ActionList <> nil then
    Result := True
  else
    Result := inherited HasParent;
end;

procedure TCustomAction.ReadState(Reader: TReader);
begin
  if Reader.Parent is TActionList then
    ActionList := TActionList(Reader.Parent);
  inherited ReadState(Reader);
end;

procedure TCustomAction.SetParentComponent(AParent: TComponent);
begin
  if (not (csLoading in ComponentState)) and (AParent is TActionList) then
    ActionList := TActionList(AParent);
end;



procedure TCustomAction.SetIndex(Value: Integer);
var
  CurIndex, Count: Integer;
begin
  CurIndex := GetIndex;
  if CurIndex >= 0 then
  begin
    Count := ActionList.FActions.Count;

    if Value < 0 then
      Value := 0
    else if Value >= Count then
      Value := Count - 1;

    if Value <> CurIndex then
      ActionList.FActions.Move(CurIndex, Value);
  end;
end;

procedure TCustomAction.SetCategory(const Value: string);
begin
  if Value <> Category then
  begin
    FCategory := Value;
//    if ActionList <> nil then
//      ActionList.Change;
  end;
end;

procedure TCustomAction.SetActionList(AActionList: TActionList);
begin
  if AActionList <> ActionList then
  begin
    if Assigned(ActionList) then
      ActionList.RemoveAction(Self);
    if Assigned(AActionList) then
      AActionList.AddAction(Self);
  end;
end;

procedure TCustomAction.Execute;
begin
  DoUpdate;
  if Sensitive then
    DoExecute;
end;

procedure TCustomAction.Update;
begin
  DoUpdate;
end;

procedure TCustomAction.DoExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TCustomAction.DoUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

procedure TCustomAction.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAction then
    with TCustomAction(Dest) do
    begin
      Caption := Self.Caption;
      Sensitive := Self.Sensitive;
      Visible := Self.Visible;
      OnExecute := Self.OnExecute;
      OnUpdate := Self.OnUpdate;
    end
  else
    inherited;
end;

function TCustomAction.IsCaptionStored: Boolean;
begin
  Result := FCaption <> '';
end;

function TCustomAction.IsIconNameStored: Boolean;
begin
  Result := FIconName <> '';
end;

procedure Hnd_TCustomAction_Execute(AAccelGroup: Pointer; Arg1: PGObject; AKey: guint; AMod: TGdkModifierType; Sender: TCustomAction); cdecl;
begin
  Sender.Execute;
end;

function TCustomAction.GetAccelLabel: String;
var
  LKey: guint;
  LMod: TGdkModifierType;
begin
  gtk_accelerator_parse(PChar(FAccelerator), @LKey, @LMod);
  if (LKey = 0) and (LMod = 0) then
    Result := ''
  else
    Result := gtk_accelerator_get_label(LKey, LMod);
end;

procedure TCustomAction.SetAccelerator(const AValue: String);
var
  LKey: guint;
  LMod: TGdkModifierType;
begin
  if FAccelerator <> AValue then
  begin
    if Assigned(FAccelClosure) then
    begin
      gtk_accel_group_disconnect(ActionList.AccelGroup.Handle, FAccelClosure);
      FAccelClosure := nil;
    end;
    //--
    gtk_accelerator_parse(PChar(AValue), @LKey, @LMod);
    if (LKey = 0) and (LMod = 0) then
      FAccelerator := ''
    else
      FAccelerator := AValue;
    //--
    if (FAccelerator <> '') and Assigned(ActionList) then
    begin
      FAccelClosure := g_cclosure_new(G_CALLBACK(@Hnd_TCustomAction_Execute), Pointer(Self), nil);
      gtk_accel_group_connect(ActionList.AccelGroup.Handle, LKey, LMod, GTK_ACCEL_VISIBLE, FAccelClosure);
    end;
  end;
end;

procedure TCustomAction.SetCaption(const AValue: String);
begin
  if AValue <> FCaption then
  begin
    FCaption := AValue;
    Changed;
  end;
end;

procedure TCustomAction.SetTooltip(const AValue: String);
begin
  if AValue <> FTooltip then
  begin
    FTooltip := AValue;
    Changed;
  end;
end;

procedure TCustomAction.SetIconName(const AValue: String);
begin
  if AValue <> FIconName then
  begin
    FIconName := AValue;
    Changed;
  end;
end;

procedure TCustomAction.SetSensitive(AValue: Boolean);
begin
  if AValue <> FSensitive then
  begin
    FSensitive := AValue;
    Changed;
  end;
end;

procedure TCustomAction.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

{$ENDIF}
