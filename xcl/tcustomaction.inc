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

  TCustomAction = class(TContainedAction)
  private
    FAccelClosure: Pointer;
    FAccelerator: String;
    FCaption: String;
    FStockID: String;
    FStockCaption: String;
    FSensitive: Boolean;
    FVisible: Boolean;
    procedure SetAccelerator(const AValue: String);
    procedure SetCaption(const AValue: String);
    procedure SetStockID(const AValue: String);
    procedure SetSensitive(AValue: Boolean);
    procedure SetVisible(AValue: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function IsCaptionStored: Boolean; virtual;
    function IsStockIDStored: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function GetCaption: String;
  public
    property Accelerator: String read FAccelerator write SetAccelerator;
    property Caption: String read FCaption write SetCaption stored IsCaptionStored;
    property StockID: String read FStockID write SetStockID stored IsStockIDStored;
    property Sensitive: Boolean read FSensitive write SetSensitive default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TAction = class(TCustomAction)
  published
    property Caption;
    property StockID;
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
  Application.AddAction(Self);
end;

destructor TCustomAction.Destroy;
begin
  Application.RemoveAction(Self);
  inherited;
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
      OnChange := Self.OnChange;
    end
  else
    inherited;
end;

function TCustomAction.IsCaptionStored: Boolean;
begin
  Result := FCaption <> '';
end;

function TCustomAction.IsStockIDStored: Boolean;
begin
  Result := FStockID <> '';
end;

function TCustomAction.GetCaption: String;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := FStockCaption
end;

procedure Hnd_TCustomAction_Execute(AAccelGroup: Pointer; Arg1: PGObject; AKey: guint; AMod: TGdkModifierType; Sender: TCustomAction); cdecl;
begin
  Sender.Execute;
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
var
  I: Integer;
begin
  if AValue <> FCaption then
  begin
    for I := 0 to FClients.Count - 1 do
      if TObject(FClients.List^[I]) is TActionLink then
        TActionLink(FClients.List^[I]).SetCaption(AValue);
    FCaption := AValue;
    Change;
  end;
end;

procedure TCustomAction.SetStockID(const AValue: String);
var
  I: Integer;
  StockItem: TGtkStockItem;
begin
  if AValue <> FStockID then
  begin
    // Set FStockCaption
    if gtk_stock_lookup(PChar(AValue), @StockItem) then
      FStockCaption := StockItem._label
    else
      FStockCaption := '';

    for I := 0 to FClients.Count - 1 do
      if TObject(FClients.List^[I]) is TActionLink then
        TActionLink(FClients.List^[I]).SetStockID(AValue);

    // se vc nao tem um Caption, ele usa o StockCaption
    if (FCaption = '') and (FStockCaption <> '') then
      for I := 0 to FClients.Count - 1 do
        if TObject(FClients.List^[I]) is TActionLink then
          TActionLink(FClients.List^[I]).SetCaption(FStockCaption);
    //----
    FStockID := AValue;
    Change;
  end;
end;

procedure TCustomAction.SetSensitive(AValue: Boolean);
var
  I: Integer;
begin
  if AValue <> FSensitive then
  begin
    for I := 0 to FClients.Count - 1 do
      if TObject(FClients.List^[I]) is TActionLink then
      begin
        TActionLink(FClients.List^[I]).SetSensitive(AValue);
      end;
    FSensitive := AValue;
    Change;
  end;
end;

procedure TCustomAction.SetVisible(AValue: Boolean);
var
  I: Integer;
begin
  if AValue <> FVisible then
  begin
    for I := 0 to FClients.Count - 1 do
      if TObject(FClients.List^[I]) is TActionLink then
        TActionLink(FClients.List^[I]).SetVisible(AValue);
    FVisible := AValue;
    Change;
  end;
end;

function TCustomAction.Execute: Boolean;
begin
  Update;
  Result := Sensitive and inherited Execute;
end;

{$ENDIF}
