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

  TAccelerator = class(TComponent)
  private
    Closure: Pointer;
    FCurrAG: TAccelGroup;
    FAccelGroup: TAccelGroup;
    FAccelName: String;
    FOnActivate: TNotifyEvent;
    procedure SetAccelName(AValue: String);
    procedure SetAccelGroup(AValue: TAccelGroup);
  protected
    procedure Update(AAccelGroup: TAccelGroup; AAccelName: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoActivate;
  published
    property AccelGroup: TAccelGroup read FAccelGroup write SetAccelGroup;
    property AccelName: String read FAccelName write SetAccelName;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TAccelerator }

constructor TAccelerator.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TForm then
  begin
    FCurrAG := TForm(AOwner).AccelGroup;
    FCurrAG.FreeNotification(Self);
  end
  else
    FCurrAG := nil;
  FAccelGroup := nil;
  FAccelName := '';
  FOnActivate := nil;
  Closure := nil;
end;

destructor TAccelerator.Destroy;
begin
  if Assigned(Closure) then
  begin
    gtk_accel_group_disconnect(FCurrAG.Handle, Closure);
    Closure := nil;
  end;
  inherited;
end;

procedure TAccelerator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FCurrAG) and (Operation = opRemove) then
  begin
    if FAccelGroup = AComponent then
    begin
      FAccelGroup := nil;
      Update(nil, FAccelName);
    end
    else
      Update(nil, '');
  end;
  inherited;
end;

procedure TAccelerator.SetAccelName(AValue: String);
var
  LKey: guint;
  LMod: TGdkModifierType;
begin
  gtk_accelerator_parse(PChar(AValue), @LKey, @LMod);
  if (LKey = 0) and (LMod = 0) then
    AValue := '';
  if AValue <> FAccelName then
  begin
    Update(FCurrAG, AValue);
  end;
end;

procedure TAccelerator.SetAccelGroup(AValue: TAccelGroup);
begin
  if AValue <> FAccelGroup then
  begin
    if Assigned(FCurrAG) then
      FCurrAG.RemoveFreeNotification(Self);

    FAccelGroup := AValue;

    if (AValue = nil) and (Owner is TForm) then
      Update(TForm(Owner).AccelGroup, FAccelName)
    else
      Update(AValue, FAccelName);

    if Assigned(FCurrAG) then
      FCurrAG.FreeNotification(Self);
  end;
end;

procedure Hnd_TAccelerator_Activate(AAccelGroup: Pointer; Arg1: PGObject; AKey: guint; AMod: TGdkModifierType; Sender: TAccelerator); cdecl;
begin
  Sender.DoActivate;
end;

procedure TAccelerator.DoActivate;
begin
  if Assigned(FOnActivate) then
    FOnActivate(Self);
end;

procedure TAccelerator.Update(AAccelGroup: TAccelGroup; AAccelName: String);
var
  LKey: guint;
  LMod: TGdkModifierType;
begin

  if Assigned(Closure) then
  begin
    gtk_accel_group_disconnect(FCurrAG.Handle, Closure);
    Closure := nil;
  end;

  FCurrAG := AAccelGroup;
  FAccelName := AAccelName;

  if (FAccelName <> '') and (FCurrAG <> nil) then
  begin
    closure := g_cclosure_new(G_CALLBACK(@Hnd_TAccelerator_Activate), Pointer(Self), nil);
    gtk_accelerator_parse(PChar(FAccelName), @LKey, @LMod);
    gtk_accel_group_connect(FCurrAG.Handle, LKey, LMod, GTK_ACCEL_VISIBLE, Closure)
  end;
end;

{$ENDIF}
