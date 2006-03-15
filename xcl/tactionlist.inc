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

  TActionList = class(TComponent)
  private
    FActions: TList;
    FAccelGroup: TAccelGroup;
    function GetAction(Index: Integer): TCustomAction;
    function GetActionCount: Integer;
    procedure SetAction(Index: Integer; Value: TCustomAction);
  protected
    procedure AddAction(AAction: TCustomAction); virtual;
    procedure RemoveAction(AAction: TCustomAction); virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetChildOrder(AComponent: TComponent; AOrder: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOfName(const AActionName: String): Integer;
    function ActionByName(const AActionName: String): TCustomAction;
    property Actions[Index: Integer]: TCustomAction read GetAction write SetAction; default;
    property ActionCount: Integer read GetActionCount;
    property AccelGroup: TAccelGroup read FAccelGroup;
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TActionList }

constructor TActionList.Create(AOwner: TComponent);
begin
  inherited;
  FActions := TList.Create;
  FAccelGroup := TAccelGroup.Create(nil);
  if AOwner is TForm then
    TForm(AOwner).AddAccelGroup(FAccelGroup);
end;

destructor TActionList.Destroy;
begin
  while FActions.Count > 0 do
    TCustomAction(FActions.Last).Free;
  FActions.Free;
  if Owner is TForm then
    TForm(Owner).RemoveAccelGroup(FAccelGroup);
  FAccelGroup.Free;
  inherited;
end;

procedure TActionList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Action: TAction;
begin
  for I := 0 to FActions.Count - 1 do
  begin
    Action := TAction(FActions.List^[I]);
    if Action.Owner = Root then
      Proc(Action);
  end;
end;

procedure TActionList.SetChildOrder(AComponent: TComponent; AOrder: Integer);
begin
  if FActions.IndexOf(AComponent) >= 0 then
    TCustomAction(AComponent).Index := AOrder;
end;

function TActionList.GetAction(Index: Integer): TCustomAction;
begin
  Result := TCustomAction(FActions[Index]);
end;

function TActionList.GetActionCount: Integer;
begin
  Result := FActions.Count;
end;

procedure TActionList.SetAction(Index: Integer; Value: TCustomAction);
begin
  TCustomAction(FActions[Index]).Assign(Value);
end;

procedure TActionList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if (AComponent is TCustomAction) then
      RemoveAction(TCustomAction(AComponent));
end;

procedure TActionList.AddAction(AAction: TCustomAction);
begin
  FActions.Add(AAction);
  AAction.FActionList := Self;
  AAction.FreeNotification(Self);
end;

procedure TActionList.RemoveAction(AAction: TCustomAction);
begin
  if FActions.Remove(AAction) >= 0 then
    AAction.FActionList := nil;
end;

function TActionList.IndexOfName(const AActionName: String): Integer;
begin
  Result := FActions.Count-1;
  while (Result>=0) and (AnsiCompareText(TAction(FActions[Result]).Name,AActionName)<>0) do
    Dec(Result);
end;

function TActionList.ActionByName(const AActionName: String): TCustomAction;
var
  I: Integer;
begin
  I := IndexOfName(AActionName);
  if I >= 0 then
    Result := Actions[i]
  else
    Result := nil;
end;

{$ENDIF}
