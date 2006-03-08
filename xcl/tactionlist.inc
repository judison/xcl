(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{** INTERFACE *************************************}{$IFDEF INC_READ_INTERFACE}

  TActionEvent = procedure(Action: TBasicAction; var Handled: Boolean) of object;

  TActionList = class(TComponent)
  private
    FActions: TList;
    FOnChange: TNotifyEvent;
    FOnExecute: TActionEvent;
    FOnUpdate: TActionEvent;
    function GetAction(Index: Integer): TContainedAction;
    function GetActionCount: Integer;
    procedure SetAction(Index: Integer; Value: TContainedAction);
  protected
    procedure AddAction(AAction: TContainedAction); virtual;
    procedure RemoveAction(AAction: TContainedAction); virtual;
    procedure Change; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetChildOrder(AComponent: TComponent; AOrder: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;
    function IndexOfName(const AActionName: String): Integer;
    function ActionByName(const AActionName: String): TContainedAction;
    property Actions[Index: Integer]: TContainedAction read GetAction write SetAction; default;
    property ActionCount: Integer read GetActionCount;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnExecute: TActionEvent read FOnExecute write FOnExecute;
    property OnUpdate: TActionEvent read FOnUpdate write FOnUpdate;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TActionList }

constructor TActionList.Create(AOwner: TComponent);
begin
  inherited;
  FActions := TList.Create;
end;

destructor TActionList.Destroy;
begin
  while FActions.Count > 0 do
    TContainedAction(FActions.Last).Free;
  FActions.Free;
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
    TContainedAction(AComponent).Index := AOrder;
end;

function TActionList.GetAction(Index: Integer): TContainedAction;
begin
  Result := TContainedAction(FActions[Index]);
end;

function TActionList.GetActionCount: Integer;
begin
  Result := FActions.Count;
end;

procedure TActionList.SetAction(Index: Integer; Value: TContainedAction);
begin
  TContainedAction(FActions[Index]).Assign(Value);
end;

procedure TActionList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if (AComponent is TContainedAction) then
      RemoveAction(TContainedAction(AComponent));
end;

procedure TActionList.AddAction(AAction: TContainedAction);
begin
  FActions.Add(AAction);
  AAction.FActionList := Self;
  AAction.FreeNotification(Self);
end;

procedure TActionList.RemoveAction(AAction: TContainedAction);
begin
  if FActions.Remove(AAction) >= 0 then
    AAction.FActionList := nil;
end;

procedure TActionList.Change;
var
  I: Integer;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  for I := 0 to FActions.Count - 1 do
    TContainedAction(FActions.List^[I]).Change;
end;

function TActionList.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnExecute) then
    FOnExecute(AAction, Result);
end;

function TActionList.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnUpdate) then
    FOnUpdate(AAction, Result);
end;

function TActionList.IndexOfName(const AActionName: String): Integer;
begin
  Result := FActions.Count-1;
  while (Result>=0) and (AnsiCompareText(TAction(FActions[Result]).Name,AActionName)<>0) do
    Dec(Result);
end;

function TActionList.ActionByName(const AActionName: String): TContainedAction;
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
