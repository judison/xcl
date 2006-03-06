(*
   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
(*
   Portions from LCL (Lazarus Component Library)
*)
{$IFDEF INC_READ_INTERFACE}
//** INTERFACE
//****************************************************************************

  TContainedAction = class(TBasicAction)
  private
    FCategory: string;
    FActionList: TActionList;
    function GetIndex: Integer;
    procedure SetCategory(const Value: string);
    procedure SetIndex(Value: Integer);
    procedure SetActionList(AActionList: TActionList);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;
  public
    destructor Destroy; override;
    function Execute: Boolean; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    function Update: Boolean; override;
    property ActionList: TActionList read FActionList write SetActionList;
    property Index: Integer read GetIndex write SetIndex stored False;
  published
    property Category: string read FCategory write SetCategory;
  end;

  TContainedActionClass = class of TContainedAction;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TContainedAction }

destructor TContainedAction.Destroy;
begin
  if ActionList <> nil then
    ActionList.RemoveAction(Self);
  inherited Destroy;
end;

function TContainedAction.GetIndex: Integer;
begin
  if ActionList <> nil then
    Result := ActionList.FActions.IndexOf(Self)
  else
    Result := -1;
end;

function TContainedAction.GetParentComponent: TComponent;
begin
  if ActionList <> nil then
    Result := ActionList
  else
    Result := inherited GetParentComponent;
end;

function TContainedAction.HasParent: Boolean;
begin
  if ActionList <> nil then
    Result := True
  else
    Result := inherited HasParent;
end;

procedure TContainedAction.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TActionList then
    ActionList := TActionList(Reader.Parent);
end;

procedure TContainedAction.SetIndex(Value: Integer);
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

procedure TContainedAction.SetCategory(const Value: string);
begin
  if Value <> Category then
  begin
    FCategory := Value;
    if ActionList <> nil then
      ActionList.Change;
  end;
end;

procedure TContainedAction.SetActionList(AActionList: TActionList);
begin
  if AActionList <> ActionList then
  begin
    if ActionList <> nil then
      ActionList.RemoveAction(Self);
    if AActionList <> nil then
      AActionList.AddAction(Self);
  end;
end;

procedure TContainedAction.SetParentComponent(AParent: TComponent);
begin
  if (not (csLoading in ComponentState)) and (AParent is TActionList) then
    ActionList := TActionList(AParent);
end;

function TContainedAction.Execute: Boolean;
begin
  Result := ((ActionList <> nil) and ActionList.ExecuteAction(Self)) or inherited Execute;
end;

function TContainedAction.Update: Boolean;
begin
  Result := ((ActionList <> nil) and ActionList.UpdateAction(Self)) or inherited Update;
end;

{$ENDIF}
