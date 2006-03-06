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

  TGStrings = class(TStrings)
  private
    FUpdating: Boolean;
    FOnChange: TNotifyEvent;
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    Handle: Pointer;
    constructor Create(AHandle: Pointer);
    destructor Destroy; override;
    procedure Changed;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

const
  SListIndexError = 'List index exceeds bounds (%d)';

{ TGStrings }

constructor TGStrings.Create(AHandle: Pointer);
begin
  Handle := AHandle;
  FUpdating := False;
  inherited Create;
end;

destructor TGStrings.Destroy;
begin
  Clear;
  inherited;
end;

procedure TGStrings.SetUpdateState(Updating: Boolean);
begin
  WriteLn('SetUp :', Updating);
  FUpdating := Updating;
  if not Updating then
    Changed;
end;

procedure TGStrings.Changed;
begin
  WriteLn('Changed');
  if (not FUpdating) and Assigned(FOnChange) then
    FOnChange(Self);
end;

function TGStrings.Get(Index: Integer): string;
var
  Item: PGList;
begin
  WriteLn('Get I:', Index);
  if Handle <> nil then
  begin
    Item := g_list_nth(Handle, Index);
    if Item <> nil then
      Result := PChar(Item^.Data)
    else
      Error(SListIndexError, Index);
  end
  else
    Error(SListIndexError, Index);
end;

function TGStrings.GetCapacity: Integer;
begin
  Result := Count+1; //?
end;

function TGStrings.GetCount: Integer;
begin
  if Handle <> nil then
    Result := g_list_length(Handle)
  else
    Result := 0;
end;

procedure TGStrings.Put(Index: Integer; const S: string);
var
  Item: PGList;
  Str: PChar;
begin
  WriteLn('Put I:', Index, ' S:',S);
  Item := g_list_nth(Handle, Index);
  if Item <> nil then
  begin
    Str := StrNew(PChar(S));
    Item^.Data := Str;
    Changed;
  end
  else
    Error(SListIndexError,Index);
end;

procedure TGStrings.Clear;
begin
  WriteLn('Clear');
  while Handle <> nil do
  begin
    StrDispose(PGList(Handle)^.Data);
    Handle := g_list_delete_link(Handle, Handle);
  end;
  Changed;
end;

procedure TGStrings.Delete(Index: Integer);
var
  Item: PGList;
begin
  WriteLn('Delete I:', Index);
  Item := g_list_nth(Handle, Index);
  StrDispose(Item^.Data);
  Handle := g_list_delete_link(Handle, Item);
  Changed;
end;

procedure TGStrings.Insert(Index: Integer; const S: string);
var
  Str: PChar;
begin
  WriteLn('Insert I:', Index, ' S:',S);
  Str := StrNew(PChar(S));
  Handle := g_list_insert(Handle, Str, Index);
  Changed;
end;

{$ENDIF}
