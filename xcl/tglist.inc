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

  TGList = class(TObject)
  private
    FHandle: Pointer;
    FListOwner: Boolean;
  protected
    function Get(AIndex: Integer): Pointer; dynamic;
    procedure Put(AIndex: Integer; AValue: Pointer); dynamic;
    function GetCount: Integer;
  public
    constructor Create(AHandle: Pointer = nil; AListOwner: Boolean = True); virtual;
    destructor Destroy; override;
    //--
    procedure Add(AValue: Pointer); dynamic;
    procedure Remove(AIdx: Integer);
    //--
    property ListOwner: Boolean read FListOwner write FListOwner;
    property Handle: Pointer read FHandle write FHandle;
    //--
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property Count: Integer read GetCount;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

constructor TGList.Create(AHandle: Pointer = nil; AListOwner: Boolean = True);
begin
  FHandle := AHandle;
  FListOwner := AListOwner;
end;

destructor TGList.Destroy;
begin
  if FListOwner and (FHandle <> nil) then
    g_list_free(FHandle);
end;

function TGList.Get(AIndex: Integer): Pointer;
begin
  Result := g_list_nth_data(FHandle, AIndex);
end;

procedure TGList.Put(AIndex: Integer; AValue: Pointer);
begin
  if AIndex > GetCount-1 then
    Add(AValue)
  else
    glib2.PGList(g_list_nth(FHandle, AIndex))^.data := AValue;
end;

procedure TGlist.Add(AValue: Pointer);
begin
  if FHandle = nil then
    FListOwner := True;
  FHandle := g_list_append(FHandle, AValue);
end;

procedure TGList.Remove(AIdx: Integer);
begin
  FHandle := g_list_delete_link(FHandle, g_list_nth(FHandle, AIdx));
end;

function TGList.GetCount: Integer;
begin
  Result := g_list_length(FHandle);
end;

{$ENDIF}
