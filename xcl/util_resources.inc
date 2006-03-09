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

type
  TResourceStream = class(TCustomMemoryStream)
  public
    constructor Create(RType, RName: AnsiString);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

procedure AddResource(const RType, RName: AnsiString; const RData; RSize: LongWord);
procedure GetResource(const RType, RName: AnsiString; var RData: Pointer; var RSize: LongWord);
function ResourceExists(const RType, RName: AnsiString): Boolean;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

constructor TResourceStream.Create(RType, RName: AnsiString);
var
  P: Pointer;
  S: LongWord;
begin
  inherited Create;
  GetResource(RType, RName, P, S);
  SetPointer(P, S);
end;

function TResourceStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  raise EStreamError.Create('Can''t write to a TResourceStream.');
end;

type
  PResItem = ^TResItem;
  TResItem = record
    RType: AnsiString;
    RName: AnsiString;
    RData: Pointer;
    RSize: LongWord;
  end;

var
  ResourceList: TList;

procedure AddResource(const RType, RName: AnsiString; const RData; RSize: LongWord);
var
  NewRes: PResItem;
begin
  NewRes := New(PResItem);
  NewRes^.RType := RType;
  NewRes^.RName := RName;
  NewRes^.RData := @(RData);
  NewRes^.RSize := RSize;
  ResourceList.Add(NewRes);
end;

function ResourceExists(const RType, RName: AnsiString): Boolean;
var
  I: Integer;
  R: PResItem;
begin
  Result := False;
  for I := 0 to ResourceList.Count -1 do
  begin
    R := PResItem(ResourceList[I]);
    if (R^.RType = RType) and (R^.RName = RName) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure GetResource(const RType, RName: AnsiString; var RData: Pointer; var RSize: LongWord);
var
  I: Integer;
  R: PResItem;
begin
  for I := 0 to ResourceList.Count -1 do
  begin
    R := PResItem(ResourceList[I]);
    if (R^.RType = RType) and (R^.RName = RName) then
    begin
      RData := R^.RData;
      RSize := R^.RSize;
      exit;
    end;
  end;
  raise Exception.CreateFmt('Resource not found: %s\%s', [RType, RName]);
end;

{$ENDIF}
