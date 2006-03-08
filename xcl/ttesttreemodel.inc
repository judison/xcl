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

  TTestTreeModel = class(TTreeModel)
  private
  protected
  public
    // ------------------
    function TMGetFlags: Longint; override;
    function TMGetNColumns: Integer; override;
    function TMGetColumnType(AIdx: Integer): DWord; override;
    function TMGetIter(var AIter: TTreeIter; APath: String): Boolean; override;
    function TMGetPath(const AIter: TTreeIter): String; override;
    function TMGetValue(const AIter: TTreeIter; AColumn: Integer; AValue: Pointer): Boolean; override;
    function TMIterNext(var AIter: TTreeIter): Boolean; override;
    function TMIterChildren(var AIter: TTreeIter; const AParent: TTreeIter): Boolean; override;
    function TMIterHasChild(const AIter: TTreeIter): Boolean; override;
    function TMIterNChildren(const AIter: TTreeIter): Integer; override;
    function TMIterNthChildren(var AIter: TTreeIter; const AParent: TTreeIter; AN: Integer): Boolean; override;
    function TMIterParent(var AIter: TTreeIter; const AChild: TTreeIter): Boolean; override;
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TTestTreeModel }

function TTestTreeModel.TMGetFlags: Longint;
begin
  WriteLn('TMGetFlags');
  Result := 0;
end;

function TTestTreeModel.TMGetNColumns: Integer;
begin
  WriteLn('TMGetNColumns');
  Result := 1;
end;

function TTestTreeModel.TMGetColumnType(AIdx: Integer): DWord;
begin
  WriteLn('TMGetColumnType(', AIdx, ')');
  Result := G_TYPE_STRING;
end;

function TTestTreeModel.TMGetIter(var AIter: TTreeIter; APath: String): Boolean;
begin
  WriteLn('TMGetIter("',APath,'")');
  AIter.Stamp := StrToInt(APath)+1;
  Result := True;
end;

function TTestTreeModel.TMGetPath(const AIter: TTreeIter): String;
begin
  WriteLn('TMGetPath');
  Result := IntToStr(AIter.Stamp-1);
end;

function TTestTreeModel.TMGetValue(const AIter: TTreeIter; AColumn: Integer; AValue: Pointer): Boolean;
begin
  WriteLn('TMGetValue');
  g_value_set_string(AValue, PChar(Format('Line %d', [AIter.Stamp -1])));
  Result := True;
end;

function TTestTreeModel.TMIterNext(var AIter: TTreeIter): Boolean;
begin
  WriteLn('TMIterNext');
  if AIter.Stamp < 8 then
  begin
    AIter.Stamp := AIter.Stamp +1;
    Result := True;
  end
  else
    Result := False;
end;

function TTestTreeModel.TMIterChildren(var AIter: TTreeIter; const AParent: TTreeIter): Boolean;
begin
  WriteLn('TMIterChildren');
  Result := False;
end;

function TTestTreeModel.TMIterHasChild(const AIter: TTreeIter): Boolean;
begin
  WriteLn('TMIterHasChild');
  Result := False;
end;

function TTestTreeModel.TMIterNChildren(const AIter: TTreeIter): Integer;
begin
  WriteLn('TMIterNChildren');
  Result := 0;
end;

function TTestTreeModel.TMIterNthChildren(var AIter: TTreeIter; const AParent: TTreeIter; AN: Integer): Boolean;
begin
  WriteLn('TMIterNthChildren');
  Result := False;
end;

function TTestTreeModel.TMIterParent(var AIter: TTreeIter; const AChild: TTreeIter): Boolean;
begin
  WriteLn('TMIterParent');
  Result := False;
end;

{$ENDIF}
