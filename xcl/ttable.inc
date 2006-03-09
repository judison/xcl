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

  TTable = class(TContainerControl)
  private
    function GetHomogeneous: Boolean;
    procedure SetHomogeneous(AValue: Boolean);
    function GetNCols: Word;
    procedure SetNCols(AValue: Word);
    function GetNRows: Word;
    procedure SetNRows(AValue: Word);
  protected
    procedure CreateHandle; override;
  public
    procedure Resize(ARows, AColumns: Word);
  published
    property Homogeneous: Boolean read GetHomogeneous write SetHomogeneous;
    property NCols: Word read GetNCols write SetNCols;
    property NRows: Word read GetNRows write SetNRows;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TTable }

procedure TTable.CreateHandle;
begin
  Handle := gtk_table_new(1, 1, False);
end;

procedure TTable.Resize(ARows, AColumns: Word);
begin
  gtk_table_resize(Handle, ARows, AColumns);
end;

function TTable.GetHomogeneous: Boolean;
begin
  Result := gtk_table_get_homogeneous(Handle);
end;

procedure TTable.SetHomogeneous(AValue: Boolean);
begin
  gtk_table_set_homogeneous(Handle, AValue);
end;

function TTable.GetNCols: Word;
begin
  Result := PGtkTable(Handle)^.ncols;
end;

function TTable.GetNRows: Word;
begin
  Result := PGtkTable(Handle)^.nrows;
end;

procedure TTable.SetNCols(AValue: Word);
begin
  if AValue <> GetNCols then
    Resize(GetNRows, AValue);
end;

procedure TTable.SetNRows(AValue: Word);
begin
  if AValue <> GetNRows then
    Resize(AValue, GetNCols);
end;

{$ENDIF}
