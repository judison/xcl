(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

//** INTERFACE
//****************************************************************************

{$IFDEF INTF_DECLARATION}
  private
    function GetSortColumn: Integer;
    function GetSortOrder: TSortType;
    procedure SetSortColumn(AValue: Integer);
    procedure SetSortOrder(AValue: TSortType);
  public
    procedure GetSortColumnID(var AColumn: Integer; var AOrder: TSortType);
    procedure SetSortColumnID(AColumn: Integer; AOrder: TSortType);
  published
    property SortColumn: Integer read GetSortColumn write SetSortColumn default -1;
    property SortOrder: TSortType read GetSortOrder write SetSortOrder default stAscending;
{$ENDIF}


//** IMPLEMENTATION
//****************************************************************************


{$IFDEF INTF_IMPLEMENTATION}

procedure the_class.GetSortColumnID(var AColumn: Integer; var AOrder: TSortType);
begin
  gtk_tree_sortable_get_sort_column_id(Handle, @AColumn, @AOrder);
end;

procedure the_class.SetSortColumnID(AColumn: Integer; AOrder: TSortType);
begin
  gtk_tree_sortable_set_sort_column_id(Handle, AColumn, Ord(AOrder));
end;

function the_class.GetSortColumn: Integer;
var
  dummy: TSortType;
begin
  {$WARNINGS OFF}
  GetSortColumnID(Result, dummy);
  {$WARNINGS ON}
end;

function the_class.GetSortOrder: TSortType;
var
  dummy: integer;
begin
  {$WARNINGS OFF}
  GetSortColumnID(dummy, Result);
  {$WARNINGS ON}
end;

procedure the_class.SetSortColumn(AValue: Integer);
var
  CurColumn: Integer;
  CurOrder: TSortType;
begin
  GetSortColumnID(CurColumn, CurOrder);
  if AValue <> CurColumn then
    SetSortColumnID(AValue, CurOrder);
end;

procedure the_class.SetSortOrder(AValue: TSortType);
var
  CurColumn: Integer;
  CurOrder: TSortType;
begin
  GetSortColumnID(CurColumn, CurOrder);
  if AValue <> CurOrder then
    SetSortColumnID(CurColumn, AValue);
end;

{$ENDIF}
