(*
   $Id

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
//** INTERFACE
//****************************************************************************
{$IFDEF INC_READ_INTERFACE}

  IDesigner = interface
    function IsDesignMsg(Sender: TControl; var Message): Boolean;
    procedure Modified;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure PaintGrid;
    procedure ValidateRename(AComponent: TComponent; const CurName, NewName: string);
    function GetShiftState: TShiftState;
    Procedure SelectOnlyThisComponent(AComponent: TComponent);
    function UniqueName(const BaseName: string): string;
  end;

{$ENDIF}
{
$Log
}