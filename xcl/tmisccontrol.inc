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

  TMiscControl = class(TControl)
  private
  protected
    function GetXPad: Integer;
    function GetYPad: Integer;
    procedure SetXPad(AValue: Integer);
    procedure SetYPad(AValue: Integer);
    function GetXAlign: Single;
    function GetYAlign: Single;
    procedure SetXAlign(AValue: Single);
    procedure SetYAlign(AValue: Single);
    function IsXAlignStored: Boolean;
    function IsYAlignStored: Boolean;
  public
    procedure GetPadding(var AXPad, AYPad: Integer);
    procedure SetPadding(AXPad, AYPad: Integer);
    procedure GetAlignment(var AXAlign, AYAlign: Single);
    procedure SetAlignment(AXAlign, AYAlign: Single);
  published
    property XPad: Integer read GetXPad write SetXPad default 0;
    property YPad: Integer read GetYPad write SetYPad default 0;
    property XAlign: Single read GetXAlign write SetXAlign stored IsXAlignStored;
    property YAlign: Single read GetYAlign write SetYAlign stored IsYAlignStored;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TMiscControl }

function TMiscControl.GetXPad: Integer;
var
  Dummy: Integer;
begin
  {$WARNINGS OFF}
  GetPadding(Result, Dummy);
  {$WARNINGS ON}
end;

function TMiscControl.GetYPad: Integer;
var
  Dummy: Integer;
begin
  {$WARNINGS OFF}
  GetPadding(Dummy, Result);
  {$WARNINGS ON}
end;

procedure TMiscControl.SetXPad(AValue: Integer);
var
  LX, LY: Integer;
begin
  GetPadding(LX, LY);
  SetPadding(AValue, LY);
end;

procedure TMiscControl.SetYPad(AValue: Integer);
var
  LX, LY: Integer;
begin
  GetPadding(LX, LY);
  SetPadding(LX, AValue);
end;

function TMiscControl.GetXAlign: Single;
var
  Dummy: Single;
begin
  {$WARNINGS OFF}
  GetAlignment(Result, Dummy);
  {$WARNINGS ON}
end;

function TMiscControl.GetYAlign: Single;
var
  Dummy: Single;
begin
  {$WARNINGS OFF}
  GetAlignment(Dummy, Result);
  {$WARNINGS ON}
end;

procedure TMiscControl.SetXAlign(AValue: Single);
var
  LX, LY: Single;
begin
  GetAlignment(LX, LY);
  SetAlignment(AValue, LY);
end;

procedure TMiscControl.SetYAlign(AValue: Single);
var
  LX, LY: Single;
begin
  GetAlignment(LX, LY);
  SetAlignment(LX, AValue);
end;

function TMiscControl.IsXAlignStored: Boolean;
begin
  Result := GetXAlign <> 0.5;
end;

function TMiscControl.IsYAlignStored: Boolean;
begin
  Result := GetYAlign <> 0.5;
end;

procedure TMiscControl.GetPadding(var AXPad, AYPad: Integer);
begin
  gtk_misc_get_padding(Handle, @AXPad, @AYPad);
end;

procedure TMiscControl.SetPadding(AXPad, AYPad: Integer);
begin
  gtk_misc_set_padding(Handle, AXPad, AYPad);
end;

procedure TMiscControl.GetAlignment(var AXAlign, AYAlign: Single);
begin
  gtk_misc_get_alignment(Handle, @AXAlign, @AYAlign);
end;

procedure TMiscControl.SetAlignment(AXAlign, AYAlign: Single);
begin
  gtk_misc_set_alignment(Handle, AXAlign, AYAlign);
end;

{$ENDIF}
