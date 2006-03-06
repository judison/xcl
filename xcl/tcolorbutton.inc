(*
   $Id$

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

  TColorButton = class(TCustomButton)
  private
    procedure SetColor(AValue: TColor);
    function GetColor: TColor;
  protected
    procedure CreateHandle; override;
  public
  published
    property Color: TColor read GetColor write SetColor;
  end;


{$ENDIF}
//** IMPLEMENTATION
//****************************************************************************
{$IFDEF INC_READ_IMPLEMENTATION}

{ TColorButton }

procedure TColorButton.CreateHandle;
begin
  Handle := gtk_color_button_new();
end;

procedure TColorButton.SetColor(AValue: TColor);
var
  Data: TGdkColor;
begin
  gdk_color_parse(PChar(AValue), @Data);
  gtk_color_button_set_color(Handle, @Data);
end;

function TColorButton.GetColor: TColor;
var
  Data: TGdkColor;
begin
  gtk_color_button_get_color(Handle, @Data);
  Result := GdkColorToColor(@Data);
end;

{$ENDIF}
{
$Log$
}