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

  TActionLink = class(TBasicActionLink)
  protected
    procedure SetCaption(AValue: String); virtual;
    procedure SetStockID(AValue: String); virtual;
    procedure SetSensitive(AValue: Boolean); virtual;
    procedure SetVisible(AValue: Boolean); virtual;
  end;

  TActionLinkClass = class of TActionLink;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TActionLink }

procedure TActionLink.SetCaption(AValue: string);
begin
end;

procedure TActionLink.SetStockID(AValue: string);
begin
end;

procedure TActionLink.SetSensitive(AValue: Boolean);
begin
end;

procedure TActionLink.SetVisible(AValue: Boolean);
begin
end;


{$ENDIF}
