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

  TControlActionLink = class(TActionLink)
  private
    FControl: TControl;
  protected
    procedure AssignClient(AClient: TObject); override;
    //--
    procedure SetCaption(AValue: String); override;
    procedure SetSensitive(AValue: Boolean); override;
    procedure SetStockID(AValue: String); override;
    procedure SetVisible(AValue: Boolean); override;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TControlActionLink }

procedure TControlActionLink.AssignClient(AClient: TObject);
begin
  FControl := TControl(AClient);
end;

procedure TControlActionLink.SetCaption(AValue: string);
begin
  FControl.DoActionSetCaption(AValue);
end;

procedure TControlActionLink.SetSensitive(AValue: Boolean);
begin
  FControl.DoActionSetSensitive(AValue);
end;

procedure TControlActionLink.SetStockID(AValue: string);
begin
  FControl.DoActionSetStockID(AValue);
end;

procedure TControlActionLink.SetVisible(AValue: Boolean);
begin
  FControl.DoActionSetVisible(AValue);
end;

{$ENDIF}
