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

  TScale = class(TRange)
  private
    function GetDigits: Integer;
    function GetDrawValue: Boolean;
    function GetValuePos: TPosition;
    procedure SetDigits(AValue: Integer);
    procedure SetDrawValue(AValue: Boolean);
    procedure SetValuePos(AValue: TPosition);
  protected
  public
  published
    property Digits: Integer read GetDigits write SetDigits default 0; // default GTK's value is 1
    property DrawValue: Boolean read GetDrawValue write SetDrawValue default True;
    property ValuePos: TPosition read GetValuePos write SetValuePos default psTop;
  end;

  THScale = class(TScale)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

  TVScale = class(TScale)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TScale }

function TScale.GetDigits: Integer;
begin
  Result := gtk_scale_get_digits(Handle);
end;

function TScale.GetDrawValue: Boolean;
begin
  Result := gtk_scale_get_draw_value(Handle);
end;

function TScale.GetValuePos: TPosition;
begin
  Result := TPosition(gtk_scale_get_value_pos(Handle));
end;

procedure TScale.SetDigits(AValue: Integer);
begin
  gtk_scale_set_digits(Handle, AValue);
end;

procedure TScale.SetDrawValue(AValue: Boolean);
begin
  gtk_scale_set_draw_value(Handle, AValue);
end;

procedure TScale.SetValuePos(AValue: TPosition);
begin
  gtk_scale_set_value_pos(Handle, Ord(AValue));
end;

{ THScale }

procedure THScale.CreateHandle;
begin
  CreateMyAdj;
  Handle := gtk_hscale_new(FAdjustment.Handle);
  SetDigits(0);
end;

{ TVScale }

procedure TVScale.CreateHandle;
begin
  CreateMyAdj;
  Handle := gtk_vscale_new(FAdjustment.Handle);
  SetDigits(0);
end;

{$ENDIF}
