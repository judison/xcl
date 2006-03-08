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

  TButtonBox = class(TBox)
  private
    function GetLayout: TButtonBoxLayout;
    procedure SetLayout(AValue: TButtonBoxLayout);
  protected
  public
  published
    property Layout: TButtonBoxLayout read GetLayout write SetLayout default bblDefault;
  end;

  THButtonBox = class(TButtonBox)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

  TVButtonBox = class(TButtonBox)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TButtonBox }

function TButtonBox.GetLayout: TButtonBoxLayout;
begin
  Result := TButtonBoxLayout(gtk_button_box_get_layout(Handle));
end;

procedure TButtonBox.SetLayout(AValue: TButtonBoxLayout);
begin
  gtk_button_box_set_layout(Handle, Ord(AValue));
end;

{ THButtonBox }

procedure THButtonBox.CreateHandle;
begin
  Handle := gtk_hbutton_box_new;
end;

{ TVButtonBox }

procedure TVButtonBox.CreateHandle;
begin
  Handle := gtk_vbutton_box_new;
end;

{$ENDIF}
