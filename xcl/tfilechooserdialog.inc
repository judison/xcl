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

  TFileChooserDialog = class(TDialog)
  {$I ifilechooser.inc}
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TFileChooserDialog }

{$define the_class := TFileChooserDialog }
  {$I ifilechooser.inc}
{$undef the_class}

procedure TFileChooserDialog.CreateHandle;
begin
  // pensar num jeito melhor pra esses botoes....
  Handle := gtk_file_chooser_dialog_new(nil, nil, Ord(fcaOpen), GTK_STOCK_CANCEL, [GTK_RESPONSE_CANCEL,	GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, nil]);
end;

{$ENDIF}
