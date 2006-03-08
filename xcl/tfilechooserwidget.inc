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

  TFileChooserWidget = class(TControl)
  {$I ifilechooser.inc}
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TFileChooserWidget }

{$define the_class := TFileChooserWidget }
  {$I ifilechooser.inc}
{$undef the_class}

procedure TFileChooserWidget.CreateHandle;
begin
  Handle := gtk_file_chooser_widget_new(Ord(fcaOpen));
end;

{$ENDIF}
