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

  TFileChooserWidget = class(TControl)
  private
    {$DEFINE INTF_PRIVATE}{$I ifilechooser.inc}{$UNDEF INTF_PRIVATE}
  protected
    {$DEFINE INTF_PROTECTED}{$I ifilechooser.inc}{$UNDEF INTF_PROTECTED}
    procedure CreateHandle; override;
  public
    {$DEFINE INTF_PUBLIC}{$I ifilechooser.inc}{$UNDEF INTF_PUBLIC}
  published
    {$DEFINE INTF_PUBLISHED}{$I ifilechooser.inc}{$UNDEF INTF_PUBLISHED}
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TFileChooserWidget }

{$DEFINE INTF_IMPLEMENTATION}
  {$define the_class := TFileChooserWidget }
    {$I ifilechooser.inc}
  {$undef the_class}
{$UNDEF INTF_IMPLEMENTATION}

procedure TFileChooserWidget.CreateHandle;
begin
  Handle := gtk_file_chooser_widget_new(Ord(fcaOpen));
end;

{$ENDIF}
