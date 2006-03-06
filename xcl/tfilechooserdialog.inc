(*
   $Id: tfilechooserdialog.inc,v 1.7 2005/11/18 20:06:58 judison Exp $

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

  TFileChooserDialog = class(TDialog)
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

{ TFileChooserDialog }

{$DEFINE INTF_IMPLEMENTATION}
  {$define the_class := TFileChooserDialog }
    {$I ifilechooser.inc}
  {$undef the_class}
{$UNDEF INTF_IMPLEMENTATION}

procedure TFileChooserDialog.CreateHandle;
begin
  // pensar num jeito melhor pra esses botoes....
  Handle := gtk_file_chooser_dialog_new(nil, nil, Ord(fcaOpen), GTK_STOCK_CANCEL, [GTK_RESPONSE_CANCEL,	GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, nil]);
end;

{$ENDIF}
{
$Log: tfilechooserdialog.inc,v $
Revision 1.7  2005/11/18 20:06:58  judison
long-term changes...

Revision 1.6  2005/03/28 05:52:42  judison
* GtkFileChooser Interface

Revision 1.5  2005/03/26 05:21:47  judison
+ CVS Log Tag

}