(*
   $Id: frm_newfile.pas,v 1.2 2005/12/18 05:05:59 judison Exp $

   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit frm_newfile;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, xcl;

type
  TFrmNewFile = class(TForm)
    procedure BtnClicked(Sender: TObject);
  public
  end;

var
  frmNewFile: TFrmNewFile;

implementation

{ TFrmNewFile }

procedure TFrmNewFile.BtnClicked(Sender: TObject);
begin
  Tag := TComponent(Sender).Tag;
  Close;
end;

end.
{
$Log: frm_newfile.pas,v $
Revision 1.2  2005/12/18 05:05:59  judison
copyleft stuff

Revision 1.1.1.1  2005/12/17 17:29:39  judison
Initial Import

Revision 1.2  2005/12/08 22:44:23  judison
Long term changes

Revision 1.1  2005/12/02 22:31:34  judison
Long-term changes (again) I need to use cvs more!


}
