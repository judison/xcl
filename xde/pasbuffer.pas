(*
   $Id: pasbuffer.pas,v 1.4 2006/02/25 19:19:16 judison Exp $

   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit pasbuffer;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, xcl, Buffer, TxtBuffer;

type
  TPasBuffer = class(TTxtBuffer)
  protected
    procedure InitEdt; override;
  public
  end;

implementation

uses main;

{ TPasBuffer }

procedure TPasBuffer.InitEdt;
begin
  inherited;
  Buf.Highlight := True;
  Buf.SetLanguage(MainForm.LangMan.GetLanguageFromMime('text/x-pascal'));

  Edt.ShowLineNumbers := True;
  Edt.AutoIndent := True;
  Edt.Margin := 80;
  Edt.ShowMargin := True;
  Edt.TabsWidth := 2;
  Edt.InsertSpacesInsteadOfTabs := True;
end;

end.
{
$Log: pasbuffer.pas,v $
Revision 1.4  2006/02/25 19:19:16  judison
Added GtkSourceView/XCLSourceView
Removed all deps on Scintilla

Revision 1.3  2005/12/18 05:05:59  judison
copyleft stuff

Revision 1.2  2005/12/17 21:48:30  judison
*** empty log message ***

Revision 1.1.1.1  2005/12/17 17:29:44  judison
Initial Import

Revision 1.3  2005/12/09 02:47:17  judison
Added TTxtBuffer, now TPasBuffer derives from TTxtBuffer

Revision 1.2  2005/12/08 22:44:23  judison
Long term changes

Revision 1.1  2005/12/02 22:31:34  judison
Long-term changes (again) I need to use cvs more!


}
