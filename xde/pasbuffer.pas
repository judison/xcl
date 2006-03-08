(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

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
