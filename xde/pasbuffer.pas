(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit PasBuffer;

{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}
{$H+}

interface

uses Classes, SysUtils, xcl, Buffer, TxtBuffer;

type
  TPasBuffer = class(TTxtBuffer)
  protected
    procedure InitEdt; override;
  public
  end;

implementation

uses frm_main, xclsourceview;

{ TPasBuffer }

procedure TPasBuffer.InitEdt;
var
  Lang: TSourceLanguage;
begin
  inherited;
  Buf.Highlight := True;
  Lang := FrmMain.LangMan.GetLanguageFromMime('text/x-pascal');

  // String and Numbers
  Lang.SetTagStyle('String',                                                '#0000FF', '', False, False);
  Lang.SetTagStyle('Number',                                                '#0000FF', '', False, False);
  Lang.SetTagStyle('Hex@32@Number',                                         '#0000FF', '', False, False);
  // Comments
  Lang.SetTagStyle('Line@32@Comment',                                       '#00A000', '', False, False);
  Lang.SetTagStyle('Block@32@Comment@32@1',                                 '#00A000', '', False, False);
  Lang.SetTagStyle('Block@32@Comment@32@2',                                 '#00A000', '', False, False);
  Lang.SetTagStyle('Preprocessor@32@Defines',                               '#FF0000', '', False, False);
  //  Keywords
  Lang.SetTagStyle('General@32@Format',                                     '#0000A0', '', True, False);
  Lang.SetTagStyle('Functions@32@and@32@Function@32@Modifiers',             '#0000A0', '', True, False);
  Lang.SetTagStyle('Boolean@32@Bitwise@32@Operators',                       '#0000A0', '', True, False);
  Lang.SetTagStyle('Math@32@Operators',                                     '#0000A0', '', True, False);
  Lang.SetTagStyle('Loop@44@@32@Flow@44@@32@and@32@Exceptions@32@Keywords', '#0000A0', '', True, False);
  Lang.SetTagStyle('Type@44@@32@Class@32@and@32@Object@32@Keywords',        '#0000A0', '', True, False);
  Lang.SetTagStyle('Builtin@32@Types',                                      '#0000A0', '', False, False);
  Lang.SetTagStyle('Builtin@32@Functions',                                  '#0000A0', '', False, False);
  Lang.SetTagStyle('Builtin@32@Values',                                     '#0000A0', '', False, False);

  Buf.SetLanguage(Lang);

  Edt.ShowLineNumbers := True;
  Edt.AutoIndent := True;
  Edt.Margin := 80;
  Edt.ShowMargin := True;
  Edt.TabsWidth := 2;
  Edt.InsertSpacesInsteadOfTabs := True;
end;

end.
