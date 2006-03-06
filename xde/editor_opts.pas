(*
   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit editor_opts;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, xcl;

type
  TFrmEditorOpts = class(TForm)
    procedure BtnOKClicked(Sender: TObject);
    procedure BtnCancelClicked(Sender: TObject);
  private
  protected
  public
  end;

var
  FrmEditorOpts: TFrmEditorOpts;

implementation

{ TFrmEditorOpts }

procedure TFrmEditorOpts.BtnOKClicked(Sender: TObject);
begin
  Close;
end;

procedure TFrmEditorOpts.BtnCancelClicked(Sender: TObject);
begin
  Close;
end;

end.
