(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution, for
   details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit Project_Opts;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, XCL;

type
  TFrmProjectOpts = class(TForm)
    procedure BtnOKClicked(Sender: TObject);
    procedure BtnCancelClicked(Sender: TObject);
  private
  protected
  public
  end;

var
  FrmProjectOpts: TFrmProjectOpts;

implementation

{ TFrmProjectOps }

procedure TFrmProjectOpts.BtnOKClicked(Sender: TObject);
begin
  Close;
end;

procedure TFrmProjectOpts.BtnCancelClicked(Sender: TObject);
begin
  Close;
end;

end.
