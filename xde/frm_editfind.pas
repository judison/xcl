(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution, for
   details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit frm_EditFind;

{$MODE ObjFpc}
{$H+}

interface

uses Classes, xcl;

type
  TFrmEditFind = class(TForm)
    procedure BtnFindClicked(Sender: TObject);
    procedure BtnCancelClicked(Sender: TObject);
  private
  protected
  public
  end;

var
  FrmEditFind: TFrmEditFind;

implementation

{ TFrmEditFind }

procedure TFrmEditFind.BtnFindClicked(Sender: TObject);
begin
  Close;
end;

procedure TFrmEditFind.BtnCancelClicked(Sender: TObject);
begin
  Close;
end;

end.
