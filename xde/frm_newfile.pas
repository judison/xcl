(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

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
