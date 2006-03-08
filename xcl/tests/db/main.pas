(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit main;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, SysUtils, xcl, xcldb, db, sdfdata;

type
  TMainForm = class(TForm)
    Table: TSdfDataSet;
    dsMain: TDataSource;
    procedure FileQuit(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Table.Open;
end;

procedure TMainForm.FileQuit(Sender: TObject);
begin
  Close;
end;

end.
