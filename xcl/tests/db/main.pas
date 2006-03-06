(*
   $Id: main.pas,v 1.2 2005/11/22 01:05:28 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

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
{
$Log: main.pas,v $
Revision 1.2  2005/11/22 01:05:28  judison
* Action Support

Revision 1.1  2005/11/18 20:30:28  judison
*** empty log message ***

}