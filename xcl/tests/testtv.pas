(*
   $Id: testtv.pas,v 1.2 2006/02/28 16:22:58 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

program testtv;
{$H+}
{$Mode ObjFpc}

uses Classes, SysUtils, xcl, testtv_xrc;

type
  TTestTV = class(TForm)
    TV: TTreeView;
    TS: TTreeStore;
    procedure CloseFrm(Sender: TObject);
    procedure MyFormShow(Sender: TObject);
    procedure AddTree(C: TComponent; P: TTreeIter);
  end;

procedure TTestTV.CloseFrm(Sender: TObject);
begin
  Close;
end;

procedure TTestTV.AddTree(C: TComponent; P: TTreeIter);
var
  It: TTreeIter;
  I: Integer;
begin
  TS.Append(It, P);
  TS.SetStringValue(It, 0, C.ClassName);
  TS.SetStringValue(It, 1, C.Name);

  if C is TControl then
    for I := 0 to TControl(C).ControlCount -1 do
      AddTree(TControl(C).Controls[I], It);
end;

procedure TTestTV.MyFormShow(Sender: TObject);
var
  It: TTreeIter;
  I: Integer;
begin
  TS.Append(It);
  TS.SetStringValue(It, 0, ClassName);
  TS.SetStringValue(It, 1, Name);
  for I := 0 to ComponentCount -1 do
    if (not (Components[I] is TControl)) and (not (Components[I] is TCustomAction))  then
      AddTree(Components[I], It);
  for I := 0 to ControlCount -1 do
      AddTree(Controls[I], It);
end;


var
  MyForm : TTestTV;
begin
  Application.Initialize;
  Application.CreateForm(TTestTV, MyForm);
  Application.Run;
end.
{
  $Log: testtv.pas,v $
  Revision 1.2  2006/02/28 16:22:58  judison
  New resource system

  Revision 1.1  2005/11/18 20:24:46  judison
  *** empty log message ***

}