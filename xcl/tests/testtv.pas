(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

program testtv;

{$Mode ObjFpc}
{$H+}

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
