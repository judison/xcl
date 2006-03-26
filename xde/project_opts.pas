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

uses Classes, XCL, XPR;

type
  TFrmProjectOpts = class(TForm)
    entOutputPath: TEntry;
    entUnitOutputPath: TEntry;
    entSearchPath: TEntry;
    entConditionals: TEntry;
    procedure BtnOKClicked(Sender: TObject);
    procedure BtnCancelClicked(Sender: TObject);
  private
    FProject: TXPRProject;
    Opt: TXPROptions;
    procedure SetProject(AValue: TXPRProject);
  protected
  public
    property Project: TXPRProject read FProject write SetProject;
  end;

var
  FrmProjectOpts: TFrmProjectOpts;

implementation

{ TFrmProjectOps }

procedure TFrmProjectOpts.SetProject(AValue: TXPRProject);
begin
  FProject := AValue;
  Opt := FProject.Options;
  //== Directories/Conditionals
  entOutputPath.Text := Opt.GetS('OutputPath');
  entUnitOutputPath.Text := Opt.GetS('UnitOutputPath');
  entSearchPath.Text := Opt.GetS('UnitSearchPath');
  entConditionals.Text := Opt.GetS('Conditionals');
end;

procedure TFrmProjectOpts.BtnOKClicked(Sender: TObject);
begin
  //== Directories/Conditionals
  Opt.SetS('OutputPath', entOutputPath.Text);
  Opt.SetS('UnitOutputPath', entUnitOutputPath.Text);
  Opt.SetS('UnitSearchPath', entSearchPath.Text);
  Opt.SetS('Conditionals', entConditionals.Text);
  //==
  Close;
end;

procedure TFrmProjectOpts.BtnCancelClicked(Sender: TObject);
begin
  Close;
end;

end.
