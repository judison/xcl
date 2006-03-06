(*
   $Id: compiler_opts.pas,v 1.3 2005/12/18 05:05:59 judison Exp $

   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING, included in this distribution, for
   details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit compiler_opts;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, xcl;

type
  TFrmCompilerOpts = class(TForm)
    procedure BtnOKClicked(Sender: TObject);
    procedure BtnCancelClicked(Sender: TObject);
  private
  protected
  public
  end;

var
  FrmCompilerOpts: TFrmCompilerOpts;

implementation

{ TFrmCompilerOps }

procedure TFrmCompilerOpts.BtnOKClicked(Sender: TObject);
begin
  Close;
end;

procedure TFrmCompilerOpts.BtnCancelClicked(Sender: TObject);
begin
  Close;
end;


end.
{
  $Log: compiler_opts.pas,v $
  Revision 1.3  2005/12/18 05:05:59  judison
  copyleft stuff

  Revision 1.2  2005/12/17 21:48:30  judison
  *** empty log message ***

  Revision 1.1.1.1  2005/12/17 17:29:39  judison
  Initial Import

  Revision 1.1  2005/03/29 00:57:27  judison
  + Compiler Options

}