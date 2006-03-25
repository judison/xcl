(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution, for
   details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit CompilingDlg;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, SysUtils, XCL;

type
  TFrmCompilingDlg = class(TForm)
    lblProject: TLabel;
    lblStatus: TLabel;
    lblHints: TLabel;
    lblWarnings: TLabel;
    lblErrors: TLabel;
    Btn: TButton;
  private
  protected
  public
  end;

var
  FrmCompilingDlg: TFrmCompilingDlg;

implementation

{ TFrmCompilingDlg }


end.
