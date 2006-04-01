(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit CompilerMsg;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, XCL, ExCtrls;

type
  TCompilerMsgPage = class(TNotebookPageEx)
  private
    FSW: TScrolledWindow;
    FTV: TTreeView;
    FLS: TListStore;
  protected
    procedure CloseBtnClicked(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    //==
    procedure Clear;
    procedure Add(AStr: String);
  end;

implementation

{ TCompilerMsgPage }

constructor TCompilerMsgPage.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Compiler Messages';
  //==
  FLS := TListStore.Create(Self);
  FLS.Structure := 'S';
  //--
  FSW := TScrolledWindow.Create(Self);
  FSW.Parent := Self;
  FSW.ShadowType := stIn;
  FSW.HPolicy := sbpAutomatic;
  FSW.VPolicy := sbpAutomatic;
  FTV := TTreeView.Create(Self);
  FTV.Parent := FSW;
  FTV.Model := FLS;
  FTV.HeadersVisible := False;
  FTV.SelectionMode := smBrowse;

  with FTV.Columns.Add do
  begin
    Clickable := False;
    Sizing := tvcsAutosize;
    Title := 'Messages';
  end;
end;

procedure TCompilerMsgPage.CloseBtnClicked(Sender: TObject);
begin
  Parent := nil;
end;

procedure TCompilerMsgPage.Clear;
begin
  FLS.Clear;
end;

procedure TCompilerMsgPage.Add(AStr: String);
var
  It: TTreeIter;
begin
  FLS.Append(It);
  FLS.SetStringValue(It, 0, AStr);
end;

end.