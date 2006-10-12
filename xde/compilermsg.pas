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

{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}
{$H+}

interface

uses Classes, SysUtils, XCL, ExCtrls;

type
  TCompilerMsgPage = class(TNotebookPageEx)
  private
    FSW: TScrolledWindow;
    FTV: TTreeView;
    FLS: TListStore;
    FParentNotebook: TNotebook;
  protected
    procedure CloseBtnClicked(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    //==
    procedure Clear;
    procedure Add(AStr: String);
    procedure Process(AStr: String);
    //==
    procedure Show; override;
    procedure Hide; override;
  end;

implementation

{ TCompilerMsgPage }

constructor TCompilerMsgPage.Create(AOwner: TComponent);
begin
  FParentNotebook := nil;
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

procedure TCompilerMsgPage.Show;
begin
  if Assigned(FParentNotebook) then
    Parent := FParentNotebook;
  inherited;
end;

procedure TCompilerMsgPage.Hide;
begin
  if Assigned(Parent) and (Parent is TNotebook) then
  begin
    FParentNotebook := TNotebook(Parent);
    Parent := nil;
  end;
  inherited;
end;

procedure TCompilerMsgPage.CloseBtnClicked(Sender: TObject);
begin
  Hide;
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
  //==
  Show;
end;

procedure TCompilerMsgPage.Process(AStr: String);
var
  P: Integer;
begin
  P := Pos(' Error: ', AStr);
  if P = 0 then
  begin
    P := Pos(' Warning: ', AStr);
    if P = 0 then
    begin
      P := Pos(' Fatal: ', AStr);
      if P = 0 then
      begin
        P := Pos(' Note: ', AStr);
        if P = 0 then
        begin
          P := Pos(' Hint: ', AStr);
        end;
      end;
    end;
  end;
  if P > 0 then
  begin
    Add(AStr);
  end;
end;

end.
