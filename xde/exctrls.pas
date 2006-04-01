(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit ExCtrls;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, XCL;

type
  TNotebookPageEx = class(TNotebookPage)
  private
  protected
    FCloseBtn: TButton;
    procedure CreateControls; override;
    procedure DestroyControls; override;
    //--
    // TODO: Make it an Event.
    procedure CloseBtnClicked(Sender: TObject); virtual;
  public
  end;

implementation

{ TNotebookPageEx }

procedure TNotebookPageEx.CreateControls;
begin
  inherited;
  FCloseBtn := TButton.Create(Self);
  FCloseBtn.IconName := 'gtk-close';
  FCloseBtn.IconSize := iszMenu;
  FCloseBtn.Relief := rlfNone;
  FCloseBtn.WidthRequest := 16;
  FCloseBtn.HeightRequest := 16;
  FCloseBtn.OnClicked := @CloseBtnClicked;
  FCloseBtn.Parent := FBox;
  FCloseBtn.BoxExpand := False;
end;

procedure TNotebookPageEx.DestroyControls;
begin
  if Assigned(FCloseBtn) then
  begin
    try FBox.RemoveControl(FCloseBtn); except end;
    FCloseBtn.Free;
    FCloseBtn := nil;
  end;
  inherited;
end;

procedure TNotebookPageEx.CloseBtnClicked(Sender: TObject);
begin
end;

end.