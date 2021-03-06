(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit componentpalette;

{$MODE ObjFpc}
{$H+}

interface

uses Classes, SysUtils, xcl;

type
  TClassSelectedNotify = procedure (Sender: TObject; AClass: TComponentClass) of object;

  TComponentPalette = class(TNotebook)
  private
    FOnClassSelected: TClassSelectedNotify;
    procedure BtnClicked(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Build;
  published
    property OnClassSelected: TClassSelectedNotify read FOnClassSelected write FOnClassSelected;
  end;

implementation

uses regcomps;

{ TComponentButton }
type
  TComponentButton = class(TButton)
  private
    FCompClass: TComponentClass;
  public
    property CompClass: TComponentClass read FCompClass write FCompClass;
  end; 

{ TComponentPalette }

constructor TComponentPalette.Create(AOwner: TComponent);
begin
  inherited;
  Build;
end;

procedure TComponentPalette.Build;
var
  I,J: Integer;
  pg: TComponentPage;
  NBP: TNotebookPage;
{
  SW: TScrolledWindow;
  VP: TViewPort;
}
  Box: THBox;
  Btn: TComponentButton;
  Img: TImage;
  R: String;
begin
  Scrollable := True;

  for I := 0 to ComponentCount -1 do
    Components[I].Free;

  for I := 0 to ComponentPages.Count-1 do
  begin
    pg := TComponentPage(ComponentPages.Items[I]);
    NBP := TNotebookPage.Create(Self);
    NBP.Parent := Self;
    NBP.Caption := pg.Name;
{
    SW := TScrolledWindow.Create(Self);
    SW.Parent := NBP;
    SW.HPolicy := sbpAutomatic;
    SW.VPolicy := sbpNever;
    SW.HeightRequest := 23;
    VP := TViewPort.Create(Self);
    VP.Parent := SW;
    VP.ShadowType := stNone;
}
    Box := THBox.Create(Self);
    Box.Parent := NBP;
    for J := 0 to pg.Classes.Count -1 do
    begin
      Btn := TComponentButton.Create(Self);
      Btn.Parent := Box;
      Btn.BoxExpand := False;

      Btn.Relief := rlfNone;
      //Btn.Tag := Integer(TComponentClass(pg.Classes[J]));
      Btn.CompClass := TComponentClass(pg.Classes[J]);
      Btn.OnClicked := @BtnClicked;
      Btn.ToolTip := TComponentClass(pg.Classes[J]).ClassName;

      Img := TImage.Create(Self);
      Img.Parent := Btn;

      R := Lowercase(TComponentClass(pg.Classes[J]).ClassName);
      if ResourceExists('PIXDATA', R) then
        Img.Resource := R
      else
        Img.Resource := 'unknown';
    end;
  end;
end;

procedure TComponentPalette.BtnClicked(Sender: TObject);
begin
  if Assigned(FOnClassSelected) then
    //FOnClassSelected(Self, TComponentClass(Pointer(TComponent(Sender).Tag)));
    FOnClassSelected(Self, TComponentButton(Sender).CompClass);
end;

end.
