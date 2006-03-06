(*
   $Id: componentpalette.pas,v 1.4 2006/03/04 20:15:54 judison Exp $

   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit componentpalette;

{$H+}
{$MODE ObjFpc}

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
  TB: TToolBar;
  TI: TToolItem;
  Btn: TButton;
  Img: TImage;
  R: String;
begin
  for I := 0 to ComponentCount -1 do
    Components[I].Free;

  for I := 0 to ComponentPages.Count-1 do
  begin
    pg := TComponentPage(ComponentPages.Items[I]);
    NBP := TNotebookPage.Create(Self);
    NBP.Parent := Self;
    NBP.Caption := pg.Name;
    TB := TToolBar.Create(Self);
    TB.Parent := NBP;
    for J := 0 to pg.Classes.Count -1 do
    begin
      TI := TToolItem.Create(Self);
      TI.Parent := TB;
      Btn := TButton.Create(Self);
      Btn.Parent := TI;
      Btn.Relief := rlfNone;
      Btn.Tag := Integer(TComponentClass(pg.Classes[J]));
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
    FOnClassSelected(Self, TComponentClass(Pointer(TComponent(Sender).Tag)));
end;

end.
{
$Log: componentpalette.pas,v $
Revision 1.4  2006/03/04 20:15:54  judison
Now uses ToolTips
icons_xrc.pp permanent

Revision 1.3  2006/03/01 20:54:38  judison
Now using TImage.Resource

Revision 1.2  2006/03/01 18:41:13  judison
Changes to use new PIXDATA resources

Revision 1.1  2006/02/27 15:16:48  judison
*** empty log message ***

}