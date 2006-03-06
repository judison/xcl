(*
   $Id: tmenuitem.inc,v 1.7 2005/11/22 01:05:28 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{$IFDEF INC_READ_INTERFACE}
//** INTERFACE
//****************************************************************************

  TMenuItem = class(TCustomMenuItem)
  private
    FBox: THBox;
    FImage: TImage;
    FLabel: TLabel;
    FSizeGroup: Pointer;
    //---
    procedure SetCaption(AValue: string);
    function GetCaption: String;
    function IsCaptionStored: Boolean;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure SetParent(ANewParent: TControl); override;
    procedure SetAction(AValue: TAction); override;
    procedure DoActionSetCaption(AValue: String); override;
    procedure DoActionSetStockID(AValue: String); override;
    //---
  public
  published
    property Action;
    property Caption: String read GetCaption write SetCaption stored IsCaptionStored;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TMenuItem }

procedure TMenuItem.CreateHandle;
begin
  FSizeGroup := gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);

  Handle := gtk_menu_item_new;
  FBox := THBox.Create(Self);
  FBox.Spacing := 4;
  FImage := TImage.Create(Self);
  FImage.Parent := FBox;
  FImage.StockIconSize := iszMenu;
  FImage.BoxExpand := False;
  FImage.Visible := True;
  FLabel := TLabel.Create(Self);
  gtk_misc_set_alignment(FLabel.Handle, 0, 0);
  FLabel.Parent := FBox;
  FLabel.UseUnderline := True;
  gtk_container_add(Handle, FBox.Handle);
  inherited;
end;

procedure TMenuItem.DestroyHandle;
begin
  FBox.RemoveControl(FLabel);
  FLabel.Free;

  if Parent is TMenuItem then
    gtk_size_group_remove_widget(TMenuItem(Parent).FSizeGroup, FImage.Handle);

  FBox.RemoveControl(FImage);
  FImage.Free;
  gtk_container_remove(Handle, FBox.Handle);
  FBox.Free;
  inherited;
end;

procedure TMenuItem.SetParent(ANewParent: TControl);
begin
  if Parent is TMenuItem then
  begin
    gtk_size_group_remove_widget(TMenuItem(Parent).FSizeGroup, FImage.Handle);
  end;

  inherited;

  if Parent is TMenuItem then
  begin
    gtk_size_group_add_widget(TMenuItem(Parent).FSizeGroup, FImage.Handle);
    FImage.Visible := True;
  end
  else
  begin
    FImage.Visible := False;
  end;
end;

procedure TMenuItem.SetAction(AValue: TAction);
begin
  inherited SetAction(AValue);
  SetCaption(Action.GetCaption);
  FImage.StockID := Action.StockID;
end;

procedure TMenuItem.DoActionSetCaption(AValue: String); 
begin
  SetCaption(AValue);
end;

procedure TMenuItem.DoActionSetStockID(AValue: String);
begin
  FImage.StockID := AValue;
end;

function TMenuItem.GetCaption: string;
begin
  Result := FLabel.Caption;
end;

procedure TMenuItem.SetCaption(AValue: string);
begin
  FLabel.Caption := AValue;
end;

function TMenuItem.IsCaptionStored: Boolean;
begin
  Result := (Caption <> '') and (Action = nil);
end;

{$ENDIF}
{
  $Log: tmenuitem.inc,v $
  Revision 1.7  2005/11/22 01:05:28  judison
  * Action Support

  Revision 1.6  2005/03/27 09:53:51  judison
  * New Event System

  Revision 1.5  2005/03/27 09:00:21  judison
  * Bug Fix (Caption <> Text)

  Revision 1.4  2005/03/27 01:55:06  judison
  * Text -> Caption
  + IsCaptionStored

  Revision 1.3  2005/03/27 00:27:54  judison
  + TControlActionLink, TControl.Action
  + TCustomToolitem
  + TMenuItem.Action, TToolItem.Action
  + TControl.Visible

  Revision 1.2  2005/03/26 05:21:47  judison
  + CVS Log Tag

}