(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{** INTERFACE *************************************}{$IFDEF INC_READ_INTERFACE}

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

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TMenuItem }

procedure TMenuItem.CreateHandle;
begin
  FSizeGroup := gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);

  Handle := gtk_menu_item_new;
  FBox := THBox.Create(Self);
  FBox.Spacing := 4;
  FImage := TImage.Create(Self);
  FImage.Parent := FBox;
  FImage.IconSize := iszMenu;
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
