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

  TMenuItem = class(TCustomMenuItem, IActionClient)
  private
    FAction: TCustomAction;
    //--
    FBox: THBox;
    FImage: TImage;
    FLabel: TLabel;
    FAccel: TLabel;
    FIconSizeGroup: Pointer;
    FLabelSizeGroup: Pointer;
    //---
    procedure SetCaption(AValue: string);
    function GetCaption: String;
    function IsCaptionStored: Boolean;
  protected
    procedure SetAction(AValue: TCustomAction); virtual;
    procedure DoActionChanged(Sender: TCustomAction); virtual;
    //--
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure SetParent(ANewParent: TControl); override;
    //--
    procedure DoActivate(var Msg: TGtkSig); message 'GTK:activate'; override;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Action: TCustomAction read FAction write SetAction;
    property Caption: String read GetCaption write SetCaption stored IsCaptionStored;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TMenuItem }

procedure TMenuItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FAction) and (Operation = opRemove) then
  begin
    FAction := nil;
  end;
  inherited;
end;


procedure TMenuItem.CreateHandle;
begin
  FIconSizeGroup := gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
  FLabelSizeGroup := gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);

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
    
      FAccel := TLabel.Create(Self);
      gtk_misc_set_alignment(FAccel.Handle, 1, 0);
      FAccel.Parent := FBox;
      FAccel.UseUnderline := True;

    gtk_container_add(Handle, FBox.Handle);
  inherited;
end;

procedure TMenuItem.DestroyHandle;
begin
  FBox.RemoveControl(FLabel);
  FLabel.Free;
  FAccel.Free;

  if Parent is TMenuItem then
  begin
    gtk_size_group_remove_widget(TMenuItem(Parent).FIconSizeGroup, FImage.Handle);
    gtk_size_group_remove_widget(TMenuItem(Parent).FLabelSizeGroup, FLabel.Handle);
  end;

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
    gtk_size_group_remove_widget(TMenuItem(Parent).FIconSizeGroup, FImage.Handle);
    gtk_size_group_remove_widget(TMenuItem(Parent).FLabelSizeGroup, FLabel.Handle);
  end;

  inherited;

  if Parent is TMenuItem then
  begin
    gtk_size_group_add_widget(TMenuItem(Parent).FIconSizeGroup, FImage.Handle);
    gtk_size_group_add_widget(TMenuItem(Parent).FLabelSizeGroup, FLabel.Handle);
    FImage.Visible := True;
  end
  else
  begin
    FImage.Visible := False;
  end;
end;

procedure TMenuItem.SetAction(AValue: TCustomAction);
begin
  if Assigned(FAction) then
  begin
    FAction.RemoveClient(Self);
    FAction.RemoveFreeNotification(Self);
  end;
  FAction := AValue;
  if Assigned(FAction) then
  begin
    FAction.AddClient(Self);
    FAction.FreeNotification(Self);
  end;

  if Assigned(FAction) then
  begin
    FAccel.Caption := FAction.GetAccelLabel;
    SetCaption(FAction.Caption);
    FImage.IconName := FAction.IconName;
    //ToolTip := FAction.ToolTip;
    Sensitive := FAction.Sensitive;
    Visible := FAction.Visible;
  end;
end;

procedure TMenuItem.DoActionChanged(Sender: TCustomAction);
begin
  if Sender = FAction then
  begin
    FAccel.Caption := FAction.GetAccelLabel;
    SetCaption(FAction.Caption);
    FImage.IconName := FAction.IconName;
    //ToolTip := FAction.ToolTip;
    Sensitive := FAction.Sensitive;
    Visible := FAction.Visible;
  end;
end;

procedure TMenuItem.DoActivate(var Msg: TGtkSig);
begin
  if Assigned(FAction) then
    FAction.Execute
  else
    inherited;
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
  Result := (Caption <> '') and (FAction = nil);
end;

{$ENDIF}
