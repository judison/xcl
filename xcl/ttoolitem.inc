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

  TToolItem = class(TCustomToolItem, IActionClient)
  private
    FAction: TCustomAction;
    //--
    FBtn: TButton;
    FBox: TBox;
    FImage: TImage;
    FLabel: TLabel;
    procedure CreateControls;
    procedure DestroyControls;
    procedure FBtnClicked(Sender: TObject);
  protected
    procedure AddControl(AControl: TControl); override;
    procedure DestroyHandle; override;
    //--
    procedure SetAction(AValue: TCustomAction); virtual;
    procedure DoActionChanged(Sender: TCustomAction); virtual;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property SubLabel: TLabel read FLabel;
    property Image: TImage read FImage;
    property Button: TButton read FBtn;
  published
    property Action: TCustomAction read FAction write SetAction;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TToolItem }

procedure TToolItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FAction) and (Operation = opRemove) then
  begin
    DestroyControls;
    FAction := nil;
  end;
  inherited;
end;

procedure TToolItem.SetAction(AValue: TCustomAction);
begin
  if FAction <> AValue then
  begin
    if Assigned(FAction) then
      DestroyControls;

    if Assigned(Child) then
      raise Exception.Create('Cannot set Action in a TToolItem with a Child Control.')
    else
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
    end;

    if Assigned(FAction) then
    begin
      CreateControls;
      DoActionChanged(FAction);
    end;
  end;
end;

procedure TToolItem.AddControl(AControl: TControl);
begin
  if Action <> nil then
    raise Exception.Create('Cannot insert a Child Control in a TToolItem with Action set.')
  else
    inherited;
end;

procedure TToolItem.DestroyHandle;
begin
  DestroyControls;
  inherited;
end;

// TODO: ToolBarStyle diferente e tudo o mais
// levar em consideracao o IsImportant, Orientation e ToolBarStyle
procedure TToolItem.CreateControls;
begin
  if Parent is TToolBar then
    Homogeneous := True;

  FBtn := TButton.Create(Self);
  FBtn.Relief := rlfNone;
  FBtn.FocusOnClick := False;
  FBtn.OnClicked := @FBtnClicked;
  //--
  if Orientation = orHorizontal then
    FBox := TVBox.Create(Self)
  else
    FBox := THBox.Create(Self);
  FBox.Parent := FBtn;
  //--
  FImage := TImage.Create(Self);
  if ToolBarStyle <> tbsText then
    FImage.Parent := FBox;
  if SmallIcons then
    FImage.IconSize := iszSmallToolBar
  else
    FImage.IconSize := iszLargeToolBar;
  //--
  FLabel := TLabel.Create(Self);
  if ToolBarStyle <> tbsIcons then
    FLabel.Parent := FBox;
  FLabel.UseUnderline := False;
  //--
  gtk_container_add(Handle, FBtn.Handle);
  gtk_widget_show_all(FBtn.Handle);
end;

procedure TToolItem.DestroyControls;
begin
  if FBtn <> nil then
  begin
    try FBox.RemoveControl(FLabel); except end;
    FLabel.Free;
    try FBox.RemoveControl(FImage); except end;
    FImage.Free;
    FBtn.RemoveControl(FBox);
    FBox.Free;
    gtk_container_remove(Handle, FBtn.Handle);
    FBtn.Free;
    FBtn := nil;
  end;
end;

procedure TToolItem.DoActionChanged(Sender: TCustomAction);
begin
  if Sender = FAction then
  begin
    FLabel.Caption := FAction.Caption;
    FImage.IconName := FAction.IconName;
    FBtn.ToolTip := FAction.ToolTip;
    FBtn.Sensitive := FAction.Sensitive;
    Visible := FAction.Visible;
  end;
end;

procedure TToolItem.FBtnClicked(Sender: TObject);
begin
  if FAction <> nil then
    FAction.Execute;
end;

{$ENDIF}
