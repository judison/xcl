(*
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

  TToolItem = class(TCustomToolItem)
  private
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
    procedure SetAction(AValue: TAction); override;
    procedure DestroyHandle; override;
    procedure DoActionSetCaption(AValue: String); override;
    procedure DoActionSetStockID(AValue: String); override;
  public
    property SubLabel: TLabel read FLabel;
    property Image: TImage read FImage;
    property Button: TButton read FBtn;
  published
    property Action;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TToolItem }

procedure TToolItem.SetAction(AValue: TAction);
begin
  if Action <> nil then
    DestroyControls;

  if Child <> nil then
    raise Exception.Create('Cannot set Action in a TToolItem with a Child Control.')
  else
    inherited;

  if Action <> nil then
  begin
    CreateControls;
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
// TODO: Isso deve se tornar deprecated, usar TToolButton
procedure TToolItem.CreateControls;
begin
  Homogeneous := True;
  FBtn := TButton.Create(Self);
  FBtn.Relief := rlfNone;
  FBtn.FocusOnClick := False;
  FBtn.Sensitive := Action.Sensitive;
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
  FImage.StockIconSize := iszLargeToolBar;
  FImage.StockID := Action.StockID;
  //--
  FLabel := TLabel.Create(Self);
  if ToolBarStyle <> tbsIcons then
    FLabel.Parent := FBox;
  FLabel.UseUnderline := False;
  FLabel.Caption := Action.GetCaption;
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

procedure TToolItem.DoActionSetCaption(AValue: String);
begin
  FLabel.Caption := AValue;
end;

procedure TToolItem.DoActionSetStockID(AValue: String);
begin
  FImage.StockID := AValue;
end;

procedure TToolItem.FBtnClicked(Sender: TObject);
begin
  if ActionLink <> nil then
    ActionLink.Execute(Self);
end;

{$ENDIF}
