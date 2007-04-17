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

  TCustomButton = class(TBinControl, IActionClient)
  private
    FAction: TCustomAction;
    //--
    FCaption: String;
    FIconName: String;
    //--
    FOnClicked: TNotifyEvent;
    function GetFocusOnClick: Boolean;
    function GetRelief: TRelief;
    function GetUseUnderline: Boolean;
    procedure SetCaption(AValue: string);
    procedure SetIconName(AValue: string);
    procedure SetFocusOnClick(AValue: Boolean);
    procedure SetRelief(AValue: TRelief);
    procedure SetUseUnderline(AValue: Boolean);
    function GetIconSize: TIconSize;
    procedure SetIconSize(AValue: TIconSize);
  protected
    FAlignment: TAlignment;
    FBox: TBox;
    FImage: TImage;
    FLabel: TLabel;
    //--
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure ConnectSignals; override;
    //--
    procedure CreateControls; virtual;
    procedure DestroyControls; virtual;
    procedure UpdateControls; virtual;
    //--
    procedure AddControl(AControl: TControl); override;
    procedure RemoveControl(AControl: TControl); override;
    //--
    procedure SetAction(AValue: TCustomAction); virtual;
    procedure DoActionChanged(Sender: TCustomAction); virtual;
    //--
    procedure DoClicked(var Msg: TGtkSig); message 'GTK:clicked';
    // Not published yet
    property Action: TCustomAction read FAction write SetAction;
    property Caption: string read FCaption write SetCaption;
    property IconName: string read FIconName write SetIconName;
    property UseUnderline: Boolean read GetUseUnderline write SetUseUnderline default True;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    //--
    property IconSize: TIconSize read GetIconSize write SetIconSize;
  published
    property FocusOnClick: Boolean read GetFocusOnClick write SetFocusOnClick default True;
    property Relief: TRelief read GetRelief write SetRelief default rlfNormal;
    //-- Events
    property OnClicked: TNotifyEvent read FOnClicked write FOnClicked;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TCustomButton }

procedure TCustomButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FAction) and (Operation = opRemove) then
  begin
    FAction := nil;
    UpdateControls;
  end;
  inherited;
end;

procedure TCustomButton.SetAction(AValue: TCustomAction);
begin
  if FAction <> AValue then
  begin
    if Assigned(Child) then
      raise Exception.Create('Cannot set Action in a TCustomButton with a Child Control.')
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
      if not Assigned(FAlignment) then
        CreateControls;
      DoActionChanged(FAction);
    end;
  end;
end;

procedure TCustomButton.CreateHandle;
begin
  Handle := gtk_button_new;
end;

procedure TCustomButton.DestroyHandle;
begin
  DestroyControls;
  inherited;
end;

procedure TCustomButton.CreateControls;
begin
  FAlignment := TAlignment.Create(Self);
  FAlignment.XScale := 0;
  FAlignment.YScale := 0;

  FBox := THBox.Create(Self);
  FBox.Parent := FAlignment;
  FBox.Spacing := 4;
  //--
  FImage := TImage.Create(Self);
  FImage.Parent := FBox;
  FImage.IconSize := iszButton;
  //--
  FLabel := TLabel.Create(Self);
  FLabel.Parent := FBox;
  FLabel.UseUnderline := True;
  //--
  gtk_container_add(Handle, FAlignment.Handle);
  gtk_widget_show_all(FAlignment.Handle);
  //--
  UpdateControls;
end;

procedure TCustomButton.DestroyControls;
begin
  if Assigned(FBox) then
  begin
    try FBox.RemoveControl(FLabel); except end;
    FLabel.Free;
    FLabel := nil;
    try FBox.RemoveControl(FImage); except end;
    FImage.Free;
    FImage := nil;

    try FAlignment.RemoveControl(FBox); except end;
    FBox.Free;
    FBox := nil;

    gtk_container_remove(Handle, FAlignment.Handle);
    FAlignment.Free;
    FAlignment := nil;
  end;
end;

procedure TCustomButton.UpdateControls;
begin
  if Assigned(FAlignment) then
  begin
    if Assigned(FAction) then
    begin
      FImage.IconName := FAction.IconName;
      FLabel.Caption := FAction.Caption;
    end
    else
    begin
      FImage.IconName := FIconName;
      FLabel.Caption := FCaption;
    end;
    FImage.Visible := FImage.IconName <> '';
    FLabel.Visible := FLabel.Caption <> '';
  end;
end;

procedure TCustomButton.AddControl(AControl: TControl);
begin
  if Assigned(FAction) then
    raise Exception.Create('Cannot insert a Child Control in a TCustomButton with Action set.')
  else
  begin
    DestroyControls;
    inherited;
  end;
end;

procedure TCustomButton.RemoveControl(AControl: TControl);
begin
  inherited;
  if (FCaption <> '') or (FIconName <> '') then
    CreateControls;
end;

procedure TCustomButton.DoActionChanged(Sender: TCustomAction);
begin
  if Sender = FAction then
  begin
    UpdateControls;
    //--
    ToolTip := FAction.ToolTip;
    Sensitive := FAction.Sensitive;
    Visible := FAction.Visible;
  end;
end;

function TCustomButton.GetIconSize: TIconSize;
begin
  if Assigned(FImage) then
    Result := FImage.IconSize
  else
    Result := iszButton;
end;

procedure TCustomButton.SetIconSize(AValue: TIconSize);
begin
  if Assigned(FImage) then
    FImage.IconSize := AValue;
end;

function TCustomButton.GetFocusOnClick: Boolean;
begin
  Result := gtk_button_get_focus_on_click(Handle);
end;

function TCustomButton.GetRelief: TRelief;
begin
  Result := TRelief(gtk_button_get_relief(Handle));
end;

function TCustomButton.GetUseUnderline: Boolean;
begin
  if Assigned(FLabel) then
    Result := FLabel.UseUnderline
  else
    Result := True;
end;

procedure TCustomButton.SetCaption(AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;

    if not Assigned(FAction) then
      if Assigned(FAlignment) then
        UpdateControls
      else if ControlCount = 0 then
        CreateControls;
  end;
end;

procedure TCustomButton.SetIconName(AValue: string);
begin
  if FIconName <> AValue then
  begin
    FIconName := AValue;

    if not Assigned(FAction) then
      if Assigned(FAlignment) then
        UpdateControls
      else if ControlCount = 0 then
        CreateControls;
  end;
end;

procedure TCustomButton.SetFocusOnClick(AValue: Boolean);
begin
  gtk_button_set_focus_on_click(Handle, AValue);
end;

procedure TCustomButton.SetRelief(AValue: TRelief);
begin
  gtk_button_set_relief(Handle, Ord(AValue));
end;

procedure TCustomButton.SetUseUnderline(AValue: Boolean);
begin
  if Assigned(FLabel) then
    FLabel.UseUnderline := AValue;
end;

procedure TCustomButton.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('clicked');
end;

procedure TCustomButton.DoClicked(var Msg: TGtkSig);
begin
  if Assigned(FAction) then
    FAction.Execute
  else if Assigned(FOnClicked) then
    FOnClicked(Self);
end;

{$ENDIF}
