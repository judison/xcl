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

  TNotebookPage = class(TBinControl)
  private
    function GetCaption: String;
    procedure SetCaption(AValue: String);
    function GetIconName: String;
    procedure SetIconName(AValue: String);
    function GetUseUnderline: Boolean;
    procedure SetUseUnderline(AValue: Boolean);
  protected
    FBox: TBox;
    FImage: TImage;
    FLabel: TLabel;
    procedure CreateControls; virtual;
    procedure DestroyControls; virtual;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    //--
    procedure SetParent(ANewParent: TControl); override;
  public
  published
    property Caption: String read GetCaption write SetCaption;
    property IconName: string read GetIconName write SetIconName;
    property UseUnderline: Boolean read GetUseUnderline write SetUseUnderline default True;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TNotebookPage }

procedure TNotebookPage.SetParent(ANewParent: TControl);
begin
  if (ANewParent = nil) or (ANewParent is TNotebook) then
  begin
    if Assigned(Parent) then
      gtk_notebook_set_tab_label(Parent.Handle, Handle, nil);

    inherited;

    if Assigned(ANewParent) then
      gtk_notebook_set_tab_label(Parent.Handle, Handle, FBox.Handle);
  end
  else
    raise Exception.Create('A TNotebookPage can only be placed in a TNotebook.');
end;

procedure TNotebookPage.CreateHandle;
begin
  Handle := gtk_frame_new(nil);
  gtk_frame_set_shadow_type(Handle, GTK_SHADOW_NONE);
  //--
  CreateControls;
end;

procedure TNotebookPage.DestroyHandle;
begin
  DestroyControls;
  inherited;
end;

procedure TNotebookPage.CreateControls;
begin
  FBox := THBox.Create(Self);
  FBox.Spacing := 2;
  //--
  FImage := TImage.Create(Self);
  FImage.Parent := FBox;
  FImage.IconSize := iszMenu;
  FImage.Visible := False;
  FImage.BoxExpand := False;
  //--
  FLabel := TLabel.Create(Self);
  FLabel.Parent := FBox;
  FLabel.UseUnderline := True;
  FLabel.Visible := False;
end;

procedure TNotebookPage.DestroyControls;
begin
  if Assigned(FBox) then
  begin
    try FBox.RemoveControl(FLabel); except end;
    FLabel.Free;
    FLabel := nil;
    try FBox.RemoveControl(FImage); except end;
    FImage.Free;
    FImage := nil;
    //--
    FBox.Free;
    FBox := nil;
  end;
end;

function TNotebookPage.GetCaption: String;
begin
  Result := FLabel.Caption;
end;

procedure TNotebookPage.SetCaption(AValue: String);
begin
  FLabel.Caption := AValue;
  FLabel.Visible := FLabel.Caption <> '';
end;

function TNotebookPage.GetIconName: String;
begin
  Result := FImage.IconName;
end;

procedure TNotebookPage.SetIconName(AValue: String);
begin
  FImage.IconName := AValue;
  FImage.Visible := FImage.IconName <> '';
end;

function TNotebookPage.GetUseUnderline: Boolean;
begin
  Result := FLabel.UseUnderline;
end;

procedure TNotebookPage.SetUseUnderline(AValue: Boolean);
begin
  FLabel.UseUnderline := AValue;
end;


{$ENDIF}
