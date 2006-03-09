(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
{** INTERFACE *************************************}{$IFDEF INC_READ_INTERFACE}

  TImage = class(TMiscControl, IPixbufClient)
  private
    FResource: String;
    FResPB: TPixbuf;
    FPixbuf: TPixbuf;
    FStockIconSize: TIconSize;
    FStockID: String;
    procedure SetStockIconSize(AValue: TIconSize);
    procedure SetStockID(AValue: String);
    procedure SetPixbuf(AValue: TPixbuf);
    procedure SetResource(AValue: String);
  protected
    procedure CreateHandle; override;
    procedure ChangeImage;
    procedure PixbufChanged(APixbuf: TPixbuf);
    procedure ClearSourceProps;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Resource: String read FResource write SetResource;
    //--
    property Pixbuf: TPixbuf read FPixbuf write SetPixbuf;
    //--
    property StockIconSize: TIconSize read FStockIconSize write SetStockIconSize default iszInvalid;
    property StockID: String read FStockID write SetStockID;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TImage }

procedure TImage.CreateHandle;
begin
  Handle := gtk_image_new;
end;

procedure TImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FPixbuf) and (Operation = opRemove) then
  begin
    FPixbuf := nil;
    ChangeImage;
  end;
  inherited;
end;

procedure TImage.ChangeImage;
begin
  if FPixbuf <> nil then
    gtk_image_set_from_pixbuf(Handle, FPixbuf.GetGdkPixbuf)
  else if (FStockID <> '') and (FStockIconSize <> iszInvalid) then
    gtk_image_set_from_stock(Handle, PChar(FStockID), Ord(FStockIconSize))
  else if (FResource <> '') and (FResPB <> nil) then
    gtk_image_set_from_pixbuf(Handle, FResPB.GetGdkPixbuf)
  else
    gtk_image_set_from_stock(Handle, nil, 1); // trick to get a "blank" image
end;

procedure TImage.ClearSourceProps;
begin
  //== StockID
  FStockID := '';

  //== Pixbuf
  if Assigned(FPixbuf) then
  begin
    FPixbuf.RemoveClient(Self);
    FPixbuf.RemoveFreeNotification(Self);
    FPixbuf := nil;
  end;

  //== Resource
  FResource := '';
  if Assigned(FResPB) then
  begin
    FResPB.RemoveClient(Self);
    FResPB.Free;
    FResPB := nil;
  end;
end;

procedure TImage.SetResource(AValue: String);
begin
  if FResource <> AValue then
  begin
    ClearSourceProps;
    FResource := AValue;

    if (FResource <> '') and ResourceExists('PIXDATA', FResource) then
    begin
      if not Assigned(FResPB) then
      begin
        FResPB := TPixbuf.Create(Self);
        FResPB.AddClient(Self);
      end;
      FResPB.Resource := FResource;
    end;

    ChangeImage;
  end;
end;

procedure TImage.SetStockIconSize(AValue: TIconSize);
begin
  if FStockIconSize <> AValue then
  begin
    FStockIconSize := AValue;
    if FStockID <> '' then
      ChangeImage;
  end;
end;

procedure TImage.SetStockID(AValue: String);
begin
  if FStockID <> AValue then
  begin
    ClearSourceProps;
    FStockID := AValue;
    ChangeImage;
  end;
end;

procedure TImage.SetPixbuf(AValue: TPixbuf);
begin
  if FPixbuf <> AValue then
  begin
    if Assigned(FPixbuf) then
      FPixbuf.RemoveFreeNotification(Self);

    ClearSourceProps;
    FPixbuf := AValue;

    if Assigned(FPixbuf) then
    begin
      FPixbuf.AddClient(Self);
      FPixbuf.FreeNotification(Self);
    end;

    ChangeImage;
  end;
end;

procedure TImage.PixbufChanged(APixbuf: TPixbuf);
begin
  if APixbuf = FPixbuf then
    ChangeImage;
end;

{$ENDIF}
