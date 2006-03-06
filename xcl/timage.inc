(*
   $Id: timage.inc,v 1.6 2006/03/01 20:52:34 judison Exp $

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

  TImage = class(TMiscControl, IPixBufClient)
  private
    FResource: String;
    FResPB: TPixbuf;
    FPixbuf: TPixbuf;
    FStockIconSize: TIconSize;
    FStockID: String;
    procedure SetStockIconSize(AValue: TIconSize);
    procedure SetStockID(AValue: String);
    procedure SetPixBuf(AValue: TPixbuf);
    procedure SetResource(AValue: String);
  protected
    procedure CreateHandle; override;
    procedure ChangeImage;
    procedure PixbufChanged(APixbuf: TPixbuf);
    procedure ClearSourceProps;
  public
  published
    property Resource: String read FResource write SetResource;
    //--
    property Pixbuf: TPixBuf read FPixbuf write SetPixbuf;
    //--
    property StockIconSize: TIconSize read FStockIconSize write SetStockIconSize default iszInvalid;
    property StockID: String read FStockID write SetStockID;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TImage }

procedure TImage.CreateHandle;
begin
  Handle := gtk_image_new;
end;

procedure TImage.ChangeImage;
begin
  if FPixbuf <> nil then
    gtk_image_set_from_pixbuf(Handle, FPixBuf.GetGdkPixBuf)
  else if (FStockID <> '') and (FStockIconSize <> iszInvalid) then
    gtk_image_set_from_stock(Handle, PChar(FStockID), Ord(FStockIconSize))
  else if (FResource <> '') and (FResPB <> nil) then
    gtk_image_set_from_pixbuf(Handle, FResPB.GetGdkPixBuf)
  else
    gtk_image_set_from_stock(Handle, nil, 1); // trick to get a "blank" image
end;

procedure TImage.ClearSourceProps;
begin
  //== StockID
  FStockID := '';

  //== Pixbuf
  if Assigned(FPixBuf) then
  begin
    FPixBuf.RemoveClient(Self);
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

procedure TImage.SetPixBuf(AValue: TPixBuf);
begin
  if FPixBuf <> AValue then
  begin
    ClearSourceProps;
    FPixBuf := AValue;

    if Assigned(FPixBuf) then
      FPixBuf.AddClient(Self);

    ChangeImage;
  end;
end;

procedure TImage.PixBufChanged(APixBuf: TPixBuf);
begin
  if APixBuf = FPixBuf then
    ChangeImage;
end;

{$ENDIF}
{
$Log: timage.inc,v $
Revision 1.6  2006/03/01 20:52:34  judison
Added TImage.Resource
Removed TImage.StorageType

Revision 1.5  2005/12/11 21:38:16  judison
TImage.StorageType

Revision 1.4  2005/12/02 22:31:34  judison
Long-term changes (again) I need to use cvs more!

Revision 1.3  2005/03/26 05:21:47  judison
+ CVS Log Tag

}
