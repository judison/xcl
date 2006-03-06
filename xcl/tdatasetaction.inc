(*
   $Id: tdatasetaction.inc,v 1.3 2005/12/30 17:21:38 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
//** INTERFACE
//****************************************************************************
{$IFDEF INC_READ_INTERFACE}

  TDataSetAction = class(TCustomAction)
  private
    FDefaultCaption: String;
    FDefaultStockID: String;
    //==
    FDataSource: TDataSource;
    procedure SetDataSource(AValue: TDataSource);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function IsCaptionStored: Boolean; override;
    function IsStockIDStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DataSource: TDataSource read FDataSource write SetDataSource;
    //--
    property Caption;
    property StockID;
    //property Sensitive;
    property Visible;
    //--
    property OnExecute;
    property OnUpdate;
  end;

  TDataSetFirst = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

  TDataSetLast = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

  TDataSetNext = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

  TDataSetPrior = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

  TDataSetRefresh = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

  TDataSetCancel = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

  TDataSetDelete = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

  TDataSetEdit = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

  TDataSetInsert = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

  TDataSetPost = Class(TDataSetAction)
  public
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    function Execute: Boolean; override;
  end;

{$ENDIF}
//** IMPLEMENTATION
//****************************************************************************
{$IFDEF INC_READ_IMPLEMENTATION}

{ TDataSetAction }

constructor TDataSetAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := FDefaultCaption;
  StockID := FDefaultStockID;
end;

procedure TDataSetAction.SetDataSource(AValue: TDataSource);
begin
  if (AValue <> FDataSource) then
  begin
    FDataSource := AValue;
    if (AValue <> nil) then
      AValue.FreeNotification(Self);
  end;
end;

procedure TDataSetAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
  if (Operation = opRemove) and (AComponent = DataSource) then
    FDataSource := nil;
end;

function TDataSetAction.IsCaptionStored: Boolean;
begin
  Result := Caption <> FDefaultCaption;
end;

function TDataSetAction.IsStockIDStored: Boolean;
begin
  Result := StockID <> FDefaultStockID;
end;

{ TDataSetFirst }

constructor TDataSetFirst.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'First';
  FDefaultStockID := 'gtk-goto-first';
  inherited;
end;

function TDataSetFirst.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse) and (not DataSource.DataSet.BOF);
  Result := inherited;
end;

function TDataSetFirst.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.First;
  Result := inherited;
end;

{ TDataSetLast }

constructor TDataSetLast.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Last';
  FDefaultStockID := 'gtk-goto-last';
  inherited;
end;

function TDataSetLast.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse) and (not DataSource.DataSet.EOF);
  Result := inherited;
end;

function TDataSetLast.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Last;
  Result := inherited;
end;

{ TDataSetNext }

constructor TDataSetNext.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Next';
  FDefaultStockID := 'gtk-go-forward';
  inherited;
end;

function TDataSetNext.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse) and (not DataSource.DataSet.EOF);
  Result := inherited;
end;

function TDataSetNext.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Next;
  Result := inherited;
end;

{ TDataSetPrior }

constructor TDataSetPrior.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Prior';
  FDefaultStockID := 'gtk-go-back';
  inherited;
end;

function TDataSetPrior.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse) and (not DataSource.DataSet.BOF);
  Result := inherited;
end;

function TDataSetPrior.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Prior;
  Result := inherited;
end;

{ TDataSetRefresh }

constructor TDataSetRefresh.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Refresh';
  FDefaultStockID := 'gtk-refresh';
  inherited;
end;

function TDataSetRefresh.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil);
  Result := inherited;
end;

function TDataSetRefresh.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Refresh;
  Result := inherited;
end;

{ TDataSetCancel }

constructor TDataSetCancel.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Cancel';
  FDefaultStockID := 'gtk-cancel';
  inherited;
end;

function TDataSetCancel.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State in dsEditModes);
  Result := inherited;
end;

function TDataSetCancel.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Cancel;
  Result := inherited;
end;

{ TDataSetDelete }

constructor TDataSetDelete.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Delete';
  FDefaultStockID := 'gtk-delete';
  inherited;
end;

function TDataSetDelete.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse);
  Result := inherited;
end;

function TDataSetDelete.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Delete;
  Result := inherited;
end;

{ TDataSetEdit }

constructor TDataSetEdit.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Edit';
  FDefaultStockID := 'gtk-edit';
  inherited;
end;

function TDataSetEdit.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse);
  Result := inherited;
end;

function TDataSetEdit.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Edit;
  Result := inherited;
end;

{ TDataSetInsert }

constructor TDataSetInsert.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Insert';
  FDefaultStockID := 'gtk-new';
  inherited;
end;

function TDataSetInsert.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse);
  Result := inherited;
end;

function TDataSetInsert.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Insert;
  Result := inherited;
end;

{ TDataSetPost }

constructor TDataSetPost.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Post';
  FDefaultStockID := 'gtk-apply';
  inherited;
end;

function TDataSetPost.Update: Boolean;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State in dsEditModes);
  Result := inherited;
end;

function TDataSetPost.Execute: Boolean;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Post;
  Result := inherited;
end;

{$ENDIF}
{
$Log: tdatasetaction.inc,v $
Revision 1.3  2005/12/30 17:21:38  judison
* Moved DB Comps to xcldb.inc
* Added TCanvas

}