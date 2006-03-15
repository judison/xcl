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

  TDataSetAction = class(TAction) // TCustom?
  private
    FDefaultCaption: String;
    FDefaultIconName: String;
    //==
    FDataSource: TDataSource;
    procedure SetDataSource(AValue: TDataSource);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function IsCaptionStored: Boolean; override;
    function IsIconNameStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DataSource: TDataSource read FDataSource write SetDataSource;
    //--
    property Caption;
    property IconName;
    //property Sensitive;
    property Visible;
    //--
    property OnExecute;
    property OnUpdate;
  end;

  TDataSetFirst = Class(TDataSetAction)
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDataSetLast = Class(TDataSetAction)
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDataSetNext = Class(TDataSetAction)
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDataSetPrior = Class(TDataSetAction)
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDataSetRefresh = Class(TDataSetAction)
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDataSetCancel = Class(TDataSetAction)
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDataSetDelete = Class(TDataSetAction)
  public
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDataSetEdit = Class(TDataSetAction)
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDataSetInsert = Class(TDataSetAction)
  public
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDataSetPost = Class(TDataSetAction)
  protected
    procedure DoUpdate; override;
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TDataSetAction }

constructor TDataSetAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := FDefaultCaption;
  IconName := FDefaultIconName;
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

function TDataSetAction.IsIconNameStored: Boolean;
begin
  Result := IconName <> FDefaultIconName;
end;

{ TDataSetFirst }

constructor TDataSetFirst.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'First';
  FDefaultIconName := 'gtk-goto-first-ltr';
  inherited;
end;

procedure TDataSetFirst.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse) and (not DataSource.DataSet.BOF);
  inherited;
end;

procedure TDataSetFirst.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.First;
  inherited;
end;

{ TDataSetLast }

constructor TDataSetLast.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Last';
  FDefaultIconName := 'gtk-goto-last-ltr';
  inherited;
end;

procedure TDataSetLast.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse) and (not DataSource.DataSet.EOF);
  inherited;
end;

procedure TDataSetLast.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Last;
  inherited;
end;

{ TDataSetNext }

constructor TDataSetNext.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Next';
  FDefaultIconName := 'gtk-go-forward-ltr';
  inherited;
end;

procedure TDataSetNext.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse) and (not DataSource.DataSet.EOF);
  inherited;
end;

procedure TDataSetNext.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Next;
  inherited;
end;

{ TDataSetPrior }

constructor TDataSetPrior.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Prior';
  FDefaultIconName := 'gtk-go-back-ltr';
  inherited;
end;

procedure TDataSetPrior.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse) and (not DataSource.DataSet.BOF);
  inherited;
end;

procedure TDataSetPrior.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Prior;
  inherited;
end;

{ TDataSetRefresh }

constructor TDataSetRefresh.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Refresh';
  FDefaultIconName := 'gtk-refresh';
  inherited;
end;

procedure TDataSetRefresh.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil);
  inherited;
end;

procedure TDataSetRefresh.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Refresh;
  inherited;
end;

{ TDataSetCancel }

constructor TDataSetCancel.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Cancel';
  FDefaultIconName := 'gtk-cancel';
  inherited;
end;

procedure TDataSetCancel.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State in dsEditModes);
  inherited;
end;

procedure TDataSetCancel.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Cancel;
  inherited;
end;

{ TDataSetDelete }

constructor TDataSetDelete.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Delete';
  FDefaultIconName := 'gtk-delete';
  inherited;
end;

procedure TDataSetDelete.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse);
  inherited;
end;

procedure TDataSetDelete.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Delete;
  inherited;
end;

{ TDataSetEdit }

constructor TDataSetEdit.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Edit';
  FDefaultIconName := 'gtk-edit';
  inherited;
end;

procedure TDataSetEdit.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse);
  inherited;
end;

procedure TDataSetEdit.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Edit;
  inherited;
end;

{ TDataSetInsert }

constructor TDataSetInsert.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Insert';
  FDefaultIconName := 'gtk-new';
  inherited;
end;

procedure TDataSetInsert.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State = dsBrowse);
  inherited;
end;

procedure TDataSetInsert.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Insert;
  inherited;
end;

{ TDataSetPost }

constructor TDataSetPost.Create(AOwner: TComponent);
begin
  FDefaultCaption := 'Post';
  FDefaultIconName := 'gtk-apply';
  inherited;
end;

procedure TDataSetPost.DoUpdate;
begin
  Sensitive := (DataSource <> nil) and (DataSource.DataSet <> nil) and (DataSource.DataSet.State in dsEditModes);
  inherited;
end;

procedure TDataSetPost.DoExecute;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.Post;
  inherited;
end;

{$ENDIF}
