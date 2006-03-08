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

  TDBEntry = class(TCustomEntry)
  private
    FDataLink: TFieldDataLink;
    FFocused: Boolean;
    FIgnoreChanged: Boolean;
    //
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    //
    function GetDataField: String;
    procedure SetDataField(const AValue: String);
    function GetDataSource: TDataSource;
    procedure SetDataSource(AValue: TDataSource);
    function GetField: TField;
  protected
    procedure DoChanged(var Msg: TGtkSig); message 'GTK:changed'; override;
    procedure DoFocusIn(var Msg: TGtkSigEvent); message 'GTK:focus-in-event'; override;
    procedure DoFocusOut(var Msg: TGtkSigEvent); message 'GTK:focus-out-event'; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ConnectSignals; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property DataField: String read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    //--
    property HasFrame;
    property MaxLength;
    property Visibility;
    property WidthChars;
    property OnChanged;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TDBEntry }

constructor TDBEntry.Create(AOwner: TComponent);
begin
  FFocused := False;
  FIgnoreChanged := False;
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnEditingChange := @EditingChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLink.OnActiveChange := @ActiveChange;
end;

destructor TDBEntry.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDBEntry.ConnectSignals;
begin
  inherited;
end;

procedure TDBEntry.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TDBEntry.DoChanged(var Msg: TGtkSig);
begin
  if not FIgnoreChanged then
  begin
    FIgnoreChanged := True;
    if (FDataLink.Field <> nil) and (not FDataLink.Editing) and (GetText <> FText) then
      DataChange(nil)
    else
    begin
      inherited;
      FDataLink.Modified;
    end;
    FIgnoreChanged := False;
  end;
end;

function TDBEntry.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBEntry.SetDataSource(AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

function TDBEntry.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBEntry.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

function TDBEntry.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBEntry.ActiveChange(Sender: TObject);
begin
end;

procedure TDBEntry.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    if FFocused and (FDataLink.Field.Text <> '') then
      Text := FDataLink.Field.Text
    else
      Text := FDataLink.Field.DisplayText
  else
    Text := '';
end;

procedure TDBEntry.EditingChange(Sender: TObject);
begin
end;

procedure TDBEntry.UpdateData(Sender: TObject);
var
  Aux: String;
begin
  Aux := Text;
  FDataLink.Field.AsString := Aux;
  if Aux <> FDataLink.Field.AsString then
    Text := FDataLink.Field.AsString;
end;

procedure TDBEntry.DoFocusIn(var Msg: TGtkSigEvent);
begin
  FFocused := True;
  inherited;
end;

procedure TDBEntry.DoFocusOut(var Msg: TGtkSigEvent);
begin
  try
    FDataLink.UpdateRecord;
  except
    DataChange(nil);
    raise;
  end;
  FFocused := False;
  inherited;
end;

{$ENDIF}
