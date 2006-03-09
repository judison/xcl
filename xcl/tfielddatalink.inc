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

  TFieldDataLink = class(TDataLink)
  private
    FField: TField;
    FFieldName: String;
    FControl: TComponent;
    FEditing: Boolean;
    FModified: Boolean;
    FOnDataChange: TNotifyEvent;
    FOnEditingChange: TNotifyEvent;
    FOnUpdateData: TNotifyEvent;
    FOnActiveChange: TNotifyEvent;
    function GetCanModify: Boolean;
    procedure SetEditing(AValue: Boolean);
    procedure SetField(AValue: TField);
    procedure SetFieldName(const AValue: String);
    procedure UpdateField;
  protected
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create;
    function Edit: Boolean;
    procedure Modified;
    procedure Reset;
    property CanModify: Boolean read GetCanModify;
    property Control: TComponent read FControl write FControl;
    property Editing: Boolean read FEditing;
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnEditingChange: TNotifyEvent read FOnEditingChange write FOnEditingChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TFieldDataLink }

constructor TFieldDataLink.Create;
begin
  inherited Create;
  VisualControl := True;
end;

function TFieldDataLink.Edit: Boolean;
begin
  if CanModify then
    inherited Edit;
  Result := FEditing;
end;

procedure TFieldDataLink.Modified;
begin
  FModified := True;
end;

procedure TFieldDataLink.Reset;
begin
  RecordChanged(nil);
end;

procedure TFieldDataLink.ActiveChanged;
begin
  UpdateField;
  if Assigned(FOnActiveChange) then
    FOnActiveChange(Self);
end;

procedure TFieldDataLink.EditingChanged;
var
  E: Boolean;
begin
  E := inherited Editing;
  SetEditing(E and CanModify);
end;

procedure TFieldDataLink.FocusControl(Field: TFieldRef);
begin
  // TODO
end;

procedure TFieldDataLink.LayoutChanged;
begin
  UpdateField;
end;

procedure TFieldDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FField) then
  begin
    if Assigned(FOnDataChange) then
      FOnDataChange(Self);
    FModified := False;
  end;
end;

procedure TFieldDataLink.UpdateData;
begin
  if FModified then
  begin
    if (Field <> nil) and Assigned(FOnUpdateData) then
      FOnUpdateData(Self);
    FModified := False;
  end;
end;

function TFieldDataLink.GetCanModify: Boolean;
begin
  Result := (not ReadOnly) and (Field <> nil);// and Field.CanModify;
  // Why is Field.CanModify aways False?
end;

procedure TFieldDataLink.SetEditing(AValue: Boolean);
begin
  if AValue <> FEditing then
  begin
    FEditing := AValue;
    FModified := False;
    if Assigned(FOnEditingChange) then
      FOnEditingChange(Self);
  end;
end;

procedure TFieldDataLink.SetFieldName(const AValue: String);
begin
  if AValue <> FFieldName then
  begin
    FFieldName := AValue;
    UpdateField;
  end;
end;

procedure TFieldDataLink.SetField(AValue: TField);
begin
  if AValue <> FField then
  begin
    FField := AValue;
    EditingChanged;
    RecordChanged(nil);
  end;
end;

procedure TFieldDataLink.UpdateField;
begin
  if (Active) and (FFieldName <> '') then
    SetField(DataSource.DataSet.FieldByName(FFieldName))
  else
    SetField(nil);
end;

{$ENDIF}
