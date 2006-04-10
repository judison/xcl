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

  TRange = class(TControl)
  private
    FMyAdjustment: TAdjustment;
    FAdjustment: TAdjustment;
    function GetInverted: Boolean;
    function GetUpdatePolicy: TUpdatePolicy;
    procedure SetAdjustment(AValue: TAdjustment);
    procedure SetInverted(AValue: Boolean);
    procedure SetUpdatePolicy(AValue: TUpdatePolicy);
    function IsMyAdjStored: Boolean;
    function GetLower: Single;
    function GetUpper: Single;
    function GetValue: Single;
    function GetStepIncrement: Single;
    function GetPageIncrement: Single;
    function GetPageSize: Single;
    procedure SetLower(AValue: Single);
    procedure SetUpper(AValue: Single);
    procedure SetValue(AValue: Single);
    procedure SetStepIncrement(AValue: Single);
    procedure SetPageIncrement(AValue: Single);
    procedure SetPageSize(AValue: Single);
  protected
    procedure CreateMyAdj;
    procedure DestroyMyAdj;
    procedure DestroyHandle; override;
  public
    property Adjustment: TAdjustment read FAdjustment write SetAdjustment;
  published
    property Inverted: Boolean read GetInverted write SetInverted default False;
    property UpdatePolicy: TUpdatePolicy read GetUpdatePolicy write SetUpdatePolicy default upContinuous;
    //---
    property Lower: Single read GetLower write SetLower stored IsMyAdjStored;
    property Upper: Single read GetUpper write SetUpper stored IsMyAdjStored;
    property Value: Single read GetValue write SetValue stored IsMyAdjStored;
    property StepIncrement: Single read GetStepIncrement write SetStepIncrement stored IsMyAdjStored;
    property PageIncrement: Single read GetPageIncrement write SetPageIncrement stored IsMyAdjStored;
    property PageSize: Single read GetPageSize write SetPageSize stored IsMyAdjStored;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TRange }

procedure TRange.CreateMyAdj;
begin
  if FMyAdjustment <> nil then
    raise Exception.Create('Already have a MyAdjustment');

  FMyAdjustment := TAdjustment.Create(nil);

  FAdjustment := FMyAdjustment;
end;

procedure TRange.DestroyMyAdj;
begin
  if FMyAdjustment <> nil then
  begin
    FMyAdjustment.Free;
    FMyAdjustment := nil;
  end;
end;

procedure TRange.DestroyHandle;
begin
  inherited;
  DestroyMyAdj;
end;

procedure TRange.SetAdjustment(AValue: TAdjustment);
begin
  if AValue <> FAdjustment then
  begin
    if FMyAdjustment <> nil then
      DestroyMyAdj;
    //---
    FAdjustment := AValue;
    //---
    if FAdjustment = nil then
      CreateMyAdj;
  end;
end;

function TRange.GetInverted: Boolean;
begin
  Result := gtk_range_get_inverted(Handle);
end;

function TRange.GetUpdatePolicy: TUpdatePolicy;
begin
  Result := TUpdatePolicy(gtk_range_get_update_policy(Handle));
end;

procedure TRange.SetInverted(AValue: Boolean);
begin
  gtk_range_set_inverted(Handle, AValue);
end;

procedure TRange.SetUpdatePolicy(AValue: TUpdatePolicy);
begin
  gtk_range_set_update_policy(Handle, Ord(AValue));
end;

function TRange.IsMyAdjStored: Boolean;
begin
  // Only store if I use FMyAdjustment
  Result := (FAdjustment = FMyAdjustment);
end;

function TRange.GetLower: Single;
begin
  Result := Adjustment.Lower;
end;

function TRange.GetUpper: Single;
begin
  Result := Adjustment.Upper;
end;

function TRange.GetValue: Single;
begin
  Result := Adjustment.Value;
end;

function TRange.GetStepIncrement: Single;
begin
  Result := Adjustment.StepIncrement;
end;

function TRange.GetPageIncrement: Single;
begin
  Result := Adjustment.PageIncrement;
end;

function TRange.GetPageSize: Single;
begin
  Result := Adjustment.PageSize;
end;

procedure TRange.SetLower(AValue: Single);
begin
  Adjustment.Lower := AValue;
end;

procedure TRange.SetUpper(AValue: Single);
begin
  Adjustment.Upper := AValue;
end;

procedure TRange.SetValue(AValue: Single);
begin
  Adjustment.Value := AValue;
end;

procedure TRange.SetStepIncrement(AValue: Single);
begin
  Adjustment.StepIncrement := AValue;
end;

procedure TRange.SetPageIncrement(AValue: Single);
begin
  Adjustment.PageIncrement := AValue;
end;

procedure TRange.SetPageSize(AValue: Single);
begin
  Adjustment.PageSize := AValue;
end;

{$ENDIF}
