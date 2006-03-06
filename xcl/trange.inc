(*
   $Id: trange.inc,v 1.3 2005/12/16 03:29:52 judison Exp $

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
    function GetLower: Double;
    function GetUpper: Double;
    function GetValue: Double;
    function GetStepIncrement: Double;
    function GetPageIncrement: Double;
    function GetPageSize: Double;
    procedure SetLower(AValue: Double);
    procedure SetUpper(AValue: Double);
    procedure SetValue(AValue: Double);
    procedure SetStepIncrement(AValue: Double);
    procedure SetPageIncrement(AValue: Double);
    procedure SetPageSize(AValue: Double);
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
    property Lower: Double read GetLower write SetLower stored IsMyAdjStored;
    property Upper: Double read GetUpper write SetUpper stored IsMyAdjStored;
    property Value: Double read GetValue write SetValue stored IsMyAdjStored;
    property StepIncrement: Double read GetStepIncrement write SetStepIncrement stored IsMyAdjStored;
    property PageIncrement: Double read GetPageIncrement write SetPageIncrement stored IsMyAdjStored;
    property PageSize: Double read GetPageSize write SetPageSize stored IsMyAdjStored;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

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

function TRange.GetLower: Double;
begin
  Result := Adjustment.Lower;
end;

function TRange.GetUpper: Double;
begin
  Result := Adjustment.Upper;
end;

function TRange.GetValue: Double;
begin
  Result := Adjustment.Value;
end;

function TRange.GetStepIncrement: Double;
begin
  Result := Adjustment.StepIncrement;
end;

function TRange.GetPageIncrement: Double;
begin
  Result := Adjustment.PageIncrement;
end;

function TRange.GetPageSize: Double;
begin
  Result := Adjustment.PageSize;
end;

procedure TRange.SetLower(AValue: Double);
begin
  Adjustment.Lower := AValue;
end;

procedure TRange.SetUpper(AValue: Double);
begin
  Adjustment.Upper := AValue;
end;

procedure TRange.SetValue(AValue: Double);
begin
  Adjustment.Value := AValue;
end;

procedure TRange.SetStepIncrement(AValue: Double);
begin
  Adjustment.StepIncrement := AValue;
end;

procedure TRange.SetPageIncrement(AValue: Double);
begin
  Adjustment.PageIncrement := AValue;
end;

procedure TRange.SetPageSize(AValue: Double);
begin
  Adjustment.PageSize := AValue;
end;

{$ENDIF}
{
$Log: trange.inc,v $
Revision 1.3  2005/12/16 03:29:52  judison
*** empty log message ***

Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}