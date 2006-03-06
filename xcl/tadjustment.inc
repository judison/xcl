(*
   $Id: tadjustment.inc,v 1.3 2005/12/19 22:49:23 judison Exp $

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

  TAdjustment = class(TComponent)
  private
    Handle: Pointer;
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
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AHandle: Pointer);
    destructor Destroy; override;
  published
    property Lower: Double read GetLower write SetLower;
    property Upper: Double read GetUpper write SetUpper;
    property Value: Double read GetValue write SetValue;
    property StepIncrement: Double read GetStepIncrement write SetStepIncrement;
    property PageIncrement: Double read GetPageIncrement write SetPageIncrement;
    property PageSize: Double read GetPageSize write SetPageSize;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TAdjustment }

constructor TAdjustment.Create(AOwner: TComponent);
begin
  inherited;
  Handle := gtk_adjustment_new(0, 0, 100, 1, 10, 10);
  g_object_ref(Handle);
end;

constructor TAdjustment.Create(AOwner: TComponent; AHandle: Pointer);
begin
  Handle := nil;
  inherited Create(AOwner);
  Handle := AHandle;
  if Handle <> nil then
    g_object_ref(Handle);
end;

destructor TAdjustment.Destroy;
begin
  // todo: verificar isso:
  gtk_object_destroy(Handle);
  //---
  inherited;
end;

function TAdjustment.GetLower: Double;
begin
  Result := PGtkAdjustment(Handle)^.lower;
end;

function TAdjustment.GetUpper: Double;
begin
  Result := PGtkAdjustment(Handle)^.upper;
end;

function TAdjustment.GetValue: Double;
begin
  Result := PGtkAdjustment(Handle)^.value;
end;

function TAdjustment.GetStepIncrement: Double;
begin
  Result := PGtkAdjustment(Handle)^.step_increment;
end;

function TAdjustment.GetPageIncrement: Double;
begin
  Result := PGtkAdjustment(Handle)^.page_increment;
end;

function TAdjustment.GetPageSize: Double;
begin
  Result := PGtkAdjustment(Handle)^.page_size;
end;

procedure TAdjustment.SetLower(AValue: Double);
begin
  if AValue <> PGtkAdjustment(Handle)^.lower then
  begin
    PGtkAdjustment(Handle)^.lower := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

procedure TAdjustment.SetUpper(AValue: Double);
begin
  if AValue <> PGtkAdjustment(Handle)^.upper then
  begin
    PGtkAdjustment(Handle)^.upper := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

procedure TAdjustment.SetValue(AValue: Double);
begin
  if AValue <> PGtkAdjustment(Handle)^.value then
  begin
    PGtkAdjustment(Handle)^.value := AValue;
    gtk_adjustment_value_changed(Handle);
  end;
end;

procedure TAdjustment.SetStepIncrement(AValue: Double);
begin
  if AValue <> PGtkAdjustment(Handle)^.step_increment then
  begin
    PGtkAdjustment(Handle)^.step_increment := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

procedure TAdjustment.SetPageIncrement(AValue: Double);
begin
  if AValue <> PGtkAdjustment(Handle)^.page_increment then
  begin
    PGtkAdjustment(Handle)^.page_increment := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

procedure TAdjustment.SetPageSize(AValue: Double);
begin
  if AValue <> PGtkAdjustment(Handle)^.page_size then
  begin
    PGtkAdjustment(Handle)^.page_size := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

{$ENDIF}
{
$Log: tadjustment.inc,v $
Revision 1.3  2005/12/19 22:49:23  judison
various changes
added TSpinButton

Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}