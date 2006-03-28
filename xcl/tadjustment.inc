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

  TAdjustment = class(TComponent)
  private
    Handle: Pointer;
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
  public
    function GetHandle: Pointer;
    //==
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AHandle: Pointer);
    destructor Destroy; override;
  published
    property Lower: Single read GetLower write SetLower;
    property Upper: Single read GetUpper write SetUpper;
    property Value: Single read GetValue write SetValue;
    property StepIncrement: Single read GetStepIncrement write SetStepIncrement;
    property PageIncrement: Single read GetPageIncrement write SetPageIncrement;
    property PageSize: Single read GetPageSize write SetPageSize;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

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
  g_object_unref(Handle);
  //---
  inherited;
end;

function TAdjustment.GetHandle: Pointer;
begin
  Result := Handle;
end;

function TAdjustment.GetLower: Single;
begin
  Result := PGtkAdjustment(Handle)^.lower;
end;

function TAdjustment.GetUpper: Single;
begin
  Result := PGtkAdjustment(Handle)^.upper;
end;

function TAdjustment.GetValue: Single;
begin
  Result := PGtkAdjustment(Handle)^.value;
end;

function TAdjustment.GetStepIncrement: Single;
begin
  Result := PGtkAdjustment(Handle)^.step_increment;
end;

function TAdjustment.GetPageIncrement: Single;
begin
  Result := PGtkAdjustment(Handle)^.page_increment;
end;

function TAdjustment.GetPageSize: Single;
begin
  Result := PGtkAdjustment(Handle)^.page_size;
end;

procedure TAdjustment.SetLower(AValue: Single);
begin
  if AValue <> PGtkAdjustment(Handle)^.lower then
  begin
    PGtkAdjustment(Handle)^.lower := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

procedure TAdjustment.SetUpper(AValue: Single);
begin
  if AValue <> PGtkAdjustment(Handle)^.upper then
  begin
    PGtkAdjustment(Handle)^.upper := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

procedure TAdjustment.SetValue(AValue: Single);
begin
  if AValue <> PGtkAdjustment(Handle)^.value then
  begin
    PGtkAdjustment(Handle)^.value := AValue;
    gtk_adjustment_value_changed(Handle);
  end;
end;

procedure TAdjustment.SetStepIncrement(AValue: Single);
begin
  if AValue <> PGtkAdjustment(Handle)^.step_increment then
  begin
    PGtkAdjustment(Handle)^.step_increment := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

procedure TAdjustment.SetPageIncrement(AValue: Single);
begin
  if AValue <> PGtkAdjustment(Handle)^.page_increment then
  begin
    PGtkAdjustment(Handle)^.page_increment := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

procedure TAdjustment.SetPageSize(AValue: Single);
begin
  if AValue <> PGtkAdjustment(Handle)^.page_size then
  begin
    PGtkAdjustment(Handle)^.page_size := AValue;
    gtk_adjustment_changed(Handle);
  end;
end;

{$ENDIF}
