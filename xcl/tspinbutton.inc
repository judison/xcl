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

type
  TSpinButton = class(TCustomEntry)
  private
    function GetValue: Double;
    procedure SetValue(const AValue: Double);
    function GetDigits: LongWord;
    procedure SetDigits(const AValue: LongWord);
    function GetSnapToTicks: Boolean;
    procedure SetSnapToTicks(const AValue: Boolean);
    function GetMin: Double;
    procedure SetMin(const AValue: Double);
    function GetMax: Double;
    procedure SetMax(const AValue: Double);
    function GetStep: Double;
    procedure SetStep(const AValue: Double);
    function GetPage: Double;
    procedure SetPage(const AValue: Double);
  protected
    procedure CreateHandle; override;
  public
    procedure GetRange(var AMin, AMax: Double);
    procedure SetRange(const AMin, AMax: Double);
    procedure GetIncrements(var AStep, APage: Double);
    procedure SetIncrements(const AStep, APage: Double);
  published
    property Value: Double read GetValue write SetValue;
    property Digits: LongWord read GetDigits write SetDigits;
    property SnapToTicks: Boolean read GetSnapToTicks write SetSnapToTicks;
    property Min: Double read GetMin write SetMin;
    property Max: Double read GetMax write SetMax;
    property Step: Double read GetStep write SetStep;
    property Page: Double read GetPage write SetPage;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TSpinButton }

procedure TSpinButton.CreateHandle;
begin
  Handle := gtk_spin_button_new_with_range(0, 100, 1.0);
end;

function TSpinButton.GetValue: Double;
begin
  Result := gtk_spin_button_get_value(Handle);
end;

procedure TSpinButton.SetValue(const AValue: Double);
begin
  gtk_spin_button_set_value(Handle, AValue);
end;

function TSpinButton.GetDigits: LongWord;
begin
  Result := gtk_spin_button_get_digits(Handle);
end;

procedure TSpinButton.SetDigits(const AValue: LongWord);
begin
  gtk_spin_button_set_digits(Handle, AValue);
end;

function TSpinButton.GetSnapToTicks: Boolean;
begin
  Result := gtk_spin_button_get_snap_to_ticks(Handle);
end;

procedure TSpinButton.SetSnapToTicks(const AValue: Boolean);
begin
  gtk_spin_button_set_snap_to_ticks(Handle, AValue);
end;

procedure TSpinButton.GetRange(var AMin, AMax: Double);
begin
  gtk_spin_button_get_range(Handle, @AMin, @AMax);
end;

procedure TSpinButton.SetRange(const AMin, AMax: Double);
begin
  gtk_spin_button_set_range(Handle, AMin, AMax);
end;

procedure TSpinButton.GetIncrements(var AStep, APage: Double);
begin
  gtk_spin_button_get_increments(Handle, @AStep, @APage);
end;

procedure TSpinButton.SetIncrements(const AStep, APage: Double);
begin
  gtk_spin_button_set_increments(Handle, AStep, APage);
end;

function TSpinButton.GetMin: Double;
var
  dummy: Double;
begin
  gtk_spin_button_get_range(Handle, @Result, @dummy);
end;

procedure TSpinButton.SetMin(const AValue: Double);
var
  LMin, LMax: Double;
begin
  gtk_spin_button_get_range(Handle, @LMin, @LMax);
  gtk_spin_button_set_range(Handle, AValue, LMax);
end;

function TSpinButton.GetMax: Double;
var
  dummy: Double;
begin
  gtk_spin_button_get_range(Handle, @dummy, @Result);
end;

procedure TSpinButton.SetMax(const AValue: Double);
var
  LMin, LMax: Double;
begin
  gtk_spin_button_get_range(Handle, @LMin, @LMax);
  gtk_spin_button_set_range(Handle, LMin, AValue);
end;

function TSpinButton.GetStep: Double;
var
  dummy: Double;
begin
  gtk_spin_button_get_increments(Handle, @Result, @dummy);
end;

procedure TSpinButton.SetStep(const AValue: Double);
var
  LStep, LPage: Double;
begin
  gtk_spin_button_get_increments(Handle, @LStep, @LPage);
  gtk_spin_button_set_increments(Handle, AValue, LPage);
end;

function TSpinButton.GetPage: Double;
var
  dummy: Double;
begin
  gtk_spin_button_get_increments(Handle, @dummy, @Result);
end;

procedure TSpinButton.SetPage(const AValue: Double);
var
  LStep, LPage: Double;
begin
  gtk_spin_button_get_increments(Handle, @LStep, @LPage);
  gtk_spin_button_set_increments(Handle, LStep, AValue);
end;

{$ENDIF}
