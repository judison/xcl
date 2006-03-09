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

  TProgressBar = class(TControl)
  private
    function GetText: string;
    function GetFraction: Double;
    function GetPulseStep: Double;
    function GetOrientation: TProgressBarOrientation;
    procedure SetText(AValue: string);
    procedure SetFraction(AValue: Double);
    procedure SetPulseStep(AValue: Double);
    procedure SetOrientation(AValue: TProgressBarOrientation);
  protected
    procedure CreateHandle; override;
  public
    procedure Pulse;
  published
    property Text: string read GetText write SetText;
    property Fraction: Double read GetFraction write SetFraction;
    property PulseStep: Double read GetPulseStep write SetPulseStep;
    property Orientation: TProgressBarOrientation read GetOrientation write SetOrientation;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TProgressBar }

procedure TProgressBar.CreateHandle;
begin
  Handle := gtk_progress_bar_new;
end;

procedure TProgressBar.Pulse;
begin
  gtk_progress_bar_pulse(Handle);
end;

function TProgressBar.GetText: string;
begin
  Result := gtk_progress_bar_get_text(Handle);
end;

function TProgressBar.GetFraction: Double;
begin
  Result := gtk_progress_bar_get_fraction(Handle);
end;

function TProgressBar.GetPulseStep: Double;
begin
  Result := gtk_progress_bar_get_pulse_step(Handle);
end;

function TProgressBar.GetOrientation: TProgressBarOrientation;
begin
  Result := TProgressBarOrientation(gtk_progress_bar_get_orientation(Handle));
end;

procedure TProgressBar.SetText(AValue: string);
begin
  gtk_progress_bar_set_text(Handle, PChar(AValue));
end;

procedure TProgressBar.SetFraction(AValue: Double);
begin
  gtk_progress_bar_set_fraction(Handle, AValue);
end;

procedure TProgressBar.SetPulseStep(AValue: Double);
begin
  gtk_progress_bar_set_pulse_step(Handle, AValue);
end;

procedure TProgressBar.SetOrientation(AValue: TProgressBarOrientation);
begin
  gtk_progress_bar_set_orientation(Handle, TGtkProgressBarOrientation(AValue));
end;

{$ENDIF}
