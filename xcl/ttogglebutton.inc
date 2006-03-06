(*
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

  TCustomToggleButton = class(TButton)
  private
    FOnToggled: TNotifyEvent;
    function GetActive: Boolean;
    function GetDrawIndicator: Boolean;
    function GetInconsistent: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetDrawIndicator(Value: Boolean);
    procedure SetInconsistent(Value: Boolean);
  protected
    procedure CreateHandle; override;
    procedure ConnectSignals; override;
    //--
    procedure DoToggled(var Msg: TGtkSig); message 'GTK:toggled';
    //==
    property DrawIndicator: Boolean read GetDrawIndicator write SetDrawIndicator default False;
  public
  published
    property Active: Boolean read GetActive write SetActive default False;
    property Inconsistent: Boolean read GetInconsistent write SetInconsistent default False;
    //-- Events
    property OnToggled: TNotifyEvent read FOnToggled write FOnToggled;
  end;

  TToggleButton = class(TCustomToggleButton)
  private
  protected
  public
  published
    property DrawIndicator;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TCustomToggleButton }

procedure TCustomToggleButton.CreateHandle;
begin
  Handle := gtk_toggle_button_new;
end;

procedure TCustomToggleButton.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('toggled');
end;

procedure TCustomToggleButton.DoToggled(var Msg: TGtkSig);
begin
  if Assigned(FOnToggled) then
    FOnToggled(Self);
end;

function TCustomToggleButton.GetActive: Boolean;
begin
  Result := gtk_toggle_button_get_active(Handle);
end;

function TCustomToggleButton.GetDrawIndicator: Boolean;
begin
  Result := gtk_toggle_button_get_mode(Handle);
end;

function TCustomToggleButton.GetInconsistent: Boolean;
begin
  Result := gtk_toggle_button_get_inconsistent(Handle);
end;

procedure TCustomToggleButton.SetActive(Value: Boolean);
begin
  gtk_toggle_button_set_active(Handle, Value);
end;

procedure TCustomToggleButton.SetDrawIndicator(Value: Boolean);
begin
  gtk_toggle_button_set_mode(Handle, Value);
end;

procedure TCustomToggleButton.SetInconsistent(Value: Boolean);
begin
  gtk_toggle_button_set_inconsistent(Handle, Value);
end;

{$ENDIF}
