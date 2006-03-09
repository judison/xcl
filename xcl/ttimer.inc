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

  TTimer = class(TComponent)
  private
    Handle: Cardinal;
    FActive: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    procedure SetActive(AValue: Boolean);
    procedure SetInterval(AValue: Cardinal);
  protected
    procedure DoTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Interval: Cardinal read FInterval write SetInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

function Hnd_TTimer_Timer(Self: TTimer): Boolean; cdecl;
begin
  Self.DoTimer;
  Result := True;
end;

{ TTimer }

constructor TTimer.Create(AOwner: TComponent);
begin
  inherited;
  FActive := False;
  FInterval := 1000;
  Handle := 0;
end;

destructor TTimer.Destroy;
begin
  SetActive(False);
  inherited;
end;

procedure TTimer.SetActive(AValue: Boolean);
begin
  if (FActive <> AValue) then
  begin
    if AValue then
      Handle := gtk_timeout_add(FInterval, TGtkFunction(@Hnd_TTimer_Timer), Pointer(Self))
    else
      gtk_timeout_remove(Handle);
    FActive := AValue;
  end;
end;

procedure TTimer.SetInterval(AValue: Cardinal);
var
  OldActive: Boolean;
begin
  if AValue <> FInterval then
  begin
    OldActive := FActive;
    SetActive(False);
    FInterval := AValue;
    SetActive(OldActive);
  end;
end;

procedure TTimer.DoTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

{$ENDIF}
