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

  TCalendar = class(TControl)
  private
    //---
    FOnDaySelected: TNotifyEvent;
    FOnDaySelectedDoubleClick: TNotifyEvent;
    FOnMonthChanged: TNotifyEvent;
    FOnChange: TNotifyEvent;
    //---
    function GetDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  protected
    procedure CreateHandle; override;
    procedure ConnectSignals; override;
    //--
    procedure DoDaySelected(var Msg: TGtkSig); message 'GTK:day-selected';
    procedure DoDaySelectedDoubleClick(var Msg: TGtkSig); message 'GTK:day-selected-double-click';
    procedure DoMonthChanged(var Msg: TGtkSig); message 'GTK:month-changed';
  public
  published
    property Date: TDateTime read GetDate write SetDate;
    //-- Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDaySelected: TNotifyEvent read FOnDaySelected write FOnDaySelected;
    property OnDaySelectedDoubleClick: TNotifyEvent read FOnDaySelectedDoubleClick write FOnDaySelectedDoubleClick;
    property OnMonthChanged: TNotifyEvent read FOnMonthChanged write FOnMonthChanged;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TCalendar }

procedure TCalendar.CreateHandle;
begin
  Handle := gtk_calendar_new;
end;

procedure TCalendar.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('day-selected');
  ConnectGtkSignal('day-selected-double-click');
  ConnectGtkSignal('month-changed');
end;

function TCalendar.GetDate: TDateTime;
var
  lY,lM,lD: dword;
begin
  gtk_calendar_get_date(Handle, @lY, @lM, @lD);
  Result := EncodeDate(lY, lM+1, lD);
end;

procedure TCalendar.SetDate(AValue: TDateTime);
var
  lY,lM,lD: Word;
  lcY, lcM, lcD: dword;
begin
  DecodeDate(AValue, lY, lM, lD);
  lcY := lY;
  lcM := lM;
  lcD := lD;
  gtk_calendar_select_month(Handle, lcM, lcY);
  gtk_calendar_select_day(Handle, lcD);
end;

procedure TCalendar.DoDaySelected(var Msg: TGtkSig);
begin
  if Assigned(FOnDaySelected) then
    FOnDaySelected(Self);
  //---
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCalendar.DoDaySelectedDoubleClick(var Msg: TGtkSig);
begin
  if Assigned(FOnDaySelectedDoubleClick) then
    FOnDaySelectedDoubleClick(Self);
end;

procedure TCalendar.DoMonthChanged(var Msg: TGtkSig);
begin
  if Assigned(FOnMonthChanged) then
    FOnMonthChanged(Self);
end;

{$ENDIF}
