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

  TToolButton = class(TCustomToolItem)
  private
    FOnClicked: TNotifyEvent;
    procedure SetCaption(AValue: String);
    function GetCaption: String;
    procedure SetUseUnderline(AValue: Boolean);
    function GetUseUnderline: Boolean;
    procedure SetStockId(AValue: String);
    function GetStockId: String;
    function IsCaptionStored: Boolean;
    function IsStockIDStored: Boolean;
  protected
    procedure CreateHandle; override;
    procedure ConnectSignals; override;
    //--
    procedure DoClicked(var Msg: TGtkSig); message 'GTK:clicked';
    //--
    procedure SetAction(AValue: TAction); override;
    procedure DoActionSetCaption(AValue: String); override;
    procedure DoActionSetStockID(AValue: String); override;
  public
  published
    property Action;
    property Caption: String read GetCaption write SetCaption stored IsCaptionStored;
    property UseUnderline: Boolean read GetUseUnderline write SetUseUnderline default False;
    property StockId: String read GetStockId write SetStockId stored IsStockIDStored;
    // Events
    property OnClicked: TNotifyEvent read FOnClicked write FOnClicked;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TToolButton }

procedure TToolButton.CreateHandle;
begin
  Handle := gtk_tool_button_new(nil, nil);
end;

procedure TToolButton.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('clicked');
end;

procedure TToolButton.DoClicked(var Msg: TGtkSig);
begin
  if Action <> nil then
    ActionLink.Execute(Self);
  //--
  if Assigned(FOnClicked) then
    FOnClicked(Self);
end;

procedure TToolButton.SetAction(AValue: TAction);
begin
  inherited;
  if Action <> nil then
  begin
    DoActionSetStockID(Action.StockID);
    if Action.Caption <> '' then
      DoActionSetCaption(Action.Caption);
  end;
end;

procedure TToolButton.DoActionSetCaption(AValue: String);
begin
  Caption := AValue;
end;

procedure TToolButton.DoActionSetStockID(AValue: String);
begin
  StockID := AValue;
end;

function TToolButton.IsCaptionStored: Boolean;
begin
  Result := ((Action = nil) and (Caption <> '')) or ((Action <> nil) and (Action.Caption <> Caption));
end;

function TToolButton.IsStockIDStored: Boolean;
begin
  Result := ((Action = nil) and (StockID <> '')) or ((Action <> nil) and (Action.StockID <> StockID));
end;

procedure TToolButton.SetCaption(AValue: String);
begin
  gtk_tool_button_set_label(Handle, PChar(AValue));
end;

function TToolButton.GetCaption: String;
begin
  Result := gtk_tool_button_get_label(Handle);
end;

procedure TToolButton.SetUseUnderline(AValue: Boolean);
begin
  gtk_tool_button_set_use_underline(Handle, AValue);
end;

function TToolButton.GetUseUnderline: Boolean;
begin
  Result := gtk_tool_button_get_use_underline(Handle);
end;

procedure TToolButton.SetStockId(AValue: String);
begin
  gtk_tool_button_set_stock_id(Handle, PChar(AValue));
end;

function TToolButton.GetStockId: String;
begin
  Result := gtk_tool_button_get_stock_id(Handle)
end;

{$ENDIF}
