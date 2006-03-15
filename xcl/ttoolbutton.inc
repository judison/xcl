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
  public
  published
    property Caption: String read GetCaption write SetCaption stored IsCaptionStored;
    property UseUnderline: Boolean read GetUseUnderline write SetUseUnderline default False;
    property StockId: String read GetStockId write SetStockId stored IsStockIDStored;
    // Events
    property OnClicked: TNotifyEvent read FOnClicked write FOnClicked;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

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
  if Assigned(FOnClicked) then
    FOnClicked(Self);
end;

function TToolButton.IsCaptionStored: Boolean;
begin
  Result := Caption <> '';
end;

function TToolButton.IsStockIDStored: Boolean;
begin
  Result := StockID <> '';
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
