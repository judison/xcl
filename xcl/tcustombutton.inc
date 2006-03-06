(*
   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
//** INTERFACE
//****************************************************************************
{$IFDEF INC_READ_INTERFACE}

  TCustomButton = class(TBinControl)
  private
    FOnClicked: TNotifyEvent;
    function GetCaption: string;
    function GetFocusOnClick: Boolean;
    function GetRelief: TRelief;
    function GetUseStock: Boolean;
    function GetUseUnderline: Boolean;
    procedure SetCaption(Value: string);
    procedure SetFocusOnClick(Value: Boolean);
    procedure SetRelief(Value: TRelief);
    procedure SetUseStock(Value: Boolean);
    procedure SetUseUnderline(Value: Boolean);
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure ConnectSignals; override;
    //--
    procedure AddControl(AControl: TControl); override;
    procedure RemoveControl(AControl: TControl); override;
    //--
    procedure DoClicked(var Msg: TGtkSig); message 'GTK:clicked';
    // Not published yet
    property Caption: string read GetCaption write SetCaption;
    property UseStock: Boolean read GetUseStock write SetUseStock default False;
    property UseUnderline: Boolean read GetUseUnderline write SetUseUnderline default False;
  public
  published
    property FocusOnClick: Boolean read GetFocusOnClick write SetFocusOnClick default True;
    property Relief: TRelief read GetRelief write SetRelief default rlfNormal;
    //-- Events
    property OnClicked: TNotifyEvent read FOnClicked write FOnClicked;
  end;


{$ENDIF}
//** IMPLEMENTATION
//****************************************************************************
{$IFDEF INC_READ_IMPLEMENTATION}

{ TCustomButton }

procedure TCustomButton.CreateHandle;
begin
  Handle := gtk_button_new;
end;

procedure TCustomButton.DestroyHandle;
begin
  inherited;
end;

procedure TCustomButton.AddControl(AControl: TControl);
begin
  inherited;
end;

procedure TCustomButton.RemoveControl(AControl: TControl);
begin
  inherited;
end;

function TCustomButton.GetCaption: string;
begin
  Result := gtk_button_get_label(Handle);
end;

function TCustomButton.GetFocusOnClick: Boolean;
begin
  Result := gtk_button_get_focus_on_click(Handle);
end;

function TCustomButton.GetRelief: TRelief;
begin
  Result := TRelief(gtk_button_get_relief(Handle));
end;

function TCustomButton.GetUseStock: Boolean;
begin
  Result := gtk_button_get_use_stock(Handle);
end;

function TCustomButton.GetUseUnderline: Boolean;
begin
  Result := gtk_button_get_use_underline(Handle);
end;

procedure TCustomButton.SetCaption(Value: string);
begin
  gtk_button_set_label(Handle, PChar(Value));
end;

procedure TCustomButton.SetFocusOnClick(Value: Boolean);
begin
  gtk_button_set_focus_on_click(Handle, Value);
end;

procedure TCustomButton.SetRelief(Value: TRelief);
begin
  gtk_button_set_relief(Handle, Ord(Value));
end;

procedure TCustomButton.SetUseStock(Value: Boolean);
begin
  gtk_button_set_use_stock(Handle, Value);
end;

procedure TCustomButton.SetUseUnderline(Value: Boolean);
begin
  gtk_button_set_use_underline(Handle, Value);
end;

procedure TCustomButton.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('clicked');
end;

procedure TCustomButton.DoClicked(var Msg: TGtkSig);
begin
  if Assigned(FOnClicked) then
    FOnClicked(Self);
end;


{$ENDIF}
