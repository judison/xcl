(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{** INTERFACE *************************************}{$IFDEF INC_READ_INTERFACE}

  TCustomEntry = class(TControl)
  {$I ieditablecontrol.inc}
  private
    FOnChanged: TNotifyEvent;
    function GetHasFrame: Boolean;
    procedure SetHasFrame(AValue: Boolean);
    function GetVisibility: Boolean;
    procedure SetVisibility(AValue: Boolean);
    function GetMaxLength: Integer;
    procedure SetMaxLength(AValue: Integer);
    function GetWidthChars: Integer;
    procedure SetWidthChars(AValue: Integer);
  protected
    procedure CreateHandle; override;
    procedure ConnectSignals; override;
    //== Do's
    procedure DoChanged(var Msg: TGtkSig); message 'GTK:changed'; virtual;
    //-- Text
    FText: String; // duplicated :(
    function GetText: String; inline;
    procedure SetText(AValue: String); inline;
  public
  published
    property HasFrame: Boolean read GetHasFrame write SetHasFrame default True;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property Text: String read FText write SetText;
    property Visibility: Boolean read GetVisibility write SetVisibility default True;
    property WidthChars: Integer read GetWidthChars write SetWidthChars default -1;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TEntry = class(TCustomEntry)
  private
  protected
  public
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TCustomEntry }

{$define the_class := TCustomEntry }
  {$I ieditablecontrol.inc}
{$undef the_class}

procedure TCustomEntry.CreateHandle;
begin
  Handle := gtk_entry_new;
  FText := GetText;
end;

procedure TCustomEntry.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('changed');
end;

procedure TCustomEntry.DoChanged(var Msg: TGtkSig);
begin
  FText := GetText;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TCustomEntry.GetText: string; inline;
begin
  Result := gtk_entry_get_text(Handle);
end;

procedure TCustomEntry.SetText(AValue: string); inline;
begin
//  if AValue <> FText then
//  begin
    FText := AValue;
    gtk_entry_set_text(Handle, PChar(FText));
//  end;
end;

function TCustomEntry.GetVisibility: Boolean;
begin
  Result := gtk_entry_get_visibility(Handle);
end;

procedure TCustomEntry.SetVisibility(AValue: Boolean);
begin
  gtk_entry_set_visibility(Handle, AValue);
end;

function TCustomEntry.GetHasFrame: Boolean;
begin
  Result := gtk_entry_get_has_frame(Handle);
end;

procedure TCustomEntry.SetHasFrame(AValue: Boolean);
begin
  gtk_entry_set_has_frame(Handle, AValue);
end;

function TCustomEntry.GetMaxLength: Integer;
begin
  Result := gtk_entry_get_max_length(Handle);
end;

procedure TCustomEntry.SetMaxLength(AValue: Integer);
begin
  gtk_entry_set_max_length(Handle, AValue);
end;

function TCustomEntry.GetWidthChars: Integer;
begin
  Result := gtk_entry_get_width_chars(Handle);
end;

procedure TCustomEntry.SetWidthChars(AValue: Integer);
begin
  gtk_entry_set_width_chars(Handle, AValue);
end;

{$ENDIF}
