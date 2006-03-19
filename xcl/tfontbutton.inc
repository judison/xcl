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

  TFontButton = class(TCustomButton)
  private
    function GetFontName: String;
    procedure SetFontName(AValue: String);
    function IsFontNameStored: Boolean;
    function GetShowStyle: Boolean;
    procedure SetShowStyle(AValue: Boolean);
    function GetShowSize: Boolean;
    procedure SetShowSize(AValue: Boolean);
    function GetUseFont: Boolean;
    procedure SetUseFont(AValue: Boolean);
    function GetUseSize: Boolean;
    procedure SetUseSize(AValue: Boolean);
    function GetTitle: String;
    procedure SetTitle(AValue: String);
    function IsTitleStored: Boolean;
  protected
    procedure CreateHandle; override;
  public
  published
    property FontName: String read GetFontName write SetFontName stored IsFontNameStored;
    property ShowStyle: Boolean read GetShowStyle write SetShowStyle default True;
    property ShowSize: Boolean read GetShowSize write SetShowSize default True;
    property UseFont: Boolean read GetUseFont write SetUseFont default False;
    property UseSize: Boolean read GetUseSize write SetUseSize default False;
    property Title: String read GetTitle write SetTitle stored IsTitleStored;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TFontButton }

procedure TFontButton.CreateHandle;
begin
  Handle := gtk_font_button_new();
end;

function TFontButton.GetFontName: String;
begin
  Result := gtk_font_button_get_font_name(Handle);
end;

procedure TFontButton.SetFontName(AValue: String);
begin
  gtk_font_button_set_font_name(Handle, PChar(AValue));
end;

function TFontButton.IsFontNameStored: Boolean;
begin
  Result := FontName <> 'Sans 12';
end;

function TFontButton.GetShowStyle: Boolean;
begin
  Result := gtk_font_button_get_show_style(Handle);
end;

procedure TFontButton.SetShowStyle(AValue: Boolean);
begin
  gtk_font_button_set_show_style(Handle, AValue);
end;

function TFontButton.GetShowSize: Boolean;
begin
  Result := gtk_font_button_get_show_size(Handle);
end;

procedure TFontButton.SetShowSize(AValue: Boolean);
begin
  gtk_font_button_set_show_size(Handle, AValue);
end;

function TFontButton.GetUseFont: Boolean;
begin
  Result := gtk_font_button_get_use_font(Handle);
end;

procedure TFontButton.SetUseFont(AValue: Boolean);
begin
  gtk_font_button_set_use_font(Handle, AValue);
end;

function TFontButton.GetUseSize: Boolean;
begin
  Result := gtk_font_button_get_use_size(Handle);
end;

procedure TFontButton.SetUseSize(AValue: Boolean);
begin
  gtk_font_button_set_use_size(Handle, AValue);
end;

function TFontButton.GetTitle: String;
begin
  Result := gtk_font_button_get_title(Handle);
end;

procedure TFontButton.SetTitle(AValue: String);
begin
  gtk_font_button_set_title(Handle, PChar(AValue));
end;

function TFontButton.IsTitleStored: Boolean;
begin
  Result := FontName <> 'Pick a Font';
end;

{$ENDIF}
