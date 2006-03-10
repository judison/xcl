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

  TLabel = class(TMiscControl)
  private
    FCaption: String;
    FFocusControl: TControl;
    function GetCaption: string;
    function GetJustify: TJustification;
    function GetLineWrap: Boolean;
    function GetSelectable: Boolean;
    function GetUseMarkup: Boolean;
    function GetUseUnderline: Boolean;
    procedure SetCaption(AValue: string);
    procedure SetFocusControl(AValue: TControl);
    procedure SetJustify(AValue: TJustification);
    procedure SetLineWrap(AValue: Boolean);
    procedure SetSelectable(AValue: Boolean);
    procedure SetUseMarkup(AValue: Boolean);
    procedure SetUseUnderline(AValue: Boolean);
    function GetEllipsize: TEllipsizeMode;
    procedure SetEllipsize(AValue: TEllipsizeMode);
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption: string read GetCaption write SetCaption;
    property FocusControl: TControl read FFocusControl write SetFocusControl;
    property Justify: TJustification read GetJustify write SetJustify default jsLeft;
    property LineWrap: Boolean read GetLineWrap write SetLineWrap default False;
    property Selectable: Boolean read GetSelectable write SetSelectable default False;
    property UseMarkup: Boolean read getUseMarkup write SetUseMarkup default False;
    property UseUnderline: Boolean read getUseUnderline write SetUseUnderline default False;
    property Ellipsize: TEllipsizeMode read GetEllipsize write SetEllipsize default emNone;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TLabel }

constructor TLabel.Create(AOwner: TComponent);
begin
  inherited;
  FFocusControl := nil;
end;

procedure TLabel.CreateHandle;
begin
  Handle := gtk_label_new('');
end;

function TLabel.GetCaption: string;
begin
  Result := gtk_label_get_label(Handle);
end;

function TLabel.GetJustify: TJustification;
begin
  Result := TJustification(gtk_label_get_justify(Handle));
end;

function TLabel.GetLineWrap: Boolean;
begin
  Result := gtk_label_get_line_wrap(Handle);
end;

function TLabel.GetSelectable: Boolean;
begin
  Result := gtk_label_get_selectable(Handle);
end;

function TLabel.GetUseMarkup: Boolean;
begin
  Result := gtk_label_get_use_markup(Handle);
end;

function TLabel.GetUseUnderline: Boolean;
begin
  Result := gtk_label_get_use_underline(Handle);
end;

procedure TLabel.SetCaption(AValue: string);
begin
  FCaption := AValue;
  if not UseUnderline then
    AValue := StringReplace(FCaption, '_', '', [rfReplaceAll]);
  gtk_label_set_label(Handle, PChar(AValue));
end;

procedure TLabel.SetFocusControl(AValue: TControl);
begin
  if AValue <> FFocusControl then
  begin
    FFocusControl := AValue;
    if FFocusControl = nil then
      gtk_label_set_mnemonic_widget(Handle, nil)
    else
      gtk_label_set_mnemonic_widget(Handle, FFocusControl.Handle);
  end;
end;

procedure TLabel.SetJustify(AValue: TJustification);
begin
  gtk_label_set_justify(Handle, Ord(AValue));
end;

procedure TLabel.SetLineWrap(AValue: Boolean);
begin
  gtk_label_set_line_wrap(Handle, AValue);
end;

procedure TLabel.SetSelectable(AValue: Boolean);
begin
  gtk_label_set_selectable(Handle, AValue);
end;

procedure TLabel.SetUseMarkup(AValue: Boolean);
begin
  gtk_label_set_use_markup(Handle, AValue);
end;

procedure TLabel.SetUseUnderline(AValue: Boolean);
begin
  gtk_label_set_use_underline(Handle, AValue);
  if AValue and (Pos('_', FCaption) <> 0) then
    gtk_label_set_label(Handle, PChar(FCaption));
end;

function TLabel.GetEllipsize: TEllipsizeMode;
begin
  Result := TEllipsizeMode(gtk_label_get_ellipsize(Handle));
end;

procedure TLabel.SetEllipsize(AValue: TEllipsizeMode);
begin
  gtk_label_set_ellipsize(Handle, Ord(AValue));
end;

{$ENDIF}
