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

  TExpander = class(TBinControl)
  private
    function GetCaption: String;
    procedure SetCaption(AValue: String);
    function GetExpanded: Boolean;
    procedure SetExpanded(AValue: Boolean);
    function GetSpacing: Integer;
    procedure SetSpacing(AValue: Integer);
    function GetUseUnderline: Boolean;
    procedure SetUseUnderline(AValue: Boolean);
    function GetUseMarkup: Boolean;
    procedure SetUseMarkup(AValue: Boolean);
  protected
    procedure CreateHandle; override;
  public
  published
    property Caption: String read GetCaption write SetCaption;
    property Expanded: Boolean read GetExpanded write SetExpanded default True;
    property Spacing: Integer read GetSpacing write SetSpacing;
    property UseUnderline: Boolean read GetUseUnderline write SetUseUnderline;
    property UseMarkup: Boolean read GetUseMarkup write SetUseMarkup;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TExpander }

procedure TExpander.CreateHandle;
begin
  Handle := gtk_expander_new('Expander');
end;

function TExpander.GetCaption: String;
begin
  Result := gtk_expander_get_label(Handle);
end;

procedure TExpander.SetCaption(AValue: String);
begin
  gtk_expander_set_label(Handle, PChar(AValue));
end;

function TExpander.GetExpanded: Boolean;
begin
  Result := gtk_expander_get_expanded(Handle);
end;

procedure TExpander.SetExpanded(AValue: Boolean);
begin
  gtk_expander_set_expanded(Handle, AValue);
end;

function TExpander.GetSpacing: Integer;
begin
  Result := gtk_expander_get_spacing(Handle);
end;

procedure TExpander.SetSpacing(AValue: Integer);
begin
  gtk_expander_set_spacing(Handle, AValue);
end;

function TExpander.GetUseUnderline: Boolean;
begin
  Result := gtk_expander_get_use_underline(Handle);
end;

procedure TExpander.SetUseUnderline(AValue: Boolean);
begin
  gtk_expander_set_use_underline(Handle, AValue);
end;

function TExpander.GetUseMarkup: Boolean;
begin
  Result := gtk_expander_get_use_markup(Handle);
end;

procedure TExpander.SetUseMarkup(AValue: Boolean);
begin
  gtk_expander_set_use_markup(Handle, AValue);
end;

{$ENDIF}
