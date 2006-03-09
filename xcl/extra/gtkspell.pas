(*
   Extra Components for XCL/XDE
   Copyright (C) 2006 Judison Oliveira Gil Filho <judison@gmail.com>

   TGtkSpell - GtkSpell for XCL

   GtkSpell provides word-processor-style highlighting and replacement
   of misspelled words in a GtkTextView widget. Right-clicking a
   misspelled word pops up a menu of suggested replacements.

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit gtkspell;

{$H+}
{$MODE ObjFpc}

interface

uses Classes, xcl;

type
  TGtkSpell = class(TComponent)
  private
    Handle: Pointer;
    FTextView: TTextView;
    FLanguage: String;
    procedure SetTextView(AValue: TTextView);
    procedure SetLanguage(AValue: String);
    procedure Detach; inline;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecheckAll;
  published
    property TextView: TTextView read FTextView write SetTextView;
    property Language: String read FLanguage write SetLanguage;
  end;

procedure Register;

const
{$IFDEF WIN32}
  gtkspelllib = 'libgtkspell.dll';
  {$IFDEF FPC}
    {$SMARTLINK ON}
  {$ENDIF}
{$ELSE}
  gtkspelllib = 'libgtkspell.so';
{$ENDIF}

implementation

uses sysutils, glib2, gtk2;

//==================
// gtkspell.h
function  gtkspell_new_attach(view: PGtkTextView; lang: PGChar; error: PPGError): Pointer; cdecl; external gtkspelllib;
function  gtkspell_get_from_text_view(view: PGtkTextView): Pointer; cdecl; external gtkspelllib;
procedure gtkspell_detach(spell: Pointer); cdecl; external gtkspelllib;
function  gtkspell_set_language(spell: Pointer; lang: PGChar; error: PPGError): gboolean; cdecl; external gtkspelllib;
procedure gtkspell_recheck_all(spell: Pointer); cdecl; external gtkspelllib;
//==================

{ TGtkSpell }

constructor TGtkSpell.Create(AOwner: TComponent);
begin
  inherited;
  Handle := nil;
  FLanguage := '';
end;

destructor TGtkSpell.Destroy;
begin
//  Detach; // This causes EAccessViolation
//TODO: use notification and bla bla
  inherited;
end;

procedure TGtkSpell.Detach; inline;
begin
  if (Handle <> nil) then
  begin
    gtkspell_detach(Handle);
    Handle := nil;
  end;
end;

procedure TGtkSpell.SetTextView(AValue: TTextView);
begin
  if AValue <> FTextView then
  begin
    Detach;

    FTextView := AValue;

    if FTextView <> nil then
    begin
      if FLanguage = '' then
        Handle := gtkspell_new_attach(FTextView.GetHandle, nil, nil)
      else
        Handle := gtkspell_new_attach(FTextView.GetHandle, PChar(FLanguage), nil);

      if Handle = nil then
      begin
        FTextView := nil;
        raise Exception.Create('GtkSpell can''t attach to this TextView.');
      end;
    end;
  end;
end;

procedure TGtkSpell.SetLanguage(AValue: String);
begin
  if AValue <> FLanguage then
  begin
    if gtkspell_set_language(Handle, PChar(AValue), nil) then
      FLanguage := AValue
    else
      raise Exception.Create('Can''t set this language.');
  end;
end;

procedure TGtkSpell.RecheckAll;
begin
  gtkspell_recheck_all(Handle);
end;

procedure Register;
begin
  RegisterComponents('Extra', [TGtkSpell]);
end;

initialization
  RegisterClass(TGtkSpell);
end.
