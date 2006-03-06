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

  TCombo = class(TControl)
  private
    FItems: TStrings;
    function GetText: string;
    procedure SetText(Value: string);
    procedure SetItems(AValue: TStrings);
  protected
    procedure CreateHandle; override;
    procedure ItemsChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStrings read FItems write SetItems;
    property Text: string read GetText write SetText;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{DEFINE USE_TGSTRINGS}

{ TCombo }

constructor TCombo.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF USE_TGSTRINGS}
  FItems := TGStrings.Create(nil);
{$ELSE}
  FItems := TStringList.Create;
{$ENDIF}
  TStringList(FItems).OnChange := @ItemsChange;
end;

destructor TCombo.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TCombo.CreateHandle;
begin
  Handle := gtk_combo_new;
end;

procedure TCombo.ItemsChange(Sender: TObject);
{$IFDEF USE_TGSTRINGS}
begin
  gtk_combo_set_popdown_strings(Handle, TGStrings(FItems).Handle);
{$ELSE}
var
  LL: TGStrings;
begin
  LL := TGStrings.Create(nil);
  LL.Add('Ola');
  LL.Assign(FItems);
  gtk_combo_set_popdown_strings(Handle, LL.Handle);
  LL.Free;
{$ENDIF}
end;

function TCombo.GetText: string;
begin
  Result := PChar(gtk_entry_get_text(PGtkEntry(PGtkCombo(Handle)^.Entry)));
end;

procedure TCombo.SetText(Value: string);
begin
  gtk_entry_set_text(PGtkEntry(PGtkCombo(Handle)^.Entry), PChar(Value));
end;

procedure TCombo.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

{$ENDIF}
