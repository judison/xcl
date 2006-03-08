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

  TClipboard = class
  private
  protected
  public
    Handle: Pointer;
    //--
    constructor Create(AClipboardAtom: LongWord);
    destructor Destroy; override;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TClipboard }

constructor TClipboard.Create(AClipboardAtom: LongWord);
begin
  Handle := gtk_clipboard_get(AClipboardAtom);
end;

destructor TClipboard.Destroy;
begin
  inherited;
end;

{$ENDIF}
