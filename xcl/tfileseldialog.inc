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

  TFileSelDialog = class(TDialog)
  private
    function GetFileName: String;
    procedure SetFileName(AValue: String);
  protected
    procedure CreateHandle; override;
  public
  published
    property FileName: String read GetFileName write SetFileName;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TFileSelDialog }

procedure TFileSelDialog.CreateHandle;
begin
  Handle := gtk_file_selection_new(nil);
end;

function TFileSelDialog.GetFileName: String;
begin
  Result := gtk_file_selection_get_filename(Handle);
end;

procedure TFileSelDialog.SetFileName(AValue: String);
begin
  gtk_file_selection_set_filename(Handle, PChar(AValue));
end;

{$ENDIF}
