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

  TIconTheme = class(TObject)
  private
  protected
    Handle: Pointer;
  public
    constructor Create(AHandle: Pointer);
    destructor Destroy; override;
    //--
    function GetHandle: Pointer;
    //==
    procedure AppendSearchPath(APath: String);
    procedure PrependSearchPath(APath: String);
    function HasIcon(AName: String): Boolean;
    //--
    function LoadIcon(AName: String; ASize: Integer): Pointer; // PGdkPixbuf
  published
  end;


{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TIconTheme }

constructor TIconTheme.Create(AHandle: Pointer);
begin
  Handle := AHandle;
end;

destructor TIconTheme.Destroy;
begin
  inherited;
end;

function TIconTheme.GetHandle: Pointer;
begin
  Result := Handle;
end;

procedure TIconTheme.AppendSearchPath(APath: String);
begin
  gtk_icon_theme_append_search_path(Handle, PChar(APath));
end;

procedure TIconTheme.PrependSearchPath(APath: String);
begin
  gtk_icon_theme_prepend_search_path(Handle, PChar(APath));
end;

function TIconTheme.HasIcon(AName: String): Boolean;
begin
  Result := gtk_icon_theme_has_icon(Handle, PChar(AName));
end;

function TIconTheme.LoadIcon(AName: String; ASize: Integer): Pointer;
var
  Err: PGError;
begin
  Err := nil;
  Result := gtk_icon_theme_load_icon(Handle, PChar(AName), ASize, 4{GTK_ICON_LOOKUP_USE_BUILTIN}, nil);
  RaiseGError(Err);
end;

{$ENDIF}
