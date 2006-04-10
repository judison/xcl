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

type
  EGError = class(Exception)
  public
    Domain: TQuark;
    Code: LongInt;
    constructor Create(ADomain: TQuark; ACode: LongInt; AMsg: String);
  end;

  // GLib ==================================
  EGConvertError = class(EGError);
  EGFileError = class(EGError);
  EGIOChannelError = class(EGError);
  EGMarkupError = class(EGError);
  EGShellError = class(EGError);
  EGSpawnError = class(EGError);

  // GDK ===================================
  EGdkError = class(EGError);
  EGdkPixbufError = class(EGdkError);

  // GTK ===================================
  EGtkError = class(EGError);
  EGtkFileChooserError = class(EGtkError);
  EGtkFileSystemError = class(EGtkError);
  EGtkIconThemeError = class(EGtkError);

procedure RaiseGError(var AError: Pointer{PGError});

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

procedure RaiseGError(var AError: Pointer{PGError});
var
  Domain: TQuark;
  Code: LongInt;
  Msg: String;
  DN: String;
begin
  if Assigned(AError) then
  begin
    Domain := PGError(AError)^.domain;
    Code := PGError(AError)^.code;
    Msg := PGError(AError)^.message;
    g_error_free(AError);
    //== GLib =======================================
    if {--} Domain = g_convert_error_quark then
      raise EGConvertError.Create(Domain, Code, Msg)
    else if Domain = g_file_error_quark then
      raise EGFileError.Create(Domain, Code, Msg)
    else if Domain = g_io_channel_error_quark then
      raise EGIOChannelError.Create(Domain, Code, Msg)
    else if Domain = g_markup_error_quark then
      raise EGMarkupError.Create(Domain, Code, Msg)
    else if Domain = g_shell_error_quark then
      raise EGShellError.Create(Domain, Code, Msg)
    else if Domain = g_spawn_error_quark then
      raise EGSpawnError.Create(Domain, Code, Msg)
    //== Gdk ========================================
    else if Domain = gdk_pixbuf_error_quark then
      raise EGdkPixbufError.Create(Domain, Code, Msg)
    //== Gtk ========================================
    else if Domain = gtk_file_chooser_error_quark then
      raise EGtkFileChooserError.Create(Domain, Code, Msg)
    else if Domain = gtk_file_system_error_quark then
      raise EGtkFileSystemError.Create(Domain, Code, Msg)
    else if Domain = gtk_icon_theme_error_quark then
      raise EGtkIconThemeError.Create(Domain, Code, Msg)
    else //==========================================
    begin
      DN := UpperCase(Copy(QuarkName(Domain), 1, 4));
      if {--} (DN = 'GDK-') or (DN = 'GDK_') then
        raise EGdkError.Create(Domain, Code, Msg)
      else if (DN = 'GTK-') or (DN = 'GTK_') then
        raise EGtkError.Create(Domain, Code, Msg)
      else
        raise EGError.Create(Domain, Code, Msg);
    end;
  end;
end;

constructor EGError.Create(ADomain: TQuark; ACode: LongInt; AMsg: String);
begin
  inherited Create(AMsg);
  Domain := ADomain;
  Code := ACode;
end;

{$ENDIF}
