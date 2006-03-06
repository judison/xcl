(*
   $Id: ifilechooser.inc,v 1.8 2005/04/06 00:37:02 judison Exp $

   XCL - XDE's Component Library
   Copyright (C) 2005 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

//** INTERFACE
//****************************************************************************

{$IFDEF INTF_PRIVATE}
    procedure SetFileAction(AValue: TFileChooserAction);
    function GetFileAction: TFileChooserAction;
    procedure SetLocalOnly(AValue: Boolean);
    function GetLocalOnly: Boolean;
    procedure SetSelectMultiple(AValue: Boolean);
    function GetSelectMultiple: Boolean;
    procedure SetShowHidden(AValue: Boolean);
    function GetShowHidden: Boolean;
    procedure SetFileName(AValue: String);
    function GetFileName: String;
    procedure SetCurrentFolder(AValue: String);
    function GetCurrentFolder: String;
    procedure SetURI(AValue: String);
    function GetURI: String;
    procedure SetCurrentFolderURI(AValue: String);
    function GetCurrentFolderURI: String;
{$ENDIF}
{$IFDEF INTF_PROTECTED}

{$ENDIF}
{$IFDEF INTF_PUBLIC}
    property FileName: String read GetFileName write SetFileName;
    property CurrentFolder: String read GetCurrentFolder write SetCurrentFolder;
    property URI: String read GetURI write SetURI;
    property CurrentFolderURI: String read GetCurrentFolderURI write SetCurrentFolderURI;
{$ENDIF}
{$IFDEF INTF_PUBLISHED}
    property FileAction: TFileChooserAction read GetFileAction write SetFileAction default fcaOpen;
    property LocalOnly: Boolean read GetLocalOnly write SetLocalOnly default True;
    property SelectMultiple: Boolean read GetSelectMultiple write SetSelectMultiple default False;
    property ShowHidden: Boolean read GetShowHidden write SetShowHidden default False;
{$ENDIF}

//** IMPLEMENTATION
//****************************************************************************

{$IFDEF INTF_IMPLEMENTATION}

procedure the_class.SetFileAction(AValue: TFileChooserAction);
begin
  gtk_file_chooser_set_action(Handle, Ord(AValue));
end;

function the_class.GetFileAction: TFileChooserAction;
begin
  Result := TFileChooserAction(gtk_file_chooser_get_action(Handle));
end;

procedure the_class.SetLocalOnly(AValue: Boolean);
begin
  gtk_file_chooser_set_local_only(Handle, AValue);
end;

function the_class.GetLocalOnly: Boolean;
begin
  Result := gtk_file_chooser_get_local_only(Handle);
end;

procedure the_class.SetSelectMultiple(AValue: Boolean);
begin
  gtk_file_chooser_set_select_multiple(Handle, AValue);
end;

function the_class.GetSelectMultiple: Boolean;
begin
  Result := gtk_file_chooser_get_select_multiple(Handle);
end;

procedure the_class.SetShowHidden(AValue: Boolean);
begin
  gtk_file_chooser_set_show_hidden(Handle, AValue);
end;

function the_class.GetShowHidden: Boolean;
begin
  Result := gtk_file_chooser_get_show_hidden(Handle);
end;

procedure the_class.SetFileName(AValue: String);
begin
  gtk_file_chooser_set_filename(Handle, PChar(AValue));
end;

function the_class.GetFileName: String;
var
  R	: PChar;
begin
  R := gtk_file_chooser_get_filename(Handle);
  Result := R;
  g_free(R);
end;

procedure the_class.SetCurrentFolder(AValue: String);
begin
//TODO: tratar excessoes
  gtk_file_chooser_set_current_folder(Handle, PChar(AValue));
end;

function the_class.GetCurrentFolder: String;
var
  R	: PChar;
begin
  R := gtk_file_chooser_get_current_folder(Handle);
  Result := R;
  g_free(R);
end;

procedure the_class.SetURI(AValue: String);
begin
  gtk_file_chooser_set_uri(Handle, PChar(AValue));
end;

function the_class.GetURI: String;
var
  R	: PChar;
begin
  R := gtk_file_chooser_get_uri(Handle);
  Result := R;
  g_free(R);
end;

procedure the_class.SetCurrentFolderURI(AValue: String);
begin
  gtk_file_chooser_set_current_folder_uri(Handle, PChar(AValue));
end;

function the_class.GetCurrentFolderURI: String;
var
  R	: PChar;
begin
  R := gtk_file_chooser_get_current_folder_uri(Handle);
  Result := R;
  g_free(R);
end;

{$ENDIF}
{
  $Log: ifilechooser.inc,v $
  Revision 1.8  2005/04/06 00:37:02  judison
  * IFileChooser.Action -> FileAction

  Revision 1.7  2005/03/28 06:34:56  judison
  *** empty log message ***

  Revision 1.6  2005/03/28 06:03:39  judison
  *** empty log message ***

  Revision 1.5  2005/03/28 05:52:42  judison
  * GtkFileChooser Interface

  Revision 1.4  2005/03/26 05:21:47  judison
  + CVS Log Tag

}
