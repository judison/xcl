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

  TAboutDialog = class(TDialog, IPixbufClient)
  private
    FLogo: TPixbuf;
    function GetAppName: String;
    procedure SetAppName(AValue: String);
    function GetVersion: String;
    procedure SetVersion(AValue: String);
    function GetCopyright: String;
    procedure SetCopyright(AValue: String);
    function GetComments: String;
    procedure SetComments(AValue: String);
    function GetLicense: String;
    procedure SetLicense(AValue: String);
    function GetWrapLicense: Boolean;
    procedure SetWrapLicense(AValue: Boolean);
    function GetWebsite: String;
    procedure SetWebsite(AValue: String);
    function GetWebsiteLabel: String;
    procedure SetWebsiteLabel(AValue: String);
    procedure SetLogo(AValue: TPixbuf);
  protected
    procedure CreateHandle; override;
    procedure PixbufChanged(APixbuf: TPixbuf);
    procedure ChangeLogo;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property AppName: String read GetAppName write SetAppName;
    property Version: String read GetVersion write SetVersion;
    property Copyright: String read GetCopyright write SetCopyright;
    property Comments: String read GetComments write SetComments;
    property License: String read GetLicense write SetLicense;
    property WrapLicense: Boolean read GetWrapLicense write SetWrapLicense;
    property Website: String read GetWebsite write SetWebsite;
    property WebsiteLabel: String read GetWebsiteLabel write SetWebsiteLabel;
    property Logo: TPixbuf read FLogo write SetLogo;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TAboutDialog }

procedure TAboutDialog.CreateHandle;
begin
  Handle := gtk_about_dialog_new();
end;

procedure TAboutDialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FLogo) and (Operation = opRemove) then
  begin
    FLogo := nil;
    ChangeLogo;
  end;
  inherited;
end;

procedure TAboutDialog.PixbufChanged(APixbuf: TPixbuf);
begin
  if APixbuf = FLogo then
    ChangeLogo;
end;

procedure TAboutDialog.ChangeLogo;
begin
  if Assigned(FLogo) then
    gtk_about_dialog_set_logo(Handle, FLogo.GetGdkPixbuf)
  else
    gtk_about_dialog_set_logo(Handle, nil);
end;

function TAboutDialog.GetAppName: String;
begin
  Result := gtk_about_dialog_get_name(Handle);
end;

procedure TAboutDialog.SetAppName(AValue: String);
begin
  gtk_about_dialog_set_name(Handle, PChar(AValue));
end;

function TAboutDialog.GetVersion: String;
begin
  Result := gtk_about_dialog_get_version(Handle);
end;

procedure TAboutDialog.SetVersion(AValue: String);
begin
  gtk_about_dialog_set_version(Handle, PChar(AValue));
end;

function TAboutDialog.GetCopyright: String;
begin
  Result := gtk_about_dialog_get_copyright(Handle);
end;

procedure TAboutDialog.SetCopyright(AValue: String);
begin
  gtk_about_dialog_set_copyright(Handle, PChar(AValue));
end;

function TAboutDialog.GetComments: String;
begin
  Result := gtk_about_dialog_get_comments(Handle);
end;

procedure TAboutDialog.SetComments(AValue: String);
begin
  gtk_about_dialog_set_comments(Handle, PChar(AValue));
end;

function TAboutDialog.GetLicense: String;
begin
  Result := gtk_about_dialog_get_license(Handle);
end;

procedure TAboutDialog.SetLicense(AValue: String);
begin
  gtk_about_dialog_set_license(Handle, PChar(AValue));
end;

function TAboutDialog.GetWrapLicense: Boolean;
begin
  Result := gtk_about_dialog_get_wrap_license(Handle);
end;

procedure TAboutDialog.SetWrapLicense(AValue: Boolean);
begin
  gtk_about_dialog_set_wrap_license(Handle, AValue);
end;

function TAboutDialog.GetWebsite: String;
begin
  Result := gtk_about_dialog_get_website(Handle);
end;

procedure TAboutDialog.SetWebsite(AValue: String);
begin
  gtk_about_dialog_set_website(Handle, PChar(AValue));
end;

function TAboutDialog.GetWebsiteLabel: String;
begin
  Result := gtk_about_dialog_get_website_label(Handle);
end;

procedure TAboutDialog.SetWebsiteLabel(AValue: String);
begin
  gtk_about_dialog_set_website_label(Handle, PChar(AValue));
end;

procedure TAboutDialog.SetLogo(AValue: TPixbuf);
begin
  if AValue <> FLogo then
  begin
    if Assigned(FLogo) then
      FLogo.RemoveFreeNotification(Self);

    FLogo := AValue;
    ChangeLogo;

    if Assigned(FLogo) then
      FLogo.FreeNotification(Self);
  end;
end;

{$ENDIF}
