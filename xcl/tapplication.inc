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

  TApplication = class(TCustomApplication)
  private
    FColormap: TColormap;
    FCurrentLocale: string;
    FMainForm: TForm;
    FActionList: TList;
    FToolTips: Pointer;
  protected
    procedure SetTitle(const AValue: string); override;
    procedure DoRun; override;
    procedure DoIdle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize; override;
    procedure ShowException(E: Exception); override;
    procedure CreateForm(AClass: TFormClass; var Ref);
    //==
    function ShowMessage(AType: TMessageType; AButtons: TMessageButtons; ATitle: String; AMsg: String; ASec: String): TMessageResponse; inline;
    function ShowMessage(AType: TMessageType; AButtons: TMessageButtons; ATitle: String; AMsg: String): TMessageResponse;
    function ShowMessage(AType: TMessageType; AButtons: TMessageButtons; AMsg: String): TMessageResponse;
    function ShowMessage(AType: TMessageType; AMsg: String): TMessageResponse;
    function ShowMessage(AMsg: String): TMessageResponse;
    // ToolTips
    procedure EnableToolTips;
    procedure DisableToolTips;
    //
    procedure ProcessMessages;
    procedure ProcessMessages(ABlock: Boolean);
    //
    procedure AddAction(AAction: TCustomAction);
    procedure RemoveAction(AAction: TCustomAction);
    //--
    property Colormap: TColormap read FColormap;
    property CurrentLocale: string read FCurrentLocale;
    property MainForm: TForm read FMainForm write FMainForm;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TApplication }

constructor TApplication.Create(AOwner: TComponent);
begin
  inherited;
  FColormap := nil;
  FActionList := TList.Create;
end;

destructor TApplication.Destroy;
begin
  if FColormap <> nil then
    FColormap.Free;

  inherited;

  if FActionList <> nil then
    FActionList.Free;
end;

procedure TApplication.EnableToolTips;
begin
  gtk_tooltips_enable(FToolTips);
end;

procedure TApplication.DisableToolTips;
begin
  gtk_tooltips_disable(FToolTips);
end;

procedure TApplication.AddAction(AAction: TCustomAction);
begin
  FActionList.Add(AAction);
end;

procedure TApplication.RemoveAction(AAction: TCustomAction);
begin
  FActionList.Remove(AAction);
end;

procedure TApplication.ProcessMessages;
begin
  ProcessMessages(False);
end;

procedure TApplication.ProcessMessages(ABlock: Boolean);
begin
  // TODO: Remove this try ... except
  try
    while (gtk_events_pending > 0) do
      gtk_main_iteration_do(False); // Don't Block

    DoIdle;

    if (not Terminated) and ABlock then
      gtk_main_iteration_do(True); // Block
  except
    on E: Exception do
    begin
      WriteLn('You are not supossed to read it!!! please tell me (judison@gmail.com)');
      ShowException(E);
    end;
  end;
end;

procedure TApplication.DoIdle;
var
  I: Integer;
begin
  // Action Lists
  I := 0;
  while I < FActionList.Count do
  begin
    try
      TCustomAction(FActionList[I]).Update;
    except
      on E: Exception do
        ShowException(E);
    end;
    Inc(I);
  end;

  // Forms
  // TODO: TForm must call some AddForm, like TCustomAction does
  for I := 0 to ComponentCount -1 do
    if Components[I] is TForm then
    try
      TForm(Components[I]).DoIdle;
    except
      on E: Exception do
        ShowException(E);
    end;
end;

procedure TApplication.DoRun;
begin
  // Show MainForm
  if FMainForm <> nil then
    try
      FMainForm.Show;
    except
      on E: Exception do
        ShowException(E);
    end;

  while (not Terminated) do
    ProcessMessages(True); // Block
end;

procedure TApplication.SetTitle(const AValue: string);
begin
  inherited;
  g_set_application_name(PChar(AValue));
end;
{
procedure hnd_log(log_domain: pgchar; log_level: TGLogLevelFlags; msg: pgchar; user_data: gpointer); cdecl;
begin
  case log_level of
    G_LOG_LEVEL_ERROR + G_LOG_FLAG_FATAL: Application.ShowMessage(mtError, [mbOK], 'Error', log_domain + '-ERROR', msg);
    G_LOG_LEVEL_CRITICAL: Application.ShowMessage(mtError, [mbOK], 'Critical', log_domain + '-CRITICAL', msg);
    G_LOG_LEVEL_WARNING: Application.ShowMessage(mtWarning, [mbOK], 'Warning', log_domain + '-WARNING', msg);
    G_LOG_LEVEL_MESSAGE: Application.ShowMessage(mtInfo, [mbOK], 'Message', log_domain + '-MESSAGE', msg);
    G_LOG_LEVEL_INFO: Application.ShowMessage(mtInfo, [mbOK], 'Info', log_domain + '-INFO', msg);
    G_LOG_LEVEL_DEBUG: Application.ShowMessage(mtInfo, [mbOK], 'Debug', log_domain + '-DEBUG', msg);
  else
    WriteLn('=====================================================');
    WriteLn('Domain : ', log_domain);
    WriteLn('Level  : ', Integer(log_level));
    WriteLn('Message: ', Msg);
  end
end;
}

procedure TApplication.Initialize;
begin
  inherited;
  FCurrentLocale := PChar(gtk_set_locale);
  gtk_init(@argc, @argv);

//  g_log_set_handler('Gtk', G_LOG_FLAG_FATAL {or G_LOG_FLAG_RECURSION} or G_LOG_LEVEL_ERROR or G_LOG_LEVEL_CRITICAL or G_LOG_LEVEL_WARNING or G_LOG_LEVEL_MESSAGE or G_LOG_LEVEL_INFO or G_LOG_LEVEL_DEBUG , @hnd_log, nil);
//  g_log_set_handler('GLib', G_LOG_FLAG_FATAL {or G_LOG_FLAG_RECURSION} or G_LOG_LEVEL_ERROR or G_LOG_LEVEL_CRITICAL or G_LOG_LEVEL_WARNING or G_LOG_LEVEL_MESSAGE or G_LOG_LEVEL_INFO or G_LOG_LEVEL_DEBUG , @hnd_log, nil);

  // create default colormap
  FColormap := TColormap.Create(gdk_colormap_get_system);
  //-- Clipboards
  Clipboard := TClipboard.Create(GDK_SELECTION_CLIPBOARD);
  Primary := TClipboard.Create(GDK_SELECTION_PRIMARY);
  //-- ToolTips
  FToolTips := gtk_tooltips_new();
  g_object_ref(FToolTips);
end;

var
  InSE: Boolean = False;

procedure TApplication.ShowException(E: Exception);
{IFDEF Win32
begin
  WriteLn('Exception (',E.ClassName,'):');
  WriteLn(E.Message);
  WriteLn('*************************');
end;
ELSE}
begin
  if E is EAbort then
    exit;

  if InSE then
  begin
    //WriteLn('Aha!');
    //It comes here, sometimes...
    exit;
  end;
  InSE := True;

  if E.ClassType = Exception then
    ShowMessage(mtError, [mbOK], 'Exception', 'Exception:', E.Message)
  else
    ShowMessage(mtError, [mbOK], 'Exception', 'Exception '+E.ClassName+':', E.Message);

  InSE := False;
end;
{ENDIF}

procedure TApplication.CreateForm(AClass: TFormClass; var Ref);
begin
  TForm(Ref) := AClass.Create(Self);
  if FMainForm = nil then
    FMainForm := TForm(Ref);
end;

function TApplication.ShowMessage(AType: TMessageType; AButtons: TMessageButtons; ATitle: String; AMsg: String; ASec: String): TMessageResponse; inline;
var
  Frm: Pointer;
  Dlg: Pointer;
  R: Integer;
begin
  if Assigned(MainForm) then
    Frm := MainForm.Handle
  else
    Frm := nil;
  //--
  Dlg := gtk_message_dialog_new(Frm, GTK_DIALOG_MODAL, TGtkMessageType(AType), GTK_BUTTONS_NONE, PChar(AMsg));
  //--
  if ATitle <> '' then
    gtk_window_set_title(Dlg, PChar(ATitle));
  //--
  if ASec <> '' then
    gtk_message_dialog_format_secondary_text(Dlg, PChar(ASec));
  //--
  if mbCancel in AButtons then
    gtk_dialog_add_button(Dlg, 'gtk-cancel', Ord(mrCancel));
  if mbOK in AButtons then
    gtk_dialog_add_button(Dlg, 'gtk-ok', Ord(mrOK));
  if mbNo in AButtons then
    gtk_dialog_add_button(Dlg, 'gtk-no', Ord(mrNo));
  if mbYes in AButtons then
    gtk_dialog_add_button(Dlg, 'gtk-yes', Ord(mrYes));
  if mbClose in AButtons then
    gtk_dialog_add_button(Dlg, 'gtk-close', Ord(mrClose));
  //--
  R := gtk_dialog_run(Dlg);
  gtk_widget_destroy(Dlg);
  //--
  Result := TMessageResponse(R);
end;

function TApplication.ShowMessage(AType: TMessageType; AButtons: TMessageButtons; ATitle: String; AMsg: String): TMessageResponse;
begin
  Result := ShowMessage(AType, AButtons, ATitle, AMsg, '');
end;


function TApplication.ShowMessage(AType: TMessageType; AButtons: TMessageButtons; AMsg: String): TMessageResponse;
begin
  Result := ShowMessage(AType, AButtons, '', AMsg, '');
end;

function TApplication.ShowMessage(AType: TMessageType; AMsg: String): TMessageResponse;
begin
  Result := ShowMessage(AType, [mbOK], '', AMsg, '');
end;

function TApplication.ShowMessage(AMsg: String): TMessageResponse;
begin
  Result := ShowMessage(mtInfo, [mbOK], '', AMsg, '');
end;

{$ENDIF}
