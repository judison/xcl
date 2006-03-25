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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize; override;
    procedure ShowException(E: Exception); override;
    procedure CreateForm(AClass: TFormClass; var Ref);
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
var
  I: Integer;
begin
  try
    while (gtk_events_pending > 0) do
      gtk_main_iteration_do(False); // Don't Block
   
    I := 0;
    while I < FActionList.Count do
    begin
      TCustomAction(FActionList[I]).Update;
      Inc(I);
    end;
  
    if ABlock then
      gtk_main_iteration_do(True); // Block
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

procedure TApplication.Initialize;
begin
  inherited;
  FCurrentLocale := PChar(gtk_set_locale);
  gtk_init(@argc, @argv);

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
var
  Dlg: Pointer;
  Msg: String;
  Frm: Pointer;
begin
  if InSE then
  begin
    //WriteLn('Aha!');
    //It comes here, sometimes...
    exit;
  end;
  InSE := True;

  Dlg := nil;
  if Assigned(FMainForm) then
    Frm := FMainForm.Handle
  else
    Frm := nil;

  ProcessMessages(False);

  Msg := Format('Exception (%s):'#10'%s', [E.ClassName, E.Message]);
  Dlg := gtk_message_dialog_new(Frm, GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, PChar(Msg));
  if Assigned(Dlg) then
  begin
    gtk_dialog_run(Dlg);
    gtk_widget_destroy(Dlg);
  end
  else
  begin
    WriteLn('Exception (',E.ClassName,'):');
    WriteLn(E.Message);
    WriteLn('*************************');
  end;
  InSE := False;
end;

procedure TApplication.CreateForm(AClass: TFormClass; var Ref);
begin
  TForm(Ref) := AClass.Create(Self);
  if FMainForm = nil then
    FMainForm := TForm(Ref);
end;


{$ENDIF}
