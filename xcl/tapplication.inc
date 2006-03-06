(*
   $Id: tapplication.inc,v 1.8 2006/03/04 20:17:28 judison Exp $

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

{$IFDEF FPC}
  TApplication = class(TCustomApplication)
{$ELSE}
  TApplication = class(TComponent)
{$ENDIF}
  private
    FColormap: TColormap;
    FCurrentLocale: string;
    FMainForm: TForm;
    FActionList: TList;
    FToolTips: Pointer;
{$IFNDEF FPC}
    FTerminated: Boolean;
{$ENDIF}
  protected
    procedure SetTitle(const AValue: string); {$IFDEF FPC} override; {$ENDIF}
    procedure DoRun; {$IFDEF FPC} override; {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize; {$IFDEF FPC} override; {$ENDIF}
    procedure ShowException(E: Exception); {$IFDEF FPC} override; {$ENDIF}
    procedure Terminate; {$IFDEF FPC} override; {$ENDIF}

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
{$IFNDEF FPC}
    procedure Run;
{$ENDIF}

    property Colormap: TColormap read FColormap;
    property CurrentLocale: string read FCurrentLocale;
    property MainForm: TForm read FMainForm write FMainForm;
{$IFNDEF FPC}
    property Terminated: Boolean read FTerminated write FTerminated;
{$ENDIF}
  end;


{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

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
    FMainForm.Show;

  while (not Terminated) do
    ProcessMessages(True); // Block
end;

procedure TApplication.SetTitle(const AValue: string);
begin
  inherited;
  //???
end;

procedure TApplication.Initialize;
begin
  inherited;
  FCurrentLocale := PChar(gtk_set_locale);
{$IFDEF FPC}
  gtk_init(@argc, @argv);
{$ELSE}
  // TODO
  gtk_init(nil, nil);
{$ENDIF}

  // create default colormap
  FColormap := TColormap.Create(gdk_colormap_get_system);
  //-- Clipboards
  Clipboard := TClipboard.Create(GDK_SELECTION_CLIPBOARD);
  Primary := TClipboard.Create(GDK_SELECTION_PRIMARY);
  //-- ToolTips
  FToolTips := gtk_tooltips_new();
  g_object_ref(FToolTips);
end;

procedure TApplication.ShowException(E: Exception);
begin
{IFDEF FPC}
//  inherited;
{ELSE}
  WriteLn('***************');
  WriteLn('** EXCEPTION **');
  WriteLn('***************');
  WriteLn(E.Message);
  WriteLn('***************');
{ENDIF}
end;

procedure TApplication.Terminate;
begin
{$IFDEF FPC}
  inherited;
{$ELSE}
  FTerminated := True
{$ENDIF}
end;

procedure TApplication.CreateForm(AClass: TFormClass; var Ref);
begin
  TForm(Ref) := AClass.Create(Self);
  if FMainForm = nil then
    FMainForm := TForm(Ref);
end;

{$IFNDEF FPC}
procedure TApplication.Run;
begin
  DoRun;
end;
{$ENDIF}


{$ENDIF}
{
$Log: tapplication.inc,v $
Revision 1.8  2006/03/04 20:17:28  judison
Added ToolTip

Revision 1.7  2006/02/27 17:43:30  judison
Added TClipboard

Revision 1.6  2005/12/19 22:49:23  judison
various changes
added TSpinButton

Revision 1.5  2005/12/02 22:31:34  judison
Long-term changes (again) I need to use cvs more!

Revision 1.4  2005/11/22 01:05:28  judison
* Action Support

Revision 1.3  2005/03/27 09:53:51  judison
* New Event System

Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}