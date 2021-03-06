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

  TForm = class(TBinControl, IPixbufClient)
  private
    FIcon: TPixbuf;
    FInModal: Boolean;
    FDesigner: IDesigner;
    FAccelGroup: TAccelGroup;
    function GetTitle: string;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetResizable: Boolean;
    function GetDecorated: Boolean;
    procedure SetTitle(Value: string);
    procedure SetIcon(Value: TPixbuf);
    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetResizable(Value: Boolean);
    procedure SetDecorated(Value: Boolean);
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    //--
    procedure PixbufChanged(APixbuf: TPixbuf);
    procedure ChangeIcon;
    //--
    procedure ConnectSignals; override;
    procedure DoCloseQuery(var CanClose: Boolean); virtual;
    procedure DoIdle; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetSize(var AWidth, AHeight: Integer);
    procedure Resize(AWidth, AHeight: Integer);
    //
    procedure Iconify;
    procedure Deiconify;
    procedure Stick;
    procedure UnStick;
    procedure Maximize;
    procedure UnMaximize;
    //
    procedure Show; override;
    procedure Show(ATransientFor: TForm);
    procedure Hide; override;
    procedure ShowModal;
    procedure ShowModal(ATransientFor: TForm);
    procedure Close;
    procedure DoDeleteEvent(var Msg: TGtkSigEvent); message 'GTK:delete-event';
    //
    procedure AddAccelGroup(AAccelGroup: TAccelGroup);
    procedure RemoveAccelGroup(AAccelGroup: TAccelGroup);
    //
    procedure ReadBinFrm(AStream: TStream);
    procedure ReadTxtFrm(AStream: TStream);
    procedure WriteBinFrm(AStream: TStream);
    procedure WriteTxtFrm(AStream: TStream);
    //
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    //--
    property Designer: IDesigner read FDesigner write FDesigner;
    property AccelGroup: TAccelGroup read FAccelGroup;
  published
    property Title: string read GetTitle write SetTitle;
    property Icon: TPixbuf read FIcon write SetIcon;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property ResizeMode default rmQueue; // override default value don't work in fpc
    property Resizable: Boolean read GetResizable write SetResizable default True;
    property Decorated: Boolean read GetDecorated write SetDecorated default True;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TForm }

constructor TForm.Create(AOwner: TComponent);
var
  Stream: TStream;
  FN: string;
  Sig: array[0..3] of char;
begin
  // TODO: funcao aki dentro, p/ varrer todas as classes entre TForm e a classe instanciada
  // e na ordem (TForm, TMyCustomForm, TMyCustomFormAA, TMyForm) ir lendo os .frm de cada classe
  // tornando assim, possivel a hierarquia de forms (.frm)
  inherited;
  //---
  FAccelGroup := TAccelGroup.Create(nil);
  AddAccelGroup(FAccelGroup);
  //---
  try
    Stream := TResourceStream.Create('FORMDATA', ClassName);
    try
      ReadBinFrm(Stream);
    finally
      Stream.Free;
    end;
  except
    FN := LowerCase(ClassName) + '.frm';
    if FileExists(FN) then
    begin
      Stream := TFileStream.Create(FN, fmOpenRead);
      try
        Stream.ReadBuffer(Sig, SizeOf(Sig));
        Stream.Position := 0;
        if Sig = 'TPF0' then
          ReadBinFrm(Stream)
        else
          ReadTxtFrm(Stream);
      finally
        Stream.Free;
      end;
    end;
  end;
end;

destructor TForm.Destroy;
begin
  FAccelGroup.Free;
  inherited;
end;

procedure TForm.CreateHandle;
begin
  Handle := gtk_window_new(GTK_WINDOW_TOPLEVEL);
end;

procedure TForm.DestroyHandle;
begin
  inherited;
end;

procedure TForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FIcon) and (Operation = opRemove) then
  begin
    FIcon := nil;
    ChangeIcon;
  end;
  inherited;
end;

procedure TForm.PixbufChanged(APixbuf: TPixbuf);
begin
  if APixbuf = FIcon then
    ChangeIcon;
end;

procedure TForm.ChangeIcon;
begin
  if Assigned(FIcon) then
    gtk_window_set_icon(Handle, FIcon.GetGdkPixbuf)
  else
    gtk_window_set_icon(Handle, nil);
end;

function TForm.GetHeight: Integer;
var
  Dummy: Integer;
begin
  {$WARNINGS OFF}
  GetSize(Dummy, Result);
  {$WARNINGS ON}
end;

procedure TForm.GetSize(var AWidth, AHeight: Integer);
begin
  gtk_window_get_size(Handle, @AWidth, @AHeight);
end;

function TForm.GetTitle: string;
begin
  Result := gtk_window_get_title(Handle);
end;

function TForm.GetWidth: Integer;
var
  Dummy: Integer;
begin
  {$WARNINGS OFF}
  GetSize(Result, Dummy);
  {$WARNINGS ON}
end;

function TForm.GetResizable: Boolean;
begin
  Result := gtk_window_get_resizable(Handle);
end;

function TForm.GetDecorated: Boolean;
begin
  Result := gtk_window_get_decorated(Handle);
end;

procedure TForm.Resize(AWidth, AHeight: Integer);
begin
  gtk_window_resize(Handle, AWidth, AHeight);
end;

procedure TForm.SetTitle(Value: string);
begin
  gtk_window_set_title(Handle, PChar(Value));
end;

procedure TForm.SetIcon(Value: TPixbuf);
begin
  if Value <> FIcon then
  begin
    if Assigned(FIcon) then
      FIcon.RemoveFreeNotification(Self);

    FIcon := Value;
    ChangeIcon;

    if Assigned(FIcon) then
      FIcon.FreeNotification(Self);
  end;
end;

procedure TForm.SetHeight(Value: Integer);
var
  W, H: Integer;
begin
  GetSize(W, H);
  if H <> Value then
    Resize(W, Value);
end;


procedure TForm.SetWidth(Value: Integer);
var
  W, H: Integer;
begin
  GetSize(W, H);
  if W <> Value then
    Resize(Value, H);
end;

procedure TForm.SetResizable(Value: Boolean);
var
  V: Longbool;
begin
  V := Value;
  gtk_window_set_resizable(Handle, V);
end;

procedure TForm.SetDecorated(Value: Boolean);
var
  V: Longbool;
begin
  V := Value;
  gtk_window_set_decorated(Handle, V);
end;

// Methods

procedure TForm.Iconify;
begin
  gtk_window_iconify(Handle);
end;

procedure TForm.Deiconify;
begin
  gtk_window_deiconify(Handle);
end;

procedure TForm.Stick;
begin
  gtk_window_stick(Handle);
end;

procedure TForm.Unstick;
begin
  gtk_window_unstick(Handle);
end;

procedure TForm.Maximize;
begin
  gtk_window_maximize(Handle);
end;

procedure TForm.UnMaximize;
begin
  gtk_window_unmaximize(Handle);
end;

procedure TForm.Show;
begin
  inherited; // Show Ctrls ???
end;

procedure TForm.Show(ATransientFor: TForm);
begin
  gtk_window_set_transient_for(Handle, ATransientFor.Handle);
  Show;
end;

procedure TForm.Hide;
begin
  inherited; // Hide Ctrls ???
end;

procedure TForm.ShowModal(ATransientFor: TForm);
begin
  gtk_window_set_transient_for(Handle, ATransientFor.Handle);
  ShowModal;
end;

procedure TForm.ShowModal;
begin
  FInModal := True;
  gtk_window_set_modal(Handle, True);
  Show;
  while (not Application.Terminated) and FInModal do
    Application.ProcessMessages(True);
end;

procedure TForm.Close;
var
  CanClose: Boolean;
begin

  CanClose := True;
  DoCloseQuery(CanClose);
  if CanClose then
  begin
    FInModal := False;
    gtk_window_set_modal(Handle, False);
    gtk_window_set_transient_for(Handle, nil);

    Hide;

    if Application.MainForm = Self then
      Application.Terminate;
  end;
end;

procedure TForm.DoDeleteEvent(var Msg: TGtkSigEvent);
begin
  Close;
  Msg.Stop := True;
end;

procedure TForm.DoIdle;
begin
  //--
end;

procedure TForm.AddAccelGroup(AAccelGroup: TAccelGroup);
begin
  gtk_window_add_accel_group(Handle, AAccelGroup.Handle);
end;

procedure TForm.RemoveAccelGroup(AAccelGroup: TAccelGroup);
begin
  gtk_window_remove_accel_group(Handle, AAccelGroup.Handle);
end;

//--------------------

procedure TForm.ReadBinFrm(AStream: TStream);
begin
  try
    AStream.Position := 0;
    AStream.ReadComponent(Self);
  except
    on E: Exception do
      Application.ShowException(E);
  end;
end;

procedure TForm.ReadTxtFrm(AStream: TStream);
var
  BStream: TStream;
begin
  BStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(AStream, BStream);
    ReadBinFrm(BStream);
  finally
    BStream.Free;
  end;
end;

procedure TForm.WriteBinFrm(AStream: TStream);
begin
  AStream.WriteComponent(Self);
end;

procedure TForm.WriteTxtFrm(AStream: TStream);
var
  BStream: TStream;
begin
  BStream := TMemoryStream.Create;
  try
    WriteBinFrm(BStream);
    BStream.Position := 0;
    ObjectBinaryToText(BStream, AStream);
  finally
    BStream.Free;
  end;
end;

procedure TForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
      if not Components[I].HasParent
        then Proc(Components[I]);
  inherited GetChildren(Proc, Root);
end;

procedure TForm.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('delete-event');
end;

procedure TForm.DoCloseQuery(var CanClose: Boolean);
begin
  //---
end;

{$ENDIF}
