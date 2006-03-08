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

  TForm = class(TBinControl)
  private
    FInModal: Boolean;
    FDesigner: IDesigner;
    function GetHeight: Integer;
    function GetTitle: string;
    function GetWidth: Integer;
    function GetResizable: Boolean;
    function GetDecorated: Boolean;
    procedure SetHeight(Value: Integer);
    procedure SetTitle(Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetResizable(Value: Boolean);
    procedure SetDecorated(Value: Boolean);
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    //--
    procedure ConnectSignals; override;
    procedure DoCloseQuery(CanClose: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
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
    procedure Hide; override;
    procedure ShowModal;
    procedure Close;
    procedure DoDeleteEvent(var Msg: TGtkSigEvent); message 'GTK:delete-event';
    //
    procedure ReadBinFrm(AStream: TStream);
    procedure ReadTxtFrm(AStream: TStream);
    procedure WriteBinFrm(AStream: TStream);
    procedure WriteTxtFrm(AStream: TStream);
    //
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    //--
    property Designer: IDesigner read FDesigner write FDesigner;
  published
    property Title: string read GetTitle write SetTitle;
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

procedure TForm.CreateHandle;
begin
  Handle := gtk_window_new(GTK_WINDOW_TOPLEVEL);
end;

procedure TForm.DestroyHandle;
begin
  inherited;
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

procedure TForm.SetHeight(Value: Integer);
var
  W, H: Integer;
begin
  GetSize(W, H);
  if H <> Value then
    Resize(W, Value);
end;

procedure TForm.SetTitle(Value: string);
begin
  gtk_window_set_title(Handle, PChar(Value));
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

procedure TForm.Hide;
begin
  inherited; // Hide Ctrls ???
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

//--------------------

procedure TForm.ReadBinFrm(AStream: TStream);
begin
  AStream.Position := 0;
  AStream.ReadComponent(Self);
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

procedure TForm.DoCloseQuery(CanClose: Boolean);
begin
  //---
end;

{$ENDIF}
