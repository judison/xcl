(*
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

  TDialog = class(TComponent)
  private
  protected
    Handle: Pointer;
    function GetTitle: String;
    procedure SetTitle(AValue: String);
    //
    procedure CreateHandle; virtual;
    procedure ConnectSignals; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //--
    function Run: Integer;
    function Execute: Integer;
    procedure Hide;
    procedure Response(AValue: Integer);
    procedure AddButton(ABtnText: String; AResponseId: Integer);
    procedure SetDefaultResponse(AValue: Integer);
    procedure SetResponseSensitive(AResponseID: Integer; ASensitive: Boolean);
  published
    property Title: String read GetTitle write SetTitle;
  end;


{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TDialog }

constructor TDialog.Create(AOwner: TComponent);
begin
  inherited;
  CreateHandle;
  //-- Set Data TObject
  if Handle <> nil then
    g_object_set_qdata(Handle, QTObject, Self);
  //-- Conecta signals
  ConnectSignals;
end;

destructor TDialog.Destroy;
begin
  inherited;
end;

procedure TDialog.CreateHandle;
begin
  Handle := gtk_dialog_new;
end;

procedure TDialog.ConnectSignals;
begin
end;

function TDialog.Run: Integer;
begin
  Result := gtk_dialog_run(Handle);
end;

function TDialog.Execute: Integer;
begin
  Result := Run;
  Hide;
end;

procedure TDialog.Hide;
begin
  gtk_widget_hide(Handle);
end;

procedure TDialog.Response(AValue: Integer);
begin
  gtk_dialog_response(Handle, AValue);
end;

procedure TDialog.AddButton(ABtnText: String; AResponseId: Integer);
begin
  gtk_dialog_add_button(Handle, PChar(ABtnText), AResponseId);
end;

procedure TDialog.SetDefaultResponse(AValue: Integer);
begin
  gtk_dialog_set_default_response(Handle, AValue);
end;

procedure TDialog.SetResponseSensitive(AResponseID: Integer; ASensitive: Boolean);
begin
  gtk_dialog_set_response_sensitive(Handle, AResponseID, ASensitive);
end;

procedure TDialog.SetTitle(AValue: string);
begin
  gtk_window_set_title(Handle, PChar(AValue));
end;

function TDialog.GetTitle: string;
begin
  Result := gtk_window_get_title(Handle);
end;

{$ENDIF}
