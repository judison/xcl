(*
   $Id: ttextview.inc,v 1.3 2006/02/27 17:43:30 judison Exp $

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

  TTextView = class(TControl)
  private
    FOwnTextBuffer: Boolean;
    procedure SetWrapMode(AValue:TWrapMode);
    function GetWrapMode: TWrapMode;
    procedure SetEditable(AValue: Boolean);
    function GetEditable: Boolean;
    {
    procedure SetPixelsAboveLines(AValue: Integer);
    function GetPixelsAboveLines: Integer;
    procedure SetPixelsBelowLines(AValue: Integer);
    function GetPixelsBelowLines: Integer;
    procedure SetPixelsInsideWrap(AValue: Integer);
    function GetPixelsInsideWrap: Integer;
    }
    procedure SetJustification(AValue: TJustification);
    function GetJustification: TJustification;
    {
    procedure SetLeftMargin(AValue: Integer);
    function GetLeftMargin: Integer;
    procedure SetRightMargin(AValue: Integer);
    function GetRightMargin: Integer;
    procedure SetIndent(AValue: Integer);
    function GetIndent: Integer;
    }


  protected
    FTextBuffer: TTextBuffer;
    //--
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure CreateTextBuffer; virtual;
    procedure DestroyTextBuffer; virtual;
    procedure SetTextBuffer(AValue: TTextBuffer); virtual;
    //function GetOverwrite: Boolean;
    //procedure SetOverwrite(AValue: Boolean);
  public
    //property Overwrite: Boolean read GetOvewrite write SetOverwrite;
  published
    property FontDesc; // from TControl
    property TextBuffer: TTextBuffer read FTextBuffer write SetTextBuffer;
    property Editable: Boolean read GetEditable write SetEditable default True;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TTextView }

procedure TTextView.CreateHandle;
begin
  CreateTextBuffer;
  Handle := gtk_text_view_new_with_buffer(FTextBuffer.Handle);
end;

procedure TTextView.DestroyHandle;
begin
  inherited;
  DestroyTextBuffer;
end;

procedure TTextView.CreateTextBuffer;
begin
  FTextBuffer := TTextBuffer.Create(nil);
  FOwnTextBuffer := True;
end;

procedure TTextView.DestroyTextBuffer;
begin
  if FOwnTextBuffer then
    FTextBuffer.Free;
  FTextBuffer := nil;
  FOwnTextBuffer := False;
end;

procedure TTextView.SetTextBuffer(AValue: TTextBuffer);
begin
  if AValue <> FTextBuffer then
  begin
    DestroyTextBuffer;
    if AValue <> nil then
      FTextBuffer := AValue
    else
      CreateTextBuffer;
    gtk_text_view_set_buffer(Handle, FTextBuffer.Handle);
  end;
end;

procedure TTextView.SetWrapMode(AValue:TWrapMode);
begin
  gtk_text_view_set_wrap_mode(Handle, Ord(AValue));
end;

function TTextView.GetWrapMode: TWrapMode;
begin
  Result := TWrapMode(gtk_text_view_get_wrap_mode(Handle));
end;

procedure TTextView.SetEditable(AValue: Boolean);
begin
  gtk_text_view_set_editable(Handle, AValue);
end;

function TTextView.GetEditable: Boolean;
begin
  Result := gtk_text_view_get_editable(Handle);
end;

procedure TTextView.SetJustification(AValue: TJustification);
begin
  gtk_text_view_set_justification(Handle, Ord(AValue));
end;

function TTextView.GetJustification: TJustification;
begin
  Result := TJustification(gtk_text_view_get_justification(Handle));
end;

{$ENDIF}
{
$Log: ttextview.inc,v $
Revision 1.3  2006/02/27 17:43:30  judison
Added TClipboard

Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}