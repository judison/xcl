(*
   $Id: tentry.inc,v 1.6 2005/12/30 17:21:38 judison Exp $

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
{$IFDEF INC_READ_INTERFACE}

  TCustomEntry = class(TControl)
  private
    FOnChanged: TNotifyEvent;
    {$DEFINE INTF_PRIVATE}{$I ieditablecontrol.inc}{$UNDEF INTF_PRIVATE}
    function GetHasFrame: Boolean;
    procedure SetHasFrame(AValue: Boolean);
    function GetVisibility: Boolean;
    procedure SetVisibility(AValue: Boolean);
    function GetMaxLength: Integer;
    procedure SetMaxLength(AValue: Integer);
    function GetWidthChars: Integer;
    procedure SetWidthChars(AValue: Integer);
  protected
    {$DEFINE INTF_PROTECTED}{$I ieditablecontrol.inc}{$UNDEF INTF_PROTECTED}
    procedure CreateHandle; override;
    procedure ConnectSignals; override;
    //== Do's
    procedure DoChanged(var Msg: TGtkSig); message 'GTK:changed'; virtual;
    //-- Text
    FText: String; // duplicated :(
    function GetText: String; inline;
    procedure SetText(AValue: String); inline;

  public
    {$DEFINE INTF_PUBLIC}{$I ieditablecontrol.inc}{$UNDEF INTF_PUBLIC}
  published
    {$DEFINE INTF_PUBLISHED}{$I ieditablecontrol.inc}{$UNDEF INTF_PUBLISHED}
    property HasFrame: Boolean read GetHasFrame write SetHasFrame default True;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property Text: String read FText write SetText;
    property Visibility: Boolean read GetVisibility write SetVisibility default True;
    property WidthChars: Integer read GetWidthChars write SetWidthChars default -1;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TEntry = class(TCustomEntry)
  private
  protected
  public
  published
  end;

{$ENDIF}
//** IMPLEMENTATION
//****************************************************************************
{$IFDEF INC_READ_IMPLEMENTATION}

{ TCustomEntry }

{$DEFINE INTF_IMPLEMENTATION}
  {$define the_class := TCustomEntry }
    {$I ieditablecontrol.inc}
  {$undef the_class}
{$UNDEF INTF_IMPLEMENTATION}

procedure TCustomEntry.CreateHandle;
begin
  Handle := gtk_entry_new;
  FText := GetText;
end;

procedure TCustomEntry.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('changed');
end;

procedure TCustomEntry.DoChanged(var Msg: TGtkSig);
begin
  FText := GetText;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TCustomEntry.GetText: string; inline;
begin
  Result := gtk_entry_get_text(Handle);
end;

procedure TCustomEntry.SetText(AValue: string); inline;
begin
//  if AValue <> FText then
//  begin
    FText := AValue;
    gtk_entry_set_text(Handle, PChar(FText));
//  end;
end;

function TCustomEntry.GetVisibility: Boolean;
begin
  Result := gtk_entry_get_visibility(Handle);
end;

procedure TCustomEntry.SetVisibility(AValue: Boolean);
begin
  gtk_entry_set_visibility(Handle, AValue);
end;

function TCustomEntry.GetHasFrame: Boolean;
begin
  Result := gtk_entry_get_has_frame(Handle);
end;

procedure TCustomEntry.SetHasFrame(AValue: Boolean);
begin
  gtk_entry_set_has_frame(Handle, AValue);
end;

function TCustomEntry.GetMaxLength: Integer;
begin
  Result := gtk_entry_get_max_length(Handle);
end;

procedure TCustomEntry.SetMaxLength(AValue: Integer);
begin
  gtk_entry_set_max_length(Handle, AValue);
end;

function TCustomEntry.GetWidthChars: Integer;
begin
  Result := gtk_entry_get_width_chars(Handle);
end;

procedure TCustomEntry.SetWidthChars(AValue: Integer);
begin
  gtk_entry_set_width_chars(Handle, AValue);
end;

{$ENDIF}
{
  $Log: tentry.inc,v $
  Revision 1.6  2005/12/30 17:21:38  judison
  * Moved DB Comps to xcldb.inc
  * Added TCanvas

  Revision 1.5  2005/11/18 20:06:58  judison
  long-term changes...

  Revision 1.4  2005/03/28 06:34:56  judison
  *** empty log message ***

  Revision 1.3  2005/03/28 05:41:23  judison
  - TEditableControl
  + GtkEditableControl Interface Implementation

  Revision 1.2  2005/03/26 05:21:47  judison
  + CVS Log Tag

}