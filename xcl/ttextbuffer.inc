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

  // its a opaque datatype, for GtkTextIter (56 bytes on IA32)
  TTextIter = record
    dummy1: Pointer;
    dummy2: Pointer;
    dummy3: Integer;
    dummy4: Integer;
    dummy5: Integer;
    dummy6: Integer;
    dummy7: Integer;
    dummy8: Integer;
    dummy9: Pointer;
    dummy10: Pointer;
    dummy11: Integer;
    dummy12: Integer;
    dummy13: Integer;
    dummy14: Pointer;
  end;

  TTextBuffer = class(TComponent)
  private
    FOnChanged: TNotifyEvent;
    function GetLineCount: Integer;
    function GetCharCount: Integer;
    function GetSelText: String;
    function GetModified: Boolean;
    procedure SetModified(AValue: Boolean);
  protected
    Handle: Pointer;
    procedure CreateHandle; virtual;
    //
    function GetText: String; virtual;
    procedure SetText(AValue: String); virtual;
    //-- DOs
    procedure DoChanged(var Msg: TGtkSig); message 'GTK:changed'; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //--
    procedure InsertAtCursor(AStr: String);
    procedure BeginUserAction;
    procedure EndUserAction;
    //--
    procedure DeleteSelection(AInteractive: Boolean; ADefaultEditable: Boolean);
    procedure PasteClipboard(AClipboard: TClipboard; ADefaultEditable: Boolean);
    procedure CopyClipboard(AClipboard: TClipboard);
    procedure CutClipboard(AClipboard: TClipboard; ADefaultEditable: Boolean);
    //--
    function GetIter(ALine, AChar: Integer): TTextIter;
    function GetIterAtOffset(AChar: Integer): TTextIter;
    function GetIterAtLine(ALine: Integer): TTextIter;
    procedure GetBounds(var AStart, AEnd: TTextIter);
    procedure GetSelectionBounds(var AStart, AEnd: TTextIter);
    function GetText(const AStart, AEnd: TTextIter): String;
    // TAGS
    procedure ApplyTagByName(ATag: String; AStart, AEnd: TTextIter);
    procedure RemoveAllTags(AStart, AEnd: TTextIter);
    procedure RemoveAllTags;
    //--NON GTK
    procedure LoadFromFile(AFileName: String);
    procedure SaveToFile(AFileName: String);
    //--
    property LineCount: Integer read GetLineCount;
    property CharCount: Integer read GetCharCount;
    property SelText: String read GetSelText;
    property Modified: Boolean read GetModified write SetModified;
    //--
  published
    property Text: String read GetText write SetText;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TTextBuffer }

constructor TTextBuffer.Create(AOwner: TComponent);
begin
  inherited;
  CreateHandle;
  g_object_ref(Handle);
  g_object_set_qdata(Handle, QTObject, Self);
  ConnectGtkSignal(Handle, 'changed');
end;

destructor TTextBuffer.Destroy;
begin
  g_object_unref(Handle);
  inherited;
end;

procedure TTextBuffer.CreateHandle;
begin
  Handle := gtk_text_buffer_new(nil);
end;

procedure TTextBuffer.DoChanged(var Msg: TGtkSig);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TTextBuffer.LoadFromFile(AFileName: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFileName);
    Text := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TTextBuffer.SaveToFile(AFileName: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Text;
    SL.SaveToFile(AFileName);
  finally
    SL.Free;
  end;
end;

function TTextBuffer.GetText(const AStart, AEnd: TTextIter): String;
begin
  Result := gtk_text_buffer_get_text(Handle, @AStart, @AEnd, False);
end;

function TTextBuffer.GetText: String;
var
  IStart, IEnd: TTextIter;
begin
  //GetBounds(IStart, IEnd);
  //Result := GetText(IStart, IEnd);
  gtk_text_buffer_get_bounds(Handle, @IStart, @IEnd);
  Result := gtk_text_buffer_get_text(Handle, @IStart, @IEnd, False);
end;

procedure TTextBuffer.SetText(AValue: String);
begin
  gtk_text_buffer_set_text(Handle, PChar(AValue), Length(AValue));
end;

function TTextBuffer.GetSelText: String;
var
  IStart, IEnd: TTextIter;
begin
  //GetSelectionBounds(IStart, IEnd);
  //Result := GetText(IStart, IEnd);
  gtk_text_buffer_get_selection_bounds(Handle, @IStart, @IEnd);
  Result := gtk_text_buffer_get_text(Handle, @IStart, @IEnd, False);
end;

function TTextBuffer.GetModified: Boolean;
begin
  Result := gtk_text_buffer_get_modified(Handle);
end;

procedure TTextBuffer.SetModified(AValue: Boolean);
begin
  gtk_text_buffer_set_modified(Handle, AValue);
end;

function TTextBuffer.GetLineCount: Integer;
begin
  Result := gtk_text_buffer_get_line_count(Handle);
end;

function TTextBuffer.GetCharCount: Integer;
begin
  Result := gtk_text_buffer_get_line_count(Handle);
end;

procedure TTextBuffer.InsertAtCursor(AStr: String);
begin
  gtk_text_buffer_insert_at_cursor(Handle, PChar(AStr), Length(AStr));
end;

procedure TTextBuffer.BeginUserAction;
begin
  gtk_text_buffer_begin_user_action(Handle);
end;

procedure TTextBuffer.EndUserAction;
begin
  gtk_text_buffer_end_user_action(Handle);
end;

procedure TTextBuffer.DeleteSelection(AInteractive: Boolean; ADefaultEditable: Boolean);
begin
  gtk_text_buffer_delete_selection(Handle, AInteractive, ADefaultEditable);
end;

procedure TTextBuffer.PasteClipboard(AClipboard: TClipboard; ADefaultEditable: Boolean);
begin
  gtk_text_buffer_paste_clipboard(Handle, AClipboard.Handle, nil, ADefaultEditable);
end;

procedure TTextBuffer.CopyClipboard(AClipboard: TClipboard);
begin
   gtk_text_buffer_copy_clipboard(Handle, AClipboard.Handle);
end;

procedure TTextBuffer.CutClipboard(AClipboard: TClipboard; ADefaultEditable: Boolean);
begin
  gtk_text_buffer_cut_clipboard(Handle, AClipboard.Handle, ADefaultEditable);
end;

procedure TTextBuffer.ApplyTagByName(ATag: String; AStart, AEnd: TTextIter);
begin
  gtk_text_buffer_apply_tag_by_name(Handle, PChar(ATag), @AStart, @AEnd);
end;

procedure TTextBuffer.RemoveAllTags(AStart, AEnd: TTextIter);
begin
  gtk_text_buffer_remove_all_tags(Handle, @AStart, @AEnd);
end;

procedure TTextBuffer.RemoveAllTags;
var
  lS, lE: TTextIter;
begin
  GetBounds(lS, lE);
  RemoveAllTags(lS, lE);
end;

function TTextBuffer.GetIter(ALine, AChar: Integer): TTextIter;
begin
  gtk_text_buffer_get_iter_at_line_offset(Handle, @Result, ALine, AChar);
end;

function TTextBuffer.GetIterAtOffset(AChar: Integer): TTextIter;
begin
  gtk_text_buffer_get_iter_at_offset(Handle, @Result, AChar);
end;

function TTextBuffer.GetIterAtLine(ALine: Integer): TTextIter;
begin
  gtk_text_buffer_get_iter_at_line(Handle, @Result, ALine);
end;

procedure TTextBuffer.GetBounds(var AStart, AEnd: TTextIter);
begin
  gtk_text_buffer_get_bounds(Handle, @AStart, @AEnd);
end;

procedure TTextBuffer.GetSelectionBounds(var AStart, AEnd: TTextIter);
begin
  gtk_text_buffer_get_selection_bounds(Handle, @AStart, @AEnd);
end;

{$ENDIF}
