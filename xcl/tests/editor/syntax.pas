(*
   XCL - XDE's Component Library
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

(*

  TODO: Number, HexNumber

*)

unit syntax;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, xcl;

type
  TPasTextBuffer = class(TTextBuffer)
  private
    FTagsDone: Boolean;
    FTimer: TTimer;
    procedure Timer(Sender: TObject);
    procedure ParseChars(AText: PChar; var end_ptr: PChar; var state: integer; var ATag:  String);
  protected
    procedure CreateTags;
    procedure Highlight; 
    procedure DoChanged(var Msg: TGtkSig) message 'GTK:changed'; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses glib2, pango, gtk2, strings;

{ TPasTextBuffer }

constructor TPasTextBuffer.Create(AOwner: TComponent);
begin
  FTagsDone := False;
  inherited;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := @Timer;
  FTimer.Interval := 0;
end;

procedure TPasTextBuffer.CreateTags;
begin
  if not FTagsDone then
  begin
    gtk_text_buffer_create_tag(Handle, 'comment', 'foreground', ['ForestGreen', NULL]);
    gtk_text_buffer_create_tag(Handle, 'type', 'foreground', ['navy', NULL]);
    gtk_text_buffer_create_tag(Handle, 'string', 'foreground', ['blue', NULL]);
    gtk_text_buffer_create_tag(Handle, 'keyword', 'foreground', ['navy', NULL]);
    gtk_text_buffer_create_tag(Handle, 'preprocessor', 'foreground', ['red', NULL]);
    gtk_text_buffer_create_tag(Handle, 'symbol', 'foreground', ['magenta', NULL]);
    FTagsDone := True;
  end;
end;

procedure TPasTextBuffer.DoChanged(var Msg: TGtkSig);
begin
  inherited;
  FTimer.Active := True;
end;

procedure TPasTextBuffer.Timer(Sender: TObject);
var
  lt: PGTimer;
  d: integer;
begin
  FTimer.Active := False;
  lt := g_timer_new;
  g_timer_start(lt);
  Highlight;
  g_timer_stop(lt);
  d := Trunc(g_timer_elapsed(lt, nil)*1000);
  FTimer.Interval := d;
  g_timer_destroy(lt);
end;

const
  tokens: array [1..4] of PChar = ('(*', '''', '{', '//');

  keywords: array[0..106] of PChar =
  (
    'program', 'const', 'type', 'var',
    'begin', 'end', 'array', 'set', 'record', 'string', 
    'if', 'then', 'else', 'while', 'for', 'to', 'downto', 'do', 'with',
    'repeat', 'until', 'case', 'of', 'goto', 'exit', 'label',
    'procedure', 'function', 'nil', 'file', 'and', 'or', 'not', 'xor',
    'div', 'mod', 'unit', 'uses', 'implementation', 'interface', 
    'asm', 'inline', 'object', 'constructor', 'destructor', 'inherited', 
    'except', 'finally', 'initialization', 'out', 'property',
    'resourcestring', 'try', 'exports', 'library', 'packed',
    'raise', 'as', 'class', 'dispinterface', 'in', 'shl', 'shr',
    'threadvar', 'finalization', 'is', 'at', 'on',
    
    'private', 'protected', 'public', 'published', 'automated',
    'absolute', 'abstract', 'cdecl', 'contains', 'default', 'dispid',
    'dynamic', 'export', 'external', 'far', 'assembler', 'virtual', 
    'near', 'forward', 'implements', 'index', 'message', 'name',
    'nodefault', 'overload', 'override', 'package', 'pascal', 'read',
    'readonly', 'register', 'reintroduce', 'requires', 'resident',
    'safecall', 'stdcall', 'stored', 'write', 'writeonly'
  );

  types: array[0..25] of PChar =
  (
    'integer', 'cardinal', 'shortint', 'smallint', 'longint', 'int64',
    'byte', 'word', 'longword', 'char', 'boolean', 'bytebool',
    'wordbool', 'longbool', 'true', 'false', 'real48', 'single',
    'double', 'extended', 'comp', 'currency', 'real',
    'shortstring', 'ansistring', 'widestring'
  );

  symbols: array[0..25] of PChar =
  (
    // compostos
    '>=', '<=', '<>',
    ':=', '+=', '-=', '*=', '/=',
    // simples
    '.', ',', ':', ';',
    '=', '>', '<',
    '-', '+', '*', '/',
    '[', ']', '(', ')',
    '^', '@', '#'
  );

const
  Separators = [#8, #32, #0, #10, #13, '.', ',', ':', ';', '=', '>', '<', '-', '+', '*', '/', '[', ']', '(', ')', '^', '@', '#'];

const
  STATE_NORMAL     = 0;
  STATE_COMMENT_A  = 1;
  STATE_COMMENT_B  = 2;

procedure TPasTextBuffer.ParseChars(AText: PChar; var end_ptr: PChar; var state: integer; var ATag:  String);
var
  i          : gint;
  next_token : PChar;
begin
  // Case Insensitive
  StrLower(AText);

  if State = STATE_COMMENT_A then
  begin
    next_token := StrPos(AText, '*)');
    if next_token <> NULL then
    begin
      state := STATE_NORMAL;
      end_ptr := next_token + 2;
      ATag := 'comment';
    end;
    exit;
  end;

  if State = STATE_COMMENT_B then
  begin
    next_token := StrPos(AText, '}');
    if next_token <> NULL then
    begin
      state := STATE_NORMAL;
      end_ptr := next_token + 1;
      ATag := 'comment';
    end;
    exit;
  end;

  ATag := '';
  end_ptr := NULL;

  if AText^ = #0 then
    exit;

  // check for preprocessor defines
  if ((StrLComp(AText, '{$', 2)) = 0) then
  begin
    next_token := StrPos (AText, '}');
    if next_token <> NULL then
      end_ptr := next_token + 1;
    ATag := 'preprocessor';
    exit;
  end;

  // check for comment

  if ((StrLComp (AText, '(*', 2)) = 0) then
  begin
    next_token := StrPos (AText, '*)');

    if next_token <> NULL then
      end_ptr := next_token + 2
    else
      state := STATE_COMMENT_A;

    ATag := 'comment';
    exit;
  end;

  if ((StrLComp(AText, '{', 1)) = 0) then
  begin
    next_token := StrPos (AText, '}');

    if next_token <> NULL then
      end_ptr := next_token + 1
    else
      state := STATE_COMMENT_B;

    ATag := 'comment';
    exit;
  end;

  if (StrLComp (AText, '//', 2)) = 0 then
  begin
    end_ptr := NULL;
    ATag := 'comment';
    exit;
  end;

  // check for types
  for i := 0 to high(types) do
    if ((StrLComp(AText, types[i], strlen(types[i]))) = 0 ) and ((AText+strlen(types[i]))^ in Separators) then
    begin
      end_ptr := AText + strlen(types[i]);
      ATag := 'type';
      exit;
    end;

  // check for keywords
  for i := 0 to high (keywords) do
    if ((StrLComp(AText, keywords[i], strlen (keywords[i]))) = 0) and ((AText+strlen(keywords[i]))^ in Separators)  then
    begin
      end_ptr := AText + strlen(keywords[i]);
      ATag := 'keyword';
      exit;
    end;

  // check for symbols
  for i := 0 to high(symbols) do
    if ((StrLComp(AText, symbols[i], strlen (symbols[i]))) = 0) then
    begin
      end_ptr := AText + strlen(symbols[i]);
      ATag := 'symbol';
      exit;
    end;

  // check for string
  if AText^= '''' then
  begin
    end_ptr := AText + 1;
    ATag := 'string';

    while end_ptr^ <> #0 do
    begin
      if (end_ptr^ = '''') then
      begin
        inc(end_ptr);
        if end_ptr^ <> '''' then
          exit;
      end;
      inc(end_ptr);
    end;

    exit;
  end;


  // not at the start of a tag.  Find the next one.
  for i := 1 to high(tokens) do
  begin
    next_token := StrPos(AText, tokens[i]);
    if next_token <> NULL then
      if (end_ptr = NULL) or (end_ptr > next_token) then
        end_ptr := next_token;
  end;

  for i := 0 to high(types) do
  begin
    next_token := StrPos(AText, types[i]);
    if next_token <> NULL then
      if ( (next_token+strlen(types[i]))^ in Separators) and  g_ascii_isspace ((next_token-1)^) then
        if (end_ptr = NULL) or (end_ptr > next_token) then
          end_ptr := next_token;
  end;


  for i := 0 to high(keywords) do
  begin
    next_token := StrPos(AText, keywords[i]);
    if next_token <> NULL then
      if ( (next_token+strlen(keywords[i]))^  in Separators) and g_ascii_isspace ((next_token-1)^) then
        if (end_ptr = NULL) or (end_ptr > next_token) then
          end_ptr := next_token;
  end;

  for i := 0 to high(symbols) do
  begin
    next_token := StrPos(AText, symbols[i]);
    if next_token <> NULL then
      if (end_ptr = NULL) or (end_ptr > next_token) then
        end_ptr := next_token;
  end;
end;

procedure TPasTextBuffer.Highlight;
var
  start_iter, next_iter, tmp_iter: TTextIter;

  state       : Integer;
  ltext       : PChar;

  start_ptr, end_ptr : PChar;
  ltag        : String;
begin
  CreateTags;
  RemoveAllTags;

  state := STATE_NORMAL;
  start_iter := GetIterAtOffset(0);

  next_iter := start_iter;

  while (gtk_text_iter_forward_line(@next_iter)) do
  begin
    ltext  := gtk_text_iter_get_text (@start_iter, @next_iter);

    start_ptr := ltext;
    repeat
      ParseChars(start_ptr, end_ptr, state, ltag);

      if end_ptr <> NULL then
      begin
        tmp_iter := start_iter;
        gtk_text_iter_forward_chars (@tmp_iter, end_ptr - start_ptr);
      end
      else
        tmp_iter := next_iter;

      if ltag <> '' then
        ApplyTagByName(lTag, start_iter, tmp_iter);

      start_iter := tmp_iter;
      start_ptr  := end_ptr;
    until end_ptr = NULL;

    g_free(ltext);
    start_iter := next_iter;
  end;
end;

end.
