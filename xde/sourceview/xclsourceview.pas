(*
   XCL TSourceView Component
   Copyright (C) 2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)


unit XCLSourceView;

{$H+}
{$MODE ObjFpc}

// If you don't have gtksourceview add a line
// "#DEFINE DISABLE_GTK_SOURCE_VIEW" in your fpc.cfg
{$IFNDEF DISABLE_GTK_SOURCE_VIEW}
  {$DEFINE HAS_GTK_SOURCE_VIEW}
{$ENDIF}
interface

uses Classes, SysUtils, xcl;

type
  TSourceLanguage = class;

  TSourceLanguagesManager = class(TComponent)
  private
    Handle: Pointer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetLanguageFromMime(AMime: String): TSourceLanguage;
  published
  end;

  TSourceLanguage = class
  private
    Handle: Pointer;
  protected
  public
    constructor Create(AHandle: Pointer);
  end;

  TSourceBuffer = class(TTextBuffer)
  private
    UMHandle: Pointer;
    procedure SetHighlight(AValue: Boolean);
    function GetHighlight: Boolean;
  protected
    procedure CreateHandle; override;
  public
    destructor Destroy; override;
    procedure SetLanguage(ALang: TSourceLanguage);
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    procedure Undo;
    procedure Redo;
    procedure BeginNotUndoableAction;
    procedure EndNotUndoableAction;
  published
    property Highlight: Boolean read GetHighlight write SetHighlight;
  end;

  TSourceView = class(TTextView)
  private
    procedure SetShowLineNumbers(AValue: Boolean);
    function GetShowLineNumbers: Boolean;
    procedure SetShowLineMarkers(AValue: Boolean);
    function GetShowLineMarkers: Boolean;
    procedure SetTabsWidth(AValue: Word);
    function GetTabsWidth: Word;
    procedure SetAutoIndent(AValue: Boolean);
    function GetAutoIndent: Boolean;
    procedure SetInsertSpacesInsteadOfTabs(AValue: Boolean);
    function GetInsertSpacesInsteadOfTabs: Boolean;
    procedure SetShowMargin(AValue: Boolean);
    function GetShowMargin: Boolean;
    procedure SetHighlightCurrentLine(AValue: Boolean);
    function GetHighlightCurrentLine: Boolean;
    procedure SetMargin(AValue: Word);
    function GetMargin: Word;
    procedure SetSmartHomeEnd(AValue: Boolean);
    function GetSmartHomeEnd: Boolean;
  protected
    procedure CreateHandle; override;
  public
  published
    property ShowLineNumbers: Boolean read GetShowLineNumbers write SetShowLineNumbers;
    property ShowLineMarkers: Boolean read GetShowLineMarkers write SetShowLineMarkers;
    property TabsWidth: Word read GetTabsWidth write SetTabsWidth;
    property AutoIndent: Boolean read GetAutoIndent write SetAutoIndent;
    property InsertSpacesInsteadOfTabs: Boolean read GetInsertSpacesInsteadOfTabs write SetInsertSpacesInsteadOfTabs;
    property ShowMargin: Boolean read GetShowMargin write SetShowMargin;
    property HighlightCurrentLine: Boolean read GetHighlightCurrentLine write SetHighlightCurrentLine;
    property Margin: Word read GetMargin write SetMargin;
    property SmartHomeEnd: Boolean read GetSmartHomeEnd write SetSmartHomeEnd;
  end;

implementation

{$IFDEF HAS_GTK_SOURCE_VIEW}
uses glib2, gtksourceview;
{$ENDIF}

{ TSourceLanguage }

constructor TSourceLanguage.Create(AHandle: Pointer);
begin
  Handle := AHandle;
end;

{ TSourceLanguagesManager }

constructor TSourceLanguagesManager.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Handle := gtk_source_languages_manager_new;
  g_object_ref(Handle);
{$ENDIF}
end;

destructor TSourceLanguagesManager.Destroy;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  g_object_unref(Handle);
{$ENDIF}
  inherited;
end;

function TSourceLanguagesManager.GetLanguageFromMime(AMime: String): TSourceLanguage;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := TSourceLanguage.Create(gtk_source_languages_manager_get_language_from_mime_type(Handle, PChar(AMime)));
{$ELSE}
  Result := TSourceLanguage.Create(nil);
{$ENDIF}
end;

{ TSourceBuffer }

procedure TSourceBuffer.CreateHandle;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Handle := gtk_source_buffer_new(nil);

  UMHandle := gtk_source_undo_manager_new(Handle);
  g_object_ref(UMHandle);
{$ELSE}
  inherited;
{$ENDIF}
end;

destructor TSourceBuffer.Destroy;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  g_object_unref(UMHandle);
{$ENDIF}
  inherited;
end;

function TSourceBuffer.CanUndo: Boolean;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_undo_manager_can_undo(UMhandle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TSourceBuffer.CanRedo: Boolean;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_undo_manager_can_redo(UMhandle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TSourceBuffer.Undo;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_undo_manager_undo(UMhandle);
{$ENDIF}
end;

procedure TSourceBuffer.Redo;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_undo_manager_redo(UMhandle);
{$ENDIF}
end;

procedure TSourceBuffer.BeginNotUndoableAction;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_undo_manager_begin_not_undoable_action(UMhandle);
{$ENDIF}
end;

procedure TSourceBuffer.EndNotUndoableAction;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_undo_manager_end_not_undoable_action(UMhandle);
{$ENDIF}
end;

procedure TSourceBuffer.SetLanguage(ALang: TSourceLanguage);
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_buffer_set_language(Handle, ALang.Handle);
{$ENDIF}
end;

procedure TSourceBuffer.SetHighlight(AValue: Boolean);
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_buffer_set_highlight(Handle, AValue);
{$ENDIF}
end;

function TSourceBuffer.GetHighlight: Boolean;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_buffer_get_highlight(Handle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

{ TSourceView }

procedure TSourceView.CreateHandle;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Handle := gtk_source_view_new();
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure TSourceView.SetShowLineNumbers(AValue: Boolean);
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_view_set_show_line_numbers(Handle, AValue);
{$ENDIF}
end;

function TSourceView.GetShowLineNumbers: Boolean;
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_view_get_show_line_numbers(Handle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TSourceView.SetShowLineMarkers(AValue: Boolean);           
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_view_set_show_line_markers(Handle, AValue);
{$ENDIF}
end;

function TSourceView.GetShowLineMarkers: Boolean;                    
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_view_get_show_line_markers(Handle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TSourceView.SetTabsWidth(AValue: Word);                    
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_view_set_tabs_width(Handle, AValue);
{$ENDIF}
end;

function TSourceView.GetTabsWidth: Word;                             
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_view_get_tabs_width(Handle);
{$ELSE}
  Result := 0;
{$ENDIF}
end;

procedure TSourceView.SetAutoIndent(AValue: Boolean);                
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_view_set_auto_indent(Handle, AValue);
{$ENDIF}
end;

function TSourceView.GetAutoIndent: Boolean;                         
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_view_get_auto_indent(Handle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TSourceView.SetInsertSpacesInsteadOfTabs(AValue: Boolean); 
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_view_set_insert_spaces_instead_of_tabs(Handle, AValue);
{$ENDIF}
end;

function TSourceView.GetInsertSpacesInsteadOfTabs: Boolean;          
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_view_get_insert_spaces_instead_of_tabs(Handle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TSourceView.SetShowMargin(AValue: Boolean);                
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_view_set_show_margin(Handle, AValue);
{$ENDIF}
end;

function TSourceView.GetShowMargin: Boolean;                         
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_view_get_show_margin(Handle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TSourceView.SetHighlightCurrentLine(AValue: Boolean);      
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_view_set_highlight_current_line(Handle, AValue);
{$ENDIF}
end;

function TSourceView.GetHighlightCurrentLine: Boolean;               
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_view_get_highlight_current_line(Handle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TSourceView.SetMargin(AValue: Word);                       
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_view_set_margin(Handle, AValue);
{$ENDIF}
end;

function TSourceView.GetMargin: Word;                                
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_view_get_margin(Handle);
{$ELSE}
  Result := 80;
{$ENDIF}
end;

procedure TSourceView.SetSmartHomeEnd(AValue: Boolean);              
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  gtk_source_view_set_smart_home_end(Handle, AValue);
{$ENDIF}
end;

function TSourceView.GetSmartHomeEnd: Boolean;                       
begin
{$IFDEF HAS_GTK_SOURCE_VIEW}
  Result := gtk_source_view_get_smart_home_end(Handle);
{$ELSE}
  Result := False;
{$ENDIF}
end;

begin
  RegisterClass(TSourceView);
end.
