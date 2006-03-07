{
   Copyright (C) 2006 - Judison Oliveira Gil Filho <judison@gmail.com>

   Based on work of others, mainly Gustavo Giraldez and Paolo Maggi.
   Copyright notices are provided for each part of this file.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Library General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License* along with this program; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
}
unit gtksourceview;

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

interface

uses glib2, gdk2, gdk2pixbuf, gtk2;

const
{$ifdef win32}
  gsvlib = 'libgtksourceview-1-0-0.dll';
  {$IFDEF FPC}
    {$smartlink on}
  {$ENDIF}
{$else}
  gsvlib = 'libgtksourceview-1.0.so';
{$endif}


{
  gtksourceregex.h
  Copyright (C) 2003 - Paolo Maggi <paolo.maggi@polito.it>
}

type
  PGtkSourceBufferMatch = ^TGtkSourceBufferMatch;
  TGtkSourceBufferMatch = record
    startpos : gInt;
    endpos : gInt;
    startindex : gInt;
    endindex : gInt;
  end;

  PGtkSourceRegex = Pointer;
  TGtkSourceRegexOptions = (GTK_SOURCE_REGEX_NOT_BOL := 1 shl 0,GTK_SOURCE_REGEX_NOT_EOL := 1 shl 1);

function gtk_source_regex_compile(pattern:Pgchar):PGtkSourceRegex;cdecl;external gsvlib;
procedure gtk_source_regex_destroy(regex:PGtkSourceRegex);cdecl;external gsvlib;
function gtk_source_regex_search(regex:PGtkSourceRegex; text:Pgchar; pos:gInt; length:gInt; match:PGtkSourceBufferMatch; options:gUInt):gInt;cdecl;external gsvlib;
function gtk_source_regex_match(regex:PGtkSourceRegex; text:Pgchar; pos:gInt; len:gInt; options:gUInt):gBoolean;cdecl;external gsvlib;

{
  gtksourcetagstyle.h
  Copyright (C) 2003 - Paolo Maggi <paolo.maggi@polito.it>
}

type
  TGtkSourceTagStyleMask = (
    GTK_SOURCE_TAG_STYLE_USE_BACKGROUND := 1 shl 0,
    GTK_SOURCE_TAG_STYLE_USE_FOREGROUND := 1 shl 1
  );

  PGtkSourceTagStyle = ^TGtkSourceTagStyle;
  TGtkSourceTagStyle = record
    is_default : gBoolean;
    mask : gUInt;
    foreground : TGdkColor;
    background : TGdkColor;
    italic : gBoolean;
    bold : gBoolean;
    underline : gBoolean;
    strikethrough : gBoolean;
    reserved : array[0..15] of gUInt8;
  end;

function gtk_source_tag_style_mask_get_type:GType;cdecl;external gsvlib;

function gtk_source_tag_style_get_type:GType;cdecl;external gsvlib;
function gtk_source_tag_style_new:PGtkSourceTagStyle;cdecl;external gsvlib;
function gtk_source_tag_style_copy(style:PGtkSourceTagStyle):PGtkSourceTagStyle;cdecl;external gsvlib;
procedure gtk_source_tag_style_free(style:PGtkSourceTagStyle);cdecl;external gsvlib;


{
  gtksourcetag.h
  Copyright (C) 2001 - Mikael Hermansson<tyan@linux.se>
                     - Chris Phelps <chicane@reninet.com>
  Copyright (C) 2003 - Paolo Maggi <paolo.maggi@polito.it>
}

type
  PGtkSourceTag = ^TGtkSourceTag;
  TGtkSourceTag = record
    parent_instance : TGtkTextTag;
    id : Pgchar;
    style : PGtkSourceTagStyle;
  end;

  PGtkSourceTagClass = ^TGtkSourceTagClass;
  TGtkSourceTagClass = record
    parent_class : TGtkTextTagClass;
  end;

  PGtkSyntaxTag = ^TGtkSyntaxTag;
  TGtkSyntaxTag = record
    parent_instance : TGtkSourceTag;
    start : Pgchar;
    reg_start : PGtkSourceRegex;
    reg_end : PGtkSourceRegex;
  end;

  PGtkSyntaxTagClass = ^TGtkSyntaxTagClass;
  TGtkSyntaxTagClass = record
    parent_class : TGtkSourceTagClass;
  end;

  PGtkPatternTag = ^TGtkPatternTag;
  TGtkPatternTag = record
    parent_instance : TGtkSourceTag;
    reg_pattern : PGtkSourceRegex;
  end;

  PGtkPatternTagClass = ^TGtkPatternTagClass;
  TGtkPatternTagClass = record
    parent_class : TGtkSourceTagClass;
  end;

function gtk_source_tag_get_type:GType;cdecl;external gsvlib;
function gtk_source_tag_get_id(tag:PGtkSourceTag):Pgchar;cdecl;external gsvlib;
function gtk_source_tag_get_style(tag:PGtkSourceTag):PGtkSourceTagStyle;cdecl;external gsvlib;
procedure gtk_source_tag_set_style(tag:PGtkSourceTag; style:PGtkSourceTagStyle);cdecl;external gsvlib;
function gtk_syntax_tag_get_type:GType;cdecl;external gsvlib;
function gtk_syntax_tag_new(id:Pgchar; name:Pgchar; pattern_start:Pgchar; pattern_end:Pgchar):PGtkTextTag;cdecl;external gsvlib;
function gtk_pattern_tag_get_type:GType;cdecl;external gsvlib;
function gtk_pattern_tag_new(id:Pgchar; name:Pgchar; pattern:Pgchar):PGtkTextTag;cdecl;external gsvlib;
function gtk_keyword_list_tag_new(id:Pgchar; name:Pgchar; keywords:PGSList; case_sensitive:gBoolean; match_empty_string_at_beginning:gBoolean; match_empty_string_at_end:gBoolean; beginning_regex:Pgchar; end_regex:Pgchar):PGtkTextTag;cdecl;external gsvlib;
function gtk_line_comment_tag_new(id:Pgchar; name:Pgchar; pattern_start:Pgchar):PGtkTextTag;cdecl;external gsvlib;
function gtk_string_tag_new(id:Pgchar; name:Pgchar; pattern_start:Pgchar; pattern_end:Pgchar; end_at_line_end:gBoolean):PGtkTextTag;cdecl;external gsvlib;

{
  gtksourcetagtable.h
  Copyright (C) 2003 - Paolo Maggi <paolo.maggi@polito.it>
}

type
  PGtkSourceTagTable = ^TGtkSourceTagTable;
  TGtkSourceTagTable = record
    parent_instance : TGtkTextTagTable;
    priv : pointer;
  end;

  PGtkSourceTagTableClass = ^TGtkSourceTagTableClass;
  TGtkSourceTagTableClass = record
    parent_class : TGtkTextTagTableClass;
    changed : procedure (table:PGtkSourceTagTable);cdecl;
    _gtk_source_reserved1 : procedure ;
    _gtk_source_reserved2 : procedure ;
  end;

function gtk_source_tag_table_get_type:GType;cdecl;external gsvlib;
function gtk_source_tag_table_new:PGtkSourceTagTable;cdecl;external gsvlib;
procedure gtk_source_tag_table_add_tags(table:PGtkSourceTagTable; tags:PGSList);cdecl;external gsvlib;
procedure gtk_source_tag_table_remove_source_tags(table:PGtkSourceTagTable);cdecl;external gsvlib;

{
  gtksourcestylescheme.h
  Copyright (C) 2003 - Paolo Maggi <paolo.maggi@polito.it>
}

{* A theme should define at least the following styles to work well with the included .lang files:
 * - Base-N Integer
 * - Character
 * - Comment
 * - Data Type
 * - Function
 * - Decimal
 * - Floating Point
 * - Keyword
 * - Preprocessor
 * - String
 * - Specials
 * - Others (DEPRECATED, it has been replaced by "Data Type")
 * - Others 2
 * - Others 3
 * The default theme defines all of them.
 *}

type
  PGtkSourceStyleScheme = Pointer; //Judison

  PGtkSourceStyleSchemeClass = ^TGtkSourceStyleSchemeClass;
  TGtkSourceStyleSchemeClass = record
    base_iface : TGTypeInterface;
    style_changed : procedure (scheme:PGtkSourceStyleScheme; tag_id:Pgchar);cdecl;
    get_name : function (scheme:PGtkSourceStyleScheme):Pgchar;
    get_tag_style : function (scheme:PGtkSourceStyleScheme; style_name:Pgchar):PGtkSourceTagStyle;
    get_style_names : function (scheme:PGtkSourceStyleScheme):PGSList;
    _gtk_source_reserved1 : procedure ;
    _gtk_source_reserved2 : procedure ;
    _gtk_source_reserved3 : procedure ;
    _gtk_source_reserved4 : procedure ;
  end;


function gtk_source_style_scheme_get_type:GType;cdecl;external gsvlib;
function gtk_source_style_scheme_get_tag_style(scheme:PGtkSourceStyleScheme; style_name:Pgchar):PGtkSourceTagStyle;cdecl;external gsvlib;
function gtk_source_style_scheme_get_name(scheme:PGtkSourceStyleScheme):Pgchar;cdecl;external gsvlib;
function gtk_source_style_scheme_get_style_names(scheme:PGtkSourceStyleScheme):PGSList;cdecl;external gsvlib;
function gtk_source_style_scheme_get_default:PGtkSourceStyleScheme;cdecl;external gsvlib;

{
  gtksourcelanguage.h
  Copyright (C) 2003 - Paolo Maggi <paolo.maggi@polito.it>
}

type
  PGtkSourceLanguage = ^TGtkSourceLanguage;
  TGtkSourceLanguage = record
    parent : TGObject;
    priv : pointer;
  end;

  PGtkSourceLanguageClass = ^TGtkSourceLanguageClass;
  TGtkSourceLanguageClass = record
    parent_class : TGObjectClass;
    tag_style_changed : procedure (language:PGtkSourceLanguage; name:Pgchar);cdecl;
    _gtk_source_reserved1 : procedure ;
    _gtk_source_reserved2 : procedure ;
    _gtk_source_reserved3 : procedure ;
  end;


function gtk_source_language_get_type:GType;cdecl;external gsvlib;
function gtk_source_language_get_id(language:PGtkSourceLanguage):Pgchar;cdecl;external gsvlib;
function gtk_source_language_get_name(language:PGtkSourceLanguage):Pgchar;cdecl;external gsvlib;
function gtk_source_language_get_section(language:PGtkSourceLanguage):Pgchar;cdecl;external gsvlib;
{ The list must be freed and the tags unref'ed  }
function gtk_source_language_get_tags(language:PGtkSourceLanguage):PGSList;cdecl;external gsvlib; 
function gtk_source_language_get_escape_char(language:PGtkSourceLanguage):gunichar;cdecl;external gsvlib;
{ Should free the list (and free each string in it also).  }
function gtk_source_language_get_mime_types(language:PGtkSourceLanguage):PGSList;cdecl;external gsvlib;
procedure gtk_source_language_set_mime_types(language:PGtkSourceLanguage; mime_types:PGSList);cdecl;external gsvlib;
function gtk_source_language_get_style_scheme(language:PGtkSourceLanguage):PGtkSourceStyleScheme;cdecl;external gsvlib;
procedure gtk_source_language_set_style_scheme(language:PGtkSourceLanguage; scheme:PGtkSourceStyleScheme);cdecl;external gsvlib;
function gtk_source_language_get_tag_style(language:PGtkSourceLanguage; tag_id:Pgchar):PGtkSourceTagStyle;cdecl;external gsvlib;
procedure gtk_source_language_set_tag_style(language:PGtkSourceLanguage; tag_id:Pgchar; style:PGtkSourceTagStyle);cdecl;external gsvlib;
function gtk_source_language_get_tag_default_style(language:PGtkSourceLanguage; tag_id:Pgchar):PGtkSourceTagStyle;cdecl;external gsvlib;

{
  gtksourcelanguagesmanager.h
  Copyright (C) 2003 - Paolo Maggi <paolo.maggi@polito.it>
}

type
  PGtkSourceLanguagesManager = ^TGtkSourceLanguagesManager;
  TGtkSourceLanguagesManager = record
    parent : TGObject;
    priv : pointer;
  end;

  PGtkSourceLanguagesManagerClass = ^TGtkSourceLanguagesManagerClass;
  TGtkSourceLanguagesManagerClass = record
    parent_class : TGObjectClass;
    _gtk_source_reserved1 : procedure ;cdecl;
    _gtk_source_reserved2 : procedure ;
  end;

function gtk_source_languages_manager_get_type:GType;cdecl;external gsvlib;
function gtk_source_languages_manager_new:PGtkSourceLanguagesManager;cdecl;external gsvlib;
function gtk_source_languages_manager_get_available_languages(lm:PGtkSourceLanguagesManager):PGSList;cdecl;external gsvlib;
function gtk_source_languages_manager_get_language_from_mime_type(lm:PGtkSourceLanguagesManager; mime_type:Pgchar):PGtkSourceLanguage;cdecl;external gsvlib;
{ Property  }
function gtk_source_languages_manager_get_lang_files_dirs(lm:PGtkSourceLanguagesManager):PGSList;cdecl;external gsvlib;

{
  gtksourceiter.h
  Copyright (C) 2000, 2002 - Paolo Maggi
  Copyright (C) 2002, 2003 - Jeroen Zwartepoorte
}

type
  TGtkSourceSearchFlags = (
    GTK_SOURCE_SEARCH_VISIBLE_ONLY := 1 shl 0,
    GTK_SOURCE_SEARCH_TEXT_ONLY := 1 shl 1,
    GTK_SOURCE_SEARCH_CASE_INSENSITIVE := 1 shl 2
  );

function gtk_source_search_flags_get_type:GType;cdecl;external gsvlib;

function gtk_source_iter_forward_search(iter:PGtkTextIter; str:Pgchar; flags:TGtkSourceSearchFlags; match_start:PGtkTextIter; match_end:PGtkTextIter; limit:PGtkTextIter):gBoolean;cdecl;external gsvlib;
function gtk_source_iter_backward_search(iter:PGtkTextIter; str:Pgchar; flags:TGtkSourceSearchFlags; match_start:PGtkTextIter; match_end:PGtkTextIter; limit:PGtkTextIter):gBoolean;cdecl;external gsvlib;
function gtk_source_iter_find_matching_bracket(iter:PGtkTextIter):gBoolean;cdecl;external gsvlib;

{
  gtksourceundomanager.h
  Copyright (C) 1998, 1999 - Alex Roberts, Evan Lawrence
  Copyright (C) 2000, 2001 - Chema Celorio, Paolo Maggi
  Copyright (C) 2002, 2003 - Paolo Maggi
}

type
  PGtkSourceUndoManager = ^TGtkSourceUndoManager;
  TGtkSourceUndoManager = record
    base : TGObject;
    priv : Pointer;
  end;

  PGtkSourceUndoManagerClass = ^TGtkSourceUndoManagerClass;
  TGtkSourceUndoManagerClass = record
    parent_class : TGObjectClass;
    can_undo : procedure (um:PGtkSourceUndoManager; can_undo:gBoolean);cdecl;
    can_redo : procedure (um:PGtkSourceUndoManager; can_redo:gBoolean);
  end;

function gtk_source_undo_manager_get_type:GType;cdecl;external gsvlib;
function gtk_source_undo_manager_new(buffer:PGtkTextBuffer):PGtkSourceUndoManager;cdecl;external gsvlib;
function gtk_source_undo_manager_can_undo(um:PGtkSourceUndoManager):gBoolean;cdecl;external gsvlib;
function gtk_source_undo_manager_can_redo(um:PGtkSourceUndoManager):gBoolean;cdecl;external gsvlib;
procedure gtk_source_undo_manager_undo(um:PGtkSourceUndoManager);cdecl;external gsvlib;
procedure gtk_source_undo_manager_redo(um:PGtkSourceUndoManager);cdecl;external gsvlib;
procedure gtk_source_undo_manager_begin_not_undoable_action(um:PGtkSourceUndoManager);cdecl;external gsvlib;
procedure gtk_source_undo_manager_end_not_undoable_action(um:PGtkSourceUndoManager);cdecl;external gsvlib;
function gtk_source_undo_manager_get_max_undo_levels(um:PGtkSourceUndoManager):gInt;cdecl;external gsvlib;
procedure gtk_source_undo_manager_set_max_undo_levels(um:PGtkSourceUndoManager; undo_levels:gInt);cdecl;external gsvlib;

{
  gtksourcebuffer.h
  Copyright (C) 1999,2000,2001,2002 by:
                     - Mikael Hermansson <tyan@linux.se>
                     - Chris Phelps <chicane@reninet.com>
                     - Jeroen Zwartepoorte <jeroen@xs4all.nl>
  Copyright (C) 2003 - Paolo Maggi, Gustavo Giraldez
  gtksourcemarker.h
  Copyright (C) 2003 - Gustavo Giraldez <gustavo.giraldez@gmx.net>

}

type
  PGtkSourceMarker = ^TGtkSourceMarker;
  TGtkSourceMarker = TGtkTextMark;

  PGtkSourceMarkerClass = ^TGtkSourceMarkerClass;
  TGtkSourceMarkerClass = TGtkTextMarkClass;

type
  PGtkSourceBuffer = ^TGtkSourceBuffer;
  TGtkSourceBuffer = record
    text_buffer : TGtkTextBuffer;
    priv : pointer;
  end;

  PGtkSourceBufferClass = ^TGtkSourceBufferClass;
  TGtkSourceBufferClass = record
    parent_class : TGtkTextBufferClass;
    can_undo : procedure (buffer:PGtkSourceBuffer; can_undo:gBoolean);cdecl;
    can_redo : procedure (buffer:PGtkSourceBuffer; can_redo:gBoolean);
    highlight_updated : procedure (buffer:PGtkSourceBuffer; start:PGtkTextIter; _end:PGtkTextIter);
    marker_updated : procedure (buffer:PGtkSourceBuffer; where:PGtkTextIter);
    _gtk_source_reserved1 : procedure ;
    _gtk_source_reserved2 : procedure ;
    _gtk_source_reserved3 : procedure ;
  end;

function gtk_source_buffer_get_type:GType;cdecl;external gsvlib;
{ Constructor  }
function gtk_source_buffer_new(table:PGtkSourceTagTable):PGtkSourceBuffer;cdecl;external gsvlib;
function gtk_source_buffer_new_with_language(language:PGtkSourceLanguage):PGtkSourceBuffer;cdecl;external gsvlib;
{ Properties.  }
function gtk_source_buffer_get_check_brackets(buffer:PGtkSourceBuffer):gBoolean;cdecl;external gsvlib;
procedure gtk_source_buffer_set_check_brackets(buffer:PGtkSourceBuffer; check_brackets:gBoolean);cdecl;external gsvlib;
procedure gtk_source_buffer_set_bracket_match_style(source_buffer:PGtkSourceBuffer; style:PGtkSourceTagStyle);cdecl;external gsvlib;
function gtk_source_buffer_get_highlight(buffer:PGtkSourceBuffer):gBoolean;cdecl;external gsvlib;
procedure gtk_source_buffer_set_highlight(buffer:PGtkSourceBuffer; highlight:gBoolean);cdecl;external gsvlib;
function gtk_source_buffer_get_max_undo_levels(buffer:PGtkSourceBuffer):gInt;cdecl;external gsvlib;
procedure gtk_source_buffer_set_max_undo_levels(buffer:PGtkSourceBuffer; max_undo_levels:gInt);cdecl;external gsvlib;
function gtk_source_buffer_get_language(buffer:PGtkSourceBuffer):PGtkSourceLanguage;cdecl;external gsvlib;
procedure gtk_source_buffer_set_language(buffer:PGtkSourceBuffer; language:PGtkSourceLanguage);cdecl;external gsvlib;
function gtk_source_buffer_get_escape_char(buffer:PGtkSourceBuffer):gunichar;cdecl;external gsvlib;
procedure gtk_source_buffer_set_escape_char(buffer:PGtkSourceBuffer; escape_char:gunichar);cdecl;external gsvlib;
{ Undo/redo methods  }
function gtk_source_buffer_can_undo(buffer:PGtkSourceBuffer):gBoolean;cdecl;external gsvlib;
function gtk_source_buffer_can_redo(buffer:PGtkSourceBuffer):gBoolean;cdecl;external gsvlib;
procedure gtk_source_buffer_undo(buffer:PGtkSourceBuffer);cdecl;external gsvlib;
procedure gtk_source_buffer_redo(buffer:PGtkSourceBuffer);cdecl;external gsvlib;
procedure gtk_source_buffer_begin_not_undoable_action(buffer:PGtkSourceBuffer);cdecl;external gsvlib;
procedure gtk_source_buffer_end_not_undoable_action(buffer:PGtkSourceBuffer);cdecl;external gsvlib;
{ marker methods.  }
function gtk_source_buffer_create_marker(buffer:PGtkSourceBuffer; name:Pgchar; _type:Pgchar; where:PGtkTextIter):PGtkSourceMarker;cdecl;external gsvlib;
procedure gtk_source_buffer_move_marker(buffer:PGtkSourceBuffer; marker:PGtkSourceMarker; where:PGtkTextIter);cdecl;external gsvlib;
procedure gtk_source_buffer_delete_marker(buffer:PGtkSourceBuffer; marker:PGtkSourceMarker);cdecl;external gsvlib;
function gtk_source_buffer_get_marker(buffer:PGtkSourceBuffer; name:Pgchar):PGtkSourceMarker;cdecl;external gsvlib;
function gtk_source_buffer_get_markers_in_region(buffer:PGtkSourceBuffer; _begin:PGtkTextIter; _end:PGtkTextIter):PGSList;cdecl;external gsvlib;
function gtk_source_buffer_get_first_marker(buffer:PGtkSourceBuffer):PGtkSourceMarker;cdecl;external gsvlib;
function gtk_source_buffer_get_last_marker(buffer:PGtkSourceBuffer):PGtkSourceMarker;cdecl;external gsvlib;
procedure gtk_source_buffer_get_iter_at_marker(buffer:PGtkSourceBuffer; iter:PGtkTextIter; marker:PGtkSourceMarker);cdecl;external gsvlib;
function gtk_source_buffer_get_next_marker(buffer:PGtkSourceBuffer; iter:PGtkTextIter):PGtkSourceMarker;cdecl;external gsvlib;
function gtk_source_buffer_get_prev_marker(buffer:PGtkSourceBuffer; iter:PGtkTextIter):PGtkSourceMarker;cdecl;external gsvlib;

function gtk_source_marker_get_type:GType;cdecl;external gsvlib;
procedure gtk_source_marker_set_marker_type(marker:PGtkSourceMarker; _type:Pgchar);cdecl;external gsvlib;
function gtk_source_marker_get_marker_type(marker:PGtkSourceMarker):Pgchar;cdecl;external gsvlib;
function gtk_source_marker_get_line(marker:PGtkSourceMarker):gInt;cdecl;external gsvlib;
function gtk_source_marker_get_name(marker:PGtkSourceMarker):Pgchar;cdecl;external gsvlib;
function gtk_source_marker_get_buffer(marker:PGtkSourceMarker):PGtkSourceBuffer;cdecl;external gsvlib;
function gtk_source_marker_next(marker:PGtkSourceMarker):PGtkSourceMarker;cdecl;external gsvlib;
function gtk_source_marker_prev(marker:PGtkSourceMarker):PGtkSourceMarker;cdecl;external gsvlib;


{
  gtktextregion.h
  Copyright (C) 2002 Gustavo Giraldez <gustavo.giraldez@gmx.net>
}

type
  PGtkTextRegionIterator = ^TGtkTextRegionIterator;
  TGtkTextRegionIterator = record
    dummy1 : gPointer;
    dummy2 : gUInt32;
    dummy3 : gPointer;
  end;

  PGtkTextRegion = Pointer; //judison

function gtk_text_region_new(buffer:PGtkTextBuffer):PGtkTextRegion;cdecl;external gsvlib;
procedure gtk_text_region_destroy(region:PGtkTextRegion; delete_marks:gBoolean);cdecl;external gsvlib;
function gtk_text_region_get_buffer(region:PGtkTextRegion):PGtkTextBuffer;cdecl;external gsvlib;
procedure gtk_text_region_add(region:PGtkTextRegion; _start:PGtkTextIter; _end:PGtkTextIter);cdecl;external gsvlib;
procedure gtk_text_region_subtract(region:PGtkTextRegion; _start:PGtkTextIter; _end:PGtkTextIter);cdecl;external gsvlib;
function gtk_text_region_subregions(region:PGtkTextRegion):gInt;cdecl;external gsvlib;
function gtk_text_region_nth_subregion(region:PGtkTextRegion; subregion:gUInt; start:PGtkTextIter; _end:PGtkTextIter):gBoolean;cdecl;external gsvlib;
function gtk_text_region_intersect(region:PGtkTextRegion; _start:PGtkTextIter; _end:PGtkTextIter):PGtkTextRegion;cdecl;external gsvlib;
procedure gtk_text_region_get_iterator(region:PGtkTextRegion; iter:PGtkTextRegionIterator; start:gUInt);cdecl;external gsvlib;
function gtk_text_region_iterator_is_end(iter:PGtkTextRegionIterator):gBoolean;cdecl;external gsvlib;
function gtk_text_region_iterator_next(iter:PGtkTextRegionIterator):gBoolean;cdecl;external gsvlib;
procedure gtk_text_region_iterator_get_subregion(iter:PGtkTextRegionIterator; start:PGtkTextIter; _end:PGtkTextIter);cdecl;external gsvlib;
procedure gtk_text_region_debug_print(region:PGtkTextRegion);cdecl;external gsvlib;

{
  gtksourceview.h
  Copyright (C) 2001 - Mikael Hermansson <tyan@linux.se>
                     - Chris Phelps <chicane@reninet.com>
  Copyright (C) 2003 - Gustavo Giraldez and Paolo Maggi
}

type
  PGtkSourceView = ^TGtkSourceView;
  TGtkSourceView = record
    parent : TGtkTextView;
    priv : pointer;
  end;

  PGtkSourceViewClass = ^TGtkSourceViewClass;
  TGtkSourceViewClass = record
    parent_class : TGtkTextViewClass;
    undo : procedure (view:PGtkSourceView);cdecl;
    redo : procedure (view:PGtkSourceView);
    _gtk_source_reserved1 : procedure ;
    _gtk_source_reserved2 : procedure ;
    _gtk_source_reserved3 : procedure ;
  end;

function gtk_source_view_get_type:GType;cdecl;external gsvlib;
{ Constructors  }
function gtk_source_view_new:PGtkWidget;cdecl;external gsvlib;
function gtk_source_view_new_with_buffer(buffer:PGtkSourceBuffer):PGtkWidget;cdecl;external gsvlib;
{ Properties  }
procedure gtk_source_view_set_show_line_numbers(view:PGtkSourceView; show:gBoolean);cdecl;external gsvlib;
function gtk_source_view_get_show_line_numbers(view:PGtkSourceView):gBoolean;cdecl;external gsvlib;
procedure gtk_source_view_set_show_line_markers(view:PGtkSourceView; show:gBoolean);cdecl;external gsvlib;
function gtk_source_view_get_show_line_markers(view:PGtkSourceView):gBoolean;cdecl;external gsvlib;
procedure gtk_source_view_set_tabs_width(view:PGtkSourceView; width:gUInt);cdecl;external gsvlib;
function gtk_source_view_get_tabs_width(view:PGtkSourceView):gUInt;cdecl;external gsvlib;
procedure gtk_source_view_set_auto_indent(view:PGtkSourceView; enable:gBoolean);cdecl;external gsvlib;
function gtk_source_view_get_auto_indent(view:PGtkSourceView):gBoolean;cdecl;external gsvlib;
procedure gtk_source_view_set_insert_spaces_instead_of_tabs(view:PGtkSourceView; enable:gBoolean);cdecl;external gsvlib;
function gtk_source_view_get_insert_spaces_instead_of_tabs(view:PGtkSourceView):gBoolean;cdecl;external gsvlib;
procedure gtk_source_view_set_show_margin(view:PGtkSourceView; show:gBoolean);cdecl;external gsvlib;
function gtk_source_view_get_show_margin(view:PGtkSourceView):gBoolean;cdecl;external gsvlib;
procedure gtk_source_view_set_highlight_current_line(view:PGtkSourceView; show:gBoolean);cdecl;external gsvlib;
function gtk_source_view_get_highlight_current_line(view:PGtkSourceView):gBoolean;cdecl;external gsvlib;
procedure gtk_source_view_set_margin(view:PGtkSourceView; margin:gUInt);cdecl;external gsvlib;
function gtk_source_view_get_margin(view:PGtkSourceView):gUInt;cdecl;external gsvlib;
procedure gtk_source_view_set_marker_pixbuf(view:PGtkSourceView; marker_type:Pgchar; pixbuf:PGdkPixbuf);cdecl;external gsvlib;
function gtk_source_view_get_marker_pixbuf(view:PGtkSourceView; marker_type:Pgchar):PGdkPixbuf;cdecl;external gsvlib;
procedure gtk_source_view_set_smart_home_end(view:PGtkSourceView; enable:gBoolean);cdecl;external gsvlib;
function gtk_source_view_get_smart_home_end(view:PGtkSourceView):gBoolean;cdecl;external gsvlib;

implementation

end.
