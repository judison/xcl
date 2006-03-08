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

  TNotebook = class(TContainerControl)
  private
    function GetCurrentPage: Integer;
    procedure SetCurrentPage(AValue: Integer);
    function GetShowTabs: Boolean;
    procedure SetShowTabs(AValue: Boolean);
    function GetScrollable: Boolean;
    procedure SetScrollable(AValue: Boolean);
  protected
    procedure CreateHandle; override;
    procedure AddControl(AControl: TControl); override;
    procedure InternalRemoveControl(AControl: TControl); override;
  public
    function GetNthPage(AIdx: Integer): TNotebookPage;
    function GetNPages: Integer;
    function PageNum(APage: TNotebookPage): Integer;
    procedure NextPage;
    procedure PrevPage;
    //--
    property CurrentPage: Integer read GetCurrentPage write SetCurrentPage;
    property Pages[Index: Integer]: TNotebookPage read GetNthPage;
    property PageCount: Integer read GetNPages;
  published
    property ShowTabs: Boolean read GetShowTabs write SetShowTabs;
    property Scrollable: Boolean read GetScrollable write SetScrollable;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TNotebook }

procedure TNotebook.CreateHandle;
begin
  Handle := gtk_notebook_new;
end;

procedure TNotebook.AddControl(AControl: TControl);
begin
  if AControl is TNotebookPage then
    inherited
  else
    raise Exception.Create('Only TNotebookPage or decendants can be placed inside a TNotebook');
end;

procedure TNotebook.InternalRemoveControl(AControl: TControl);
begin
  gtk_notebook_remove_page(Handle, PageNum(TNotebookPage(AControl)));
end;

function TNotebook.GetCurrentPage: Integer;
begin
  Result := gtk_notebook_get_current_page(Handle);
end;

procedure TNotebook.SetCurrentPage(AValue: Integer);
begin
  gtk_notebook_set_current_page(Handle, AValue);
end;

function TNotebook.GetNthPage(AIdx: Integer): TNotebookPage;
var
  I: Integer;
  C: Pointer;
begin
  C := gtk_notebook_get_nth_page(Handle, AIdx);
  for I := 0 to ControlCount -1 do
    if Controls[I].Handle = C then
    begin
      Result := TNotebookPage(Controls[I]);
      exit;
    end;
  Result := nil;
end;

function TNotebook.GetNPages: Integer;
begin
  Result := gtk_notebook_get_n_pages(Handle);
end;

function TNotebook.PageNum(APage: TNotebookPage): Integer;
begin
  Result := gtk_notebook_page_num(Handle, APage.Handle);
end;

procedure TNotebook.NextPage;
begin
  gtk_notebook_next_page(Handle);
end;

procedure TNotebook.PrevPage;
begin
  gtk_notebook_prev_page(Handle);
end;

function TNotebook.GetShowTabs: Boolean;
begin
  Result := gtk_notebook_get_show_tabs(Handle);
end;

procedure TNotebook.SetShowTabs(AValue: Boolean);
begin
  gtk_notebook_set_show_tabs(Handle, AValue);
end;

function TNotebook.GetScrollable: Boolean;
begin
  Result := gtk_notebook_get_scrollable(Handle);
end;

procedure TNotebook.SetScrollable(AValue: Boolean);
begin
  gtk_notebook_set_scrollable(Handle, AValue);
end;

{$ENDIF}
