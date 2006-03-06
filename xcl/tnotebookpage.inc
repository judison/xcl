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

  TNotebookPage = class(TBinControl)
  private
  protected
    procedure CreateHandle; override;
    function GetCaption: String;
    procedure SetCaption(AValue: String);
    procedure SetParent(ANewParent: TControl); override;
  public
  published
    property Caption: String read GetCaption write SetCaption;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TNotebookPage }

procedure TNotebookPage.SetParent(ANewParent: TControl);
begin
  if (ANewParent = nil) or (ANewParent is TNotebook) then
    inherited
  else
    raise Exception.Create('A TNotebookPage can only be placed in a TNotebook.');
end;

procedure TNotebookPage.CreateHandle;
begin
  Handle := gtk_frame_new(nil);
  gtk_frame_set_shadow_type(Handle, GTK_SHADOW_NONE);
end;

function TNotebookPage.GetCaption: String;
begin
  Result := gtk_notebook_get_tab_label_text(Parent.Handle, Handle);
end;

procedure TNotebookPage.SetCaption(AValue: String);
begin
  gtk_notebook_set_tab_label_text(Parent.Handle, Handle, PChar(AValue));
end;

{$ENDIF}
