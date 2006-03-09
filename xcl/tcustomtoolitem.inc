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

  TCustomToolItem = class(TBinControl)
  private
    function GetOrientation: TOrientation;
    function GetToolBarStyle: TToolBarStyle;
    function GetHomogeneous: Boolean;
    procedure SetHomogeneous(AValue: Boolean);
    function GetExpand: Boolean;
    procedure SetExpand(AValue: Boolean);
    function GetVisibleHorizontal: Boolean;
    procedure SetVisibleHorizontal(AValue: Boolean);
    function GetVisibleVertical: Boolean;
    procedure SetVisibleVertical(AValue: Boolean);
    function GetIsImportant: Boolean;
    procedure SetIsImportant(AValue: Boolean);
  protected
    procedure CreateHandle; override;
  public
    property Orientation: TOrientation read GetOrientation;
    property ToolBarStyle: TToolBarStyle read GetToolBarStyle;
  published
    property Homogeneous: Boolean read GetHomogeneous write SetHomogeneous default True;
    property Expand: Boolean read GetExpand write SetExpand; //TODO: Default ???
    property VisibleHorizontal: Boolean read GetVisibleHorizontal write SetVisibleHorizontal default True;
    property VisibleVertical: Boolean read GetVisibleVertical write SetVisibleVertical default True;
    property IsImportant: Boolean read GetIsImportant write SetIsImportant default False;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TCustomToolItem }

procedure TCustomToolItem.CreateHandle;
begin
  Handle := gtk_tool_item_new;
end;

function TCustomToolItem.GetOrientation: TOrientation;
begin
  Result := TOrientation(gtk_tool_item_get_orientation(Handle));
end;

function TCustomToolItem.GetToolBarStyle: TToolBarStyle;
begin
  Result := TToolBarStyle(gtk_tool_item_get_toolbar_style(Handle)+1);
end;

function TCustomToolItem.GetHomogeneous: Boolean;
begin
  Result := gtk_tool_item_get_homogeneous(Handle);
end;

procedure TCustomToolItem.SetHomogeneous(AValue: Boolean);
begin
  gtk_tool_item_set_homogeneous(Handle, AValue);
end;

function TCustomToolItem.GetExpand: Boolean;
begin
  Result := gtk_tool_item_get_expand(Handle);
end;

procedure TCustomToolItem.SetExpand(AValue: Boolean);
begin
  gtk_tool_item_set_expand(Handle, AValue);
end;

function TCustomToolItem.GetVisibleHorizontal: Boolean;
begin
  Result := gtk_tool_item_get_visible_horizontal(Handle);
end;

procedure TCustomToolItem.SetVisibleHorizontal(AValue: Boolean);
begin
  gtk_tool_item_set_visible_horizontal(Handle, AValue);
end;

function TCustomToolItem.GetVisibleVertical: Boolean;
begin
  Result := gtk_tool_item_get_visible_vertical(Handle);
end;

procedure TCustomToolItem.SetVisibleVertical(AValue: Boolean);
begin
  gtk_tool_item_set_visible_vertical(Handle, AValue);
end;

function TCustomToolItem.GetIsImportant: Boolean;
begin
  Result := gtk_tool_item_get_is_important(Handle);
end;

procedure TCustomToolItem.SetIsImportant(AValue: Boolean);
begin
  gtk_tool_item_set_is_important(Handle, AValue);
end;

{$ENDIF}
