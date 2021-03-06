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

  TToolBar = class(TContainerControl)
  private
    FDefaultToolBarStyle: Boolean;
    FSmallIcons: Boolean;
    function GetOrientation: TOrientation;
    procedure SetOrientation(AValue: TOrientation);
    function GetShowArrow: Boolean;
    procedure SetShowArrow(AValue: Boolean);
    procedure SetSmallIcons(AValue: Boolean);
    function GetToolBarStyle: TToolBarStyle;
    procedure SetToolBarStyle(AValue: TToolBarStyle);
  protected
    procedure CreateHandle; override;
    procedure ConnectSignals; override;
    // TContainerControl
    procedure AddControl(AControl: TControl); override;
    procedure InternalAddControl(AControl: TControl); override;
    // TControl
    procedure SetParent(ANewParent: TControl); override;
    function IsBoxExpandStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    //--
    function GetCurrentToolBarStyle: TToolBarStyle;
  published
    property Orientation: TOrientation read GetOrientation write SetOrientation default orHorizontal;
    property ShowArrow: Boolean read GetShowArrow write SetShowArrow default True;
    property SmallIcons: Boolean read FSmallIcons write SetSmallIcons;
    property ToolBarStyle: TToolBarStyle read GetToolBarStyle write SetToolBarStyle default tbsDefault;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TToolBar }

constructor TToolBar.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultToolBarStyle := True;
end;

procedure TToolBar.CreateHandle;
begin
  Handle := gtk_toolbar_new;
end;

procedure TToolBar.ConnectSignals;
begin
  inherited;
  //orientation changed
end;

procedure TToolBar.AddControl(AControl: TControl);
begin
  if AControl is TCustomToolItem then
    inherited
  else
    raise Exception.Create('Only TCustomToolItem or decendants can be placed inside a TToolBar');
end;

procedure TToolBar.InternalAddControl(AControl: TControl);
begin
  gtk_toolbar_insert(Handle, AControl.Handle, -1);
  // last is position, -1 means "insert @ the end"
end;

function TToolBar.IsBoxExpandStored: Boolean;
begin
  Result := (Parent is TBox) and (BoxExpand = True);
end;

procedure TToolBar.SetParent(ANewParent: TControl);
begin
  inherited;
  if Parent is TBox then
    BoxExpand := False;
end;

function TToolBar.GetOrientation: TOrientation;
begin
  Result := TOrientation(gtk_toolbar_get_orientation(Handle));
end;

procedure TToolBar.SetOrientation(AValue: TOrientation);
begin
  gtk_toolbar_set_orientation(Handle, Ord(AValue));
end;

function TToolBar.GetShowArrow: Boolean;
begin
  Result := gtk_toolbar_get_show_arrow(Handle);
end;

procedure TToolBar.SetShowArrow(AValue: Boolean);
begin
  gtk_toolbar_set_show_arrow(Handle, AValue);
end;

procedure TToolBar.SetSmallIcons(AValue: Boolean);
begin
  if FSmallIcons <> AValue then
  begin
    FSmallIcons := True;
    //Changed; TODO
  end;
end;

function TToolBar.GetToolBarStyle: TToolBarStyle;
begin
  if FDefaultToolBarStyle then
    Result := tbsDefault
  else
    Result := TToolBarStyle(gtk_toolbar_get_style(Handle) + 1);
end;

procedure TToolBar.SetToolBarStyle(AValue: TToolBarStyle);
begin
  if AValue = tbsDefault then
    gtk_toolbar_unset_style(Handle)
  else
    gtk_toolbar_set_style(Handle, Ord(AValue)-1);
  //--
  FDefaultToolBarStyle := AValue = tbsDefault;
end;

function TToolBar.GetCurrentToolBarStyle: TToolBarStyle;
begin
  Result := TToolBarStyle(gtk_toolbar_get_style(Handle) + 1);
end;

{$ENDIF}
