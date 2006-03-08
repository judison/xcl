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

  TCustomMenuItem = class(TControl) // ou TComponent???
  private
    MenuHandle: Pointer;
    FSubItems: TComponentList;
    FOnClick: TNotifyEvent;
    //---
    function GetRightJustified: Boolean;
    procedure SetRightJustified(AValue: Boolean);
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure ConnectSignals; override;
    procedure CreateMenuHandle;
    procedure DestroyMenuHandle;
    //--- TControl Holder functionality
    procedure AddControl(AControl: TControl); override;
    procedure RemoveControl(AControl: TControl); override;
    function GetControlByIndex(AIndex: Integer): TControl; override;
    function GetControlCount: Integer; override;
    procedure ShowCtrls; override;
    //--
    procedure DoActivate(var Msg: TGtkSig); message 'GTK:activate';
  public
    procedure Activate;
    procedure Select;
    procedure Deselect;
  published
    property RightJustified: Boolean read GetRightJustified write SetRightJustified default False;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TCustomMenuItem }

procedure TCustomMenuItem.CreateHandle;
begin
end;

procedure TCustomMenuItem.DestroyHandle;
begin
  if FSubItems <> nil then
    DestroyMenuHandle;
  inherited;
end;

procedure TCustomMenuItem.CreateMenuHandle;
begin
  MenuHandle := gtk_menu_new;
  g_object_ref(MenuHandle);
  gtk_menu_item_set_submenu(Handle, MenuHandle);
end;

procedure TCustomMenuItem.DestroyMenuHandle;
begin
  gtk_menu_item_remove_submenu(Handle);
  gtk_widget_destroy(MenuHandle);
end;

procedure TCustomMenuItem.ConnectSignals;
begin
  inherited;
  ConnectGtkSignal('activate');
end;


procedure TCustomMenuItem.AddControl(AControl: TControl);
begin
  if AControl <> nil then
    if (AControl is TCustomMenuItem) then
    begin
      if FSubItems = nil then
      begin
        FSubItems := TComponentList.Create(False);
        CreateMenuHandle;
      end;
      gtk_menu_shell_append(MenuHandle, AControl.Handle);
      //---
      FSubItems.Add(AControl);
      AControl.FParent := Self;
    end
    else
      raise Exception.Create('A MenuItem can hold only others MenuItems.');
end;

procedure TCustomMenuItem.RemoveControl(AControl: TControl);
begin
  if AControl.Parent = Self then
  begin
    gtk_container_remove(MenuHandle, AControl.Handle);
    FSubItems.Remove(AControl);
    AControl.FParent := nil;
    //
    if FSubItems.Count = 0 then
    begin
      FreeAndNil(FSubItems);
      DestroyMenuHandle;
    end;
  end
  else
    raise Exception.Create('Can''t remove a control that does not belong to this MenuItem.');
end;

function TCustomMenuItem.GetControlByIndex(AIndex: Integer): TControl;
begin
  if FSubItems = nil then
    raise Exception.CreateFmt('Item index out of bounds. (%d)', [AIndex]);
  //--
  Result := TControl(FSubItems.Items[AIndex]);
end;

function TCustomMenuItem.GetControlCount: Integer;
begin
  if FSubItems = nil then
    Result := 0
  else
    Result := FSubItems.Count;
end;

procedure TCustomMenuItem.ShowCtrls;
var
  I: Integer;
begin
  if FSubItems <> nil then
    for I := 0 to ControlCount - 1 do
      Controls[I].ShowCtrls;
  //---
  inherited;
end;

procedure TCustomMenuItem.DoActivate(var Msg: TGtkSig);
begin
  if ActionLink <> nil then
    ActionLink.Execute(Self)
  else if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCustomMenuItem.Activate;
begin
  gtk_menu_item_activate(Handle);
end;

procedure TCustomMenuItem.Select;
begin
  gtk_menu_item_select(Handle);
end;

procedure TCustomMenuItem.Deselect;
begin
  gtk_menu_item_deselect(Handle);
end;

function TCustomMenuItem.GetRightJustified: Boolean;
begin
  Result := gtk_menu_item_get_right_justified(Handle);
end;

procedure TCustomMenuItem.SetRightJustified(AValue: Boolean);
begin
  gtk_menu_item_set_right_justified(Handle, AValue);
end;

{$ENDIF}
