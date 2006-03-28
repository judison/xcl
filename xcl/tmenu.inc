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

  TMenu = class(TComponent)
  private
    FMenus: TList;
    //==
    FIconSizeGroup: Pointer;
    FLabelSizeGroup: Pointer;
  protected
    Handle: Pointer;
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    //==
    procedure AddMenu(AMenu: TCustomMenuItem); virtual;
    procedure RemoveMenu(AMenu: TCustomMenuItem); virtual;
    //==
    procedure Popup(AButton: Integer); virtual;
    procedure Popup;
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TMenu }

constructor TMenu.Create(AOwner: TComponent);
begin
  inherited;
  FMenus := TList.Create;
  CreateHandle;
end;

destructor TMenu.Destroy;
begin
  DestroyHandle;
  FMenus.Free;
  inherited;
end;

procedure TMenu.CreateHandle;
begin
  FIconSizeGroup := gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
  FLabelSizeGroup := gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
  //--
  Handle := gtk_menu_new;
end;

procedure TMenu.DestroyHandle;
begin
  gtk_widget_destroy(Handle);
  //--
  g_object_unref(FIconSizeGroup);
  g_object_unref(FLabelSizeGroup);
end;

procedure TMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent is TCustomMenuItem) and (TCustomMenuItem(AComponent).ParentMenu = Self) and (Operation = opRemove) then
    RemoveMenu(TCustomMenuItem(AComponent));
  inherited;
end;


procedure TMenu.AddMenu(AMenu: TCustomMenuItem);
begin
  FMenus.Add(AMenu);
  AMenu.FParentMenu := Self;
  AMenu.FreeNotification(Self);
  gtk_menu_shell_append(Handle, AMenu.Handle);
end;

procedure TMenu.RemoveMenu(AMenu: TCustomMenuItem);
begin
  gtk_container_remove(Handle, AMenu.Handle);
  FMenus.Remove(AMenu);
  AMenu.FParentMenu := nil;
end;

procedure TMenu.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FMenus.Count - 1 do
    if TCustomMenuItem(FMenus[I]).Owner = Root then
      Proc(TCustomMenuItem(FMenus[I]));
end;

procedure TMenu.Popup;
begin
  Popup(0);
end;

procedure TMenu.Popup(AButton: Integer);
begin
  gtk_menu_popup(Handle, nil, nil, nil, nil, AButton, gtk_get_current_event_time());
end;

{$ENDIF}
