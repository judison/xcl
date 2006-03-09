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

  TContainerControl = class(TControl)
  private
    FControlList: TComponentList;
    function GetBorderWidth: Integer;
    procedure SetBorderWidth(Value: Integer);
    function GetResizeMode: TResizeMode;
    procedure SetResizeMode(Value: TResizeMode);
  protected
    function IsResizeModeStored: Boolean; virtual;
    //--
    procedure InternalAddControl(AControl: TControl); virtual;
    procedure AddControl(AControl: TControl); override;
    procedure RemoveControl(AControl: TControl); override;
    procedure InternalRemoveControl(AControl: TControl); virtual;
    function GetControlByIndex(AIndex: Integer): TControl; override;
    function GetControlCount: Integer; override;
    procedure ShowCtrls; override;
    //==
  public
    destructor Destroy; override;
    //
    procedure CheckResize;
    //
    procedure ReorderControl(AChildControl: TControl; APos: Integer); virtual;
  published
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth default 0;
    property ResizeMode: TResizeMode read GetResizeMode write SetResizeMode stored IsResizeModeStored;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TContainerControl }

destructor TContainerControl.Destroy;
begin
  inherited;
end;

procedure TContainerControl.InternalAddControl(AControl: TControl);
begin
  gtk_container_add(Handle, AControl.Handle);
end;

procedure TContainerControl.AddControl(AControl: TControl);
begin
  if AControl <> nil then
  begin
    if FControlList = nil then
      FControlList := TComponentList.Create(False);
    //
    InternalAddControl(AControl);
    FControlList.Add(AControl);
    AControl.FParent := Self;
    AControl.FreeNotification(Self);
  end;
end;

procedure TContainerControl.RemoveControl(AControl: TControl);
begin
  if AControl.Parent = Self then
  begin
    InternalRemoveControl(AControl);
    FControlList.Remove(AControl);
    AControl.FParent := nil;
    //
    if FControlList.Count = 0 then
      FreeAndNil(FControlList);
  end
  else
    raise Exception.Create('Can''t remove a control that does not belong to this control.');
end;

procedure TContainerControl.InternalRemoveControl(AControl: TControl);
begin
  // Should work on most ContainerControl descendents
  gtk_container_remove(Handle, AControl.Handle);
end;

function TContainerControl.GetControlByIndex(AIndex: Integer): TControl;
begin
  if FControlList = nil then
    raise Exception.CreateFmt('Item index out of bounds. (%d)', [AIndex]);

  Result := TControl(FControlList.Items[AIndex]);
end;

procedure TContainerControl.ReorderControl(AChildControl: TControl; APos: Integer);
begin
  FControlList.Move(FControlList.IndexOf(AChildControl), APos);
end;

function TContainerControl.GetControlCount: Integer;
begin
  if FControlList = nil then
    Result := 0
  else
    Result := FControlList.Count;
end;

function TContainerControl.GetBorderWidth: Integer;
begin
  Result := gtk_container_get_border_width(Handle);
end;

procedure TContainerControl.SetBorderWidth(Value: Integer);
begin
  gtk_container_set_border_width(Handle, Value);
end;

function TContainerControl.GetResizeMode: TResizeMode;
begin
  Result := TResizeMode(gtk_container_get_resize_mode(Handle));
end;

procedure TContainerControl.SetResizeMode(Value: TResizeMode);
begin
  gtk_container_set_resize_mode(Handle, Ord(Value));
end;

function TContainerControl.IsResizeModeStored: Boolean;
begin
  Result := ResizeMode <> rmParent;
end;

procedure TContainerControl.CheckResize;
begin
  gtk_container_check_resize(Handle);
end;

procedure TContainerControl.ShowCtrls;
var
  I: Integer;
begin
  if FControlList <> nil then
    for I := 0 to ControlCount - 1 do
      Controls[I].ShowCtrls;
  //---
  inherited;
end;

{$ENDIF}
