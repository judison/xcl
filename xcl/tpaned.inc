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

  TPaned = class(TContainerControl)
  private
    FChildA: TControl;
    FChildB: TControl;
    function GetPosition: Integer;
    procedure SetPosition(AValue: Integer);
  protected
    procedure AddControl(AControl: TControl); override;
    procedure RemoveControl(AControl: TControl); override;
    function GetControlByIndex(AIndex: Integer): TControl; override;
    function GetControlCount: Integer; override;
    procedure ShowCtrls; override;
  public
    property ChildA: TControl read FChildA;
    property ChildB: TControl read FChildB;
    property Position: Integer read GetPosition write SetPosition;
  published
  end;

  THPaned = class(TPaned)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

  TVPaned = class(TPaned)
  private
  protected
    procedure CreateHandle; override;
  public
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TPaned }

procedure TPaned.AddControl(AControl: TControl);
begin
  if AControl <> nil then
    if FChildA = nil then
    begin
      gtk_paned_add1(Handle, AControl.Handle);
      FChildA := AControl;
      AControl.FParent := Self;
      AControl.FreeNotification(Self);
    end
    else if FChildB = nil then
    begin
      gtk_paned_add2(Handle, AControl.Handle);
      FChildB := AControl;
      AControl.FParent := Self;
      AControl.FreeNotification(Self);
    end
    else
      raise Exception.CreateFmt('%s can hold only two controls.', [ClassName]);
end;

procedure TPaned.RemoveControl(AControl: TControl);
begin
  if (AControl.Parent = Self) then
  begin
    if (AControl = FChildA) then
    begin
      gtk_container_remove(Handle, AControl.Handle);
      FChildA := nil;
      AControl.FParent := nil;
    end
    else if (AControl = FChildB) then
    begin
      gtk_container_remove(Handle, AControl.Handle);
      FChildB := nil;
      AControl.FParent := nil;
    end;
  end
  else
    raise Exception.Create('Can''t remove a control that does not belong to this control.');
end;

function TPaned.GetControlByIndex(AIndex: Integer): TControl;
begin
  case AIndex of
  0:
    if (FChildA <> nil) then
      Result := FChildA
    else if (FChildB <> nil) then
      Result := FChildB
    else
      raise Exception.CreateFmt('Item index out of bounds. (%d)', [AIndex]);
  1:
    if (FChildA <> nil) and (FChildB <> nil) then
      Result := FChildB
    else
      raise Exception.CreateFmt('Item index out of bounds. (%d)', [AIndex]);
  else
    raise Exception.CreateFmt('Item index out of bounds. (%d)', [AIndex]);
  end;
end;

function TPaned.GetControlCount: Integer;
begin
  if (FChildA = nil) and (FChildB = nil) then
    Result := 0
  else if (FChildA = nil) or (FChildB = nil) then
    Result := 1
  else
    Result := 2;
end;

procedure TPaned.ShowCtrls;
begin
  if FChildA <> nil then
    FChildA.ShowCtrls;
  if FChildB <> nil then
    FChildB.ShowCtrls;
  //---
  inherited;
end;

function TPaned.GetPosition: Integer;
begin
  Result := gtk_paned_get_position(Handle);
end;

procedure TPaned.SetPosition(AValue: Integer);
begin
  gtk_paned_set_position(Handle, AValue);
end;

{ THPaned }

procedure THPaned.CreateHandle;
begin
  Handle := gtk_hpaned_new;
end;

{ TVPaned }

procedure TVPaned.CreateHandle;
begin
  Handle := gtk_vpaned_new;
end;

{$ENDIF}
