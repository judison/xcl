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

  TPaned = class(TContainerControl)
  private
    FChildA: TControl;
    FChildB: TControl;
  protected
    procedure AddControl(AControl: TControl); override;
    procedure RemoveControl(AControl: TControl); override;
    function GetControlByIndex(AIndex: Integer): TControl; override;
    function GetControlCount: Integer; override;
    procedure ShowCtrls; override;
  public
    property ChildA: TControl read FChildA;
    property ChildB: TControl read FChildB;
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

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

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
