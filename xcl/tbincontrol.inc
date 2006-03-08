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

  TBinControl = class(TContainerControl)
  private
    FChild: TControl;
  protected
    procedure AddControl(AControl: TControl); override;
    procedure RemoveControl(AControl: TControl); override;
    function GetControlByIndex(AIndex: Integer): TControl; override;
    function GetControlCount: Integer; override;
    procedure ShowCtrls; override;
  public
    property Child: TControl read FChild;
  published
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TBinControl }

procedure TBinControl.AddControl(AControl: TControl);
begin
  if AControl <> nil then
    if FChild = nil then
    begin
      InternalAddControl(AControl);
      FChild := AControl;
      AControl.FParent := Self;
      AControl.FreeNotification(Self);
    end
    else
      raise Exception.CreateFmt('%s can hold only one control.', [ClassName]);
end;

procedure TBinControl.RemoveControl(AControl: TControl);
begin
  if AControl.Parent = Self then
  begin
    gtk_container_remove(Handle, AControl.Handle);
    FChild := nil;
    AControl.FParent := nil;
  end
  else
    raise Exception.Create('Can''t remove a control that does not belong to this control.');
end;

function TBinControl.GetControlByIndex(AIndex: Integer): TControl;
begin
  if (AIndex = 0) and (FChild <> nil) then
    Result := FChild
  else
    raise Exception.CreateFmt('Item index out of bounds. (%d)', [AIndex]);
end;

function TBinControl.GetControlCount: Integer;
begin
  if FChild = nil then
    Result := 0
  else
    Result := 1;
end;

procedure TBinControl.ShowCtrls;
begin
  if FChild <> nil then
  begin
    FChild.ShowCtrls;
  end;
  //---
  inherited;
end;

{$ENDIF}
