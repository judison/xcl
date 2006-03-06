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

  TBox = class(TContainerControl)
  private
    function GetHomogeneous: Boolean;
    function GetSpacing: Integer;
    procedure SetHomogeneous(Value: Boolean);
    procedure SetSpacing(Value: Integer);
  protected
  public
    procedure ReorderControl(AChildControl: TControl; APos: Integer); override;
  published
    property Homogeneous: Boolean read GetHomogeneous write SetHomogeneous;
    property Spacing: Integer read GetSpacing write SetSpacing;
  end;

  THBox = class(TBox)
  protected
    procedure CreateHandle; override;
  end;

  TVBox = class(TBox)
  protected
    procedure CreateHandle; override;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TBox }

procedure TBox.ReorderControl(AChildControl: TControl; APos: Integer);
begin
  inherited;
  gtk_container_child_set(Handle, AChildControl.Handle, 'position', [APos, 0])
end;

function TBox.GetHomogeneous: Boolean;
begin
  Result := gtk_box_get_homogeneous(Handle);
end;

function TBox.GetSpacing: Integer;
begin
  Result := gtk_box_get_spacing(Handle);
end;

procedure TBox.SetHomogeneous(Value: Boolean);
begin
  gtk_box_set_homogeneous(Handle, Value);
end;

procedure TBox.SetSpacing(Value: Integer);
begin
  gtk_box_set_spacing(Handle, Value);
end;

{ THBox }

procedure THBox.CreateHandle;
begin
  Handle := gtk_hbox_new(False, 0);
end;

{ TVBox }

procedure TVBox.CreateHandle;
begin
  Handle := gtk_vbox_new(False, 0);
end;

{$ENDIF}
