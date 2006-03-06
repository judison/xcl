(*
   $Id: tarrow.inc,v 1.2 2005/03/26 05:21:47 judison Exp $

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

  TArrow = class(TMiscControl)
  private
    FArrowType: TArrowType;
    FShadowType: TShadowType;
    procedure SetArrowType(AValue: TArrowType);
    procedure SetShadowType(AValue: TShadowType);
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ArrowType: TArrowType read FArrowType write SetArrowType stored True;
    property ShadowType: TShadowType read FShadowType write SetShadowType default stOut;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TArrow }

constructor TArrow.Create(AOwner: TComponent);
begin
  FArrowType := atUp;
  FShadowType := stOut;
  inherited;
end;

procedure TArrow.CreateHandle;
begin
  Handle := gtk_arrow_new(Ord(FArrowType), Ord(FShadowType));
end;

procedure TArrow.SetArrowType(AValue: TArrowType);
begin
  if FArrowType <> AValue then
  begin
    FArrowType := AValue;
    gtk_arrow_set(Handle, Ord(FArrowType), Ord(FShadowType));
  end;
end;

procedure TArrow.SetShadowType(AValue: TShadowType);
begin
  if FShadowType <> AValue then
  begin
    FShadowType := AValue;
    gtk_arrow_set(Handle, Ord(FArrowType), Ord(FShadowType));
  end;
end;

{$ENDIF}
{
$Log: tarrow.inc,v $
Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}