(*
   $Id: tscrolledwindow.inc,v 1.4 2005/12/17 21:50:20 judison Exp $

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

  TScrolledWindow = class(TBinControl)
  private
    FVAdjustment: TAdjustment;
    FHAdjustment: TAdjustment;
    function GetShadowType: TShadowType;
    procedure SetShadowType(AValue: TShadowType);
    function GetPolicy(AIdx: Integer): TScrollBarPolicy;
    procedure SetPolicy(AIdx: Integer; AValue: TScrollBarPolicy);
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    destructor Destroy; override;
    //--
    property VAdjustment: TAdjustment read FVAdjustment;
    property HAdjustment: TAdjustment read FHAdjustment;
  published
    property ShadowType: TShadowType read GetShadowType write SetShadowType default stNone;
    property HPolicy: TScrollBarPolicy index 0 read GetPolicy write SetPolicy default sbpAlways;
    property VPolicy: TScrollBarPolicy index 1 read GetPolicy write SetPolicy default sbpAlways;
  end;

{$ENDIF}
{$IFDEF INC_READ_IMPLEMENTATION}
//** IMPLEMENTATION
//****************************************************************************

{ TScrolledWindow }

destructor TScrolledWindow.Destroy;
begin
  inherited;
end;

procedure TScrolledWindow.CreateHandle;
begin
  FHAdjustment := TAdjustment.Create(nil);
  FVAdjustment := TAdjustment.Create(nil);
  Handle := gtk_scrolled_window_new(FHAdjustment.Handle, FVAdjustment.Handle);
end;

procedure TScrolledWindow.DestroyHandle;
begin
  inherited;
  // devem ser destruidos depois pois sao criadas antes do handle
  FHAdjustment.Free;
  FVAdjustment.Free;
end;

function TScrolledWindow.GetShadowType: TShadowType;
begin
  Result := TShadowType(gtk_scrolled_window_get_shadow_type(Handle));
end;

procedure TScrolledWindow.SetShadowType(AValue: TShadowType);
begin
  gtk_scrolled_window_set_shadow_type(Handle, Ord(AValue));
end;

function TScrolledWindow.GetPolicy(AIdx: Integer): TScrollBarPolicy;
var
  HP, VP: TScrollBarPolicy;
begin
  gtk_scrolled_window_get_policy(Handle, @HP, @VP);
  if AIdx = 0 then
    Result := HP
  else
    Result := VP;
end;

procedure TScrolledWindow.SetPolicy(AIdx: Integer; AValue: TScrollBarPolicy);
var
  HP, VP: TScrollBarPolicy;
begin
  gtk_scrolled_window_get_policy(Handle, @HP, @VP);
  if (AIdx = 0) and (HP <> AValue) then
    gtk_scrolled_window_set_policy(Handle, LongInt(AValue), LongInt(VP))
  else if (AIdx = 1) and (VP <> AValue) then
    gtk_scrolled_window_set_policy(Handle, LongInt(HP), LongInt(AValue));
end;

{$ENDIF}
{
$Log: tscrolledwindow.inc,v $
Revision 1.4  2005/12/17 21:50:20  judison
*** empty log message ***

Revision 1.3  2005/12/16 02:50:45  judison
Added TScrolledWindow.HPolicy and .VPolicy

Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}