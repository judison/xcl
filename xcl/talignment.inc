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

  TAlignment = class(TBinControl)
  private
    FXAlign: Single;
    FYAlign: Single;
    FXScale: Boolean;
    FYScale: Boolean;
    procedure SetXAlign(AValue: Single);
    procedure SetYAlign(AValue: Single);
    procedure SetXScale(AValue: Boolean);
    procedure SetYScale(AValue: Boolean);
    procedure SetAll; inline;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetAll(AXAlign, AYAlign: Single; AXScale, AYScale: Boolean);
  published
    property XAlign: Single read FXAlign write SetXAlign default 0.5;
    property YAlign: Single read FYAlign write SetYAlign default 0.5;
    property XScale: Boolean read FXScale write SetXScale default True;
    property YScale: Boolean read FYScale write SetYScale default True;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TAlignment }

constructor TAlignment.Create(AOwner: TComponent);
begin
  FXAlign := 0.5;
  FYAlign := 0.5;
  FXScale := True;
  FYScale := True;
  inherited;
end;

procedure TAlignment.CreateHandle;
var
  LXScale, LYScale: Single;
begin
  if FXScale then
    LXScale := 1
  else
    LXScale := 0;

  if FYScale then
    LYScale := 1
  else
    LYScale := 0;

  Handle := gtk_alignment_new(FXAlign, FYAlign, LXScale, LYScale);
end;

procedure TAlignment.SetXAlign(AValue: Single);
begin
  if FXAlign <> AValue then
  begin
    FXAlign := AValue;
    SetAll;
  end;
end;

procedure TAlignment.SetYAlign(AValue: Single);
begin
  if FYAlign <> AValue then
  begin
    FYAlign := AValue;
    SetAll;
  end;
end;

procedure TAlignment.SetXScale(AValue: Boolean);
begin
  if FXScale <> AValue then
  begin
    FXScale := AValue;
    SetAll;
  end;
end;

procedure TAlignment.SetYScale(AValue: Boolean);
begin
  if FYScale <> AValue then
  begin
    FYScale := AValue;
    SetAll;
  end;
end;

procedure TAlignment.SetAll; inline;
var
  LXScale, LYScale: Single;
begin
  if FXScale then
    LXScale := 1
  else
    LXScale := 0;

  if FYScale then
    LYScale := 1
  else
    LYScale := 0;

  gtk_alignment_set(Handle, FXAlign, FYAlign, LXScale, LYScale);
end;

procedure TAlignment.SetAll(AXAlign, AYAlign: Single; AXScale, AYScale: Boolean);
begin
  FXAlign := AXAlign;
  FYAlign := AYAlign;
  FXScale := AXScale;
  FYScale := AYScale;
  SetAll;
end;

{$ENDIF}
