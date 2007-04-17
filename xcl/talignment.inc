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
    FXScale: Single;
    FYScale: Single;
    procedure SetXAlign(AValue: Single);
    procedure SetYAlign(AValue: Single);
    procedure SetXScale(AValue: Single);
    procedure SetYScale(AValue: Single);
    procedure SetAll; inline;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetAll(AXAlign, AYAlign, AXScale, AYScale: Single);
  published
    property XAlign: Single read FXAlign write SetXAlign default 0.5;
    property YAlign: Single read FYAlign write SetYAlign default 0.5;
    property XScale: Single read FXScale write SetXScale default 1;
    property YScale: Single read FYScale write SetYScale default 1;
  end;

{** IMPLEMENTATION *******************}{$ENDIF}{$IFDEF INC_READ_IMPLEMENTATION}

{ TAlignment }

constructor TAlignment.Create(AOwner: TComponent);
begin
  FXAlign := 0.5;
  FYAlign := 0.5;
  FXScale := 1;
  FYScale := 1;
  inherited;
end;

procedure TAlignment.CreateHandle;
begin
  Handle := gtk_alignment_new(FXAlign, FYAlign, FXScale, FYScale);
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

procedure TAlignment.SetXScale(AValue: Single);
begin
  if FXScale <> AValue then
  begin
    FXScale := AValue;
    SetAll;
  end;
end;

procedure TAlignment.SetYScale(AValue: Single);
begin
  if FYScale <> AValue then
  begin
    FYScale := AValue;
    SetAll;
  end;
end;

procedure TAlignment.SetAll; inline;
begin
  gtk_alignment_set(Handle, FXAlign, FYAlign, FXScale, FYScale);
end;

procedure TAlignment.SetAll(AXAlign, AYAlign, AXScale, AYScale: Single);
begin
  FXAlign := AXAlign;
  FYAlign := AYAlign;
  FXScale := AXScale;
  FYScale := AYScale;
  SetAll;
end;

{$ENDIF}
