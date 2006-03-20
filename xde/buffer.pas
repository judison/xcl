(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit buffer;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, xcl;

type
  TBuffer = class(TNotebookPage)
  private
    // Properties
    FModified: Boolean;
    FFileName: String;
    procedure SetFileName(AValue: String);
    procedure SetModified(AValue: Boolean);
    // Non Visual
    procedure UpdateCaption;
  protected
//    FCloseBtn: TButton;
    procedure CreateControls; override;
    procedure DestroyControls; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //--
    procedure New; virtual;
    procedure Open(AFileName: String); virtual;
    procedure SaveAs(AFileName: String); virtual;
    procedure Save; virtual;
    // Properties
    property Modified: Boolean read FModified write SetModified;
    property FileName: String read FFileName write SetFileName;
  end;

implementation

{ TBuffer }

constructor TBuffer.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TBuffer.CreateControls;
begin
  inherited;
{
  FCloseBtn := TButton.Create(Self);
  FCloseBtn.IconName := 'gtk-close';
  FCloseBtn.IconSize := iszMenu;
  FCloseBtn.Relief := rlfNone;
  FCloseBtn.WidthRequest := 16;
  FCloseBtn.HeightRequest := 16;
  FCloseBtn.Parent := FBox;
}
end;

procedure TBuffer.DestroyControls;
begin
{
  if Assigned(FCloseBtn) then
  begin
    try FBox.RemoveControl(FCloseBtn); except end;
    FCloseBtn.Free;
    FCloseBtn := nil;
  end;
}
  inherited;
end;

destructor TBuffer.Destroy;
begin
  inherited;
end;

procedure TBuffer.New;
begin
end;

procedure TBuffer.Open(AFileName: String);
begin
  FileName := AFileName;
  Modified := False;
end;

procedure TBuffer.SaveAs(AFileName: String);
begin
  FileName := AFileName;
  Save;
end;

procedure TBuffer.Save;
begin
  Modified := False;
end;

procedure TBuffer.SetFileName(AValue: String);
begin
  FFileName := AValue;
  UpdateCaption;
end;

procedure TBuffer.SetModified(AValue: Boolean);
begin
  if FModified <> AValue then
  begin
    FModified := AValue;
    UpdateCaption;
  end;
end;

procedure TBuffer.UpdateCaption;
var
  lC: String;
begin
  lC := ExtractFileName(FFileName);
  if FModified then
    Caption := lC + '*'
  else
    Caption := lC;
end;

end.
