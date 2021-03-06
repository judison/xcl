(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit Buffer;

{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}
{$H+}

interface

uses Classes, SysUtils, XCL, ExCtrls;

type
  TBuffer = class(TNotebookPageEx)
  private
    // Properties
    FModified: Boolean;
    FFileName: String;
    procedure SetFileName(AValue: String);
    procedure SetModified(AValue: Boolean);
    // Non Visual
    procedure UpdateCaption;
    //--
  protected
    procedure CloseBtnClicked(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //--
    procedure New; virtual;
    procedure Open(AFileName: String); virtual;
    procedure SaveAs(AFileName: String); virtual;
    procedure Save; virtual;
    function Close: Boolean; virtual;
    // Properties
    property Modified: Boolean read FModified write SetModified;
    property FileName: String read FFileName write SetFileName;
  end;

implementation

{ TBuffer }

constructor TBuffer.Create(AOwner: TComponent);
begin
  inherited;
  UseUnderline := False;
  FLabel.UseMarkup := True;
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

function TBuffer.Close: Boolean;
var
  R: TMessageResponse;
begin
  Result := True;
  if Modified then
  begin
    R := Application.ShowMessage(mtQuestion, [mbYes, mbNo, mbCancel], 'Save Changes?', 'File is modified, save changes?', FileName);
    case R of
      mrYes:
      begin
        Save;
        Free;
      end;
      mrNo: Free;
      mrCancel: Result := False;
    end;
  end
  else
    Free;
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

procedure TBuffer.CloseBtnClicked(Sender: TObject);
begin
  Close;
end;

procedure TBuffer.UpdateCaption;
var
  lC: String;
begin
  lC := ExtractFileName(FFileName);
  if FModified then
  begin
    IconName := 'gtk-edit';
    Caption := '<i>'+lC+'</i>'
  end
  else
  begin
    IconName := 'gtk-file';
    Caption := lC;
  end;
end;

end.
