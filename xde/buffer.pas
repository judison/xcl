(*
   $Id: buffer.pas,v 1.2 2005/12/18 05:05:59 judison Exp $

   XDE - XCL's Development Environment
   Copyright (C) 2005 Judison Oliveira Gil Filho

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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //--
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

destructor TBuffer.Destroy;
begin
  inherited;
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
{
$Log: buffer.pas,v $
Revision 1.2  2005/12/18 05:05:59  judison
copyleft stuff

Revision 1.1.1.1  2005/12/17 17:29:39  judison
Initial Import

Revision 1.4  2005/12/02 22:31:34  judison
Long-term changes (again) I need to use cvs more!

Revision 1.3  2005/11/18 20:06:58  judison
long-term changes...

Revision 1.2  2005/03/26 05:21:47  judison
+ CVS Log Tag

}
