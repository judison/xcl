(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)
unit BufferList;

{$H+}
{$IFDEF FPC}
  {$MODE ObjFpc}
{$ENDIF}

interface

uses Classes, SysUtils, Buffer;

type
  TBufferList = class(TComponent)
  private
    FList: TList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //--
    procedure Add(ABuffer: TBuffer);
    procedure Remove(ABuffer: TBuffer);
    //--
    function GetBuffer(AIdx: Integer): TBuffer;
    function Count: Integer;
    function GetByFileName(AFileName: String): TBuffer;
    //--
    property Buffer[AIdx: Integer]: TBuffer read GetBuffer; default;
  end;

implementation

{ TBufferList }

constructor TBufferList.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
end;

destructor TBufferList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TBufferList.Add(ABuffer: TBuffer);
begin
  FList.Add(ABuffer);
end;

procedure TBufferList.Remove(ABuffer: TBuffer);
begin
  FList.Remove(ABuffer);
end;

function TBufferList.GetBuffer(AIdx: Integer): TBuffer;
begin
  Result := TBuffer(FList[AIdx]);
end;

function TBufferList.Count: Integer;
begin
  Result := FList.Count;
end;

function TBufferList.GetByFileName(AFileName: String): TBuffer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FList.Count -1 do
    if TBuffer(FList[I]).FileName = AFileName then
      exit(TBuffer(FList[I]));
end;

end.