(*
   XDE - XCL's Development Environment
   Copyright (C) 2005-2006 Judison Oliveira Gil Filho <judison@gmail.com>

   See the file COPYING, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit regcomps;

{$H+}
{$MODE ObjFpc}

{$IFNDEF WIN32}
  {$DEFINE USE_PQ_CONN}
{$ENDIF}

interface

uses Classes;

type
  TComponentPage = class(TCollectionItem)
  public
    Name: String;
    Classes: TList;
    destructor Destroy; override;
  end;

var
  ComponentPages: TCollection;

procedure DoRegComps;

implementation

uses
  XCL,
  DB,
  interbase,
  memds,
  dbf,
//  mysqldb4,
  sqldb, ibconnection, {$IFDEF USE_PQ_CONN}pqconnection,{$ENDIF} mysql4conn,
  XCLDB,
  sdfdata;

destructor TComponentPage.Destroy;
begin
  Classes.Free;
  inherited Destroy;
end;

procedure RegisterComp(const APage: string; AComponentClasses: array of TComponentClass);
var
  i: Integer;
  pg: TComponentPage;
begin
  if APage = '' then
    exit;  { prevent caller from doing nonsense }

  pg := nil;
  for i := 0 to ComponentPages.Count - 1 do
    if TComponentPage(ComponentPages.Items[i]).Name = APage then
    begin
      pg := TComponentPage(ComponentPages.Items[i]);
      break;
    end;

  if pg = nil then
  begin
    pg := TComponentPage(ComponentPages.Add);
    pg.Classes := TList.Create;
    pg.Name := APage;
  end;

  for i := Low(AComponentClasses) to High(AComponentClasses) do
  begin
    pg.Classes.Add(AComponentClasses[i]);
    RegisterClass(AComponentClasses[i]);
    //WriteLn(APage,' \ ', AComponentClasses[i].ClassName);
  end;
end;

procedure DB_Register;
begin
  RegisterComponents('Data Access', [TDataSource]);
end;

procedure DBF_Register;
begin
  RegisterComponents('Data Access', [TDBF]);
end;

procedure Interbase_Register;
begin
  RegisterComponents('Data Access', [TIBDataBase, TIBTransaction, TIBQuery, TIBStoredProc]);
end;

procedure Memds_Register;
begin
  RegisterComponents('Data Access', [TMemDataset]);
end;

//procedure mysql4_Register;
//begin
//  RegisterComponents('MySQL', [TMySQLDatabase, TMySQLDataset]);
//end;

procedure sqldb_Register;
begin
  RegisterComponents('Data Access', [TSQLTransaction, TSQLQuery, TIBConnection, {$IFDEF USE_PQ_CONN}TPQConnection,{$ENDIF} TMySQlConnection]);
end;

procedure DoRegComps;
begin
  RegisterComponentsProc := @RegisterComp;
  XCL.Register;
  DB_Register;
  sqldb_Register;
  XCLDB.Register;
  Interbase_Register;
  Memds_Register;
  DBF_Register;
  //Mysql4_Register;
  
  sdfdata.Register;
end;

initialization
  ComponentPages := TCollection.Create(TComponentPage);
finalization
  ComponentPages.Free;
end.
