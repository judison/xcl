(*
  $Id: xrc.pas,v 1.4 2006/03/01 18:32:40 judison Exp $

  This source is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This code is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A copy of the GNU General Public License is available on the World
  Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also
  obtain it by writing to the Free Software Foundation Inc.,
  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  **********************************************************************
  ATTENTION:
  **********
  This tool is based on LazRes by Mattias Gaertner, part of The Lazarus
  Project, so, Mattias Gaertner is the original author.

  The Lazarus Project: http://lazarus.freepascal.org/

                                              Judison Oliveira Gil Filho
                                         <judison@users.sourceforge.net>
  **********************************************************************
*)

program xrc;

{$MODE ObjFpc}
{$H+}

uses Classes, SysUtils, glib2, {gdk2,} gdk2pixbuf;

//gdkpixbuflib


type
  PGdkPixdata = ^TGdkPixdata;
  TGdkPixdata = record
    magic: guint32;        // GDK_PIXBUF_MAGIC_NUMBER
    length: gint32;        // <1 to disable length checks, otherwise:
			   // GDK_PIXDATA_HEADER_LENGTH + pixel_data length
    pixdata_type: guint32; // GdkPixdataType
    rowstride: guint32;
    width: guint32;
    height: guint32;
    pixel_data: PByte;
  end;

function gdk_pixdata_from_pixbuf(pixdata: PGdkPixdata; const pixbuf: PGdkPixbuf; use_rle: gboolean): pointer; cdecl; external gdkpixbuflib;
function gdk_pixdata_serialize(const pixdata: PGdkPixdata; stream_length_p: pguint): PByte; cdecl; external gdkpixbuflib;
function gdk_pixdata_deserialize(pixdata: PGdkPixdata; stream_length: guint; const stream: PByte; error: pointer): gboolean; cdecl; external gdkpixbuflib;

type
  TMStream = class(TCustomMemoryStream)
  public
    constructor Create(AData: Pointer; ASize: LongWord);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

constructor TMStream.Create(AData: Pointer; ASize: LongWord);
begin
  inherited Create;
  SetPointer(AData, ASize);
end;

function TMStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  raise EStreamError.Create('Can''t write to a TMStream.');
end;

function BinToXRC(Input, Output:TStream; const RType, RName: String): LongWord;
var
  x: integer;
  b: byte;

  procedure WriteString(S: String);
  begin
    Output.WriteBuffer(Pointer(S)^, Length(S));
  end;

begin
  Result := Input.Size;
  WriteString('  '+RType+'_'+RName+': array[0..'+IntToStr(Result-1)+'] of byte = ('+LineEnding+'    ');
  x := 4;
  while Input.Read(b, 1) = 1 do
  begin
    if x > 75 then
    begin
      WriteString(','+LineEnding+'    ');
      x := 4;
    end;
    if X > 4 then
      WriteString(Format(',%.3d', [b]))
    else
      WriteString(Format('%.3d', [b]));
    inc(x,4);
  end;
  WriteString(');'+LineEnding+LineEnding);
end;

function FrmClassName(AStream:TStream):ansistring;
var
  c: char;
  StartPos, EndPos: int64;
begin
  Result:='';
  StartPos:=-1;
  c:=' ';
  // read till end of line
  repeat
    // remember last non identifier char position
    if (not (c in ['a'..'z','A'..'Z','0'..'9','_'])) then
      StartPos:=AStream.Position;
    if AStream.Read(c,1)<>1 then exit;
    if AStream.Position>1000 then exit;
  until c in [#10,#13];
  if StartPos<0 then exit;
  EndPos:=AStream.Position-1;
  if EndPos-StartPos>255 then exit;
  SetLength(Result,EndPos-StartPos);
  AStream.Position:=StartPos;
  AStream.Read(Result[1],length(Result));
  AStream.Position:=0;
  if (Result='') or (not IsValidIdent(Result)) then
    Result:='';
end;

procedure Usage;
begin
  WriteLn('Usage: ',ExtractFileName(ParamStr(0)),' <input files> <output file>');
  exit;
end;

procedure Error(Msg: String);
begin
  WriteLn('ERROR: ', Msg);
  Halt(1);
end;

var
  Reg: TStringList;

procedure ProcessFile(AStream: TStream; AFN: String);
var
  FileStream, MemStream, BStream: TStream;
  Ext: String;
  RType, RName: String;
  S: LongWord;
  pixbuf: pointer;
  pixdata: TGdkPixdata;
  RawData: PByte;
  RawDataSize: guint;
begin
  Write(Format('*** %-30s ', [AFN]));
  try
    FileStream := TFileStream.Create(AFN, fmOpenRead);
    MemStream := TMemoryStream.Create;
    try
      MemStream.CopyFrom(FileStream, FileStream.Size);
      MemStream.Position := 0;
      Ext := UpperCase(ExtractFileExt(AFN));
      if (Ext='.FRM') then
      begin
        RType := 'FORMDATA';
        RName := FrmClassName(MemStream);
        WriteLn('OK: ', RType, '\', RName);

        BStream := TMemoryStream.Create;
        try
          ObjectTextToBinary(MemStream, BStream);
          BStream.Position := 0;
          S := BinToXRC(BStream, AStream, RType, RName);
        finally
          BStream.Free;
        end;

        //BinToXRC(MemStream, AStream, RType, RName);
      end
      // Pegar essa lista em runtime, com query modules e bla bla....
      else if (Ext='.PNG') or (Ext='.JPEG') or (Ext='.JPE') or (Ext='.JPG') or
              (Ext='.GIF') or (Ext='.ICO') or (Ext='.CUR') or (Ext='.ANI') or
              (Ext='.RAS') or (Ext='.XPM') or (Ext='.PNM') or (Ext='.PBM') or
              (Ext='.PGM') or (Ext='.PPM') or (Ext='.BMP') or (Ext='.WBMP') or
              (Ext='.XBM') or (Ext='.TGA') or (Ext='.TARGA') or (Ext='.PCX') or
              (Ext='.SVG') or (Ext='.SVGZ') or (Ext='.TIFF') or (Ext='.TIF') then
      begin
        RType := 'PIXDATA';
        RName := ExtractFileName(AFN);
        RName := Copy(RName, 1, Length(RName) - Length(Ext));
        WriteLn('OK: ', RType, '\', RName);

        pixbuf := gdk_pixbuf_new_from_file(PChar(AFN), nil);
        gdk_pixdata_from_pixbuf(@pixdata, pixbuf, false);

        RawData := gdk_pixdata_serialize(@pixdata, @RawDataSize);

        BStream := TMStream.Create(RawData, RawDataSize);
        try
          S := BinToXRC(BStream, AStream, RType, RName);
        finally
          BStream.Free;
        end;
        g_free(RawData);

        g_object_unref(pixbuf);
      end
      else
      begin
        RType:=Copy(Ext, 2, Length(Ext)-1);
        RName:=ExtractFileName(AFN);
        RName:=Copy(RName, 1, Length(RName) - Length(Ext));
        if RName = '' then
          Error('No Resource Name.');
        if RType = '' then
          Error('No Resource Type.');

        WriteLn('OK: ', RType, '\', RName);
        S := BinToXRC(MemStream, AStream, RType, RName);
      end;
      Reg.Add(Format('  AddResource(''%s'', ''%s'', %s_%s, %d);',[RType,RName,RType,RName,S]));
    finally
      FileStream.Free;
      MemStream.Free;
    end;
  except
    Error('Unable to read file "'+AFN+'".');
  end;
end;

var
  OutFilename: String;
  I: Integer;
  FileStream, MemStream: TStream;
  procedure WriteString(S: String);
  begin
    MemStream.WriteBuffer(Pointer(S)^, Length(S));
  end;
begin
  if ParamCount < 2 then
    Usage;

  for I := 1 to ParamCount-1 do
  begin
    if not FileExists(ParamStr(I)) then
      Error('File not found: '+ParamStr(I));
    if ExpandFileName(ParamStr(I)) = ExpandFileName(ParamStr(ParamCount)) then
      Error('Output file may not be one of input files.');
  end;

  OutFilename := ParamStr(ParamCount);
  try
    FileStream := TFileStream.Create(OutFilename, fmCreate);
  except
    Error('Unable to create file "'+OutFilename+'".');
  end;

  //------
  g_type_init (); // initialize glib/GdkPixbuf


  Reg := TStringList.Create;

  MemStream := TMemoryStream.Create;
  try
    WriteString('(* This file is autogenerated by xrc tool.      *)'+LineEnding);
    WriteString('(* Any modifications to this file will be lost. *)'+LineEnding+LineEnding);
    WriteString('unit '+ChangeFileExt(OutfileName,'')+';'+LineEnding+LineEnding);
    WriteString('interface'+LineEnding+LineEnding);
    WriteString('const'+LineEnding);

    for I := 1 to ParamCount-1 do
      ProcessFile(MemStream, ParamStr(I));

    WriteString('implementation'+LineEnding+LineEnding);
    WriteString('uses xcl;'+LineEnding+LineEnding);
    WriteString('initialization'+LineEnding);

    for I := 0 to Reg.Count-1 do
      WriteString(Reg.Strings[I]+LineEnding);

    WriteString('end.'+LineEnding);

    MemStream.Position:=0;
    FileStream.CopyFrom(MemStream,MemStream.Size);
  finally
    Reg.Free;
    MemStream.Free;
    FileStream.Free;
  end;
end.
