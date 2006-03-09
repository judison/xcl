(*
   GtkPas - Code generation tool
   Copyright (C) 2006 Judison Oliveira Gil Filho

   See the file COPYING.XCL, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

program gtkpas;

{$MODE OBJFPC}
{$H+}

uses Classes, SysUtils;

var
  F: TStringList;
  inc_file: String;
  gtk_prefix: String;
  gtk_name: String;
  a_type: String;
  xcl_name: String;
  xcl_type: String;
  gtk_type: char;
  class_name: String;
  I: Integer;
begin
  if ParamCount < 5 then
  begin
    WriteLn('usage: ',ExtractFileName(paramstr(0)),' <inc_file> <gtk_prefix> <gtk_name> <type> <xcl_name> [rw] [default]');
    exit;
  end;
  inc_file := ParamStr(1);
  gtk_prefix := ParamStr(2);
  gtk_name := ParamStr(3);
  a_type := ParamStr(4);
  xcl_name := ParamStr(5);

  a_type := lowercase(a_type);

  if (a_type = 's') or (a_type = 'str') or (a_type = 'string') then
  begin
    xcl_type := 'String';
    gtk_type := 's';
  end
  else if (a_type = 'i') or (a_type = 'int') or (a_type = 'integer') then
  begin
    xcl_type := 'Integer';
    gtk_type := 'n';
  end
  else if (a_type = 'w') or (a_type = 'uint16') or (a_type = 'word') then
  begin
    xcl_type := 'Word';
    gtk_type := 'n';
  end
  else if (a_type = 'lw') or (a_type = 'uint32') or (a_type = 'longword') then
  begin
    xcl_type := 'LongWord';
    gtk_type := 'n';
  end
  else if (a_type = 'b') or (a_type = 'bol') or (a_type = 'bool') or (a_type = 'boolean') then
  begin
    xcl_type := 'Boolean';
    gtk_type := 'n';
  end
  else
  begin
    xcl_type := ParamStr(4);//a_type;
    gtk_type := 'o';
  end;

  F := TStringList.Create;
  F.LoadFromFile(inc_file);

  I := F.IndexOf('  private') - 1;
  class_name := Copy(F[I], 3, Pos('=', F[I])-4);

  I := F.IndexOf('  protected');

  F.Insert(I, '    function Get'+xcl_name+': '+xcl_type+';'); Inc(I);
  F.Insert(I, '    procedure Set'+xcl_name+'(AValue: '+xcl_type+');');

  I := F.IndexOf('  end;');

  F.Insert(I, '    property '+xcl_name+': '+xcl_type+' read Get'+xcl_name+' write Set'+xcl_name+';');

  I := F.Count -1;
  while F[I] <> '{$ENDIF}' do
    Dec(I);
  if F[I-1] = '' then
    Dec(I);

  F.Insert(I, ''); Inc(I);
  F.Insert(I, 'function '+class_name+'.Get'+xcl_name+': '+xcl_type+';'); Inc(I);
  F.Insert(I, 'begin'); Inc(I);
  case gtk_type of
    'n': F.Insert(I, '  Result := '+gtk_prefix+'_get_'+gtk_name+'(Handle);');
    's': F.Insert(I, '  Result := '+gtk_prefix+'_get_'+gtk_name+'(Handle);');
    'o': F.Insert(I, '  Result := '+xcl_type+'('+gtk_prefix+'_get_'+gtk_name+'(Handle));');
  end; Inc(I);
  F.Insert(I, 'end;'); Inc(I);
  F.Insert(I, ''); Inc(I);
  F.Insert(I, 'procedure '+class_name+'.Set'+xcl_name+'(AValue: '+xcl_type+');'); Inc(I);
  F.Insert(I, 'begin'); Inc(I);
  case gtk_type of
    'n': F.Insert(I, '  '+gtk_prefix+'_set_'+gtk_name+'(Handle, AValue);');
    's': F.Insert(I, '  '+gtk_prefix+'_set_'+gtk_name+'(Handle, PChar(AValue));');
    'o': F.Insert(I, '  '+gtk_prefix+'_set_'+gtk_name+'(Handle, Ord(AValue));');
  end; Inc(I);
  F.Insert(I, 'end;'); Inc(I);
  //
  F.SaveToFile(inc_file);
end.
