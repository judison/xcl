(*
   GtkPas - Code generation tool
   Copyright (C) 2006 Judison Oliveira Gil Filho

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

program gtkpas;

{$MODE OBJFPC}
{$H+}

uses Classes, SysUtils;

// gtkpas <gtk_prefix> <gtk_name> <type> <xcl_name> [rw] [default]

var
  gtk_prefix: String;
  gtk_name: String;
  a_type: String;
  xcl_name: String;
  xcl_type: String;
  gtk_type: char;
begin
  gtk_prefix := ParamStr(1);
  gtk_name := ParamStr(2);
  a_type := ParamStr(3);
  xcl_name := ParamStr(4);

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
    xcl_type := ParamStr(3);//a_type;
    gtk_type := 'o';
  end;

  WriteLn('  private');
  WriteLn('    function Get',xcl_name,': ', xcl_type,';');
  WriteLn('    procedure Set',xcl_name,'(AValue: ',xcl_type,');');
  WriteLn;
  WriteLn('  published');
  WriteLn('    property ',xcl_name,': ',xcl_type,' read Get',xcl_name,' write Set',xcl_name,';');
  WriteLn;
  WriteLn('function Get',xcl_name,': ',xcl_type,';');
  WriteLn('begin');
  case gtk_type of
    'n': WriteLn('  Result := ',gtk_prefix,'_get_',gtk_name,'(Handle);');
    's': WriteLn('  Result := ',gtk_prefix,'_get_',gtk_name,'(Handle);');
    'o': WriteLn('  Result := ',xcl_type,'(',gtk_prefix,'_get_',gtk_name,'(Handle));');
  end;
  WriteLn('end;');
  WriteLn;
  WriteLn('procedure Set',xcl_name,'(AValue: ',xcl_type,');');
  WriteLn('begin');
  case gtk_type of
    'n': WriteLn('  ',gtk_prefix,'_set_',gtk_name,'(Handle, AValue);');
    's': WriteLn('  ',gtk_prefix,'_set_',gtk_name,'(Handle, PChar(AValue));');
    'o': WriteLn('  ',gtk_prefix,'_set_',gtk_name,'(Handle, Ord(AValue));');
  end;
  WriteLn('end;');

end.
