#!/usr/bin/python

import string
import re

words = {
    'KeyWords' : 'Keywords',
    'BackSpace' : 'Backspace',
    'UnIndents' : 'Unindents',
}

dont_render = []

sci_const = []
sci_defs = []
sci_impl = []

def fix_name(name):
    str = ''

    for word in words.keys():
        if string.find(name, word) >= 0:
            name = string.replace(name, word, words[word])

    for i in range(len(name)):
        if i == 0:
            str = string.upper(name[i])
        else:
            str = str + name[i]
    if name == 's':
      str = 'Str'
    return str

def write_fun(dict):
    def_full = '    %(procfun)s %(fun_name)s(%(first_name)s: %(first_type)s; %(second_name)s: %(second_type)s)%(ret_def)s;'
    def_first = '    %(procfun)s %(fun_name)s(%(first_name)s: %(first_type)s)%(ret_def)s;'
    def_second = '    %(procfun)s %(fun_name)s(%(second_name)s: %(second_type)s)%(ret_def)s;'
    def_none = '    %(procfun)s %(fun_name)s()%(ret_def)s;'

    body_full = '''
%(procfun)s TScintilla.%(fun_name)s(%(first_name)s: %(first_type)s; %(second_name)s: %(second_type)s)%(ret_def)s;
begin
  %(return)sscintilla_send_message(Handle, %(msg_num)s, %(first_cast)s, %(second_cast)s)%(ret_cast)s;
end;'''
    body_first = '''
%(procfun)s TScintilla.%(fun_name)s(%(first_name)s: %(first_type)s)%(ret_def)s;
begin
  %(return)sscintilla_send_message(Handle, %(msg_num)s, %(first_cast)s, 0)%(ret_cast)s;
end;'''
    body_second = '''
%(procfun)s TScintilla.%(fun_name)s(%(second_name)s: %(second_type)s)%(ret_def)s;
begin
  %(return)sscintilla_send_message(Handle, %(msg_num)s, 0, %(second_cast)s)%(ret_cast)s;
end;'''
    body_none = '''
%(procfun)s TScintilla.%(fun_name)s()%(ret_def)s;
begin
  %(return)sscintilla_send_message(Handle, %(msg_num)s, 0, 0)%(ret_cast)s;
end;'''
    
    dict['fun_name'] = fix_name(dict['fun_name'])
    if dict['fun_name'] in dont_render:
        return



    # Nome dos Parametros
    if dict['first_name'] is not None:
        dict['first_name'] = 'A'+fix_name(dict['first_name'])
    if dict['second_name'] is not None:
        dict['second_name'] = 'A'+fix_name(dict['second_name'])

    # Cast do Primeiro Parametro
    if dict['first_type'] is None:
        pass
    elif dict['first_type'] == 'string':
        dict['first_cast'] = 'DWord(PChar('+dict['first_name']+'))'
    else:
        dict['first_cast'] = 'DWord('+dict['first_name']+')'

    # Cast do Segundo Parametro
    if dict['second_type'] is None:
        pass
    elif dict['second_type'] == 'string':
        dict['second_cast'] = 'LongInt(PChar('+dict['second_name']+'))'
    else:
        dict['second_cast'] = 'LongInt('+dict['second_name']+')'


    for key in dict.keys():
        if string.find(key, 'type') > 0:
            if dict[key] == 'void':
                pass
            elif dict[key] == 'int':
                dict[key] = 'Integer' # SmallInt ?
            elif dict[key] == 'bool':
                dict[key] = 'Boolean'
            elif dict[key] == 'position':
                dict[key] = 'Integer'  #Integer
            elif dict[key] == 'colour':
                dict[key] = 'Integer'
            elif dict[key] == 'string':
                dict[key] = 'String'
            elif dict[key] == 'stringresult':
                dict[key] = 'PChar'
            elif dict[key] is None:
                pass
            else:
                print 'Function not wrapped: ' + dict['fun_name']
                return
    
    # Resultado de Function ou nada (Procedure)
    if dict['ret_type'] == 'void':
        dict['ret_def'] = ''
    else:
        dict['ret_def'] = ': ' + dict['ret_type'];


    if dict['ret_type'] != 'void':
        dict['procfun'] = 'function '
        dict['return'] = 'Result := '
    else:
        dict['procfun'] = 'procedure'
        dict['return'] = ''

    if dict['ret_type'] == 'Boolean':
        dict['ret_cast'] = ' <> 0'
    else:
        dict['ret_cast'] = ''
    
    if dict['first_type'] is None and dict['second_type'] is None:
        fun_def = def_none % dict
        fun_body = body_none % dict
    elif dict['second_type'] is None:
        fun_def = def_first % dict
        fun_body = body_first % dict
    elif dict['first_type'] is None:
        fun_def = def_second % dict
        fun_body = body_second % dict
    else:
        fun_def = def_full % dict
        fun_body = body_full % dict

    sci_defs.append(fun_def + '\n')
    sci_impl.append(fun_body + '\n')

def write_val(dict):
    val_template = '  %(name)s = %(value)s;'
    
    sci_const.append(val_template % dict + '\n')

def main():
    iface = open('Scintilla.iface', 'r')
    
    fun_re = '^(fun|get|set)\s+(?P<ret_type>\S+)\s+(?P<fun_name>\S+)=(?P<msg_num>\d+)?\(((?P<first_type>\w+)\s+(?P<first_name>\w+)\s*)?,(\s*(?P<second_type>\w+)\s+(?P<second_name>\w+))?\)'
    val_re = '^val\s+(?P<name>\w+)\s*=\s*(?P<value>.+)$'
    
    line = iface.readline()
    while line != '':
        if string.find(line, 'cat') == 0 and \
           string.split(string.strip(line))[1] == 'Deprecated':
            break
        
        if re.match(fun_re, line):
            write_fun(re.match(fun_re, line).groupdict())

        elif re.match(val_re, line):
            write_val(re.match(val_re, line).groupdict())
        
        line = iface.readline()
    
    iface.close()

    template = open('scintilla.tmpl').read()
    template = string.replace(template, '%const%', string.join(sci_const, ''))
    template = string.replace(template, '%defs%', string.join(sci_defs, ''))
    template = string.replace(template, '%impl%', string.join(sci_impl, ''))
    template = string.replace(template, '\r', '')
    template = string.replace(template, '0x', '$')
    open('scintilla.pas', 'w').write(template)

if __name__ == '__main__':
    main()
