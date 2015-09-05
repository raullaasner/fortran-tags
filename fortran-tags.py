#!/usr/bin/env python

# Copyright (C) 2015 Raul Laasner
# This file is distributed under the terms of the GNU General Public
# License, see 'LICENSE' in the root directory of the present
# distribution or http://gnu.org/copyleft/gpl.txt .

# This script contains two main functions:
#  1) Generating the tags file. The scope and position of every
#     variable and procedure are determined. Once the initial tags
#     file has been generated, only minimal changes are made with
#     subsequent calls to this function.
#  2) Finding the current scope. The same function is called as for
#     generating the tags file except that finding the definitions
#     parts are turned off. It can be invoked by fortran-find-scope of
#     fortran-tags.el, which provides the buffer contents of a source
#     file up to the cursor position in the form of stdin and receives
#     the scope at that position after it has been determined.

# FORMAT OF THE TAGS FILE
#
# The first line is the version number, which is required for deleting
# and regenerating the tags file in case a newer version of
# Fortra-tags is used. The second line contains the paths of all the
# source files included in the tags file and is used mainly by
# fortran-find-proc-calls. The rest of the lines contain 6 or 7 words,
# which have the following meaning:
#
# 1 - Absolute path to the source file
# 2 - Scope level. Counts the colons in the scope, which has the
#     format :a:b: (scope level is 3 for :a:b:). As an exception,
#     module wide variables are defined to have the scope level of
#     0. This allows to distinguish them from the local variables of
#     an external procedure which would otherwise have the same
#     scope. The the latter are never visible to the outside, while
#     the converse is not true for module wide variables. This
#     definition of the scope level allows fortran-find-tag to easily
#     grep for the correct variables.
# 3 - Variable or procedure name
# 4 - Scope of the variable or procedure
# 5 - Line number
# 6 - Position of the definition on the line
# 7 - If present, marks the the number of lines corresponding to a
#     source file in the tags file, i.e., this many lines can be
#     copied to the new tags file if the source file is unmodified.

from sys import argv, stdin, stdout
from os import path, stat
from re import match, search
import argparse

VERSION="1.2.0"

def clean_string(l):
    """Removes strings and comments from the line.
    """
    # Since there are two delimiters for the character type which can
    # be nested, all combinations need to be considered.
    global lock # lock[0] (lock[1]) is True if the previous line
                # didn't have matching single (double) quotes
    char_symbol = ['\'', '"']
    str_length = len(l)
    # If x (x being ', ", or !) is not in l, we define its position to
    # be at str_length.
    pos_c  = l.find('!') if '!' in l else str_length
    pos_s = [0,0]
    for i in -1,0:
        pos_s[i] = l.find(char_symbol[i]) if char_symbol[i] in l else str_length
    for i in -1,0:
        if lock[i]:
            if pos_s[i] < str_length:
                lock[i] = False
                return clean_string(l[pos_s[i]+1:])
            else:
                return ""
    if pos_c == str_length and pos_s[0] == str_length and \
       pos_s[1] == str_length:
        return l
    if pos_c < pos_s[0] and pos_c < pos_s[1]:
        return l[0:pos_c]
    for i in -1,0:
        if pos_s[i] < pos_s[i+1]:
            l0 = l[pos_s[i]+1:]
            if char_symbol[i] in l0:
                return clean_string(l[0:pos_s[i]]+
                                    l[pos_s[i]+l0.find(char_symbol[i])+2:])
            else:
                lock[i] = True
                return l[:pos_s[i]]

def process_input(input_text, find_definitions, TAGS="", filepath=""):
    """Finds either all definitions and their scopes
    (find_definitions=True) or finds only the current scope
    (find_definitions=False).
    """
    global lock
    global scope        # Current scope
    # Whether the current line is a continuation of a
    cont_var = False    # 1) variable definition,
    cont_redef = False  # 2) redefinition,
    cont_func = False   # 3) function definition.
    in_type = False     # Whether we are currently inside a type
                        # definition. Variable definitions inside a
                        # type definition are ignored.
    scope_unused = True # True if there haven't been any definitions
                        # with the present scope so far.
    in_mod = False      # Wether we are currently inside a
                        # module. This helps to set the scope level of
                        # module wide variables to 0 as explained
                        # above.
    lock = [False, False]
    scope = ":"
    scope_count = 1
    line_nr = 0
    for line_raw in input_text:
        line_nr += 1
        if line_raw.lstrip().startswith('#'): continue
        line_raw = line_raw.lower()
        line = clean_string(line_raw).lstrip()
        if not line: continue
        if find_definitions and not in_type:
            # PART 1 - variables, redefinitions, operator overloading
            m = match('((real|integer|logical|character|complex|class|' +
                      'enumerator|external)[ ,([:]|type *\()', line)
            if (m and not ' function ' in line) or cont_var:
                # In the following, 'names' contains all the variables
                # found on the current line.
                if cont_var:
                    for symbol in ('&', ','):
                        # Cannot use startswith(('&', ',')) here!
                        if line.startswith(symbol): line = line[1:].lstrip()
                    if line.startswith(('/', '*', '-', '+')):
                        cont_var = False
                        continue
                    names = line.split(',')
                elif '::' in line:
                    names = line[line.find('::')+2:].split(',')
                else:
                    # Any of the following has happened at this point:
                    # 1) old style was used (no :: in the line); 2) a
                    # keyword was used as a variable; 3) a keyword was
                    # used as a built-in function (e.g. real(1,8)). In
                    # any case, this is slow.
                    if m.group()[-1] == '(':
                        line = line[m.end()-1:].lstrip()
                    else:
                        line = line[m.end():].lstrip()
                    if line.startswith('('):
                        shift = 1
                        while shift < len(line) and \
                              line[:shift].count('(') \
                              != line[:shift].count(')'):
                            shift += 1
                        if shift == len(line): continue
                        line = line[shift:].lstrip()
                        if not line: continue
                    if line.startswith(('=', '/', '.', '*', \
                                        '-', '+', ')', ']')):
                        continue
                    names = line.split(',')
                i = 0
                while i < len(names):
                    # Since the names are separated by splitting the
                    # line at commas, special care needs to be taken
                    # against multi-dimensional arrays and coarrays.
                    name = names[i]
                    while name.count('(') > name.count(')') or \
                          name.count('[') > name.count(']'):
                        i += 1
                        if i == len(names): break
                        name += ',' + names[i] # The name is pieced
                                               # back together
                    for symbol in ('(', '=', '[', '*'):
                        if symbol in name:
                            if '&' in name and name.rstrip()[-1] == '&':
                                # If the next line continues with the
                                # initialization of the current
                                # variable, then don't allow the
                                # continuation by removing the '&'.
                                line = line.rstrip()[:-1]
                            name = name[:name.find(symbol)]
                    name = name.strip()
                    if name != '&':
                        r = '([ ,:&]|^)' + name + '([ ,(\[=!\*\t]|$)'
                        m = search(r, line_raw)
                        position = int((m.start()+m.end())/2)
                        TAGS.append("{} {} {} {} {} {}\n".\
                                    format(filepath, scope_count, name, \
                                           scope, line_nr, position))
                        scope_unused = False
                    i += 1
                    cont_var = True if line.rstrip()[-1] == '&' else False
                continue
            if ('=>' in line and \
                (('use' in line and line.startswith(('use ', 'use,'))) or \
                 ('associate' in line and \
                  line.startswith(('associate ', 'associate('))))) or \
                cont_redef:
                # Redefinitions count as new definitions.
                if (line.startswith(('associate ', 'associate('))):
                    scope += "associate_construct:"
                    scope_count = scope.count(':')
                names = line.split('=>')
                for i in range(len(names)-1):
                    name = names[i].split()[-1]
                    for symbol in ('(', ',', '&'):
                        if symbol in name: name = name[name.find(symbol)+1:]
                    name = name.strip()
                    m = search('([ ,(&]|^)'+name+'[ =]', line_raw)
                    position = str(int((m.start()+m.end())/2))
                    TAGS.append("{} {} {} {} {} {}\n".\
                                format(filepath, scope_count, name, \
                                       scope, line_nr, position))
                    scope_unused = False
                if line.rstrip()[-1] == '&':
                    cont_redef = True
                else:
                    cont_redef = False
                continue
            if match('interface +(?!(operator|assignment|$))', line):
                # Operator overloading also counts as a new definition.
                name = line.split(None,2)[1]
                m = search(' '+name+'([ !]|$)', line_raw)
                position = int((m.start()+m.end())/2)
                TAGS.append("{} {} {} {} {} {}\n".\
                            format(filepath, 0, name, \
                                   scope, line_nr, position))
                scope_unused = False
                continue
        # PART 2 - scope delocalization
        if line.startswith('end '):
            if match('end +(subroutine|function|type|associate' +
                     '|module|program)', line):
                # Decrease scope level by one or signal that we are longer
                # inside a type definition.
                if in_type:
                    in_type = False
                else:
                    if find_definitions and scope_unused:
                        # If the current procedure doesn't contain any
                        # dummy or local variables, then add the
                        # following line to the tags file to signal
                        # that such scope still exists even though it
                        # is empty of declarations. This ensures not
                        # to confuse the Elisp function
                        # fortran-find-scope.
                        TAGS.append(filepath + ' ' + scope + ' \n')
                        scope_unused = False
                    scope = scope[0:scope[0:len(scope)-1].rfind(':')+1]
                    s = scope.count(':')
                    scope_count = 0 if s == 2 and in_mod else s
                    if scope_count == 1: in_mod = False
            continue
        if line.startswith('end') and \
           (match('(endsubroutine[ \n]|endfunction[ \n]|endassociate[ \n]' +
                  '|endmodule[ \n]|endprogram[ \n])', line) or line == 'end\n'):
            if find_definitions and scope_unused:
                # Same as above
                TAGS.append(filepath + ' ' + scope + ' \n')
                scope_unused = False
            scope = scope[0:scope[0:len(scope)-1].rfind(':')+1]
            s = scope.count(':')
            scope_count = 0 if s == 2 and in_mod else s
            if scope_count == 1: in_mod = False
            continue
        # PART 3 - procedures, modules, type definitions; scope localization
        if 'subroutine ' in line:
            first_word = line.startswith('subroutine ')
            if first_word or ' subroutine ' in line:
                if first_word:
                    name = line.partition('subroutine ')[2]
                else:
                    name = line.partition(' subroutine ')[2]
                name = name[:name.find('(')].strip()
                if find_definitions:
                    m = search(' '+name+'([ (&!]|$)', line_raw)
                    position = int((m.start()+m.end())/2)
                    TAGS.append("{} {} {} {} {} {}\n".\
                                format(filepath, scope_count, name, \
                                       scope, line_nr, position))
                    scope_unused = True
                scope += name + ":"
                s = scope.count(':')
                scope_count = 0 if s == 2 and in_mod else s
                continue
        if 'function ' in line:
            first_word = line.partition(' ')[0] == 'function'
            if first_word or ' function ' in line:
                if first_word:
                    name = line.partition('function ')[2]
                else:
                    name = line.partition(' function ')[2]
                name = name[:name.find('(')].strip()
                if find_definitions:
                    m = search(' '+name+'([ (&!]|$)', line_raw)
                    position = int((m.start()+m.end())/2)
                    TAGS.append("{} {} {} {} {} {}\n".\
                                format(filepath, scope_count, name, \
                                       scope, line_nr, position))
                    scope_unused = True
                scope += name + ":"
                s = scope.count(':')
                scope_count = 0 if s == 2 and in_mod else s
                if find_definitions: cont_func = True
        if cont_func:
            if line.rstrip()[-1] != '&': cont_func = False
            if 'result' in line:
                m = search('[ )]result[( ]', line)
                if m:
                    name = line.partition(m.group())[2]
                    if ')' in name: name = name[:name.find(')')]
                    if '(' in name: name = name[name.find('(')+1:]
                    m = search('\( *'+name+' *\)', line_raw)
                    position = int((m.start()+m.end())/2)
                    TAGS.append("{} {} {} {} {} {}\n".\
                                format(filepath, scope_count, name, \
                                       scope, line_nr, position))
                    scope_unused = False
                continue
        if 'module ' in line and ' procedure ' in line and \
           line.split(None,2)[0:2] == ['module', 'procedure']:
            continue
        if 'module ' in line and line.partition(' ')[0] == 'module':
            name = line.split(None,2)[1]
            if find_definitions:
                m = search(' '+name+'([ (&!]|$)', line_raw)
                position = int((m.start()+m.end())/2)
                TAGS.append("{} {} {} {} {} {}\n".\
                            format(filepath, 1, name, scope, line_nr, position))
                in_mod = True
                scope_unused = True
            scope += name + ":"
            s = scope.count(':')
            scope_count = 0 if s == 2 and in_mod else s
            continue
        if 'program ' in line and line.partition(' ')[0] == 'program':
            name = line.split(None,2)[1]
            if find_definitions:
                m = search(' '+name+'([ (&!]|$)', line_raw)
                position = int((m.start()+m.end())/2)
                TAGS.append("{} {} {} {} {} {}\n".\
                            format(filepath, 1, name, scope, line_nr, position))
                scope_unused = True
            scope += name + ":"
            s = scope.count(':')
            scope_count = 0 if s == 2 and in_mod else s
            continue
        if line.startswith(('type,', 'type:')) or \
           (line.startswith('type ') and \
            not line[5:].lstrip().startswith('(')):
           # 'type' is special because it can both declare a variable
           # and define a new type.
            if find_definitions:
                if ':' in line:
                    name = line[line.find(':')+2:].strip()
                else:
                    name = line.split(None,2)[1]
                m = search('[ :]'+name+'([ !]|$)', line_raw)
                position = int((m.start()+m.end())/2)
                TAGS.append("{} {} {} {} {} {}\n".\
                            format(filepath, scope_count, name, \
                                   scope, line_nr, position))
                scope_unused = False
            in_type = True
            continue
    
parser = argparse.ArgumentParser()
parser.add_argument('-s', '--find-scope', action='store_true',
                    help="Return the current scope based on text from stdin")
parser.add_argument('-g', '--generate', nargs='+', metavar='FILE',
                    help="Generate or update the tags file.")
parser.add_argument('-o', '--output', nargs=1,
                    help="Target file for tags (default: FORTAGS)")

if len(argv) < 2:
    parser.parse_args('-h'.split())
else:
    args = parser.parse_args()

if args.find_scope:
    input_text = stdin.readlines()
    process_input(input_text, False)
    print(scope)

if args.generate:
    if args.output:
        tags_path = args.output[0]
    else:
        tags_path = 'FORTAGS'
    if path.exists(tags_path):
        tags_old = open(tags_path).readlines()
    input_files = [path.abspath(f) for f in args.generate]
    TAGS = [] # Work array for tags
    untouched = [] # Unmodified source files
    # STEP 1 - Scan the old tags file for changes
    if path.exists(tags_path) and str(tags_old[0]) != VERSION+'\n':
        stdout.write("Deleting {}, which was created with version {}".\
                     format(tags_path, tags_old[0]))
    elif path.exists(tags_path):
        stdout.write("Scanning {} for changes ...".format(tags_path))
        stdout.flush()
        line_nr = 2
        while line_nr < len(tags_old):
            words = tags_old[line_nr].split()
            filename = words[0]
            shift = int(words[-1])
            if filename in input_files and \
               path.getctime(filename) < path.getctime(tags_path):
                TAGS.extend(tags_old[line_nr:line_nr+shift])
                untouched.append(filename)
            line_nr += shift
        # Include unmodified files devoid of any declarations
        old_filenames = tags_old[1].split()
        for filename in input_files:
            if filename in old_filenames and not filename in untouched and \
               path.getctime(filename) < path.getctime(tags_path):
                untouched.append(filename)
        stdout.write("\rScanning {} for changes ... done\n".format(tags_path))
        stdout.flush()
    # STEP 2 - Process modified source files
    total_size = sum(stat(f).st_size for f in args.generate) - \
                 sum(stat(f).st_size for f in untouched)
    current_size = 0
    for f in args.generate:
        if not path.abspath(f) in untouched:
            stdout.write("\rProcessing input files ... {0}%".\
                         format(int(float(current_size)/total_size*100)))
            stdout.flush()
            current_size += stat(f).st_size
            try:
                input_text = open(f, 'r').readlines()
            except UnicodeDecodeError as e:
                from codecs import open as copen
                input_text = copen(f, 'r', encoding='utf-8', \
                                         errors='ignore').readlines()
                print("\rutf-8 codec can't fully decode {}. ".format(f) +
                      "Skipping some characters.")
                stdout.write("\rProcessing input files ... {0}%".\
                             format(int(float(current_size)/total_size*100)))
            try:
                process_input(input_text, True, TAGS, path.abspath(f))
            except AttributeError as e:
                from inspect import trace
                from sys import exit
                line_raw = trace()[-1][0].f_locals['line_raw']
                line_nr = trace()[-1][0].f_locals['line_nr']
                print("\nError on line {}:{}:".format(path.abspath(f), line_nr))
                print(line_raw)
                print("AttributeError: {}".format(e))
                print("If you consider this a bug, please report at")
                print("https://github.com/raullaasner/fortran-tags/issues")
                exit()
    n_processed = len(args.generate) - len(untouched)
    stdout.write("\rProcessing input files ... done ({} file{})\n".\
                 format(n_processed, 's' if n_processed != 1 else ''))
    stdout.flush()
    # STEP 3 - Write data to file
    TAGS.sort()
    line_nr = 0
    while line_nr < len(TAGS):
        # Add size specifiers to certain lines (see the format of the
        # tags file).
        tmp = line_nr
        filename = TAGS[line_nr].partition(' ')[0]
        while TAGS[line_nr].startswith(filename):
            line_nr += 1
            if line_nr == len(TAGS): break
        if not filename in untouched:
            TAGS[tmp] = TAGS[tmp][0:-1]+' '+str(line_nr-tmp)+'\n'
    with open(tags_path, 'w') as tags_file:
        tags_file.write(VERSION+'\n')
        tags_file.writelines(path.abspath(f)+' ' for f in args.generate)
        tags_file.write('\n')
        tags_file.writelines(TAGS)
