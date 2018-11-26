#!/usr/bin/env python3

# Copyright (C) 2015-2018 Raul Laasner
# This file is distributed under the terms of the GNU General Public
# License, see 'LICENSE' in the root directory of the present
# distribution or http://gnu.org/copyleft/gpl.txt .

# This script contains two main functions:
#  1) Generating a tags file. The scope and position of every variable
#     and procedure are determined. Once the initial tags file has
#     been generated, subsequent calls produce only minimal changes,
#     which means that it is cheap to call this function every time a
#     software project is opened.
#  2) Finding the current scope. The same function is called as for
#     generating the tags file except that the definition finding
#     parts are turned off. It can be invoked by fortran-find-scope in
#     fortran-tags.el, which provides the buffer contents of a source
#     file up to the cursor position in the form of stdin and receives
#     the scope at that position after it has been determined. It is
#     only invoked if fortran-find-scope is unable to find the scope
#     on its own for some reason.

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

import sys
import os
import re
import argparse

VERSION = '1.5.1'


def clean_string(l):
    """Removes strings and comments from the line.

    """
    # Since there are two delimiters for the character type which can
    # be nested, all combinations need to be considered.

    # lock[0] (lock[1]) is True if the previous line didn't have a
    # matching single (double) quote
    global lock
    char_symbol = ["'", '"']
    str_length = len(l)
    # If x (x being ', ", or !) is not in l, we define its position to
    # be at str_length.
    pos_c = l.find('!') if '!' in l else str_length
    pos_s = [0, 0]
    for i in -1, 0:
        pos_s[i] = (l.find(char_symbol[i]) if char_symbol[i] in l
                    else str_length)
    for i in -1, 0:
        if lock[i]:
            if pos_s[i] < str_length:
                lock[i] = False
                return clean_string(l[pos_s[i]+1:])
            else:
                return ''
    if pos_c == str_length and pos_s[0] == str_length and (pos_s[1] ==
                                                           str_length):
        return l
    if pos_c < pos_s[0] and pos_c < pos_s[1]:
        return l[0:pos_c]
    for i in -1, 0:
        if pos_s[i] < pos_s[i+1]:
            l0 = l[pos_s[i]+1:]
            if char_symbol[i] in l0:
                return clean_string(l[0:pos_s[i]] +
                                    l[pos_s[i]+l0.find(char_symbol[i])+2:])
            else:
                lock[i] = True
                return l[:pos_s[i]]


def process_input(input_text, find_definitions, TAGS='', filepath=''):
    """Find definitions and their scopes.

    If not find_definitions, then only find the scopes.

    """
    global lock
    # Current scope
    global scope
    # Whether the current line is a continuation of a 1) variable definition,
    cont_var = False
    # 2) renaming,
    cont_rename = False
    # 3) function definition,
    cont_func = False
    # 4) any other line.
    cont_line = False
    # Whether we are currently inside a type definition. Variable
    # definitions inside a type definition are ignored.
    in_type = False
    # Same but with interfaces.
    in_interface = False
    # True if there haven't been any definitions with the present
    # scope so far.
    scope_unused = True
    # Wether we are currently inside a module. This helps to set the
    # scope level of module wide variables to 0 as explained above.
    in_mod = False
    # Initialize clean_string
    lock = [False, False]
    scope = ':'
    scope_count = 1
    line_nr = 0
    for line_raw in input_text:
        line_nr += 1
        if line_raw.lstrip().startswith('#'):
            continue
        line_raw = line_raw.lower()
        line = clean_string(line_raw).strip()
        if not line:
            continue
        if cont_line:
            cont_line = line.endswith('&')
            continue
        if find_definitions and not in_type and not in_interface:
            # PART 1 - variables, renamings, operator overloading
            exceptions = 'real[*]8|complex[*]16|'
            m = re.match('(('+exceptions+'real|integer|logical|character|'
                         'complex|class|enumerator|external)'
                         r'[ ,([:]|type *\()', line)
            if (m and ' function ' not in line) or cont_var:
                # In the following, 'names' contains all the variables
                # found on the current line.
                if cont_var:
                    for symbol in ('&', ','):
                        # Cannot use startswith(('&', ',')) here!
                        if line.startswith(symbol):
                            line = line[1:].lstrip()
                    if line.startswith(('/', '*', '-', '+')):
                        cont_var = False
                        continue
                    names = line.split(',')
                elif '::' in line:
                    names = line[line.find('::')+2:].split(',')
                else:
                    # Any of the following has happened at this point:
                    # 1) old style was used (no :: on the line); 2) a
                    # keyword was used as a variable; 3) a keyword was
                    # used as a built-in function (e.g.,
                    # real(1,8)). In any case, this is slow.
                    if m.group()[-1] == '(':
                        line = line[m.end()-1:].lstrip()
                    else:
                        line = line[m.end():].lstrip()
                    if line.startswith('('):
                        shift = 1
                        while shift < len(line) and (line[:shift].count('(') !=
                                                     line[:shift].count(')')):
                            shift += 1
                        if shift == len(line):
                            continue
                        line = line[shift:].lstrip()
                        if not line:
                            continue
                    if line.startswith(('=', '/', '.', '*',
                                        '-', '+', ')', ']')):
                        continue
                    names = line.split(',')
                i_name = 0
                while i_name < len(names):
                    # Since names are separated by splitting the line
                    # at commas, special care needs to be taken
                    # against multi-dimensional arrays and coarrays.
                    name = names[i_name]
                    while name.count('(') > name.count(')') or (
                            name.count('[') > name.count(']')):
                        i_name += 1
                        if i_name == len(names):
                            break
                        # The name is pieced back together
                        name += f',{names[i_name]}'
                    for symbol in ('(', '=', '[', '*'):
                        if symbol in name:
                            if '&' in name and name.rstrip()[-1] == '&':
                                # If the next line continues with the
                                # initialization of the current
                                # variable, don't follow the
                                # continuation.
                                line = line.rstrip()[:-1]
                            name = name[:name.find(symbol)]
                    name = name.strip()
                    if name != '&':
                        # If the current scope is ':' and we are
                        # looking at a variable declaration, it means
                        # we are in the program construct but the
                        # beginning 'program' keyword is missing. In
                        # this case set scope to
                        # :fortags_program_scope: manually.
                        if scope_count == 1:
                            scope = ':fortags_program_scope:'
                            scope_count = 2
                        r = '([ ,:&\t]|^)' + name + r'([ ,(\[=!\*\t]|$)'
                        m = re.search(r, line_raw)
                        position = int((m.start()+m.end())/2)
                        TAGS.append(f'{filepath} {scope_count} {name} {scope} '
                                    f'{line_nr} {position}\n')
                        scope_unused = False
                    i_name += 1
                    cont_var = line.rstrip()[-1] == '&'
                continue
            if ('=>' in line and (
                    ('use' in line and line.startswith(('use ', 'use,'))) or
                    ('associate' in line and
                     line.startswith(('associate ', 'associate('))))) or (
                         cont_rename):
                # Same comment as above with variable declarations
                if scope_count == 1:
                    scope = ':fortags_program_scope:'
                    scope_count = 2
                # Renamings count as new definitions.
                if (line.startswith(('associate ', 'associate('))):
                    scope += 'fortags_associate_construct:'
                    scope_count = scope.count(':')
                names = line.split('=>')
                for i in range(len(names)-1):
                    name = names[i].split()[-1]
                    for symbol in ('(', ',', '&'):
                        if symbol in name:
                            name = name[name.find(symbol)+1:]
                    name = name.strip()
                    m = re.search('([ ,(&]|^)'+name+'[ =]', line_raw)
                    position = str(int((m.start()+m.end())/2))
                    TAGS.append(f'{filepath} {scope_count} {name} {scope} '
                                f'{line_nr} {position}\n')
                    scope_unused = False
                cont_rename = line.rstrip()[-1] == '&'
                continue
            if re.match('interface +(?!(operator|assignment|$))', line):
                # Operator overloading also counts as a new definition.
                if scope_count == 1:
                    # Same comment as above with variable declarations.
                    scope = ':fortags_program_scope:'
                    scope_count = 2
                name = line.split(None, 2)[1]
                m = re.search(' '+name+'([ !]|$)', line_raw)
                position = int((m.start()+m.end())/2)
                TAGS.append(f'{filepath} 0 {name} {scope} {line_nr} {position}'
                            '\n')
                scope_unused = False
                continue
            # Treat procedure renamings inside a type definition
            # separately.
            if in_type and '=>' in line:
                # Exclude pointer initializations
                if re.search(',[ ]*pointer[ ]*( |[ ]*::)', line):
                    continue
                # Exclude operator(...) type overloading.
                if ')' in line:
                    continue
                name = line.split('=>', 1)[0].rstrip()
                if '::' in name:
                    name = name.split('::', 1)[1]
                if ' ' in name:
                    name = name.rsplit(' ', 1)[1]
                m = re.search('( |[ ]*::)'+name+'[ ]*=>', line_raw)
                # If the search failed, there was something weird in the
                # source code (maybe a preprocessor macro that
                # Fortran-tags couldn't handle).
                if m is None:
                    continue
                position = int((m.start()+m.end())/2)
                TAGS.append(f'{filepath} 0 {name} {scope} {line_nr} {position}'
                            '\n')
                scope_unused = False
                continue
        # PART 2 - scope delocalization
        if (
                line.startswith('end interface') and
                line.rstrip() == 'end interface' or
                line.startswith('endinterface') and
                line.rstrip() == 'endinterface'):
            in_interface = False
            continue
        if in_interface:
            continue
        if line.startswith('end '):
            if re.match('end +(subroutine|function|type|associate|block|'
                        'module|program)', line):
                # Decrease scope level by one or signal that we are
                # longer inside a type definition.
                if in_type:
                    in_type = False
                elif in_interface:
                    in_interface = False
                else:
                    if find_definitions and scope_unused:
                        # If the current procedure doesn't contain any
                        # dummy or local variables, then add the
                        # following line to the tags file to signal
                        # that such scope still exists even though it
                        # is empty of declarations. This ensures not
                        # to confuse the Elisp function
                        # fortran-find-scope.
                        TAGS.append(f'{filepath} {scope} \n')
                        scope_unused = False
                    scope = scope[0:scope[0:len(scope)-1].rfind(':')+1]
                    s = scope.count(':')
                    scope_count = 0 if s == 2 and in_mod else s
                    if scope_count == 1:
                        in_mod = False
            continue
        if line.startswith('end') and (re.match(
                '(endsubroutine|endfunction|endassociate|endblock|endmodule|'
                'endprogram) ?', line) or line == 'end'):
            if find_definitions and scope_unused:
                # Same as above
                TAGS.append(f'{filepath} {scope} \n')
                scope_unused = False
            scope = scope[0:scope[0:len(scope)-1].rfind(':')+1]
            s = scope.count(':')
            scope_count = 0 if s == 2 and in_mod else s
            if scope_count == 1:
                in_mod = False
            continue
        # PART 3 - procedures, modules, type definitions; scope localization
        if 'subroutine ' in line:
            first_word = line.startswith('subroutine ')
            if first_word or ' subroutine ' in line:
                if first_word:
                    name = line.partition('subroutine ')[2]
                else:
                    name = line.partition(' subroutine ')[2]
                for symbol in ('(', '&'):
                    if symbol in name:
                        name = name[:name.find(symbol)]
                name = name.strip()
                if find_definitions:
                    m = re.search(' '+name+'([ (&!]|$)', line_raw)
                    position = int((m.start()+m.end())/2)
                    TAGS.append(f'{filepath} {scope_count} {name} {scope} '
                                f'{line_nr} {position}\n')
                    scope_unused = True
                scope += f'{name}:'
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
                for symbol in ('(', '&'):
                    if symbol in name:
                        name = name[:name.find(symbol)]
                name = name.strip()
                if find_definitions:
                    m = re.search(' '+name+'([ (&!]|$)', line_raw)
                    position = int((m.start()+m.end())/2)
                    TAGS.append(f'{filepath} {scope_count} {name} {scope} '
                                f'{line_nr} {position}\n')
                    scope_unused = True
                scope += f'{name}:'
                s = scope.count(':')
                scope_count = 0 if s == 2 and in_mod else s
                cont_func = find_definitions
        if cont_func:
            cont_func = line.rstrip()[-1] == '&'
            if 'result' in line:
                m = re.search('[ )]result[( ]', line)
                if m:
                    name = line.partition(m.group())[2]
                    if ')' in name:
                        name = name[:name.find(')')]
                    if '(' in name:
                        name = name[name.find('(')+1:]
                    m = re.search(r'\( *'+name+r' *\)', line_raw)
                    position = int((m.start()+m.end())/2)
                    TAGS.append(f'{filepath} {scope_count} {name} {scope} '
                                f'{line_nr} {position}\n')
                    scope_unused = False
                continue
        if 'module ' in line and ' procedure ' in line and (
                line.split(None, 2)[0:2] == ['module', 'procedure']):
            continue
        if 'module ' in line and line.partition(' ')[0] == 'module':
            name = line.split(None, 2)[1]
            if find_definitions:
                m = re.search(' '+name+'([ (&!]|$)', line_raw)
                position = int((m.start()+m.end())/2)
                TAGS.append(f'{filepath} 1 {name} {scope} {line_nr} {position}'
                            f'\n')
                in_mod = True
                scope_unused = True
            scope += f'{name}:'
            s = scope.count(':')
            scope_count = 0 if s == 2 and in_mod else s
            continue
        if 'program ' in line and line.partition(' ')[0] == 'program':
            name = line.split(None, 2)[1]
            if find_definitions:
                m = re.search(' '+name+'([ (&!]|$)', line_raw)
                position = int((m.start()+m.end())/2)
                TAGS.append(f'{filepath} 1 {name} {scope} {line_nr} {position}'
                            '\n')
                scope_unused = True
            scope += f'{name}:'
            s = scope.count(':')
            scope_count = 0 if s == 2 and in_mod else s
            continue
        if line.startswith(('type,', 'type:')) or (
                line.startswith('type ') and not line[5:].lstrip().startswith(
                    '(')):
            # 'type' is special because it can both declare a variable
            # and define a new type.
            line0 = line[5:].lstrip()
            # Ignore 'select type' construct
            if line0.startswith('is'):
                line0 = line0[2:].lstrip()
                if line0.startswith('('):
                    continue
            if find_definitions:
                if ':' in line:
                    name = line[line.find(':')+2:].strip()
                else:
                    name = line.split(None, 2)[1]
                m = re.search('[ :]'+name+'([ !]|$)', line_raw)
                position = int((m.start()+m.end())/2)
                TAGS.append(f'{filepath} {scope_count} {name} {scope} '
                            f'{line_nr} {position}\n')
                scope_unused = False
            in_type = True
            continue
        if line.startswith('interface') and line.rstrip() == 'interface':
            in_interface = True
            continue
        if line.startswith('block') and line.rstrip() == 'block':
            scope += 'fortags_block_construct:'
            scope_count = scope.count(':')
            continue
        # Set cont_line to True if the line ends with an '&'. Note
        # that line == '&' is not valid Fortran and what must have
        # happened is that a string was removed from the line by
        # clean_string, leaving it almost empty. The '&' then marks
        # the beginning of a line (not continuation) or it marks the
        # continuation of a variable definition. Either way, we shall
        # not count it as line continuation.
        cont_line = line.endswith('&') and not line == '&'


parser = argparse.ArgumentParser()
parser.add_argument('-s', '--find-scope', action='store_true',
                    help='Return the current scope based on text from stdin')
parser.add_argument('-g', '--generate', nargs='+', metavar='FILE',
                    help='Generate or update the tags file.')
parser.add_argument('-o', '--output', nargs=1,
                    help='Target file for tags (default: FORTAGS)')

if len(sys.argv) < 2:
    parser.parse_args('-h'.split())
else:
    args = parser.parse_args()

if args.find_scope:
    input_text = sys.stdin.readlines()
    process_input(input_text, False)
    print(scope)

if args.generate:
    if args.output:
        tags_path = args.output[0]
    else:
        tags_path = 'FORTAGS'
    if os.path.exists(tags_path):
        tags_old = open(tags_path).readlines()
    input_files = [os.path.abspath(f) for f in args.generate]
    TAGS = []  # Work array for tags
    untouched = []  # Unmodified source files
    # STEP 1 - Scan the old tags file for changes
    if os.path.exists(tags_path) and str(tags_old[0]) != VERSION+'\n':
        sys.stdout.write(f'Deleting {tags_path}, which was created with '
                         f'version {tags_old[0]}')
    elif os.path.exists(tags_path):
        sys.stdout.write(f'Scanning {tags_path} for changes ...')
        sys.stdout.flush()
        line_nr = 2
        while line_nr < len(tags_old):
            words = tags_old[line_nr].split()
            filename = words[0]
            shift = int(words[-1])
            if filename in input_files and (
                    os.path.getctime(filename) < os.path.getctime(tags_path)):
                TAGS.extend(tags_old[line_nr:line_nr+shift])
                untouched.append(filename)
            line_nr += shift
        # Include unmodified files devoid of any declarations
        old_filenames = tags_old[1].split()
        for filename in input_files:
            if filename in old_filenames and filename not in untouched and (
                    os.path.getctime(filename) < os.path.getctime(tags_path)):
                untouched.append(filename)
        sys.stdout.write(f'\rScanning {tags_path} for changes ... done\n')
        sys.stdout.flush()
    # STEP 2 - Process modified source files
    total_size = (sum(os.stat(f).st_size for f in args.generate) -
                  sum(os.stat(f).st_size for f in untouched))
    current_size = 0
    for f in args.generate:
        if not os.path.abspath(f) in untouched:
            sys.stdout.write('\rProcessing input files ... '
                             f'{int(float(current_size)/total_size*100)}%')
            sys.stdout.flush()
            current_size += os.stat(f).st_size
            try:
                input_text = open(f, 'r').readlines()
            except UnicodeDecodeError:
                from codecs import open as copen
                input_text = copen(f, 'r', encoding='utf-8',
                                   errors='ignore').readlines()
                print(f"\rutf-8 codec can't fully decode {f}. "
                      'Skipping some characters.')
                sys.stdout.write('\rProcessing input files ... '
                                 f'{int(float(current_size)/total_size*100)}%')
            try:
                process_input(input_text, True, TAGS, os.path.abspath(f))
            except AttributeError as e:
                from inspect import trace
                line_raw = trace()[-1][0].f_locals['line_raw']
                line_nr = trace()[-1][0].f_locals['line_nr']
                print(f'\nError on line {os.path.abspath(f)}:{line_nr}:')
                print(line_raw)
                print(f'AttributeError: {e}')
                print('If you consider this a bug, please report at')
                print('https://github.com/raullaasner/fortran-tags/issues')
                raise
    n_processed = len(args.generate) - len(untouched)
    sys.stdout.write(f'\rProcessing input files ... done ({n_processed} '
                     f'file{"s" if n_processed != 1 else ""})\n')
    sys.stdout.flush()
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
            if line_nr == len(TAGS):
                break
        if filename not in untouched:
            TAGS[tmp] = f'{TAGS[tmp][0:-1]} {line_nr-tmp}\n'
    with open(tags_path, 'w') as tags_file:
        tags_file.write(f'{VERSION}\n')
        tags_file.writelines(os.path.abspath(f)+' ' for f in args.generate)
        tags_file.write('\n')
        tags_file.writelines(TAGS)
