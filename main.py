

import os
import re
import sys
from pprint import pprint
import string

tokens = [
    "\\w+",
    "\\'.*\\'"
    "\[",
    "\]",
    "\(\*?",
    "\)",
    "\*\)?",
    "\+",
    ",",
    "\-",
    "\.\.?",
    "\:\=?",
    ";",
    "<=?",
    "<>?",
    ">=?"
    # "[^\s]+"
    # "(?!\s+)"
    # "(?!\(\**+\*\))"
]

if len(sys.argv) < 2:
    raise Exception("No input file provided")

filename = sys.argv[1]

tokenstream = []

WHITESPACE = re.compile("\\s")
IDENT_START = re.compile("[a-zA-Z_]")
IDENT_BODY = re.compile("\w")
DIGIT = re.compile("\d")

IDENT = re.compile("[a-zA-Z_]\w*")
INTCONST = re.compile("\d+")
CHARCONST = re.compile("\'.\'")

CHARCONST_INVALID = re.compile("^\'")

LITERALS = {
    ":=": "T_ASSIGN",
    "*": "T_MULT",
    "+": "T_PLUS",
    "-": "T_MINUS",
    "div": "T_DIV",
    "and": "T_AND",
    "or": "T_OR",
    "not": "T_NOT",
    "<": "T_LT",
    ">": "T_GT",
    "<=": "T_LE",
    ">=": "T_GE",
    "=": "T_EQ",
    "<>": "T_NE",
    "var": "T_VAR",
    "array": "T_ARRAY",
    "of": "T_OF",
    "boolean": "T_BOOL",
    "char": "T_CHAR",
    "integer": "T_INT",
    "program": "T_PROG",
    "procedure": "T_PROC",
    "begin": "T_BEGIN",
    "end": "T_END",
    "while": "T_WHILE",
    "do": "T_DO",
    "if": "T_IF",
    "then": "T_THEN",
    "else": "T_ELSE",
    "read": "T_READ",
    "write": "T_WRITE",
    "true": "T_TRUE",
    "false": "T_FALSE",
    "[": "T_LBRACK",
    "]": "T_RBRACK",
    ";": "T_SCOLON",
    ":": "T_COLON",
    "(": "T_LPAREN",
    ")": "T_RPAREN",
    ",": "T_COMMA",
    ".": "T_DOT",
    "..": "T_DOTDOT"
}

def print_token(tok, lexeme):
    print(f"TOKEN: {tok} LEXEME: {lexeme}")

with open(filename) as f:
    filestr = f.read()
    
    tokstart = 0
    tokend = 0

    while tokstart < len(filestr):
        while re.match(WHITESPACE, filestr[tokstart]):
            tokstart += 1
            if tokstart >= len(filestr):
                break
            
        if tokstart >= len(filestr):
                break

        tokend = tokstart + 1


        if filestr[tokend-1:tokend+1] == "(*":
            while filestr[tokend-1:tokend+1] != "*)":
                tokend += 1
            
            tokstart = tokend + 1
            continue

        elif re.match(IDENT_START, filestr[tokstart]):
            while re.match(IDENT_BODY, filestr[tokend]):
                tokend += 1
            
        elif re.match(DIGIT, filestr[tokstart]):
            while re.match(DIGIT, filestr[tokend]):
                tokend += 1

        elif filestr[tokstart] == "\'":
            tokend += 1
            while tokend < len(filestr) and filestr[tokend] != "\'":
                tokend += 1
            tokend += 1
        
        elif filestr[tokstart] == ".":
            if filestr[tokend] == ".":
                tokend += 1
        
        elif filestr[tokstart] == ":":
            if filestr[tokend] == "=":
                tokend += 1
        
        elif filestr[tokstart] == "<":
            if filestr[tokend] in "=>":
                tokend += 1
        
        elif filestr[tokstart] == ">":
            if filestr[tokend] == "=":
                tokend += 1
        
        # print(filestr[tokstart:tokend])

        tokenstream.append(filestr[tokstart:tokend])
        tokstart = tokend
    
    for lexeme in tokenstream:
        if lexeme in LITERALS.keys():
            print_token(LITERALS[lexeme], lexeme)

        elif re.match(IDENT, lexeme):
            print_token("T_IDENT", lexeme)
        
        elif re.match(INTCONST, lexeme):
            if not (int(lexeme, 10) >> 31):
                print_token("T_INTCONST", lexeme)
            else:
                print(f"**** invalid integer constant: {lexeme}")
        
        elif re.match(CHARCONST, lexeme):
            print_token("T_CHARCONST", lexeme)
        
        elif re.match(CHARCONST_INVALID, lexeme):
            print(f"**** invalid character constant: {lexeme}")
        
        else:
            print_token("UNKNOWN", lexeme)
