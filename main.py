

import os
import re
import sys
from pprint import pprint
import string

from tokenizer import Tokenizer
from lexer import Lexer

from parser import parse, Nonterminal

if len(sys.argv) < 2:
    raise Exception("No input file provided")

filename = sys.argv[1]

tokenizer = Tokenizer(filename)
lexer = Lexer(tokenizer)

token = lexer.next()
try:
    parse(Nonterminal.PROG, token, lexer)
except StopIteration:
    print("---- Completed parsing ----")
except Exception as e:
    print(e)
