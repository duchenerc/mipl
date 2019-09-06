

import os
import re
import sys
from pprint import pprint
import string

from tokenizer import Tokenizer
from lexer import Lexer

if len(sys.argv) < 2:
    raise Exception("No input file provided")

filename = sys.argv[1]

tokenizer = Tokenizer(filename)
lexer = Lexer(tokenizer)

for lexeme in lexer:
    if lexeme:
        print(lexeme)
