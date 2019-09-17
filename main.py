
import os
import sys

from lexer import Lexer
from elements import Nonterminal
from rules import mipl

if len(sys.argv) < 2:
    raise Exception("No input file provided")

filename = sys.argv[1]

lexer = Lexer(filename)

token = lexer.next()

try:
    # parse(Nonterminal.PROG, token, lexer)
    mipl.parse(Nonterminal.PROG, token, lexer)
except StopIteration:
    print("---- Completed parsing ----")
except Exception as e:
    print(e)
else:
    print("Syntax error: unexpected chars at end of program!")
