
import os
import sys

from lexer import Lexer
from parser import parse, Nonterminal

if len(sys.argv) < 2:
    raise Exception("No input file provided")

filename = sys.argv[1]

lexer = Lexer(filename)

token = lexer.next()
try:
    parse(Nonterminal.PROG, token, lexer)
except StopIteration:
    print("---- Completed parsing ----")
except Exception as e:
    print(e)
