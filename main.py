
import os
import sys

from lexer import Lexer
from elements import Nonterminal
from rules import mipl

filename = input("Enter filename: ") if len(sys.argv) < 2 else sys.argv[1]

lexer = Lexer(filename)

token = lexer.next()

try:
    # parse(Nonterminal.PROG, token, lexer)
    mipl.parse(Nonterminal.PROG, token, lexer)
except StopIteration:
    print("\n---- Completed parsing ----")
except Exception as e:
    print(e)
else:
    print("Syntax error: unexpected chars at end of program!")
