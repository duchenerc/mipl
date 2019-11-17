
import os
import sys

from lexer import Lexer
from elements import Nonterminal
from rules import mipl, oal

fin_filename = input("Enter input filename: ") if len(sys.argv) < 2 else sys.argv[1]
fout_filename = input("Enter output filename: ") if len(sys.argv) < 3 else sys.argv[2]

lexer = Lexer(fin_filename)

token = lexer.next()
fout = open(fout_filename, "w")

try:
    # parse(Nonterminal.PROG, token, lexer)
    mipl.parse(Nonterminal.PROG, None, token, lexer)

except StopIteration:
    oal.write(fout)
    # print("\n---- Completed parsing ----")
except Exception as e:
    print(e)
    
else:
    print("Syntax error: unexpected chars at end of program!")

fout.close()
