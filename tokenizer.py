
import re

WHITESPACE = re.compile("\\s")
IDENT_START = re.compile("[a-zA-Z_]")
IDENT_BODY = re.compile("\w")
DIGIT = re.compile("\d")

class Tokenizer:

    def __init__(self, filename):
        
        filestr = ""

        with open(filename) as fin:
            filestr = fin.read()
        
        self.tokens = self._tokenize(filestr)
        self.last_token = -1
    
    def __iter__(self):
        return self
    
    def __next__(self):
        self.last_token += 1

        if self.last_token < len(self.tokens):
            return self.tokens[self.last_token]
        
        else:
            raise StopIteration
    
    def _tokenize(self, operand):

        tokens = []

        tokstart = 0
        tokend = 0

        while tokstart < len(operand):
            while re.match(WHITESPACE, operand[tokstart]):
                tokstart += 1
                if tokstart >= len(operand):
                    break
                
            if tokstart >= len(operand):
                break

            tokend = tokstart + 1


            if operand[tokend-1:tokend+1] == "(*":
                while operand[tokend-1:tokend+1] != "*)":
                    tokend += 1
                
                tokstart = tokend + 1
                continue

            elif re.match(IDENT_START, operand[tokstart]):
                while re.match(IDENT_BODY, operand[tokend]):
                    tokend += 1
                
            elif re.match(DIGIT, operand[tokstart]):
                while re.match(DIGIT, operand[tokend]):
                    tokend += 1

            elif operand[tokstart] == "\'":
                tokend += 1
                while tokend < len(operand) and operand[tokend] != "\'":
                    tokend += 1
                tokend += 1
            
            elif operand[tokstart] == ".":
                if operand[tokend] == ".":
                    tokend += 1
            
            elif operand[tokstart] == ":":
                if operand[tokend] == "=":
                    tokend += 1
            
            elif operand[tokstart] == "<":
                if operand[tokend] in "=>":
                    tokend += 1
            
            elif operand[tokstart] == ">":
                if operand[tokend] == "=":
                    tokend += 1
            
            # print(filestr[tokstart:tokend])

            tokens.append(operand[tokstart:tokend])
            tokstart = tokend

        return tokens
