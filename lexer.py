from enum import Enum
import re

IDENT = re.compile("[a-zA-Z_]\w*")
INTCONST = re.compile("\d+")
CHARCONST = re.compile("\'.\'")
CHARCONST_INVALID = re.compile("^\'")

class Token(Enum):
    ASSIGN = "T_ASSIGN"
    MULT = "T_MULT"
    PLUS = "T_PLUS"
    MINUS = "T_MINUS"
    DIV = "T_DIV"
    AND = "T_AND"
    OR = "T_OR"
    NOT = "T_NOT"
    LT = "T_LT"
    GT = "T_GT"
    LE = "T_LE"
    GE = "T_GE"
    EQ = "T_EQ"
    NE = "T_NE"
    VAR = "T_VAR"
    ARRAY = "T_ARRAY"
    OF = "T_OF"
    BOOL = "T_BOOL"
    CHAR = "T_CHAR"
    INT = "T_INT"
    PROG = "T_PROG"
    PROC = "T_PROC"
    BEGIN = "T_BEGIN"
    END = "T_END"
    WHILE = "T_WHILE"
    DO = "T_DO"
    IF = "T_IF"
    THEN = "T_THEN"
    ELSE = "T_ELSE"
    READ = "T_READ"
    WRITE = "T_WRITE"
    TRUE = "T_TRUE"
    FALSE = "T_FALSE"
    LBRACK = "T_LBRACK"
    RBRACK = "T_RBRACK"
    SCOLON = "T_SCOLON"
    COLON = "T_COLON"
    LPAREN = "T_LPAREN"
    RPAREN = "T_RPAREN"
    COMMA = "T_COMMA"
    DOT = "T_DOT"
    DOTDOT = "T_DOTDOT"
    IDENT = "T_IDENT"
    INTCONST = "T_INTCONST"
    CHARCONST = "T_CHARCONST"
    UNKNOWN = "UNKNOWN"

LITERALS = {
    ":=": Token.ASSIGN,
    "*": Token.MULT,
    "+": Token.PLUS,
    "-": Token.MINUS,
    "div": Token.DIV,
    "and": Token.AND,
    "or": Token.OR,
    "not": Token.NOT,
    "<": Token.LT,
    ">": Token.GT,
    "<=": Token.LE,
    ">=": Token.GE,
    "=": Token.EQ,
    "<>": Token.NE,
    "var": Token.VAR,
    "array": Token.ARRAY,
    "of": Token.OF,
    "boolean": Token.BOOL,
    "char": Token.CHAR,
    "integer": Token.INT,
    "program": Token.PROG,
    "procedure": Token.PROC,
    "begin": Token.BEGIN,
    "end": Token.END,
    "while": Token.WHILE,
    "do": Token.DO,
    "if": Token.IF,
    "then": Token.THEN,
    "else": Token.ELSE,
    "read": Token.READ,
    "write": Token.WRITE,
    "true": Token.TRUE,
    "false": Token.FALSE,
    "[": Token.LBRACK,
    "]": Token.RBRACK,
    ";": Token.SCOLON,
    ":": Token.COLON,
    "(": Token.LPAREN,
    ")": Token.RPAREN,
    ",": Token.COMMA,
    ".": Token.DOT,
    "..": Token.DOTDOT
}

class Lexeme:

    def __init__(self, literal, token):

        self.literal = literal
        self.token = token
    
    def __str__(self):
        return f"TOKEN: {self.token.value} LEXEME: {self.literal}"

class Lexer:

    def __init__(self, tokenizer):

        self.tokenizer = tokenizer
        self.debug_output = False

    def __iter__(self):
        return self
    
    def __next__(self):
        return self.process_literal(self.tokenizer.next())
    
    def process_literal(self, literal):
        if literal in LITERALS.keys():
            return Lexeme(literal, LITERALS[literal])

        elif re.match(IDENT, literal):
            return Lexeme(literal, Token.IDENT)
        
        elif re.match(INTCONST, literal):
            if not (int(literal, 10) >> 31):
                return Lexeme(literal, Token.INTCONST)
            else:
                print(f"**** invalid integer constant: {literal}")
        
        elif re.match(CHARCONST, literal):
            return Lexeme(literal, Token.CHARCONST)
        
        elif re.match(CHARCONST_INVALID, literal):
            print(f"**** invalid character constant: {literal}")
        
        else:
            return Lexeme(literal, Token.UNKNOWN)
