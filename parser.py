
from lexer import Token
from enum import Enum

from errors import MiplSyntaxError

class Nonterminal(Enum):
    PROG = "N_PROG"
    PROGLBL = "N_PROGLBL"
    BLOCK = "N_BLOCK"
    VARDECPART = "N_VARDECPART"
    VARDEC = "N_VARDEC"
    IDENT = "N_IDENT"
    IDENTLST = "N_IDENTLST"
    TYPE = "N_TYPE"
    ARRAY = "N_ARRAY"
    IDXRANGE = "N_IDXRANGE"
    IDX = "N_IDX"
    SIMPLE = "N_SIMPLE"
    VARDECLST = "N_VARDECLST"
    PROCDECPART = "N_PROCDECPART"
    PROCDEC = "N_PROCDEC"
    PROCHDR = "N_PROCHDR"
    STMTPART = "N_STMTPART"
    COMPOUND = "N_COMPOUND"
    STMT = "N_STMT"
    ASSIGN = "N_ASSIGN"
    VARIABLE = "N_VARIABLE"
    IDXVAR = "N_IDXVAR"
    EXPR = "N_EXPR"
    SIMPLEEXPR = "N_SIMPLEEXPR"
    TERM = "N_TERM"
    FACTOR = "N_FACTOR"
    SIGN = "N_SIGN"
    MULTOPLST = "N_MULTOPLST"
    MULTOP = "N_MULTOP"
    ADDOPLST = "N_ADDOPLST"
    ADDOP = "N_ADDOP"
    OPEXPR = "N_OPEXPR"
    STMTLST = "N_STMTLST"
    BOOLCONST = "N_BOOLCONST"
    CONST = "N_CONST"
    CONDITION = "N_CONDITION"
    ELSEPART = "N_ELSEPART"
    WHILE = "N_WHILE"
    RELOP = "N_RELOP"
    READ = "N_READ"
    WRITE = "N_WRITE"
    INPUTVAR = "N_INPUTVAR"
    INPUTLST = "N_INPUTLST"
    OUTPUT = "N_OUTPUT"
    OUTPUTLST = "N_OUTPUTLST"
    PROCIDENT = "N_PROCIDENT"
    PROCSTMT = "N_PROCSTMT"

GRAMMAR = {

    Nonterminal.PROGLBL: [[
        Token.PROG,
    ]],

    Nonterminal.PROG: [[
        Nonterminal.PROGLBL,
        Token.IDENT,
        Token.SCOLON,
        Nonterminal.BLOCK,
        Token.DOT
    ]],

    Nonterminal.BLOCK: [[
        Nonterminal.VARDECPART,
        Nonterminal.PROCDECPART,
        Nonterminal.STMTPART,
    ]],

    Nonterminal.VARDECPART: [
        [
            Token.VAR,
            Nonterminal.VARDEC,
            Token.SCOLON,
            Nonterminal.VARDECLST,
        ],
        [],
    ],

    Nonterminal.VARDECLST: [
        [
            Nonterminal.VARDEC,
            Token.SCOLON,
            Nonterminal.VARDECLST,
        ],
        [],
    ],

    Nonterminal.VARDEC: [[
        Nonterminal.IDENT,
        Nonterminal.IDENTLST,
        Token.COLON,
        Nonterminal.TYPE,
    ]],

    Nonterminal.IDENT: [[
        Token.IDENT
    ]],

    Nonterminal.IDENTLST: [
        [
            Token.COMMA,
            Nonterminal.IDENT,
            Nonterminal.IDENTLST,
        ],
        []
    ],

    Nonterminal.TYPE: [
        [ Nonterminal.SIMPLE ],
        [ Nonterminal.ARRAY ]
    ],

    Nonterminal.ARRAY: [[
        Token.ARRAY,
        Token.LBRACK,
        Nonterminal.IDXRANGE,
        Token.RBRACK,
        Token.OF,
        Nonterminal.SIMPLE,
    ]],

    Nonterminal.IDX: [[
        Token.INTCONST
    ]],

    Nonterminal.IDXRANGE: [[
        Nonterminal.IDX,
        Token.DOTDOT,
        Nonterminal.IDX
    ]],

    Nonterminal.SIMPLE: [
        [ Token.INT ],
        [ Token.CHAR ],
        [ Token.BOOL ],
    ],

    Nonterminal.PROCDECPART: [
        [
            Nonterminal.PROCDEC,
            Token.SCOLON,
            Nonterminal.PROCDECPART,
        ],
        [],
    ],

    Nonterminal.PROCDEC: [[
        Nonterminal.PROCHDR,
        Nonterminal.BLOCK,
    ]],

    Nonterminal.PROCHDR: [[
        Token.PROC,
        Token.IDENT,
        Token.SCOLON,
    ]],

    Nonterminal.STMTPART: [[
        Nonterminal.COMPOUND
    ]],

    Nonterminal.COMPOUND: [[
        Token.BEGIN,
        Nonterminal.STMT,
        Nonterminal.STMTLST,
        Token.END
    ]],

    Nonterminal.STMTLST: [
        [
            Token.SCOLON,
            Nonterminal.STMT,
            Nonterminal.STMTLST,
        ],
        [],
    ],

    Nonterminal.STMT: [
        [ Nonterminal.ASSIGN ],
        [ Nonterminal.READ ],
        [ Nonterminal.WRITE ],
        [ Nonterminal.CONDITION ],
        [ Nonterminal.WHILE ],
        [ Nonterminal.COMPOUND ],
    ],

    Nonterminal.ASSIGN: [[
        Nonterminal.VARIABLE,
        Token.ASSIGN,
        Nonterminal.EXPR,
    ]],

    Nonterminal.PROCSTMT: [[
        Nonterminal.PROCIDENT,
    ]],

    Nonterminal.PROCIDENT: [[
        Token.IDENT
    ]],

    Nonterminal.READ: [[
        Token.READ,
        Token.LPAREN,
        Nonterminal.INPUTVAR,
        Nonterminal.INPUTLST,
        Token.RPAREN,
    ]],

    Nonterminal.INPUTLST: [
        [
            Token.COMMA,
            Nonterminal.INPUTVAR,
            Nonterminal.INPUTLST,
        ],
        [],
    ],

    Nonterminal.INPUTVAR: [[
        Nonterminal.VARIABLE
    ]],

    Nonterminal.WRITE: [[
        Token.WRITE,
        Token.LPAREN,
        Nonterminal.OUTPUT,
        Nonterminal.OUTPUTLST,
        Token.RPAREN
    ]],

    Nonterminal.OUTPUTLST: [
        [
            Token.COMMA,
            Nonterminal.OUTPUT,
            Nonterminal.OUTPUTLST,
        ],
        [],
    ],

    Nonterminal.OUTPUT: [[
        Nonterminal.EXPR
    ]],

    Nonterminal.CONDITION: [[
        Token.IF,
        Nonterminal.EXPR,
        Token.THEN,
        Nonterminal.STMT,
        Nonterminal.ELSEPART,
    ]],

    Nonterminal.ELSEPART: [
        [
            Token.ELSE,
            Nonterminal.STMT,
        ],
        [],
    ],

    Nonterminal.WHILE: [[
        Token.WHILE,
        Nonterminal.EXPR,
        Token.DO,
        Nonterminal.STMT,
    ]],

    Nonterminal.EXPR: [[
        Nonterminal.SIMPLEEXPR,
        Nonterminal.OPEXPR,
    ]],

    Nonterminal.OPEXPR: [
        [
            Nonterminal.RELOP,
            Nonterminal.SIMPLEEXPR
        ],
        [],
    ],

    Nonterminal.SIMPLEEXPR: [[
        Nonterminal.TERM,
        Nonterminal.ADDOPLST,
    ]],

    Nonterminal.ADDOPLST: [
        [
            Nonterminal.ADDOP,
            Nonterminal.TERM,
            Nonterminal.ADDOPLST,
        ],
        [],
    ],

    Nonterminal.TERM: [[
        Nonterminal.FACTOR,
        Nonterminal.MULTOPLST
    ]],

    Nonterminal.MULTOPLST: [
        [
            Nonterminal.MULTOP,
            Nonterminal.FACTOR,
            Nonterminal.MULTOPLST,
        ],
        [],
    ],

    Nonterminal.FACTOR: [
        [
            Nonterminal.SIGN,
            Nonterminal.VARIABLE,
        ],
        [
            Nonterminal.CONST,
        ],
        [
            Token.LPAREN,
            Nonterminal.EXPR,
            Token.RPAREN,
        ],
        [
            Token.NOT,
            Nonterminal.FACTOR
        ],
    ],

    Nonterminal.SIGN: [
        [ Token.PLUS ],
        [ Token.MINUS ],
        [],
    ],

    Nonterminal.ADDOP: [
        [ Token.PLUS ],
        [ Token.MINUS ],
        [ Token.OR ],
    ],

    Nonterminal.MULTOP: [
        [ Token.MULT ],
        [ Token.DIV ],
        [ Token.AND ],
    ],

    Nonterminal.RELOP: [
        [ Token.LT ],
        [ Token.LE ],
        [ Token.NE ],
        [ Token.EQ ],
        [ Token.GT ],
        [ Token.GE ],
    ],

    Nonterminal.VARIABLE: [[
        Token.IDENT,
        Nonterminal.IDXVAR,
    ]],

    Nonterminal.IDXVAR: [
        [
            Token.LBRACK,
            Nonterminal.EXPR,
            Token.RBRACK,
        ],
        [],
    ],

    Nonterminal.CONST: [
        [ Token.INTCONST ],
        [ Token.CHARCONST ],
        [ Nonterminal.BOOLCONST ],
    ],

    Nonterminal.BOOLCONST: [
        [ Token.TRUE ],
        [ Token.FALSE ],
    ],

}

def first(production):
    first_set = []
    nt_first_set = []

    if len(production) == 0:
        first_set.append(None)
    
    for term in production:

        if isinstance(term, Token):
            first_set.append(term)
            break

        else:

            for subproduction in GRAMMAR[term]:
                nt_first_set += first(subproduction)
                
            if None in nt_first_set and term != production[-1]:
                nt_first_set.remove(None)
                continue
            
            first_set += nt_first_set
            break
    
    return first_set

def print_nonterminal(nonterminal, production):
    production_str = " ".join([p.value for p in production]) if len(production) > 0 else "epsilon"
    print(f"{nonterminal.value} -> {production_str}")

def parse(nonterminal, token, lexer):

    rule = GRAMMAR[nonterminal]

    for production in rule:
        if len(production) == 0:
            print_nonterminal(nonterminal, production)
            return token
        
        if token.kind in first(production):

            print_nonterminal(nonterminal, production)

            for term in production:

                if isinstance(term, Token):
                    if term == token.kind:
                        token = lexer.next()
                    else:
                        raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
                else: # is a nonterminal
                    token = parse(term, token, lexer)
            
            return token
    
    raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
