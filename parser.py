
from lexer import Terminal
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
        Terminal.PROG,
    ]],

    Nonterminal.PROG: [[
        Nonterminal.PROGLBL,
        Terminal.IDENT,
        Terminal.SCOLON,
        Nonterminal.BLOCK,
        Terminal.DOT
    ]],

    Nonterminal.BLOCK: [[
        Nonterminal.VARDECPART,
        Nonterminal.PROCDECPART,
        Nonterminal.STMTPART,
    ]],

    Nonterminal.VARDECPART: [
        [
            Terminal.VAR,
            Nonterminal.VARDEC,
            Terminal.SCOLON,
            Nonterminal.VARDECLST,
        ],
        [],
    ],

    Nonterminal.VARDECLST: [
        [
            Nonterminal.VARDEC,
            Terminal.SCOLON,
            Nonterminal.VARDECLST,
        ],
        [],
    ],

    Nonterminal.VARDEC: [[
        Nonterminal.IDENT,
        Nonterminal.IDENTLST,
        Terminal.COLON,
        Nonterminal.TYPE,
    ]],

    Nonterminal.IDENT: [[
        Terminal.IDENT
    ]],

    Nonterminal.IDENTLST: [
        [
            Terminal.COMMA,
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
        Terminal.ARRAY,
        Terminal.LBRACK,
        Nonterminal.IDXRANGE,
        Terminal.RBRACK,
        Terminal.OF,
        Nonterminal.SIMPLE,
    ]],

    Nonterminal.IDX: [[
        Terminal.INTCONST
    ]],

    Nonterminal.IDXRANGE: [[
        Nonterminal.IDX,
        Terminal.DOTDOT,
        Nonterminal.IDX
    ]],

    Nonterminal.SIMPLE: [
        [ Terminal.INT ],
        [ Terminal.CHAR ],
        [ Terminal.BOOL ],
    ],

    Nonterminal.PROCDECPART: [
        [
            Nonterminal.PROCDEC,
            Terminal.SCOLON,
            Nonterminal.PROCDECPART,
        ],
        [],
    ],

    Nonterminal.PROCDEC: [[
        Nonterminal.PROCHDR,
        Nonterminal.BLOCK,
    ]],

    Nonterminal.PROCHDR: [[
        Terminal.PROC,
        Terminal.IDENT,
        Terminal.SCOLON,
    ]],

    Nonterminal.STMTPART: [[
        Nonterminal.COMPOUND
    ]],

    Nonterminal.COMPOUND: [[
        Terminal.BEGIN,
        Nonterminal.STMT,
        Nonterminal.STMTLST,
        Terminal.END
    ]],

    Nonterminal.STMTLST: [
        [
            Terminal.SCOLON,
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
        Terminal.ASSIGN,
        Nonterminal.EXPR,
    ]],

    Nonterminal.PROCSTMT: [[
        Nonterminal.PROCIDENT,
    ]],

    Nonterminal.PROCIDENT: [[
        Terminal.IDENT
    ]],

    Nonterminal.READ: [[
        Terminal.READ,
        Terminal.LPAREN,
        Nonterminal.INPUTVAR,
        Nonterminal.INPUTLST,
        Terminal.RPAREN,
    ]],

    Nonterminal.INPUTLST: [
        [
            Terminal.COMMA,
            Nonterminal.INPUTVAR,
            Nonterminal.INPUTLST,
        ],
        [],
    ],

    Nonterminal.INPUTVAR: [[
        Nonterminal.VARIABLE
    ]],

    Nonterminal.WRITE: [[
        Terminal.WRITE,
        Terminal.LPAREN,
        Nonterminal.OUTPUT,
        Nonterminal.OUTPUTLST,
        Terminal.RPAREN
    ]],

    Nonterminal.OUTPUTLST: [
        [
            Terminal.COMMA,
            Nonterminal.OUTPUT,
            Nonterminal.OUTPUTLST,
        ],
        [],
    ],

    Nonterminal.OUTPUT: [[
        Nonterminal.EXPR
    ]],

    Nonterminal.CONDITION: [[
        Terminal.IF,
        Nonterminal.EXPR,
        Terminal.THEN,
        Nonterminal.STMT,
        Nonterminal.ELSEPART,
    ]],

    Nonterminal.ELSEPART: [
        [
            Terminal.ELSE,
            Nonterminal.STMT,
        ],
        [],
    ],

    Nonterminal.WHILE: [[
        Terminal.WHILE,
        Nonterminal.EXPR,
        Terminal.DO,
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
            Terminal.LPAREN,
            Nonterminal.EXPR,
            Terminal.RPAREN,
        ],
        [
            Terminal.NOT,
            Nonterminal.FACTOR
        ],
    ],

    Nonterminal.SIGN: [
        [ Terminal.PLUS ],
        [ Terminal.MINUS ],
        [],
    ],

    Nonterminal.ADDOP: [
        [ Terminal.PLUS ],
        [ Terminal.MINUS ],
        [ Terminal.OR ],
    ],

    Nonterminal.MULTOP: [
        [ Terminal.MULT ],
        [ Terminal.DIV ],
        [ Terminal.AND ],
    ],

    Nonterminal.RELOP: [
        [ Terminal.LT ],
        [ Terminal.LE ],
        [ Terminal.NE ],
        [ Terminal.EQ ],
        [ Terminal.GT ],
        [ Terminal.GE ],
    ],

    Nonterminal.VARIABLE: [[
        Terminal.IDENT,
        Nonterminal.IDXVAR,
    ]],

    Nonterminal.IDXVAR: [
        [
            Terminal.LBRACK,
            Nonterminal.EXPR,
            Terminal.RBRACK,
        ],
        [],
    ],

    Nonterminal.CONST: [
        [ Terminal.INTCONST ],
        [ Terminal.CHARCONST ],
        [ Nonterminal.BOOLCONST ],
    ],

    Nonterminal.BOOLCONST: [
        [ Terminal.TRUE ],
        [ Terminal.FALSE ],
    ],

}

def first(production):
    first_set = []
    nt_first_set = []

    if len(production) == 0:
        first_set.append(None)
    
    for term in production:

        if isinstance(term, Terminal):
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
        
        if token.terminal in first(production):

            print_nonterminal(nonterminal, production)

            for term in production:

                if isinstance(term, Terminal):
                    if term == token.terminal:
                        token = lexer.next()
                    else:
                        raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
                else: # is a nonterminal
                    token = parse(term, token, lexer)
            
            return token
    
    raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
