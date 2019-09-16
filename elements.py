from enum import Enum

class Terminal(Enum):
    """
    A 
    """
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
