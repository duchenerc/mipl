from grammar import Grammar
from elements import Terminal, Nonterminal

mipl = Grammar(Nonterminal, Terminal)

@mipl.production(Nonterminal.PROGLBL, (Terminal.PROG,))
def p_program_label():
    pass

@mipl.production(Nonterminal.PROG, (
    Nonterminal.PROGLBL,
    Terminal.IDENT,
    Terminal.SCOLON,
    Nonterminal.BLOCK,
    Terminal.DOT
))
def p_program():
    pass

@mipl.production(Nonterminal.BLOCK, (
    Nonterminal.VARDECPART,
    Nonterminal.PROCDECPART,
    Nonterminal.STMTPART,
))
def p_block():
    pass

@mipl.production(Nonterminal.VARDECPART, (
    Terminal.VAR,
    Nonterminal.VARDEC,
    Terminal.SCOLON,
    Nonterminal.VARDECLST
))
def p_variable_declaration_part():
    pass

@mipl.production(Nonterminal.VARDECPART, ())
def p_variable_declaration_part_epsilon():
    pass

@mipl.production(Nonterminal.VARDECLST, (
    Nonterminal.VARDEC,
    Terminal.SCOLON,
    Nonterminal.VARDECLST
))
def p_variable_declaration_list():
    pass

@mipl.production(Nonterminal.VARDECLST, ())
def p_variable_declaration_list_epsilon():
    pass

@mipl.production(Nonterminal.VARDEC, (
    Nonterminal.IDENT,
    Nonterminal.IDENTLST,
    Terminal.COLON,
    Nonterminal.TYPE
))
def p_variable_declaration():
    pass

@mipl.production(Nonterminal.IDENT, (Terminal.IDENT,))
def p_identifier():
    pass

@mipl.production(Nonterminal.IDENTLST, (
    Terminal.COMMA,
    Nonterminal.IDENT,
    Nonterminal.IDENTLST
))
def p_identifier_list():
    pass

@mipl.production(Nonterminal.IDENTLST, ())
def p_identifier_list_epsilon():
    pass

@mipl.production(Nonterminal.TYPE, (Nonterminal.SIMPLE,))
def p_type_simple():
    pass

@mipl.production(Nonterminal.TYPE, (Nonterminal.ARRAY,))
def p_type_array():
    pass

@mipl.production(Nonterminal.ARRAY, (
    Terminal.ARRAY,
    Terminal.LBRACK,
    Nonterminal.IDXRANGE,
    Terminal.RBRACK,
    Terminal.OF,
    Nonterminal.SIMPLE
))
def p_array():
    pass

@mipl.production(Nonterminal.IDX, (Terminal.INTCONST,))
def p_index():
    pass

@mipl.production(Nonterminal.IDXRANGE, (
    Nonterminal.IDX,
    Terminal.DOTDOT,
    Nonterminal.IDX
))
def p_index_range():
    pass

@mipl.production(Nonterminal.SIMPLE, (Terminal.INT,))
def p_simple_int():
    pass

@mipl.production(Nonterminal.SIMPLE, (Terminal.CHAR,))
def p_simple_int():
    pass

@mipl.production(Nonterminal.SIMPLE, (Terminal.BOOL,))
def p_simple_int():
    pass

@mipl.production(Nonterminal.PROCDECPART, (
    Nonterminal.PROCDEC,
    Terminal.SCOLON,
    Nonterminal.PROCDECPART
))
def p_procedure_part():
    pass

@mipl.production(Nonterminal.PROCDECPART, ())
def p_procedure_part_epsilon():
    pass

@mipl.production(Nonterminal.PROCDEC, (
    Nonterminal.PROCHDR,
    Nonterminal.BLOCK
))
def p_procedure():
    pass

@mipl.production(Nonterminal.PROCHDR, (
    Terminal.PROC,
    Terminal.IDENT,
    Terminal.SCOLON
))
def p_procedure_header():
    pass

@mipl.production(Nonterminal.STMTPART, (Nonterminal.COMPOUND,))
def p_statement_part():
    pass

@mipl.production(Nonterminal.COMPOUND, (
    Terminal.BEGIN,
    Nonterminal.STMT,
    Nonterminal.STMTLST,
    Terminal.END
))
def p_compound():
    pass

@mipl.production(Nonterminal.STMTLST, (
    Terminal.SCOLON,
    Nonterminal.STMT,
    Nonterminal.STMTLST
))
def p_statement_list():
    pass

@mipl.production(Nonterminal.STMTLST, ())
def p_statement_list_epsilon():
    pass

@mipl.production(Nonterminal.STMT, (Nonterminal.ASSIGN,))
def p_statement_assign():
    pass

@mipl.production(Nonterminal.STMT, (Nonterminal.READ,))
def p_statement_read():
    pass

@mipl.production(Nonterminal.STMT, (Nonterminal.WRITE,))
def p_statement_write():
    pass

@mipl.production(Nonterminal.STMT, (Nonterminal.CONDITION,))
def p_statement_condition():
    pass

@mipl.production(Nonterminal.STMT, (Nonterminal.WHILE,))
def p_statement_while():
    pass

@mipl.production(Nonterminal.STMT, (Nonterminal.COMPOUND,))
def p_statement_compound():
    pass

@mipl.production(Nonterminal.ASSIGN, (
    Nonterminal.VARIABLE,
    Terminal.ASSIGN,
    Nonterminal.EXPR
))
def p_assign():
    pass

@mipl.production(Nonterminal.PROCSTMT, (Nonterminal.PROCIDENT,))
def p_procedure_statement():
    pass

@mipl.production(Nonterminal.PROCIDENT, (Terminal.IDENT,))
def p_procedure_identifier():
    pass

@mipl.production(Nonterminal.READ, (
    Terminal.READ,
    Terminal.LPAREN,
    Nonterminal.INPUTVAR,
    Nonterminal.INPUTLST,
    Terminal.RPAREN
))
def p_read():
    pass

@mipl.production(Nonterminal.INPUTLST, (
    Terminal.COMMA,
    Nonterminal.INPUTVAR,
    Nonterminal.INPUTLST
))
def p_input_list():
    pass

@mipl.production(Nonterminal.INPUTLST, ())
def p_input_lst_epsilon():
    pass

@mipl.production(Nonterminal.INPUTVAR, (Nonterminal.VARIABLE,))
def p_input_variable():
    pass

@mipl.production(Nonterminal.WRITE, (
    Terminal.WRITE,
    Terminal.LPAREN,
    Nonterminal.OUTPUT,
    Nonterminal.OUTPUTLST,
    Terminal.RPAREN
))
def p_write():
    pass

@mipl.production(Nonterminal.OUTPUTLST, (
    Terminal.COMMA,
    Nonterminal.OUTPUT,
    Nonterminal.OUTPUTLST
))
def p_output_list():
    pass

@mipl.production(Nonterminal.OUTPUTLST, ())
def p_output_list_epsilon():
    pass

@mipl.production(Nonterminal.OUTPUT, (Nonterminal.EXPR,))
def p_output():
    pass

@mipl.production(Nonterminal.CONDITION, (
    Terminal.IF,
    Nonterminal.EXPR,
    Terminal.THEN,
    Nonterminal.STMT,
    Nonterminal.ELSEPART
))
def p_condition():
    pass

@mipl.production(Nonterminal.ELSEPART, (
    Terminal.ELSE,
    Nonterminal.STMT
))
def p_elsepart():
    pass

@mipl.production(Nonterminal.ELSEPART, ())
def p_elsepart_epsilon():
    pass

@mipl.production(Nonterminal.WHILE, (
    Terminal.WHILE,
    Nonterminal.EXPR,
    Terminal.DO,
    Nonterminal.STMT
))
def p_while():
    pass

@mipl.production(Nonterminal.EXPR, (
    Nonterminal.SIMPLEEXPR,
    Nonterminal.OPEXPR
))
def p_expr():
    pass

@mipl.production(Nonterminal.OPEXPR, (
    Nonterminal.RELOP,
    Nonterminal.SIMPLEEXPR
))
def p_opexpr():
    pass

@mipl.production(Nonterminal.OPEXPR, ())
def p_opexpr_epsilon():
    pass

@mipl.production(Nonterminal.SIMPLEEXPR, (
    Nonterminal.TERM,
    Nonterminal.ADDOPLST
))
def p_simpleexpr():
    pass

@mipl.production(Nonterminal.ADDOPLST, (
    Nonterminal.ADDOP,
    Nonterminal.TERM,
    Nonterminal.ADDOPLST
))
def p_addition_op_list():
    pass

@mipl.production(Nonterminal.ADDOPLST, ())
def p_addition_op_list_epsilon():
    pass

@mipl.production(Nonterminal.TERM, (
    Nonterminal.FACTOR,
    Nonterminal.MULTOPLST
))
def p_term():
    pass

@mipl.production(Nonterminal.MULTOPLST, (
    Nonterminal.MULTOP,
    Nonterminal.FACTOR,
    Nonterminal.MULTOPLST
))
def p_multiplication_op_list():
    pass

@mipl.production(Nonterminal.MULTOPLST, ())
def p_multiplication_op_list_epsilon():
    pass

@mipl.production(Nonterminal.FACTOR, (
    Nonterminal.SIGN,
    Nonterminal.VARIABLE
))
def p_factor_variable():
    pass

@mipl.production(Nonterminal.FACTOR, (
    Nonterminal.CONST,
))
def p_factor_const():
    pass

@mipl.production(Nonterminal.FACTOR, (
    Terminal.LPAREN,
    Nonterminal.EXPR,
    Terminal.RPAREN
))
def p_factor_expr():
    pass

@mipl.production(Nonterminal.FACTOR, (
    Terminal.NOT,
    Nonterminal.FACTOR
))
def p_factor_not():
    pass

@mipl.production(Nonterminal.SIGN, (Terminal.PLUS,))
def p_sign_plus():
    pass

@mipl.production(Nonterminal.SIGN, (Terminal.MINUS,))
def p_sign_minus():
    pass

@mipl.production(Nonterminal.SIGN, ())
def p_sign_epsilon():
    pass

@mipl.production(Nonterminal.ADDOP, (Terminal.PLUS,))
def p_addition_op_plus():
    pass

@mipl.production(Nonterminal.ADDOP, (Terminal.MINUS,))
def p_addition_op_minus():
    pass

@mipl.production(Nonterminal.ADDOP, (Terminal.OR,))
def p_addition_op_or():
    pass

@mipl.production(Nonterminal.MULTOP, (Terminal.MULT,))
def p_multiplication_op_mult():
    pass

@mipl.production(Nonterminal.MULTOP, (Terminal.DIV,))
def p_multiplication_op_div():
    pass

@mipl.production(Nonterminal.MULTOP, (Terminal.AND,))
def p_multiplication_op_and():
    pass

@mipl.production(Nonterminal.RELOP, (Terminal.LT,))
def p_relative_op_less():
    pass

@mipl.production(Nonterminal.RELOP, (Terminal.LE,))
def p_relative_op_lesseq():
    pass

@mipl.production(Nonterminal.RELOP, (Terminal.NE,))
def p_relative_op_noteq():
    pass

@mipl.production(Nonterminal.RELOP, (Terminal.EQ,))
def p_relative_op_eq():
    pass

@mipl.production(Nonterminal.RELOP, (Terminal.GT,))
def p_relative_op_greater():
    pass

@mipl.production(Nonterminal.RELOP, (Terminal.GE,))
def p_relative_op_greatereq():
    pass

@mipl.production(Nonterminal.VARIABLE, (
    Terminal.IDENT,
    Nonterminal.IDXVAR
))
def p_variable():
    pass

@mipl.production(Nonterminal.IDXVAR, (
    Terminal.LBRACK,
    Nonterminal.EXPR,
    Terminal.RBRACK
))
def p_index_variable():
    pass

@mipl.production(Nonterminal.IDXVAR, ())
def p_index_variable_epsilon():
    pass

@mipl.production(Nonterminal.CONST, (Terminal.INTCONST,))
def p_constant_int():
    pass

@mipl.production(Nonterminal.CONST, (Terminal.CHARCONST,))
def p_constant_char():
    pass

@mipl.production(Nonterminal.CONST, (Nonterminal.BOOLCONST,))
def p_constant_bool():
    pass

@mipl.production(Nonterminal.BOOLCONST, (Terminal.TRUE,))
def p_constant_true():
    pass

@mipl.production(Nonterminal.BOOLCONST, (Terminal.FALSE,))
def p_constant_false():
    pass
