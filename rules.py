from grammar import Grammar
from elements import Terminal, Nonterminal
from symbols import Symbol, SymbolTable, SymbolCat, SymbolType, MiplMultiplyDefinedIdentifierError, MiplUndeclaredIdentifierError

mipl = Grammar(Nonterminal, Terminal)

def print_symbol_add(sym: Symbol):
    t = sym.sym_cat if sym.sym_cat != SymbolCat.VARIABLE else sym.var_type

    out = f"+++ Adding {sym.sym_name} to symbol table with type {t.value}"

    if sym.sym_cat == SymbolCat.VARIABLE and sym.var_type == SymbolType.ARRAY:
        b = sym.array_bounds
        bt = sym.array_base_type
        out += f" {b[0]} .. {b[1]} of {bt.value}"

    print("\n" + out)    

@mipl.production(Nonterminal.PROGLBL, (Terminal.PROG,))
def p_program_label(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.PROG, (
    Nonterminal.PROGLBL,
    Terminal.IDENT,
    Terminal.SCOLON,
    Nonterminal.BLOCK,
    Terminal.DOT
))
def p_program(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    
    # print("prog_ident")
    prog_ident = prod.lexeme

    prod = yield

    symbol_table.scope_enter()

    sym = Symbol(prog_ident, SymbolCat.PROGRAM)
    print_symbol_add(sym)

    if prog_ident in symbol_table.this_scope():
        raise MiplMultiplyDefinedIdentifierError()

    symbol_table.new_id(sym)

    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.BLOCK, (
    Nonterminal.VARDECPART,
    Nonterminal.PROCDECPART,
    Nonterminal.STMTPART,
))
def p_block(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    symbol_table.scope_exit()
    yield

@mipl.production(Nonterminal.VARDECPART, (
    Terminal.VAR,
    Nonterminal.VARDEC,
    Terminal.SCOLON,
    Nonterminal.VARDECLST
))
def p_variable_declaration_part(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.VARDECPART, ())
def p_variable_declaration_part_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.VARDECLST, (
    Nonterminal.VARDEC,
    Terminal.SCOLON,
    Nonterminal.VARDECLST
))
def p_variable_declaration_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.VARDECLST, ())
def p_variable_declaration_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.VARDEC, (
    Nonterminal.IDENT,
    Nonterminal.IDENTLST,
    Terminal.COLON,
    Nonterminal.TYPE
))
def p_variable_declaration(symbol_table: SymbolTable):
    prod = yield
    # print("var_ident")
    var_idents = [prod]

    prod = yield
    var_idents += [] if prod is None else prod

    prod = yield
    line_number = prod.line_number

    prod = yield

    for ident in var_idents:
        sym = Symbol(ident, SymbolCat.VARIABLE, **prod)
        print_symbol_add(sym)

        if ident in symbol_table.this_scope():
            raise MiplMultiplyDefinedIdentifierError(f"Line {line_number}: Multiply defined identifier")

        symbol_table.new_id(sym)

    yield

@mipl.production(Nonterminal.IDENT, (Terminal.IDENT,))
def p_identifier(symbol_table: SymbolTable):
    prod = yield
    yield prod.lexeme

@mipl.production(Nonterminal.IDENTLST, (
    Terminal.COMMA,
    Nonterminal.IDENT,
    Nonterminal.IDENTLST
))
def p_identifier_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    ident = prod
    prod = yield
    idents = prod
    yield [ident, *idents]

@mipl.production(Nonterminal.IDENTLST, ())
def p_identifier_list_epsilon(symbol_table: SymbolTable):
    yield []
    yield 

@mipl.production(Nonterminal.TYPE, (Nonterminal.SIMPLE,))
def p_type_simple(symbol_table: SymbolTable):
    prod = yield
    yield {"type": prod}

@mipl.production(Nonterminal.TYPE, (Nonterminal.ARRAY,))
def p_type_array(symbol_table: SymbolTable):
    prod = yield
    a = {"type": SymbolType.ARRAY}
    yield {**a, **prod}

@mipl.production(Nonterminal.ARRAY, (
    Terminal.ARRAY,
    Terminal.LBRACK,
    Nonterminal.IDXRANGE,
    Terminal.RBRACK,
    Terminal.OF,
    Nonterminal.SIMPLE
))
def p_array(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    bounds = prod
    prod = yield
    prod = yield
    prod = yield
    base_type = prod
    yield {"bounds": bounds, "base_type": base_type}

@mipl.production(Nonterminal.IDX, (Terminal.INTCONST,))
def p_index(symbol_table: SymbolTable):
    prod = yield
    yield int(prod.lexeme)

@mipl.production(Nonterminal.IDXRANGE, (
    Nonterminal.IDX,
    Terminal.DOTDOT,
    Nonterminal.IDX
))
def p_index_range(symbol_table: SymbolTable):
    prod = yield
    left_bound = prod
    prod = yield
    prod = yield
    right_bound = prod
    yield (left_bound, right_bound)

@mipl.production(Nonterminal.SIMPLE, (Terminal.INT,))
def p_simple_int(symbol_table: SymbolTable):
    prod = yield
    yield SymbolType.INT

@mipl.production(Nonterminal.SIMPLE, (Terminal.CHAR,))
def p_simple_int(symbol_table: SymbolTable):
    prod = yield
    yield SymbolType.CHAR

@mipl.production(Nonterminal.SIMPLE, (Terminal.BOOL,))
def p_simple_int(symbol_table: SymbolTable):
    prod = yield
    yield SymbolType.BOOL

@mipl.production(Nonterminal.PROCDECPART, (
    Nonterminal.PROCDEC,
    Terminal.SCOLON,
    Nonterminal.PROCDECPART
))
def p_procedure_part(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.PROCDECPART, ())
def p_procedure_part_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.PROCDEC, (
    Nonterminal.PROCHDR,
    Nonterminal.BLOCK
))
def p_procedure(symbol_table: SymbolTable):
    prod = yield
    symbol_table.scope_enter()
    prod = yield
    yield

@mipl.production(Nonterminal.PROCHDR, (
    Terminal.PROC,
    Terminal.IDENT,
    Terminal.SCOLON
))
def p_procedure_header(symbol_table: SymbolTable):
    prod = yield
    prod = yield

    proc_ident = prod.lexeme
    line_number = prod.line_number

    sym = Symbol(proc_ident, SymbolCat.PROCEDURE, parameters=[])
    print_symbol_add(sym)

    if proc_ident in symbol_table.this_scope():
        raise MiplMultiplyDefinedIdentifierError(f"Line {line_number}: Multiply defined identifier")

    symbol_table.new_id(sym)

    prod = yield
    yield

@mipl.production(Nonterminal.STMTPART, (Nonterminal.COMPOUND,))
def p_statement_part(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.COMPOUND, (
    Terminal.BEGIN,
    Nonterminal.STMT,
    Nonterminal.STMTLST,
    Terminal.END
))
def p_compound(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.STMTLST, (
    Terminal.SCOLON,
    Nonterminal.STMT,
    Nonterminal.STMTLST
))
def p_statement_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.STMTLST, ())
def p_statement_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.STMT, (Nonterminal.ASSIGN,))
def p_statement_assign(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.STMT, (Nonterminal.READ,))
def p_statement_read(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.STMT, (Nonterminal.WRITE,))
def p_statement_write(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.STMT, (Nonterminal.CONDITION,))
def p_statement_condition(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.STMT, (Nonterminal.WHILE,))
def p_statement_while(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.STMT, (Nonterminal.COMPOUND,))
def p_statement_compound(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.ASSIGN, (
    Nonterminal.VARIABLE,
    Terminal.ASSIGN,
    Nonterminal.EXPR
))
def p_assign(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.PROCSTMT, (Nonterminal.PROCIDENT,))
def p_procedure_statement(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.PROCIDENT, (Terminal.IDENT,))
def p_procedure_identifier(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.READ, (
    Terminal.READ,
    Terminal.LPAREN,
    Nonterminal.INPUTVAR,
    Nonterminal.INPUTLST,
    Terminal.RPAREN
))
def p_read(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.INPUTLST, (
    Terminal.COMMA,
    Nonterminal.INPUTVAR,
    Nonterminal.INPUTLST
))
def p_input_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.INPUTLST, ())
def p_input_lst_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.INPUTVAR, (Nonterminal.VARIABLE,))
def p_input_variable(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.WRITE, (
    Terminal.WRITE,
    Terminal.LPAREN,
    Nonterminal.OUTPUT,
    Nonterminal.OUTPUTLST,
    Terminal.RPAREN
))
def p_write(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.OUTPUTLST, (
    Terminal.COMMA,
    Nonterminal.OUTPUT,
    Nonterminal.OUTPUTLST
))
def p_output_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.OUTPUTLST, ())
def p_output_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.OUTPUT, (Nonterminal.EXPR,))
def p_output(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.CONDITION, (
    Terminal.IF,
    Nonterminal.EXPR,
    Terminal.THEN,
    Nonterminal.STMT,
    Nonterminal.ELSEPART
))
def p_condition(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.ELSEPART, (
    Terminal.ELSE,
    Nonterminal.STMT
))
def p_elsepart(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.ELSEPART, ())
def p_elsepart_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.WHILE, (
    Terminal.WHILE,
    Nonterminal.EXPR,
    Terminal.DO,
    Nonterminal.STMT
))
def p_while(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.EXPR, (
    Nonterminal.SIMPLEEXPR,
    Nonterminal.OPEXPR
))
def p_expr(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.OPEXPR, (
    Nonterminal.RELOP,
    Nonterminal.SIMPLEEXPR
))
def p_opexpr(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.OPEXPR, ())
def p_opexpr_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.SIMPLEEXPR, (
    Nonterminal.TERM,
    Nonterminal.ADDOPLST
))
def p_simpleexpr(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.ADDOPLST, (
    Nonterminal.ADDOP,
    Nonterminal.TERM,
    Nonterminal.ADDOPLST
))
def p_addition_op_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.ADDOPLST, ())
def p_addition_op_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.TERM, (
    Nonterminal.FACTOR,
    Nonterminal.MULTOPLST
))
def p_term(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.MULTOPLST, (
    Nonterminal.MULTOP,
    Nonterminal.FACTOR,
    Nonterminal.MULTOPLST
))
def p_multiplication_op_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.MULTOPLST, ())
def p_multiplication_op_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.FACTOR, (
    Nonterminal.SIGN,
    Nonterminal.VARIABLE
))
def p_factor_variable(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.FACTOR, (
    Nonterminal.CONST,
))
def p_factor_const(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.FACTOR, (
    Terminal.LPAREN,
    Nonterminal.EXPR,
    Terminal.RPAREN
))
def p_factor_expr(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.FACTOR, (
    Terminal.NOT,
    Nonterminal.FACTOR
))
def p_factor_not(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.SIGN, (Terminal.PLUS,))
def p_sign_plus(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.SIGN, (Terminal.MINUS,))
def p_sign_minus(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.SIGN, ())
def p_sign_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.ADDOP, (Terminal.PLUS,))
def p_addition_op_plus(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.ADDOP, (Terminal.MINUS,))
def p_addition_op_minus(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.ADDOP, (Terminal.OR,))
def p_addition_op_or(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.MULTOP, (Terminal.MULT,))
def p_multiplication_op_mult(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.MULTOP, (Terminal.DIV,))
def p_multiplication_op_div(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.MULTOP, (Terminal.AND,))
def p_multiplication_op_and(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.RELOP, (Terminal.LT,))
def p_relative_op_less(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.RELOP, (Terminal.LE,))
def p_relative_op_lesseq(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.RELOP, (Terminal.NE,))
def p_relative_op_noteq(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.RELOP, (Terminal.EQ,))
def p_relative_op_eq(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.RELOP, (Terminal.GT,))
def p_relative_op_greater(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.RELOP, (Terminal.GE,))
def p_relative_op_greatereq(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.VARIABLE, (
    Terminal.IDENT,
    Nonterminal.IDXVAR
))
def p_variable(symbol_table: SymbolTable):
    prod = yield

    ident = prod.lexeme
    line_number = prod.line_number
    if ident not in symbol_table:
        raise MiplUndeclaredIdentifierError(f"Line {line_number}: Undefined identifier")

    prod = yield
    yield

@mipl.production(Nonterminal.IDXVAR, (
    Terminal.LBRACK,
    Nonterminal.EXPR,
    Terminal.RBRACK
))
def p_index_variable(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(Nonterminal.IDXVAR, ())
def p_index_variable_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(Nonterminal.CONST, (Terminal.INTCONST,))
def p_constant_int(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.CONST, (Terminal.CHARCONST,))
def p_constant_char(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.CONST, (Nonterminal.BOOLCONST,))
def p_constant_bool(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.BOOLCONST, (Terminal.TRUE,))
def p_constant_true(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(Nonterminal.BOOLCONST, (Terminal.FALSE,))
def p_constant_false(symbol_table: SymbolTable):
    prod = yield
    yield
