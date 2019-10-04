from grammar import Grammar
from elements import Terminal as T, Nonterminal as NT
from pprint import pprint
from symbols import Symbol, SymbolTable, SymbolCat, SymbolType, MiplMultiplyDefinedIdentifierError, MiplUndeclaredIdentifierError

mipl = Grammar(NT, T)

def print_symbol_add(sym: Symbol):
    t = sym.sym_cat if sym.sym_cat != SymbolCat.VARIABLE else sym.var_type

    out = f"+++ Adding {sym.sym_name} to symbol table with type {t.value}"

    if sym.sym_cat == SymbolCat.VARIABLE and sym.var_type == SymbolType.ARRAY:
        b = sym.array_bounds
        bt = sym.array_base_type
        out += f" {b[0]} .. {b[1]} of {bt.value}"

    # print("\n" + out)

@mipl.production(NT.PROGLBL, (T.PROG,))
def p_program_label(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.PROG, (
    NT.PROGLBL,
    T.IDENT,
    T.SCOLON,
    NT.BLOCK,
    T.DOT
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

@mipl.production(NT.BLOCK, (
    NT.VARDECPART,
    NT.PROCDECPART,
    NT.STMTPART,
))
def p_block(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    symbol_table.scope_exit()
    yield

@mipl.production(NT.VARDECPART, (
    T.VAR,
    NT.VARDEC,
    T.SCOLON,
    NT.VARDECLST
))
def p_variable_declaration_part(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.VARDECPART, ())
def p_variable_declaration_part_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.VARDECLST, (
    NT.VARDEC,
    T.SCOLON,
    NT.VARDECLST
))
def p_variable_declaration_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.VARDECLST, ())
def p_variable_declaration_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.VARDEC, (
    NT.IDENT,
    NT.IDENTLST,
    T.COLON,
    NT.TYPE
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

@mipl.production(NT.IDENT, (T.IDENT,))
def p_identifier(symbol_table: SymbolTable):
    prod = yield
    yield prod.lexeme

@mipl.production(NT.IDENTLST, (
    T.COMMA,
    NT.IDENT,
    NT.IDENTLST
))
def p_identifier_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    ident = prod
    prod = yield
    idents = prod
    yield [ident, *idents]

@mipl.production(NT.IDENTLST, ())
def p_identifier_list_epsilon(symbol_table: SymbolTable):
    yield []
    yield 

@mipl.production(NT.TYPE, (NT.SIMPLE,))
def p_type_simple(symbol_table: SymbolTable):
    prod = yield
    yield {"type": prod}

@mipl.production(NT.TYPE, (NT.ARRAY,))
def p_type_array(symbol_table: SymbolTable):
    prod = yield
    a = {"type": SymbolType.ARRAY}
    yield {**a, **prod}

@mipl.production(NT.ARRAY, (
    T.ARRAY,
    T.LBRACK,
    NT.IDXRANGE,
    T.RBRACK,
    T.OF,
    NT.SIMPLE
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

@mipl.production(NT.IDX, (T.INTCONST,))
def p_index(symbol_table: SymbolTable):
    prod = yield
    yield int(prod.lexeme)

@mipl.production(NT.IDXRANGE, (
    NT.IDX,
    T.DOTDOT,
    NT.IDX
))
def p_index_range(symbol_table: SymbolTable):
    prod = yield
    left_bound = prod
    prod = yield
    prod = yield
    right_bound = prod
    yield (left_bound, right_bound)

@mipl.production(NT.SIMPLE, (T.INT,))
def p_simple_int(symbol_table: SymbolTable):
    prod = yield
    yield SymbolType.INT

@mipl.production(NT.SIMPLE, (T.CHAR,))
def p_simple_int(symbol_table: SymbolTable):
    prod = yield
    yield SymbolType.CHAR

@mipl.production(NT.SIMPLE, (T.BOOL,))
def p_simple_int(symbol_table: SymbolTable):
    prod = yield
    yield SymbolType.BOOL

@mipl.production(NT.PROCDECPART, (
    NT.PROCDEC,
    T.SCOLON,
    NT.PROCDECPART
))
def p_procedure_part(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.PROCDECPART, ())
def p_procedure_part_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.PROCDEC, (
    NT.PROCHDR,
    NT.BLOCK
))
def p_procedure(symbol_table: SymbolTable):
    prod = yield
    symbol_table.scope_enter()
    prod = yield
    yield

@mipl.production(NT.PROCHDR, (
    T.PROC,
    T.IDENT,
    T.SCOLON
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

@mipl.production(NT.STMTPART, (NT.COMPOUND,))
def p_statement_part(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.COMPOUND, (
    T.BEGIN,
    NT.STMT,
    NT.STMTLST,
    T.END
))
def p_compound(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.STMTLST, (
    T.SCOLON,
    NT.STMT,
    NT.STMTLST
))
def p_statement_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.STMTLST, ())
def p_statement_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.hint(NT.STMT)
def h_ident(possible, token, symbol_table):
    ident = token.lexeme

    if ident not in symbol_table:
        return possible[0]

    sym_cat = symbol_table[ident].sym_cat

    if sym_cat == SymbolCat.PROCEDURE and (NT.PROCSTMT,) in possible:
        return (NT.PROCSTMT,)
    
    else:
        return possible[0]

@mipl.production(NT.STMT, (NT.ASSIGN,))
def p_statement_assign(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.PROCSTMT,))
def p_statement_procedure(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.READ,))
def p_statement_read(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.WRITE,))
def p_statement_write(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.CONDITION,))
def p_statement_condition(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.WHILE,))
def p_statement_while(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.COMPOUND,))
def p_statement_compound(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.ASSIGN, (
    NT.VARIABLE,
    T.ASSIGN,
    NT.EXPR
))
def p_assign(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.PROCSTMT, (NT.PROCIDENT,))
def p_procedure_statement(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.PROCIDENT, (T.IDENT,))
def p_procedure_identifier(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.READ, (
    T.READ,
    T.LPAREN,
    NT.INPUTVAR,
    NT.INPUTLST,
    T.RPAREN
))
def p_read(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.INPUTLST, (
    T.COMMA,
    NT.INPUTVAR,
    NT.INPUTLST
))
def p_input_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.INPUTLST, ())
def p_input_lst_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.INPUTVAR, (NT.VARIABLE,))
def p_input_variable(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.WRITE, (
    T.WRITE,
    T.LPAREN,
    NT.OUTPUT,
    NT.OUTPUTLST,
    T.RPAREN
))
def p_write(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.OUTPUTLST, (
    T.COMMA,
    NT.OUTPUT,
    NT.OUTPUTLST
))
def p_output_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.OUTPUTLST, ())
def p_output_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.OUTPUT, (NT.EXPR,))
def p_output(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.CONDITION, (
    T.IF,
    NT.EXPR,
    T.THEN,
    NT.STMT,
    NT.ELSEPART
))
def p_condition(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.ELSEPART, (
    T.ELSE,
    NT.STMT
))
def p_elsepart(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(NT.ELSEPART, ())
def p_elsepart_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.WHILE, (
    T.WHILE,
    NT.EXPR,
    T.DO,
    NT.STMT
))
def p_while(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.EXPR, (
    NT.SIMPLEEXPR,
    NT.OPEXPR
))
def p_expr(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(NT.OPEXPR, (
    NT.RELOP,
    NT.SIMPLEEXPR
))
def p_opexpr(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(NT.OPEXPR, ())
def p_opexpr_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.SIMPLEEXPR, (
    NT.TERM,
    NT.ADDOPLST
))
def p_simpleexpr(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(NT.ADDOPLST, (
    NT.ADDOP,
    NT.TERM,
    NT.ADDOPLST
))
def p_addition_op_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.ADDOPLST, ())
def p_addition_op_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.TERM, (
    NT.FACTOR,
    NT.MULTOPLST
))
def p_term(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(NT.MULTOPLST, (
    NT.MULTOP,
    NT.FACTOR,
    NT.MULTOPLST
))
def p_multiplication_op_list(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.MULTOPLST, ())
def p_multiplication_op_list_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.FACTOR, (
    NT.SIGN,
    NT.VARIABLE
))
def p_factor_variable(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(NT.FACTOR, (
    NT.CONST,
))
def p_factor_const(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.FACTOR, (
    T.LPAREN,
    NT.EXPR,
    T.RPAREN
))
def p_factor_expr(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.FACTOR, (
    T.NOT,
    NT.FACTOR
))
def p_factor_not(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    yield

@mipl.production(NT.SIGN, (T.PLUS,))
def p_sign_plus(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.SIGN, (T.MINUS,))
def p_sign_minus(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.SIGN, ())
def p_sign_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.ADDOP, (T.PLUS,))
def p_addition_op_plus(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.ADDOP, (T.MINUS,))
def p_addition_op_minus(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.ADDOP, (T.OR,))
def p_addition_op_or(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.MULTOP, (T.MULT,))
def p_multiplication_op_mult(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.MULTOP, (T.DIV,))
def p_multiplication_op_div(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.MULTOP, (T.AND,))
def p_multiplication_op_and(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.RELOP, (T.LT,))
def p_relative_op_less(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.RELOP, (T.LE,))
def p_relative_op_lesseq(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.RELOP, (T.NE,))
def p_relative_op_noteq(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.RELOP, (T.EQ,))
def p_relative_op_eq(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.RELOP, (T.GT,))
def p_relative_op_greater(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.RELOP, (T.GE,))
def p_relative_op_greatereq(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.VARIABLE, (
    T.IDENT,
    NT.IDXVAR
))
def p_variable(symbol_table: SymbolTable):
    prod = yield

    ident = prod.lexeme
    line_number = prod.line_number
    if ident not in symbol_table:
        raise MiplUndeclaredIdentifierError(f"Line {line_number}: Undefined identifier")

    prod = yield
    yield

@mipl.production(NT.IDXVAR, (
    T.LBRACK,
    NT.EXPR,
    T.RBRACK
))
def p_index_variable(symbol_table: SymbolTable):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.IDXVAR, ())
def p_index_variable_epsilon(symbol_table: SymbolTable):
    yield
    yield

@mipl.production(NT.CONST, (T.INTCONST,))
def p_constant_int(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.CONST, (T.CHARCONST,))
def p_constant_char(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.CONST, (NT.BOOLCONST,))
def p_constant_bool(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.BOOLCONST, (T.TRUE,))
def p_constant_true(symbol_table: SymbolTable):
    prod = yield
    yield

@mipl.production(NT.BOOLCONST, (T.FALSE,))
def p_constant_false(symbol_table: SymbolTable):
    prod = yield
    yield
