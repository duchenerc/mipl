from grammar import Grammar
from elements import Terminal as T, Nonterminal as NT
from symbols import *
from oal import OalWriter, OalOpCode as OpCode

# The number of words to allow for the display
DISPLAY = 20

# Maps token types to OAL op codes
OPS_OP_CODES = {
    T.PLUS: OpCode.ADD,
    T.MINUS: OpCode.SUB,
    T.MULT: OpCode.MULT,
    T.DIV: OpCode.DIV,
    T.AND: OpCode.AND,
    T.OR: OpCode.OR,
    T.GT: OpCode.GT,
    T.LT: OpCode.LT,
    T.GE: OpCode.GE,
    T.LE: OpCode.LE,
    T.EQ: OpCode.EQ,
    T.NE: OpCode.NE
}

class MiplSemanticError(Exception):
    pass

def print_symbol_add(sym: Symbol):
    t = sym.sym_cat if sym.sym_cat != SymbolCat.VARIABLE else sym.var_type

    out = f"+++ Adding {sym.sym_name} to symbol table with type {t.value}"

    if sym.sym_cat == SymbolCat.VARIABLE and sym.var_type == SymbolType.ARRAY:
        b = sym.array_bounds
        bt = sym.array_base_type
        out += f" {b[0]} .. {b[1]} of {bt.value}"

    # print("\n" + out)

mipl = Grammar(NT, T)

symbol_table = SymbolTable()

oal = OalWriter()
l_globals = oal.label_id()
l_stack = oal.label_id()
l_code = oal.label_id()
l_entry = oal.label_id()

@mipl.production(NT.PROGLBL, (T.PROG,))
def p_program_label(*args):
    prod = yield
    yield

@mipl.production(NT.PROG, (
    NT.PROGLBL,
    T.IDENT,
    T.SCOLON,
    NT.BLOCK,
    T.DOT
))
def p_program(*args):

    # set up oal output
    init_instrx = (
        oal.label(l_globals),
        DISPLAY,
        oal.label(l_stack),
        oal.label(l_code),
        oal.label(l_entry)
    )

    oal.add_instrx(OpCode.INIT, init_instrx)

    prod = yield
    prod = yield
    
    prog_ident = prod.lexeme

    prod = yield

    symbol_table.scope_enter()

    sym = Symbol(prog_ident, SymbolCat.PROGRAM)
    sym.linkage["label"] = l_entry
    print_symbol_add(sym)

    if prog_ident in symbol_table.this_scope():
        raise MiplMultiplyDefinedIdentifierError()

    symbol_table.new_id(sym)

    # send symbol to new block
    prod = yield sym
    prod = yield

    # finish oal output
    oal.add_instrx(OpCode.HALT)
    oal.add_instrx(OpCode.BSS, (500), label=l_stack)
    oal.add_instrx(OpCode.END)

    yield



@mipl.production(NT.BLOCK, (
    NT.VARDECPART,
    NT.PROCDECPART,
    NT.STMTPART,
))
def p_block(*args):
    global symbol_table

    # get the symbol of the parent
    # (the program or procedure that owns this block)
    sym = args[0]

    # get the offset for this block
    # if we're in the root scope, give some room for the display
    offset_start = DISPLAY if sym.sym_cat == SymbolCat.PROGRAM else 0

    # get number of words from variable declarations
    num_words = yield offset_start

    if sym.sym_cat == SymbolCat.PROGRAM:
        oal.add_instrx(OpCode.BSS, (num_words + DISPLAY), label=l_globals)
        oal.add_instrx(OpCode.NONE, label=l_code)
    
    elif sym.sym_cat == SymbolCat.PROCEDURE:
        sym.linkage["words"] = num_words
        symbol_table.overwrite_id(sym)

    prod = yield

    # send the parent symbol down to the statements for thie block
    prod = yield sym

    symbol_table.scope_exit()
    yield sym



@mipl.production(NT.VARDECPART, (
    T.VAR,
    NT.VARDEC,
    T.SCOLON,
    NT.VARDECLST
))
def p_variable_declaration_part(*args):
    # get the starting offset at which we can store vars
    offset = args[0]

    prod = yield

    # send the offset to the vardec
    # it will declare its vars starting at this offset
    # the vardec passes back the number of words it occupies
    words_head = yield offset
    prod = yield
    
    # offset + words_head -> the starting offset for the next vardec
    # get back the total number of words used in the tail
    words_tail = yield offset + words_head

    # return the total number of words used
    yield words_head + words_tail

@mipl.production(NT.VARDECPART, ())
def p_variable_declaration_part_epsilon(*args):
    # empty, so occupies no words
    yield 0

@mipl.production(NT.VARDECLST, (
    NT.VARDEC,
    T.SCOLON,
    NT.VARDECLST
))
def p_variable_declaration_list(*args):
    # get the starting offset at which we can store vars
    offset = args[0]

    # send the offset to the vardec
    # it will declare its vars starting at this offset
    # the vardec passes back the number of words it occupies
    words_head = yield offset
    prod = yield

    # offset + words_head -> the starting offset for the next vardec
    # get back the total number of words used in the tail
    words_tail = yield offset + words_head

    # return the total number of words used
    yield words_head + words_tail

@mipl.production(NT.VARDECLST, ())
def p_variable_declaration_list_epsilon(*args):
    # empty, so occupies no words
    yield 0

@mipl.production(NT.VARDEC, (
    NT.IDENT,
    NT.IDENTLST,
    T.COLON,
    NT.TYPE
))
def p_variable_declaration(*args):
    # get the starting offset
    # we can declare variables beginning at this offset
    offset = args[0]

    # get the first identifier
    prod = yield
    var_idents = [prod]

    # get the rest of the identifiers and merge all identifiers
    prod = yield
    var_idents += [] if prod is None else prod

    tok = yield
    line_number = tok.line_number

    # get the delcared type
    # this comes with information about base_type, bounds, and words
    var_type = yield

    # get the nesting level for one down
    # if this is global, our offset starts at 20
    nesting_level = symbol_table.nesting_level()

    for ident in var_idents:
        sym = Symbol(ident, SymbolCat.VARIABLE, **var_type)
        sym.linkage["nesting_level"] = nesting_level
        sym.linkage["offset"] = offset
        print_symbol_add(sym)

        if ident in symbol_table.this_scope():
            raise MiplMultiplyDefinedIdentifierError(f"Line {line_number}: Multiply defined identifier")

        symbol_table.new_id(sym)

        # advance the offset in preparation for the next ident
        offset += var_type["words"]

    # yield number of words this variable declaration needs
    yield var_type["words"] * len(var_idents)

@mipl.production(NT.IDENT, (T.IDENT,))
def p_identifier(*args):
    prod = yield
    yield prod.lexeme

@mipl.production(NT.IDENTLST, (
    T.COMMA,
    NT.IDENT,
    NT.IDENTLST
))
def p_identifier_list(*args):
    prod = yield
    ident = yield
    idents = yield
    yield [ident, *idents]

@mipl.production(NT.IDENTLST, ())
def p_identifier_list_epsilon(*args):
    yield []

@mipl.production(NT.TYPE, (NT.SIMPLE,))
def p_type_simple(*args):
    simple_type = yield

    # give back the type and number of words
    yield {
        "type": simple_type,
        "words": 1
    }

@mipl.production(NT.TYPE, (NT.ARRAY,))
def p_type_array(*args):
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
def p_array(*args):
    prod = yield
    prod = yield
    bounds = yield
    prod = yield
    prod = yield
    prod = yield
    base_type = prod

    yield {
        **bounds,
        "base_type": base_type,
    }

@mipl.production(NT.IDX, (T.INTCONST,))
def p_index(*args):
    prod = yield
    yield int(prod.lexeme)

@mipl.production(NT.IDXRANGE, (
    NT.IDX,
    T.DOTDOT,
    NT.IDX
))
def p_index_range(*args):
    left_bound = yield
    tok = yield
    right_bound = yield

    line_number = tok.line_number

    if left_bound > right_bound:
        raise MiplSemanticError(f"Line {line_number}: Start index must be less than or equal to end index of array")

    bounds = (left_bound, right_bound)

    yield {
        "bounds": bounds,
        "words": right_bound - left_bound + 1
    }

@mipl.production(NT.SIMPLE, (T.INT,))
def p_simple_int(*args):
    prod = yield
    yield SymbolType.INT

@mipl.production(NT.SIMPLE, (T.CHAR,))
def p_simple_int(*args):
    prod = yield
    yield SymbolType.CHAR

@mipl.production(NT.SIMPLE, (T.BOOL,))
def p_simple_int(*args):
    prod = yield
    yield SymbolType.BOOL

@mipl.production(NT.PROCDECPART, (
    NT.PROCDEC,
    T.SCOLON,
    NT.PROCDECPART
))
def p_procedure_part(*args):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.PROCDECPART, ())
def p_procedure_part_epsilon(*args):
    yield
    yield

@mipl.production(NT.PROCDEC, (
    NT.PROCHDR,
    NT.BLOCK
))
def p_procedure(*args):
    sym = yield

    symbol_table.scope_enter()

    prod = yield sym
    yield

@mipl.production(NT.PROCHDR, (
    T.PROC,
    T.IDENT,
    T.SCOLON
))
def p_procedure_header(*args):
    prod = yield
    prod = yield

    proc_ident = prod.lexeme
    line_number = prod.line_number

    sym = Symbol(proc_ident, SymbolCat.PROCEDURE, parameters=[])
    # this procedure is nested one level underneath the current one
    sym.linkage["nesting_level"] = symbol_table.nesting_level() + 1
    sym.linkage["label"] = oal.label_id()
    print_symbol_add(sym)

    if proc_ident in symbol_table.this_scope():
        raise MiplMultiplyDefinedIdentifierError(f"Line {line_number}: Multiply defined identifier")

    symbol_table.new_id(sym)

    prod = yield

    # give back the symbol used to create this procedure
    yield sym

@mipl.production(NT.STMTPART, (NT.COMPOUND,))
def p_statement_part(*args):

    sym = args[0]

    label = sym.linkage.get("label", l_entry)
    level = sym.linkage.get("nesting_level", 0)
    frame_size = sym.linkage.get("words", DISPLAY)

    # if we're executing a procedure,
    # save the current execution pointer so we can return to it
    if label != l_entry:
        oal.add_instrx(OpCode.SAVE, (level, 0), label=label)

        # if this new procedure declares any varibles, move stack pointer
        if frame_size > 0:
            oal.add_instrx(OpCode.ADD_STACK_PTR, (frame_size))
    
    else:
        oal.add_instrx(OpCode.NONE, label=l_entry)

    oal.add_comment("Beginning of block's N_STMTPART")

    prod = yield

    # reload execution pointer
    if label != l_entry:
        if frame_size > 0:
            oal.add_instrx(OpCode.ADD_STACK_PTR, (-1 * frame_size))
        oal.add_instrx(OpCode.JUMP_INSTR, ())

    yield

@mipl.production(NT.COMPOUND, (
    T.BEGIN,
    NT.STMT,
    NT.STMTLST,
    T.END
))
def p_compound(*args):
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
def p_statement_list(*args):
    prod = yield
    prod = yield
    prod = yield
    yield

@mipl.production(NT.STMTLST, ())
def p_statement_list_epsilon(*args):
    yield

@mipl.hint(NT.STMT)
def h_ident(possible, token):
    ident = token.lexeme

    if ident not in symbol_table:
        return possible[0]

    sym_cat = symbol_table[ident].sym_cat

    if sym_cat == SymbolCat.PROCEDURE and (NT.PROCSTMT,) in possible:
        return (NT.PROCSTMT,)
    
    else:
        return possible[0]

@mipl.production(NT.STMT, (NT.ASSIGN,))
def p_statement_assign(*args):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.PROCSTMT,))
def p_statement_procedure(*args):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.READ,))
def p_statement_read(*args):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.WRITE,))
def p_statement_write(*args):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.CONDITION,))
def p_statement_condition(*args):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.WHILE,))
def p_statement_while(*args):
    prod = yield
    yield

@mipl.production(NT.STMT, (NT.COMPOUND,))
def p_statement_compound(*args):
    prod = yield
    yield

@mipl.production(NT.ASSIGN, (
    NT.VARIABLE,
    T.ASSIGN,
    NT.EXPR
))
def p_assign(*args):
    var_type = yield
    tok = yield
    line_number = tok.line_number

    expr_type = yield

    if var_type != expr_type:

        if SymbolType.ARRAY in (var_type, expr_type):
            raise MiplSemanticError(f"Line {line_number}: Array variable must be indexed")
        else:

            raise MiplSemanticError(f"Line {line_number}: Expression must be of same type as variable")

    oal.add_instrx(OpCode.STORE)

    yield

@mipl.production(NT.PROCSTMT, (NT.PROCIDENT,))
def p_procedure_statement(*args):
    global symbol_table
    ident = yield

    # get the nesting level of this procedure
    caller_level = symbol_table.nesting_level()
    
    # get the nesting level of the procedure to be called
    sym = symbol_table[ident]
    callee_level = sym.linkage["nesting_level"]
    callee_label = sym.linkage["label"]

    # if the caller's nesting level is at least the callee's nesting level,
    # then the caller is nested inside the callee
    # need to unload everything
    if caller_level >= callee_level:
        for i in reversed(range(callee_level, caller_level+1)):
            oal.add_instrx(OpCode.PUSH, (i, 0))

    oal.add_instrx(OpCode.JUMP_STACK, oal.label(callee_label))

    # reload everything
    if caller_level >= callee_level:
        for i in range(callee_level, caller_level+1):
            oal.add_instrx(OpCode.POP, (i, 0))

    yield

@mipl.production(NT.PROCIDENT, (T.IDENT,))
def p_procedure_identifier(*args):
    tok = yield
    yield tok.lexeme

@mipl.production(NT.READ, (
    T.READ,
    T.LPAREN,
    NT.INPUTVAR,
    NT.INPUTLST,
    T.RPAREN
))
def p_read(*args):
    prod = yield
    tok = yield
    line_number = tok.line_number

    var_type = yield

    if var_type not in (SymbolType.INT, SymbolType.CHAR):
        raise MiplSemanticError(f"Line {line_number}: Input variable must be of type integer or char")
    
    # read in and store
    if var_type == SymbolType.INT:
        oal.add_instrx(OpCode.READ_INT)

    elif var_type == SymbolType.CHAR:
        oal.add_instrx(OpCode.READ_CHAR)
    
    oal.add_instrx(OpCode.STORE)

    prod = yield
    prod = yield
    yield

@mipl.production(NT.INPUTLST, (
    T.COMMA,
    NT.INPUTVAR,
    NT.INPUTLST
))
def p_input_list(*args):
    tok = yield
    line_number = tok.line_number
    var_type = yield

    if var_type not in (SymbolType.INT, SymbolType.CHAR):
        raise MiplSemanticError(f"Line {line_number}: Input variable must be of type integer or type char")

    # read in and store
    if var_type == SymbolType.INT:
        oal.add_instrx(OpCode.READ_INT)

    elif var_type == SymbolType.CHAR:
        oal.add_instrx(OpCode.READ_CHAR)
    
    oal.add_instrx(OpCode.STORE)

    prod = yield
    yield

@mipl.production(NT.INPUTLST, ())
def p_input_lst_epsilon(*args):
    yield

@mipl.production(NT.INPUTVAR, (NT.VARIABLE,))
def p_input_variable(*args):
    var_type = yield
    yield var_type

@mipl.production(NT.WRITE, (
    T.WRITE,
    T.LPAREN,
    NT.OUTPUT,
    NT.OUTPUTLST,
    T.RPAREN
))
def p_write(*args):
    prod = yield
    tok = yield
    line_number = tok.line_number
    output_type = yield

    if output_type not in (SymbolType.INT, SymbolType.CHAR):
        raise MiplSemanticError(f"Line {line_number}: Output expression must be of type integer or char")
    
    # output
    if output_type == SymbolType.INT:
        oal.add_instrx(OpCode.WRITE_INT)
        
    elif output_type == SymbolType.CHAR:
        oal.add_instrx(OpCode.WRITE_CHAR)

    prod = yield
    prod = yield
    yield

@mipl.production(NT.OUTPUTLST, (
    T.COMMA,
    NT.OUTPUT,
    NT.OUTPUTLST
))
def p_output_list(*args):
    tok = yield
    line_number = tok.line_number

    output_type = yield

    if output_type not in (SymbolType.INT, SymbolType.CHAR):
        raise MiplSemanticError(f"Line {line_number}: Output expression must be of type integer or char")
    
    # output
    if output_type == SymbolType.INT:
        oal.add_instrx(OpCode.WRITE_INT)

    elif output_type == SymbolType.CHAR:
        oal.add_instrx(OpCode.WRITE_CHAR)

    prod = yield
    yield

@mipl.production(NT.OUTPUTLST, ())
def p_output_list_epsilon(*args):
    yield

@mipl.production(NT.OUTPUT, (NT.EXPR,))
def p_output(*args):
    expr_type = yield
    yield expr_type

@mipl.production(NT.CONDITION, (
    T.IF,
    NT.EXPR,
    T.THEN,
    NT.STMT,
    NT.ELSEPART
))
def p_condition(*args):

    # generate labels
    label_else = oal.label_id()
    label_post = oal.label_id()

    tok = yield
    line_number = tok.line_number

    # load conditional expr
    condition_type = yield

    if condition_type != SymbolType.BOOL:
        raise MiplSemanticError(f"Line {line_number}: Expression must be of type boolean")
    
    # if the conditional is false, jump to else part
    oal.add_instrx(OpCode.JUMP_FALSE, (oal.label(label_else)))

    prod = yield

    # execute if part
    prod = yield

    # if part is finished, jump to end
    oal.add_instrx(OpCode.JUMP, (oal.label(label_post)))
    oal.add_instrx(OpCode.NONE, label=label_else)

    # execute else part
    prod = yield

    # else part is finished
    # add jump point for if part
    oal.add_instrx(OpCode.NONE, label=label_post)

    yield

@mipl.production(NT.ELSEPART, (
    T.ELSE,
    NT.STMT
))
def p_elsepart(*args):
    prod = yield
    prod = yield
    yield

@mipl.production(NT.ELSEPART, ())
def p_elsepart_epsilon(*args):
    yield

@mipl.production(NT.WHILE, (
    T.WHILE,
    NT.EXPR,
    T.DO,
    NT.STMT
))
def p_while(*args):

    # generate labels
    label_top = oal.label_id()
    label_post = oal.label_id()

    tok = yield

    line_number = tok.line_number

    # label top of loop
    oal.add_instrx(OpCode.NONE, label=label_top)

    # execute conditional expr
    expr_type = yield

    if expr_type != SymbolType.BOOL:
        raise MiplSemanticError(f"Line {line_number}: Expression must be of type boolean")
    
    # if the conditional is false, jump to end
    oal.add_instrx(OpCode.JUMP_FALSE, (oal.label(label_post)))

    prod = yield

    # execute main block
    prod = yield

    # jump to top of loop
    oal.add_instrx(OpCode.JUMP, (oal.label(label_top)))
    oal.add_instrx(OpCode.NONE, label=label_post)

    yield






@mipl.production(NT.EXPR, (
    NT.SIMPLEEXPR,
    NT.OPEXPR
))
def p_expr(*args):
    lhs_type = yield
    rhs_type, line_number = yield

    # fast return if there's no opexpr
    if rhs_type == SymbolType.INVALID:
        yield lhs_type

    if rhs_type != lhs_type:
        raise MiplSemanticError(f"Line {line_number}: Expressions must both be int, or both char, or both boolean")
    
    # if there's a relop, we return bool
    yield SymbolType.BOOL

@mipl.production(NT.OPEXPR, (
    NT.RELOP,
    NT.SIMPLEEXPR
))
def p_opexpr(*args):
    op, line_number = yield
    expr_type = yield

    oal.add_instrx(OPS_OP_CODES[op])

    yield expr_type, line_number

@mipl.production(NT.OPEXPR, ())
def p_opexpr_epsilon(*args):
    yield SymbolType.INVALID, -1

@mipl.production(NT.SIMPLEEXPR, (
    NT.TERM,
    NT.ADDOPLST
))
def p_simpleexpr(*args):
    term_type = yield
    op_list = yield
    yield term_type

@mipl.production(NT.ADDOPLST, (
    NT.ADDOP,
    NT.TERM,
    NT.ADDOPLST
))
def p_addition_op_list(*args):
    op, line_number = yield
    term_type = yield
    op_list = yield

    is_mathexpr = op in (T.PLUS, T.MINUS)

    if is_mathexpr and term_type != SymbolType.INT:
        raise MiplSemanticError(f"Line {line_number}: Expression must be of type integer")
    
    oal.add_instrx(OPS_OP_CODES[op])

    yield SymbolType.INT if is_mathexpr else SymbolType.BOOL

@mipl.production(NT.ADDOPLST, ())
def p_addition_op_list_epsilon(*args):
    yield

@mipl.production(NT.TERM, (
    NT.FACTOR,
    NT.MULTOPLST
))
def p_term(*args):
    factor_type = yield
    op_list = yield
    yield factor_type

@mipl.production(NT.MULTOPLST, (
    NT.MULTOP,
    NT.FACTOR,
    NT.MULTOPLST
))
def p_multiplication_op_list(*args):
    op, line_number = yield
    factor_type = yield
    op_list = yield
    
    is_math_expr = op in (T.MULT, T.DIV)

    if is_math_expr and factor_type != SymbolType.INT:
        raise MiplSemanticError(f"Line {line_number}: Expression must be of type integer")
    
    oal.add_instrx(OPS_OP_CODES[op])

    yield SymbolType.INT if is_math_expr else SymbolType.BOOL

@mipl.production(NT.MULTOPLST, ())
def p_multiplication_op_list_epsilon(*args):
    yield

@mipl.production(NT.FACTOR, (
    NT.SIGN,
    NT.VARIABLE
))
def p_factor_variable(*args):
    sign, line_number = yield
    var_type = yield

    if sign is not None and var_type != SymbolType.INT:
        raise MiplSemanticError(f"Line {line_number}: Expression must be of type integer")
    
    # we know we're going to need the value inside this variable,
    # so deref it.
    # this is the only place necessary to deref
    oal.add_instrx(OpCode.DEREF)

    if sign == T.MINUS:
        oal.add_instrx(OpCode.NEG)

    yield var_type

@mipl.production(NT.FACTOR, (
    NT.CONST,
))
def p_factor_const(*args):
    const_type = yield
    yield const_type

@mipl.production(NT.FACTOR, (
    T.LPAREN,
    NT.EXPR,
    T.RPAREN
))
def p_factor_expr(*args):
    prod = yield
    expr_type = yield
    prod = yield
    yield expr_type

@mipl.production(NT.FACTOR, (
    T.NOT,
    NT.FACTOR
))
def p_factor_not(*args):
    tok = yield
    line_number = tok.line_number

    factor_type = yield

    if factor_type != SymbolType.BOOL:
        raise MiplSemanticError(f"Line {line_number}: Expression must be of type boolean")

    oal.add_instrx(OpCode.NOT)

    yield SymbolType.BOOL

@mipl.production(NT.SIGN, (T.PLUS,))
def p_sign_plus(*args):
    tok = yield
    yield T.PLUS, tok.line_number

@mipl.production(NT.SIGN, (T.MINUS,))
def p_sign_minus(*args):
    tok = yield
    yield T.MINUS, tok.line_number

@mipl.production(NT.SIGN, ())
def p_sign_epsilon(*args):
    yield None, -1

@mipl.production(NT.ADDOP, (T.PLUS,))
def p_addition_op_plus(*args):
    tok = yield
    yield T.PLUS, tok.line_number

@mipl.production(NT.ADDOP, (T.MINUS,))
def p_addition_op_minus(*args):
    tok = yield
    yield T.MINUS, tok.line_number

@mipl.production(NT.ADDOP, (T.OR,))
def p_addition_op_or(*args):
    tok = yield
    yield T.OR, tok.line_number

@mipl.production(NT.MULTOP, (T.MULT,))
def p_multiplication_op_mult(*args):
    tok = yield
    yield T.MULT, tok.line_number

@mipl.production(NT.MULTOP, (T.DIV,))
def p_multiplication_op_div(*args):
    tok = yield
    yield T.DIV, tok.line_number

@mipl.production(NT.MULTOP, (T.AND,))
def p_multiplication_op_and(*args):
    tok = yield
    yield T.AND, tok.line_number

@mipl.production(NT.RELOP, (T.LT,))
def p_relative_op_less(*args):
    tok = yield
    yield T.LT, tok.line_number

@mipl.production(NT.RELOP, (T.LE,))
def p_relative_op_lesseq(*args):
    tok = yield
    yield T.LE, tok.line_number

@mipl.production(NT.RELOP, (T.NE,))
def p_relative_op_noteq(*args):
    tok = yield
    yield T.NE, tok.line_number

@mipl.production(NT.RELOP, (T.EQ,))
def p_relative_op_eq(*args):
    tok = yield
    yield T.EQ, tok.line_number

@mipl.production(NT.RELOP, (T.GT,))
def p_relative_op_greater(*args):
    tok = yield
    yield T.GT, tok.line_number

@mipl.production(NT.RELOP, (T.GE,))
def p_relative_op_greatereq(*args):
    tok = yield
    yield T.GE, tok.line_number

@mipl.production(NT.VARIABLE, (
    T.IDENT,
    NT.IDXVAR
))
def p_variable(*args):
    tok = yield

    ident = tok.lexeme
    line_number = tok.line_number
    if ident not in symbol_table:
        raise MiplUndeclaredIdentifierError(f"Line {line_number}: Undefined identifier")

    sym: Symbol = symbol_table[ident]

    if sym.sym_cat == SymbolCat.PROCEDURE:
        raise MiplSemanticError(f"Line {line_number}: Procedure/variable mismatch")

    offset = sym.linkage["offset"]
    nesting_level = sym.linkage["nesting_level"]

    # if this is an array, the offset is going to be off by the starting index
    # TODO: this should be stored correctly to begin with
    if sym.var_type == SymbolType.ARRAY:
        start_index = sym.array_bounds[0]
        offset -= start_index
    
    oal.add_instrx(OpCode.LOAD_ADDR, (offset, nesting_level))

    indexed = yield

    if indexed and sym.var_type != SymbolType.ARRAY:
        raise MiplSemanticError(f"Line {line_number}: Indexed variable must be of array type")
    
    # if we're accessing an element of this array,
    # we need to add its idxvar to its offset
    if indexed:
        start_index = sym.array_bounds[0]
        oal.add_instrx(OpCode.ADD)

    yield sym.var_type if not indexed else sym.array_base_type

@mipl.production(NT.IDXVAR, (
    T.LBRACK,
    NT.EXPR,
    T.RBRACK
))
def p_index_variable(*args):
    token = yield
    line_number = token.line_number

    expr_type = yield

    if expr_type != SymbolType.INT:
        raise MiplSemanticError(f"Line {line_number}: Index expression must be of type integer")

    token = yield
    yield True

@mipl.production(NT.IDXVAR, ())
def p_index_variable_epsilon(*args):
    yield False

@mipl.production(NT.CONST, (T.INTCONST,))
def p_constant_int(*args):
    prod = yield
    oal.add_instrx(OpCode.LOAD_CONST, int(prod.lexeme))
    yield SymbolType.INT

@mipl.production(NT.CONST, (T.CHARCONST,))
def p_constant_char(*args):
    prod = yield
    # the character literal is wrapped by single quotes
    oal.add_instrx(OpCode.LOAD_CONST, ord(prod.lexeme[1]))
    yield SymbolType.CHAR

@mipl.production(NT.CONST, (NT.BOOLCONST,))
def p_constant_bool(*args):
    prod = yield
    yield SymbolType.BOOL

@mipl.production(NT.BOOLCONST, (T.TRUE,))
def p_constant_true(*args):
    prod = yield
    oal.add_instrx(OpCode.LOAD_CONST, 1)
    yield

@mipl.production(NT.BOOLCONST, (T.FALSE,))
def p_constant_false(*args):
    prod = yield
    oal.add_instrx(OpCode.LOAD_CONST, 0)
    yield
