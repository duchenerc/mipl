# `mipl`

`mipl` (or Mini Imperative Programming Language) is a subset of the Pascal programming language. This project is a compiler for `mipl`.

Primary compiler features include the following:

* a custom lexer
* a (mostly) context-free grammar
* a (mostly) recursive descent parser
* symbol analysis
* semantic analysis
* intermediate code generation (in progress)
* optimization (in progress)
* final code generation (in progress)

## Parser framework

The `mipl` compiler uses a context-free grammar with a recursive descent parser to process the mipl language.

First, a grammar is created as such. Note how language nonterminals and terminals must be registered to the grammar.

```python
# import grammar
from grammar import Grammar

# import language elements
from elements import Nonterminal as NT, Terminal as T

# create grammar
mipl = Grammar(NT, T)
```

Grammar rules are created by applying the `production` decorator available on this `Grammar` object to a method:

```python
@mipl.production(NT.ASSIGN, (
    NT.VARIABLE,
    T.ASSIGN,
    NT.EXPR
))
def p_assign():
	# semantic/symbol analysis here

	# get production from variable nonterminal
	nt_variable = yield

	# get token object for assign
	t_assign = yield

	# get production from expression nonterminal
	nt_expr = yield

	# return final production for this nonterminal
	yield
```

The `hint` decorator can be used to resolve ambiguity in the grammar, given the parser's state at runtime.

I have some improvements that I'd like to make in this area:

* Decouple the parser from the grammar. This would allow multiple kinds of parsers to be used with a single grammar API.
* Remove symbol table logic from grammars. This is technically semantic analysis and should not be managed from within a grammar. I'm not sure if this should be managed from within a decoupled parser (see above) or if it should be managed entirely on its own.
