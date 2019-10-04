from functools import wraps
from pprint import pprint

from symbols import Symbol, SymbolTable

def print_nonterminal(nonterminal, production):
    production_str = " ".join([p.value for p in production]) if len(production) > 0 else "epsilon"
    print(f"{nonterminal.value} -> {production_str}")

class MiplSyntaxError(Exception):
    pass

class Grammar():
    """
    Represents a context-free language grammar
    """

    def __init__(self, nonterminal_class, terminal_class):

        # remember the terminal and nonterminal classes associated with this grammar
        self.nonterminal_class = nonterminal_class
        self.terminal_class = terminal_class

        # store a list of rules for this grammar
        # a nonterminal key maps to a list of tuples, where each tuple
        # represents a possible production (terminals and nonterminals) for its nonterminal
        self.rules = dict()

        # store a list of producers for this grammar
        # a nonterminal and an associated production map to a generator
        # the generated will be nexted once for each value in the production
        self.production_funcs = dict()

        # store a list of hints for this grammar
        # a nonterminal maps to a function
        # whenever the nonterminal is encountered in an anbiguous context,
        # this function resolves that ambiguity given the parser's current state
        self.hints = dict()

        # keep a symbol table
        self.symbol_table = SymbolTable()

    
    def production(self, nonterminal, production):
        """
        Decorator that converts functions to grammar rules.
        """

        # add this production to our list
        if nonterminal not in self.rules.keys():
            self.rules[nonterminal] = []
        
        self.rules[nonterminal].append(production)

        def decorator_production(target):

            @wraps(target)
            def wrapper_production(*args, **kwargs):
                return target(*args, **kwargs)

            # add this producer to our list
            signature = nonterminal, production
            self.production_funcs[signature] = target
            
            return wrapper_production

        return decorator_production
    
    def hint(self, nonterminal):
        """
        Decorator that resolves ambiguous generations for a given nonterminal.
        """

        def decorator_hint(target):
            self.hints[nonterminal] = target
            return target

        return decorator_hint
    
    def parse(self, nonterminal, token, lexer):
        rules = self.rules[nonterminal]

        # check for hints
        if nonterminal in self.hints.keys():
            # get a list of all possible productions for this nonterminal
            # that this terminal can start
            possible = [x for x in self.rules[nonterminal] if token.terminal in self.first(x)]

            # call the hint function that can resolve the ambiguity
            # give it the list of possible productions and some context
            production = self.hints[nonterminal](possible, token, self.symbol_table)

            # use that production
            return self.produce(nonterminal, production, token, lexer)

        else:
            for production in rules:

                # if this production is epsilon, take it immediately
                # if this terminal can start this production, take it immediately
                if len(production) == 0 or token.terminal in self.first(production):
                    return self.produce(nonterminal, production, token, lexer)
        
        raise MiplSyntaxError(f"Line {token.line_number}: syntax error")

    
    def produce(self, nonterminal, production, token, lexer):
        producer = self.production_funcs[(nonterminal, production)](self.symbol_table)

        # print_nonterminal(nonterminal, production)

        if len(production) == 0:
            return token, next(producer)
        else:
            next(producer)

        prod = None

        for term in production:
            # if this is a terminal, process it
            if isinstance(term, self.terminal_class):
                prod = producer.send(token)
                if term == token.terminal:
                    token = lexer.next()
                else:
                    raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
            
            # if this is a nonterminal, recursive call
            else:
                try:
                    token, prod = self.parse(term, token, lexer)
                    prod = producer.send(prod)
                except StopIteration:
                    raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
        
        # finally, return the next token and the value produced by this production
        return token, prod

    def first(self, production):
        """
        Returns the first() set for the given production.

        The first() set consists of all the terminals that can be used to start this nonterminal.
        """
        first_set = []
        nt_first_set = []

        # if this set is epsilon, there is no terminal produced.
        if len(production) == 0:
            first_set.append(None)
        
        for term in production:

            # if this term is a terminal, it must be the first one
            if isinstance(term, self.terminal_class):
                first_set.append(term)
                break

            else: # this is a nonterminal
                
                # try recursive calls for the productions of this nonterminal
                for subproduction in self.rules[term]:
                    nt_first_set += self.first(subproduction)
                
                # all possible first productions for this nonterminal are now
                # stored in nt_first_set
                #
                # if None is in nt_first_set, it means that epsilon could be produced,
                # which means that this nonterminal could fall out,
                # which means that we need to check the next nonterminal
                if None in nt_first_set and term != production[-1]:
                    nt_first_set.remove(None)
                    continue
                
                # otherwise, call it a day
                first_set += nt_first_set
                break
        
        return first_set
