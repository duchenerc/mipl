from functools import wraps

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
    
    def parse(self, nonterminal, token, lexer):
        rule = self.rules[nonterminal]

        for production in rule:

            # if this production is epsilon, take it immediately
            if len(production) == 0:
                producer = self.production_funcs[(nonterminal, production)]
                producer_gen = producer(self.symbol_table)
                return token, next(producer_gen)
            
            # if this terminal is in the first() set, take it
            if token.terminal in self.first(production):
                
                # look up the defined producer generator for this production
                producer = self.production_funcs[(nonterminal, production)]
                producer_gen = producer(self.symbol_table)
                next(producer_gen)

                prod = None

                for term in production:

                    # if this is a terminal, process it
                    if isinstance(term, self.terminal_class):
                        prod = producer_gen.send(token)
                        if term == token.terminal:
                            token = lexer.next()
                        else:
                            raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
                    
                    # if this is a nonterminal, recursive call
                    else:
                        try:
                            token, prod = self.parse(term, token, lexer)
                            prod = producer_gen.send(prod)
                        except StopIteration:
                            raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
                
                # finally, return the next token and the value produced by this nonterminal
                return token, prod
        
        raise MiplSyntaxError(f"Line {token.line_number}: syntax error")

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
