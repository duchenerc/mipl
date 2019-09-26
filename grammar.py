from functools import wraps

from errors import MiplSyntaxError
from symbols import Symbol, SymbolTable

def print_nonterminal(nonterminal, production):
    production_str = " ".join([p.value for p in production]) if len(production) > 0 else "epsilon"
    print(f"{nonterminal.value} -> {production_str}")

class Grammar():

    def __init__(self, nonterminal_class, terminal_class):
        self.nonterminal_class = nonterminal_class
        self.terminal_class = terminal_class

        self.rules = dict()
        self.production_funcs = dict()

        self.symbol_table = SymbolTable()

    
    def production(self, nonterminal, production):
        """
        Decorator that converts functions to grammar rules.
        """
        if not nonterminal in self.rules.keys():
            self.rules[nonterminal] = []
        
        self.rules[nonterminal].append(production)

        def decorator_production(target):

            @wraps(target)
            def wrapper_production(*args, **kwargs):
                return target(*args, **kwargs)

            signature = nonterminal, production
            self.production_funcs[signature] = target
            
            return wrapper_production

        return decorator_production
    
    def parse(self, nonterminal, token, lexer):
        rule = self.rules[nonterminal]

        for production in rule:
            if len(production) == 0:
                producer = self.production_funcs[(nonterminal, production)]
                producer_gen = producer(self.symbol_table)
                return token, next(producer_gen)
            
            if token.terminal in self.first(production):

                producer = self.production_funcs[(nonterminal, production)]
                producer_gen = producer(self.symbol_table)
                next(producer_gen)

                prod = None

                for term in production:

                    if isinstance(term, self.terminal_class):
                        prod = producer_gen.send(token)
                        if term == token.terminal:
                            token = lexer.next()
                        else:
                            raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
                    else: # is a nonterminal
                        try:
                            token, prod = self.parse(term, token, lexer)
                            prod = producer_gen.send(prod)
                        except StopIteration:
                            raise MiplSyntaxError(f"Syntax error: line {token.line_number}")
                
                return token, prod
        
        raise MiplSyntaxError(f"Line {token.line_number}: syntax error")

    def first(self, production):
        first_set = []
        nt_first_set = []

        if len(production) == 0:
            first_set.append(None)
        
        for term in production:

            if isinstance(term, self.terminal_class):
                first_set.append(term)
                break

            else:

                for subproduction in self.rules[term]:
                    nt_first_set += self.first(subproduction)
                    
                if None in nt_first_set and term != production[-1]:
                    nt_first_set.remove(None)
                    continue
                
                first_set += nt_first_set
                break
        
        return first_set
