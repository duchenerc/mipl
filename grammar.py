from functools import wraps

from errors import MiplSyntaxError

def print_nonterminal(nonterminal, production):
    production_str = " ".join([p.value for p in production]) if len(production) > 0 else "epsilon"
    print(f"{nonterminal.value} -> {production_str}")

class Grammar():

    def __init__(self, nonterminal_class, terminal_class):
        self.nonterminal_class = nonterminal_class
        self.terminal_class = terminal_class

        self.rules = dict()

    
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
            
            return wrapper_production

        return decorator_production
    
    def parse(self, nonterminal, token, lexer):
        rule = self.rules[nonterminal]

        for production in rule:
            if len(production) == 0:
                print_nonterminal(nonterminal, production)
                return token
            
            if token.terminal in self.first(production):

                print_nonterminal(nonterminal, production)

                for term in production:

                    if isinstance(term, self.terminal_class):
                        if term == token.terminal:
                            token = lexer.next()
                        else:
                            raise MiplSyntaxError(f"Line {token.line_number}: syntax error")
                    else: # is a nonterminal
                        token = self.parse(term, token, lexer)
                
                return token
        
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
