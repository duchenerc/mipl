
from enum import Enum

class MiplSymbolCreationError(Exception):
    pass

class MiplSymbolAccessError(Exception):
    pass

class SymbolType(Enum):
    INVALID = "INVALID"
    BOOL = "BOOLEAN"
    INT = "INTEGER"
    CHAR = "CHAR"
    ARRAY = "ARRAY"

class SymbolCat(Enum):
    INVALID = "INVALID"
    PROGRAM = "PROGRAM"
    PROCEDURE = "PROCEDURE"
    VARIABLE = "VARIABLE"

SYMBOL_ATTRS = (
    "type",
    "bounds",
    "base_type",
    "parameters",
)

class Symbol():
    """
    An identifier and associated information.
    """

    def __init__(self, sym_name, sym_cat, **kwargs):
        self._sym_name = sym_name
        self._sym_cat = sym_cat

        self._sym_data = dict()

        self.linkage = dict()

        if "label" in kwargs.keys():
            self.linkage["label"] = kwargs["label"]

        if "nesting_lavel" in kwargs.keys():
            self.linkage["nesting_level"] = kwargs["nesting_level"]
        
        if "words" in kwargs.keys():
            self.linkage["words"] = kwargs["words"]

        # handle procedures
        if self._sym_cat == SymbolCat.PROCEDURE:
            if "parameters" in kwargs.keys():
                self._sym_data["parameters"] = kwargs.pop("parameters")
            else:
                raise MiplSymbolCreationError(f"Given symbol category is {self._sym_cat.value}, but parameters not given")
        
        # handle variables
        elif self._sym_cat == SymbolCat.VARIABLE:
            if "type" in kwargs.keys():
                self._sym_data["type"] = kwargs.pop("type")

                # handle array special parts
                if self._sym_data["type"] == SymbolType.ARRAY:
                    if "bounds" in kwargs.keys():
                        self._sym_data["bounds"] = kwargs.pop("bounds")
                    else:
                        raise MiplSymbolCreationError(f"Given symbol type is ARRAY, but bounds not given")
                    
                    if "base_type" in kwargs.keys():
                        self._sym_data["base_type"] = kwargs.pop("base_type")
                    else:
                        raise MiplSymbolCreationError(f"Given symbol type is ARRAY, but base_type not given")

            else:
                raise MiplSymbolCreationError(f"Given symbol category is {self._sym_cat}, but type not given")
    
    @property
    def sym_name(self):
        return self._sym_name
    
    @property
    def sym_cat(self):
        return self._sym_cat
    
    @property
    def proc_parameters(self):
        if self._sym_cat != SymbolCat.PROCEDURE:
            raise MiplSymbolAccessError(f"Symbol category {self._sym_cat.value} does not have member parameters")
        
        return self._sym_data["parameters"]
    
    @property
    def var_type(self):
        if self._sym_cat not in (SymbolCat.VARIABLE, SymbolCat.PROGRAM):
            raise MiplSymbolAccessError(f"Symbol category {self._sym_cat.value} does not have member type")
        
        return self._sym_data["type"] if self._sym_cat == SymbolCat.VARIABLE else SymbolType.INVALID
    
    @property
    def array_bounds(self):
        if self._sym_cat != SymbolCat.VARIABLE:
            raise MiplSymbolAccessError(f"Symbol category {self._sym_cat.value} does not have member bounds")
        
        if self._sym_data["type"] != SymbolType.ARRAY:
            raise MiplSymbolAccessError(f"Symbol type {self._sym_data['type'].value} is not array") 

        return self._sym_data["bounds"]
    
    @property
    def array_base_type(self):
        if self._sym_cat != SymbolCat.VARIABLE:
            raise MiplSymbolAccessError(f"Symbol category {self._sym_cat.value} does not have member base_type")
        
        if self._sym_data["type"] != SymbolType.ARRAY:
            raise MiplSymbolAccessError(f"Symbol type {self._sym_data['type'].value} is not array") 

        return self._sym_data["base_type"]

class MiplMultiplyDefinedIdentifierError(Exception):
    pass

class MiplUndeclaredIdentifierError(Exception):
    pass

class SymbolTable():
    """
    A database of symbols.
    """

    def __init__(self):

        self._symbols = []

    def scope_enter(self):
        """
        Create a new scope.
        """

        # print("\n\n>>> Entering new scope...")
        self._symbols.append(dict())
    
    def scope_exit(self):
        """
        Delete the current scope.
        """
        # print("\n<<< Exiting scope...")
        self._symbols.pop()
    
    def this_scope(self):
        """
        Returns a list of identifiers for the symbols in the curent scope.
        """
        return self._symbols[-1].keys()
    
    def nesting_level(self):
        # global variables are nesting level 0,
        # which equates to 1 table (the one storing the global tables)
        return len(self._symbols) - 1
    
    def new_id(self, sym):
        """
        Creates a new symbol in the current scope.
        """
        if sym.sym_name in self._symbols[-1].keys():
            raise MiplMultiplyDefinedIdentifierError(f"Identifier {sym.sym_name} already definied in current scope")

        self._symbols[-1][sym.sym_name] = sym
    
    def overwrite_id(self, sym: Symbol):
        for table in self._symbols[::-1]:
            if sym.sym_name in table.keys():
                table[sym.sym_name] = sym
                return
        
        self.new_id(sym)
    
    def __getitem__(self, sym_name):
        for table in self._symbols[::-1]:
            if sym_name in table.keys():
                return table[sym_name]

        raise MiplUndeclaredIdentifierError(f"Identifier {sym_name} is undeclared")
    
    def __contains__(self, sym_name):
        for table in self._symbols[::-1]:
            if sym_name in table.keys():
                return True
        
        return False
