
from enum import Enum, IntEnum

class MiplSymbolCreationError(Exception):
    pass

class MiplSymbolAccessError(Exception):
    pass

class SymbolType(IntEnum):
    INVALID = 0
    BOOL = 1
    INT = 2
    CHAR = 3
    ARRAY = 4

class SymbolCat(Enum):
    INVALID = "SYMBOL_INVALID"
    PROGRAM = "SYMBOL_PROGRAM"
    PROCEDURE = "SYMBOL_PROCEDURE"
    VARIABLE = "SYMBOL_VARIABLE"

SYMBOL_ATTRS = (
    "type",
    "bounds",
    "base_type",
    "parameters",
)

class Symbol():
    
    def __init__(self, sym_name, sym_cat, **kwargs):
        self._sym_name = sym_name
        self._sym_cat = sym_cat

        self._sym_data = dict()

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
                if self._sym_type == SymbolType.ARRAY:
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
        if self._sym_cat != SymbolCat.VARIABLE:
            raise MiplSymbolAccessError(f"Symbol category {self._sym_cat.value} does not have member type")
        
        return self._sym_data["type"]
    
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
