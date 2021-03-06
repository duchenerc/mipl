from enum import Enum

class OalOpCode(Enum):

    NONE = ""
    COMMENT = "#"

    # management
    INIT = "init"
    HALT = "halt"
    END = "end"
    BSS = "bss"

    # jump
    JUMP_FALSE = "jf"
    JUMP_TRUE = "jt"
    JUMP_STACK = "js"
    JUMP = "jp"
    JUMP_INSTR = "ji"

    # stack
    ADD_STACK_PTR = "asp"
    PUSH = "push"
    POP = "pop"
    SAVE = "save"
    LOAD_CONST = "lc"
    LOAD_ADDR = "la"
    STORE = "st"
    DEREF = "deref"
    LOAD_VAL = "lv"

    # binary ops
    # arithmetic
    ADD = "add"
    SUB = "sub"
    MULT = "mult"
    DIV = "div"
    AND = "and"
    OR = "or"

    # logical
    EQ = ".eq."
    NE = ".ne."
    LT = ".lt."
    LE = ".le."
    GT = ".gt."
    GE = ".ge."

    # unary ops
    NEG = "neg"
    NOT = "not"

    # i/o
    READ_CHAR = "cread"
    READ_INT = "iread"
    WRITE_CHAR = "cwrite"
    WRITE_INT = "iwrite"

class OalWriter():

    def __init__(self):
        self.next_label = 0

        self.instrx = list()
        self.labels = dict()

    def label_id(self):
        """
        Return a new label id.
        """
        label = self.next_label
        self.next_label += 1

        return label
    
    def label(self, id):
        """
        Return a string for the given label id
        """
        return f"L.{id}"
    
    def add_instrx(self, op: OalOpCode, params=(), *, label=None):
        """
        Add an instruction to the internal reel
        """
        if type(params) != tuple:
            params = (params,)

        self.instrx.append((op, params))

        if label is not None:
            pos = len(self.instrx) - 1
            self.labels[pos] = int(label)
    
    def add_comment(self, comment: str):
        self.add_instrx(OalOpCode.COMMENT, comment)
    
    def write(self, fout):
        """
        Output all instructions to the passed file pointer
        """

        for i in range(len(self.instrx)):
            op, params = self.instrx[i]

            if op == OalOpCode.COMMENT:
                fout.write(f"# {str(params[0])}\n")
                fout.flush()
                continue

            label_id = self.labels[i] if i in self.labels.keys() else -1
            label_str = f"L.{label_id}:\n" if label_id > -1 else ""

            if op in (OalOpCode.NONE,):
                fout.write(label_str)
                continue

            params_conv = [str(param) for param in params]
            params_str = (" " + ", ".join(params_conv)) if len(params) > 0 else ""
            fout.write(f"{label_str}  {op.value}{params_str}")

            if op not in (OalOpCode.NONE,):
                fout.write("\n")

            fout.flush()


        # fout.flush()
