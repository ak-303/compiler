signature TREE = 
sig
    datatype binop = PLUS | MINUS | MUL | DIV 
                |    AND | OR | XOR
                |    LSHIFT | RSHIFT | ARSHIFT

    datatype relop = EQ | NEQ | LT | GT | GTE | LTE 
                |   ULT | UGT | ULTE | UGTE 

    datatype expr = CONST of int
                |   NAME  of Temp.label
                |   TEMP  of Temp.temp 
                |   BINOP of binop * expr * expr
                |   MEM   of expr
                |   CALL  of expr * expr list
                |   ESEQ  of stm * expr

    and       stm = MOVE  of expr * expr
                |   EXP   of expr
                |   JUMP  of expr * Temp.label list
                |   CJUMP of relop * expr * expr * Temp.label * Temp.label
                |   SEQ   of stm * stm
                |   LABEL of Temp.label 

end
