structure Translate =
struct
    datatype exp = Ex of Tree.expr
                |  Nx of Tree.stm
                |  Cx of Temp.label * Temp.label -> Tree.stm
    
    

end