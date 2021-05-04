structure Env : ENV =
struct
    structure Ord : ORD_KEY =
    struct 
        type ord_key = string
        val compare = String.compare
    end

    structure rbMap = RedBlackMapFn(Ord)
    type env =  Temp.temp rbMap.map 
    
    fun generate_new() = let val (ret : env) = rbMap.empty
                            in ret
                         end
    
    fun insert(curr, var, temp) = let val ret = rbMap.insert(curr, var, temp)
                                    in ret
                                  end
    
    fun find(curr, var) = rbMap.find(curr, var)

end