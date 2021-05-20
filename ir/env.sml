structure Env : ENV =
struct
    structure Ord : ORD_KEY =
    struct 
        type ord_key = string
        val compare = String.compare
    end

    structure rbMap = RedBlackMapFn(Ord)
    type env =  Temp.temp rbMap.map 
    
    structure rbMap2 = RedBlackMapFn(Ord)
    type fn_env = (Temp.label*int) rbMap2.map 

    fun generate_new() = let val (ret : env) = rbMap.empty
                            in ret
                         end
    
    fun generate_new_fn() = let val (ret : fn_env) = rbMap2.empty
                                in ret
                            end

    fun insert(curr, var, temp) = let val ret = rbMap.insert(curr, var, temp)
                                    in ret
                                  end
    
    fun insert_fn(curr, var, lab, num)  = let val ret = rbMap2.insert(curr, var, (lab, num))
                                            in ret
                                          end

    fun addPredefined(curr) = let val withPrint = rbMap2.insert(curr, "print", (Temp.newLabel(), 1))
                                  val withExit  = rbMap2.insert(withPrint, "exit", (Temp.newLabel(), 1))
                                  val withPrintInt = rbMap2.insert(withExit, "print_int", (Temp.newLabel(), 1))
                                  val withFlush = rbMap2.insert(withPrintInt, "flush", (Temp.newLabel(), 0))
                                  val withNot   = rbMap2.insert(withFlush, "not", (Temp.newLabel(), 1))
                                  val withSize  = rbMap2.insert(withNot, "size", (Temp.newLabel(), 1))
                                  val withOrd   = rbMap2.insert(withSize, "ord", (Temp.newLabel(), 1))
                                  val withChr   = rbMap2.insert(withOrd, "chr", (Temp.newLabel(), 1))
                                  val withGetChar = rbMap2.insert(withChr, "getchar", (Temp.newLabel(), 0))
                                  val withSubString = rbMap2.insert(withGetChar, "substring", (Temp.newLabel(), 3))
                                  val withConcat = rbMap2.insert(withSubString, "concat", (Temp.newLabel(), 2))
                                  
                              in withConcat
                              end

    fun find(curr, var) = rbMap.find(curr, var)

    fun find_fn(curr, var) = rbMap2.find(curr, var)

end