structure Canonize =
struct
    structure T = Tree
    structure Tmp = Temp 
    
    fun commute(T.EXP(T.CONST _), _) = true
	| commute(_, T.CONST _) = true
    | commute(_, T.NAME _) = true
	| commute _ = false

    fun linearize(statement) = 
    let 
        fun reorder (expr_list) = case expr_list of 
                    (T.CALL(f, arg) :: exprs) => let val t = Tmp.newTemp() 
                                                in reorder(T.ESEQ(T.MOVE(T.TEMP(t), T.CALL(f, arg)), T.TEMP(t)) :: exprs)
                                                end

        |                      (e1 :: exprs)  => let val e1_out    = do_exp e1
                                                     val exprs_out = reorder exprs 
                                                  in if commute(#1(exprs_out), #2(e1_out)) 
                                                     then (T.SEQ(#1(e1_out), #1(exprs_out)), (#2(e1_out) :: #2(exprs_out))) 
                                                     else let 
                                                            val t = Tmp.newTemp()
                                                            in (T.SEQ(T.SEQ(#1(e1_out), T.MOVE(T.TEMP t, #2(e1_out))), #1(exprs_out)), (T.TEMP t :: #2(exprs_out)))
                                                            end
                                                  end


        |                       ([])          => (T.EXP(T.CONST 0), [])

        and reorder_stm (expr_list, f) = let val reorder_out = reorder expr_list
                                            in T.SEQ(#1(reorder_out), f(#2(reorder_out)))
                                         end

        and reorder_exp (expr_list, f) = let val reorder_out = reorder expr_list
                                            in (#1(reorder_out), f(#2(reorder_out)))
                                         end

        and do_exp(T.BINOP(bop, e1, e2)) = reorder_exp([e1, e2], fn [x, y] => T.BINOP(bop, x, y))
        |   do_exp(T.MEM(e))            = reorder_exp([e], fn [x] => T.MEM(x))            
        |   do_exp(T.ESEQ(s, e))        = let val stm_out = do_stm(s)
                                              val expr_out = do_exp(e)
                                          in (T.SEQ(stm_out, #1(expr_out)), #2(expr_out))
                                          end 
        |   do_exp(T.CALL(f, arg))      = reorder_exp(f::arg, fn (x :: xs) => T.CALL(x, xs))
        |   do_exp(T.CONST x)           = (T.EXP(T.CONST 0), T.CONST x)
        |   do_exp(T.NAME x)            = (T.EXP(T.CONST 0), T.NAME x)
        |   do_exp(T.TEMP x)            = (T.EXP(T.CONST 0), T.TEMP x) 

        and do_stm(T.JUMP(e, labs))                  = reorder_stm([e], fn [x] => T.JUMP(x, labs))
        |   do_stm(T.CJUMP(rop, e1, e2, l1, l2))     = reorder_stm([e1, e2], fn [x, y] => T.CJUMP(rop, x, y, l1, l2))
        |   do_stm(T.SEQ(s1, s2))                    = let val s1_out = do_stm s1
                                                        val s2_out = do_stm s2
                                                        in T.SEQ(s1_out, s2_out)
                                                       end
        |   do_stm(T.LABEL l)                        = T.LABEL l
        |   do_stm(T.EXP(T.CALL(f, arg)))            = reorder_stm(f :: arg, fn (x:: xs) => T.EXP(T.CALL(x, xs))) 
        |   do_stm(T.EXP e)                          = reorder_stm([e], fn [x] => T.EXP(x))
        |   do_stm(T.MOVE(T.TEMP t, T.CALL(f, arg))) = reorder_stm(f :: arg, fn(x:: xs) => T.MOVE(T.TEMP t, T.CALL(x, xs)))
        |   do_stm(T.MOVE(T.TEMP t, e))              = reorder_stm([e], fn [x] => T.MOVE(T.TEMP t, x))
        |   do_stm(T.MOVE(T.MEM e1, T.CALL(f, arg))) = reorder_stm(e1 :: f :: arg, fn (x1 :: x2 :: xs)=> T.MOVE(T.MEM x1, T.CALL(x2, xs))) 
        |   do_stm(T.MOVE(T.MEM e1, e2))             = reorder_stm([e1, e2], fn[x, y] => T.MOVE(T.MEM x, y))


        fun linear(T.SEQ (stm1, stm2), out) = linear(stm1, linear(stm2, out))
        |                 linear(stm1, out) = stm1 :: out 

        fun peephole ([], lst) = lst
        |   peephole ((x::xs), lst) = case x of 
                                        T.EXP(T.CONST 0) => peephole(xs, lst)
                                     |  z as _           => peephole(xs, lst@[z])
    in

        peephole(linear(do_stm statement, []), [])

    end 

    type block = Tree.stm list

    fun basicBlocks(lst: Tree.stm list) = 
    let 
        val block_list = ref []
        val done = Tmp.newLabel()
        fun get_block((T.LABEL l)::xs,curr_block) = let val appended = curr_block @ [T.JUMP (T.NAME l, [l])]
                                                    in (new_block((T.LABEL l) :: xs, appended); ())
                                                    end
        
        |   get_block((T.JUMP(x, y)::xs),curr_block) = let val appended = curr_block @ [T.JUMP(x, y)]
                                                       in (new_block(xs, appended);())
                                                       end
                                                       
        |   get_block((T.CJUMP(a1,a2,a3,a4,a5)::xs),curr_block) = let val appended = curr_block @ [T.CJUMP(a1,a2,a3,a4,a5)]
                                                                  in (new_block(xs, appended);())
                                                                  end
        
        |   get_block([], curr_block) = let val appended = curr_block @ [T.JUMP(T.NAME done, [done])]
                                        in (block_list := (!block_list) @ appended)
                                        end  
        
        |   get_block(x::xs, curr_block) = get_block(xs, curr_block@[x])
        
        and new_block((T.LABEL l)::xs,last_block) = if List.length(!block_list) = 0
                                                    then (
                                                          if List.length(last_block) = 0
                                                          then (get_block(xs, [(T.LABEL l)]))
                                                          else (block_list:=(!block_list)@last_block; 
                                                                get_block(xs, [(T.LABEL l)])) 
                                                          )

                                                    else (block_list:=(!block_list)@last_block; 
                                                          get_block(xs, [(T.LABEL l)]))

        |   new_block([], last_block)             = if List.length(!block_list) = 0
                                                    then (
                                                          if List.length(last_block) = 0
                                                          then ()
                                                          else (block_list:=(!block_list)@last_block;
                                                              get_block([], [(T.LABEL (Tmp.newLabel()))]))   
                                                         )
                                                    else
                                                    (block_list:=(!block_list)@last_block;
                                                     get_block([], [(T.LABEL (Tmp.newLabel()))]))

        |   new_block(x::xs, last_block)          = if List.length(!block_list) = 0
                                                    then (
                                                          if List.length(last_block) = 0
                                                          then get_block(x::xs, [(T.LABEL (Tmp.newLabel()))])
                                                          else (block_list:=(!block_list)@last_block; 
                                                                get_block(xs, [(T.LABEL (Tmp.newLabel()))]))
                                                          )
                                                    
                                                    else (block_list:=(!block_list)@last_block; 
                                                          get_block(xs, [(T.LABEL (Tmp.newLabel()))]))
                                                    
    in
        (new_block(lst, []); (!block_list, done))
    end

end