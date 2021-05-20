exception Unsupported of string 
exception Error of string 

structure Translate =
struct
    
    structure T = Tree 
    structure A = Ast 
    structure E = Env
    structure F = Frame 

    datatype exp = Ex of Tree.expr
                |  Nx of Tree.stm
                |  Cx of Temp.label * Temp.label -> Tree.stm
    
    fun toTree []      = T.EXP(T.CONST 0)
     |  toTree [s]     = s
     |  toTree (s::sr) = T.SEQ(s, toTree(sr))

    fun unEx (Ex e) = e 
     |  unEx (Nx s) = T.ESEQ(s, T.CONST 0)
     |  unEx (Cx x) = let val r = Temp.newTemp()
                          val t = Temp.newLabel()
                          val f = Temp.newLabel()
                      in 
                          T.ESEQ(toTree([T.MOVE(T.TEMP r, T.CONST 1),
                                         x(t, f),
                                         T.LABEL f,
                                         T.MOVE(T.TEMP r, T.CONST 0),
                                         T.LABEL r]), T.TEMP r)
                      end

    fun unNx (Nx s) = s 
     |  unNx (Ex e) = T.EXP(e)
     |  unNx (Cx f) = T.EXP(unEx(Cx f))

    fun unCx (Cx f) = f
     |  unCx (Ex e) = (fn (t,f) => T.CJUMP(T.NEQ, e, T.CONST 0, t, f))
     |  unCx (Nx s) = (print("unCx on Nx not allowed.\n"); raise Error "unCx on Nx.\n")


    fun compile(prg) = 
        let 
        val doneList : Temp.label list ref = ref []

        fun oper((e1, opr, e2), env, fn_env) = let val v1 = unEx(compile_exp(e1, env, fn_env))
                                                   val v2 = unEx(compile_exp(e2, env, fn_env))
                                                in 
                                                case opr of
                                                (*binary*)
                                                  A.Plus  => Ex(T.BINOP(T.PLUS, v1, v2))
                                                | A.Minus => Ex(T.BINOP(T.MINUS, v1, v2))
                                                | A.Div   => Ex(T.BINOP(T.DIV, v1, v2))
                                                | A.Mul   => Ex(T.BINOP(T.MUL, v1, v2))
                                                | A.And   => Ex(T.BINOP(T.AND, v1, v2))
                                                | A.Or    => Ex(T.BINOP(T.OR, v1, v2))
                                                (*conditionals*)
                                                | A.Eq    => Cx(fn(t,f)=>T.CJUMP(T.EQ, v1, v2, t, f))
                                                | A.NotEq => Cx(fn(t,f)=>T.CJUMP(T.NEQ, v1, v2, t, f))
                                                | A.GT    => Cx(fn(t,f)=>T.CJUMP(T.GT, v1, v2, t, f))
                                                | A.LT    => Cx(fn(t,f)=>T.CJUMP(T.LT, v1, v2, t, f))
                                                | A.GTEq  => Cx(fn(t,f)=>T.CJUMP(T.GTE, v1, v2, t, f))
                                                | A.LTEq  => Cx(fn(t,f)=>T.CJUMP(T.LTE, v1, v2, t, f))
                                                end
        (* Rectify let for functions *)
        and let_exp(decs, in_, env, fn_env) = let fun get_decs([],   stms,n_env,n_fn_env) = (stms, n_env, n_fn_env)
                                                  |   get_decs((x::xs),stms,n_env,n_fn_env) = let val t = compile_dec(x, n_env, n_fn_env)
                                                                                  in get_decs(xs, stms@[(#1t)], (#2t), (#3t))
                                                                                  end
                                                  val d = get_decs(decs, [], env, fn_env)
                                                  
                                            in Ex( T.ESEQ( toTree((#1d)), unEx(compile_exp(in_, (#2d), (#3d))) ) )
                                            end 

        and if_exp(if_, then_, else_, env, fn_env) = let val t   = Temp.newLabel()
                                                         val f   = Temp.newLabel()
                                                         val join = Temp.newLabel()
                                                         val tmp  = Temp.newTemp()
                                                      in 
                                                      case else_ of 
                                                        NONE => Ex(T.ESEQ(toTree([(unCx(compile_exp(if_, env, fn_env)))(t, f),
                                                                          T.LABEL(t),
                                                                          T.MOVE(T.TEMP tmp, unEx(compile_exp(then_, env, fn_env))),
                                                                          T.JUMP(T.NAME(join), [join]),
                                                                          T.LABEL(f),
                                                                          T.MOVE(T.TEMP tmp, T.CONST 0),
                                                                          T.LABEL(join)
                                                                          ]), T.TEMP tmp)
                                                                  )
                                                      | (SOME x) => Ex(T.ESEQ(toTree([(unCx(compile_exp(if_, env, fn_env)))(t, f),
                                                                                      T.LABEL(t),
                                                                                      T.MOVE(T.TEMP tmp, unEx(compile_exp(then_, env, fn_env))),
                                                                                      T.JUMP(T.NAME(join), [join]),
                                                                                      T.LABEL(f), 
                                                                                      T.MOVE(T.TEMP tmp, unEx(compile_exp(x, env, fn_env))),
                                                                                      T.LABEL(join)
                                                                                    ]), 
                                                                              T.TEMP tmp)
                                                                      )
                                                              
                                                    end

        and while_exp(while_, do_, done, env, fn_env) = let val t = Temp.newLabel()
                                                            val tmp = Temp.newTemp()
                                                        in 
                                                        Ex(T.ESEQ(toTree([
                                                                    T.LABEL(t),
                                                                    (unCx(compile_exp(while_, env, fn_env)))(t, done),
                                                                    unNx(compile_exp(do_, env, fn_env)),
                                                                    T.JUMP(T.NAME(t), [t]),  
                                                                    T.LABEL(done), 
                                                                    T.MOVE(T.TEMP tmp, T.CONST 0)
                                                                  ]), T.TEMP tmp)
                                                          )
                                                        end

        and for_exp(var, from, to_, do_, done, env, fn_env) = let val e1 = unEx(compile_exp(from, env, fn_env))
                                                              val e2 = unEx(compile_exp(to_, env, fn_env))
                                                              val t  = Temp.newLabel()
                                                              val t2 = Temp.newLabel()
                                                              val vtmp = Temp.newTemp()
                                                              val n_env = E.insert(env, var, vtmp)
                                                              val tmp = Temp.newTemp()
                                                          in 
                                                          Ex(T.ESEQ(toTree([ T.MOVE(T.TEMP vtmp, e1),
                                                                      T.LABEL(t),
                                                                      T.CJUMP(T.LT, T.TEMP vtmp, e2, t2, done),
                                                                      T.LABEL(t2),
                                                                      unNx(compile_exp(do_, n_env, fn_env)),
                                                                      T.MOVE(T.TEMP vtmp, T.BINOP(T.PLUS, T.TEMP vtmp, T.CONST 1)),
                                                                      T.JUMP(T.NAME(t), [t]),
                                                                      T.LABEL(done),
                                                                      T.MOVE(T.TEMP tmp, T.CONST 0)
                                                                    ]), T.TEMP tmp)
                                                            )
                                                          end

        and compile_exp(e, env, fn_env) = 
        case e of 
          (A.IntExp(i))  => Ex(T.CONST i)
        | A.NilExp      => Ex(T.CONST 0)

        | A.StringExp _ => (print("String not supported.\n"); raise Unsupported "String")

        | A.ArrayExp _  => (print("Arrays not supported.\n"); raise Unsupported "Array")

        | A.RecordExp _ => (print("Record not supported.\n"); raise Unsupported "Record")

        | A.ObjectExp _ => (print("Classes not supported.\n"); raise Unsupported "Object")

        | (A.LvalueExp lval) => compile_lvalue(lval, env)

        | A.FunCallExp{function_id=id,function_args=args} => 
                  let val v = E.find_fn(fn_env, id)
                  in 
                    case v of
                      NONE   => (print("Function "^id^": Not defined\n"); raise Error "Function")
                    | SOME x => if (#2x)<>List.length(args) 
                                then 
                                (print("Incorrect no. of arguments supplied to "^id^"\n"); raise Error "Function")
                                else
                                let 
                                fun do_args([], out) = out 
                                 |  do_args((x::xs), out) = do_args(xs, out@[unEx(compile_exp(x, env, fn_env))])

                                val done_args = do_args(args, [])
                                in 
                                Ex(T.CALL(T.NAME(#1x), done_args))
                                end
                  end 


        | A.MethodCallExp _ => (print("Classes not supported.\n"); raise Unsupported "Method")

        | (A.NegationExp(e)) => oper( (A.IntExp(0), A.Minus, e), env, fn_env )

        | (A.OpExp(e1, opr, e2)) => oper((e1, opr, e2), env, fn_env)

        | (A.SequenceExp(l)) => let fun do_seq([]) = Ex(T.CONST 0)
                                      | do_seq([x]) = compile_exp(x, env, fn_env)
                                      | do_seq(x::xs) = let val t = unEx(do_seq(xs))
                                                            val z = unNx(compile_exp(x, env, fn_env))
                                                                in Ex(T.ESEQ(z, t))
                                                                end
                                in 
                                  do_seq(l)
                                end
                               

        | (A.AssignExp(lval, e)) => let val ve = unEx(compile_exp(e, env, fn_env))
                                        val vl = unEx(compile_lvalue(lval, env))   
                                    in 
                                        Ex( T.ESEQ(T.MOVE(vl, ve), T.CONST 0) )
                                    end

        | A.IfExp{if_=if_,then_=then_,else_=else_} => if_exp(if_, then_, else_, env, fn_env)

        | A.WhileExp{while_=while_, do_=do_} => let val done = Temp.newLabel()
                                                    val keep = !doneList
                                                    val emp  = (doneList := (done :: (!doneList)))
                                                    val v    = while_exp(while_, do_, done, env, fn_env)
                                                    val emp  = (doneList := keep)
                                                in v
                                                end 
                                                
        | A.ForExp{var=var,from=from,to_=to_,do_=do_} => let val done = Temp.newLabel()
                                                             val keep = !doneList
                                                             val emp  = (doneList := (done :: (!doneList)))
                                                             val v    = for_exp(var, from, to_, do_, done, env, fn_env)
                                                             val emp  = (doneList := keep)
                                                         in v
                                                         end 

        | A.BreakExp      => let val isEmpty = List.null(!doneList)
                             in 
                              case isEmpty of
                                true  => (print("Using break outside loop.\n"); raise Error "Break")
                              | false => Nx(T.JUMP(T.NAME(List.nth(!doneList, 0)), [List.nth(!doneList, 0)]))
                             end

        | (A.LetExp{declarations=decs, in_=in_}) =>  let_exp(decs, in_, env, fn_env)

        and compile_lvalue(e, env) = 
        case e of 
          (A.Lvalue_id id)   => let val v = E.find(env, id)
                                in case v of 
                                     NONE      => (print("Variable: "^ id ^ " Not Defined.\n"); 
                                                  raise Error "Undefined") 
                                   | SOME(x)   => Ex(T.TEMP(x))
                                end

        | (A.Lvalue_array _) => (print("Array not supported.\n"); raise Unsupported "Array")
        | (A.Lvalue_field _) => (print("Record not supported.\n"); raise Unsupported "Record")

        and compile_dec(e, env, fn_env) = 
        case e of 
          (A.VarDec{var_id, var_type, value}) => let val t = Temp.newTemp()
                                                   val v = unEx(compile_exp(value, env, fn_env))
                                                   val env = E.insert(env, var_id, t)
                                                   in 
                                                    (T.MOVE(T.TEMP t, v), env, fn_env)
                                                   end

          | A.FunDec lst => 
            let 
            fun add_in_map([], org, chk) = org 
            |   add_in_map(({function_id=id,function_args=A.Tyfield args,return_type=_,function_body=_}::xs), 
                            org, chk) = case E.find_fn(chk,id) of 
                                        NONE => let val lab = Temp.newLabel()
                                                    val new_org = E.insert_fn(org,id,lab,List.length(args))
                                                    val new_chk = E.insert_fn(chk,id,lab,List.length(args))
                                                in add_in_map(xs,new_org,new_chk)
                                                end 
                                        | SOME _ => (print("Mutually recursive but same name"); raise Error "Function")
            
            val n_fn_env = add_in_map(lst, fn_env, E.generate_new_fn())

            fun transFunDec(id,args,retType,body) = 
            let 
                val saveRegs  = let val SOME x = E.find_fn(n_fn_env, id)
                                in 
                                [T.LABEL (#1(x)), 
                                 T.MOVE( T.TEMP(F.sp), T.BINOP(T.MINUS, T.TEMP(F.sp), T.CONST(F.wordSize)) ),
                                 T.MOVE(T.MEM(T.TEMP(F.sp)), T.TEMP(F.fp)),
                                 T.MOVE( T.TEMP(F.sp), T.BINOP(T.MINUS, T.TEMP(F.sp), T.CONST(F.wordSize)) ),
                                 T.MOVE(T.MEM(T.TEMP(F.sp)), T.TEMP(F.ra)),
                                 T.MOVE(T.TEMP(F.fp), T.BINOP(T.PLUS, T.TEMP(F.sp), T.CONST(2*F.wordSize)) ) 
                                ]
                                end
                
                fun getArgs([],n_env,out,pos) = (n_env, out)
                |   getArgs((id,typ)::xs,n_env,out,pos) = let val t = Temp.newTemp()
                                                              val e = E.insert(n_env, id, t)
                                                              val opt = T.MOVE(T.TEMP t, 
                                                                             T.MEM(T.BINOP(T.PLUS, T.TEMP(F.fp), T.CONST(pos*F.wordSize))))
                                                          in
                                                          ( if typ <> "int" then (print("Unexpected type\n"); raise Error "Type")
                                                            else getArgs(xs,e,out@[opt],pos+1))
                                                          end
                
                val (n_env, fetchArgs) = getArgs(args, env, [], 0)

                val compile_body = case retType of 
                                     NONE   => T.MOVE(T.TEMP(F.ret), unEx(compile_exp(body, n_env, n_fn_env)))
                                   | SOME x => (if x <> "int" then (print("Unexpected type\n"); raise Error "Type")
                                                else T.MOVE(T.TEMP(F.ret), unEx(compile_exp(body, n_env, n_fn_env)))) 
                
                val popnJump  = [T.MOVE(T.TEMP(F.ra), T.MEM(T.BINOP(T.MINUS, T.TEMP(F.fp), T.CONST(2*F.wordSize)))),
                                 T.MOVE(T.TEMP(F.sp), T.TEMP(F.fp)),
                                 T.MOVE(T.TEMP(F.fp), T.MEM(T.BINOP(T.MINUS, T.TEMP(F.fp), T.CONST(F.wordSize)))),
                                 T.JUMP(T.TEMP(F.ra), [])
                                ]
                                
            in 
              saveRegs @ fetchArgs @ [compile_body] @ popnJump
            end 

            fun allFnDec([], inst) = inst
            |   allFnDec(({function_id=id,function_args=A.Tyfield args,return_type=retType,function_body=body}::xs),
                          inst) = allFnDec(xs, inst@transFunDec(id,args,retType,body))

            val total_inst = allFnDec(lst, [])

            in 
              (toTree(total_inst), env, n_fn_env)
            end                                 

          | _ => (print("Not Implemented"); raise Unsupported "Declaration")

        and compile_prg(A.Program (e)) = compile_exp(e, E.generate_new(), E.addPredefined(E.generate_new_fn()))
        in 
        unNx(compile_prg(prg))
        end
end
