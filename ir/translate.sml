exception Unsupported of string 
exception Error of string 

structure Translate =
struct
    
    structure T = Tree 
    structure A = Ast 
    structure E = Env

    datatype exp = Ex of Tree.expr
                |  Nx of Tree.stm
                |  Cx of Temp.label * Temp.label -> Tree.stm
    
    fun toTree []      = T.EXP(T.CONST 0)
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
        fun oper((e1, opr, e2), env) = let val v1 = unEx(compile_exp(e1, env))
                                           val v2 = unEx(compile_exp(e2, env))
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
        
        and let_exp(decs, in_, env) = let fun get_decs([],   stms,n_env) = (stms, n_env)
                                          |   get_decs((x::xs),stms,n_env) = let val t = compile_dec(x, n_env)
                                                                          in get_decs(xs, stms@[(#1t)], (#2t))
                                                                          end
                                          val d = get_decs(decs, [], env)
                                          
                                     in Ex( T.ESEQ( toTree((#1d)), unEx(compile_exp(in_, (#2d))) ) )
                                     end 

        and compile_exp(e, env) = 
        case e of 
          (A.IntExp(i))  => Ex(T.CONST i)
        | A.NilExp      => Ex(T.CONST 0)

        | A.StringExp _ => (print("String not supported.\n"); raise Unsupported "String")

        | A.ArrayExp _  => (print("Arrays not supported.\n"); raise Unsupported "Array")

        | A.RecordExp _ => (print("Record not supported.\n"); raise Unsupported "Record")

        | A.ObjectExp _ => (print("Classes not supported.\n"); raise Unsupported "Object")

        | (A.LvalueExp lval) => compile_lvalue(lval, env)

        | A.FunCallExp _ => (print("Function not supported.\n"); raise Unsupported "Function")

        | A.MethodCallExp _ => (print("Classes not supported.\n"); raise Unsupported "Method")

        | (A.NegationExp(e)) => oper( (A.IntExp(0), A.Minus, e), env )

        | (A.OpExp(e1, opr, e2)) => oper((e1, opr, e2), env)

        | (A.SequenceExp(l)) => let fun do_seq([], lst) = (lst, [])
                                      | do_seq([x], lst) = let val t = unEx(compile_exp(x, env))
                                                            in (lst, [t])
                                                            end
                                      | do_seq((x::xs), lst) = let val t = unNx(compile_exp(x, env))
                                                                in do_seq(xs, lst@[t])
                                                                end
                                    val d = do_seq(l, [])
                                in 
                                  Ex( T.ESEQ(toTree((#1d)), List.nth((#2d), 0)) )
                                end

        | (A.AssignExp(lval, e)) => let val vl = unEx(compile_lvalue(lval, env)) 
                                        val ve = unEx(compile_exp(e, env))
                                    in 
                                        Ex( T.ESEQ(T.MOVE(vl, ve), T.CONST 0) )
                                    end

        | A.IfExp _       => (print("If not supported.\n"); raise Unsupported "IfExp")
        | A.WhileExp _    => (print("While not supported.\n"); raise Unsupported "While")
        | A.ForExp _      => (print("For not supported.\n"); raise Unsupported "For")
        | A.BreakExp      => (print("Break not supported"); raise Unsupported "Break")
        | (A.LetExp{declarations=decs, in_=in_}) =>  let_exp(decs, in_, env)

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

        and compile_dec(e, env) = 
        case e of 
          (A.VarDec{var_id, var_type, value}) => let val t = Temp.newTemp()
                                                   val v = unEx(compile_exp(value, env))
                                                   val env = E.insert(env, var_id, t)
                                                   in 
                                                    (T.MOVE(T.TEMP t, v), env )
                                                   end

                                                
          | _ => (print("Not Implemented"); raise Unsupported "Declaration")

        and compile_prg(A.Program (e)) = compile_exp(e, E.generate_new())
        in 
        unNx(compile_prg(prg))
        end
end