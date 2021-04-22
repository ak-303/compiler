structure PP : sig
    val compile : Ast.program -> string 
end =
struct 

structure A = Ast 

fun compile (e0) = 
    let val out = ref ""
    in 
        let 
        fun blue s    = "\027[94m" ^ s ^ "\027[0m"
        fun magneta s = "\027[95m" ^ s ^ "\027[0m"
        fun grey s    = "\027[90m" ^ s ^ "\027[0m"
        fun cyan s    = "\027[96m" ^ s ^ "\027[0m"
        fun red  s    = "\027[31m" ^ s ^ "\027[0m"
        fun green s   = "\027[32m" ^ s ^ "\027[0m"
        fun yellow s  = "\027[33m" ^ s ^ "\027[0m"

        fun opname A.Plus  = cyan "+"
        |   opname A.Minus = cyan "-"
        |   opname A.Mul   = cyan "*"
        |   opname A.Div   = cyan "/"
        |   opname A.Eq    = cyan "="
        |   opname A.NotEq = cyan "<>"
        |   opname A.LT    = cyan "<"
        |   opname A.LTEq  = cyan "<="
        |   opname A.GT    = cyan ">"
        |   opname A.GTEq  = cyan ">="
        |   opname A.And   = cyan "&"
        |   opname A.Or    = cyan "|"
        
        fun cat1 (A.LvalueExp(v)) = true
        |   cat1 (A.IntExp(i))    = true
        |   cat1 (A.StringExp(s)) = true
        |   cat1 (A.FunCallExp{function_id, function_args}) = true
        |   cat1 (A.MethodCallExp{caller, method_id, method_args}) = true
        |   cat1 (A.ObjectExp(s)) = true
        |   cat1 (A.ArrayExp{array_type, initial_val, length}) = true
        |   cat1 (A.RecordExp{field,record_type}) = true
        |   cat1 (A.NegationExp(e)) = true
        |   cat1 (A.OpExp(left, oper, right)) = true
        |   cat1 (A.BreakExp) = true
        |   cat1 (A.AssignExp(v, e)) = true
        |   cat1 _                = false
        
        fun say   s = (out := !out ^ s)
        fun sayln s = (say s; say "\n")

        fun indent 0 = ()
        |   indent i = (say "    "; indent(i-1))

        fun dolist d f [a] del   = (f(a,d))
        |   dolist d f (a::r) del = (f(a,d); say (cyan(del)); (dolist d f r del) )
        |   dolist d f nil del    = ()

        fun dolist_inline f [a] del   = (f(a))
        |   dolist_inline f (a::r) del = (f(a); say (cyan(del)); (dolist_inline f r del) )
        |   dolist_inline f nil del    = ()

        fun exp_inline(A.LvalueExp(v)) = (lvalue_inline(v))
        |   exp_inline(A.NilExp)      = (say(magneta("nil")) )

        |   exp_inline(A.IntExp(i))    = (say(green(Int.toString(i))) )

        |   exp_inline(A.StringExp(s)) = (say(yellow("\"")); say( yellow (concat(map Char.toString (explode s))) ); say(yellow "\"")) 

        |   exp_inline(A.FunCallExp{function_id, function_args}) = (say(function_id); say(grey("(")); 
                                                    ((dolist_inline exp_inline function_args ", ")); say(grey(")")))
                                        
        |   exp_inline(A.MethodCallExp{caller, method_id, method_args}) = (lvalue_inline(caller); say(grey(".")); say(green(method_id));
                                                    say(grey("(")); (dolist_inline exp_inline method_args ", "); say(grey(")")) )
        
        |   exp_inline(A.ObjectExp(s)) = (say(blue("new")); say " "; say(s))
        
        |   exp_inline(A.ArrayExp{array_type, initial_val, length}) = (say(green(array_type)); say(grey("[")); exp_inline(length); 
                                                    say(grey("]")); say " "; say(blue("of ")); exp_inline(initial_val); ())

        |   exp_inline(A.RecordExp{field,record_type}) = let fun f((name,e)) = (say(name); say(cyan(" : ")); exp_inline(e); ())
                                                in  (say(record_type); say " "; say(grey("{"));
                                                    (dolist_inline f field ", "); say(grey("}")))
                                                end
        
        |   exp_inline(A.NegationExp(e)) = ( say(cyan("- ")); exp_inline(e); ())

        |   exp_inline(A.OpExp(left, oper, right)) = (exp_inline(left); say " "; say(opname(oper)); say " "; exp_inline(right); ())

        |   exp_inline(A.SequenceExp(l)) = ( say "( "; dolist_inline exp_inline l "; "; say ") " )

        |   exp_inline(A.AssignExp(v,e)) = ( lvalue_inline(v); say(cyan(" := ")); exp_inline(e); ())

        |   exp_inline(A.IfExp{if_,then_,else_}) = ( say(magneta("if ")); exp_inline (if_); say " "; say(magneta("then "));
                                       exp_inline(then_); say " "; case else_ of NONE => ()
                                                                | SOME(e) => ( say(magneta("else ")); exp_inline(e); say " "; () ); 
                                                                 ())
        
        |   exp_inline(A.WhileExp{while_,do_}) = (say(magneta("while ")); exp_inline(while_); say " "; say(magneta("do "));
                                              exp_inline(do_); ())

        |   exp_inline(A.ForExp{var=v,from=lo,to_=hi,do_=body}) = (say(magneta("for ")); say(red v); say(cyan(" := ")); exp_inline(lo); 
                                                        say(magneta(" to ")); exp_inline(hi); say " "; say(magneta("do ")); 
                                                        exp_inline(body); ())

        |   exp_inline(A.BreakExp) = (say(magneta(" break ")); ())

        |   exp_inline(A.LetExp{declarations,in_}) = (sayln(magneta(" let")); (dolist_inline dec_inline declarations "  "); say "  "; 
                                                  sayln(magneta("in")); exp_inline(in_); say "  "; say(magneta("end ")); ())
        
        and lvalue_inline(A.Lvalue_id(s)) = ( say(yellow(s)); ()) 

        |   lvalue_inline(A.Lvalue_array(v,e)) = ( lvalue_inline(v); say((grey("["))); (exp_inline(e));  say(grey("]")); ())

        |   lvalue_inline(A.Lvalue_field(v,s)) = ( lvalue_inline(v); say((grey "." ^ green (s))); ())

        and dec_inline(A.TypeDec(l)) = let fun tdec((type_id,t)) = (say(blue(" type ")); say(red type_id); say(cyan " = "); ty_inline(t) )                          
                                in ( (dolist_inline tdec l "  "); say " " )
                                end
        
        |   dec_inline(A.VarDec{var_id,var_type,value}) = ( say(blue "var "); say(red var_id); say " "; 
                                                       case var_type of NONE => ()
                                                        |            SOME(v) => (say(cyan ": "); say(green v); say " ");
                                                        say(cyan ":= "); exp_inline(value); say " ")

        |   dec_inline(A.ClassDec{class_id, class_extends, class_fields}) = ( say (blue "class "); say(red class_id); say " ";
                                                        case class_extends of NONE => ()
                                                        |                   SOME(v) => (say (blue "extends "); say (green v); say " ");
                                                        say(grey "{"); (dolist_inline classfield_inline class_fields "  "); say "  "; say(grey "}"))

        |   dec_inline(A.FunDec(l)) = let fun f({function_id,function_args,return_type,function_body}) =
                                ( say(blue "function "); say(red function_id); say(grey " (");
                                    tyfields_inline(function_args); say(grey ") ");
                                    case return_type of NONE => ()
                                    | SOME(r) => ( say(cyan ": "); say(green r); say " " );
                                    say(cyan "= "); exp_inline(function_body); () )
                                in (dolist_inline f l "  "); say " "
	                            end

        |   dec_inline(A.PrimDec{primitiveid, primitive_args,return_type}) = ( say (blue "primitive "); say(red primitiveid);
                                                        say(grey " ("); tyfields_inline(primitive_args); say(grey ") ");
                                                        case return_type of NONE => ()
                                                        |                 SOME(r) => (say(cyan ": "); say(green r));
                                                        say " "; ())
        
        and classfield_inline(A.ClassVarDec{class_var_id,class_var_type,value}) = (say(blue "var "); say(red class_var_id); say " ";
                                                        case class_var_type of NONE => ()
                                                        |                    SOME(v) => (say(cyan ": "); say(green v); say " ");
                                                        say(cyan ":= "); exp_inline(value); say " " )

        |   classfield_inline(A.Method(l)) = let fun f({method_id,method_args,return_type,method_body}) =
                                        (say(blue "method "); say(red method_id); say(grey " (");
                                            tyfields_inline(method_args); say(grey ") ");
                                            case return_type of NONE => ()
                                            | SOME(r) => ( say(cyan ": "); say(green r); say " " );
                                            say(cyan "= "); exp_inline(method_body); () )
                                        in (dolist_inline f l "  "); say " "
                                        end
        
        and ty_inline(A.Type_id(s)) = ( say s)
        |   ty_inline(A.Record_ty(l)) = (say(grey "{"); tyfields_inline(l); say(grey "}"))
        |   ty_inline(A.Array_ty(s)) = ( say(blue "array of "); say (green s))
        |   ty_inline(A.Class_ty{class_extends,class_fields}) = (say(blue "class "); 
                                            case class_extends of NONE => ()
                                            |                   SOME(c) => (say(blue "extends "); say(green c); say " ");
                                            say(grey "{"); (dolist_inline classfield_inline class_fields "  "); sayln " "; say(grey "}") )

        and tyfields_inline(A.Tyfield(l)) = let fun f((name,typ)) = ( say (name); say(cyan " : "); say(green typ))
                                        in  (dolist_inline f l ", ")
                                        end
        
        and lvalue(A.Lvalue_id(s), d) = (indent d; say(yellow(s)); ()) 

        |   lvalue(A.Lvalue_array(v,e), d) = (indent d; lvalue(v, 0); say((grey("["))); (exp_inline(e));  say(grey("]")); ())

        |   lvalue(A.Lvalue_field(v,s), d) = (indent d; lvalue(v, 0); say((grey "." ^ green (s))); ())

        and exp(A.LvalueExp(v), d) = (indent d; lvalue(v, 0))     

        |   exp(A.NilExp, d)      = (indent d; say(magneta("nil")) )

        |   exp(A.IntExp(i), d)    = (indent d; say(green(Int.toString(i))) )

        |   exp(A.StringExp(s), d) = (indent d; say(yellow("\"")); say( yellow(concat(map Char.toString (explode s))) ); say(yellow "\"")) 

        |   exp(A.FunCallExp{function_id, function_args}, d) = (indent d; say(function_id); say(grey("(")); 
                                                    ((dolist_inline exp_inline function_args ", ")); say(grey(")")))
                                        
        |   exp(A.MethodCallExp{caller, method_id, method_args}, d) = (indent d; lvalue(caller, 0); say(grey(".")); say(green(method_id));
                                                    say(grey("(")); (dolist_inline exp_inline method_args ", "); say(grey(")")) )
        
        |   exp(A.ObjectExp(s), d) = (indent d; say(blue("new")); say " "; say(s))
        
        |   exp(A.ArrayExp{array_type, initial_val, length}, d) = (indent d; say(green(array_type)); say(grey("[")); exp_inline(length); 
                                                    say(grey("]")); say " "; say(blue("of ")); exp_inline(initial_val); ())

        |   exp(A.RecordExp{field,record_type}, d) = let fun f((name,e),d) = (say(name); say(cyan(" : ")); exp_inline(e); ())
                                                in  (indent d; say(record_type); say " "; say(grey("{"));
                                                    (dolist 0 f field ", "); say(grey("}")))
                                                end
        
        |   exp(A.NegationExp(e), d) = (indent d; say(cyan("- ")); exp_inline(e); ())

        |   exp(A.OpExp(left, oper, right), d) = (indent d; exp_inline(left); say " "; say(opname(oper)); say " "; exp_inline(right); ())

        |   exp(A.SequenceExp(l), d) = (indent d; sayln "("; dolist (d+1) exp l "; \n"; sayln ""; indent d; say ")" )

        |   exp(A.AssignExp(v,e), d) = (indent d; lvalue(v, 0); say(cyan(" := ")); exp_inline(e); say " "; ())

        |   exp(A.IfExp{if_,then_,else_}, d) = (indent d; say(magneta("if ")); exp_inline(if_); say " "; say(magneta("then \n"));
                                       exp(then_, d+1); case else_ of NONE => ()
                                                                | SOME(e) => (say("\n"); indent d; say(magneta("else\n")); exp(e, d+1); () ); 
                                                                ())
        
        |   exp(A.WhileExp{while_,do_}, d) = (indent d; say(magneta("while ")); exp_inline(while_); say " "; say(magneta("do \n"));
                                              exp(do_, d+1); ())

        |   exp(A.ForExp{var=v,from=lo,to_=hi,do_=body},d) = (indent d; say(magneta("for ")); say(red v); say(cyan(" := ")); exp_inline(lo); 
                                                        say(magneta(" to ")); exp_inline(hi); say " "; say(magneta("do \n")); 
                                                        exp(body, d+1); ())

        |   exp(A.BreakExp, d) = (indent d; say(magneta("break")); ())

        |   exp(A.LetExp{declarations,in_}, d) = (indent d; sayln(magneta("let")); (dolist (d+1) dec declarations "\n\n"); sayln ""; 
                                                  indent d; sayln(magneta("in")); exp(in_, d+1); sayln ""; indent d; say(magneta("end")); ())

        and dec(A.TypeDec(l), d) = let fun tdec((type_id,t),d) = (indent d; say(blue("type ")); say(red type_id); say(cyan " = "); ty_inline(t) )                          
                                in ( (dolist d tdec l "\n"); say " " )
                                end
        
        |   dec(A.VarDec{var_id,var_type,value}, d) = (indent d; say(blue "var "); say(red var_id); say " "; 
                                                       case var_type of NONE => ()
                                                        |            SOME(v) => (say(cyan ": "); say(green v); say " ");
                                                        say(cyan ":= "); exp_inline(value); say " ")

        |   dec(A.ClassDec{class_id, class_extends, class_fields}, d) = (indent d; say (blue "class "); say(red class_id); say " ";
                                                        case class_extends of NONE => ()
                                                        |                   SOME(v) => (say (blue "extends "); say (green v); say " ");
                                                        sayln(grey "{"); (dolist (d+1) classfield class_fields "\n\n"); sayln ""; indent d; say(grey "}"))

        |   dec(A.FunDec(l), d) = let fun f({function_id,function_args,return_type,function_body},d) =
                                (indent d; say(blue "function "); say(red function_id); say(grey " (");
                                    tyfields_inline(function_args); say(grey ") ");
                                    case return_type of NONE => ()
                                    | SOME(r) => ( say(cyan ": "); say(green r); say " " );
                                    say(cyan "= \n"); exp(function_body, d+1); () )
                                in (dolist d f l "\n\n"); say " "
	                            end

        |   dec(A.PrimDec{primitiveid, primitive_args,return_type}, d) = (indent d; say (blue "primitive "); say(red primitiveid);
                                                        say(grey " ("); tyfields_inline(primitive_args); say(grey ") ");
                                                        case return_type of NONE => ()
                                                        |                 SOME(r) => (say(cyan ": "); say(green r));
                                                        ())
        
        and classfield(A.ClassVarDec{class_var_id,class_var_type,value}, d) = (indent d; say(blue "var "); say(red class_var_id); say " ";
                                                        case class_var_type of NONE => ()
                                                        |                    SOME(v) => (say(cyan ": "); say(green v); say " ");
                                                        say(cyan ":= "); exp_inline(value); say " " )

        |   classfield(A.Method(l), d) = let fun f({method_id,method_args,return_type,method_body},d) =
                                        (indent d; say(blue "method "); say(red method_id); say(grey " (");
                                            tyfields_inline(method_args); say(grey ") ");
                                            case return_type of NONE => ()
                                            | SOME(r) => ( say(cyan ": "); say(green r); say " " );
                                            say(cyan "= \n"); exp(method_body, d+1); () )
                                        in (dolist d f l "\n\n"); say " "
                                        end

        and   ty(A.Type_id(s), d) = (indent d; say s)
        |   ty(A.Record_ty(l), d) = (indent d; say(grey "{"); tyfields(l, d); say(grey "}"))
        |   ty(A.Array_ty(s), d) = (indent d; say(blue "array of "); say (green s))
        |   ty(A.Class_ty{class_extends,class_fields}, d) = (indent d; say(blue "class "); 
                                            case class_extends of NONE => ()
                                            |                   SOME(c) => (say(blue "extends "); say(green c); say " ");
                                            sayln(grey "{"); (dolist (d+1) classfield class_fields "\n"); sayln ""; indent d; say(grey "}") )

        and   tyfields(A.Tyfield(l), d) = let fun f((name,typ),d) = (indent d; say (name); say(cyan " : "); say(green typ))
                                        in  (dolist d f l ", ")
                                        end

        and   program(A.Program(e), d) = (exp(e, d); ())

        in
            (program(e0,0); sayln ""; !out)
        end
    end


end

