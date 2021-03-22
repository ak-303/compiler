structure PrintAst : 
     sig val print : TextIO.outstream * Ast.program -> unit end =
struct 

structure A = Ast

fun print (outstream, e0) =
 let fun say s =  TextIO.output(outstream,s)
  fun sayln s= (say s; say "\n") 

  fun indent 0 = ()
    | indent i = (say "   "; indent(i-1))

  fun opname A.Plus = "Plus"
    | opname A.Minus = "Minus"
    | opname A.Mul = "Mul"
    | opname A.Div = "Div"
    | opname A.Eq = "Eq"
    | opname A.NotEq = "NotEq"
    | opname A.LT = "LT"
    | opname A.LTEq = "LTEq"
    | opname A.GT = "GT"
    | opname A.GTEq = "GTEq"
    | opname A.And = "And"
    | opname A.Or = "Or"

  fun dolist d f [a] = (sayln ""; f(a,d+1))
    | dolist d f (a::r) = (sayln ""; f(a,d+1); say ","; dolist d f r)
    | dolist d f nil = ()

  fun lvalue(A.Lvalue_id(s),d) = ( indent d; say "Lvalue_id("; say(s); say ")" )

    | lvalue(A.Lvalue_field(v,s),d) = (indent d; sayln "Lvalue_field("; 
                                       lvalue(v,d+1); sayln ",";
				                       indent(d+1); say(s); say ")")

    | lvalue(A.Lvalue_array(v,e),d) = (indent d; sayln "Lvalue_array("; 
                                       lvalue(v,d+1); sayln ",";
				                       exp(e,d+1); say ")")

  and exp(A.LvalueExp v, d) = (indent d; sayln "LvalueExp("; lvalue(v,d+1); say ")")

    | exp(A.NilExp, d) = (indent d; say "NilExp")

    | exp(A.IntExp i, d) = (indent d; say "IntExp("; say(Int.toString i); say ")")

    | exp(A.StringExp(s),d) = (indent d; say "StringExp(\""; say s; say "\")")

    | exp(A.FunCallExp{function_id, function_args},d) = (indent d; say "FunCallExp("; say(function_id);
			                                            say ",["; dolist d exp function_args; say "])")

    | exp(A.MethodCallExp{caller, method_id, method_args},d) = (indent d; say "MethodCallExp("; 
                                                        lvalue(caller, d); say(method_id);
			                                            say ",["; dolist d exp method_args; say "])")
    
    | exp(A.ObjectExp(s),d) = (indent d; say "ObjectExp("; say(s); say ")")

    | exp(A.ArrayExp{array_type, initial_val, length},d) =
	        (indent d; say "ArrayExp("; say(array_type); sayln ",";
		 exp(initial_val, d+1); sayln ","; exp(length, d+1); say ")")

    | exp(A.RecordExp{field,record_type},d) =
	    let fun f((name,e),d) = 
			(indent d; say "("; say(name);
			 sayln ","; exp(e,d+1);
			 say ")")
	     in indent d; say "RecordExp("; say(record_type); 
	        sayln ",["; dolist d f field; say "])" 
	    end

    | exp(A.NegationExp(e), d) = (indent d; say "NegationExp("; exp(e, d+1); say ")")

    | exp(A.OpExp(left,oper,right),d) = (indent d; say "OpExp("; say(opname oper); sayln ",";
		                                exp(left,d+1); sayln ","; exp(right,d+1); say ")")

    | exp(A.SequenceExp l, d) = (indent d; say "SeqExp["; dolist d exp l; say "]")

    | exp(A.AssignExp(v,e),d) = (indent d; sayln "AssignExp("; lvalue(v,d+1); sayln ",";
		                        exp(e,d+1); say ")")

    | exp(A.IfExp{if_,then_,else_},d) = (indent d; sayln "IfExp("; exp(if_,d+1); sayln ",";
                                        exp(then_,d+1);
                                        case else_ of NONE => ()
                                            | SOME e => (sayln ","; exp(e,d+1));
                                        say ")")

    | exp(A.WhileExp{while_,do_},d) = (indent d; say "WhileExp("; exp(while_,d+1); sayln ",";
		                              exp(do_,d+1); say ")")

    | exp(A.ForExp{var=v,from=lo,to_=hi,do_=body},d) = (indent d; say "ForExp("; say(v); say ",";
                                                        exp(lo,d+1); sayln ","; exp(hi,d+1); sayln ",";
                                                        exp(body,d+1); say ")")

    | exp(A.BreakExp, d) = (indent d; say "BreakExp")

    | exp(A.LetExp{declarations,in_},d) = (indent d; say "LetExp([";
		                                  dolist d dec declarations; sayln "],"; exp(in_,d+1); say")")


  and dec(A.TypeDec l, d) = let fun tdec((type_id,t),d) = (indent d; say"("; 
                                say(type_id); sayln ",";
                                ty(t,d+1); say ")")                          
                                in indent d; say "TypeDec["; dolist d tdec l; say "]"
                            end

    | dec(A.VarDec{var_id,var_type,value},d) = (indent d; say "VarDec("; say(var_id); say ",";
                                                case var_type of NONE => say "NONE" 
                                                    | SOME(s)=> (say "SOME("; say(s); say ")");
                                                    sayln ","; exp(value,d+1); say ")")

    | dec(A.ClassDec{class_id, class_extends, class_fields},d) = (indent d; say "ClassDec("; say(class_id); say ",";
                                                                case class_extends of NONE => say "NONE"
                                                                                 | SOME (s) => (say "SOME("; say(s); say ")");
                                                                            sayln ","; dolist d classfield class_fields; say ")")

    | dec(A.FunDec l, d) = 
	    let fun f({function_id,function_args,return_type,function_body},d) =
		   (indent d; say "("; say (function_id); say ",[";
		    tyfields(function_args, d); say "],";
		    case return_type of NONE => say "NONE"
			 | SOME(s) => (say "SOME("; say(s); say ")");
		    sayln ","; exp(function_body,d+1); say ")")
	     in indent d; say "FunDec["; dolist d f l; say "]"
	    end
    
    | dec(A.PrimDec{primitiveid, primitive_args,return_type},d) = (indent d; say "PrimDec(";
                                                say (primitiveid); say ",[";
                                                tyfields(primitive_args, d); sayln "],";
                                                case return_type of NONE => say "NONE"
			                                        | SOME(s) => (say "SOME("; say(s); say ")");
		                                        sayln ")")

 and   classfield(A.ClassVarDec{class_var_id,class_var_type,value}, d) = 
            (indent d; say "ClassVarDec("; say(class_var_id); say ","; 
            case class_var_type of NONE => say "NONE"
                     |          SOME(s) => (say "SOME("; say(s); say ")");
            say ","; exp(value, d+1); say(")"))

    |  classfield(A.Method l, d) = 
            let fun f({method_id,method_args,return_type,method_body},d) =
                (indent d; say "("; say (method_id); say ",[";
                tyfields(method_args, d); sayln "],";
                case return_type of NONE => say "NONE"
                | SOME(s) => (say "SOME("; say(s); say ")");
                sayln ","; exp(method_body,d+1); say ")")
	        in indent d; say "Method["; dolist d f l; say "]"
	        end

 and  ty(A.Type_id(s), d) = (indent d; say "Type_id("; say(s); say ")")

    | ty(A.Record_ty l, d) = (indent d; say "Record_ty["; tyfields(l, d); say "]")
		
    | ty(A.Array_ty(s),d) = (indent d; say "Array_ty("; say(s); say ")")

    | ty(A.Class_ty{class_extends,class_fields},d) = (indent d; say "Class_ty(";
                case class_extends of NONE => say "NONE"
                     |             SOME(s) => (say "SOME("; say(s); say ")");
                say ","; dolist d classfield class_fields; say ")" )

 and tyfields(A.Tyfield l, d) = let fun f((name,typ),d) = (indent d; say "("; say (name);
		                                                    say ","; say (typ); say ")")
                                in indent d; say "Tyfield["; dolist d f l; say "]"
                                end

 and program(A.Program e, d) = (indent d; say "Program("; exp(e, d+1); say ")")  
 
 in  program(e0,0); sayln ""; TextIO.flushOut outstream

 end
                                

end