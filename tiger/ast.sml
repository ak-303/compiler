structure Ast =
struct
    
    type id = string
    type type_id = id
    type tyfield = id * type_id

    datatype program = exp  

    datatype exp = (* Literals *)
                   NilExp
                |  IntExp of int 
                |  StringExp of string 

                   (* Arrays and Records *)
                |  ArrayExp of {array: type_id list, initial_val: exp, length: exp}
                |  RecordExp of {field: (id * exp) list, record_type: type_id}                                                            (* DOUBT *)

                   (* Creation of object *)
                |  ObjectExp of type_id

                   (* Variables, fields and elements of an array *)
                |  LvalueExp of lvalue

                   (* Function and Method calls *)
                |  FunCallExp of {function_id: id, function_args: exp list}                  
                |  MethodCallExp of {caller: lvalue, method_id: id, method_args: exp list}

                   (* Operations and sequence of expressions *)
                |  NegationExp of exp
                |  OpExp of exp * BinOp * exp
                |  SequenceExp of exp list
                   
                   (* Assignment expressions *)
                |  AssignExp of lvalue * exp

                   (* Control Blocks *)
                |  IfExp of {if_: exp, then_: exp, else_: exp option}
                |  WhileExp of {while_: exp, do_: exp}
                |  ForExp of {var: id, from: exp, to_: exp, do_: exp}
                |  BreakExp
                |  LetExp of {declarations: dec list, in_: exp}

    (* and   exps   = Exps of exp list option *)

    and   lvalue = (* Variable *)
                   Lvalue_id of id

                   (* To access object of a class or field of a record *)
                |  Lvalue_field of lvalue * id

                   (* To access an element of an array *)
                |  Lvalue_array of lvalue * exp

    and   dec    = (* Type declaration *)
                   TypeDec of (type_id * ty) list
                   
                   (* Class definition - only supports alternative form -- SEE EPITA *)
                |  ClassDec of {class_id: id, class_extends: type_id option, class_fields: classfield list}

                   (* Variable declaration *)
                |  VarDec of {var_id: id, var_type: type_id option, value: exp}

                   (* Function definition *)                 
                |  FunDec of {function_id: id, function_args: tyfields, return_type: type_id option, function_body: exp} list

                   (* Primitive definition *)
                |  PrimDec of {primitiveid: id, primitive_args: tyfields, return_type: type_id option}

 (* and classfields = Classfields of classfield list *)

 and classfield  = (* Attributes of class *)
                   ClassVarDec of {class_var_id: id, class_var_type: type_id option, value: exp}
                   (* Methods of class*)
                |  Method of {method_id: id, method_args: tyfields, return_type: type_id option, method_body: exp} list

    and     ty   = (* type alias *)
                   Type_id of type_id
                   (* Record type definition *)
                |  Record_ty of tyfields
                   (* Array type definition *)
                |  Array_ty of type_id 
                   (*Class Canonical definition *)
                |  Class_ty of {class_extends: type_id option, class_fields: classfield list}

    and tyfields = Tyfield of tyfield list
    and BinOp    = Plus | Minus | Mul | Div | Eq | NotEq | GT | LT | GTEq | LTEq | And | Or 
    
    
        
     
end