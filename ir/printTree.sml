structure PrintIRTree : sig 
    val printTree : TextIO.outstream * Tree.stm * int -> unit
    val printLinearize : TextIO.outstream * Tree.stm list -> unit 
    val printBlocks : TextIO.outstream * Tree.stm list list -> unit 
end =
struct 
    structure T = Tree

    fun printTree(outstream, t0, flag) =
    let 

    fun say s = TextIO.output(outstream, s)
    fun sayln s = if flag = 1 then (say s; say "\n") 
                  else (say s; say " ")
                 

    fun indent 0 = ()
     |  indent n = if flag = 1 then (say "    "; indent(n-1))
                   else indent(n-1)
    
    fun bopname T.PLUS    = "PLUS"
    |   bopname T.MINUS   = "MINUS"
    |   bopname T.MUL     = "MUL"
    |   bopname T.DIV     = "DIV"
    |   bopname T.AND     = "AND"
    |   bopname T.OR      = "OR"
    |   bopname T.XOR     = "XOR"
    |   bopname T.LSHIFT  = "LSHIFT"
    |   bopname T.ARSHIFT = "ARSHIFT"
    |   bopname T.RSHIFT  = "RSHIFT" 

    fun ropname T.EQ      = "EQ"
    |   ropname T.NEQ     = "NEQ"
    |   ropname T.GT      = "GT"
    |   ropname T.GTE     = "GTE"
    |   ropname T.LT      = "LT"
    |   ropname T.LTE     = "LTE"
    |   ropname T.UGT     = "UGT"
    |   ropname T.ULT     = "ULT"
    |   ropname T.ULTE    = "ULTE"
    |   ropname T.UGTE    = "UGTE"  

    fun expr(T.CONST(x), d) = (indent d; say "CONST "; say(Int.toString x))
    |   expr(T.NAME(x), d)  = (indent d; say "NAME "; say(Int.toString x))
    |   expr(T.TEMP(x), d)  = (indent d; say "TEMP "; say(Int.toString x))

    |   expr(T.BINOP(opr, e1, e2), d) = (indent d; say "BINOP("; say(bopname(opr)); sayln ", "; 
                                        expr(e1, d+1); sayln", "; expr(e2, d+1); say ")")

    |   expr(T.MEM(e), d)             = (indent d; sayln "MEM("; expr(e, d+1); say ")")
    |   expr(T.CALL(e, lst), d)       = let fun f([], d)      = ()
                                            |   f((x::xs), d) = (indent d; expr(e, d); 
                                                                sayln ","; f(xs, d))
                                        in (indent d; sayln "CALL("; expr(e, d+1); 
                                            sayln ","; f(lst, d+1); say ")")
                                        end
    |   expr(T.ESEQ(s, e), d)         = (indent d; sayln("ESEQ("); stm(s, d+1); sayln ",";
                                        expr(e, d+1); say ")")

    and stm(T.MOVE(e1, e2), d)        = (indent d; sayln("MOVE("); expr(e1, d+1); sayln ",";
                                        expr(e2, d+1); say")")
    |   stm(T.EXP(e), d)              = (indent d; sayln("EXP("); expr(e, d+1); say ")")
    |   stm(T.JUMP(e, lst), d)        = let fun f([])      = ()
                                            |   f((x::xs)) = (say(Int.toString x); say", ";
                                                                f(xs))
                                        in (indent d; sayln "JUMP("; expr(e, d+1); 
                                            say ", "; say "("; f(lst); say ")"; say ")")
                                        end 
    |   stm(T.SEQ(s1, s2), d)         = (indent d; sayln "SEQ("; stm(s1, d+1); sayln ",";
                                        stm(s2, d+1); say ")" )
    |   stm(T.LABEL(l), d)            = (indent d; say "LABEL "; say(Int.toString l))
    |   stm(T.CJUMP(opr, e1, e2, l1, l2), d) = (indent d; sayln "CJUMP("; say(ropname(opr)); 
                                                sayln ","; expr(e1, d+1); sayln ","; 
                                                expr(e2, d+1); sayln ", "; say(Int.toString l1);
                                                sayln ","; say(Int.toString l2); say ")")                                           
    in  
        stm(t0, 0); sayln ""; TextIO.flushOut outstream
    end

    fun printLinearize(outstream, [])      = ()
     |  printLinearize(outstream, (x::xs)) = (printTree(outstream, x, 0); 
                                              TextIO.output(outstream, "\n"); 
                                              printLinearize(outstream, xs))

    fun printBlocks(outstream, []) = ()
     |  printBlocks(outstream, (x::xs)) = (printLinearize(outstream, x); 
                                           TextIO.output(outstream, "\n\n"); 
                                           printBlocks(outstream, xs))
end
