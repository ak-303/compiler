(* structure TC = struct

fun Compile filename = let
    fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm, n))
    val makeFileLexer = makeTigerLexer o TextIO.openIn
    val thisLexer =  makeFileLexer filename

fun print_error (s,i:int,c:int) = TextIO.output(TextIO.stdErr,
						"Error, line " ^ (Int.toString i)^", col "^ (Int.toString c) ^ ", " ^ s ^ "\n")
					       
    
val (program, _) = TigerParser.parse(1, thisLexer, print_error, ())
in
    program
end

end *)

(* The compiler from expression to rp *)
structure TC =
struct
structure TigerLrVals    = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex       = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser    = Join( structure ParserData = TigerLrVals.ParserData
				 structure Lex = TigerLex
				 structure LrParser = LrParser
			       )
(* structure ExprLrVals = ExprLrValsFun(structure Token = LrParser.Token)
structure ExprLex    = ExprLexFun(structure Tokens = ExprLrVals.Tokens)
structure ExprParser = Join( structure ParserData = ExprLrVals.ParserData
			     structure Lex        = ExprLex
			     structure LrParser   = LrParser
			   ) *)

(* 

At this point every thing regarding lexing and parsing is contained in
the ExprParser structure. Let us create a lexer using this.

*)
(* Build Lexers *)
fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm,n))
val makeFileLexer      = makeTigerLexer o TextIO.openIn


(* Parse command line and set a suitable lexer *)

val thisLexer = case CommandLine.arguments() of
		    []  => makeTigerLexer TextIO.stdIn
		 |  [x] => makeFileLexer x
		 |  _   => (TextIO.output(TextIO.stdErr, "usage: ec file"); OS.Process.exit OS.Process.failure)


fun print_error (s,i:int,c:int) = TextIO.output(TextIO.stdErr,
						"Error, line " ^ (Int.toString i)^", col "^ (Int.toString c) ^ ", " ^ s ^ "\n")


val (program,_) = TigerParser.parse (0,thisLexer,print_error,()) (* parsing *)

val temp = PrintAst.print(TextIO.stdOut, program)
val temp = PP.compile(program)
val _ = TextIO.output(TextIO.stdOut, temp)
end
