structure TC =
struct
structure TigerLrVals    = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex       = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser    = Join( structure ParserData = TigerLrVals.ParserData
				 structure Lex = TigerLex
				 structure LrParser = LrParser
			       )

fun makeTigerLexer strm = TigerParser.makeLexer (fn n => TextIO.inputN(strm,n))
val makeFileLexer      = makeTigerLexer o TextIO.openIn


val second_argument = ref ""

val thisLexer = case CommandLine.arguments() of
		        []   	 => makeTigerLexer TextIO.stdIn
		 |     [x] 	 	 => makeFileLexer x
		 |  (x0::x1::[]) => (second_argument := x1;  makeFileLexer x0)
		 |  		_    => (TextIO.output(TextIO.stdErr, "usage: tc file\n"); OS.Process.exit OS.Process.failure)


fun print_error (s,i:int,c:int) = TextIO.output(TextIO.stdErr,
						"Error, line " ^ (Int.toString i)^", col "^ (Int.toString c) ^ ", " ^ s ^ "\n")


val (program,_) = TigerParser.parse (0,thisLexer,print_error,()) (* parsing *)

fun execute (prg, "")    = (PrintAst.print(TextIO.stdOut, prg); TextIO.output(TextIO.stdOut, PP.compile(prg)))
 |  execute (prg, "ast") = PrintAst.print(TextIO.stdOut, prg)
 | 	execute (prg, "pp")  = TextIO.output(TextIO.stdOut, PP.compile(prg))
 |	execute (prg, _)     = (TextIO.output(TextIO.stdErr, "undefined: unknown option passed\n"); OS.Process.exit OS.Process.failure)

val _ = execute (program, !second_argument) 


end