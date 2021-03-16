structure TC = struct

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

end
