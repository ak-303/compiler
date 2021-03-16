structure TigerLrVals    = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex       = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser    = Join( structure ParserData = TigerLrVals.ParserData
				 structure Lex = TigerLex
				 structure LrParser = LrParser
			       )
