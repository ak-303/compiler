

COMMON=./tiger/ast.sml ./target/pp.sml ./target/mips.sml



%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<


all: tc mips



.PHONY: all clean test

clean:
	rm -f ./tiger/*.grm.sml ./tiger/*.grm.desc ./tiger/*.grm.sig tc ./tiger/*.lex.sml ./target/mips 

tc: tc.sml tc.mlb ./tiger/tiger.grm.sml ./tiger/tiger.lex.sml ./tiger/printast.sml ${COMMON} 
	mlton tc.mlb
	

mips: ./target/mips.sml
	mlton target/mips.sml

test: all
	./tc test/test1.tig pp
	./tc test/test2.tig pp
	./tc test/test3.tig pp