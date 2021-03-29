

COMMON=./src/tiger/ast.sml ./src/target/pp.sml ./src/target/mips.sml



%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<


all: tc mips



.PHONY: all clean test

clean:
	rm -f ./src/tiger/*.grm.sml ./src/tiger/*.grm.desc ./src/tiger/*.grm.sig tc ./src/tiger/*.lex.sml ./src/target/mips 

tc: ./src/tiger/tc.sml ./src/tc.mlb ./src/tiger/tiger.grm.sml ./src/tiger/tiger.lex.sml ./src/tiger/printast.sml ${COMMON} 
	mlton src/tc.mlb
	mv src/tc .

mips: ./src/target/mips.sml
	mlton src/target/mips.sml

test: all
	./tc src/test/test1.tig 
	./tc src/test/test2.tig 
	./tc src/test/test3.tig