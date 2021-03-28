

COMMON=ast.sml ../target/pp.sml



%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<


all: tc



.PHONY: all clean test

clean:
	rm -f *.grm.sml *.grm.desc *.grm.sig tc *.lex.sml

tc: tc.sml tc.mlb tiger.grm.sml tiger.lex.sml printast.sml ${COMMON} 
	mlton tc.mlb
