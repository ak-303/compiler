
.PHONY: all clean

all: 
	mlton src/tc.sml
	mv src/tc .


clean:
	rm tc

