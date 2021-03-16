Name : Ashish Kumar Gupta

Roll No. : 111801052


STEPS TO FOLLOW TO RUN TIGER PARSER: (Generates AST - can be visualized)
Please type the following in your terminal:
mlyacc tiger.grm
mllex  tiger.lex
sml    
Control.Print.printDepth := 100   
CM.make "tiger.cm";
TC.Compile "example1.tig"
TC.Compile "example2.tig"

The third step is to just open sml mode in terminal.
The fourth step is done for better visualization of AST. 
You can get the generated AST on custom .tig files by following same approach as in step 5 and 6.

NOTE that the files (in assignment 3 which asked us to just write grm and lex file) - tc.sml, glue.sml were written by taking inspiration from Yuvraj Raghuvanshi (111801048). They will be changed and updated when the assignment to do so will be put. They are just added to find out the correctness of code.
 
 