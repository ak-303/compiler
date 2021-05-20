# FOR EVALUATION 

# LAB-5 
- Since, this lab didn't have any output the initial commit just contain code and no output can be seen corresponding to it, so no ir option here. 
Date: 28/04/2021 
Hash: bdb0531b0c0ac4d77f3eab8bf37d2440bc6b3f9b

- For checking output of canonization(LAB-5) with --can, use LAB-6 commits.

# LAB-6
- General Info: sp, fp, ret, and ra in output are special registers which hold the value of stack pointer, frame pointer, return value and return address respectively. I am not identifying nested functions so if they are present wrong output would be generated or an error. Also, everything is integer typed - So for eg while returns 0 by default.

- I request you to evaluate this lab in two parts because I wasn't able to complete within deadline. You can choose the best out of two for final grade. 

Option 1: This one contains all implementations except support for functions. To print IR Tree use '--ir' option and to print Canonization output use '--can' option. The canonization here only does Linearization (Chapter 7). 
Date: 15/05/2021
Hash: 9a8162cbdedf979ffc5ef37adafa987d12111119

Option 2: This one contains support for functions too. To print IR Tree use '--ir' option and to print canonization output use '--can' option. The canonization here only does Linearization (Chapter 7). However, if you want basic blocks output use '--bb' option, there is some problem with it, it works fine but for some codes the output won't be completely correct, I will rectify those later since it is not part of grading and is extra. 
Date: 20/05/2021
Hash: 53eed31821dd268c83095fed2bb566a3752d6141



WEEKLY CHANGES WILL BE ADDED HERE

Week 1 : Makefile, src folder added

Week 2 : reverse-polish with integer division and brackets support added

Week 3 : Initial ASTs for tiger and MIPS added. Some changes might be required.  

Week 4 : grm and lex files for tiger added. Now AST can be generated.  

Week 5 : Added print option for the AST of tiger. Read README to see how to print.

Week 6 : Added pretty printer. 

Week 7 : Added IR Tree. Canonisation started not complete. Directory structure updated. 
Date Apr 21

Week 8 : Options in the desired format addded and basic blocks in canonisation added. Date Apr 28

Week 9 : Added partial translate and environment. Date May 6.

Week 10 : Rectified many errors. Made every expression of type integer. Linearize output printing. Some problem with basic blocks will be rectified and added. Date May 15.

Added support for Functions. --bb option for basic blocks (but there is some problem with it.) Date May 20.