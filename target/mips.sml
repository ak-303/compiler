structure MIPS = struct

    type immediate = int
    type ('l, 't) address = {register: 't option, imm: immediate, symbol: 'l option}
    

    datatype regs = zero | at | v0 | v1 | a0 | a1 | a2 | a3 | k0 | k1
                |     t0 | t1 | t2 | t3 | t4 | t5 | t6 | t7 | t8 | t9
                |     s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7 
                |     gp | sp | fp | ra | BadVAddr | Status | Cause | EPC

    datatype ('l, 't) inst = rfe | syscall | break | nop      (* exception and trap inst *) 
                             
                             (* load inst *)
                        |    la   of   't * ('l, 't) address
                        |    lb   of   't * ('l, 't) address | lbu  of 't * ('l, 't) address
                        |    ld   of   't * ('l, 't) address
                        |    lh   of   't * ('l, 't) address | lhu  of 't * ('l, 't) address
                        |    lw   of   't * ('l, 't) address
                        |    lwcz of   't * ('l, 't) address
                        |    lwl  of   't * ('l, 't) address | lwr  of 't * ('l, 't) address 
                        |    ulh  of   't * ('l, 't) address | ulhu of 't * ('l, 't) address
                        |    ulw  of   't * ('l, 't) address   

                             (* store inst *)
                        |    sb   of   't * ('l, 't) address
                        |    sd   of   't * ('l, 't) address
                        |    sh   of   't * ('l, 't) address
                        |    sw   of   't * ('l, 't) address
                        |    swcz of   't * ('l, 't) address
                        |    swl  of   't * ('l, 't) address | swr  of 't * ('l, 't) address
                        |    ush  of   't * ('l, 't) address
                        |    usw  of   't * ('l, 't) address

                             (* constant manipulating inst *)
                        |    li   of   't * immediate
                        |    lui  of   't * immediate

                             (* Arithmetic and Logical inst *)
                        |    abs     of   't * 't 
                        
                        |    add     of   't * 't * 't
                        |    addi    of   't * 't * immediate 
                        |    addu    of   't * 't * 't
                        |    addiu   of   't * 't * immediate
                        |    sub     of   't * 't * 't
                        |    subu    of   't * 't * 't
                        
                        |    andInst of   't * 't * 't
                        |    andInsti of  't * 't * immediate
                        |    or      of   't * 't * 't
                        |    ori     of   't * 't * immediate
                        |    nor     of   't * 't * 't
                        |    xor     of   't * 't * 't
                        |    xori    of   't * 't * immediate
                        |    not     of   't * 't 
                        
                        |    divi    of   't * 't 
                        |    diviu   of   't * 't
                        |    div     of   't * 't * 't
                        |    divu    of   't * 't * 't
                        |    rem     of   't * 't * 't
                        |    remu    of   't * 't * 't
                        
                        |    mul     of   't * 't * 't
                        |    mulo    of   't * 't * 't
                        |    mulou   of   't * 't * 't
                        |    mult    of   't * 't
                        |    multu   of   't * 't

                        |    neg     of   't * 't
                        |    negu    of   't * 't

                        |    rol     of   't * 't * 't
                        |    ror     of   't * 't * 't
                        |    sll     of   't * 't * 't
                        |    sllv    of   't * 't * 't
                        |    sra     of   't * 't * 't
                        |    srav    of   't * 't * 't
                        |    srl     of   't * 't * 't
                        |    srlv    of   't * 't * 't

                             (* comparision inst *)
                        |    seq     of   't * 't * 't 
                        |    sge     of   't * 't * 't
                        |    sgeu    of   't * 't * 't
                        |    sgt     of   't * 't * 't
                        |    sgtu    of   't * 't * 't
                        |    sle     of   't * 't * 't
                        |    sleu    of   't * 't * 't
                        |    slt     of   't * 't * 't
                        |    slti    of   't * 't * immediate
                        |    sltu    of   't * 't * 't
                        |    sltiu   of   't * 't * immediate
                        |    sne     of   't * 't * 't

                             (* branch and jump inst *)
                        |    b       of   'l
                        |    bczt    of   'l
                        |    bczf    of   'l
                        |    beq     of   't * 't * 'l
                        |    beqz    of   't * 'l
                        |    bge     of   't * 't * 'l
                        |    bgeu    of   't * 't * 'l
                        |    bgez    of   't * 'l
                        |    bgezal  of   't * 'l
                        |    bgt     of   't * 't * 'l
                        |    bgtu    of   't * 't * 'l
                        |    bgtz    of   't * 'l
                        |    ble     of   't * 't * 'l
                        |    bleu    of   't * 't * 'l
                        |    blez    of   't * 'l
                        |    bltzal  of   't * 'l
                        |    blt     of   't * 't * 'l
                        |    bltu    of   't * 't * 'l
                        |    bltz    of   't * 'l
                        |    bne     of   't * 't * 'l
                        |    bnez    of   't * 'l
                        |    j       of   'l
                        |    jal     of   'l
                        |    jalr    of   't
                        |    jr      of   't

                             (*Data Movement inst*)
                        |    move    of    't * 't
                        |    mfhi    of    't
                        |    mflo    of    't
                        |    mthi    of    't
                        |    mtlo    of    't
                        |    mfcz    of    't * 't
                        |    mtcz    of    't * 't
                    
    
    datatype label       =   Luser   of    string
                        |    Ltemp   of    int
end
