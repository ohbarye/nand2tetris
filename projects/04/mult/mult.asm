// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.
@i
M=1
@sum
M=0

(LOOP)
    @i
    D=M
    @1
    D=D-M // D = i - m
    @END
    D;JGT // if i - n > 0 then goto @END 
    @0
    D=M // D = n
    @sum
    M=D+M // sum = sum + n
    @i
    M=M+1 // i = i + 1
    @LOOP
    0;JMP
(END)

@END
@sum
D=M
@2
M=D
@200
0;JMP
