// push constant 0
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
// pop LCL 0
@LCL
D=M
@0
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
