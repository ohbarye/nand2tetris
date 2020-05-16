// bootstrap
@256
D=A
@SP
M=D

// call Sys.init
@_RETURN_LABEL_1
D=A
// push return-address

@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M

@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M

@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M

@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M

@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@5
D=D-A
@0
D=D-A
@ARG
M=D  // ARG = SP - n - 5
@SP
D=M
@LCL
M=D  // LCL = SP
@Sys.init
0;JMP  // goto function
(_RETURN_LABEL_1)
// function Main.fibonacci
(Main.fibonacci)

// push ARG 0
@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
// push constant 2
@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
AM=M-1
D=M
A=A-1
D=M-D
@FALSE1
D;JGE
@SP
A=M-1
M=-1
@CONTINUE1
0;JMP
(FALSE1)
@SP
A=M-1
M=0
(CONTINUE1)
// if-goto Main.fibonacci$IF_TRUE
@SP
AM=M-1
D=M
@Main.fibonacci$IF_TRUE
D;JNE
@Main.fibonacci$IF_FALSE
0;JMP
(Main.fibonacci$IF_TRUE)
// push ARG 0
@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
// return
@LCL
D=M
@R13
M=D  // R13 = FRAME = LCL
@5
D=A
@R13
A=M-D
D=M  // D = *(FRAME-5) = return-address
@R14
M=D  // R14 = return-address
@SP
M=M-1
A=M
D=M
@ARG
A=M  // M = *ARG
M=D  // *ARG = pop()

@ARG
D=M+1
@SP
M=D  // SP = ARG + 1

@R13
AM=M-1  // A = FRAME-1, R13 = FRAME-1
D=M
@THAT
M=D  // THAT = *(FRAME-1)

@R13
AM=M-1
D=M
@THIS
M=D  // THIS = *(FRAME-2)

@R13
AM=M-1
D=M
@ARG
M=D  // ARG = *(FRAME-3)

@R13
AM=M-1
D=M
@LCL
M=D  // LCL = *(FRAME-4)

@R14
A=M
0;JMP  // goto return-address
(Main.fibonacci$IF_FALSE)
// push ARG 0
@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
// push constant 2
@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
AM=M-1
D=M
A=A-1
M=M-D
// call Main.fibonacci
@_RETURN_LABEL_2
D=A
// push return-address

@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M

@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M

@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M

@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M

@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@5
D=D-A
@1
D=D-A
@ARG
M=D  // ARG = SP - n - 5
@SP
D=M
@LCL
M=D  // LCL = SP
@Main.fibonacci
0;JMP  // goto function
(_RETURN_LABEL_2)
// push ARG 0
@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
// push constant 1
@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
AM=M-1
D=M
A=A-1
M=M-D
// call Main.fibonacci
@_RETURN_LABEL_3
D=A
// push return-address

@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M

@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M

@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M

@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M

@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@5
D=D-A
@1
D=D-A
@ARG
M=D  // ARG = SP - n - 5
@SP
D=M
@LCL
M=D  // LCL = SP
@Main.fibonacci
0;JMP  // goto function
(_RETURN_LABEL_3)

@SP
AM=M-1
D=M
A=A-1
M=M+D
// return
@LCL
D=M
@R13
M=D  // R13 = FRAME = LCL
@5
D=A
@R13
A=M-D
D=M  // D = *(FRAME-5) = return-address
@R14
M=D  // R14 = return-address
@SP
M=M-1
A=M
D=M
@ARG
A=M  // M = *ARG
M=D  // *ARG = pop()

@ARG
D=M+1
@SP
M=D  // SP = ARG + 1

@R13
AM=M-1  // A = FRAME-1, R13 = FRAME-1
D=M
@THAT
M=D  // THAT = *(FRAME-1)

@R13
AM=M-1
D=M
@THIS
M=D  // THIS = *(FRAME-2)

@R13
AM=M-1
D=M
@ARG
M=D  // ARG = *(FRAME-3)

@R13
AM=M-1
D=M
@LCL
M=D  // LCL = *(FRAME-4)

@R14
A=M
0;JMP  // goto return-address
// function Sys.init
(Sys.init)

// push constant 4
@4
D=A
@SP
A=M
M=D
@SP
M=M+1
// call Main.fibonacci
@_RETURN_LABEL_4
D=A
// push return-address

@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M

@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M

@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M

@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M

@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@5
D=D-A
@1
D=D-A
@ARG
M=D  // ARG = SP - n - 5
@SP
D=M
@LCL
M=D  // LCL = SP
@Main.fibonacci
0;JMP  // goto function
(_RETURN_LABEL_4)
(Sys.init$WHILE)
@Sys.init$WHILE
0;JMP
