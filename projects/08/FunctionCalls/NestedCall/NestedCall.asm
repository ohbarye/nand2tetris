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
// function Sys.init
(Sys.init)

// push constant 4000
@4000
D=A

@SP
A=M
M=D
@SP
M=M+1
// pop THIS
@SP
AM=M-1
D=M
@THIS
M=D
// push constant 5000
@5000
D=A

@SP
A=M
M=D
@SP
M=M+1
// pop THAT
@SP
AM=M-1
D=M
@THAT
M=D
// call Sys.main
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
@0
D=D-A
@ARG
M=D  // ARG = SP - n - 5
@SP
D=M
@LCL
M=D  // LCL = SP
@Sys.main
0;JMP  // goto function
(_RETURN_LABEL_2)
// pop 6
@SP
AM=M-1
D=M
@6
M=D
(Sys.init$LOOP)
@Sys.init$LOOP
0;JMP
// function Sys.main
(Sys.main)

// push constant 0
@0
D=A

@SP
A=M
M=D
@SP
M=M+1
// push constant 0
@0
D=A

@SP
A=M
M=D
@SP
M=M+1
// push constant 0
@0
D=A

@SP
A=M
M=D
@SP
M=M+1
// push constant 0
@0
D=A

@SP
A=M
M=D
@SP
M=M+1
// push constant 0
@0
D=A

@SP
A=M
M=D
@SP
M=M+1
// push constant 4001
@4001
D=A

@SP
A=M
M=D
@SP
M=M+1
// pop THIS
@SP
AM=M-1
D=M
@THIS
M=D
// push constant 5001
@5001
D=A

@SP
A=M
M=D
@SP
M=M+1
// pop THAT
@SP
AM=M-1
D=M
@THAT
M=D
// push constant 200
@200
D=A

@SP
A=M
M=D
@SP
M=M+1
// pop LCL 1
@LCL
D=M
@1
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
// push constant 40
@40
D=A

@SP
A=M
M=D
@SP
M=M+1
// pop LCL 2
@LCL
D=M
@2
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
// push constant 6
@6
D=A

@SP
A=M
M=D
@SP
M=M+1
// pop LCL 3
@LCL
D=M
@3
D=D+A
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
// push constant 123
@123
D=A

@SP
A=M
M=D
@SP
M=M+1
// call Sys.add12
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
@Sys.add12
0;JMP  // goto function
(_RETURN_LABEL_3)
// pop 5
@SP
AM=M-1
D=M
@5
M=D
// push LCL 0
@LCL
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
// push LCL 1
@LCL
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
// push LCL 2
@LCL
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
// push LCL 3
@LCL
D=M
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1
// push LCL 4
@LCL
D=M
@4
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
AM=M-1
D=M
A=A-1
M=M+D

@SP
AM=M-1
D=M
A=A-1
M=M+D

@SP
AM=M-1
D=M
A=A-1
M=M+D

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
// function Sys.add12
(Sys.add12)

// push constant 4002
@4002
D=A

@SP
A=M
M=D
@SP
M=M+1
// pop THIS
@SP
AM=M-1
D=M
@THIS
M=D
// push constant 5002
@5002
D=A

@SP
A=M
M=D
@SP
M=M+1
// pop THAT
@SP
AM=M-1
D=M
@THAT
M=D
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
// push constant 12
@12
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
