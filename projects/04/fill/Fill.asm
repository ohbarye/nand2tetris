// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

// initializes i with 8192. The number indicates remaining addresses to be colored.
(INIT)
    @8192
    D=A
    @i
    M=D

// Core loop logic
(LOOP)
    MD=M[i]-1
    @INIT
    D;JLT     // if index < 0 then goto INIT to reset it. Coloring is done. It repeats loop even if it's done.
    D=M[KBD]  // which key is being pressed?
    @WHITE    // if (Memory at keyboard address == 0) then goto WHITE, 
    D;JEQ
    @BLACK    // else go to BLACK
    0;JMP

(BLACK)             
    @SCREEN
    D=A
    A=D+M[i]  // add `i` to the screen's initial address in order to get the address to be colored.
    M=-1      // sets value in current address to -1: -1 is 111...., means all pixels are black.
    @LOOP     // jumps back to LOOP
    0;JMP

(WHITE)
    @SCREEN
    D=A                
    A=D+M[i]  // add `i` to the screen's initial address in order to get the address to be colored.
    M=0       // sets value in current address to -1: -1 is 111...., means all pixels are black.
    @LOOP     // jumps back to LOOP
    0;JMP
