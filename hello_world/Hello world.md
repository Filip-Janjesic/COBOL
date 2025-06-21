# Hello World in COBOL 
This COBOL program is about as basic as it gets! It's named HELLO, and its sole purpose is to display the message "Hello, World!" on the console before terminating. It's often the first program people write when learning a new language, serving as a simple verification that their development environment is set up correctly.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
``` 
This program consists of three main sections:
1. **IDENTIFICATION DIVISION**: This section provides metadata about the program, such as its name.
2. **PROGRAM-ID**: This specifies the name of the program, which in this case is `HELLO`.
3. **PROCEDURE DIVISION**: This is where the actual code resides. The `DISPLAY` statement outputs the string "Hello, World!" to the console, and `STOP RUN` indicates the end of the program.
The program is straightforward and serves as a great starting point for anyone new to COBOL, demonstrating the basic structure and syntax of the language.