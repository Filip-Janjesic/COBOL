# Adding 2 numbers in COBOL 
This repository contains a simple COBOL program that allows the user to input two numbers, adds them, and displays the result.
# The program is structured to read input from the user, perform the addition, and then output the result.

```cobol
    IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDING_2_NUMBERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  NUM1       PIC 9(4).
           01  NUM2       PIC 9(4).
           01  TOTAL      PIC 9(4).

       PROCEDURE DIVISION.
           DISPLAY "Enter first number: ".
           ACCEPT NUM1.
           DISPLAY "Enter second number: ".
           ACCEPT NUM2.
           ADD NUM1 TO NUM2 GIVING TOTAL.
           DISPLAY "The sum is: " TOTAL.
           STOP RUN.
```
# Explanation of the Code:
1. **IDENTIFICATION DIVISION**: This section identifies the program with a name, `ADDING_2_NUMBERS`.
2. **DATA DIVISION**: This section defines the data structures used in the program.
   - `WORKING-STORAGE SECTION`: This is where variables are declared.
     - `NUM1` and `NUM2` are defined to hold the two numbers, each capable of storing a 4-digit integer.
     - `TOTAL` is defined to store the result of the addition.
3. **PROCEDURE DIVISION**: This section contains the executable code.
    - The program prompts the user to enter the first number and stores it in `NUM1`.
    - It then prompts for the second number and stores it in `NUM2`.
    - The `ADD` statement adds `NUM1` and `NUM2`, storing the result in `TOTAL`.
    - Finally, it displays the result using the `DISPLAY` statement.
This COBOL program demonstrates basic input/output operations and arithmetic calculations. It serves as a simple example for beginners to understand how to read user input, perform calculations, and display results in COBOL. By following the steps outlined above, you can easily run the program and see how it works in practice.
