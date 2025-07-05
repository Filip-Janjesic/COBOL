# Simple Calculator in COBOL

This repository contains a COBOL program that functions as a simple calculator. It allows the user to input two numbers and choose an arithmetic operation (addition, subtraction, multiplication, or division), then displays the result.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 NUM1 PIC 9(4)V99.
           01 NUM2 PIC 9(4)V99.
           01 TOTAL PIC 9(6)V99.
           01 DISPLAY-TOTAL PIC Z(6).99.
           01 OPERATION PIC X(1).

       PROCEDURE DIVISION.
           DISPLAY "Enter first number: ".
           ACCEPT NUM1.
           DISPLAY "Enter second number: ".
           ACCEPT NUM2.
           DISPLAY "Enter operation (+, -, *, /): ".
           ACCEPT OPERATION.

           EVALUATE OPERATION
               WHEN "+"
                   ADD NUM1 TO NUM2 GIVING TOTAL
                   MOVE TOTAL TO DISPLAY-TOTAL
                   DISPLAY "Result: " DISPLAY-TOTAL
               WHEN "-"
                   SUBTRACT NUM2 FROM NUM1 GIVING TOTAL
                   MOVE TOTAL TO DISPLAY-TOTAL
                   DISPLAY "Result: " DISPLAY-TOTAL
               WHEN "*"
                   MULTIPLY NUM1 BY NUM2 GIVING TOTAL
                   MOVE TOTAL TO DISPLAY-TOTAL
                   DISPLAY "Result: " DISPLAY-TOTAL
               WHEN "/"
                   IF NUM2 NOT = 0
                       DIVIDE NUM1 BY NUM2 GIVING TOTAL
                       MOVE TOTAL TO DISPLAY-TOTAL
                       DISPLAY "Result: " DISPLAY-TOTAL
                   ELSE
                       DISPLAY "Cannot divide by zero."
                   END-IF
               WHEN OTHER
                   DISPLAY "Invalid operation."
           END-EVALUATE.

           STOP RUN.
       END PROGRAM CALCULATOR.
```

## Explanation of the Code:

### IDENTIFICATION DIVISION
- This section identifies the program, in this case, named `CALCULATOR`.

### DATA DIVISION
- This section defines the data structures (variables) used in the program.

### WORKING-STORAGE SECTION
- This is where variables are declared.
- `NUM1` and `NUM2` are defined to hold the two input numbers. `PIC 9(4)V99` means they can store a number with up to 4 digits before the decimal point and 2 digits after.
- `TOTAL` is defined to store the result of the calculation, capable of holding up to 6 digits before the decimal and 2 after.
- `DISPLAY-TOTAL` is a display variable for `TOTAL`, formatted to include a decimal point and suppress leading zeros (`Z(6).99`).
- `OPERATION` is a single character (`PIC X(1)`) variable to store the chosen arithmetic operation.

### PROCEDURE DIVISION
- This section contains the executable logic of the program.
- The program prompts the user to enter the first number and stores it in `NUM1`.
- It then prompts for the second number and stores it in `NUM2`.
- Next, it prompts the user to enter the desired operation (`+`, `-`, `*`, or `/`) and stores it in `OPERATION`.
- An `EVALUATE` statement is used to perform different actions based on the `OPERATION` entered by the user:
  - `WHEN "+"`: Adds `NUM1` to `NUM2` and stores the result in `TOTAL`.
  - `WHEN "-"`: Subtracts `NUM2` from `NUM1` and stores the result in `TOTAL`.
  - `WHEN "*"`: Multiplies `NUM1` by `NUM2` and stores the result in `TOTAL`.
  - `WHEN "/"`: Divides `NUM1` by `NUM2`. It includes a check to prevent division by zero. If `NUM2` is zero, it displays an error message; otherwise, it performs the division.
  - `WHEN OTHER`: If the entered operation is not one of the recognized symbols, it displays "Invalid operation."
- After the calculation (or error message), the `TOTAL` (or `DISPLAY-TOTAL` for formatted output) is displayed.
- `STOP RUN` terminates the program.

This COBOL program demonstrates how to implement a basic calculator, showcasing user input, conditional logic (`EVALUATE`), and arithmetic operations in COBOL.
