# Kilograms to Pounds Converter in COBOL

This repository contains a simple COBOL program designed to convert a weight provided in kilograms to its equivalent in pounds. The program prompts the user to enter a weight in kilograms, performs the conversion using a fixed conversion factor, and then displays the result in pounds.

## Program Structure

The program is structured to:
1. Accept weight input in kilograms from the user.
2. Perform the conversion calculation.
3. Display the converted weight in pounds.

## COBOL Source Code

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KG_TO_POUNDS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-KG         PIC 9(5)V99.
           01 WS-POUNDS     PIC 9(6)V99.
           01 WS-DISPLAY-POUNDS PIC Z(5).99.
           01 WS-CONVERSION-FACTOR CONSTANT 2.20462.

       PROCEDURE DIVISION.
           DISPLAY "Enter weight in kilograms: ".
           ACCEPT WS-KG.

           MULTIPLY WS-KG BY WS-CONVERSION-FACTOR GIVING WS-POUNDS.

           MOVE WS-POUNDS TO WS-DISPLAY-POUNDS.
           DISPLAY "Weight in pounds: " WS-DISPLAY-POUNDS.

           STOP RUN.
       END PROGRAM KG_TO_POUNDS.
```

Explanation of the Code
IDENTIFICATION DIVISION:

PROGRAM-ID. KG_TO_POUNDS.: This line names the program KG_TO_POUNDS.

DATA DIVISION:

WORKING-STORAGE SECTION: This section declares the variables and constants used within the program.

01 WS-KG PIC 9(5)V99.: Declares a variable WS-KG to store the weight in kilograms. 9(5)V99 indicates it can hold up to 5 digits before the decimal point and 2 digits after.

01 WS-POUNDS PIC 9(6)V99.: Declares a variable WS-POUNDS to store the converted weight in pounds. 9(6)V99 allows for up to 6 digits before the decimal and 2 after.

01 WS-DISPLAY-POUNDS PIC Z(5).99.: Declares a display variable WS-DISPLAY-POUNDS to format the output of WS-POUNDS. Z(5).99 suppresses leading zeros and includes a decimal point for display.

01 WS-CONVERSION-FACTOR CONSTANT 2.20462.: Declares WS-CONVERSION-FACTOR as a constant with the value 2.20462, which is the approximate number of pounds in one kilogram.

PROCEDURE DIVISION:

DISPLAY "Enter weight in kilograms: ".: Prompts the user to enter the weight in kilograms.

ACCEPT WS-KG.: Reads the user's input and stores it in the WS-KG variable.

MULTIPLY WS-KG BY WS-CONVERSION-FACTOR GIVING WS-POUNDS.: Performs the core conversion calculation. It multiplies the value in WS-KG by WS-CONVERSION-FACTOR and stores the result in WS-POUNDS.

MOVE WS-POUNDS TO WS-DISPLAY-POUNDS.: Moves the numeric value from WS-POUNDS to the display-formatted variable WS-DISPLAY-POUNDS. This step is crucial for proper output formatting (e.g., suppressing leading zeros).

DISPLAY "Weight in pounds: " WS-DISPLAY-POUNDS.: Displays the converted weight to the console.

STOP RUN.: Terminates the program execution.

END PROGRAM KG_TO_POUNDS.: Explicitly marks the end of the KG_TO_POUNDS program.

How to Use
This program demonstrates fundamental COBOL concepts such as:

Input/Output Operations: Using DISPLAY for output and ACCEPT for input.

Data Declaration: Defining numeric variables with specific PIC clauses and declaring constants.

Arithmetic Operations: Performing a MULTIPLY operation for calculations.

Data Movement: Using MOVE to format data for display.

It serves as an excellent practical example for beginners to understand how to interact with users, perform basic arithmetic computations, and handle data formatting in COBOL.