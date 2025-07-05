# Even Day Checker in COBOL
This repository contains a simple COBOL program designed to determine if a user-provided day number is even or odd. The program takes a day name and a day number (from 1 to 31) as input, performs a modulo operation, and then displays the result.
## Program Structure

The program is structured to:
1. Read input from the user for the day name and day number.
2. Perform a modulo operation on the day number.
3. Display whether the day number is even or odd.

## COBOL Source Code

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVEN_DAY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-DAY-NAME    PIC X(10).
           01 WS-DAY-NUMBER  PIC 99.

       PROCEDURE DIVISION.
           DISPLAY "Enter the day name: ".
           ACCEPT WS-DAY-NAME.
           DISPLAY "Enter the day number (1-31): ".
           ACCEPT WS-DAY-NUMBER.

           IF WS-DAY-NUMBER MOD 2 = 0
               DISPLAY "The day is even."
           ELSE
               DISPLAY "The day is odd."
           END-IF.

           STOP RUN.
       END PROGRAM EVEN_DAY.
```
## Explanation of the Code

* **IDENTIFICATION DIVISION**:
    * `PROGRAM-ID. EVEN_DAY.`: This line names the program `EVEN_DAY`.

* **DATA DIVISION**:
    * **WORKING-STORAGE SECTION**: This section declares the variables used within the program.
        * `01 WS-DAY-NAME PIC X(10).`: Declares a variable `WS-DAY-NAME` that can store up to 10 alphanumeric characters, intended for the name of the day (e.g., "Monday", "Tuesday").
        * `01 WS-DAY-NUMBER PIC 99.`: Declares a variable `WS-DAY-NUMBER` that can store a two-digit numeric value, intended for the calendar day (from 1 to 31).

* **PROCEDURE DIVISION**:
    * `DISPLAY "Enter the day name: ".`: Prompts the user to enter the day name.
    * `ACCEPT WS-DAY-NAME.`: Reads the user's input and stores it in `WS-DAY-NAME`.
    * `DISPLAY "Enter the day number (1-31): ".`: Prompts the user to enter the day number.
    * `ACCEPT WS-DAY-NUMBER.`: Reads the user's input and stores it in `WS-DAY-NUMBER`.
    * `IF WS-DAY-NUMBER MOD 2 = 0`: This is a conditional statement that checks if the `WS-DAY-NUMBER` divided by 2 has a remainder of 0. The `MOD` operator performs the modulo operation.
        * `DISPLAY "The day is even."`: If the condition is true (the number is even), this message is displayed.
        * `ELSE`: If the condition is false (the number is odd).
        * `DISPLAY "The day is odd."`: This message is displayed.
    * `END-IF.`: Marks the end of the `IF` statement.
    * `STOP RUN.`: Terminates the program execution.
    * `END PROGRAM EVEN_DAY.`: Explicitly marks the end of the `EVEN_DAY` program.

## How to Use

This program demonstrates fundamental COBOL concepts such as:
* **Input/Output Operations**: Using `DISPLAY` for output and `ACCEPT` for input.
* **Data Declaration**: Defining variables with specific picture clauses.
* **Conditional Logic**: Utilizing the `IF...ELSE...END-IF` structure for decision-making.
* **Numeric Operations**: Performing a modulo operation to check for even/odd numbers.

It serves as an excellent practical example for beginners to understand how to interact with users, process numeric input, and implement simple validation logic in COBOL.