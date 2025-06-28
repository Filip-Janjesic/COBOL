# User Data Entry Program in COBOL

This COBOL program allows a user to input their name and a message, which are then written to a text file named `user_data.txt`. It demonstrates basic file I/O operations in COBOL—specifically, writing sequential records to an output file.

---

## Program Overview

The program performs the following steps:

1. Prompts the user to enter their name.
2. Prompts the user to enter a message.
3. Opens a specified output file.
4. Writes the entered name and message as a single record into the file.
5. Closes the file.
6. Includes basic error handling for file operations.

---

## COBOL Source Code

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USER-ENTRY.
       AUTHOR. Filip.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "user_data.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05 OUT-NAME      PIC X(30).
           05 FILLER        PIC X(1) VALUE SPACE.
           05 OUT-MESSAGE   PIC X(70).

       WORKING-STORAGE SECTION.
       01 WS-USER-NAME     PIC X(30).
       01 WS-USER-MESSAGE  PIC X(70).
       01 WS-FILE-STATUS   PIC X(02).
           88 FILE-OK       VALUE "00".
           88 END-OF-FILE   VALUE "10".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "--- User Data Entry Program ---".
           DISPLAY "Enter your name (max 30 chars): ".
           ACCEPT WS-USER-NAME.

           DISPLAY "Enter a message (max 70 chars): ".
           ACCEPT WS-USER-MESSAGE.

           OPEN OUTPUT OUTPUT-FILE
               INVALID KEY
                   DISPLAY "Error opening file."
                   GOBACK
           END-OPEN.

           IF NOT FILE-OK THEN
               DISPLAY "Failed to open output file."
               GOBACK
           END-IF.

           MOVE WS-USER-NAME TO OUT-NAME.
           MOVE WS-USER-MESSAGE TO OUT-MESSAGE.

           WRITE OUTPUT-RECORD
               INVALID KEY
                   DISPLAY "Error writing to file."
                   CLOSE OUTPUT-FILE
                   GOBACK
           END-WRITE.

           IF NOT FILE-OK THEN
               DISPLAY "Failed to write to file."
               GOBACK
           END-IF.

           DISPLAY "Data successfully written to user_data.txt".

           CLOSE OUTPUT-FILE.
           STOP RUN.
```
Code Explanation
IDENTIFICATION DIVISION
Specifies metadata about the program (name, author, etc.).

ENVIRONMENT DIVISION → FILE-CONTROL
Links the internal file OUTPUT-FILE to the external file user_data.txt using line-sequential organization.

DATA DIVISION → FILE SECTION
Describes the structure of the record to be written:

OUT-NAME: 30-character field for the user's name.

FILLER: 1 space for separation.

OUT-MESSAGE: 70-character field for the user's message.

WORKING-STORAGE SECTION
Temporary variables for storing user input and file operation statuses.

PROCEDURE DIVISION
The main program logic:

Prompts the user for input.

Opens the output file.

Transfers input data into a record structure.

Writes the record to the file.

Displays success or error messages.

Closes the file and ends the program.

How to Compile and Run
Save the code
Save the COBOL source as user_entry.cob in:

makefile
Kopiraj kod
C:\Workspace\COBOL\6. user_entry\
Compile the program
Open your terminal (e.g., MINGW64) and run:

bash
Kopiraj kod
cd C:/Workspace/COBOL/6. user_entry/
cobc -x user_entry.cob
Run the program

bash
Kopiraj kod
./user_entry.exe
Result
After entering the name and message, a file named user_data.txt will be created in the same directory with the stored data.