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
       FD  OUTPUT-FILE
           RECORD CONTAINS 101 CHARACTERS
           DATA RECORD IS OUTPUT-RECORD.
       01  OUTPUT-RECORD       PIC X(101).

       WORKING-STORAGE SECTION.
       01  WS-USER-NAME        PIC X(30).
       01  WS-USER-MESSAGE     PIC X(70).
       01  WS-FILE-STATUS      PIC X(02).
           88  FILE-OK         VALUE "00".
       01  WS-DUMMY            PIC X.

       PROCEDURE DIVISION.
           PERFORM MAIN-LOGIC
           STOP RUN.

       MAIN-LOGIC.
           DISPLAY "Enter your name:".
           ACCEPT WS-USER-NAME.

           DISPLAY "Enter a message:".
           ACCEPT WS-USER-MESSAGE.

           OPEN OUTPUT OUTPUT-FILE.

           STRING
               WS-USER-NAME DELIMITED BY SIZE
               " "           DELIMITED BY SIZE
               WS-USER-MESSAGE DELIMITED BY SIZE
               INTO OUTPUT-RECORD
           END-STRING

           WRITE OUTPUT-RECORD.

           CLOSE OUTPUT-FILE.

           DISPLAY "Data written to user_data.txt".
           DISPLAY "Press ENTER to exit.".
           ACCEPT WS-DUMMY.

           EXIT.
       END PROGRAM USER-ENTRY.
