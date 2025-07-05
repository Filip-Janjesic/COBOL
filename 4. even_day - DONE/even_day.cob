       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVEN_DAY.
       AUTHOR. FILIP JANJESIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-DAY-NAME    PIC X(10).
           01 WS-DAY-NUMBER  PIC 99.

       PROCEDURE DIVISION.
           DISPLAY "Enter the day name: ".
           ACCEPT WS-DAY-NAME.

           DISPLAY "Enter the day number (1-31): ".
           ACCEPT WS-DAY-NUMBER.

           IF FUNCTION MOD(WS-DAY-NUMBER, 2) = 0
               DISPLAY "The day is even."
           ELSE
               DISPLAY "The day is odd."
           END-IF.

           STOP RUN.
       END PROGRAM EVEN_DAY.
       