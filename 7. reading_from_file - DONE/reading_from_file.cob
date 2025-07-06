           IDENTIFICATION DIVISION.
           PROGRAM-ID. READING_FROM_FILE.
           AUTHOR. FILIP JANJESIC.

           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT testfile ASSIGN TO "test.dat" *> CHANGED TO RELATIVE PATH
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS file-status.

           DATA DIVISION.
           FILE SECTION.
           FD testfile.
           01 file-line PIC X(100).

           WORKING-STORAGE SECTION.
           01 file-status PIC XX.
           01 eof-flag PIC X VALUE 'N'.
               88 end-of-file VALUE 'Y'.
               88 not-at-end  VALUE 'N'.

           PROCEDURE DIVISION.
               DISPLAY "Opening file..." UPON CONSOLE.
               OPEN INPUT testfile
               DISPLAY "File status after OPEN: " file-status UPON CONSOLE. *> Added a period for clarity, not strictly needed by COBOL but good practice

               IF file-status NOT = "00"
                   DISPLAY "File could not be opened. Status: " file-status UPON CONSOLE
                   STOP RUN
               END-IF.

               DISPLAY "Trying to read file..." UPON CONSOLE.
               PERFORM UNTIL end-of-file
                   READ testfile
                       AT END
                           SET end-of-file TO TRUE
                           DISPLAY "End of file reached." UPON CONSOLE
                       NOT AT END
                           DISPLAY "Read line: " file-line UPON CONSOLE
                   END-READ
               END-PERFORM.

               CLOSE testfile.
               DISPLAY "File closed." UPON CONSOLE.
               STOP RUN.
           END PROGRAM READING_FROM_FILE.
