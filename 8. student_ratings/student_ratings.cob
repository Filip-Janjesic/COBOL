       IDENTIFICATION DIVISION.
       PROGRAM-ID. StudentRatings.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  Student-Counter     PIC 9 VALUE 1.
       77  Total-Students      PIC 9 VALUE 5.
       77  Student-Name        PIC A(20).
       77  Grade1              PIC 9.
       77  Grade2              PIC 9.
       77  Grade3              PIC 9.
       77  Avg-Grade           PIC 99V9.

       PROCEDURE DIVISION.
       BEGIN.
           PERFORM UNTIL Student-Counter > Total-Students
               DISPLAY "Enter student name:"
               ACCEPT Student-Name

               DISPLAY "Enter first grade:"
               ACCEPT Grade1

               DISPLAY "Enter second grade:"
               ACCEPT Grade2

               DISPLAY "Enter third grade:"
               ACCEPT Grade3

               COMPUTE Avg-Grade = (Grade1 + Grade2 + Grade3) / 3

               DISPLAY "Student: " Student-Name
               DISPLAY "Average grade: " Avg-Grade
               DISPLAY "---------------------------"

               ADD 1 TO Student-Counter
           END-PERFORM

           DISPLAY "All student ratings processed."
           STOP RUN.
         END PROGRAM StudentRatings.
         