       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
         AUTHOR. FILIP JANJESIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  NUM1          PIC 9(4)V99.
           01  NUM2          PIC 9(4)V99.
           01  TOTAL         PIC 9(6)V99.
           01  DISPLAY-TOTAL PIC Z(6).99.
           01  OPERATION     PIC X(1).

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
