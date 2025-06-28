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
                  05 FILLER        PIC X(1) VALUE SPACES.
                  05 OUT-MESSAGE   PIC X(70).
       
              WORKING-STORAGE SECTION.
              01  WS-USER-NAME    PIC X(30).
              01  WS-USER-MESSAGE PIC X(70).
              01  WS-FILE-STATUS  PIC X(02).
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
                          DISPLAY "Error opening file: " WS-FILE-STATUS
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
                          DISPLAY "Error writing to file: " WS-FILE-STATUS
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
       