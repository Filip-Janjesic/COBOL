# User Data Entry Program in COBOL

This COBOL program allows a user to input their name and a message, which are then written to a text file named `user_data.txt`. It demonstrates basic file I/O operations in COBOL—specifically, writing sequential records to an output file.

## Program Overview

The program performs the following steps:

1. Prompts the user to enter their name
2. Prompts the user to enter a message
3. Opens a specified output file
4. Writes the entered name and message as a single record into the file
5. Closes the file
6. Includes basic error handling for file operations

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

## Code Explanation

### IDENTIFICATION DIVISION

Specifies metadata about the program:
- **PROGRAM-ID. USER-ENTRY** - Names the program
- **AUTHOR. Filip** - Identifies the program author

### ENVIRONMENT DIVISION

**INPUT-OUTPUT SECTION → FILE-CONTROL**
- Links the internal file `OUTPUT-FILE` to the external file `user_data.txt` 
- Uses line-sequential organization for text file operations

### DATA DIVISION

**FILE SECTION**
- Describes the structure of the record to be written:
  - **`OUT-NAME`** - 30-character field for the user's name
  - **`FILLER`** - 1 space for separation between name and message
  - **`OUT-MESSAGE`** - 70-character field for the user's message

**WORKING-STORAGE SECTION**
- **`WS-USER-NAME`** - Temporary variable for storing user's name input
- **`WS-USER-MESSAGE`** - Temporary variable for storing user's message input
- **`WS-FILE-STATUS`** - Status variable for file operations with condition names:
  - `FILE-OK` - Indicates successful file operation (value "00")
  - `END-OF-FILE` - Indicates end of file condition (value "10")

### PROCEDURE DIVISION

The main program logic follows these steps:

1. **Display Program Header**
   ```cobol
   DISPLAY "--- User Data Entry Program ---"
   ```

2. **Collect User Input**
   ```cobol
   DISPLAY "Enter your name (max 30 chars): "
   ACCEPT WS-USER-NAME
   DISPLAY "Enter a message (max 70 chars): "
   ACCEPT WS-USER-MESSAGE
   ```

3. **Open Output File**
   ```cobol
   OPEN OUTPUT OUTPUT-FILE
   ```
   - Includes error handling for file opening failures

4. **Transfer Data to Record Structure**
   ```cobol
   MOVE WS-USER-NAME TO OUT-NAME
   MOVE WS-USER-MESSAGE TO OUT-MESSAGE
   ```

5. **Write Record to File**
   ```cobol
   WRITE OUTPUT-RECORD
   ```
   - Includes error handling for write operations

6. **Display Success Message and Close File**
   ```cobol
   DISPLAY "Data successfully written to user_data.txt"
   CLOSE OUTPUT-FILE
   ```

## How to Compile and Run

### Save the Code
Save the COBOL source as `user_entry.cob` in:
```
C:\Workspace\COBOL\6. user_entry\
```

### Compile the Program
Open your terminal (e.g., MINGW64) and run:
```bash
cd C:/Workspace/COBOL/6. user_entry/
cobc -x user_entry.cob
```

### Run the Program
```bash
./user_entry.exe
```

## Example Usage

```
--- User Data Entry Program ---
Enter your name (max 30 chars): John Smith
Enter a message (max 70 chars): Hello, this is my first COBOL program!
Data successfully written to user_data.txt
```

## Result

After entering the name and message, a file named `user_data.txt` will be created in the same directory with the stored data in the following format:
```
John Smith Hello, this is my first COBOL program!
```

## Key Features

- **File I/O Operations** - Demonstrates writing to sequential files
- **Error Handling** - Includes validation for file operations
- **Data Structure** - Shows how to organize output records
- **User Interaction** - Simple command-line interface for data entry
- **File Status Management** - Uses condition names for clear status checking

## Notes

- Maximum name length is 30 characters
- Maximum message length is 70 characters
- The program creates a new file each time it runs (overwrites existing data)
- Uses line-sequential organization for cross-platform compatibility
- Includes comprehensive error handling for production-ready code