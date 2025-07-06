# Reading From File Program in COBOL

This COBOL program reads the content of a text file `test.dat` line by line and displays each line on the screen. It demonstrates basic file input operations in COBOL—specifically, reading sequential records from an input file.

## Program Overview

The program performs the following steps:

1. Opens the file `test.dat` in the same directory
2. Checks if the file was successfully opened
3. Reads the file line by line until reaching the end of file
4. Displays each read line on the screen
5. Closes the file after completion
6. Displays status messages for easier execution tracking

## COBOL Source Code

```cobol
           IDENTIFICATION DIVISION.
           PROGRAM-ID. READING_FROM_FILE.
           AUTHOR. FILIP JANJESIC.

           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT testfile ASSIGN TO "test.dat"
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
               DISPLAY "File status after OPEN: " file-status UPON CONSOLE.

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
```

## Code Explanation

### IDENTIFICATION DIVISION

Specifies metadata about the program:
- **PROGRAM-ID. READING_FROM_FILE** - Names the program
- **AUTHOR. FILIP JANJESIC** - Identifies the program author

### ENVIRONMENT DIVISION

**INPUT-OUTPUT SECTION → FILE-CONTROL**
- **`SELECT testfile ASSIGN TO "test.dat"`** - Links the internal file `testfile` to the external file `test.dat`
- **`ORGANIZATION IS LINE SEQUENTIAL`** - Specifies line-by-line reading
- **`FILE STATUS IS file-status`** - Associates file operations with status tracking

### DATA DIVISION

**FILE SECTION**
- **`FD testfile`** - File descriptor for the input file
- **`01 file-line PIC X(100)`** - Record structure to hold each line (maximum 100 characters)

**WORKING-STORAGE SECTION**
- **`01 file-status PIC XX`** - Two-character field to store file operation status codes
- **`01 eof-flag PIC X VALUE 'N'`** - End-of-file flag with condition names:
  - `end-of-file` - Indicates end of file has been reached (value 'Y')
  - `not-at-end` - Indicates more data available (value 'N')

### PROCEDURE DIVISION

The main program logic follows these steps:

1. **Display Opening Message**
   ```cobol
   DISPLAY "Opening file..." UPON CONSOLE
   ```

2. **Open Input File**
   ```cobol
   OPEN INPUT testfile
   DISPLAY "File status after OPEN: " file-status UPON CONSOLE
   ```

3. **Check File Status**
   ```cobol
   IF file-status NOT = "00"
       DISPLAY "File could not be opened. Status: " file-status UPON CONSOLE
       STOP RUN
   END-IF
   ```
   - Status "00" indicates successful file opening
   - Any other status indicates an error

4. **Read File Loop**
   ```cobol
   PERFORM UNTIL end-of-file
       READ testfile
           AT END
               SET end-of-file TO TRUE
               DISPLAY "End of file reached." UPON CONSOLE
           NOT AT END
               DISPLAY "Read line: " file-line UPON CONSOLE
       END-READ
   END-PERFORM
   ```
   - Continues reading until end-of-file condition is met
   - Displays each line as it's read

5. **Close File and Terminate**
   ```cobol
   CLOSE testfile
   DISPLAY "File closed." UPON CONSOLE
   STOP RUN
   ```

## How to Compile and Run

### Save the Code
Save the COBOL source as `reading_from_file.cob` in your working directory.

### Compile the Program
Open your terminal and navigate to the directory containing the program:
```bash
cobc -x -o readfile.exe reading_from_file.cob
```

### Run the Program
```bash
./readfile.exe
```

## Example Input File

Create a file named `test.dat` in the same directory with the following content:
```
Hello COBOL!
TEST LINE 2
TEST LINE 3
TEST LINE 4
TEST LINE 5
```

## Example Output

When you run the program, it will display:
```
Opening file...
File status after OPEN: 00
Trying to read file...
Read line: Hello COBOL!
Read line: TEST LINE 2
Read line: TEST LINE 3
Read line: TEST LINE 4
Read line: TEST LINE 5
End of file reached.
File closed.
```

## File Status Codes

Common file status codes you might encounter:

| Status | Description |
|--------|-------------|
| 00     | Successful operation |
| 10     | End of file reached |
| 30     | Permanent error (file not found) |
| 35     | File does not exist |
| 37     | Permission denied |

## Key Features

- **Sequential File Reading** - Demonstrates reading files line by line
- **Error Handling** - Includes file status checking and error messages
- **End-of-File Detection** - Properly handles file termination
- **Status Tracking** - Provides detailed feedback during execution
- **Condition Names** - Uses 88-level items for clear logic flow

## Notes

- The program expects `test.dat` to be in the same directory as the executable
- Maximum line length is 100 characters
- The program uses `UPON CONSOLE` for explicit console output
- File status checking ensures robust error handling
- The program gracefully handles missing files and displays appropriate error messages

## Common Issues

- **File not found**: Ensure `test.dat` exists in the same directory
- **Permission errors**: Check file permissions if running on Unix/Linux systems
- **Path issues**: Use absolute paths if the file is in a different directory