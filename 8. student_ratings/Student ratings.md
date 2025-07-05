# Student Ratings in COBOL

This COBOL program collects student names and their grades, calculates the average for each student, and displays the results. It is useful for practicing loops, numeric computation, and user input handling in COBOL.

## Program Structure

The program is structured to:

- Loop through a predefined number of students (5 by default).
- Accept a name and three grades for each student.
- Compute the average grade.
- Display the student's name and their average grade.

## COBOL Source Code

```cobol
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
```

## Explanation of the Code

### IDENTIFICATION DIVISION

- `PROGRAM-ID. StudentRatings.`: Defines the program's name.

### ENVIRONMENT DIVISION and FILE SECTION

- Not used in this example, but included for structure.

### WORKING-STORAGE SECTION

- **Student-Counter**: Keeps track of the number of students processed.
- **Total-Students**: Fixed at 5, can be adjusted.
- **Student-Name**: Stores the name of each student (up to 20 characters).
- **Grade1, Grade2, Grade3**: Hold three numeric grades (0–9).
- **Avg-Grade**: Stores the calculated average with one decimal.

### PROCEDURE DIVISION

- Loops until all students are processed.
- Uses `DISPLAY` and `ACCEPT` to interact with the user.
- `COMPUTE` is used to calculate the average.
- Each student's result is printed with name and average grade.

## How to Use

1. Compile the COBOL program using:

```bash
cobc -x student_ratings.cob
```

2. Run the executable:

```bash
./student_ratings
```

3. Follow the prompts to input names and grades for 5 students.

## Concepts Covered

This program demonstrates:

- **Looping**: Using `PERFORM UNTIL` for iteration.
- **Input/Output**: `DISPLAY` and `ACCEPT` for interaction.
- **Numeric Calculation**: Using `COMPUTE` for averages.
- **Basic Data Types**: Including alphanumeric and numeric formats.

This is a solid foundational exercise in COBOL for working with structured data and logic. It's also a great way to practice data validation and aggregation.
