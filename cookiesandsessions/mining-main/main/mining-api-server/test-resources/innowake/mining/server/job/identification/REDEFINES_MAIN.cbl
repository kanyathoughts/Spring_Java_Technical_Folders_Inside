       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROGRAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MESSAGE PIC X(50) VALUE SPACES.
       01  WS-RECORD.
           05  WS-ID       PIC X(5).
           05  WS-NAME     PIC X(30).
           05  WS-SALARY   PIC 9(6)V99.
       01  MAN-SALARY REDEFINES
           WS-SALARY PIC 9(6)V99.
       01  MAN-DEPT  REDEFINES
           WS-DEPT PIC X(3).

       COPY REDEFINED.

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "This is the main program.".
           DISPLAY "Processing EMPLOYEE record...".
           MOVE SPACES TO EMPLOYEE-RECORD.
           MOVE '12345' TO EMPLOYEE-ID.
           MOVE 'John Doe' TO EMPLOYEE-NAME.
           DISPLAY "Employee ID: " EMPLOYEE-ID.
           DISPLAY "Employee Name: " EMPLOYEE-NAME.
           DISPLAY "Processing MANAGER record...".
           MOVE SPACES TO MANAGER-RECORD.
           MOVE '67890' TO MANAGER-ID.
           MOVE 'Jane Smith' TO MANAGER-NAME.
           DISPLAY "Manager ID: " MANAGER-ID.
           DISPLAY "Manager Name: " MANAGER-NAME.
           DISPLAY "UNKNOWN DEPT: " MAN-DEPT
       STOP RUN.
