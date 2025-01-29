       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEREADER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE ASSIGN TO INFILE.

       DATA DIVISION.
       FILE SECTION.
       FD INPUTFILE.
       01 INPUTRECORD.
           05 FIELD1       PIC X(10).
           05 FIELD2       PIC X(20).
           05 FIELD3       PIC 9(5).

       WORKING-STORAGE SECTION.
       01 ENDOFFILE       PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.

       OPEN INPUT INPUTFILE

       PERFORM UNTIL ENDOFFILE = 'Y'
          READ INPUTFILE
              AT END
                  MOVE 'Y' TO ENDOFFILE
              NOT AT END
                  DISPLAY "Field1: " FIELD1
                  DISPLAY "Field2: " FIELD2
                  DISPLAY "Field3: " FIELD3
          END-READ
       END-PERFORM

          CLOSE INPUTFILE

          STOP RUN.
