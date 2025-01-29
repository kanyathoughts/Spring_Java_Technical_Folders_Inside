       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEXEXAMPLE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STD-DET
          OCCURS 6 TIMES
          INDEXED BY STD-INDEX.
       05 STD-MARKS              PIC 9(03).
       01 TOTAL-MARKS               PIC 9(03) VALUE ZERO.
       01 STD-PERCENT               PIC 9(03).9(02).
       01 I                         PIC 9(01).
       PROCEDURE DIVISION.
           MOVE ZEROES     TO TOTAL-MARKS.
           PERFORM VARYING I FROM 1 BY 1
             UNTIL I > 6
              SET STD-INDEX TO 1
              ACCEPT STD-MARKS (STD-INDEX)
              ADD STD-MARKS (STD-INDEX) TO TOTAL-MARKS
              SET STD-INDEX UP BY 1
            END-PERFORM.
            COMPUTE STD-PERCENT = TOTAL-MARKS/6
            DISPLAY 'STUDENT PERCENTAGE : 'STD-PERCENT.
            STOP RUN.