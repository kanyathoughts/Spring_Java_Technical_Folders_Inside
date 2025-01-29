       IDENTIFICATION DIVISION.
       PROGRAM-ID. IF-ELSE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 A PIC 9(4).
       77 B PIC 9(4).
       77 C PIC 9(4).
       PROCEDURE DIVISION.
       IF  A NOT NUMERIC
           OR  B = '9'
               MOVE B TO C
               MOVE 0 TO B
           ELSE
               MOVE '0000' TO C
               MOVE A TO B
           END-IF
       STOP RUN.
