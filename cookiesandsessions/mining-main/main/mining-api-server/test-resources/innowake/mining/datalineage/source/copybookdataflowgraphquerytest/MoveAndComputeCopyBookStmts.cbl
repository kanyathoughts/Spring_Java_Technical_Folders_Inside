       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILDSIB2.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

        01 WS-ONE PIC 9 VALUE 1.
           COPY MoveAndComputeCopyBook.

       PROCEDURE DIVISION.
           MOVE WS-ONE TO WS-INPUT.
           COMPUTE WS-OUTPUT = WS-INPUT * 2.

       END PROGRAM FILDSIB2.
