       IDENTIFICATION DIVISION.
       PROGRAM-ID. START02.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.

       01 G1-F1 PIC X.
       01 G1-F2 PIC X.

       PROCEDURE DIVISION USING G1-F1.

           MOVE G1-F1 TO G1-F2
           
       END PROGRAM START02.
