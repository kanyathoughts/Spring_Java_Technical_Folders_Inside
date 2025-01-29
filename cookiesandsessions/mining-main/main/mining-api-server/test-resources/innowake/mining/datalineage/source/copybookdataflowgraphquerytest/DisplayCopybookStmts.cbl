       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMCPY01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY Copybook.
       
       01  D.
          02  E  PIC X(2).
          02  F  PIC X(3).
       

       PROCEDURE DIVISION.

         DISPLAY A D.
         DISPLAY B E.
         DISPLAY C F.

       END PROGRAM PGMCPY01.
