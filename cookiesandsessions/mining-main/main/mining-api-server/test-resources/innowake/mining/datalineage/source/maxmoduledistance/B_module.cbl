       IDENTIFICATION DIVISION.
       PROGRAM-ID. WC47PGM8.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
        01 DUMMY1 PIC X(4).

       PROCEDURE DIVISION USING DUMMY1.

         CALL "TEST42c" USING DUMMY1.

       END PROGRAM WC47PGM8.
