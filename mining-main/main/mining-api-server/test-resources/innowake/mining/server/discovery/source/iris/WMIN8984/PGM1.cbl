
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PGM1.

       DATA            DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-PGM1 PIC 9(8) VALUE 'CPY2'.
       COPY CPY3.

       LINKAGE SECTION.
        01 DFHCOMMAREA          PIC 9(4).

       PROCEDURE DIVISION.

           CALL 'CPY1'.
           CALL WS-PGM1.
           CALL 'CPY3'.