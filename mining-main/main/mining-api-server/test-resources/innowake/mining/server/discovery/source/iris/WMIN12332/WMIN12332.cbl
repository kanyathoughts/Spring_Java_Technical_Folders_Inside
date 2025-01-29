
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     WMIN12332.

       DATA            DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
        01 DFHCOMMAREA          PIC 9(4).

       PROCEDURE DIVISION.

           MOVE 'EIFSP999'         TO DAWS-INT-PGM

           CALL DAWS-INT-PGM USING DAWS-CALLED-PGM
                                   ARC2-AREA
                                   CZ14Z-AREA.
