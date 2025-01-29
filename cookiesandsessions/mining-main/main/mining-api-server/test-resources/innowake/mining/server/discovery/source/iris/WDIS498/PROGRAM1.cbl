       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROGRAM1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY CPY1.

       PROCEDURE DIVISION.

           MOVE 'EIFSP999' TO DAWS-INTERFACE-PGM.
           MOVE 'EIPRE002' TO DAWS-CALLED-PGM.

           CALL DAWS-INTERFACE-PGM USING DAWS-CALLED-PGM
                                         ARC2-AREA
                                         PE002-AREA.

