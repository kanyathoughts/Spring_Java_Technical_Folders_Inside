       IDENTIFICATION DIVISION.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01  DAWS.
         05  DAWS-CALLED-PGM   PIC X(08)
                               VALUE SPACES.
                               01  WS-PGM.
        05 WS-SUBROUTINE              PIC X(08).
           88 WS-SUBPGM01             VALUE 'SUBPGM01'.
           88 WS-SUBPGM02             VALUE 'SUBPGM02'.
           88 WS-SUBPGM03             VALUE 'SUBPGM03'.
           88 WS-SUBPGM04             VALUE 'SUBPGM04'.
       PROCEDURE DIVISION.

           MOVE 'EIFRE858' TO DAWS-CALLED-PGM.
           CALL DAWS-CALLED-PGM USING ARCH-AREA
                                               EE858-AREA.

           SET WS-SUBPGM04 TO TRUE.

           MOVE 'EIFRE857' TO DAWS-CALLED-PGM.
           CALL DAWS-CALLED-PGM USING ARCH-AREA
                                               EE857-AREA.

           SET WS-SUBPGM01 TO TRUE.
           CALL WS-SUBROUTINE.

           SET WS-SUBPGM02 TO TRUE.
           MOVE 'CINRZERR' TO DAWS-CALLED-PGM.
           EXEC SQL
               INCLUDE EIFCDLGP
           END-EXEC.
