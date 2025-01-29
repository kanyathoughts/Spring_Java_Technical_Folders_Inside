       9300-LOG-ERROR-DATA.
            CALL WS-SUBROUTINE.
            CALL DAWS-CALLED-PGM USING ALNK-AREA
                                       AERR-AREA
                                       EZ007-AREA.

           SET WS-SUBPGM03 TO TRUE.
           CALL WS-SUBROUTINE.
