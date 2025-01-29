       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MAINPGM2.
      ******************************************************************
      *              ENVIRONMENT DIVISION.                             *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       WORKING-STORAGE SECTION.

       01  WS-PGM.
           05 WS-SUBROUTINE              PIC X(08).
              88 WS-SUBPGM01             VALUE 'SUBPGM01'.
              88 WS-SUBPGM02             VALUE 'SUBPGM02'.
              88 WS-SUBPGM03             VALUE 'SUBPGM03'.
           05 WS-SUBROUTINE-2            PIC X(08).
              88 WS-SUBPGM04             VALUE 'SUBPGM04'.
              88 WS-SUBPGM05             VALUE 'SUBPGM05'.

       PROCEDURE DIVISION.

           CALL WS-SUBROUTINE.
           
           STOP RUN.