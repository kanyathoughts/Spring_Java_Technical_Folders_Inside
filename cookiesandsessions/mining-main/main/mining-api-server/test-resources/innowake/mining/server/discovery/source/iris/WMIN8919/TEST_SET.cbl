       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROGA.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAMEY PIC X(10) VALUE 'PROGY'.
       01  MY-PROGRAM-NAMEZ PIC X(10) VALUE 'PROGZ'.
       01  MY-VAR PIC X(10) VALUE 'PROGX'.
       PROCEDURE DIVISION.
           SET MY-PROGRAM-NAMEY TO 'PROGW'
           SET MY-PROGRAM-NAMEZ TO MY-PROGRAM-NAMEY
           SET MY-VAR TO MY-PROGRAM-NAMEZ
           CALL MY-VAR.
