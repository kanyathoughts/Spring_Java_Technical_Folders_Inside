       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROGA.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAMEA PIC X(10) VALUE 'PROGB'.
       01  MY-PROGRAM-NAMEB PIC X(10) VALUE 'PROGC'.
       01  MY-VAR PIC X(10) VALUE 'PROGA'.
       PROCEDURE DIVISION.
           MOVE 'PROGD' TO MY-PROGRAM-NAMEA.
           MOVE MY-PROGRAM-NAMEA TO MY-PROGRAM-NAMEB.
           MOVE MY-PROGRAM-NAMEB TO MY-VAR.
           CALL MY-VAR.