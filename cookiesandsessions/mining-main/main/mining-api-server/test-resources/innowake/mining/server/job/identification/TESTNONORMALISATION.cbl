       
**********************************************************************
*******  innoWake new cobol program structure
**********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRG1.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       FOO.
       01MY-ADD-TARGETPIC9(5)V9(2)VALUEZERO.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
           DISPLAY FOO.
           GOBACK.
      *    ************************************************************
