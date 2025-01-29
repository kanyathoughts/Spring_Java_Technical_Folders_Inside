       IDENTIFICATION DIVISION.
       PROGRAM-ID.                           UTDACTM2.
       AUTHOR.                 STEVE RICHTER TATA CONSULTANCY SERVICES.
       DATE-WRITTEN.                         11/03/14.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                      IBM-370.
       OBJECT-COMPUTER.                      IBM-370.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
      ************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA .
                 EXEC CICS SEND
                     MAP('UMACTVA')
                   MAPSET('UTACTM2')
                     FROM(UMACTVAO)
                   CURSOR
                 END-EXEC
            EXIT.

