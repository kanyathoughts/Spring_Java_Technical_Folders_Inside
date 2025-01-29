       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN537UI5.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

      * SEND TEXT
           EXEC CICS SEND TEXT
               FROM (WS-MSG-OUT)
               LENGTH(9)
               CURSOR(9)
              ERASE
           END-EXEC

           GOBACK.
