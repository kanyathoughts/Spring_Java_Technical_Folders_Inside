       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN537UI3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

      * SEND MAP
           EXEC CICS SEND
               MAP    (WS-CURRENT-MAPNAME)
               MAPSET (WS-CURRENT-MAPSET)
               FROM   (UTMHELPO)
               LENGTH (LENGTH OF UTMHELPO)
               ERASE
           END-EXEC

           GOBACK.
