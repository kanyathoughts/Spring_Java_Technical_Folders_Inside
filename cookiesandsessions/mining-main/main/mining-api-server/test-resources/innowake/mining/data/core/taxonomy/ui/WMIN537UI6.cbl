       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN537UI6.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

      * RECEIVE MAP
           EXEC CICS RECEIVE
                MAP ('UTMHELP')
                MAPSET ('AI01HLM')
                INTO (UTMHELPI)
           END-EXEC

           GOBACK.
