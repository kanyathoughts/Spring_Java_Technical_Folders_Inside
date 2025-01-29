       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSPGM.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
           EXEC CICS SEND  MAP ('MAP')
                           MAPSET ('MAPSET')
                     FROM(WS-OUTPUT)
                     ERASE
           END-EXEC.
           GOBACK.
