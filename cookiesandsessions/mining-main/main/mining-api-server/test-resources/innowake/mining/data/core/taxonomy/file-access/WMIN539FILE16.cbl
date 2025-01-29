       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN539FILE1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

           EXEC CICS WRITE
             FILE('FILE')
             FROM(WS-FROM)
             RIDFLD(WS-RIDFLD)
           END-EXEC.

      
           GOBACK.
