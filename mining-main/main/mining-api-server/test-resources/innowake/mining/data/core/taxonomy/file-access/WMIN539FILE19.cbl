       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN539FILE1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

           EXEC CICS READNEXT
             FILE('FILE')
             INTO(WS-FILE)
             RIDFLD(WS-RIDFLD)
           END-EXEC.

           EXEC CICS WRITE
             FILE('FILE')
             FROM(WS-FROM)
             RIDFLD(WS-RIDFLD)
           END-EXEC.

           EXEC CICS DELETE
             FILE('FILE')
           END-EXEC.

      
           GOBACK.
