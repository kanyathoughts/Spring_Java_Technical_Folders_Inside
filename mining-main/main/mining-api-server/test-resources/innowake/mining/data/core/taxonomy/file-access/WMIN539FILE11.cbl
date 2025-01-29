       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN539FILE1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

           EXEC CICS READPREV
             FILE('FILE')
             INTO(WS-FILE)
             RIDFLD(WS-RIDFLD)
           END-EXEC.

      
           GOBACK.
