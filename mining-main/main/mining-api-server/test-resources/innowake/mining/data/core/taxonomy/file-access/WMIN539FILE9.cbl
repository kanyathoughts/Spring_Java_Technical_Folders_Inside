       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN539FILE1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

           EXEC CICS READ
             FILE('FILE')
             INTO(WS-FILE)
             RIDFLD(WS-RIDFLD)
           END-EXEC.

      
           GOBACK.
