       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN539FILE1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

           EXEC CICS REWRITE
             FILE('FILE')
             FROM(WS-FROM)
           END-EXEC.

      
           GOBACK.
