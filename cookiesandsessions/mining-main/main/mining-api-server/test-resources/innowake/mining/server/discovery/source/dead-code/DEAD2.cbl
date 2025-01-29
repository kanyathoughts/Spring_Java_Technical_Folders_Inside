       IDENTIFICATION DIVISION. 
       PROGRAM-ID. A. 
       ENVIRONMENT DIVISION. 
       DATA DIVISION.
       PROCEDURE DIVISION.
       L1.
           DISPLAY 'ALIVE'
           PERFORM LEND.
           PERFORM L5.
           DISPLAY 'ALIVE'
           GOBACK.
       L2.
           DISPLAY 'ALIVE'.
           PERFORM L5.
           DISPLAY 'ALIVE'
           GOBACK.
       L3.
           DISPLAY 'DEAD'
       L5.
           DISPLAY 'ALIVE'
           GOBACK.
       L4.
           DISPLAY 'DEAD'
           GOBACK.
       LEND.
           DISPLAY 'ALIVE'.
