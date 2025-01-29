       IDENTIFICATION DIVISION. 
       PROGRAM-ID. A. 
       ENVIRONMENT DIVISION. 
       DATA DIVISION.
       PROCEDURE DIVISION.
       L1.
           DISPLAY 'ALIVE'.
           PERFORM L2.
           GOBACK.
       L3.
           DISPLAY 'DEAD'.
           DISPLAY 'DEAD2'.
       L2.
           DISPLAY 'ALIVE'.
           GOBACK.
           DISPLAY 'DEAD'.
