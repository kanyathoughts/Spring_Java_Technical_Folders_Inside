       IDENTIFICATION DIVISION. 
       PROGRAM-ID. A. 
       ENVIRONMENT DIVISION. 
       DATA DIVISION.
       PROCEDURE DIVISION.
       L1.
           DISPLAY '1'
           PERFORM LEND.
           COPY CC1
           DISPLAY '2'
           GOBACK.
       LEND.
           DISPLAY 'END'.
