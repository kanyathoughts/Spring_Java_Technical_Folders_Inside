       IDENTIFICATION DIVISION.
       PROGRAM-ID. NDT3185.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        COPY ISSUE-MESSAGE REPLACING  
             MESSAGE-ARG BY WS-RECORD-KEY-STRING(1:). 
        COPY PACNUMV REPLACING DEFNMIN BY RECORD-SCORE IN NS DEFNMOUT 
             BY WS-RECORD-KEY-STRING .
       PROCEDURE DIVISION.
           DISPLAY "HELLO".
       STOP