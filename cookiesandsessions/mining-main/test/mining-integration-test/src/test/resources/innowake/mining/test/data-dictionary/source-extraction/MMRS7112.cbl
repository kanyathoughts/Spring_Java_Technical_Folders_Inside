
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7112.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
      * declare section
             EXEC SQL BEGIN DECLARE SECTION END-EXEC.
      *    ************************************************************
      *    USE COPYBOOKS MMRS710A
           COPY MMRS710A.
      *    ************************************************************
       01  MY-COUNTER.
           10 MYSQLIN-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYSQLIN-DISPLAY     PIC Z(3).Z(3).ZZ9.
   

    
