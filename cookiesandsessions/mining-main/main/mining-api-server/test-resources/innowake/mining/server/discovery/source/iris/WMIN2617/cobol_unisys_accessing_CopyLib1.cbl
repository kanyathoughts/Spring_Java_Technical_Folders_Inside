       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobol_unisys_accessing_CopyLib1.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
         WORKING-STORAGE SECTION.
          REPLACE ==VAR3== BY ==D==.
          COPY SAMPLE1 OF cobol_unisys_CopyLib.
          REPLACE OFF.
       PROCEDURE DIVISION.
          COPY SAMPLE2 OF cobol_unisys_CopyLib REPLACING ==VAR1== BY ==A==.
          DISPLAY 'Hello'.
       STOP RUN.