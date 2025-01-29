       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobol_unisys_accessing_CopyLib2.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
         WORKING-STORAGE SECTION.
          REPLACE ==VAR3== BY ==D==.
          COPY SAMPLE1 OF CopyLib1000.
          REPLACE OFF.
          COPY SAMPLE2 OF CopyLib1000 REPLACING ==VAR1== BY ==A==.
       PROCEDURE DIVISION.
          DISPLAY 'Hello'.
       STOP RUN.