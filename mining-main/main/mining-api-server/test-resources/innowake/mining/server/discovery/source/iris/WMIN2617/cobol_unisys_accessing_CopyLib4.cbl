       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobol_unisys_accessing_CopyLib4.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
         WORKING-STORAGE SECTION.
          REPLACE ==VAR3== BY ==D==.
          COPY S1000 OF CopyLib5000.
          REPLACE OFF.
          COPY S2000 OF CopyLib5000 REPLACING ==VAR1== BY ==A==.
       PROCEDURE DIVISION.
          DISPLAY 'Hello'.
       STOP RUN.