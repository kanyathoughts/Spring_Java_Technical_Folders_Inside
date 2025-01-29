       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL_UNISYS_ACCESSING_COPYLIB3.
      ******************************************************************
      * DCLGEN TABLE(TINVOIC)                                          *
      ******************************************************************
       ENVIRONMENT DIVISION.

       DATA DIVISION.
         WORKING-STORAGE SECTION.
          REPLACE ==VAR3== BY ==D==.
          COPY AA OF COBOL_UNISYS_COPYLIB.
          REPLACE OFF.
          COPY BB OF COBOL_UNISYS_COPYLIB REPLACING ==VAR1== BY ==A==.
       PROCEDURE DIVISION.
          DISPLAY 'Hello'.
       STOP RUN.
