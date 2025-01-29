       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRE2_NESTEDIFELSE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GP-1.
          02 A PIC 9(2).
          02 B PIC 9(2).
          02 C PIC 9(2).
          02 D PIC 9(2).
          02 E PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 15 TO B.
           DISPLAY " C IS NOW: " C.
           COMPUTE E ROUNDED = A + C.
      * CANDIDATE: BODY CONTAINS ANOTHER BRANCH_STATEMENT
           IF D = '4'
              IF E = '3'
                 DISPLAY '3'
              ELSE
                 DISPLAY '2'
              END-IF.

           PERFORM UNTIL E = 0
              IF D = '4'
                  DISPLAY '4'
              ELSE
                  DISPLAY '5'
              END-IF.
           STOP RUN.
