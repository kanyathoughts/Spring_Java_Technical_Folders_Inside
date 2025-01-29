       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRE2_CBLEVALUATE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GP-1.
          02 A PIC 9(2).
          02 B PIC 9(2).
          02 C PIC 9(2).
          02 D PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 15 TO B.
           DISPLAY " C IS NOW: " C.
           COMPUTE D ROUNDED = A + B.
      * Candidate: body contains another BRANCH_STATEMENT
           EVALUATE B
             WHEN 3 
                DISPLAY '1'
             WHEN OTHER
                EVALUATE D
                   WHEN 5 
                      DISPLAY '500'
                   WHEN OTHER
                      DISPLAY 'OTHER'
                END-EVALUATE.
           END-EVALUATE.
           STOP RUN.
