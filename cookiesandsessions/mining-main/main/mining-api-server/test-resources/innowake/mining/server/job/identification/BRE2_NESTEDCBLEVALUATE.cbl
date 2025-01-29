       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRE2_NESTEDCBLEVALUATE.
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
           COMPUTE D ROUNDED = A + B.
           COMPUTE E ROUNDED = A + C.
      * Candidate: body contains another BRANCH_STATEMENT
           EVALUATE D
             WHEN 5 
                DISPLAY '1'
             WHEN OTHER
                EVALUATE E
                   WHEN 5 
                      DISPLAY '1'
                   WHEN OTHER
                      DISPLAY 'OTHER'
                END-EVALUATE.
           END-EVALUATE.
           
      * Candidate: body contains another BRANCH_STATEMENT
           IF E = '4'
              EVALUATE D
                   WHEN 5 
                      DISPLAY '1'
                   WHEN OTHER
                      DISPLAY 'OTHER'
                END-EVALUATE.
           ELSE
              DISPLAY '1'
           END-IF.
           
           PERFORM UNTIL D = 0
                EVALUATE E
                   WHEN 5
                      DISPLAY '1'
                   WHEN OTHER
                      DISPLAY 'OTHER'
                END-EVALUATE.
           STOP RUN.
