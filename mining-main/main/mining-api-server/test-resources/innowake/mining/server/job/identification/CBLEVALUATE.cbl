       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CBLEVALUATE.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(9) VALUE 5.
       01 B PIC 9(9) VALUE 42.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM SECTION.

      * Candidate: body contains another BRANCH_STATEMENT
           EVALUATE TRUE
             WHEN A 
                DISPLAY '1'
             WHEN OTHER
                EVALUATE TRUE
                   WHEN A 
                      DISPLAY '1'
                   WHEN OTHER
                      DISPLAY 'OTHER'
                END-EVALUATE
           END-EVALUATE.

      * Candidate: body contains another BRANCH_STATEMENT
           IF A = 1
              EVALUATE A
                 WHEN 5
                    DISPLAY '5'
                 WHEN 42
                    DISPLAY '42'
              END-EVALUATE
           END-IF.
           
      * No candidate: B > 2 ALSO B < 4 are two conditions -> no match 
      *               for at least two FIELD_REFERENCEs
           EVALUATE TRUE ALSO TRUE
             WHEN B > 2 ALSO B < 4
                DISPLAY 'WHAT'
           END-EVALUATE.

           GOBACK.