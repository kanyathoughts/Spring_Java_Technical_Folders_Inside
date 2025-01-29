       IDENTIFICATION DIVISION.
       PROGRAM-ID.     Evaluate1.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(9) VALUE 5.
       01 B PIC 9(9) VALUE 42.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM SECTION.

      * Candidate: two CONSTANT_REFERENCE
           EVALUATE A
             WHEN 5 OR 6
               DISPLAY '5'
           END-EVALUATE.

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
                END-EVALUATE.
           END-EVALUATE.
             
      * Candidate: body contains another BRANCH_STATEMENT
           EVALUATE A
             WHEN 5
               DISPLAY '5'
             WHEN 42
               EVALUATE A
                  WHEN 5
                     DISPLAY '5'
                  WHEN 42
                     DISPLAY '42'
               END-EVALUATE.
           END-EVALUATE.

      * Candidate: body contains another BRANCH_STATEMENT
           IF A = 1
              EVALUATE A
                 WHEN 5
                    DISPLAY '5'
                 WHEN 42
                    DISPLAY '42'
              END-EVALUATE.
           END-IF.

      * Candidate: body contains another BRANCH_STATEMENT
           EVALUATE A
              WHEN 5
                DISPLAY '5'
              WHEN 42
                IF A = 1
                  DISPLAY '1'
                END-IF
           END-EVALUATE.
           
      * No candidate: B > 2 ALSO B < 4 are two conditions -> no match 
      *               for at least two FIELD_REFERENCEs
           EVALUATE TRUE ALSO TRUE
             WHEN B > 2 ALSO B < 4
                DISPLAY 'WHAT'
           END-EVALUATE.

      * Candidate: body contains another BRANCH_STATEMENT
           EVALUATE TRUE ALSO TRUE
             WHEN B > 2 ALSO A < 4
               EVALUATE TRUE ALSO TRUE
                  WHEN B > 3 ALSO A < 3
                     DISPLAY 'WHAT 2'
                  END-EVALUATE.
           END-EVALUATE.

      * Candidate: two FIELD_REFERENCEs: 'A AND B'
           EVALUATE TRUE ALSO A
             WHEN B = 4
                DISPLAY 'WHAT'
             WHEN A AND B ALSO 42
                DISPLAY 'IS THIS'
           END-EVALUATE.

           EVALUATE TRUE ALSO A
             WHEN B = 4
                DISPLAY 'WHAT'
             WHEN A = 42
                DISPLAY 'IS THIS'
           END-EVALUATE.

           EVALUATE A
                 WHEN 5
                    DISPLAY '5'
                 WHEN 42
                    DISPLAY '42'
           END-EVALUATE.

      * Candidate: at least two CONSTANT_REFERENCEs: '0 THRU 5'
           EVALUATE A
              WHEN 0 THRU 5
                 DISPLAY "0-5"
              WHEN 6
                 DISPLAY "0"
              WHEN OTHER
                 DISPLAY "OTHER"
           END-EVALUATE

           GOBACK.