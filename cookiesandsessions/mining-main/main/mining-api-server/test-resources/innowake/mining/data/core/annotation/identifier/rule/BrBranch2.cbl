       IDENTIFICATION DIVISION.
       PROGRAM-ID.     Evaluate2.
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
                COPY BrBranch2.
           END-EVALUATE.

      * Candidate: body contains another BRANCH_STATEMENT
           EVALUATE TRUE
             WHEN A 
                DISPLAY '1'
             WHEN OTHER
                COPY BrBranch2.
           END-EVALUATE.

           GOBACK.