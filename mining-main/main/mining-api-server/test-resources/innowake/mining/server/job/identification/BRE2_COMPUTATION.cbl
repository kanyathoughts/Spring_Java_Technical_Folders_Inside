       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BRE2_COMPUTATION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTFIELD PIC X(80).
       01 A PIC 9(9) VALUES(5).
       01 B PIC 9(9) VALUES(42).
       01 C PIC 9(9) VALUES(42).
       01 DATE-FIELD PIC 9(6) VALUE ZEROES.

       PROCEDURE DIVISION.
            WRITE 'answer' TESTFIELD
      * Candidate: Branch statement: The if body contains an ARITHMETIC_STATEMENT
           IF TESTFIELD = '1'
              COMPUTE C = A + B
              ADD 1 TO C
           END-IF.
       END PROGRAM TEST.
