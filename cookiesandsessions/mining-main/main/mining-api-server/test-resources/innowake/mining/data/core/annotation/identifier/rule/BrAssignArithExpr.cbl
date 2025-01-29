       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTFIELD X(80).
       01 A PIC 9(9) VALUES(5).
       01 B PIC 9(9) VALUES(42).
       01 C PIC 9(9) VALUES(42).
       01 DATE-FIELD PIC 9(6) VALUE ZEROES.

       PROCEDURE DIVISION.

      * Candidate: Branch statement: The if body contains an ARITHMETIC_STATEMENT
           IF TESTFIELD = '1'
              COMPUTE C = A + B
           END-IF.

      * Candidate: Branch statement: The if body contains an ARITHMETIC_STATEMENT
           IF TESTFIELD = '2'
              ADD A TO B
           END-IF.

      * Candidate: Branch statement: The if body contains an ARITHMETIC_STATEMENT
           IF TESTFIELD = '3'
              ADD A TO B GIVING C
           END-IF.

      * Candidate: Branch statement: The else body contains an ARITHMETIC_STATEMENT
           IF TESTFIELD = '4'
              DISPLAY 'if'
           ELSE
              COMPUTE C = A + B
           END-IF.

      * Candidate: Branch statement: The if body contains an ARITHMETIC_STATEMENT
           IF TESTFIELD = '5'
              COMPUTE A = FUNCTION LENGTH(TESTFIELD)
           END-IF.

      * No Candidate: ASSIGNMENT_STATEMENT without an ARITHMETIC_EXPRESSION
           IF TESTFIELD = '6'
              ACCEPT DATE-FIELD FROM DATE
           END-IF.

      * No Candidate: ASSIGNMENT_STATEMENT without an ARITHMETIC_EXPRESSION
           IF TESTFIELD = '7'
              MOVE A TO B
           END-IF.
           
      * Candidate Branch statement: The body contains an ARITHMETIC_STATEMENT
           IF TESTFIELD = '8'
              MOVE A + B TO C
           END-IF.
           
           COMPUTE C = A + B.
           ADD A TO B.
           ADD A TO B GIVING C.
           COMPUTE A = FUNCTION LENGTH(TESTFIELD).
           ACCEPT DATE-FIELD FROM DATE.
           MOVE A + B TO C.

       END PROGRAM TEST.
