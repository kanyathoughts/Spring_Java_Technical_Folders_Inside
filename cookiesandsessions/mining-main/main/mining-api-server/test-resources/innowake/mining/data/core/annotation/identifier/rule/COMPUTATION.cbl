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
              SUBTRACT 3 FROM C
           END-IF.
           
      * Candidate: Branch statement: The if body contains an ARITHMETIC_STATEMENT
           IF TESTFIELD = '4'
              MULTIPLY C BY 5
           END-IF.
           
      * Candidate: Branch statement: The if body contains an ARITHMETIC_STATEMENT
	       IF TESTFIELD = '5'
	          DIVIDE C BY 2
	       END-IF.
           
	  END PROGRAM TEST.
