       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRE2_LOOP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTFIELD PIC X(80).
       02 A PIC 9(2).
       02 B PIC 9(2).

       PROCEDURE DIVISION.

      COMPUTE TESTFIELD ROUNDED = A + B.
      * Candidate: body contains a LOOP CONDITION
            PERFORM VARYING TESTFIELD
             FROM   1
             BY     1
             UNTIL  TESTFIELD > 60000
              MOVE A TO B
            END-PERFORM
       END PROGRAM TEST.
