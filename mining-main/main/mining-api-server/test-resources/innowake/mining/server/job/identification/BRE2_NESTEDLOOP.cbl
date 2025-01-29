       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTFIELD PIC X(80).
       02 A PIC 9(2).
       02 B PIC 9(2).
       02 C PIC 9(2).
       02 D PIC 9(2).

       PROCEDURE DIVISION.


      COMPUTE C ROUNDED = A + B.
      COMPUTE D ROUNDED = A + B.
      * Candidate: body contains a LOOP CONDITION
           IF C < 2000
              PERFORM PROC0100-PROCESS THRU PROC0100-EXIT
                   UNTIL D = 'Y'.
           END-IF
           
           EVALUATE C
             WHEN 5
                DISPLAY '1'
             WHEN OTHER
                PERFORM PROC0100-PROCESS THRU PROC0100-EXIT
                   UNTIL D = 'Y'.
           END-EVALUATE.
           
           PERFORM UNTIL C = 0
              PERFORM UNTIL D = C
                  DISPLAY 'Y'
              END-PERFORM.
           END-PERFORM.
       END PROGRAM TEST.