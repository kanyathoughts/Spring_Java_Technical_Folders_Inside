       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTFIELD PIC X(80).

       PROCEDURE DIVISION.

      * Candidate: body contains a LOOP CONDITION
           IF WS-NO-MORE-RECORDS < 2000
              PERFORM PROC0100-PROCESS THRU PROC0100-EXIT
                   UNTIL WS-NO-MORE-RECORDS = 'Y'.
           END-IF

       END PROGRAM TEST.