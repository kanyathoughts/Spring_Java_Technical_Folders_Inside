       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN517A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
                 DISPLAY ' ----- A:'
            CALL 'PROG1'
                 DISPLAY ' ----- B:'
            CALL 'ENTRY1'
                 DISPLAY ' --- END --- '
       END PROGRAM MIN517A.