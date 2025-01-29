       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN517C.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
                 DISPLAY ' ----- A:'
                 MOVE 1 TO TESTFIELD
                 IF TESTFIELD EQUALS 1
                   EXEC CICS RETURN END-EXEC
                 ELSE
                   EXEC CICS RETURN TRANSID('TRA1') END-EXEC
                 END-IF.
                 DISPLAY ' --- UNREACHABLE --- '
       END PROGRAM MIN517C.