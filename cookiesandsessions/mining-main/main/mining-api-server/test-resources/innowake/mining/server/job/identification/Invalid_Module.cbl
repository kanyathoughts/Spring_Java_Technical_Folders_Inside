       IDENTIFICATION DIVISION.
       PROGRAM-ID INVALID.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(3) VALUE 123
       01 NUM2 PIC 9(3) VALUE 456.
       01 RESULT PIC 9(3).

       PROCEDURE DIVISION.
            DISPLAY 'This is an invalid COBOL program.'.
            COMPUTE RESULT = NUM1 + NUM2

            EXEC CICS WRITE JOURNALNAME(A-JOURNAL)
                     ID(1)
                     JTYPEID('JU')
                     PREFIX(AP-ID)
                     FROM(REC)
                     RESP(WS-RESP)
           END-EXEC.
            STOP RUN

