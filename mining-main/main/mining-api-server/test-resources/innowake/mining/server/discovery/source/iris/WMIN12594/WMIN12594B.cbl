       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN12607B.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.

       PROCEDURE DIVISION.

           EXEC CICS WRITE
                     JOURNALNAME('ACCTSJNL')
                     JTYPEID('XX')
                     FROM(KEYDATA)
                     FLENGTH(40000)
                     REQID(ENT)
                     PREFIX(PROGNAME)
                     PFXLENG(6)
                     WAIT
                     NOSUSPEND
                     RESP(WS-RESP)
           END-EXEC.

           GOBACK.
