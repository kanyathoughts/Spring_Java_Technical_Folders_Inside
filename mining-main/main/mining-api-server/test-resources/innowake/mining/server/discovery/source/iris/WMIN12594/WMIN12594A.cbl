       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN12607A.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 WS-VSAM-TABLE.
          02 WS-VSAM-TABLE-KEY.
             03 WS-TABLE-NAME PIC X(8).
             03 WS-TABLE-TYPE PIC X(1).
             03 WS-ENCODE-VALUE PIC X(30).
          02 WS-DECODE-VALUE PIC X(80).
          02 FILLER          PIC X(14).
       LINKAGE SECTION.

       PROCEDURE DIVISION.

           EXEC CICS WRITE JOURNALNAME(A-JOURNAL)
                     JTYPEID('JU')
                     PREFIX(AP-ID)
                     FROM(REC)
                     RESP(WS-RESP)
           END-EXEC.
           
           GOBACK.
