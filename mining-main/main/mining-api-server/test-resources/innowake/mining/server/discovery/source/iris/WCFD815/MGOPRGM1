       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOPRGM1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  REC.

       PROCEDURE DIVISION.
              EXEC CICS CREATE TRANSACTION ('NZNL')
                               ATTRIBUTES ('UCTRAN (NO)RTIMEOUT (10 )')
      * W01-ATTR
                               ATTRLEN(LENGTH OF W01-ATTR)
                               RESP   (W30-RESP)
                   END-EXEC
            STOP RUN.
