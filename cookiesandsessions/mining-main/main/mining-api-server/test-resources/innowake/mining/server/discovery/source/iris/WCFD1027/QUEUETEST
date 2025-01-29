       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUEUETEST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-DATA.
           05  TS-QUEUE-ONE                   PIC X(6) VALUE 'TSQN'.
           05  TD-QUEUE-ONE                   PIC X(6) VALUE 'TDQN'.
           05  TD-QUEUE-WRITE                 PIC X(6) VALUE 'TDQW'.
           05  TS-QUEUE-READ                  PIC X(6) VALUE 'TSQR'.
           05  TS-QUEUE-DEL                   PIC X(6) VALUE 'TSQD'.
           05  QUEUE-RECORD-NUMBER            PIC 9(05) VALUE 1.
           05  RESP-CODE                      PIC 9(05).
           05  TS-QUEUE-NO-NAME               PIC X(8).
           05  TD-QUEUE-NO-NAME               PIC X(8).
           05  TS-QUEUE-RECORD-XFER.
               10  TS-RECORD-XFER            PIC X(02).
           05  TD-QUEUE-RECORD-XFER.
               10  TD-RECORD-XFER            PIC X(02).

       PROCEDURE DIVISION.
      * TS Queue with a defined name
         FIRST-PARA.
           EXEC CICS
               WRITEQ TS
                   QUEUE(TS-QUEUE-ONE)
                   FROM(TS-QUEUE-RECORD-XFER)
                   ITEM(QUEUE-RECORD-NUMBER)
                   RESP(RESP-CODE)
           END-EXEC.
           EXEC CICS
               READQ TS
                  QUEUE(TS-QUEUE-ONE)
                  INTO(TS-QUEUE-RECORD-XFER)
                  ITEM(QUEUE-RECORD-NUMBER)
                  RESP(RESP-CODE)
              END-EXEC.
           EXEC CICS DELETEQ TS
               QUEUE(TS-QUEUE-ONE)
           END-EXEC.
           PERFORM SECOND-PARA

      * TS Queue without a defined name
         SECOND-PARA.
             EXEC CICS
               WRITEQ TS
                   QUEUE(TS-QUEUE-NO-NAME)
                   FROM(TS-QUEUE-RECORD-XFER)
                   ITEM(QUEUE-RECORD-NUMBER)
                   RESP(RESP-CODE)
           END-EXEC.
           EXEC CICS
               READQ TS
                  QUEUE(TS-QUEUE-NO-NAME)
                  INTO(TS-QUEUE-RECORD-XFER)
                  ITEM(QUEUE-RECORD-NUMBER)
                  RESP(RESP-CODE)
           END-EXEC.
           EXEC CICS DELETEQ TS
               QUEUE(TS-QUEUE-NO-NAME)
           END-EXEC.
           PERFORM THIRD-PARA

      * TD Queue with a defined name
         THIRD-PARA.
           EXEC CICS
             WRITEQ TD
               QUEUE(TD-QUEUE-ONE)
               FROM(TD-QUEUE-RECORD-XFER)
               LENGTH(80)
           END-EXEC.
           EXEC CICS
               READQ TD
                  QUEUE(TD-QUEUE-ONE)
                  INTO(TD-QUEUE-RECORD-XFER)
           END-EXEC
           EXEC CICS DELETEQ TD
               QUEUE(TD-QUEUE-ONE)
           END-EXEC.
           PERFORM FOURTH-PARA

      * TD Queue without a defined name
         FOURTH-PARA.
           EXEC CICS
             WRITEQ TD
               QUEUE(TD-QUEUE-NO-NAME)
               FROM(TD-QUEUE-RECORD-XFER)
               LENGTH(80)
           END-EXEC.
           EXEC CICS
               READQ TD
                  QUEUE(TD-QUEUE-NO-NAME)
                  INTO(TD-QUEUE-RECORD-XFER)
           END-EXEC.
           EXEC CICS DELETEQ TD
               QUEUE(TD-QUEUE-NO-NAME)
           END-EXEC.
           PERFORM FIFTH-PARA

      * TD Queue only with write
         FIFTH-PARA.
           EXEC CICS
             WRITEQ TD
               QUEUE(TD-QUEUE-WRITE)
               FROM(TD-QUEUE-RECORD-XFER)
               LENGTH(80)
           END-EXEC.
           PERFORM SIXTH-PARA

      * TD Queue without a defined name
         SIXTH-PARA.
           EXEC CICS
               READQ TS
                  QUEUE(TS-QUEUE-READ)
                  INTO(TS-QUEUE-RECORD-XFER)
                  ITEM(QUEUE-RECORD-NUMBER)
                  RESP(RESP-CODE)
           END-EXEC.
           PERFORM SEVTH-PARA

      * TD Queue without a defined name
         SEVTH-PARA.
           EXEC CICS DELETEQ TD
               QUEUE(TS-QUEUE-DEL)
           END-EXEC.
           EXIT.
