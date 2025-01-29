       PROGRAM-ID. MMRS71C1.
       77  MY-PROGRAM-NAME        PIC X(8) VALUE 'MMRS71C1'.
       PROCEDURE DIVISION.
           IF MMRS-CLEAR-YES
             PERFORM RECEIVE-SCREEN-DATA
             IF CICS-RESP = DFHRESP(NORMAL)
               PERFORM CHECK-WHAT-TO-DO
             END-IF
           END-IF
           IF CICS-RESP = DFHRESP(NORMAL)
             PERFORM SEND-SCREEN-DATA
           ELSE
             PERFORM SEND-END-MESSAGE
           END-IF
           .
       PROCESS-SCREEN-DATA SECTION.
           .
       CHECK-WHAT-TO-DO SECTION.
           .
       SEND-SCREEN-DATA SECTION.
           PERFORM MMRS-COMMON-INIT-PROCESSING
           .
       SEND-END-MESSAGE SECTION.
           .
       RECEIVE-SCREEN-DATA SECTION.
           .
       SEND-END-MESSAGE SECTION.
           .
       END PROGRAM MMRS71C1.
