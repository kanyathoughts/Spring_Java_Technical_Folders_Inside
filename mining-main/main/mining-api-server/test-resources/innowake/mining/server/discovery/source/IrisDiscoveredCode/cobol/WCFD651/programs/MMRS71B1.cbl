****************************************************************************
******** MMRS-M01  adapted
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MMRS71B1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
       01  DFHCOMMAREA            PIC X(250).
       PROCEDURE DIVISION.

       CHECK-WHAT-TO-DO SECTION.

           EVALUATE EIBAID
            WHEN   DFHPF3
             MOVE -1 TO CICS-RESP
             MOVE MMRS-FEEDBACK-END  TO INFOTXTO
            WHEN   DFHPF4
             PERFORM PROCESS-WORK-STOR-SUBPGM
            WHEN   DFHPF5
             PERFORM PROCESS-XCTL
            WHEN   DFHPF8
             PERFORM PROCESS-VSAM-READ
            WHEN   DFHENTER
             PERFORM PROCESS-SCREEN-DATA
            WHEN OTHER
             MOVE UNKNOWN-PFKEY TO INFOTXTO
           END-EVALUATE
           .

      * SEND SCREE DATA -----------------------------------------
       SEND-SCREEN-DATA SECTION.
           EVALUATE CICS-RESP
             WHEN DFHRESP(NORMAL)
                CONTINUE
             WHEN OTHER
                MOVE -1 TO CICS-RESP
                MOVE MMRS-FEEDBACK-NOK  TO INFOTXTO
           END-EVALUATE
           .

      * RECEIVE SCREE DATA --------------------------------------
       RECEIVE-SCREEN-DATA SECTION.
           EVALUATE CICS-RESP
             WHEN DFHRESP(NORMAL)
                CALL MMRS71Z1 USING MMRS71Z1-LEN USERIDI ACTUAL02O
             WHEN DFHRESP(MAPFAIL)
                MOVE 0 TO CICS-RESP
                MOVE MMRS-ASK-FOR-LOGIN TO INFOTXTO
             WHEN OTHER
                MOVE -1 TO CICS-RESP
                MOVE MMRS-FEEDBACK-NOK  TO INFOTXTO
           END-EVALUATE
           .

       END PROGRAM MMRS71B1.
