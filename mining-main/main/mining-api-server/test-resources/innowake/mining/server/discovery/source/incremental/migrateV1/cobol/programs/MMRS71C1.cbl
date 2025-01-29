****************************************************************************
******** MMRS-M01  mainframe modernization reference system
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MMRS71C1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  MY-PROGRAM-NAME        PIC X(8) VALUE 'MMRS71C1'.
****************************************************************************
******** BMS Dialog interface
****************************************************************************
           COPY MMRS71A.
           COPY MMRS71C.
****************************************************************************
******** common used fields
****************************************************************************
       77  MMRS71Z1               PIC X(8) VALUE 'MMRS71Z1'.
       77  MMRS71Z1-LEN           PIC 9(5) VALUE 00030.
       77  MY-TEXT                PIC X(20) VALUE 'DEF WS VALUE'.
       77  CICS-RESP              PIC S9(8) COMP.
       77  CICS-TC                PIC X(4) VALUE 'MM7C'.
           COPY MMRS710D.
           COPY DFHAID.
****************************************************************************
******** specific fields
****************************************************************************
       01  HEX-DISP.
           10  HEX-DISP-BMS-1     PIC X(30) VALUE SPACES.

****************************************************************************
******** Interface data to this cobol program
****************************************************************************
       LINKAGE SECTION.
       01  DFHCOMMAREA            PIC X(250).

****************************************************************************
******** process
****************************************************************************
       PROCEDURE DIVISION.
      * do initial processing --------------------------------------
           MOVE LOW-VALUE TO MMRS71AI
           MOVE LOW-VALUE TO MMRS71CI
           MOVE 0                          TO CICS-RESP
           IF EIBCALEN > 0
             MOVE DFHCOMMAREA              TO MMRS-COMMAREA
           END-IF

      * get screen data --------------------------------------
           IF MMRS-CLEAR-YES
             MOVE 'type in your requirement'  TO INFOTXTO
           ELSE
             PERFORM RECEIVE-SCREEN-DATA
             IF CICS-RESP = DFHRESP(NORMAL)
               PERFORM CHECK-WHAT-TO-DO
             END-IF
           END-IF

      * check for next action
           IF CICS-RESP = DFHRESP(NORMAL)
             PERFORM SEND-SCREEN-DATA
             EXEC CICS RETURN TRANSID(CICS-TC)
               COMMAREA(MMRS-COMMAREA) LENGTH(LENGTH OF MMRS-COMMAREA)
               RESP(CICS-RESP)
             END-EXEC
           ELSE
             PERFORM SEND-END-MESSAGE
             EXEC CICS RETURN END-EXEC
           END-IF
      * transaction ended due cics return above
           .

      * --------------------------------------------------------------
      * process input data --------------------------------------
       PROCESS-SCREEN-DATA SECTION.
      *  System.out.println("user="+MMRS71CI.WHATINI.toStringHex());
           MOVE 15 TO MMRS71Z1-LEN
           CALL MMRS71Z1 USING MMRS71Z1-LEN
                WHATTXTI(25:15) HEX-DISP-BMS-1
           MOVE HEX-DISP-BMS-1(3:28) TO HEX-DISP-BMS-1
           .

      * common initialization -----------------------------------
           COPY MMRS710E.
           
      * missing module -----------------------------------
           CALL 'MISMOD' USING MMRS71Z1-LEN

      * check for next action -----------------------------------
       CHECK-WHAT-TO-DO SECTION.
           IF EIBAID = DFHPF3
             SET MMRS-CLEAR-YES TO TRUE
             EXEC CICS XCTL PROGRAM('MMRS71B1')
               COMMAREA(MMRS-COMMAREA) LENGTH(LENGTH OF MMRS-COMMAREA)
             END-EXEC
           ELSE
             PERFORM PROCESS-SCREEN-DATA
           END-IF
           .

      * send scree data -----------------------------------------
       SEND-SCREEN-DATA SECTION.
           IF EIBCALEN = 0
             MOVE ' '  TO WHATINO
           END-IF
           SET MMRS-CLEAR-NO               TO TRUE
           MOVE 'MMRS71C1_MMRS71C'         TO HEAD001O
           MOVE '** Requirement **     '   TO LBL001O
           MOVE 'Exit '                    TO TXTPF03O
           MOVE ' '                        TO INFOTXTO
           MOVE MY-TEXT  TO WHATINO
           MOVE 'updated WS value' TO MY-TEXT
           PERFORM MMRS-COMMON-INIT-PROCESSING

           MOVE HEX-DISP-BMS-1 TO ACTUAL02O

           EXEC CICS SEND MAP('MMRS71A') MAPSET('MMRS71A')
                ERASE RESP(CICS-RESP)
           END-EXEC
           EXEC CICS SEND MAP('MMRS71C') MAPSET('MMRS71C')
                RESP(CICS-RESP)
           END-EXEC
           EVALUATE CICS-RESP
             WHEN DFHRESP(NORMAL)
                CONTINUE
             WHEN OTHER
                MOVE -1 TO CICS-RESP
                MOVE MMRS-FEEDBACK-NOK  TO INFOTXTO
           END-EVALUATE
           .

      * receive scree data --------------------------------------
       RECEIVE-SCREEN-DATA SECTION.
           EXEC CICS RECEIVE MAP('MMRS71C') MAPSET('MMRS71C')
                RESP(CICS-RESP)
           END-EXEC
           EVALUATE CICS-RESP
             WHEN DFHRESP(NORMAL)
                CONTINUE
             WHEN DFHRESP(MAPFAIL)
                MOVE 0 TO CICS-RESP
                MOVE MMRS-ASK-FOR-LOGIN TO INFOTXTO
             WHEN OTHER
                MOVE -1 TO CICS-RESP
                MOVE MMRS-FEEDBACK-NOK  TO INFOTXTO
           END-EVALUATE
           .
      * error messag --------------------------------------------
       SEND-END-MESSAGE SECTION.
           EXEC CICS SEND TEXT FROM (INFOTXTO)
                LENGTH(LENGTH OF INFOTXTO)
                ERASE
                RESP(CICS-RESP)
           END-EXEC
           .

       END PROGRAM MMRS71C1.
