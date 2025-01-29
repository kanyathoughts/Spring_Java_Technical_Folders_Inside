****************************************************************************
******** MMRS-M01  MAINFRAME MODERNIZATION REFERENCE SYSTEM
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MMRS71B1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
****************************************************************************
******** VSAM AREAS
****************************************************************************
       01  FILLER       PIC X(16) VALUE 'xxxxxxx-VSAMxxxx'.
       01  MYVSAMK-RESP           PIC 9(08) VALUE ZERO.
       01  MYVSAMK-FILENAME       PIC X(08) VALUE 'VSAMK'.
       01  MYVSAMK-RECORD.
           05 MYVSAMK-ALL.
            10 MYVSAMK-LENX       PIC X(04).
            10 MYVSAMK-KEY        PIC X(10).
            10 MYVSAMK-DATA       PIC X(66).
****************************************************************************
******** BMS DIALOG INTERFACE
****************************************************************************
       01  FILLER       PIC X(16) VALUE 'xxxxxxx-MMRS71Ax'.
           COPY MMRS71A.
       01  FILLER       PIC X(16) VALUE 'xxxxxxx-MMRS71Bx'.
           COPY MMRS71B.
       01  FILLER       PIC X(16) VALUE 'xxxxxxx-ENDxxxxx'.
****************************************************************************
******** COMMON USED FIELDS
****************************************************************************
       01  MY-TASK-NO             PIC 9(7) VALUE ZERO.
       77  MY-PROGRAM-NAME        PIC X(8) VALUE 'MMRS71B1'.
       77  MMRS71Z1               PIC X(8) VALUE 'MMRS71Z1'.
       77  MMRS71Z1-LEN           PIC 9(5) VALUE 00020.
       77  MMRS71D1               PIC X(8) VALUE 'MMRS71D1'.
       77  MMRS71D1-LOOP          PIC 9(5) VALUE ZERO.
       77  MMRS71D1-LOOP-COUNT    PIC 9(5) VALUE 00003.
       77  CICS-RESP              PIC S9(8) COMP.
       77  CICS-TC                PIC X(4) VALUE 'MM7B'.
       77  DO-LOGIN-FIRST         PIC X(80) VALUE
           'not yet logged in. Please login first '.
       77  UNKNOWN-PFKEY          PIC X(80) VALUE
           'Please use any supported function key'.
           COPY MMRS710D.
           COPY DFHAID.

****************************************************************************
******** INTERFACE DATA TO THIS COBOL PROGRAM
****************************************************************************
       LINKAGE SECTION.
       01  DFHCOMMAREA            PIC X(250).

****************************************************************************
******** PROCESS
****************************************************************************
       PROCEDURE DIVISION.
           MOVE EIBTASKN TO MY-TASK-NO
      * DO INITIAL PROCESSING --------------------------------------
           MOVE LOW-VALUE TO MMRS71AI
           MOVE LOW-VALUE TO MMRS71BI
           MOVE 0                          TO CICS-RESP

      * PROCESS CONTROL --------------------------------------
      * GET SCREEN DATA --------------------------------------
           IF  EIBCALEN = 0
             PERFORM ONLY-AT-FIRST-TIME
           ELSE
             MOVE DFHCOMMAREA              TO MMRS-COMMAREA
           END-IF
           IF MMRS-CLEAR-YES
             MOVE MMRS-ASK-FOR-LOGIN       TO INFOTXTO
           ELSE
             PERFORM RECEIVE-SCREEN-DATA
             IF CICS-RESP = DFHRESP(NORMAL)
               PERFORM CHECK-WHAT-TO-DO
             END-IF
           END-IF
      * CHECK FOR NEXT ACTION
           IF CICS-RESP = DFHRESP(NORMAL)
             PERFORM SEND-SCREEN-DATA
             SET MMRS-CLEAR-NO TO TRUE
             EXEC CICS RETURN TRANSID(CICS-TC)
               COMMAREA(MMRS-COMMAREA) LENGTH(LENGTH OF MMRS-COMMAREA)
               RESP(CICS-RESP)
             END-EXEC
           ELSE
             PERFORM SEND-END-MESSAGE
             EXEC CICS RETURN END-EXEC
           END-IF
      * TRANSACTION ENDED DUE CICS RETURN ABOVE
           .

      * --------------------------------------------------------------
      * PROCESS INPUT DATA --------------------------------------
       PROCESS-SCREEN-DATA SECTION.
      *  SYSTEM.OUT.PRINTLN("USER="+MMRS71BI.USERIDI.TOSTRINGHEX());
      *  SYSTEM.OUT.PRINTLN("PASS="+MMRS71BI.PWDATAI.TOSTRINGHEX());
           SET MMRS-LOGIN-NOK TO TRUE
           IF  USERIDI(1:2) = 'IW'
             IF  PWDATAI(1:2) = 'IW'
               MOVE MMRS-FEEDBACK-OK    TO INFOTXTO
               SET MMRS-LOGIN-OK        TO TRUE
             ELSE
               MOVE MMRS-LOGIN-PASS-NOK TO INFOTXTO
             END-IF
           ELSE
             MOVE MMRS-LOGIN-USER-NOK   TO INFOTXTO
           END-IF
           .

      * COMMON INITIALIZATION -----------------------------------
           COPY MMRS710E.

      * CHECK FOR NEXT ACTION -----------------------------------
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


      * PROCESS PF4 -----------------------------------------
       PROCESS-WORK-STOR-SUBPGM SECTION.
       PROCESS-WORK-STOR-SUBPGM-TOP.
           MOVE SPACES          TO INFOTXTO

           MOVE 1 TO MMRS71D1-LOOP
           PERFORM UNTIL MMRS71D1-LOOP > MMRS71D1-LOOP-COUNT
             EXEC CICS LINK PROGRAM('MMRS71D1')
              COMMAREA(MMRS-COMMAREA) LENGTH(LENGTH OF MMRS-COMMAREA)
             END-EXEC
             ADD 1 TO MMRS71D1-LOOP
           END-PERFORM
           MOVE 'LINK: '        TO INFOTXTO(01:05)
           MOVE 'D1='           TO INFOTXTO(06:03)
           MOVE MMRS-COUNTER-D1 TO INFOTXTO(09:04)
           MOVE 'Z1='           TO INFOTXTO(14:03)
           MOVE MMRS-COUNTER-Z3 TO INFOTXTO(17:04)

           MOVE 1 TO MMRS71D1-LOOP
           PERFORM UNTIL MMRS71D1-LOOP > MMRS71D1-LOOP-COUNT
             CALL MMRS71D1 USING DFHEIBLK, MMRS-COMMAREA
             END-CALL
             ADD 1 TO MMRS71D1-LOOP
           END-PERFORM
           MOVE 'CALL: '        TO INFOTXTO(23:06)
           MOVE 'D1='           TO INFOTXTO(29:03)
           MOVE MMRS-COUNTER-D1 TO INFOTXTO(32:04)
           MOVE 'Z1='           TO INFOTXTO(37:03)
           MOVE MMRS-COUNTER-Z3 TO INFOTXTO(40:04)
           .

      * PROCESS PF5 -----------------------------------------
       PROCESS-XCTL SECTION.
           IF MMRS-LOGIN-OK
             SET MMRS-CLEAR-YES TO TRUE
             EXEC CICS XCTL PROGRAM('MMRS71C1')
              COMMAREA(MMRS-COMMAREA) LENGTH(LENGTH OF MMRS-COMMAREA)
             END-EXEC
           ELSE
             MOVE DO-LOGIN-FIRST TO INFOTXTO
           END-IF
           .

      * PROCESS PF8 -----------------------------------------
       PROCESS-VSAM-READ SECTION.
           IF MMRS-LOGIN-OK
             MOVE X'FF' TO MYVSAMK-KEY(10:1)
             PERFORM VSAMK-READ
             IF CICS-RESP = DFHRESP(NORMAL)
               MOVE MYVSAMK-ALL  TO INFOTXTO
             ELSE
               MOVE CICS-RESP TO MYVSAMK-RESP
               MOVE ZERO TO CICS-RESP
               STRING 'Error - VSAM READ responsecode=' MYVSAMK-RESP
                 DELIMITED BY SIZE
                 INTO INFOTXTO
               END-STRING
             END-IF
           ELSE
             MOVE DO-LOGIN-FIRST TO INFOTXTO
           END-IF
           .

      * SEND SCREE DATA -----------------------------------------
       SEND-SCREEN-DATA SECTION.
           MOVE 'MMRS71B1_MMRS71B'         TO HEAD001O
           MOVE '** Login **     '         TO LBL001O
           MOVE MY-TASK-NO                 TO LBL001O(30:8)
           MOVE 'Exit '                    TO TXTPF03O
           IF MMRS-LOGIN-OK
             MOVE '=>1D1'                  TO TXTPF04O
             MOVE '=>1C1'                  TO TXTPF05O
             MOVE 'READ '                  TO TXTPF08O
           END-IF
           PERFORM MMRS-COMMON-INIT-PROCESSING
      *    +++ INITIAL IN BMS IS NOT FILLING ALL CHARS TO BLANK
           MOVE ' ' TO USERIDO
           MOVE ' ' TO PWDATAO
           EXEC CICS SEND MAP('MMRS71A') MAPSET('MMRS71A')
                ERASE RESP(CICS-RESP)
           END-EXEC
           EXEC CICS SEND MAP('MMRS71B') MAPSET('MMRS71B')
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

      * RECEIVE SCREE DATA --------------------------------------
       RECEIVE-SCREEN-DATA SECTION.
           EXEC CICS RECEIVE MAP('MMRS71B') MAPSET('MMRS71B')
                RESP(CICS-RESP)
           END-EXEC
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
      * VSAMK READ -------------------------------------------
       VSAMK-READ SECTION.
           MOVE SPACES TO  MYVSAMK-LENX
           MOVE SPACES TO  MYVSAMK-DATA
      *         LENGTH(LENGTH OF MYVSAMK-RECORD)
           EXEC CICS READ FILE (MYVSAMK-FILENAME)
                INTO (MYVSAMK-RECORD)
                RIDFLD(MYVSAMK-KEY)
                KEYLENGTH(LENGTH OF MYVSAMK-KEY)
                GTEQ
                RESP(CICS-RESP)
           END-EXEC
           .
      * ERROR MESSAGE -------------------------------------------
       SEND-END-MESSAGE SECTION.
           EXEC CICS SEND TEXT FROM (INFOTXTO)
                LENGTH(LENGTH OF INFOTXTO)
                ERASE
                RESP(CICS-RESP)
           END-EXEC
           .
      * ONLY-ON-FIRST-RUN -------------------------------------------
       ONLY-AT-FIRST-TIME SECTION.
           MOVE SPACES                   TO MMRS-COMMAREA
           SET MMRS-LOGIN-NOK            TO TRUE
           SET MMRS-CLEAR-YES            TO TRUE
           MOVE SPACES                   TO MYVSAMK-RECORD
           .

       END PROGRAM MMRS71B1.
