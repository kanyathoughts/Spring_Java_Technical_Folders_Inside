       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTPPC003
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
        01  WORK-AREAS.
           05  WS-XCTL-PROG                     PIC X(8).
      *
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       100-XCTL-TO-PROCESSING-MODULE.
      *----------------------------------------------------------------*
           MOVE SPACE TO WS-XCTL-PROG.
               IF CHOICE-1
                   MOVE 'UTPHS004' TO WS-XCTL-PROG
                   END-IF
               IF CHOICE-2
                   MOVE 'UTPHS005' TO WS-XCTL-PROG
                   END-IF
           EXEC CICS XCTL
               PROGRAM (WS-XCTL-PROG)
               COMMAREA (W-VALUES-TO-PASS)
               LENGTH (999)
           END-EXEC.
      *----------------------------------------------------------------*
       800-SEND-DATA-ONLY.
      *----------------------------------------------------------------*
           MOVE '800-SEND-DATA-ONLY' TO ABND-PRGH-NAME.
           EXEC CICS SEND
               MAPSET('UTMPC03')
               MAP('HSMENU')
               DATAONLY
               CURSOR
           END-EXEC.
      *----------------------------------------------------------------*
       900-RETURN-UTPC.
      *----------------------------------------------------------------*
           MOVE '900-RETURN-UTPC' TO ABND-PRGH-NAME.
           EXEC CICS RETURN
               TRANSID ('UT33')
               COMMAREA (W-VALUES-TO-PASS)
               LENGTH (999)
           END-EXEC.
      *----------------------------------------------------------------*
       900-XCTL-TO-PC003.
      *----------------------------------------------------------------*
           MOVE SPACE TO W-PROCESS-INDICATOR.
           SUBTRACT 1 FROM W-SUBMENU-COUNTER.

           EXEC CICS XCTL
               PROGRAM ('UTPPC003')
               COMMAREA (W-VALUES-TO-PASS)
               LENGTH (999)
           END-EXEC.
      *----------------------------------------------------------------*
       1000-RETURN.
      *----------------------------------------------------------------*
           EXEC CICS RETURN
                END-EXEC.
