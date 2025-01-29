       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTPSPA58.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*
       100-GET-TRIGGER-PARA.
      *----------------------------------------------------------*----
      *THIS PARAGRAPH IS USED TO TRIGGER THE REQUEST QUEUE TO PROCESS  *
      *--------------------------------------------------------------*

           EXEC CICS RETRIEVE
                     INTO (MQTM)
                     RESP (WS-RESP)
           END-EXEC
           .

       100-GET-TRIGGER-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       999-RETURN.
      *----------------------------------------------------------------*

           GOBACK
           .

       999-RETURN-EXIT.
            EXIT.
