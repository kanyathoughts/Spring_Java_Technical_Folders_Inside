       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-SCREEN             PIC X(3000).
       01  WS-MAP-DATA-AREA      PIC X(3000) VALUE LOW-VALUES.
       01  WS-CICS-RESPONSE      PIC 9(04).
       01  WS-COMMAREA           PIC X(3000).
           COPY CPY1.
           COPY CPY2.
       01 ML PIC X(70).
       01 MAP-LENGTH PIC S9(04) COMP.
       01 FIELDA PIC 9(4) COMP VALUE 3599.
       01    FILLER REDEFINES FIELDA.
         02  FIELD_R1  PICTURE X.
         02  FIELD_R2  PICTURE X.
       PROCEDURE DIVISION.

           INITIALIZE M8268AI M8268BI
           IF EIBCALEN = 0

               EXEC CICS SEND CONTROL
                    ERASE
               END-EXEC

               MOVE LENGTH OF M8268AI TO MAP-LENGTH
               EXEC CICS SEND
                    MAP ('M8268A')
                    MAPSET('MS8268A')
                    FROM(M8268AI)
                    LENGTH(MAP-LENGTH)
                    FORMFEED
                    FREEKB
                    ACCUM
                    RESP (WS-CICS-RESPONSE)
               END-EXEC

               MOVE LENGTH OF M8268BI TO MAP-LENGTH
               EXEC CICS SEND
                    MAP ('M8268B')
                    MAPSET('MS8268B')
                    FROM(M8268BI)
                    LENGTH(MAP-LENGTH)
                    CURSOR
                    FORMFEED
                    FREEKB
                    ACCUM
                    RESP (WS-CICS-RESPONSE)
               END-EXEC

               EXEC CICS SEND PAGE
                   RESP(WS-CICS-RESPONSE)
               END-EXEC

               EXEC CICS RETURN
                   TRANSID(EIBTRNID)
                   COMMAREA(WS-COMMAREA)
               END-EXEC
           ELSE IF EIBAID = 3
               EXEC CICS SEND CONTROL ERASE
               END-EXEC
               MOVE 'GOOD BYE' TO ML
               EXEC CICS SEND TEXT FROM(ML) END-EXEC
               EXEC CICS RETURN
               END-EXEC
           ELSE
               MOVE LOW-VALUES TO WS-SCREEN

               EXEC CICS RECEIVE
                    INTO (WS-SCREEN)
                    LENGTH (LENGTH OF WS-SCREEN)
                    RESP (WS-CICS-RESPONSE)
               END-EXEC

               EXEC CICS RECEIVE
                         MAP ('M8268A')
                         MAPSET('MS8268A')
                         INTO(WS-MAP-DATA-AREA)
                         FROM (WS-SCREEN)
                         LENGTH (LENGTH OF WS-SCREEN)
                         RESP (WS-CICS-RESPONSE)
               END-EXEC
               MOVE WS-MAP-DATA-AREA TO M8268AI

               EXEC CICS RECEIVE
                         MAP ('M8268B')
                         MAPSET('MS8268B')
                         INTO(WS-MAP-DATA-AREA)
                         FROM (WS-SCREEN)
                         LENGTH (LENGTH OF WS-SCREEN)
                         RESP (WS-CICS-RESPONSE)
               END-EXEC
               MOVE WS-MAP-DATA-AREA TO M8268BI

               IF FIELD01L > 0
                   MOVE 'HAS L1' TO FIELDA1O
               END-IF
               IF FIELD02L > 0
                   MOVE 'HAS L2' TO FIELDA2O
               END-IF
               IF FIELD03L > 0
                   MOVE 'HAS L3' TO FIELDA3O
               END-IF

               MOVE LENGTH OF M8268AI TO MAP-LENGTH
               EXEC CICS SEND
                    MAP ('M8268A')
                    MAPSET('MS8268A')
                    FROM(M8268AI)
                    LENGTH(MAP-LENGTH)
                    DATAONLY
                    CURSOR
                    FORMFEED
                    FREEKB
                    ACCUM
                    RESP (WS-CICS-RESPONSE)
               END-EXEC

               MOVE LENGTH OF M8268BI TO MAP-LENGTH
               EXEC CICS SEND
                    MAP ('M8268B')
                    MAPSET('MS8268B')
                    FROM(M8268BI)
                    LENGTH(MAP-LENGTH)
                    CURSOR
                    FORMFEED
                    FREEKB
                    ACCUM
                    DATAONLY
                    RESP (WS-CICS-RESPONSE)
               END-EXEC

               EXEC CICS SEND PAGE
               END-EXEC

               EXEC CICS RETURN
                   TRANSID(EIBTRNID)
                   COMMAREA(WS-COMMAREA)
               END-EXEC
           END-IF.

       END PROGRAM CBL1.

