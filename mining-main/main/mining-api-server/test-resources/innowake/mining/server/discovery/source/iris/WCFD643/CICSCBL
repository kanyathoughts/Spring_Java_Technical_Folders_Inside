       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     CICSCBL.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COMMAREA          PIC 9(4).
       01  MAPBASE              PIC X(1920).
           COPY MYMAP5.
       PROCEDURE DIVISION.
           IF EIBCALEN = ZERO
               THEN
                  EXEC CICS SEND MAP('MYMAP5')
                              FROM(MAPBASE)
                              CURSOR
                              ERASE
                              FREEKB
                  END-EXEC
                  EXEC CICS RETURN
                         TRANSID(TRNS)
                         COMMAREA(WS-COMMAREA)
                  END-EXEC
           END-IF
           EXEC CICS RETURN
                 TRANSID(MISS)
                 COMMAREA(WS-COMMAREA)
           END-EXEC
           GOBACK.
       END PROGRAM CICSCBL.