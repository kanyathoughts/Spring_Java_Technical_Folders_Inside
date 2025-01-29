
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     MEE6705A.

       DATA            DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-COMMAREA          PIC 9(4).
       01  MAPBASE              PIC X(1920).
           COPY MTEMAP7.

       LINKAGE SECTION.
        01 DFHCOMMAREA          PIC 9(4).

       PROCEDURE DIVISION.

           MOVE LOW-VALUES TO MAPBASE

           EXEC CICS HANDLE CONDITION
                     MAPFAIL(CATCH-MAPFAIL)
           END-EXEC

           IF EIBCALEN = 0
               MOVE 01 TO WS-COMMAREA

               EXEC CICS SEND MAP('MTEMAP7')
                              FROM(MAPBASE)
                              CURSOR
                              ERASE
                              FREEKB
               END-EXEC
               EXEC CICS RETURN
                         TRANSID(EIBTRNID)
                         COMMAREA(WS-COMMAREA)
               END-EXEC
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA

               ADD 1 TO WS-COMMAREA
               EXEC CICS RECEIVE MAP('MTEMAP7')
                                 INTO(MAPBASE)
               END-EXEC

           END-IF
           .

       END PROGRAM MEE6705A.

