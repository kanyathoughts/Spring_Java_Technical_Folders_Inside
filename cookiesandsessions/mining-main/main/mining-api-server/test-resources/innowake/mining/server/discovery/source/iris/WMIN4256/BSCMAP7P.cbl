
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     BSCMAP7P.

       DATA            DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-COMMAREA          PIC 9(4).
       01  OUTAIDO              PIC X(4).
       01  EIBTRNID PIC X(4) VALUE 'INIT'.

       LINKAGE SECTION.
        01 DFHCOMMAREA          PIC 9(4).

       PROCEDURE DIVISION.

           MOVE 123 TO WS-COMMAREA
               
           EXEC CICS RETURN
           		TRANSID(EIBTRNID)
                COMMAREA(WS-COMMAREA)
           END-EXEC
           
           MOVE 'ABC1' TO OUTAIDO
           MOVE OUTAIDO TO EIBTRNID
               
           EXEC CICS RETURN
           		TRANSID(EIBTRNID)
                COMMAREA(WS-COMMAREA)
           END-EXEC
               
           MOVE 'ABC2' TO EIBTRNID
               
           EXEC CICS RETURN
                TRANSID(EIBTRNID)
                COMMAREA(WS-COMMAREA)
           END-EXEC

           
       END PROGRAM MEE6705A.

