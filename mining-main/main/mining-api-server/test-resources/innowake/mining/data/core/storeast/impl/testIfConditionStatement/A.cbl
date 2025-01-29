       PROCEDURE DIVISION.
                 IF TESTFIELD EQUALS 1
                   EXEC CICS RETURN END-EXEC
                   IF CICS-RESP = DFHRESP(NORMAL)
                     MOVE X'FF' TO MYVSAMK-KEY(10:1)
                     PERFORM VSAMK-READ
                   ELSE
                     MOVE CICS-RESP TO MYVSAMK-RESP
                     MOVE ZERO TO CICS-RESP
                   END-IF   
                 ELSE
                   EXEC CICS RETURN TRANSID('TRA1') END-EXEC
                 END-IF.
       END PROGRAM A.                 