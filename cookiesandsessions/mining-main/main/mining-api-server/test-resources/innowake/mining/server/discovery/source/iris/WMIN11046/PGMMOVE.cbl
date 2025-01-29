       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PGM1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
          05 WS-DISQ008        PIC X(08) VALUE 'DISQS008'.
          05 WS-DUMMY          PIC X(08) VALUE  'DUMMY'.
          01  EIBTRNID PIC X(4) VALUE 'INIT'.
       01  DAWS.
         05  DAWS-CALLED-PGM          PIC X(08)
                                      VALUE SPACES.        
                                                           
       LINKAGE SECTION.
       PROCEDURE DIVISION USING LINKAGE-PARAMS.
       
       5100-CALL-DISQS008.
         
           MOVE WS-DISQ008         TO DAWS-CALLED-PGM
           
           CALL DAWS-CALLED-PGM    USING DISCIELK-AREA 
                                         DISCIQS-QUEUE-AREA.
           MOVE "NOTVALID" TO WS-DISQ008
           MOVE 'ABC1' TO OUTAIDO
           MOVE WS-DUMMY TO OUTAIDO
           MOVE OUTAIDO TO EIBTRNID
               
           EXEC CICS RETURN
           		TRANSID(EIBTRNID)
                COMMAREA(WS-COMMAREA)
           END-EXEC
                                         
                                         
       5100-EXIT.
           EXIT.     
