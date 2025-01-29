       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.  REAL.                                               
       DATA DIVISION.                                                   
        FILE SECTION.                                                    
                                                                  
        FD    REQUEST-FILE                                               
              RECORD CONTAINS 80 CHARACTERS                              
              RECORDING MODE IS F.                                       
        01    OUT-REC               PIC X(080).                          
                                                                  
               FD    IN-FILE                                                    
              RECORD CONTAINS 300 CHARACTERS                             
              RECORDING MODE IS F.                                       
        01  INREC.                                                       
            05  WS-NAMEO                    PIC X(32).                   
            05  WS-FILLA                    PIC X(01).                   
            05  WS-CAREOFO                  PIC X(32).                   
            05  WS-FILLB                    PIC X(01).                   
            05  WS-ADDR1O                   PIC X(35).                   
            05  WS-FILLC                    PIC X(01).                   
            05  WS-ADDR2O                   PIC X(35).                   
            05  WS-FILLN                    PIC X(01).                   
            05  WS-CITYO                    PIC X(15).                   
            05  WS-FILLD                    PIC X(01).                   
            05  WS-STATEO                   PIC X(02).                   
            05  WS-FILLE                    PIC X(01).                   
            05  WS-ZIPO                     PIC X(05).                   
            05  WS-FILLF                    PIC X(01).                   
            05  WS-DATE-MAILEDO             PIC X(10).                   
            05  WS-FILLG                    PIC X(01).                   
            05  WS-DATE-DUEO                PIC X(10).                   
            05  WS-FILLH                    PIC X(01).                   
            05  WS-SSNO                     PIC X(09).                   
            05  WS-FILLI                    PIC X(01).                   
            05  WS-DOBO                     PIC X(10).                   
            05  WS-FILLJ                    PIC X(01).                   
            05  WS-PHONEO                   PIC X(10).                   
            05  WS-FILLK                    PIC X(01).                   
            05  WS-CITIZENO                 PIC X(01).                   
            05  WS-FILLL                    PIC X(01).                   
            05  WS-ALIEN-NO                 PIC X(10).                   
            05  WS-FILLM                    PIC X(01).                   
            05  WS-LNAME4                   PIC X(04).                   
            05  WS-FILLQ                    PIC X(01).                   
            05  WS-BYBO                     PIC X(10).                   
            05  WS-FILLO                    PIC X(01).                   
            05  WS-BYEO                     PIC X(10).                   
            05  WS-FILLP                    PIC X(07).                   
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  WS-FIRST-TIME                 PIC X(01) VALUE 'Y'.           
           88 FIRST-TIME                           VALUE 'Y'.           
           
        01  DOR-REQUEST-REC.                                             
           05  DOR-REQ-SSN     PIC X(09).                               
           05  DOR-REQ-LNAME   PIC X(05).                               
           05  DOR-REQ-FNAME   PIC X(01).                               
           05  DOR-REQ-DOB     PIC X(08).                               
           05  DOR-REQ-TRANID  PIC X(02).                               
           05  DOR-REQ-FILLER  PIC X(55).                               
       PROCEDURE DIVISION.                                              
                                                                        
       MAIN0100-CONTROL.                                                
                                                                        
           DISPLAY ' '.                                                 
           DISPLAY ' -------------------- '.                            
           DISPLAY 'QBGPDOR1 PGM STARTED '.                             
           DISPLAY ' -------------------- '.                            
                                                                        
           MOVE 'S'                    TO DB-PROCESSING-MODE.           
           MOVE 'S'                    TO DB-COMMAND-CODE.              
           MOVE 'SG01'                 TO DB-SEGNAME.                   
           PERFORM SERV1000-ACCESS-DATABASE THRU SERV1000-EXIT.         
                                                                        
           OPEN INPUT  IN-FILE.                                         
           OPEN OUTPUT REQUEST-FILE.                                    
                                                                        
           PERFORM PROC0010-BYPASS  THRU PROC0010-EXIT                  
                   UNTIL WS-NO-MORE-HDRS    = 'Y'                       
                   OR    WS-NO-MORE-RECORDS = 'Y'.                      
                                                                        
           IF WS-NO-MORE-RECORDS = 'Y'                                  
              CONTINUE                                                  
           ELSE                                                         
              PERFORM PROC0100-PROCESS THRU PROC0100-EXIT               
                   UNTIL WS-NO-MORE-RECORDS = 'Y'.                      
                                                                        
           CLOSE IN-FILE.                                               
           CLOSE REQUEST-FILE.                                          
                                                                        
           DISPLAY ' '.                                                 
           DISPLAY '   RECORDS READ  : ' WS-READ-CNT.                   
           DISPLAY ' '.                                                 
           DISPLAY '   RECORDS OUT   : ' WS-OUT-CNT.                    
           DISPLAY ' '.                                                 
           DISPLAY ' -------------------- '.                            
           DISPLAY 'QBGPDOR1 PGM ENDED   '.                             
           DISPLAY ' -------------------- '.                            
           DISPLAY ' '.                                                 
                                                                        
       MAIN0100-EXIT.                                                   
           GOBACK.                                                      
                                                                        
       PROC0010-BYPASS.                                                 
           READ IN-FILE                                                 
                AT END                                                  
                       MOVE 'Y'        TO WS-NO-MORE-RECORDS.           
                                                                        
           ADD 1                       TO WS-READ-CNT.                  
                                                                        
           IF WS-NO-MORE-RECORDS = 'Y'                                  
              CONTINUE                                                  
           ELSE                                                         
              IF INREC(1:5) = 'CLMNT'                                   
                 MOVE 'Y'              TO WS-NO-MORE-HDRS               
              END-IF                                                    
           END-IF.                                                      
                                                                        
       PROC0010-EXIT. EXIT.                                             
       PROC0100-PROCESS.                                                
                                                                        
           READ IN-FILE                                                 
                AT END                                                  
                       MOVE 'Y'        TO WS-NO-MORE-RECORDS.           
                                                                        
           ADD 1                       TO WS-READ-CNT.                  
           IF WS-NO-MORE-RECORDS = 'Y'                                  
           OR INREC(1:1) = '%'                                          
           OR INREC(1:1) = '('                                          
                                                                        
              CONTINUE                                                  
                                                                        
           ELSE                                                         
                                                                        
               MOVE WS-SSNO            TO DB-SSN                        
               MOVE 0                  TO DB-SEQ                        
               MOVE 'R'                TO DB-PROCESSING-MODE            
               MOVE 'R'                TO DB-COMMAND-CODE               
               MOVE 'SG01'             TO DB-SEGNAME                    
               MOVE LOW-VALUE          TO DB-COMPLETION-CODE            
               PERFORM SERV1000-ACCESS-DATABASE THRU SERV1000-EXIT      
                                                                        
               IF  DB-SUCCESSFUL-COMPLETION                             
                                                                        
                  MOVE WS-STATEO       TO DOR-REQ-TRANID                
                  MOVE CPD-SSN         TO DOR-REQ-SSN                   
                                                                        
                  PERFORM PROC0200-CHECK-NAME THRU PROC0200-EXIT        
                  MOVE WS-MP-NAME(1:5) TO DOR-REQ-LNAME                 
                                                                        
                  MOVE CPD-BIRTH-DATE  TO WS-HOLD-DOB                   
                  MOVE WS-HOLD-DOB     TO WS-DOB                        
                                                                        
                  IF WS-CENTURY = '0'                                   
                     MOVE '19'         TO WS-CENTURY-O                  
                  ELSE                                                  
                     MOVE '20'         TO WS-CENTURY-O                  
                  END-IF                                                
                                                                        
                  MOVE WS-YY           TO WS-YY-O                       
                  MOVE WS-MM           TO WS-MM-O                       
                  MOVE WS-DD           TO WS-DD-O                       
                  MOVE WS-DOB-OUT      TO DOR-REQ-DOB                   
                                                                        
                  MOVE SPACES          TO DOR-REQ-FILLER                
                                                                        
                  WRITE OUT-REC  FROM  DOR-REQUEST-REC                  
                  ADD 1                TO WS-OUT-CNT.                   
       PROC0100-EXIT. EXIT.                                             
       PROC0200-CHECK-NAME.                                             
              MOVE ZEROS                TO CCOUNT.                      
              MOVE CPD-NAME             TO WS-MP-NAME.                  
                                                                        
              INSPECT CPD-NAME TALLYING                                 
                 CCOUNT FOR ALL '/'.                                    
                                                                        
              IF CCOUNT > 0                                             
                 INSPECT WS-MP-NAME                                     
                    REPLACING CHARACTERS BY SPACE                       
                    AFTER INITIAL '/'                                   
                 INSPECT WS-MP-NAME                                     
                    REPLACING ALL '/' BY ' '                            
              END-IF.                                                   
                                                                        
              MOVE CPD-NAME             TO COM-NAM.                     
              MOVE 2                    TO COM-NAME-FORMAT.             
              PERFORM SERV7000-CONVERT-NAME THRU                        
                      SERV7000-EXIT.                                    
                                                                        
              IF COM-NAME-RETURN = ZERO                                 
                 MOVE COM-NAM(1:1)      TO DOR-REQ-FNAME                
              ELSE                                                      
                 MOVE SPACES            TO DOR-REQ-FNAME.               
       PROC0200-EXIT.                                                   
           EXIT.                                                        
       SERV1000-ACCESS-DATABASE.                                        
                                                                        
           MOVE 'QBGPDOR1' TO DB-PROGRAM-NAME.                          
           CALL 'QBGCMFC'                                               
             USING COMMON-LINKAGE-SECTION.                              
                                                                        
       SERV1000-EXIT.                                                   
           EXIT.                                                        
       SERV7000-CONVERT-NAME.                                           
           CALL 'ESP902D' USING COM-NAME-INVERT-AREA.                   
       SERV7000-EXIT.                                                   
           EXIT.                                                        