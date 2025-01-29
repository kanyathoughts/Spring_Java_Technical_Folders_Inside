       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.  REAL.                                               
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  WS-FIRST-TIME                 PIC X(01) VALUE 'Y'.           
           88 FIRST-TIME                           VALUE 'Y'.           
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