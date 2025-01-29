000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.  CDA002.                                                     
001100 ENVIRONMENT DIVISION.                                                    
001200 CONFIGURATION SECTION.                                                   
001300 SOURCE-COMPUTER.    IBM-370.                                             
001400 OBJECT-COMPUTER.    IBM-370.                                             
001500 DATA DIVISION.                                                           
001600 WORKING-STORAGE SECTION.                                                 
009800 LINKAGE SECTION.                                                         
009810 01  IO-PCB-1.                                                            
009820     03  USER-TERM-1             PIC X(8).                                
009830     03  FILLER                  PIC X(2).                                
009840     03  IO-STATUS-CODE-1        PIC X(2).                                
009850     03  FILLER                  PIC X(20).                               
009860     03  USER-NAME-1             PIC X(8).                                
009870     03  FILLER                  PIC X(20).                               
009880     03  USER-NAME-IND           PIC X(1).                                
010300 01  IO-PCB-2.                                                            
010400     03  USER-NAME-2             PIC X(8).                                
010500     03  FILLER                  PIC X(2).                                
010600     03  IO-STATUS-CODE-2        PIC X(2).                                
010700 01  DB-PCB-1.                                                            
010800     03  DBD-NAME-1              PIC X(8).                                
010900     03  FILLER                  PIC X(2).                                
011000     03  DB-STATUS-CODE-1        PIC X(2).                                
011100 01  DB-PCB-2.                                                            
011200     03  DBD-NAME-2              PIC X(8).                                
011300     03  FILLER                  PIC X(2).                                
011400     03  DB-STATUS-CODE-2        PIC X(2).                                
011500 PROCEDURE DIVISION.                                                      
011600 INITIALIZE-ENTRY.                                                        
011700     ENTRY 'DLITCBL' USING IO-PCB-1, IO-PCB-2, DB-PCB-1, DB-PCB-2.        
011800 GET-TRANSACTION.                                                         
011900     MOVE SPACES TO ERROR-MSGNO,                                          
012000                    ERROR-MSG,                                            
012100                    ERROR-STATUS,                                         
012400                    DIR-LIST,                                             
012500                    DIR-MSG,                                              
012600                    DBD-LIST,                                             
012700                    DBD-MSG,                                              
012800                    DBD-DBGROUP-NAME,                                     
012900                    TERMINAL-INPUT.                                       
013000     MOVE 'U' TO DBD-UPDATKEY.                                            
013100     MOVE ZERO TO TERM-SUB,                                               
013200                  DIR-SUB,                                                
013300                  SEG-SUB,                                                
013400                  ISRT-CNT,                                               
013500                  DBD-CURS-LINE,                                          
013600                  DBD-CURS-COLUM.                                         
013700     CALL 'CBLTDLI' USING GU, IO-PCB-1, TERMINAL-INPUT.                   
013800     IF IO-STATUS-CODE-1 IS EQUAL TO 'QC'                                 
013900                 GO TO END-OF-TRANSACTION.                                
014000     IF IO-STATUS-CODE-1 IS NOT EQUAL TO SPACES                           
014100             AND 'CF'                                                     
014200                 MOVE 'ERROR FROM IO GU CALL' TO ERROR-MSG                
014300                 MOVE IO-STATUS-CODE-1 TO ERROR-STATUS                    
014400                 GO TO CALL-ERROR.                                        
014500     IF TERM-GROUP-NAME IS EQUAL TO SPACES                                
014600                 GO TO DISPLAY-DIRECTORY.                                 
023100 CALL-ERROR.                                                              
023200     DISPLAY ERROR-LINE.                                                  
023300*    CALL 'CBLTDLI' USING CHNG, IO-PCB-2, IMSCNTRL.                       
023400*    CALL 'CBLTDLI' USING ISRT, IO-PCB-2, ERROR-LINE.                     
023500     CALL 'CBLTDLI' USING ROLL.                                           
023600 END-OF-TRANSACTION.                                                      
023700     GOBACK.                                                              
