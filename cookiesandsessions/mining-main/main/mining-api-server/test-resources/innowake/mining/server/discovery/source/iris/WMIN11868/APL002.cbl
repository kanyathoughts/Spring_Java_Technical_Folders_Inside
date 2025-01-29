000700 IDENTIFICATION DIVISION.                                                             
000800 PROGRAM-ID. APL002.                                                                  
001000 AUTHOR. XXXXX XXXXXXX.                                                               
001200 DATE-WRITTEN. FEBRUARY 12, 2024.                                                     
001300 DATE-COMPILED.                                                                       
007000 ENVIRONMENT DIVISION.                                                    
007100 CONFIGURATION SECTION.                                                   
007200 SOURCE-COMPUTER.    IBM-370.                                             
007300 OBJECT-COMPUTER.    IBM-370.                                             
007400 DATA DIVISION.                                                           
007500 WORKING-STORAGE SECTION.                                                                              
007900                                                                          
008000 01  SUNDRY-AREA.                                                         
008100     05  GU                   PIC X(04)   VALUE 'GU  '.                   
008200     05  GN                   PIC X(04)   VALUE 'GN  '.                   
008300     05  GNP                  PIC X(04)   VALUE 'GNP '.                   
008400     05  ISRT                 PIC X(04)   VALUE 'ISRT'.                   
008500     05  E1895-MOD-NAME       PIC X(08)   VALUE 'AP0791O'.                
008600     05  MESSAGE-SW           PIC X(01).                                  
008700         88  NO-MORE-MESSAGES     VALUE 'Y'.                                                               
008900     05  INVALID-MDL-ACC-SW   PIC X(01).                                                        
010700   
011000 01  OUTPUT-AREA                  PIC X(957) VALUE SPACES.                                                                                                                                                                            
014400                                                                          
014500 01  AP0791I.                 COPY AP0791I.                               
014600     EJECT                                                                
014700                                                                          
014800 01  AP0791O.                 COPY AP0791O.                               
014900                                                                                                                                                                                                                
015300 LINKAGE SECTION.                                                         
015400 01  IO-PCB.                  COPY LIOPCB.                        
015500                                                                          
015600 01  NVS-PCB.                 COPY DBPCB.                                 
015700                                                                          
015800 01  PPT-PCB.                 COPY DBPCB.                                 
015900                                                                          
016000 01  PTX-PCB.                 COPY DBPCB.                                 
016100                                                                          
016200     EJECT                                                                
016300 PROCEDURE DIVISION USING                                           
016400                        IO-PCB                                            
016500                        NVS-PCB                                           
016600                        PPT-PCB                                           
016700                        PTX-PCB.                                          
016800                                                                          
           PERFORM MAIN-LOOP  THRU  MAIN-LOOP-EXIT.                       
017300                                                                          
017400     GOBACK.                                                              
017500                                                                          
017600                                                                          
017700*****************************************************************         
017800*                    MAIN LOOP LOGIC                            *         
017900*****************************************************************         
018000 MAIN-LOOP.                                                               
018100                                                                          
023600     CALL 'APL003'                                                        
023700           USING                                                 
023800           IO-PCB                                             
023900           NVS-PCB                                            
024000           PTX-PCB                                            
024100           PA0791I                                            
024200           UPDATE-SW                                          
024300           INVALID-MDL-ACC-SW.  

           PERFORM GET-INPUT-MSG. 
            
           CALL 'CBLTDLI' USING                                                
                 GU                                                      
                 NVS-PCB                                               
                 STMPART.                                               
            
           PERFORM ISRT-SCREEN.                                         
028500                                                                          
028600 MAIN-LOOP-EXIT.                                                          
028700     EXIT.                                                                
028800     EJECT                                                                
028900                                                                          
029000                                                                          
029100*****************************************************************         
029200*                GET MESSAGE FROM INPUT QUEUE                   *         
029300*****************************************************************         
029400 GET-INPUT-MSG.                                                           
029500     CALL 'CBLTDLI'                                                       
029600              USING                                                       
029700                 GU                                                       
029800                 IO-PCB                                                   
029900                 AP0791I.                                                 
030000                                                                          
030100     IF E1864-PCB-STA-CD OF IO-PCB = SPACES                               
030200        NEXT SENTENCE                                                     
030300     ELSE                                                                 
030400     IF E1864-PCB-STA-CD OF IO-PCB = 'QC'                                 
030500        MOVE 'Y'  TO MESSAGE-SW                                           
030600     ELSE                                                                 
030700        CALL 'IMSABEND'.                                                  
030800                                                                          
030900                                                                          
031000    EJECT                                                                 
031100*****************************************************************         
031200*         SEND ERROR OR CONFORMATION SCREEN TO USER             *         
031300*****************************************************************         
031400 ISRT-SCREEN.                                                                                              
031900     MOVE AP0791O     TO OUTPUT-AREA.                                     
032000                                                                          
032100     CALL  'CBLTDLI'                                                      
032200               USING                                                      
032300                  ISRT                                                    
032400                  IO-PCB                                                  
032500                  OUTPUT-AREA                                     
032600                  E1895-MOD-NAME.                                         
032700                                                                          
032800     IF  E1864-PCB-STA-CD OF IO-PCB EQUAL SPACES                          
032900         NEXT SENTENCE                                                    
033000     ELSE                                                                 
033100         CALL 'IMSABEND'.                                                 
033200                                                                                                                                                                                                                                              
 