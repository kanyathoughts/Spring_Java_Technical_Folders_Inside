000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.  CBL001.                                                     
000300*                                                     
000400*                                     
000500*                                                                         
000600*           THIS PROGRAM WILL ISSUE AN IMS '/STOP' OR '/START'            
000700*                               
000800*                                      
000900*                                                                         
001000*                                                                         
001100 ENVIRONMENT DIVISION.                                                    
001200 CONFIGURATION SECTION.                                                   
001300 SOURCE-COMPUTER.    IBM-370.                                             
001400 OBJECT-COMPUTER.    IBM-370.                                             
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700     SELECT PARM-FILE,                                                    
001800             ASSIGN TO ZZ-A-DDNAME01.                                     
001900     SELECT PRINT-FILE,                                                   
002000             ASSIGN TO ZZ-A-DDNAME02.                                     
002100 DATA DIVISION.                                                           
002200 FILE SECTION.                                                            
002300 FD  PARM-FILE,                                                           
002400     RECORDING MODE IS F,                                                 
002500     LABEL RECORDS ARE STANDARD,                                          
002600     BLOCK CONTAINS 0 RECORDS,                                            
002700     RECORD CONTAINS 80 CHARACTERS,                                       
002800     DATA RECORD IS PARM-RECORD.                                          
002900 01  PARM-RECORD              PICTURE X(80).                              
003000 FD  PRINT-FILE,                                                          
003100     RECORDING MODE IS F,                                                 
003200     LABEL RECORDS ARE STANDARD,                                          
003300     BLOCK CONTAINS 0 RECORDS,                                            
003400     RECORD CONTAINS 133 CHARACTERS,                                      
003500     DATA RECORD IS PRINT-RECORD.                                         
003600 01  PRINT-RECORD.                                                        
003700     03  PRINT-CC             PICTURE X(01).                              
003800     03  PRINT-LINE           PICTURE X(132).                             
003900 WORKING-STORAGE SECTION.                                                 
004000 77  GU                  PIC X(4)        VALUE 'GU  '.                    
004100 77  GHU                 PIC X(4)        VALUE 'GHU '.                    
004200 77  GNP                 PIC X(4)        VALUE 'GNP '.                    
004300 77  REPL                PIC X(4)        VALUE 'REPL'.                    
004400 77  ISRT                PIC X(4)        VALUE 'ISRT'.                    
004500 77  CMD                 PIC X(4)        VALUE 'CMD '.                    
004600 77  GCMD                PIC X(4)        VALUE 'GCMD'.                    
004700 77  CHNG                PIC X(4)        VALUE 'CHNG'.                    
004800 77  ROLL                PIC X(4)        VALUE 'ROLL'.                    
004900 77  IMSCNTRL            PIC X(8)        VALUE 'MTO     '.                
005000 77  WXYZ09              PIC X(8)        VALUE 'WXYZ09  '.                
005100 01  COMMAND-AREA.                                                        
005200     03  FILLER          PIC S9(4)       VALUE +1219 COMP.                
005300     03  FILLER          PIC S9(4)       VALUE ZERO  COMP.                
005400     03  CMND-DATA       PIC X(1300).                                     
005500 01  INPZZ-AEGMENT.                                                       
005600     03  FILLER                   PIC S9(4)                 COMP.         
005700     03  FILLER                   PIC S9(4)                 COMP.         
005800     03  INPZZ-AEGMENT-DATA.                                              
005900         05  INPUT-COMMAND.                                               
006000           07  FILLER             PIC X(01).                              
006100             88  INPUT-COMMENT, VALUE IS '*'.                             
006200           07  FILLER             PIC X(79).                              
006300         05  FILLER               PIC X(999).                             
006400 01  ERROR-LINE.                                                          
006500     03  FILLER          PIC X           VALUE '-'.                       
006600     03  FILLER          PIC X(4)        VALUE SPACES.                    
006700     03  FILLER          PIC X(8)        VALUE 'CDA007  '.                
006800     03  ERROR-MSGNO     PIC X(3)        VALUE SPACES.                    
006900     03  FILLER          PIC X(14)       VALUE 'STATUS CODE = '.          
007000     03  ERROR-STATUS    PIC X(2)        VALUE SPACES.                    
007100     03  FILLER          PIC X(2)        VALUE ', '.                      
007200     03  ERROR-MSG       PIC X(75)       VALUE SPACES.                    
007300 01  DATE-SPLIT-AREA.                                                     
007400     03  CURRENT-TIME.                                                    
007500         05  CUR-HH      PIC X(2)        VALUE SPACES.                    
007600         05  FILLER      PIC X           VALUE ':'.                       
007700         05  CUR-MM      PIC X(2)        VALUE SPACES.                    
007800         05  FILLER      PIC X           VALUE ':'.                       
007900         05  CUR-SS      PIC X(2)        VALUE SPACES.                    
008000     03  TIME-AREA.                                                       
008100         05  TIME-HH     PIC X(2)        VALUE SPACES.                    
008200         05  TIME-MM     PIC X(2)        VALUE SPACES.                    
008300         05  TIME-SS     PIC X(2)        VALUE SPACES.                    
008400         05  TIME-TH     PIC X(2)        VALUE SPACES.                    
008500 LINKAGE SECTION.                                                         
008600 01  IO-PCB-1.                                                            
008700     03  IO-USER-TERM-1          PIC X(8).                                
008800     03  FILLER                  PIC X(2).                                
008900     03  IO-STATUS-CODE-1        PIC X(2).                                
009000     03  FILLER                  PIC X(20).                               
009100     03  IO-USER-NAME-1          PIC X(8).                                
009200     03  FILLER                  PIC X(20).                               
009300     03  USER-NAME-IND           PIC X(1).                                
009400 01  IO-PCB-2.                                                            
009500     03  IO-USER-NAME-2           PIC X(8).                               
009600     03  FILLER                   PIC X(2).                               
009700     03  IO-STATUS-CODE-2         PIC X(2).                               
009800 PROCEDURE DIVISION.                                                      
009900 INITIALIZE-ENTRY.                                                        
010000     ENTRY 'DLITCBL' USING IO-PCB-1, IO-PCB-2.                            
010100 GET-TRANSACTION.                                                         
010200     MOVE SPACES TO INPZZ-AEGMENT,                                        
010300                    CMND-DATA,                                            
010400                    ERROR-MSGNO,                                          
010500                    ERROR-MSG,                                            
010600                    ERROR-STATUS.                                         
010700     ACCEPT TIME-AREA FROM TIME.                                          
010800     MOVE TIME-HH TO CUR-HH.                                              
010900     MOVE TIME-MM TO CUR-MM.                                              
011000     MOVE TIME-SS TO CUR-SS.                                              
011100     OPEN INPUT PARM-FILE                                                 
011200          OUTPUT PRINT-FILE.                                              
011300     CALL 'CBLTDLI' USING GU, IO-PCB-1, INPZZ-AEGMENT.                    
011400     IF IO-STATUS-CODE-1 IS EQUAL TO SPACES                               
011500             OR 'CF'                                                      
011600             OR 'QC'                                                      
011700                 NEXT SENTENCE                                            
011800             ELSE MOVE 'ERROR FROM I/O GU CALL' TO ERROR-MSG              
011900                 MOVE IO-STATUS-CODE-1 TO ERROR-STATUS                    
012000                 GO TO CALL-ERROR.                                        
012100 READ-PARM.                                                               
012200     READ PARM-FILE INTO INPZZ-AEGMENT-DATA                               
012300                 AT END GO TO END-OF-TRANSACTION.                         
012400     MOVE INPUT-COMMAND TO CMND-DATA                                      
012500                           PRINT-LINE.                                    
012600     MOVE '-' TO PRINT-CC.                                                
012700     WRITE PRINT-RECORD.                                                  
012800     MOVE SPACES TO PRINT-RECORD.                                         
012900     IF INPUT-COMMENT                                                     
013000                 GO TO READ-PARM.                                         
013100     CALL 'CBLTDLI' USING CMD, IO-PCB-1, COMMAND-AREA.                    
013200     IF IO-STATUS-CODE-1 IS EQUAL TO SPACES                               
013300             OR 'CC'                                                      
013400                 NEXT SENTENCE                                            
013500             ELSE MOVE 'BAD STATUS FROM I/O CMD CALL' TO ERROR-MSG        
013600                 MOVE IO-STATUS-CODE-1 TO ERROR-STATUS                    
013700                 GO TO CALL-ERROR.                                         
013800     GO TO READ-PARM.                                                     
013900 CALL-ERROR.                                                              
014000*    DISPLAY ERROR-LINE.                                                  
014100     MOVE SPACES TO PRINT-RECORD.                                         
014200     WRITE PRINT-RECORD.                                                  
014300     WRITE PRINT-RECORD FROM ERROR-LINE.                                  
014400     CALL 'CBLTDLI' USING ROLL.                                           
014500 END-OF-TRANSACTION.                                                      
014600     CLOSE PARM-FILE                                                      
014700           PRINT-FILE.                                                    
014800     GOBACK.                                                              
