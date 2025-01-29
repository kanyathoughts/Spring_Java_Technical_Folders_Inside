000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.  CDA002.                                                     
000300*AUTHOR. DON RINELLA.                                                     
000400*DATE-WRITTEN.  JUNE 24,1981.                                             
000500*DATE-WRITTEN.  JULY 10,2003 ODJFS.                                       
000600*                                                                         
000700*          THIS PROGRAM WILL DISPLAY THE DBD GROUP NAMES                  
000800*          USED BY THE ODJFS #DBR & #STADB COMMANDS.                      
000900*                                                                         
001000*                                                                         
001100 ENVIRONMENT DIVISION.                                                    
001200 CONFIGURATION SECTION.                                                   
001300 SOURCE-COMPUTER.    IBM-370.                                             
001400 OBJECT-COMPUTER.    IBM-370.                                             
001500 DATA DIVISION.                                                           
001600 WORKING-STORAGE SECTION.                                                 
001700 77  SEG-SUB             PIC S9(4)       VALUE ZERO         COMP.         
001800 77  DIR-SUB             PIC S9(4)       VALUE ZERO         COMP.         
001900 77  TERM-SUB            PIC S9(4)       VALUE ZERO         COMP.         
002000 77  ISRT-CNT            PIC S9(4)       VALUE ZERO         COMP.         
002100   88  FIRST-ISRT-CALL, VALUE IS ZERO.                                    
002200 77  GU                  PIC X(4)        VALUE 'GU  '.                    
002300 77  GN                  PIC X(4)        VALUE 'GN  '.                    
002400 77  GNP                 PIC X(4)        VALUE 'GNP '.                    
002500 77  ISRT                PIC X(4)        VALUE 'ISRT'.                    
002600 77  CHNG                PIC X(4)        VALUE 'CHNG'.                    
002700 77  ROLL                PIC X(4)        VALUE 'ROLL'.                    
002800 77  IMSCNTRL            PIC X(8)        VALUE 'MTO     '.                
002900 77  CODA002             PIC X(8)        VALUE 'CODA002 '.                
003000 77  CODA003             PIC X(8)        VALUE 'CODA003 '.                
003100 77  DFSMO1              PIC X(8)        VALUE 'DFSMO1  '.                
003200 01  ROOT-SSA.                                                            
003300     03  FILLER          PIC X(8)        VALUE 'CS01GROP'.                
003400     03  FILLER          PIC X(3)        VALUE '*DP'.                     
003500     03  RSSA-LPAREN     PIC X           VALUE '('.                       
003600     03  FILLER          PIC X(8)        VALUE 'DBDGROUP'.                
003700     03  FILLER          PIC X(2)        VALUE 'EQ'.                      
003800     03  RSSA-KEY        PIC X(8)        VALUE SPACES.                    
003900     03  FILLER          PIC X           VALUE ')'.                       
004000 01  NAMES-SSA.                                                           
004100     03  FILLER          PIC X(8)        VALUE 'CS01NAME'.                
004200     03  FILLER          PIC X           VALUE SPACE.                     
004300 01  TERMINAL-INPUT.                                                      
004400     03  FILLER                   PIC S9(4)                 COMP.         
004500     03  FILLER                   PIC S9(4)                 COMP.         
004600     03  FILLER.                                                          
004700         05  TERM-TRAN                PIC X(9).                           
004800           88  DISPLAY-GROUP-COMMAND, VALUE IS '#DBGROUP '.               
004900         05  TERM-GROUP-NAME          PIC X(8).                           
005000         05  FILLER                   PIC X(999).                         
005100 01  TERM-DBDGROUP-OUTPUT.                                                
005200     03  DBD-LL                   PIC S9(4)     VALUE +1422  COMP.        
005300     03  DBD-ZZ                   PIC S9(4)                  COMP.        
005400     03  DBD-DBGROUP-NAME         PIC X(8).                               
005500     03  DBD-MSG                  PIC X(79).                              
005600     03  DBD-UPDATKEY             PIC X.                                  
005700     03  DBD-CREATE-DATE          PIC X(6).                               
005800     03  DBD-UPDATE-DATE          PIC X(6).                               
005900     03  DBD-UPDATE-TIME          PIC X(6).                               
006000     03  DBD-UPDATE-USER          PIC X(8).                               
006100     03  DBD-USE-DATE             PIC X(6).                               
006200     03  DBD-CURS-LINE            PIC S9(2)                  COMP.        
006300     03  DBD-CURS-COLUM           PIC S9(2)                  COMP.        
006400     03  DBD-LIST.                                                        
006500         05  FILLER OCCURS 133 TIMES.                                     
006600             07  DBD-NAME         PIC X(9).                               
006700 01  TERM-DIRECTORY-OUTPUT.                                               
006800     03  DIR-LL                   PIC S9(4)     VALUE +1280  COMP.        
006900     03  DIR-ZZ                   PIC S9(4)                  COMP.        
007000     03  DIR-MSG                  PIC X(79).                              
007100     03  DIR-LIST.                                                        
007200         05  FILLER OCCURS 133 TIMES.                                     
007300             07  DIR-GROUP-NAME   PIC X(9).                               
007400 01  DATABASE-SEGMENTS.                                                   
007410         COPY ABCDGROP.                                                   
007420         COPY ABCDNAME.                                                   
008700 01  DIRECTORY-SEGMENT.                                                   
008800     03  DIRECTORY-NAME               PIC X(8).                           
008900 01  ERROR-LINE.                                                          
009000     03  FILLER          PIC S9(4)       VALUE +108  COMP.                
009100     03  FILLER          PIC S9(4)       VALUE ZERO  COMP.                
009200     03  FILLER          PIC X(8)        VALUE 'CDA002  '.                
009300     03  ERROR-MSGNO     PIC X(3)        VALUE SPACES.                    
009400     03  FILLER          PIC X(14)       VALUE 'STATUS CODE = '.          
009500     03  ERROR-STATUS    PIC X(2)        VALUE SPACES.                    
009600     03  FILLER          PIC X(2)        VALUE ', '.                      
009700     03  ERROR-MSG       PIC X(75)       VALUE SPACES.                    
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
012200                    GROP01-CS01GROP,                                      
012300                    NAME01-CS01NAME,                                      
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
014700     MOVE TERM-GROUP-NAME TO RSSA-KEY.                                    
014800     CALL 'CBLTDLI' USING GU, DB-PCB-1, GROP01-CS01GROP                   
014900                          ROOT-SSA, NAMES-SSA.                            
015000     IF DB-STATUS-CODE-1 IS EQUAL TO 'GE'                                 
015100                 MOVE 'DBD GROUP NOT FOUND, ASSUMED NEW'                  
015200                                                     TO DBD-MSG           
015300                 MOVE 'A' TO DBD-UPDATKEY                                 
015400                 MOVE 3 TO DBD-CURS-LINE                                  
015500                 MOVE 5 TO DBD-CURS-COLUM                                 
015600                 GO TO DISPLAY-DBD-LIST.                                  
015700     IF DB-STATUS-CODE-1 IS NOT EQUAL TO SPACES                           
015800                 MOVE 'ERROR FROM DB GU CALL' TO ERROR-MSG                
015900                 MOVE DB-STATUS-CODE-1 TO ERROR-STATUS                    
016000                 GO TO CALL-ERROR.                                        
016100     MOVE 1 TO TERM-SUB.                                                  
016200 NAMES-SEGMENT-LOOP.                                                      
016300     PERFORM MOVE-NAMES THROUGH END-MOVE-NAMES                            
016400         VARYING SEG-SUB                                                  
016500         FROM 1                                                           
016600         BY 1                                                             
016700         UNTIL SEG-SUB IS GREATER THAN 10.                                
016800     CALL 'CBLTDLI' USING GNP, DB-PCB-1, NAME01-CS01NAME,                 
016900                                                  NAMES-SSA.              
017000     IF DB-STATUS-CODE-1 IS EQUAL TO 'GE'                                 
017100                 GO TO DISPLAY-DBD-LIST.                                  
017200     IF DB-STATUS-CODE-1 IS NOT EQUAL TO SPACES                           
017300                 MOVE 'ERROR FROM DB GNP CALL' TO ERROR-MSG               
017400                 MOVE DB-STATUS-CODE-1 TO ERROR-STATUS                    
017500                 GO TO CALL-ERROR.                                        
017600     GO TO NAMES-SEGMENT-LOOP.                                            
017700 MOVE-NAMES.                                                              
017800     IF NAME01-NAMES-DBD9 (SEG-SUB) IS EQUAL TO SPACES                    
017900                 MOVE 10 TO SEG-SUB                                       
018000                 GO TO END-MOVE-NAMES.                                    
018100     MOVE NAME01-NAMES-DBD9 (SEG-SUB) TO DBD-NAME (TERM-SUB).             
018200     ADD 1 TO TERM-SUB.                                                   
018300 END-MOVE-NAMES.           EXIT.                                          
018400 DISPLAY-DBD-LIST.                                                        
018500     MOVE TERM-GROUP-NAME TO DBD-DBGROUP-NAME.                            
018600     MOVE GROP01-CREATE-DATE TO DBD-CREATE-DATE.                          
018700     MOVE GROP01-UPDATE-DATE TO DBD-UPDATE-DATE.                          
018800     MOVE GROP01-UPDATE-TIME TO DBD-UPDATE-TIME.                          
018900     MOVE GROP01-UPDATE-USER TO DBD-UPDATE-USER.                          
019000     MOVE GROP01-USE-DATE TO DBD-USE-DATE.                                
019100     CALL 'CBLTDLI' USING ISRT, IO-PCB-1, TERM-DBDGROUP-OUTPUT,           
019200                                                         CODA002.         
019300     IF IO-STATUS-CODE-1 IS NOT EQUAL TO SPACES                           
019400                 MOVE 'ERROR FROM IO ISRT CALL (DBD LIST)'                
019500                                                    TO ERROR-MSG          
019600                 MOVE IO-STATUS-CODE-1 TO ERROR-STATUS                    
019700                 GO TO CALL-ERROR.                                        
019800     GO TO GET-TRANSACTION.                                               
019900 DISPLAY-DIRECTORY.                                                       
020000     ADD 1 TO DIR-SUB.                                                    
020100     CALL 'CBLTDLI' USING GN, DB-PCB-2, DIRECTORY-SEGMENT.                
020200     IF DB-STATUS-CODE-2 IS EQUAL TO 'GB'                                 
020300                 GO TO DISPLAY-DIRECTORY-LIST.                            
020400     IF DB-STATUS-CODE-2 IS NOT EQUAL TO SPACES                           
020500                 MOVE 'ERROR FROM DB GN CALL (DIRECTORY)'                 
020600                                                  TO ERROR-MSG            
020700                 MOVE DB-STATUS-CODE-2 TO ERROR-STATUS                    
020800                 GO TO CALL-ERROR.                                        
020900     MOVE DIRECTORY-NAME TO DIR-GROUP-NAME (DIR-SUB).                     
021000     IF DIR-SUB IS EQUAL TO 133                                           
021100                 GO TO DISPLAY-DIRECTORY-LIST.                            
021200     GO TO DISPLAY-DIRECTORY.                                             
021300 DISPLAY-DIRECTORY-LIST.                                                  
021400     IF FIRST-ISRT-CALL                                                   
021500                 CALL 'CBLTDLI' USING ISRT, IO-PCB-1,                     
021600                          TERM-DIRECTORY-OUTPUT, CODA003                  
021700             ELSE                                                         
021800                 CALL 'CBLTDLI' USING ISRT, IO-PCB-1,                     
021900                          TERM-DIRECTORY-OUTPUT.                          
022000     IF IO-STATUS-CODE-1 IS NOT EQUAL TO SPACES                           
022100                 MOVE 'ERROR FROM IO ISRT CALL (DIRECTORY LIST)'          
022200                                                      TO ERROR-MSG        
022300                 MOVE IO-STATUS-CODE-1 TO ERROR-STATUS                    
022400                 GO TO CALL-ERROR.                                        
022500     ADD 1 TO ISRT-CNT.                                                   
022600     MOVE ZERO TO DIR-SUB.                                                
022700     MOVE SPACES TO DIR-LIST.                                             
022800     IF DB-STATUS-CODE-2 IS EQUAL TO 'GB'                                 
022900                 GO TO GET-TRANSACTION.                                   
023000     GO TO DISPLAY-DIRECTORY.                                             
023100 CALL-ERROR.                                                              
023200     DISPLAY ERROR-LINE.                                                  
023300*    CALL 'CBLTDLI' USING CHNG, IO-PCB-2, IMSCNTRL.                       
023400*    CALL 'CBLTDLI' USING ISRT, IO-PCB-2, ERROR-LINE.                     
023500     CALL 'CBLTDLI' USING ROLL.                                           
023600 END-OF-TRANSACTION.                                                      
023700     GOBACK.                                                              
