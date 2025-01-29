000100*** MAINTENANCE LOG ********************************* 12/13/17 ***171213TS
000101*** 171213TS - PKG=PSS1009411 - T0Y0ECB - SHRAVANI-TANJORE     ***171213TS
000102***            TACM 19593- PC HARDCODE REMOVAL- COMPLEX PHASE2 ***171213TS
000103***            REMOVED PARTCENTER HARDCODED VARIABLES          ***171213TS
000104*** 170607KC - PKG=PSS1009097 - T050D66 - PHANI RAM VARANASI   ***170607KC
000105***            PART CENTER HARD CODE REMOVAL CHANGES           ***170607KC
000110*** 080229MA - PKG=PSS1005126 - T0J0JAG - MOHAMMED ALIYAR      ***080229MA
000120***            RECOMPILE ONLY - ST# 20070305-0915-RAT (K58364) ***080229MA
000130***            TO PICK UP SS-DAYS CHANGES IN COPYLIB YPMWINVL. ***080229MA
000200*** 060213OS - PKG=PSS1004434 - T0J0NBK - OTTO SCHNIRER.       ***060213OS
000300***            20060210-1517-LEE (38820) INITIALIZE VALUES     ***060213OS
000400***            WS-OH-STK, WS-IN-XFER, AND WS-OH-XDOCK WHERE    ***060213OS
000500***            NO DATA RECORD FOR WAREHOUSE.                   ***060213OS
000600*** 000317HJ - PKG=PSS1001666 - T050CLG - HANBO JO             ***000317HJ
000700*** 990806EL - PKG=PSS1001356 - T050FXY - ED LOU               ***990806EL
000800***            USE LATEST MKT DMD MO RATHER THAN CURRENT MO    ***990806EL
000900***            TO DETERMINE PREVIOUS 3 MONTHS FOR AVG DMD.     ***990806EL
001000******************************************************************990806EL
001100*************************** MAINTENANCE LOG *********************         
001200*  DATE         PROGRAMMER         DESCRIPTION                            
001300*  -----        -----------        ------------                           
001400*  04/29/99     T.K.SRIRAM         CREATION                               
001500*  03/14/00     H. JO              ADDED TOTALS OF IN-XFER AND    000317HJ
001600*                                        OH-XDOCK ON SCREEN.      000317HJ
001700*  08/21/06     MAHESWARAN S       APS RACF CONVERSION PROJECT            
001800*                                  SOX COMPLIANCE PHASE II                
001900*                                  (SEARCH STRING 'APSSOX').              
001910*  05/12/17     KESAVA CHENNA      PART CENTER HARD CODE REMOVAL**170607KC
002000******************************************************************        
002100 IDENTIFICATION DIVISION.                                                 
002200 PROGRAM-ID.    POM915.                                                   
002300 AUTHOR.        T.K.SRIRAM.                                               
002400 DATE-WRITTEN.  APRIL 1999.                                               
002500 DATE-COMPILED.                                                           
002600*REMARKS.                                                         000317HJ
002700*                         ***  ONLINE PGM ***                             
002800*                                                                         
002900*    TITLE     -  ONLINE STOCK STATUS INQUIRY FOR ACCESSORY               
003000*                 THIS PROGRAM IS CALLED BY POM906                        
003100*                 THIS PROGRAM ALSO CALLS POM906                          
003200*                 TRANSACTION: POM915                                     
003300*                 DIF/DOF:     PI9805                                     
003400*                                                                         
003500*    MODIFICATIONS:                                                       
003600*                                                                         
003700 ENVIRONMENT DIVISION.                                                    
003800 CONFIGURATION SECTION.                                                   
003900                                                                          
004000    SOURCE-COMPUTER.   IBM-370.                                           
004100    OBJECT-COMPUTER.   IBM-370.                                           
004200                                                                          
004300 DATA DIVISION.                                                           
004400     EJECT                                                                
004500*****************************************************************         
004600***                 WORKING STORAGE                           ***         
004700*****************************************************************         
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000 01  FILLER                    PIC X(34) VALUE                            
005100                'POM906 WORKING STORAGE BEGINS HERE'.                     
005200 01  WORK-AREA.                                                           
005300                                                                          
005400     03 WS-SYSTEM-DATE.                                                   
005500           05 WS-SYS-CCYY                PIC 9(04) VALUE ZERO.            
005600           05 WS-SYS-MM                  PIC 9(02) VALUE ZERO.            
005700           05 WS-SYS-DD                  PIC 9(02) VALUE ZERO.            
005800                                                                          
005900     03 WS-LATEST-DMD-MO              PIC 9(06) VALUE ZERO.               
006000                                                                          
006100     03 WS-TEMP-YEAR-MONTH.                                               
006200        05 WS-TEMP-CCYY               PIC 9(04) VALUE ZERO.               
006300        05 WS-TEMP-MM                 PIC 9(02) VALUE ZERO.               
006400                                                                          
006500     03 WS-PREV-YEAR-MONTH.                                               
006600        05 WS-PREV-CCYY               PIC 9(04) VALUE ZERO.               
006700        05 WS-PREV-MM                 PIC 9(02) VALUE ZERO.               
007200                                                                          
007300     03 WS-DETAIL-STRUCTURE.                                              
007400        05 WS-OH-STK               PIC S9(7) COMP-3 VALUE +0.             
007500        05 WS-IN-XFER              PIC S9(7) COMP-3 VALUE +0.     000317HJ
007600        05 WS-OH-XDOCK             PIC S9(7) COMP-3 VALUE +0.     000317HJ
007700        05 WS-ON-ORDER             PIC S9(7) COMP-3 VALUE +0.             
007800        05 WS-BACK-ORDER           PIC S9(7) COMP-3 VALUE +0.             
007900        05 WS-AVG-DMD              PIC S9(7) COMP-3 VALUE +0.             
008000        05 WS-MTH-OF-SUPPLY        PIC S9(3)V9 COMP-3 VALUE +0.           
008100                                                                          
008200     03 WS-MISC-FLDS.                                                     
008300        05 WS-ON-ORD-TMD              PIC S9(7)V COMP-3 VALUE +0.         
008400        05 WS-MKT-P                   PIC X(01)  VALUE 'P'.               
008500        05 WS-TOTAL-DMD-FCST          PIC S9(7)V COMP-3 VALUE +0.         
008600        05 WS-SCREEN-NAME             PIC X(08)  VALUE                    
008700                                                 'PI9805O '.              
008800        05 WS-INVALID-RSN-CD          PIC X(03) VALUE SPACES.             
008900        05 DS-SQLCODE                 PIC -9(09).                         
009200        05 WS-LIN-TYPE                PIC X(01) VALUE 'B'.                
009300        05 WS-LIN-STAT                PIC X(02) VALUE 'PA'.               
009400        05 WS-COMP-ON-HAND            PIC S9(07) VALUE +0.                
009500        05 WS-COMP-AVG-DMD            PIC S9(06)V9 VALUE +0.              
009600        05 WS-MTH-SUP-TOTAL           PIC S9(03)V9 VALUE +0.              
009700        05 WS-12                      PIC 9(02) VALUE 12.                 
009800     05  WS-PART-NUM2.                                                    
009900         10 WS-PART-NUM2-1-5     PIC  X(5)           VALUE SPACES.        
010000         10 FILLER               PIC  X(03)          VALUE ' - '.         
010100         10 WS-PART-NUM2-6-10    PIC  X(5)           VALUE SPACES.        
010200         10 FILLER               PIC  X(03)          VALUE ' - '.         
010300         10 WS-PART-NUM2-11-15   PIC  X(5)           VALUE SPACES.        
010400******************************************************************        
010500*      TOTAL TABLE FOR TPNA,TMD AND GRAND TOTAL        *                  
010600******************************************************************        
010700 01  WS-ACCUM-TABLES.                                                     
010800     05  ACCUM-TBL                   OCCURS 3 TIMES                       
010900                                     INDEXED BY TOT-INDX.                 
011000         10  E360-CURR-QTY-ON-HAND  PIC S9(7)   COMP-3 VALUE +0.          
011100         10  E361-CURR-QTY-ON-ORD   PIC S9(7)   COMP-3 VALUE +0.          
011200         10  E459-CURR-QTY-BO       PIC S9(7)   COMP-3 VALUE +0.          
011300         10  E000-CURR-QTY-IN-XFER  PIC S9(7)   COMP-3 VALUE +0.  000317HJ
011400         10  E000-CURR-QTY-OH-XDOCK PIC S9(7)   COMP-3 VALUE +0.  000317HJ
011500         10  E930-AVG-DMD           PIC S9(6)V9 COMP-3 VALUE +0.          
011600         10  E2705-MTHS-SUP         PIC S9(3)V9 COMP-3 VALUE +0.          
011700                                                                          
011800*****************************************************                     
011900*                   COUNTERS                        *                     
012000*****************************************************                     
012100                                                                          
012300 01  WS-MONTH-COUNTER                PIC 9(01) VALUE ZERO.                
012400                                                                          
012500************************************************************              
012600*                        SWITCHES                          *              
012700************************************************************              
012800 01  WS-SWITCHES.                                                         
012900                                                                          
013000     05  SYS-MSG-SW              PIC 9(01) VALUE ZERO.                    
013100        88 INVALID-PW                      VALUE 1.                       
013200        88 STK-IND-NOT-VALID               VALUE 2.                       
013300        88 PART-NOT-FOUND                  VALUE 3.                       
013400                                                                          
013500     05  SECURITY-ERROR-SW      PIC X(1)   VALUE SPACES.                  
013600         88  SECURITY-VIOLATION            VALUE 'Y'.                     
013700*                                                                         
013800     05  PART-STATUS            PIC X.                                    
013900         88  PART-NOT-FOUND                VALUE 'N'.                     
014000         88  PART-FOUND                    VALUE 'Y'.                     
014100*                                                                         
014200     05  DEPOT-STATUS           PIC X.                                    
014300         88  NO-MORE-DEPOTS                VALUE 'N'.                     
014400         88  DEPOT-FOUND                   VALUE 'Y'.                     
014500*                                                                         
014600     05  PTINV-STATUS           PIC X.                                    
014700         88  PTINV-NOT-FOUND               VALUE 'N'.                     
014800         88  PTINV-FOUND                   VALUE 'Y'.                     
014900*                                                                         
015000     05  PTIDINV-STATUS         PIC X.                                    
015100         88  PTIDINV-NOT-FOUND             VALUE 'N'.                     
015200         88  PTIDINV-FOUND                 VALUE 'Y'.                     
015300*                                                                         
015400     05  STK-IND-VALID-SW       PIC X.                                    
015500         88  STOCKING-IND-VALID            VALUE 'Y'.                     
015600         88  STOCKING-IND-INVALID          VALUE 'N'.                     
015700*                                                                         
015800     05  CONTINUE-SW            PIC X.                                    
015900         88 CONTINUE-YES                   VALUE 'Y'.                     
016000         88 CONTINUE-NO                    VALUE 'N'.                     
016100*                                                                         
016200     05  ERROR-SW               PIC X.                                    
016300********************************************                              
016400*     IMS ABEND-AREA DATA ELEMENTS         *                              
016500********************************************                              
016600 01  IMS-ABEND-AREA.                                                      
016700     05  E2309-MODULE-NAME            PIC X(8)   VALUE 'POM915  '.        
016800     05  IMS-ABEND-MESSAGE-AREA.                                          
016900         10  IMS-ABEND-PP             PIC X(30)  VALUE SPACES.            
017000         10  FILLER                   PIC X(18)  VALUE                    
017100             '  STATUS CODE ==> '.                                        
017200         10  IMS-ABEND-CD             PIC X(2)   VALUE SPACES.            
017300         10  FILLER                   PIC X(19)  VALUE                    
017400             ' CALL FUNCTION ==> '.                                       
017500         10  IMS-ABEND-FUNC           PIC X(4)   VALUE SPACES.            
017600         10  FILLER                   PIC X(15)  VALUE                    
017700             ' ERROR MSG ==> '.                                           
017800         10  IMS-ABEND-MSG            PIC X(40)  VALUE SPACES.            
017900     05  IMSABEND-USER-CODE           PIC 9(6)   COMP VALUE 4095.         
018000******************************************                                
018100*        IMS CALL FUNCTIONS              *                                
018200******************************************                                
018300 01  IMS-FUNCTIONS.                                                       
018400     05  ISRT                   PIC X(04)  VALUE 'ISRT'.                  
018500     05  GU                     PIC X(04)  VALUE 'GU  '.                  
018600     05  GN                     PIC X(04)  VALUE 'GN  '.                  
018700     05  GNP                    PIC X(04)  VALUE 'GNP '.                  
018800**************************************************************            
018900*          SUBROUTINE WORKING STORAGE ITEMS                  *            
019000**************************************************************            
019100 01  CALL-OPTIONS.                                                        
019200     05 CALL-PARA                     PIC X(05).                          
019300     05 VALID-REQUEST-IND             PIC X(01).                          
019400        88 VALID-REQUEST          VALUE '0'.                              
019500        88 INVALID-REQUEST        VALUE '2'.                              
019600                                                                          
019700 01  MISC-FIELDS-LINK                PIC X(100).                          
019800                                                                          
019900 01  TABLE-LAYOUT-AREA               PIC X(500).                          
020000 01  PMOPER-AREA       REDEFINES TABLE-LAYOUT-AREA.                       
020100     05 PMOPERL-REC.                                                      
020200        COPY YPMOPERL.                                                    
020300 01  PMWINV-AREA       REDEFINES TABLE-LAYOUT-AREA.                       
020400     05 PMWINVL-REC.                                                      
020500        COPY YPMWINVL.                                                    
020600 01  PLNSHP-AREA       REDEFINES TABLE-LAYOUT-AREA.                       
020700     05 PLNSHPL-REC.                                                      
020800        COPY YPLNSHPL.                                                    
020900 01  MKTDMD-AREA       REDEFINES TABLE-LAYOUT-AREA.                       
021000     05 MKTDMDL-REC.                                                      
021100        COPY YMKTDMDL.                                                    
021200                                                                          
021300        COPY SQLCA.                                                       
021400*****************************************************                     
021500*             INPUT MESSAGE AREA                    *                     
021600*****************************************************                     
021700 01  PI9805I.                                                             
021800     05  E2325-LL            PIC  S9(04)      COMP.                       
021900     05  E2326-ZZ            PIC  S9(04)      COMP.                       
022000     05  E72-TRAN-CD         PIC  X(8).                                   
022100     05  E75-SCREEN-ID       PIC  X(2).                                   
022200     05  FILLER              PIC  X(3).                                   
022300     05  E1913-USER-ID       PIC  X(4).                                   
022400     05  E1914-USER-PW       PIC  X(8).                                   
022500     05  E1-PART-NUM.                                                     
022600         10  E1-PART-NUM-A   PIC  X(05).                                  
022700         10  E1-PART-NUM-B   PIC  X(05).                                  
022800         10  E1-PART-NUM-C   PIC  X(05).                                  
022900                                                                          
023000 EJECT                                                                    
023100*****************************************************************         
023200*        OUTPUT MESSAGE AREA (DETAIL SCREEN)    LL=+363       *           
023300*****************************************************************         
023400 01  PI9805O.                                                             
023500     05  E2325-LL                       PIC S9(04)    COMP.               
023600     05  E2326-ZZ                       PIC S9(04)    COMP.               
023700     05  E2283-SYS-MSG                  PIC X(79).                        
023800     05  E1913-USER-ID                  PIC X(04).                        
023900     05  E1914-USER-PW                  PIC X(08).                        
024000     05  PART-NUM-A-ATTR                PIC X(02).                        
024100     05  PART-NUM-A                     PIC X(05).                        
024200     05  PART-NUM-B-ATTR                PIC X(02).                        
024300     05  PART-NUM-B                     PIC X(05).                        
024400     05  PART-NUM-C-ATTR                PIC X(02).                        
024500     05  PART-NUM-C                     PIC X(05).                        
024600     05  TEMP                           PIC X(04).                        
024700     05  EDIT-PART-NUM                  PIC X(21).                        
024800     05  E37-PN-DESC                    PIC X(20).                        
024900     05  DETAIL-TABLE.                                                    
025000         10  OUTPUT-TABLE      OCCURS 5 TIMES                     170607KC
025100                               INDEXED BY INDX-OUT1.                      
025200             15  OUTPUT-LINE            PIC X(68).                000317HJ
025300     05  TOTAL-TABLE.                                                     
025400         10  OUTPUT-TOTAL      OCCURS 3 TIMES                             
025500                               INDEXED BY INDX-OUT2.                      
025600             15  TOTAL-LINE             PIC X(73).                000317HJ
025700                                                                          
025800************************************************************              
025900*                   OUTPUT-DETAIL AREA                     *              
026000************************************************************              
026100 01  OUTPUT-DETAIL.                                                       
026200     05  WH-NAME                     PIC X(01).                           
026300     05  FILLER                      PIC X(03).                           
026400     05  E360-CURR-QTY-ON-HAND       PIC Z(7)-.                           
026500     05  FILLER                      PIC X(03).                           
026600     05  E000-CURR-QTY-IN-XFER       PIC Z(7).                    000317HJ
026700     05  FILLER                      PIC X(03).                   000317HJ
026800     05  E361-CURR-QTY-ON-ORD        PIC Z(7).                            
026900     05  FILLER                      PIC X(03).                   000317HJ
027000     05  E000-CURR-QTY-OH-XDOCK      PIC Z(7).                    000317HJ
027100     05  FILLER                      PIC X(02).                   031400HJ
027200     05  E459-CURR-QTY-BO            PIC Z(7).                            
027300     05  FILLER                      PIC X(02).                           
027400     05  E930-AVG-DMD                PIC Z(5)9.9.                         
027500     05  FILLER                      PIC X(02).                           
027600     05  E2705-MTHS-SUP              PIC ZZ9.9.                           
027700                                                                          
027800*************************************************************             
027900*             OUTPUT-TOTAL DETAIL AREA                      *             
028000*************************************************************             
028100 01  OUTPUT-TOTAL-DETAIL.                                                 
028200     05  TOTAL-NAME                  PIC X(06).                           
028300     05  FILLER                      PIC X(03) VALUE SPACES.              
028400     05  E360-CURR-QTY-ON-HAND       PIC Z(07)-.                          
028500     05  FILLER                      PIC X(03) VALUE SPACES.              
028600     05  E000-CURR-QTY-IN-XFER       PIC Z(7).                    000317HJ
028700     05  FILLER                      PIC X(03) VALUE SPACES.      000317HJ
028800     05  E361-CURR-QTY-ON-ORD        PIC Z(07).                           
028900     05  FILLER                      PIC X(03) VALUE SPACES.      000317HJ
029000     05  E000-CURR-QTY-OH-XDOCK      PIC Z(7).                    000317HJ
029100     05  FILLER                      PIC X(02) VALUE SPACES.      031400HJ
029200     05  E459-CURR-QTY-BO            PIC Z(07).                           
029300     05  FILLER                      PIC X(02) VALUE SPACES.              
029400     05  E930-AVG-DMD                PIC Z(5)9.9.                         
029500     05  FILLER                      PIC X(02) VALUE SPACES.              
029600     05  E2705-MTHS-SUP              PIC ZZ9.9.                           
029700                                                                          
029800 01  GRAND-TOTAL-DETAIL.                                                  
029900     05  TOTAL-NAME-G                PIC X(06).                           
030000     05  FILLER                      PIC X(03) VALUE SPACES.              
030100     05  E360-CURR-QTY-ON-HAND-G     PIC Z(07)-.                          
030200     05  FILLER                      PIC X(03) VALUE SPACES.              
030300     05  E000-CURR-QTY-IN-XFER-G     PIC Z(7).                    000317HJ
030400     05  FILLER                      PIC X(03) VALUE SPACES.      000317HJ
030500     05  E361-CURR-QTY-ON-ORD-G      PIC Z(07).                           
030600     05  FILLER                      PIC X(03) VALUE SPACES.      000317HJ
030700     05  E000-CURR-QTY-OH-XDOCK-G    PIC Z(7).                    000317HJ
030800     05  FILLER                      PIC X(02) VALUE SPACES.      031400HJ
030900     05  E459-CURR-QTY-BO-G          PIC Z(07).                           
031000     05  FILLER                      PIC X(02) VALUE SPACES.              
031100     05  E930-AVG-DMD-G              PIC Z(5)9.9.                         
031200     05  FILLER                      PIC X(02) VALUE SPACES.              
031300     05  E2705-MTHS-SUP-G            PIC ZZ9.9.                           
031400                                                                          
031500 EJECT                                                                    
031600*****************************************************************         
031700***                  PART DATA BASE                           ***         
031800*****************************************************************         
031900 01  PTROOT.                    COPY PTROOT.                              
032000                                                                          
032100 01  PTDEPOT.                   COPY PTDEPOT.                             
032200                                                                          
032300 01  PTINV.                     COPY PTINV.                               
032400                                                                          
032500 01  PTIDINV.                   COPY PTIDINV.                             
032600                                                                          
032700*****************************************************************         
032800***                SECURITY DATA BASE                         ***         
032900*****************************************************************         
033000 01  SCROOT.                    COPY SCROOT.                              
033100                                                      EJECT               
033110*****************************************************************         
033120***      PART CENTER HARD CODE REMOVAL CHANGES                *** 170607KC
033130***************************************************************** 170607KC
033140 COPY PVALPC.                                                     170607KC
033150                                                                  170607KC
033200***************************************************************** 170607KC
033300***          ATTRIBUTE BYTE TABLE DEFINITION                  ***         
033400*****************************************************************         
033500 01  ATTRTBL.                                                             
033600                             COPY ATTRTBL.                                
033700                                                                          
033800*****************************************************************         
033900***                      SSA'S                                ***         
034000*****************************************************************         
034100 01  ZPTROOT.                    COPY ZPTROOT.                            
034200                                                                          
034300 01  ZPTDEPOT.                   COPY ZPTDEPOT.                           
034400                                                                          
034500 01  ZPTINV.                     COPY ZPTINV.                             
034600                                                                          
034700 01  ZPTIDINV.                   COPY ZPTIDINV.                           
034800                                                                          
034900 01  ZSCROOT-KEY-VALUE.                                                   
035000     05  TRANS-KEY               PIC X(8).                                
035100     05  USER-ID-KEY             PIC X(4).                                
035200                                                                          
035300     EJECT                                                                
035400*****************************************************************         
035500***                 ERROR MESSAGES                            ***         
035600*****************************************************************         
035700 01  PI9805-MESSAGES.                                                     
035800     05  PI9805-MSG-001      PIC  X(79)  VALUE                            
035900****** APSSOX KEANE CHANGES1 START HERE **********************            
036000*APSSOX** 'INVALID ID OR PASSWORD. PLEASE RE-ENTER'.                      
036100         'INVALID ID. PLEASE RE-ENTER'.                                   
036200****** APSSOX KEANE CHANGES1 ENDS HERE **********************             
036300     05  PI9805-MSG-002      PIC  X(79)  VALUE                            
036400        'PART NOT FOUND. PLEASE RE-ENTER '.                               
036500     05  PI9805-MSG-003      PIC  X(79)  VALUE                            
036600        'INVALID STOCKING INDICATOR. PLEASE RE-ENTER PART'.               
036700                                                                          
036800 01  FILLER                     PIC X(32)  VALUE                          
036900     'POM906 WORKING STORAGE ENDS HERE'.                                  
037000                                                                          
037100 EJECT                                                                    
037200*****************************************************************         
037300***                 LINKAGE SECTION                            **         
037400*****************************************************************         
037500 LINKAGE SECTION.                                                         
037600                                                                          
037700****** APSSOX KEANE CHANGES2 START HERE **********************            
037800*APSSOX** 01  IO-PCB.                    COPY IOPCB.                      
037900 01  IO-PCB.                    COPY IOPCBN.                              
038000****** APSSOX KEANE CHANGES2 ENDS  HERE **********************            
038100                                                                          
038200 01  SC-PCB.                    COPY DBPCB.                               
038300                                                                          
038400 01  PPT-PCB.                   COPY DBPCB.                               
038500                                                                          
038600*****************************************************************         
038700***                 PROCEDURE DIVISION                         **         
038800*****************************************************************         
038900                                                                          
039000 PROCEDURE DIVISION    USING  IO-PCB                                      
039100                              SC-PCB                                      
039200                              PPT-PCB.                                    
039300************************************************************              
039400*                      MAIN LOGIC                          *              
039500************************************************************              
039600 0000-MAIN-LINE.                                                          
039700                                                                          
039800      PERFORM 0100-GET-INPUT-MSG.                                         
039900      IF NOT SECURITY-VIOLATION                                           
040000         PERFORM 0200-INITIALIZE                                          
040100         PERFORM 0300-TMD-TOTAL-DATA                                      
040200         PERFORM 0400-GET-TPNA-DATA                                       
040300      END-IF.                                                             
040400                                                                          
040500      PERFORM 0900-FORMAT-SYSTEM-MESSAGE.                                 
040600      PERFORM 1000-SEND-OUTPUT-MESSAGE.                                   
040700      GOBACK.                                                             
040800*****************************************************************         
040900***              RECEIVE INPUT MESSAGE                         **         
041000*****************************************************************         
041100 0100-GET-INPUT-MSG.                                                      
041200                                                                          
041300     PERFORM 7100-IO-PI9805I.                                             
041400                                                                          
041500     IF  E1864-PCB-STA-CD OF IO-PCB EQUAL SPACES                          
041600         PERFORM 0150-SECURITY-CHECK                                      
041700     ELSE                                                                 
041800         MOVE  '7100-IO-PI9805I               '  TO IMS-ABEND-PP          
041900         MOVE  E1864-PCB-STA-CD OF IO-PCB        TO IMS-ABEND-CD          
042000         MOVE  GU                                TO IMS-ABEND-FUNC        
042100         MOVE  'INVALID RTN CODE FROM INPUT MSG' TO IMS-ABEND-MSG         
042200         PERFORM 9999-CALL-IMSABEND                                       
042300     END-IF.                                                              
042400                                                                          
042500 EJECT                                                                    
042600                                                                          
042700************************************************************              
042800***                CHECK SECURITY                        ***              
042900************************************************************              
043000 0150-SECURITY-CHECK.                                                     
043100                                                                          
043200     MOVE  SPACES TO PI9805O.                                             
043300     MOVE SPACES TO WS-INVALID-RSN-CD.                                    
043400****** APSSOX KEANE CHANGES3 START HERE **********************            
043500*APSSOX**   MOVE E1913-USER-ID OF PI9805I   TO USER-ID-KEY                
043600     MOVE E1896-PCB-USER-ID1 OF IO-PCB TO USER-ID-KEY                     
043700****** APSSOX KEANE CHANGES3 ENDS HERE **********************             
043800                                     OF ZSCROOT-KEY-VALUE                 
043900                                     E1913-USER-ID                        
044000                                     OF PI9805O.                          
044100     MOVE E72-TRAN-CD OF PI9805I     TO TRANS-KEY                         
044200                                     OF ZSCROOT-KEY-VALUE.                
044300     MOVE E1914-USER-PW OF PI9805I   TO E1914-USER-PW                     
044400                                     OF PI9805O.                          
044500     MOVE E1-PART-NUM-A OF PI9805I   TO PART-NUM-A                        
044600                                     OF PI9805O.                          
044700     MOVE E1-PART-NUM-B OF PI9805I   TO PART-NUM-B                        
044800                                     OF PI9805O.                          
044900     MOVE E1-PART-NUM-C OF PI9805I   TO PART-NUM-C                        
045000                                     OF PI9805O.                          
045100     MOVE E1-PART-NUM-A OF PI9805I   TO WS-PART-NUM2-1-5.                 
045200     MOVE E1-PART-NUM-B OF PI9805I   TO WS-PART-NUM2-6-10.                
045300     MOVE E1-PART-NUM-C OF PI9805I   TO WS-PART-NUM2-11-15.               
045400     MOVE WS-PART-NUM2               TO EDIT-PART-NUM                     
045500                                     OF PI9805O.                          
045510     MOVE LENGTH OF PI9805O          TO E2325-LL OF PI9805O.      170607KC
045702     MOVE ZEROS TO E2326-ZZ OF PI9805O.                                   
045800                                                                          
045900****** APSSOX KEANE CHANGES4 START HERE **********************            
046000                                                                          
046100*APSSOX**     CALL 'PIC913' USING   SC-PCB                                
046200*APSSOX**                           ZSCROOT-KEY-VALUE                     
046300*APSSOX**                           SCROOT                                
046400*APSSOX**                           WS-INVALID-RSN-CD                     
046500*APSSOX**                           E1914-USER-PW OF PI9805I.             
046600                                                                          
046700     CALL 'PIC950' USING   SC-PCB                                         
046800                           ZSCROOT-KEY-VALUE                              
046900                           SCROOT                                         
047000                           WS-INVALID-RSN-CD                              
047100                           E1914-USER-PW OF PI9805I                       
047200                           IO-PCB.                                        
047300                                                                          
047400****** APSSOX KEANE CHANGES4 ENDS HERE **********************             
047500                                                                          
047600     IF  WS-INVALID-RSN-CD EQUAL SPACES                                   
047700          MOVE 'N' TO SECURITY-ERROR-SW                                   
047800     ELSE                                                                 
047900          MOVE 1                TO SYS-MSG-SW                             
048000*         MOVE  PI9805-MSG-001  TO E2283-SYS-MSG OF PI9805O               
048100          MOVE 'Y' TO SECURITY-ERROR-SW                                   
048200     END-IF.                                                              
048300                                                                          
048400 EJECT                                                                    
048500                                                                          
048600*********************************************************                 
048700*                INITIALIZATION LOGIC                   *                 
048800*********************************************************                 
048900 0200-INITIALIZE.                                                         
049000                                                                          
049100       PERFORM 0220-INITIALIZE-DETAIL-AREAS.                              
049200       PERFORM 0230-INITIALIZE-INDEX.                                     
049300       PERFORM 0240-INITIALIZE-WORK-AREAS.                                
049400       PERFORM 0250-GET-LATEST-DMD-MO.                                    
049500                                                                          
049600***********************************************************               
049700*         INITIALIZE DETAIL DATA AREA ON THE SCREEN,      *               
049800*         THE TOTAL ACCUMULATION TABLES AND THE GRAND     *               
049900*         TOTAL AREA                                      *               
050000***********************************************************               
050100 0220-INITIALIZE-DETAIL-AREAS.                                            
050200                                                                          
050300      PERFORM 0222-DETAIL VARYING INDX-OUT1 FROM 1 BY 1                   
050400              UNTIL INDX-OUT1 > 5.                                170607KC
050500                                                                          
050600      PERFORM 0224-TOTAL                                                  
050700              VARYING INDX-OUT2 FROM 1 BY 1    UNTIL                      
050800              INDX-OUT2 > 3.                                              
050900                                                                          
051000 0222-DETAIL.                                                             
051100                                                                          
051200*      MOVE LOW-VALUES           TO OUTPUT-DETAIL.                        
051300*      IF INDX-OUT1 = 1                                                   
051400*         MOVE WS-WH-O           TO WH-NAME                               
051500*                                OF OUTPUT-DETAIL                         
051600*      ELSE                                                               
051700*         MOVE WS-WH-T           TO WH-NAME                               
051800*                                OF OUTPUT-DETAIL                         
051900*      END-IF.                                                            
052000*                                                                         
052100       MOVE LOW-VALUES           TO OUTPUT-LINE(INDX-OUT1).               
052200*      MOVE OUTPUT-DETAIL        TO OUTPUT-LINE(INDX-OUT1).               
052300                                                                          
052400 0224-TOTAL.                                                              
052500                                                                          
052600*      MOVE LOW-VALUES           TO OUTPUT-TOTAL-DETAIL.                  
052700*      IF INDX-OUT2 = 1                                                   
052800*         MOVE ' TPNA:'           TO TOTAL-NAME                           
052900*                                 OF OUTPUT-TOTAL-DETAIL                  
053000*      ELSE                                                               
053100*         IF INDX-OUT2 = 2                                                
053200*            MOVE '  TMD:'           TO TOTAL-NAME                        
053300*                                    OF OUTPUT-TOTAL-DETAIL               
053400*         ELSE                                                            
053500*            MOVE 'TOTAL:'          TO TOTAL-NAME                         
053600*                                   OF OUTPUT-TOTAL-DETAIL                
053700*         END-IF                                                          
053800*      END-IF.                                                            
053900       MOVE LOW-VALUES             TO TOTAL-LINE(INDX-OUT2).              
054000*      MOVE OUTPUT-TOTAL-DETAIL    TO TOTAL-LINE(INDX-OUT2).              
054100                                                                          
054200****************************************************************          
054300*  INITIALIZE INDICES FOR DETAIL LINE, TOTAL LINE AND          *          
054400*  ACCUMULATION THE TABLES                                     *          
054500****************************************************************          
054600 0230-INITIALIZE-INDEX.                                                   
054700                                                                          
054800       SET TOT-INDX      TO 1.                                            
054900       SET INDX-OUT1     TO 1.                                            
055000       SET INDX-OUT2     TO 1.                                            
055100                                                                          
055200***************************************************************           
055300*   INITIALIZE DETAIL AREA STRUCTURE ON THE WORKING STORAGE   *           
055400***************************************************************           
055500 0240-INITIALIZE-WORK-AREAS.                                              
055600                                                                          
055700*       MOVE +0                 TO WS-DETAIL-STRUCTURE.                   
055800                                                                          
055900************************************************************              
056000*  LOGIC FOR ESTABLISHING WHAT THE LATEST MKT DMD MONTH IS *      990806EL
056100*  FOR ALL PARTS BY DOING A FETCH OF FIRST ROW.            *      990806EL
056200************************************************************      990806EL
056300 0250-GET-LATEST-DMD-MO.                                          990806EL
056400                                                                  990806EL
056500      MOVE 'F0001'              TO CALL-PARA.                     990806EL
056600      PERFORM 6400-YCC008-TMKTDMD.                                990806EL
056700                                                                  990806EL
056800      IF VALID-REQUEST                                            990806EL
056900         MOVE MKTDMDL-MKT-DMD-MO TO WS-LATEST-DMD-MO              990806EL
057000      ELSE                                                        990806EL
057100         DISPLAY '******************************************'     990806EL
057200         DISPLAY '** POM915 ABEND                         **'     990806EL
057300         DISPLAY '** PARA 0250-GET-LATEST-DMD-MO       DB2**'     990806EL
057400         DISPLAY '** INVALID RETURN CODE                  **'     990806EL
057500         DISPLAY '** SQLERRMC = ' SQLERRMC                        990806EL
057600         DISPLAY '******************************************'     990806EL
057700         PERFORM 9998-CALL-CANCEL                                 990806EL
057800      END-IF.                                                     990806EL
057900                                                                  990806EL
058000*****************************************************************         
058100*  LOGIC FOR OBTAINING TMD TOTAL FROM THE IMS PARTS DATABASE    *         
058200*****************************************************************         
058300 0300-TMD-TOTAL-DATA.                                                     
058400                                                                          
058500      PERFORM 3000-GET-PTROOT.                                            
058600      IF PART-FOUND                                                       
058700         PERFORM 3100-GET-DEPOT-PTINV UNTIL                               
058800                 NO-MORE-DEPOTS                                           
058900      END-IF.                                                             
059000                                                                          
059100*****************************************************************         
059200* LOGIC FOR OBTAINING TPNA DETAIL AND TOTAL FROM DB2 TABLES     *         
059300*****************************************************************         
059400 0400-GET-TPNA-DATA.                                                      
059500                                                                          
059600        PERFORM 4000-GET-DATA-FROM-DB2.                                   
059700                                                                          
059800 0900-FORMAT-SYSTEM-MESSAGE.                                              
059900*                                                                         
060000      EVALUATE SYS-MSG-SW                                                 
060100           WHEN 1                                                         
060200                MOVE PI9805-MSG-001     TO E2283-SYS-MSG                  
060300                                        OF PI9805O                        
060400           WHEN 2                                                         
060500                MOVE PI9805-MSG-002     TO E2283-SYS-MSG                  
060600                                        OF PI9805O                        
060700                PERFORM 0910-HILITE-PART-NUM                              
060800           WHEN 3                                                         
060900                MOVE PI9805-MSG-003     TO E2283-SYS-MSG                  
061000                                        OF PI9805O                        
061100                PERFORM 0910-HILITE-PART-NUM                              
061200      END-EVALUATE.                                                       
061300*                                                                         
061400 0910-HILITE-PART-NUM.                                                    
061500*                                                                         
061600      MOVE RED-HEX-89                TO                                   
061700           PART-NUM-A-ATTR           OF PI9805O                           
061800           PART-NUM-B-ATTR           OF PI9805O                           
061900           PART-NUM-C-ATTR           OF PI9805O.                          
062000*                                                                         
062100*********************************************************                 
062200*              SEND FINAL OUTPUT MESSAGE              *                   
062300*********************************************************                 
062400 1000-SEND-OUTPUT-MESSAGE.                                                
062500                                                                          
062600     PERFORM 7500-IO-PI9805O.                                             
062700                                                                          
062800     IF E1864-PCB-STA-CD OF IO-PCB NOT = SPACE                            
062900        MOVE  'ISRT CALL FAILED FOR MOD PI9805O' TO IMS-ABEND-MSG         
063000        MOVE  '                             '    TO IMS-ABEND-PP          
063100        MOVE  E1864-PCB-STA-CD OF IO-PCB         TO IMS-ABEND-CD          
063200        PERFORM  9999-CALL-IMSABEND                                       
063300     END-IF.                                                              
063400                                                                          
063500**************************************************************            
063600*      EXTRACT THE ROOT SEGMENT OF IMS PARTS DATABASE        *            
063700**************************************************************            
063800 3000-GET-PTROOT.                                                         
063900                                                                          
064000     MOVE  E1-PART-NUM OF PI9805I   TO  ZPTROOT-VALUE.                    
064100     MOVE  '('                      TO  ZPTROOT-BEGIN.                    
064200                                                                          
064300     PERFORM 7200-GU-PTROOT.                                              
064400                                                                          
064500     IF  E1864-PCB-STA-CD OF PPT-PCB = SPACES                             
064600         MOVE 'Y'  TO  PART-STATUS                                        
064700         MOVE E37-PN-DESC OF PTROOT TO E37-PN-DESC                        
064800                                    OF PI9805O                            
064900     ELSE                                                                 
065000         IF  E1864-PCB-STA-CD OF PPT-PCB EQUAL 'GE' OR  'GB'              
065100             MOVE 'N'  TO  PART-STATUS                                    
065200         ELSE                                                             
065300             MOVE  'INVALID RTN CODE GU, PTROOT' TO IMS-ABEND-MSG         
065400             MOVE  '7200-GU-PTROOT'              TO IMS-ABEND-PP          
065500             MOVE  E1864-PCB-STA-CD OF PPT-PCB   TO IMS-ABEND-CD          
065600             PERFORM  9999-CALL-IMSABEND                                  
065700         END-IF                                                           
065800     END-IF.                                                              
065900                                                                          
066000*****************************************************************         
066100*         LOGIC FOR GETTING DEPOT AND INVENTORY DATA            *         
066200*****************************************************************         
066300 3100-GET-DEPOT-PTINV.                                                    
066400                                                                          
066500     PERFORM 3150-GET-DEPOT-DATA.                                         
066600     IF DEPOT-FOUND                                                       
066700        PERFORM 3160-GET-INVENTORY-DATA                                   
066800     ELSE                                                                 
066900        MOVE 'N'              TO DEPOT-STATUS                             
067000     END-IF.                                                              
067100                                                                          
067200****************************************************************          
067300*        EXTRACT DEPOT DATA FOR THE PARTS ROOT SEGMENT         *          
067400****************************************************************          
067500 3150-GET-DEPOT-DATA.                                                     
067600                                                                          
067700     MOVE SPACES                 TO DEPOT-STATUS.                         
067800     MOVE  SPACES                TO ZPTDEPOT-BEGIN.                       
067900     CALL 'CBLTDLI' USING GN                                              
068000                          PPT-PCB                                         
068100                          PTDEPOT                                         
068200                          ZPTROOT                                         
068300                          ZPTDEPOT.                                       
068400                                                                          
068500                                                                          
068600     IF  E1864-PCB-STA-CD OF PPT-PCB = SPACES                             
068700         MOVE 'Y'           TO DEPOT-STATUS                               
068800     ELSE                                                                 
068900         IF  E1864-PCB-STA-CD OF PPT-PCB EQUAL 'GE' OR  'GB'              
069000             MOVE 'N'       TO  DEPOT-STATUS                              
069100         ELSE                                                             
069200             MOVE 'INVALID RTN CODE GNP PTDEPOT'  TO IMS-ABEND-MSG        
069300             MOVE '3150-GET-PTDEPOT'              TO IMS-ABEND-PP         
069400             MOVE  E1864-PCB-STA-CD OF PPT-PCB    TO IMS-ABEND-CD         
069500             PERFORM  9999-CALL-IMSABEND                                  
069600         END-IF                                                           
069700     END-IF.                                                              
069800                                                                          
069900 3160-GET-INVENTORY-DATA.                                                 
070000                                                                          
070100     MOVE  '('                     TO  ZPTDEPOT-BEGIN.                    
070200     MOVE  E2-DEPOT-CD OF PTDEPOT  TO  ZPTDEPOT-VALUE.                    
070300                                                                          
070400     IF E2-DEPOT-CD OF PTDEPOT     < '50'                                 
070500        MOVE SPACES              TO PTINV-STATUS                          
070600        PERFORM 3200-GET-PTINV-DATA                                       
070700     END-IF.                                                              
070800                                                                          
070900************************************************************              
071000* EXTRACT INVENTORY DATA FROM PTINV SEGMENT FOR THE PARENT                
071100* DEPOT                                                                   
071200************************************************************              
071300 3200-GET-PTINV-DATA.                                                     
071400                                                                          
071500     CALL 'CBLTDLI' USING GN                                              
071600                          PPT-PCB                                         
071700                          PTINV                                           
071800                          ZPTROOT                                         
071900                          ZPTDEPOT                                        
072000                          ZPTINV.                                         
072100                                                                          
072200                                                                          
072300     IF  E1864-PCB-STA-CD OF PPT-PCB = SPACES                             
072400         MOVE 'Y' TO PTINV-STATUS                                         
072500         PERFORM 3250-ACCUMULATE-TOTAL-PTINV                              
072600     ELSE                                                                 
072700         IF  E1864-PCB-STA-CD OF PPT-PCB EQUAL 'GE' OR  'GB'              
072800             MOVE 'N' TO  PTINV-STATUS                                    
072900         ELSE                                                             
073000             MOVE 'INVALID RTN CODE GN, PTINV'    TO IMS-ABEND-MSG        
073100             MOVE '3200-GET-PTINV'                TO IMS-ABEND-PP         
073200             MOVE  E1864-PCB-STA-CD OF PPT-PCB    TO IMS-ABEND-CD         
073300             PERFORM  9999-CALL-IMSABEND                                  
073400         END-IF                                                           
073500     END-IF.                                                              
073600                                                                          
073700                                                                          
073800************************************************************              
073900* ACCUMULATE TMD TOTAL FROM IMS PARTS DATABASE IN THE                     
074000* CORRESPONDING ACCUMULATION TABLES FOR TMD AND GRAND TOTAL               
074100************************************************************              
074200 3250-ACCUMULATE-TOTAL-PTINV.                                             
074300                                                                          
074400       ADD E360-CURR-QTY-ON-HAND OF PTINV         TO                      
074500           E360-CURR-QTY-ON-HAND OF ACCUM-TBL(2)                          
074600           E360-CURR-QTY-ON-HAND OF ACCUM-TBL(3).                         
074700                                                                          
074800       COMPUTE WS-ON-ORD-TMD  =                                           
074900          E911-CURR-AIR-ON-ORD  OF PTINV  +                               
075000          E912-CURR-SURF-ON-ORD OF PTINV.                                 
075100       ADD WS-ON-ORD-TMD                          TO                      
075200           E361-CURR-QTY-ON-ORD  OF ACCUM-TBL(2)                          
075300           E361-CURR-QTY-ON-ORD  OF ACCUM-TBL(3).                         
075400                                                                          
075500       ADD E459-CURR-QTY-BO      OF PTINV         TO                      
075600           E459-CURR-QTY-BO      OF ACCUM-TBL(2)                          
075700           E459-CURR-QTY-BO      OF ACCUM-TBL(3).                         
075800       ADD E930-AVG-DMD          OF PTINV         TO                      
075900           E930-AVG-DMD          OF ACCUM-TBL(2)                          
076000           E930-AVG-DMD          OF ACCUM-TBL(3).                         
076100                                                                          
076200*************************************************************             
076300*        LOGIC FOR EXTRACTING TPNA DATA FROM DB2            *             
076400*************************************************************             
076500 4000-GET-DATA-FROM-DB2.                                                  
076600                                                                          
076700        PERFORM 4100-INQ-TPMOPER.                                         
076800                                                                          
076810**********************************************************        170607KC
076820***      PART CENTER HARD CODE REMOVAL CHANGES           *        170607KC
076830**********************************************************        170607KC
076840                                                                  170607KC
076900        IF STOCKING-IND-VALID AND CONTINUE-YES                            
077000           PERFORM 4200-INQ-TABLES                                        
077010                   VARYING PC-INDEX FROM 1 BY 1                   170607KC
077020                   UNTIL (PC-INDEX > 5 OR                         170607KC
077030                   PCT-WH-NAME(PC-INDEX) = SPACES)                170607KC
077300           PERFORM 5000-PROCESS-TOTALS                                    
077400        END-IF.                                                           
077500                                                                          
077600**********************************************************                
077700*    LOGIC FOR PERFORMING INQUIRY ON VARIOUS TABLES      *                
077800**********************************************************                
077900 4200-INQ-TABLES.                                                         
078000                                                                          
078100           PERFORM 4300-INQ-TPMWINV.                                      
078200           PERFORM 4400-INQ-TPLNSHP.                                      
078300           PERFORM 4600-INQ-TMKTDMD.                                      
078400           PERFORM 4700-PROCESS-EXTRACTED-DATA.                           
078500                                                                          
078600*********************************************************                 
078700* INQUIRY ON TPMOPER TABLE FOR VALIDATING STOCKING      *                 
078800* INDICATOR                                             *                 
078900*********************************************************                 
079000 4100-INQ-TPMOPER.                                                        
079100                                                                          
079200        MOVE 'Y'                TO STK-IND-VALID-SW.                      
079300        MOVE 'Y'                TO CONTINUE-SW.                           
079400                                                                          
079500        MOVE E1-PART-NUM OF PI9805I                                       
079600                                TO PMOPERL-PART-NUM.                      
079700        PERFORM 4110-CALL-YCA002.                                         
079800                                                                          
079900        IF SQLCODE = +0                                                   
080000           PERFORM 4120-VALIDATE-STOCKING-IND                             
080100        ELSE                                                              
080200           IF SQLCODE = +100                                              
080300              MOVE 'N'             TO CONTINUE-SW                         
080400              MOVE 2               TO SYS-MSG-SW                          
080500*             MOVE PI9805-MSG-002  TO E2283-SYS-MSG OF PI9805O            
080600           ELSE                                                           
080700              MOVE SQLCODE             TO DS-SQLCODE                      
080800              DISPLAY '************** U0111 ABEND *************'          
080900              DISPLAY '*         SQL ERROR HAS OCCURRED       *'          
081000              DISPLAY '* PGM=POM915                           *'          
081100              DISPLAY '* PARA=6100-YCA002-S0002-TPMOPER       *'          
081200              DISPLAY '* SQLCODE = ' DS-SQLCODE                           
081300              DISPLAY '* SQLERRMC = ' SQLERRMC                            
081400              DISPLAY '****************************************'          
081500              PERFORM 9998-CALL-CANCEL                                    
081600           END-IF                                                         
081700        END-IF.                                                           
081800                                                                          
081900***********************************************************               
082000*            VALIDATE STOCKING INDICATOR                *                 
082100***********************************************************               
082200 4120-VALIDATE-STOCKING-IND.                                              
082300                                                                          
082400        IF PMOPERL-PRO-STK-IND  = 'P'                                     
082500           NEXT SENTENCE                                                  
082600        ELSE                                                              
082700           MOVE 'N'             TO STK-IND-VALID-SW                       
082800           MOVE 3               TO SYS-MSG-SW                             
082900*          MOVE PI9805-MSG-003  TO E2283-SYS-MSG OF PI9805O               
083000        END-IF.                                                           
083100                                                                          
083200*******************************************************                   
083300*       CALL DB2 MODULE FOR INQUIRY ON TPMOPER        *                   
083400*******************************************************                   
083500 4110-CALL-YCA002.                                                        
083600                                                                          
083700      MOVE 'S0002'              TO CALL-PARA.                             
083800      PERFORM 6100-YCA002-S0002-TPMOPER.                                  
083900                                                                          
084000      IF VALID-REQUEST                                                    
084100         NEXT SENTENCE                                                    
084200      ELSE                                                                
084300         DISPLAY '******************************************'             
084400         DISPLAY '** POM915 ABEND                         **'             
084500         DISPLAY '** PARA 6100-YCA002-S0002-TPMOPER    DB2**'             
084600         DISPLAY '** INVALID RETURN CODE                  **'             
084700         DISPLAY '** SQLERRMC= ' SQLERRMC                                 
084800         DISPLAY '******************************************'             
084900         PERFORM 9998-CALL-CANCEL                                         
085000      END-IF.                                                             
085100                                                                          
085200******************************************************                    
085300*             GET OH-STK FROM TPMWINV                *                    
085400******************************************************                    
085500 4300-INQ-TPMWINV.                                                        
085600                                                                          
085700      MOVE E1-PART-NUM OF PI9805I                                         
085800                                TO PMWINVL-PART-NUM.                      
085810***************************************************************** 170607KC
085820***      PART CENTER HARD CODE REMOVAL CHANGES                *** 170607KC
085830***************************************************************** 170607KC
085840      MOVE PCT-WH(PC-INDEX)        TO PMWINVL-WH.                 170607KC
086400                                                                          
086500      PERFORM 4310-CALL-YCA004.                                           
086600      IF SQLCODE = +0                                                     
086700         MOVE PMWINVL-OH-STK        TO WS-OH-STK                          
086800         MOVE PMWINVL-IN-XFER-QTY   TO WS-IN-XFER                 000317HJ
086900         MOVE PMWINVL-OH-XDOCK      TO WS-OH-XDOCK                000317HJ
087000      ELSE                                                                
087100         MOVE ZERO                  TO WS-OH-STK                  060213OS
087200         MOVE ZERO                  TO WS-IN-XFER                 060213OS
087300         MOVE ZERO                  TO WS-OH-XDOCK                060213OS
087400          IF SQLCODE NOT = +100                                           
087500             MOVE SQLCODE             TO DS-SQLCODE                       
087600             DISPLAY '************** U0111 ABEND *************'           
087700             DISPLAY '*         SQL ERROR HAS OCCURRED       *'           
087800             DISPLAY '* PGM=POM915                           *'           
087900             DISPLAY '* PARA=6200-YCA004-S0003-TPMWINV       *'           
088000             DISPLAY '* SQLCODE = ' DS-SQLCODE                            
088100             DISPLAY '* SQLERRMC = ' SQLERRMC                             
088200             DISPLAY '****************************************'           
088300             PERFORM 9998-CALL-CANCEL                                     
088400          END-IF                                                          
088500      END-IF.                                                             
088600                                                                          
088700**********************************************************                
088800*        CALL DB2 MODULE FOR INQUIRY ON TPMWINV       *                   
088900**********************************************************                
089000 4310-CALL-YCA004.                                                        
089100                                                                          
089200      MOVE 'S0001'              TO CALL-PARA.                     000317HJ
089300      PERFORM 6200-YCA004-S0003-TPMWINV.                                  
089400                                                                          
089500      IF VALID-REQUEST                                                    
089600         NEXT SENTENCE                                                    
089700      ELSE                                                                
089800         DISPLAY '******************************************'             
089900         DISPLAY '** POM915 ABEND                         **'             
090000         DISPLAY '** PARA 6200-YCA004-S0003-TPMWINV    DB2**'             
090100         DISPLAY '** INVALID RETURN CODE                  **'             
090200         DISPLAY '** SQLERRMC = ' SQLERRMC                                
090300         DISPLAY '******************************************'             
090400         PERFORM 9998-CALL-CANCEL                                         
090500      END-IF.                                                             
090600                                                                          
090700**********************************************************                
090800*      GET ON-ORDER AND BACK-ORDER FROM TPLNSHP          *                
090900**********************************************************                
091000 4400-INQ-TPLNSHP.                                                        
091100                                                                          
091200        INITIALIZE PLNSHPL-REC.                                           
091300                                                                          
091310***************************************************************** 170607KC
091320***      PART CENTER HARD CODE REMOVAL CHANGES                *** 170607KC
091330***************************************************************** 170607KC
091340        MOVE PCT-DISTFD(PC-INDEX)     TO PLNSHPL-DISTFD.          170607KC
091350        MOVE PCT-WH(PC-INDEX)         TO PLNSHPL-SHP-WH.          170607KC
092100                                                                          
092200        MOVE E1-PART-NUM OF PI9805I   TO PLNSHPL-PROCESS-PART.            
092300                                                                          
092400        PERFORM 4410-GET-ON-ORDER.                                        
092500        MOVE ZERO                     TO PLNSHPL-OUTSTD-QTY.              
092600        PERFORM 4430-GET-BACK-ORDER.                                      
092700                                                                          
092800************************************************************              
092900*                   GET ON-ORDER                           *              
093000************************************************************              
093100 4410-GET-ON-ORDER.                                                       
093200                                                                          
093300       MOVE 'S0007'                   TO CALL-PARA.                       
093400                                                                          
093500       PERFORM 4450-CALL-YCB007.                                          
093600                                                                          
093700       IF SQLCODE = +0                                                    
093800          MOVE PLNSHPL-OUTSTD-QTY           TO WS-ON-ORDER                
093900       ELSE                                                               
094000          IF SQLCODE NOT = +100                                           
094100             MOVE SQLCODE             TO DS-SQLCODE                       
094200             DISPLAY '************** U0111 ABEND *************'           
094300             DISPLAY '*         SQL ERROR HAS OCCURRED       *'           
094400             DISPLAY '* PGM=POM915                           *'           
094500             DISPLAY '* PARA=6300-YCB007-TPLNSHP,ON-ORD      *'           
094600             DISPLAY '* SQLCODE = ' DS-SQLCODE                            
094700             DISPLAY '* SQLERRMC = ' SQLERRMC                             
094800             DISPLAY '****************************************'           
094900             PERFORM 9998-CALL-CANCEL                                     
095000          END-IF                                                          
095100       END-IF.                                                            
095200                                                                          
095300*************************************************************             
095400*                     GET BACK ORDER                        *             
095500*************************************************************             
095600 4430-GET-BACK-ORDER.                                                     
095700                                                                          
095800       MOVE 'S0008'                     TO CALL-PARA.                     
095900                                                                          
096000       MOVE WS-LIN-TYPE                 TO PLNSHPL-LIN-TYP.               
096100       MOVE WS-LIN-STAT                 TO PLNSHPL-LIN-STA.               
096200       PERFORM 4450-CALL-YCB007.                                          
096300                                                                          
096400       IF SQLCODE = +0                                                    
096500          MOVE PLNSHPL-OUTSTD-QTY       TO WS-BACK-ORDER                  
096600       ELSE                                                               
096700          IF SQLCODE NOT = +100                                           
096800             MOVE SQLCODE             TO DS-SQLCODE                       
096900             DISPLAY '************** U0111 ABEND *************'           
097000             DISPLAY '*         SQL ERROR HAS OCCURRED       *'           
097100             DISPLAY '* PGM=POM915                           *'           
097200             DISPLAY '* PARA=6300-YCB007-TPLNSHP,BO          *'           
097300             DISPLAY '* SQLCODE = ' DS-SQLCODE                            
097400             DISPLAY '* SQLERRMC = ' SQLERRMC                             
097500             DISPLAY '****************************************'           
097600             PERFORM 9998-CALL-CANCEL                                     
097700          END-IF                                                          
097800       END-IF.                                                            
097900                                                                          
098000*******************************************************                   
098100*      CALL DB2 MODULE FOR INQUIRY ON TPLNSHP         *                   
098200*******************************************************                   
098300 4450-CALL-YCB007.                                                        
098400                                                                          
098500      PERFORM 6300-YCB007-TPLNSHP.                                        
098600                                                                          
098700      IF VALID-REQUEST                                                    
098800         NEXT SENTENCE                                                    
098900      ELSE                                                                
099000         DISPLAY '******************************************'             
099100         DISPLAY '** POM915 ABEND                         **'             
099200         DISPLAY '** PARA 6300-YCB007-TPLNSHP          DB2**'             
099300         DISPLAY '** INVALID RETURN CODE                  **'             
099400         DISPLAY '** SQLERRMC= ' SQLERRMC                                 
099500         DISPLAY '******************************************'             
099600         PERFORM 9998-CALL-CANCEL                                         
099700      END-IF.                                                             
099800                                                                          
099900***********************************************************               
100000*       GET AVERAGE-DEMAND FROM THE TMKTDMD TABLE         *               
100100***********************************************************               
100200 4600-INQ-TMKTDMD.                                                        
100300                                                                          
100400      MOVE E1-PART-NUM OF PI9805I     TO MKTDMDL-PART-NUM.                
100500      MOVE WS-MKT-P                   TO MKTDMDL-MKT.                     
100600                                                                          
100610***************************************************************** 170607KC
100620***      PART CENTER HARD CODE REMOVAL CHANGES                *** 170607KC
100630***************************************************************** 170607KC
100640      MOVE PCT-WH(PC-INDEX)           TO MKTDMDL-WH.              170607KC
101200                                                                          
101300      PERFORM 4650-GET-AVG-DMD                                            
101400              VARYING WS-MONTH-COUNTER FROM 1 BY 1  UNTIL                 
101500              WS-MONTH-COUNTER > 3.                                       
101600                                                                          
101700************************************************************              
101800*  LOGIC FOR GETTING AVERAGE DEMAND FOR THE LAST 3 MONTHS  *              
101900************************************************************              
102000 4650-GET-AVG-DMD.                                                        
102100                                                                          
102200       PERFORM 4900-DERIVE-MONTH.                                         
102300       PERFORM 4950-CALL-YCC008.                                          
102400       IF SQLCODE = +0                                                    
102500          ADD MKTDMDL-MKT-DMD-FCST      TO WS-TOTAL-DMD-FCST              
102600       ELSE                                                               
102700          IF SQLCODE NOT = +100                                           
102800             MOVE SQLCODE             TO DS-SQLCODE                       
102900             DISPLAY '************** U0111 ABEND *************'           
103000             DISPLAY '*         SQL ERROR HAS OCCURRED       *'           
103100             DISPLAY '* PGM=POM915                           *'           
103200             DISPLAY '* PARA=4650-GET-AVG-DMD             DB2*'   990806EL
103300             DISPLAY '* SQLCODE = ' DS-SQLCODE                            
103400             DISPLAY '* SQLERRMC = ' SQLERRMC                             
103500             DISPLAY '****************************************'           
103600             PERFORM 9998-CALL-CANCEL                                     
103700          END-IF                                                          
103800       END-IF.                                                            
103900                                                                          
104000**********************************************************                
104100* LOGIC FOR PERFORMING COMPUTATIONS, FORMATTING DATA     *                
104200* EXTRACTED FROM DB2                                     *                
104300**********************************************************                
104400 4700-PROCESS-EXTRACTED-DATA.                                             
104500                                                                          
104600      PERFORM 4710-COMPUTATIONS.                                          
104700      PERFORM 4720-ACCUMULATE-TPNA-TOTAL.                                 
104800      PERFORM 4750-FORMAT-DETAIL-DATA.                                    
104900                                                                          
105000**********************************************************                
105100*         LOGIC FOR PERFORMING COMPUTATIONS              *                
105200**********************************************************                
105300 4710-COMPUTATIONS.                                                       
105400                                                                          
105500      PERFORM 4725-COMPUTE-AVG-DMD.                                       
105600      PERFORM 4730-COMPUTE-MTH-SUPPLY.                                    
105700                                                                          
105800***************************************************************           
105900*  ACCUMULATE DATA FOR OBTAINING TOTALS ON TPNA DATA FROM DB2 *           
106000*  ON THE TPNA TOTAL TABLE AND THE GRAND TOTAL TABLE          *           
106100***************************************************************           
106200 4720-ACCUMULATE-TPNA-TOTAL.                                              
106300                                                                          
106400      IF WS-OH-STK NUMERIC                                                
106500         ADD WS-OH-STK               TO                                   
106600             E360-CURR-QTY-ON-HAND   OF ACCUM-TBL(1)                      
106700             E360-CURR-QTY-ON-HAND   OF ACCUM-TBL(3)                      
106800      END-IF.                                                             
106900                                                                          
107000      ADD WS-ON-ORDER             TO                                      
107100          E361-CURR-QTY-ON-ORD    OF ACCUM-TBL(1)                         
107200          E361-CURR-QTY-ON-ORD    OF ACCUM-TBL(3).                        
107300                                                                          
107400      ADD WS-IN-XFER              TO                              000317HJ
107500          E000-CURR-QTY-IN-XFER   OF ACCUM-TBL(1)                 000317HJ
107600          E000-CURR-QTY-IN-XFER   OF ACCUM-TBL(3).                000317HJ
107700                                                                  000317HJ
107800      ADD WS-OH-XDOCK             TO                              000317HJ
107900          E000-CURR-QTY-OH-XDOCK  OF ACCUM-TBL(1)                 000317HJ
108000          E000-CURR-QTY-OH-XDOCK  OF ACCUM-TBL(3).                000317HJ
108100                                                                  000317HJ
108200      ADD WS-BACK-ORDER           TO                                      
108300          E459-CURR-QTY-BO        OF ACCUM-TBL(1)                         
108400          E459-CURR-QTY-BO        OF ACCUM-TBL(3).                        
108500                                                                          
108600      ADD WS-AVG-DMD             TO                                       
108700          E930-AVG-DMD           OF ACCUM-TBL(1)                          
108800          E930-AVG-DMD           OF ACCUM-TBL(3).                         
108900                                                                          
109000*********************************************************                 
109100*      COMPUTE AVERAGE DEMAND FOR LAST 3 MONTHS         *                 
109200*********************************************************                 
109300 4725-COMPUTE-AVG-DMD.                                                    
109400                                                                          
109500      MOVE ZERO                  TO WS-AVG-DMD.                           
109600      IF WS-TOTAL-DMD-FCST       = ZERO                                   
109700         MOVE WS-TOTAL-DMD-FCST  TO WS-AVG-DMD                            
109800      ELSE                                                                
109900         COMPUTE WS-AVG-DMD                                               
110000                             = WS-TOTAL-DMD-FCST / 3                      
110100      END-IF.                                                             
110200      MOVE ZERO                  TO WS-TOTAL-DMD-FCST.                    
110300                                                                          
110400***********************************************************               
110500*            COMPUTE MONTHS OF SUPPLY                     *               
110600***********************************************************               
110700 4730-COMPUTE-MTH-SUPPLY.                                                 
110800                                                                          
110900       IF WS-OH-STK                    = ZERO                             
111000          MOVE ZERO                    TO WS-MTH-OF-SUPPLY                
111100       ELSE                                                               
111200          IF WS-AVG-DMD                   = ZERO                          
111300             MOVE 999.9                   TO WS-MTH-OF-SUPPLY             
111400          ELSE                                                            
111500             COMPUTE WS-MTH-OF-SUPPLY = WS-OH-STK / WS-AVG-DMD    000317HJ
111600          END-IF                                                          
111700       END-IF.                                                            
111800                                                                          
111900*************************************************************             
112000*           FORMAT DETAIL DATA ON THE SCREEN                *             
112100*************************************************************             
112200 4750-FORMAT-DETAIL-DATA.                                                 
112300                                                                          
112310***************************************************************** 170607KC
112320***      PART CENTER HARD CODE REMOVAL CHANGES                *** 170607KC
112330***************************************************************** 170607KC
112340      MOVE PCT-WH(PC-INDEX)          TO WH-NAME                   170607KC
113100                                     OF OUTPUT-DETAIL             170607KC
113110                                                                          
113200      IF WS-OH-STK NOT NUMERIC                                            
113300         MOVE ZERO                   TO E360-CURR-QTY-ON-HAND             
113400                                     OF OUTPUT-DETAIL                     
113500      ELSE                                                                
113600         MOVE WS-OH-STK              TO E360-CURR-QTY-ON-HAND             
113700                                     OF OUTPUT-DETAIL                     
113800      END-IF.                                                             
113900      MOVE WS-ON-ORDER               TO E361-CURR-QTY-ON-ORD              
114000                                     OF OUTPUT-DETAIL.                    
114100      MOVE WS-IN-XFER                TO E000-CURR-QTY-IN-XFER     000317HJ
114200                                     OF OUTPUT-DETAIL.            000317HJ
114300      MOVE WS-OH-XDOCK               TO E000-CURR-QTY-OH-XDOCK    000317HJ
114400                                     OF OUTPUT-DETAIL.            000317HJ
114500      MOVE WS-BACK-ORDER             TO E459-CURR-QTY-BO                  
114600                                     OF OUTPUT-DETAIL.                    
114700      MOVE WS-AVG-DMD                TO E930-AVG-DMD                      
114800                                     OF OUTPUT-DETAIL.                    
114900      MOVE WS-MTH-OF-SUPPLY          TO E2705-MTHS-SUP                    
115000                                     OF OUTPUT-DETAIL.                    
115100                                                                          
115200      MOVE LOW-VALUES                TO OUTPUT-LINE(INDX-OUT1).           
115300      MOVE OUTPUT-DETAIL             TO OUTPUT-LINE(INDX-OUT1).           
115400      SET INDX-OUT1                  UP BY +1.                            
115500                                                                          
115600*************************************************************             
115700*    LOGIC FOR DERIVING LAST 3 MONTHS FOR AVERAGE DEMAND    *             
115800*************************************************************             
115900 4900-DERIVE-MONTH.                                                       
116000                                                                          
116100      IF WS-MONTH-COUNTER         = 1                             990806EL
116200          MOVE WS-LATEST-DMD-MO   TO WS-TEMP-YEAR-MONTH           990806EL
116300                                     WS-PREV-YEAR-MONTH           990806EL
116400      ELSE                                                        990806EL
116500          MOVE WS-PREV-YEAR-MONTH TO WS-TEMP-YEAR-MONTH           990806EL
116600      END-IF.                                                     990806EL
116700                                                                          
116800      COMPUTE WS-TEMP-MM          = WS-TEMP-MM - 1.               990806EL
116900                                                                          
117000      IF WS-TEMP-MM               = ZERO                          990806EL
117100          MOVE WS-12              TO WS-TEMP-MM                   990806EL
117200          COMPUTE WS-TEMP-CCYY    = WS-TEMP-CCYY - 1              990806EL
117300      END-IF.                                                     990806EL
117400                                                                          
117500      MOVE WS-TEMP-YEAR-MONTH     TO MKTDMDL-MKT-DMD-MO           990806EL
117600                                     WS-PREV-YEAR-MONTH.          990806EL
117700                                                                          
117800**************************************************************            
117900*        CALL DB2 MODULE FOR GETTING AVERAGE DEMAND          *            
118000**************************************************************            
118100 4950-CALL-YCC008.                                                        
118200                                                                          
118300      MOVE 'S0010'              TO CALL-PARA.                             
118400      PERFORM 6400-YCC008-TMKTDMD.                                990806EL
118500                                                                          
118600      IF VALID-REQUEST                                                    
118700         NEXT SENTENCE                                                    
118800      ELSE                                                                
118900         DISPLAY '******************************************'             
119000         DISPLAY '** POM915 ABEND                         **'             
119100         DISPLAY '** PARA 4950-CALL-YCC008             DB2**'     990806EL
119200         DISPLAY '** INVALID RETURN CODE                  **'             
119300         DISPLAY '** SQLERRMC = ' SQLERRMC                                
119400         DISPLAY '******************************************'             
119500         PERFORM 9998-CALL-CANCEL                                         
119600      END-IF.                                                             
119700                                                                          
119800************************************************************              
119900* LOGIC FOR COMPUTING MONTHS-OF-SUPPLY FOR THE TPNA TOTAL, *              
120000* TMD TOTAL AND THE GRAND TOTAL                            *              
120100************************************************************              
120200 5000-PROCESS-TOTALS.                                                     
120300                                                                          
120400        PERFORM 5100-MOS-TOTALS.                                          
120500        SET TOT-INDX                TO 1.                                 
120600        PERFORM 5200-FORMAT-TOTALS                                        
120700                VARYING INDX-OUT2 FROM 1 BY 1   UNTIL                     
120800                INDX-OUT2 > 3.                                            
120900        PERFORM 5300-FORMAT-GRAND-TOTAL.                                  
121000                                                                          
121100***********************************************************               
121200*  COMPUTE MONTHS-OF-SUPPLY FOR TPNA TOTAL,TMD TOTAL AND  *               
121300*  GRAND TOTAL                                            *               
121400***********************************************************               
121500 5100-MOS-TOTALS.                                                         
121600                                                                          
121700       SET TOT-INDX                                 TO +1.                
121800       PERFORM 5110-COMPUTE-MOS-TOTAL                                     
121900               VARYING TOT-INDX FROM 1 BY 1      UNTIL                    
122000               TOT-INDX > 3.                                              
122100                                                                          
122200 5110-COMPUTE-MOS-TOTAL.                                                  
122300                                                                          
122400       MOVE E360-CURR-QTY-ON-HAND OF ACCUM-TBL(TOT-INDX) TO               
122500            WS-COMP-ON-HAND.                                              
122600       MOVE E930-AVG-DMD          OF ACCUM-TBL(TOT-INDX) TO               
122700            WS-COMP-AVG-DMD.                                              
122800       IF WS-COMP-ON-HAND         = ZERO                                  
122900          MOVE ZERO                  TO E2705-MTHS-SUP                    
123000                                     OF ACCUM-TBL(TOT-INDX)               
123100       ELSE                                                               
123200          IF  WS-COMP-AVG-DMD        = ZERO                               
123300              MOVE 999.9             TO E2705-MTHS-SUP                    
123400                                     OF ACCUM-TBL(TOT-INDX)               
123500          ELSE                                                            
123600              COMPUTE WS-MTH-SUP-TOTAL   =                                
123700                                WS-COMP-ON-HAND / WS-COMP-AVG-DMD         
123800              MOVE WS-MTH-SUP-TOTAL      TO E2705-MTHS-SUP                
123900                                         OF ACCUM-TBL(TOT-INDX)           
124000          END-IF                                                          
124100       END-IF.                                                            
124200                                                                          
124300******************************************************************        
124400*        FORMAT TPNA TOTAL AND TMD TOTAL ON THE SCREEN       *            
124500******************************************************************        
124600 5200-FORMAT-TOTALS.                                                      
124700                                                                          
124800          MOVE E360-CURR-QTY-ON-HAND OF ACCUM-TBL(TOT-INDX)  TO           
124900               E360-CURR-QTY-ON-HAND OF OUTPUT-TOTAL-DETAIL.              
125000                                                                          
125100          MOVE E361-CURR-QTY-ON-ORD  OF ACCUM-TBL(TOT-INDX)  TO           
125200               E361-CURR-QTY-ON-ORD  OF OUTPUT-TOTAL-DETAIL.              
125300                                                                          
125400          MOVE E000-CURR-QTY-IN-XFER OF ACCUM-TBL(TOT-INDX)  TO   000317HJ
125500               E000-CURR-QTY-IN-XFER OF OUTPUT-TOTAL-DETAIL.      000317HJ
125600                                                                  000317HJ
125700          MOVE E000-CURR-QTY-OH-XDOCK OF ACCUM-TBL(TOT-INDX)  TO  000317HJ
125800               E000-CURR-QTY-OH-XDOCK OF OUTPUT-TOTAL-DETAIL.     000317HJ
125900                                                                  000317HJ
126000          MOVE E459-CURR-QTY-BO      OF ACCUM-TBL(TOT-INDX)  TO           
126100               E459-CURR-QTY-BO      OF OUTPUT-TOTAL-DETAIL.              
126200                                                                          
126300          MOVE E930-AVG-DMD          OF ACCUM-TBL(TOT-INDX)  TO           
126400               E930-AVG-DMD          OF OUTPUT-TOTAL-DETAIL.              
126500                                                                          
126600          MOVE E2705-MTHS-SUP        OF ACCUM-TBL(TOT-INDX)  TO           
126700               E2705-MTHS-SUP        OF OUTPUT-TOTAL-DETAIL.              
126800                                                                          
126900          IF INDX-OUT2              = 1                                   
127000             MOVE ' TPNA:'          TO TOTAL-NAME                         
127100                                    OF OUTPUT-TOTAL-DETAIL                
127200          ELSE                                                            
127300           IF INDX-OUT2            = 2                                    
127400             MOVE '  TMD:'        TO TOTAL-NAME                           
127500                                  OF OUTPUT-TOTAL-DETAIL                  
127600           ELSE                                                           
127700              MOVE 'TOTAL:'      TO TOTAL-NAME                            
127800                                 OF OUTPUT-TOTAL-DETAIL                   
127900           END-IF                                                         
128000          END-IF.                                                         
128100                                                                          
128200          MOVE LOW-VALUE            TO TOTAL-LINE(INDX-OUT2).             
128300          MOVE OUTPUT-TOTAL-DETAIL  TO TOTAL-LINE(INDX-OUT2).             
128400          SET TOT-INDX              UP BY +1.                             
128500                                                                          
128600****************************************************************          
128700*            FORMAT GRAND TOTAL ON THE SCREEN                  *          
128800****************************************************************          
128900 5300-FORMAT-GRAND-TOTAL.                                                 
129000                                                                          
129100       MOVE E360-CURR-QTY-ON-HAND    OF ACCUM-TBL(3)         TO           
129200            E360-CURR-QTY-ON-HAND-G  OF GRAND-TOTAL-DETAIL.               
129300                                                                          
129400       MOVE E361-CURR-QTY-ON-ORD     OF ACCUM-TBL(3)         TO           
129500            E361-CURR-QTY-ON-ORD-G   OF GRAND-TOTAL-DETAIL.               
129600                                                                          
129700       MOVE E000-CURR-QTY-IN-XFER    OF ACCUM-TBL(3)         TO   000317HJ
129800            E000-CURR-QTY-IN-XFER-G  OF GRAND-TOTAL-DETAIL.       000317HJ
129900                                                                  000317HJ
130000       MOVE E000-CURR-QTY-OH-XDOCK   OF ACCUM-TBL(3)         TO   000317HJ
130100            E000-CURR-QTY-OH-XDOCK-G OF GRAND-TOTAL-DETAIL.       000317HJ
130200                                                                  000317HJ
130300       MOVE E459-CURR-QTY-BO         OF ACCUM-TBL(3)         TO           
130400            E459-CURR-QTY-BO-G       OF GRAND-TOTAL-DETAIL.               
130500                                                                          
130600       MOVE E930-AVG-DMD             OF ACCUM-TBL(3)         TO           
130700            E930-AVG-DMD-G           OF GRAND-TOTAL-DETAIL.               
130800                                                                          
130900       MOVE E2705-MTHS-SUP           OF ACCUM-TBL(3)         TO           
131000            E2705-MTHS-SUP-G         OF GRAND-TOTAL-DETAIL.               
131100                                                                          
131200       MOVE 'TOTAL:'                 TO TOTAL-NAME-G                      
131300                                     OF GRAND-TOTAL-DETAIL.               
131400                                                                          
131500***************************************************************           
131600*       CALL DB2 MODULE YCA002 FOR INQUIRY ON TPMOPER         *           
131700***************************************************************           
131800 6100-YCA002-S0002-TPMOPER.                                               
131900                                                                          
132000      CALL 'YCA002' USING CALL-OPTIONS,                                   
132100                          MISC-FIELDS-LINK,                               
132200                          PMOPER-AREA,                                    
132300                          SQLCA.                                          
132400                                                                          
132500***************************************************************           
132600*       CALL DB2 MODULE YCA004 FOR INQUIRY ON TPMWINV         *           
132700***************************************************************           
132800 6200-YCA004-S0003-TPMWINV.                                               
132900                                                                          
133000      CALL 'YCA004' USING CALL-OPTIONS,                                   
133100                          MISC-FIELDS-LINK,                               
133200                          PMWINV-AREA,                                    
133300                          SQLCA.                                          
133400                                                                          
133500***************************************************************           
133600*       CALL DB2 MODULE YCB007 FOR INQUIRY ON TPLNSHP         *           
133700***************************************************************           
133800 6300-YCB007-TPLNSHP.                                                     
133900                                                                          
134000      CALL 'YCB007' USING CALL-OPTIONS,                                   
134100                          MISC-FIELDS-LINK,                               
134200                          PLNSHP-AREA,                                    
134300                          SQLCA.                                          
134400                                                                          
134500***************************************************************           
134600*       CALL DB2 MODULE YCC008 FOR INQUIRY ON TMKTDMD         *           
134700***************************************************************           
134800 6400-YCC008-TMKTDMD.                                             990806EL
134900                                                                          
135000      CALL 'YCC008' USING CALL-OPTIONS,                                   
135100                          MISC-FIELDS-LINK,                               
135200                          MKTDMD-AREA,                                    
135300                          SQLCA.                                          
135400                                                                          
135500****************************************************************          
135600*   IMS CALL TO RECEIVE INPUT SCREEN FROM INPUT SCREEN         *          
135700****************************************************************          
135800 7100-IO-PI9805I.                                                         
135900                                                                          
136000     CALL 'CBLTDLI' USING  GU                                             
136100                           IO-PCB                                         
136200                           PI9805I.                                       
136300                                                                          
136400*****************************************************************         
136500*   IMS CALL TO GET PTROOT SEGMENT DATA FROM IMS PARTS DATABASE *         
136600*****************************************************************         
136700 7200-GU-PTROOT.                                                          
136800                                                                          
136900     CALL 'CBLTDLI' USING GU                                              
137000                          PPT-PCB                                         
137100                          PTROOT                                          
137200                          ZPTROOT.                                        
137300                                                                          
137400                                                                          
137500***************************************************************           
137600*           IMS CALL TO SEND OUTPUT SCREEN                    *           
137700***************************************************************           
137800 7500-IO-PI9805O.                                                         
137900                                                                          
138000     CALL 'CBLTDLI' USING ISRT                                            
138100                          IO-PCB                                          
138200                          PI9805O                                         
138300                          WS-SCREEN-NAME.                                 
138400                                                                          
138500 9998-CALL-CANCEL.                                                        
138600                                                                          
138700       CALL 'CANCEL'.                                                     
138800                                                                          
138900 9999-CALL-IMSABEND.                                                      
139000                                                                          
139100       CALL 'IMSABEND' USING IMS-ABEND-AREA.                              
139200                                                                          
