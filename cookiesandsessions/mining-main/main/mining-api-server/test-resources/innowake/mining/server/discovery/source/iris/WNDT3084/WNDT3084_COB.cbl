00001                                                                   
00002 *-----------------------------------------------------------------
00003 * Program Name         :  OG7ONOTI (Restructured Program)         
00004 * Function/Screen Title:  Display List of Notices                 
00005 * System               :  ONGARD                                  
00006 * Sub System           :  ------                                  
00007 * Author               :  S R Santaprakash / Syntel               
00008 * Date - Written       :  Jun 07, 1993                            
00009 * Trans Id             :  NIL                                     
00010 * Map Name             :  OG7NOTI                                 
00011 * DB/2 Tables          :  NOTI_ACTY                               
00012 * Description          :  This program displays and processes the 
00013 *                         Display List of Notices Screen ( OG7NOTI
00014 *                         It displays the map and validates the   
00015 *                         selection criteria specified by the user
00016 *                         If the criteria are valid, it displays a
00017 *                         list of notice activities meeting the   
00018 *                         criteria. From this screen , the user ca
00019 *                         select a particular activity and display
00020 *                         lis of addresses to the notice.         
00021 *                                                                 
00022 * Copy Books Used      :                                          
00023 *                                                                 
00024 * DATA DIVISION                                                   
00025 *                                                                 
00026 *-----------------------+---------------+-----------------------+ 
00027 *       Type                 Name          Description         
00028 *-----------------------+---------------+-----------------------+ 
00029 *      COPY                OG7EMSG      Error Message Codes    
00030 *-----------------------+---------------+-----------------------+ 
00031 *      -do-                OG7CNTSQ     Screen TSQ             
00032 *-----------------------+---------------+-----------------------+ 
00033 *      -do-                OG7CNOTI     Symbolic Map           
00034 *-----------------------+---------------+-----------------------+ 
00035 *      -do-                OGXCDATE     Date Routine           
00036 *-----------------------+---------------+-----------------------+ 
00037 *      -do-                OGXTSQCD     TSQ                    
00038 *-----------------------+---------------+-----------------------+ 
00039 *      -do-                OGXSQLCD     SQL CD                 
00040 *-----------------------+---------------+-----------------------+ 
00041 *      -do-                OGXBMSAT     BMS Attributes         
00042 *-----------------------+---------------+-----------------------+ 
00043 *      -do-                OGXACMCA     ACMP Comm Area         
00044 *-----------------------+---------------+-----------------------+ 
00045 *     INCLUDE              SQLCA        SQL Comm Area          
00046 *-----------------------+---------------+-----------------------+ 
00047 *      -do-                DCLEMT       DCLGEN For Error Tbl   
00048 *-----------------------+---------------+-----------------------+ 
00049 *      -do-                DCLNTA       DCLGEN For NOTI_ACTY   
00050 *-----------------------+---------------+-----------------------+ 
00051 *      -do-                LNTA21CD     Declare Cursor         
00052 *-----------------------+---------------+-----------------------+ 
00053 *                                                                 
00054 * PROCEDURE DIVISION                                              
00055 *                                                                 
00056 *-----------------------+---------------+-----------------------+ 
00057 *       INCLUDE            LNTA21CP     Fetch Cursor           
00058 *-----------------------+---------------+-----------------------+ 
00059 *                                                                 
00060 * PTM/CR No.  Date    Programmer    Description                   
00061 *                                                                 
00062 *    1       06/14   Narayanan      Restructured program          
00063 *-----------------------------------------------------------------
00064 * SR8401 OCT 2011 B.TRUJILLO  REMOVE WS-CODE-RTN - WRONG PROGRAM  
00065 *                             AND NOT USED                        
00066 *-----------------------------------------------------------------
00067 * SR10901    12/2013  M.HOWLES      COPYBOOK NAME CHANGED FROM   *
00068 *                                   OG7NOTIM TO OG7ONOTI AND     *
00069 *                                   ELIMINATED IGYPS2015 MSG.    *
00070 *-----------------------------------------------------------------
00071 * TR1089 07/2019 B.TRUJILLO  ADD ONG_TMST TO THE CURSOR STATMENT *
00072 *                            IN LNTA21CD                         *
00073 *-----------------------------------------------------------------
00074                                                                   
00075  IDENTIFICATION DIVISION.                                         
00076  PROGRAM-ID. OG7ONOTI.                                            
00077  ENVIRONMENT DIVISION.                                            
00078  DATA DIVISION.                                                   
00079  WORKING-STORAGE SECTION.                                         
00080                                                                   
00081  01  WS-MISC-VARIABLES.                                           
00082      05 WS-HELP-VAL.                                              
00083         10 WS-NUM-VAR-DEC1             PIC 9(12)V9.               
00084         10 WS-NUM-VAR-DEC2             PIC 9(11)V99.              
00085         10 WS-NUM-VAR-DEC3             PIC 9(10)V999.             
00086         10 WS-NUM-VAR-DEC4             PIC 9(09)V9999.            
00087         10 WS-NUM-VAR-DEC5             PIC 9(08)V99999.           
00088         10 WS-NUM-VAR-DEC6             PIC 9(07)V999999.          
00089         10 WS-NUM-VAR-DEC7             PIC 9(06)V9999999.         
00090      05 WS-HELP-VALUE.                                            
00091         10 WS-HELP-CDE-FILLER          PIC X(2).                  
00092         10 WS-HELP-CDE                 PIC X(4).                  
00093         10 WS-HELP-FLR                 PIC X(44).                 
00094      05 TABLE-ACCESSED                 PIC X(1).                  
00095      05 WS-TSQ-DELETE                  PIC x(01) VALUE 'Y'.       
00096         88 WS-DELETE-TSQ               VALUE 'Y'.                 
00097      05 WS-NUM-DATE-1.                                            
00098         10 WS-NUM-MM1                  PIC 9(02).                 
00099         10 FILLER                      PIC X(01).                 
00100         10 WS-NUM-DD1                  PIC 9(02).                 
00101         10 FILLER                      PIC X(01).                 
00102         10 WS-NUM-YYYY1                PIC 9(04).                 
00103                                                                   
00104      05 WS-NUM-DATE-2.                                            
00105         10 WS-NUM-MM2                  PIC 9(02).                 
00106         10 FILLER                      PIC X(01).                 
00107         10 WS-NUM-DD2                  PIC 9(02).                 
00108         10 FILLER                      PIC X(01).                 
00109         10 WS-NUM-YYYY2                PIC 9(04).                 
00110                                                                   
00111      05 WS-ONLY-NUM-DATE1.                                        
00112         10 WS-ONUM-YYYY1                PIC 9(04).                
00113         10 WS-ONUM-MM1                  PIC 9(02).                
00114         10 WS-ONUM-DD1                  PIC 9(02).                
00115                                                                   
00116      05 WS-ONLY-NUM-DATE2.                                        
00117         10 WS-ONUM-YYYY2                PIC 9(04).                
00118         10 WS-ONUM-MM2                  PIC 9(02).                
00119         10 WS-ONUM-DD2                  PIC 9(02).                
00120                                                                   
00121      05 WS-INIT-CSR                    PIC S9(4) COMP VALUE -1.   
00122      05 WS-FIRST-FLD                   PIC X(01).                 
00123      05 WS-NO-ROWS                     PIC X(01).                 
00124         88 WS-NOT-FND                  VALUE 'Y'.                 
00125      05 WS-DB2-IO-ERR                  PIC X(01).                 
00126         88 WS-DB2-ERR                  VALUE 'Y'.                 
00127      05 WS-NON-KEY-FLD                 PIC X(01).                 
00128      05 WS-CDE                         PIC S9(4) COMP.            
00129      05 WS-SQLCODE                     PIC 9(09).                 
00130      05 WS-DATE-RTN                    PIC X(08) VALUE 'OGXODATE'.
00131      05 WS-SELECT-CNT                  PIC 9(02) VALUE 0.         
00132      05 WS-ERR-AREA.                                              
00133         10 WS-ERR-CDE.                                            
00134            15 WS-ERR-TYP               PIC X(01).                 
00135            15 WS-ERR-ERR               PIC 9(04).                 
00136         10 FILLER                      PIC X VALUE ':'.           
00137         10 WS-ERR-DSC                  PIC X(50).                 
00138         10 FILLER                      PIC X(09) VALUE '       '. 
00139         10 WS-ERR-SQL                  PIC -----9.                
00140         10 WS-ERR-SPC REDEFINES WS-ERR-SQL.                       
00141            15  WS-ERR-BLK              PIC X(6).                  
00142      05 WS-MAP-NAM                     PIC X(07) VALUE 'OG7NOTI'. 
00143      05 WS-UPDATE                      PIC X(01) VALUE 'U'.       
00144      05 WS-INSERT                      PIC X(01) VALUE 'I'.       
00145      05 WS-DELETE                      PIC X(01) VALUE 'D'.       
00146      05 WS-ERROR                       PIC X(01) VALUE 'E'.       
00147      05 WS-ERR-TYP-DB2                 PIC X(01) VALUE 'D'.       
00148      05 WS-ERR-TYP-TSQ                 PIC X(01) VALUE 'T'.       
00149      05 WS-YES                         PIC X(01) VALUE 'Y'.       
00150      05 WS-HELP-IND                    PIC X(01).                 
00151      05 WS-NO                          PIC X(01) VALUE 'N'.       
00152      05 WS-FLD-HELP                    PIC X(01) VALUE 'F'.       
00153      05 WS-SCR-HELP                    PIC X(01) VALUE 'S'.       
00154      05 WS-TMP-TBL-CDE                 PIC X(04) VALUE SPACES.    
00155      05 WS-SCROLL-ACTIVE               PIC X(01) VALUE SPACES.    
00156      05 WS-USER-TRM.                                              
00157         10 WS-USERID                   PIC X(07).                 
00158         10 WS-DASH                     PIC X(01) VALUE '-'.       
00159         10 WS-TRMID                    PIC X(04).                 
00160                                                                   
00161      05 WS-EDIT-ERR                    PIC X(01).                 
00162         88 WS-EDIT-ERROR               VALUE 'Y'.                 
00163      05 WS-ALPH-VAR.                                              
00164         10 WS-ALPH-VAR1 OCCURS 10 TIMES.                          
00165            15 WS-ALPH-VAR-X            PIC X(01).                 
00166      05 WS-NUM-VAR                     PIC 9(10).                 
00167      05 WS-NUM-VAR-I REDEFINES WS-NUM-VAR.                        
00168         10 WS-NUM-VAR-INT OCCURS 10.                              
00169            15 WS-NUM-VAR-01            PIC 9(01).                 
00170      05 WS-EOL                         PIC X(1) VALUE X'80'.      
00171      05 WS-NULL                        PIC X(1).                  
00172      05 WS-EARLY-DATE                  PIC X(10)                  
00173         VALUE '01/01/1901'.                                       
00174      05 WS-EDITED-SW                   PIC X(01).                 
00175         88 ATLEAST-ONE-EDITED          VALUE 'Y'.                 
00176      05 WS-DATE-1.                                                
00177         10 WS-DATE-1MM                 PIC X(2).                  
00178         10 WS-DATE-FLR1                PIC X(1).                  
00179         10 WS-DATE-1DD                 PIC X(2).                  
00180         10 WS-DATE-FLR2                PIC X(1).                  
00181         10 WS-DATE-1YY                 PIC X(4).                  
00182                                                                   
00183      05 WS-DT-DATE.                                               
00184         10 WS-DT-MNTH                  PIC X(2).                  
00185         10 WS-DATE-FLR1                PIC X(1)                   
00186            VALUE '/'.                                             
00187         10 WS-DT-DAY                   PIC X(2).                  
00188         10 WS-DATE-FLR2                PIC X(1)                   
00189            VALUE '/'.                                             
00190         10 WS-DT-CENTURY               PIC X(2).                  
00191         10 WS-DT-YEAR                  PIC X(2).                  
00192                                                                   
00193 *    COPY OG7CNWSU.                                               
00194  ++INCLUDE OG7CNWSU                                               
00195 *1                                                                
00196                                                                   
00197      05 WS-DB-VARIABLES.                                          
00198         10 WS-DB-NOTI-ID-SEL          PIC X(04).                  
00199         10 WS-DB-REQ-UID              PIC X(08).                  
00200         10 WS-DB-REQ-STRT-DTE         PIC X(10).                  
00201         10 WS-DB-REQ-END-DTE          PIC X(10).                  
00202                                                                   
00203      05 WS-NOTI-ID-UND                PIC X(04) VALUE ALL '_'.    
00204      05 WS-REQ-UID-UND                PIC X(08) VALUE ALL '_'.    
00205      05 WS-ACTY-SEQ-NUM               PIC S9(9)V COMP.            
00206      05 WS-RESEND-ACTY                PIC 9(6).                   
00207                                                                   
00208  01  WS-DATE-FLDS.                                                
00209      05 WS-DATE-VAL                    PIC X(10).                 
00210      05 WS-DATE-OPT                    PIC 9.                     
00211      05 WS-DATE-RTN-CDE                PIC 9.                     
00212                                                                   
00213  01  WS-KEY-AREA.                                                 
00214      05 WS-NOTI-ID-SEL                 PIC X(04).                 
00215      05 WS-REQ-UID                     PIC X(08).                 
00216      05 WS-REQ-STRT-DTE                PIC X(10).                 
00217      05 WS-REQ-END-DTE                 PIC X(10).                 
00218                                                                   
00219 *     COPY OGXCERCD.                                              
00220  ++INCLUDE OGXCERCD                                               
00221 *1                                                                
00222 *                                                                 
00223 *    OG7 MESSAGE CODES                                            
00224 *                                                                 
00225                                                                   
00226 *     COPY OG7EMSG.                                               
00227  ++INCLUDE OG7EMSG                                                
00228 *1                                                                
00229                                                                   
00230  01  WS-SWITCHES.                                                 
00231      05 WS-CHANGE-SW                   PIC X(01) VALUE 'C'.       
00232      05 WS-SELECT-SW                   PIC X(01) VALUE 'S'.       
00233      05 WS-SPACE                       PIC X(01) VALUE SPACES.    
00234      05 WS-XCTL-SW                     PIC X(01) VALUE 'X'.       
00235      05 WS-RETURN-SW                   PIC X(01) VALUE 'R'.       
00236      05 WS-FIELD-SW                    PIC X(01) VALUE 'F'.       
00237      05 WS-DISP-DAT-SW                 PIC X(01).                 
00238         88 WS-DISPLAY-DATA             VALUE 'Y'.                 
00239      05 WS-UPDT-TSQ-SW                 PIC X(01).                 
00240         88 WS-UPDATE-SCR-TSQ           VALUE 'Y'.                 
00241      05 WS-ROW-SELECTED-SW             PIC X(01) VALUE SPACES.    
00242                                                                   
00243  01  WS-COUNTERS.                                                 
00244      05 WS-ITEM-CTR1                   PIC S9(04) COMP.           
00245      05 WS-ITEM-CTR                    PIC S9(04) COMP.           
00246      05 WS-LINE-CTR                    PIC S9(04) COMP VALUE 10.  
00247      05 WS-SKIP-CTR                    PIC S9(04) COMP VALUE 0.   
00248      05 WS-CTR                         PIC S9(04) COMP VALUE 0.   
00249      05 WS-CTR1                        PIC S9(04) COMP VALUE 0.   
00250      05 WS-CTR2                        PIC S9(04) COMP VALUE 0.   
00251      05 WS-CTR3                        PIC S9(04) COMP VALUE 0.   
00252      05 WS-CTR4                        PIC S9(04) COMP VALUE 0.   
00253      05 WS-CTR5                        PIC S9(04) COMP VALUE 0.   
00254      05 WS-LUW-CTR                     PIC S9(04) COMP VALUE 0.   
00255      05 WS-TBL-CTR                     PIC S9(04) COMP VALUE 1.   
00256      05 WS-DB2-CTR                     PIC S9(04) COMP VALUE 1.   
00257      05 WS-TOT-PAGE-CTR                PIC S9(04) COMP VALUE 1.   
00258      05 WS-DEC-POS                     PIC S9(04) COMP VALUE 0.   
00259      05 WS-DEC-CTR                     PIC S9(04) COMP VALUE 0.   
00260      05 I                              PIC S9(04) COMP.           
00261      05 J                              PIC S9(04) COMP.           
00262      05 WS-PAGE-LIMIT                  PIC S9(04) COMP VALUE 50.  
00263                                                                   
00264  01  WS-SCR-TSQ.                                                  
00265      05 WS-SCR-TRMID                   PIC X(04).                 
00266      05 WS-SCR-FASTID                  PIC X(04).                 
00267                                                                   
00268  01  WS-DB2-TSQ.                                                  
00269      05 WS-DB2-TRMID                   PIC X(04).                 
00270      05 WS-DB2-FASTID                  PIC X(04).                 
00271                                                                   
00272  01  WS-DRV-TSQ.                                                  
00273      05 WS-DRV-TRMID                   PIC X(04).                 
00274      05 WS-DRV-FASTID                  PIC X(04) VALUE 'DB2D'.    
00275                                                                   
00276  01  WS-NTA-TO-TEMP.                                              
00277      05 WS-NUM-ACTY-SEQ-N              PIC 9(6).                  
00278      05 WS-NUM-RSND-SEQ-N              PIC 9(6).                  
00279                                                                   
00280 *                                                                 
00281 * COPY BOOK FOR TSQ TO BE USED BY NOTI AND NOT1 PROGRAMS.         
00282 *                                                                 
00283 *****COPY OG7CNTSQ REPLACING ==:WS1:== BY ==WS==.                 
00284  ++INCLUDE OG7CNTSQ                                               
00285                                                                   
00286 *                                                                 
00287 * COPY BOOK FOR BMS SYMBOLIC MAP OG7NOTI                          
00288 *                                                                 
00289                                                                   
00290 * COPY OG7CNOTI.                                                  
00291  ++INCLUDE OG7CNOTI                                               
00292 *1                                                                
00293                                                                   
00294 *                                                                 
00295 * COPY BOOK FOR DATE VALIDATION ROUTINE                           
00296 *                                                                 
00297                                                                   
00298 * COPY OGXCDATE.                                                  
00299  ++INCLUDE OGXCDATE                                               
00300 *1                                                                
00301                                                                   
00302 *                                                                 
00303 * COPY BOOK FOR TS QUEUE CODE                                     
00304 *                                                                 
00305                                                                   
00306 * COPY OGXTSQCD.                                                  
00307  ++INCLUDE OGXTSQCD                                               
00308 *1                                                                
00309                                                                   
00310 *                                                                 
00311 * COPY BOOK FOR SQL CODES                                         
00312 *                                                                 
00313                                                                   
00314 * COPY OGXSQLCD.                                                  
00315  ++INCLUDE OGXSQLCD                                               
00316 *1                                                                
00317                                                                   
00318 *                                                                 
00319 * COPY BOOK FOR BMS ATTRIBUTES                                    
00320 *                                                                 
00321                                                                   
00322 * COPY OGXBMSAT.                                                  
00323  ++INCLUDE OGXBMSAT                                               
00324 *1                                                                
00325                                                                   
00326 *                                                                 
00327 * COPY BOOK FOR COMMUNICATION AREA                                
00328 *                                                                 
00329                                                                   
00330  01  WS-COMM-AREA.                                                
00331 *     COPY OGXACMCA.                                              
00332  ++INCLUDE OGXACMCA                                               
00333 *1                                                                
00334                                                                   
00335 *                                                                 
00336 * COPY BOOK FOR SQL COMMUNICATION AREA                            
00337 *                                                                 
00338                                                                   
00339      EXEC SQL                                                     
00340           INCLUDE SQLCA                                           
00341      END-EXEC.                                                    
00342                                                                   
00343 *                                                                 
00344 *    ERROR MESSAGE TABLE                                          
00345 *                                                                 
00346                                                                   
00347 *     EXEC SQL                                                    
00348 *          INCLUDE DCLEMT                                         
00349  ++INCLUDE DCLEMT                                                 
00350 *2                                                                
00351 *     END-EXEC.                                                   
00352                                                                   
00353 *                                                                 
00354 *    DCLGENS for NOTI_ACTY                                        
00355 *                                                                 
00356                                                                   
00357 *     EXEC SQL                                                    
00358 *          INCLUDE DCLNTA                                         
00359  ++INCLUDE DCLNTA                                                 
00360 *2                                                                
00361 *     END-EXEC                                                    
00362                                                                   
00363 *                                                                 
00364 *    CURSOR DECLARATION FOR DECLARE NOTI_ACTY CURSOR              
00365 *                                                                 
00366                                                                   
00367 *     EXEC SQL                                                    
00368 *          INCLUDE LNTA21CD                                       
00369  ++INCLUDE LNTA21CD                                               
00370 *2                                                                
00371 *     END-EXEC                                                    
00372                                                                   
00373  LINKAGE SECTION.                                                 
00374                                                                   
00375  01  DFHCOMMAREA                      PIC X(3000).                
00376  01  LS-TIOA-AREA.                                                
00377      05 LS-TIOA-AREA-X                PIC X(01) OCCURS 1 TO 3000  
00378                                       DEPENDING ON CA-MAP-LEN.    
00379                                                                   
00380  PROCEDURE DIVISION USING DFHCOMMAREA.                            
00381                                                                   
00382 *-----------------------------------------------------------------
00383 * PARAGRAPH  : 00000-MAIN-PARA                                    
00384 *                                                                 
00385 * LINKAGE TO : 40000-INIT-SETUP              20000-ROUT-PFKEY     
00386 *                                                                 
00387 * DESCRIPTION : This routine performs the initial setup on entry  
00388 *               to the program. It then checks the key pressed by 
00389 *               the user and takes appropriate action depending   
00390 *               on the key pressed.                               
00391 *-----------------------------------------------------------------
00392                                                                   
00393  00000-MAIN-PARA.                                                 
00394                                                                   
00395      PERFORM     40000-INIT-SETUP          THRU                   
00396                  40000-INIT-SETUP-EXIT                            
00397      PERFORM     20000-ROUT-PFKEY          THRU                   
00398                  20000-ROUT-PFKEY-EXIT.                           
00399                                                                   
00400  00000-MAIN-PARA-EXIT.                                            
00401      EXIT.                                                        
00402                                                                   
00403 *-----------------------------------------------------------------
00404 *                                                                 
00405 * PARAGRAPH : 20000-ROUT-PFKEY.                                   
00406 *                                                                 
00407 * LINKAGE TO : 21000-ERROR-PARA              22000-DISP-PARA      
00408 *              23000-RDISP-PARA              24000-RECEIVE-PARA   
00409 *              26000-SCRFWD-PARA             27000-SCRBWD-PARA    
00410 *              28000-TRAN-PARA               30000-REFRESH-PARA   
00411 *              31000-HELP-PARA               32000-ADDRESS-PARA   
00412 *              69000-READ-MSG                72000-FILL-MAP       
00413 *              73000-PROT-UNPROT-FLDS        79000-DISP-MAP       
00414 *              97000-RETN-PARA                                    
00415 *                                                                 
00416 * DESCRIPTION : This routine checks on the key pressed by the user
00417 *               and calls appropriate routine depending on the    
00418 *               same                                              
00419 *-----------------------------------------------------------------
00420                                                                   
00421  20000-ROUT-PFKEY.                                                
00422      MOVE EIBAID TO CA-PF-KEY.                                    
00423      IF CA-ERROR                                                  
00424         PERFORM   21000-ERROR-PARA              THRU              
00425                   21000-ERROR-PARA-EXIT                           
00426      ELSE                                                         
00427      IF CA-DISPLAY                                                
00428         PERFORM   22000-DISP-PARA               THRU              
00429                   22000-DISP-PARA-EXIT                            
00430      ELSE                                                         
00431      IF CA-REDISPLAY                                              
00432         PERFORM   23000-RDISP-PARA              THRU              
00433                   23000-RDISP-PARA-EXIT                           
00434      ELSE                                                         
00435      IF CA-RECEIVE                                                
00436         PERFORM   24000-RECEIVE-PARA            THRU              
00437                   24000-RECEIVE-PARA-EXIT                         
00438      ELSE                                                         
00439      IF CA-SCRL-FWD                                               
00440         PERFORM   26000-SCRFWD-PARA             THRU              
00441                   26000-SCRFWD-PARA-EXIT                          
00442      ELSE                                                         
00443      IF CA-SCRL-BWD                                               
00444         PERFORM   27000-SCRBWD-PARA             THRU              
00445                   27000-SCRBWD-PARA-EXIT                          
00446      ELSE                                                         
00447      IF CA-TRANSFER                                               
00448         PERFORM   28000-TRAN-PARA               THRU              
00449                   28000-TRAN-PARA-EXIT                            
00450      ELSE                                                         
00451      IF CA-REFRESH                                                
00452         PERFORM   30000-REFRESH-PARA            THRU              
00453                   30000-REFRESH-PARA-EXIT                         
00454      ELSE                                                         
00455      IF CA-HELP                                                   
00456         PERFORM   31000-HELP-PARA               THRU              
00457                   31000-HELP-PARA-EXIT                            
00458      ELSE                                                         
00459                                                                   
00460      IF CA-PF11                                                   
00461         PERFORM   32000-ADDRESS-PARA            THRU              
00462                   32000-ADDRESS-PARA-EXIT                         
00463      ELSE                                                         
00464         IF CA-MAP-LEN > 0                                         
00465            PERFORM 78000-RECEIVE-MAP            THRU              
00466                    78000-RECEIVE-MAP-EXIT                         
00467            PERFORM 80000-MAP-TO-TSQ             THRU              
00468                    80000-MAP-TO-TSQ-EXIT                          
00469            PERFORM 82000-UPDT-TSQ-SCR           THRU              
00470                    82000-UPDT-TSQ-SCR-EXIT                        
00471         END-IF                                                    
00472         IF CA-EXIT                                                
00473            MOVE WS-EXIT-CONF             TO WS-ERR-CDE            
00474         ELSE                                                      
00475            MOVE WS-INVLD-KEY                TO WS-ERR-CDE         
00476         END-IF                                                    
00477         MOVE WS-YES                      TO WS-FIRST-FLD          
00478         MOVE WS-YES                      TO WS-SCROLL-ACTIVE      
00479         PERFORM   69000-READ-MSG                THRU              
00480                   69000-READ-MSG-EXIT                             
00481         PERFORM   72000-FILL-MAP                THRU              
00482                   72000-FILL-MAP-EXIT                             
00483         PERFORM   73000-PROT-UNPROT-FLDS        THRU              
00484                   73000-PROT-UNPROT-FLDS-EXIT                     
00485         PERFORM   79000-DISP-MAP                THRU              
00486                   79000-DISP-MAP-EXIT                             
00487         PERFORM   97000-RETN-PARA               THRU              
00488                   97000-RETN-PARA-EXIT.                           
00489                                                                   
00490  20000-ROUT-PFKEY-EXIT.                                           
00491      EXIT.                                                        
00492                                                                   
00493 *-----------------------------------------------------------------
00494 *                                                                 
00495 * PARAGRAPH : 21000-ERROR-PARA.                                   
00496 *                                                                 
00497 * LINKAGE TO : 78000-RECEIVE-MAP             80000-MAP-TO-TSQ     
00498 *              82000-UPDT-TSQ-SCR            72000-FILL-MAP       
00499 *              73000-PROT-UNPROT-FLDS        79000-DISP-MAP       
00500 *              97000-RETN-PARA                                    
00501 *                                                                 
00502 * DESCRIPTION : This routine is performed when ACMP comes across  
00503 *               any errors and the same is to be displayed on     
00504 *               your map.                                         
00505 *-----------------------------------------------------------------
00506                                                                   
00507  21000-ERROR-PARA.                                                
00508                                                                   
00509      MOVE SPACES                         TO CA-ERR-SW             
00510      IF NOT CA-READ-DB2                                           
00511         MOVE WS-YES                      TO WS-NON-KEY-FLD        
00512      ELSE                                                         
00513         MOVE WS-YES                      TO WS-FIRST-FLD          
00514      END-IF                                                       
00515      MOVE WS-YES                         TO WS-SCROLL-ACTIVE      
00516      IF CA-MAP-LEN > 0                                            
00517         PERFORM   78000-RECEIVE-MAP             THRU              
00518                   78000-RECEIVE-MAP-EXIT                          
00519         PERFORM   80000-MAP-TO-TSQ              THRU              
00520                   80000-MAP-TO-TSQ-EXIT                           
00521         PERFORM   82000-UPDT-TSQ-SCR            THRU              
00522                   82000-UPDT-TSQ-SCR-EXIT                         
00523      END-IF                                                       
00524      PERFORM      72000-FILL-MAP                THRU              
00525                   72000-FILL-MAP-EXIT                             
00526      MOVE SPACES                         TO MAP-FAST-PATH-O       
00527      MOVE BMS-UNPROT                     TO MAP-FAST-PATH-ATR     
00528      MOVE CA-ERR-MSG-1                   TO MAP-MSG-DAT-O         
00529      PERFORM      73000-PROT-UNPROT-FLDS        THRU              
00530                   73000-PROT-UNPROT-FLDS-EXIT                     
00531      PERFORM      79000-DISP-MAP                THRU              
00532                   79000-DISP-MAP-EXIT                             
00533      PERFORM      97000-RETN-PARA               THRU              
00534                   97000-RETN-PARA-EXIT.                           
00535                                                                   
00536  21000-ERROR-PARA-EXIT.                                           
00537      EXIT.                                                        
00538                                                                   
00539 *-----------------------------------------------------------------
00540 *                                                                 
00541 *                                                                 
00542 * PARAGRAPH : 22000-DISP-PARA.                                    
00543 *                                                                 
00544 * LINKAGE TO : 83000-DELETE-TSQ-SCR          88000-DELETE-TSQ-DB2 
00545 *              60000-ACCESS-TABL             69000-READ-MSG       
00546 *              82000-UPDT-TSQ-SCR            87000-UPDT-TSQ-DB2   
00547 *              73000-PROT-UNPROT-FLDS        72000-FILL-MAP       
00548 *              73000-PROT-UNPROT-FLDS        79000-DISP-MAP       
00549 *              97000-RETN-PARA                                    
00550 *                                                                 
00551 * DESCRIPTION : This routine is performed only for the first time 
00552 *               when entering your program. If the program is     
00553 *               being entered for the first time, ACMP sets the   
00554 *               display switch. During this time all earlier      
00555 *               TSQ which were existing are deleted (some of them 
00556 *               may exist when user presses PF04 and jumps to a   
00557 *               different program), and the environment is setup  
00558 *                                                                 
00559 *-----------------------------------------------------------------
00560                                                                   
00561  22000-DISP-PARA.                                                 
00562                                                                   
00563      MOVE SPACES                         TO CA-DISPLAY-FN         
00564      COMPUTE CA-DB2-QID-2                = CA-DB2-QID-2 + 1       
00565                                                                   
00566      PERFORM      83000-DELETE-TSQ-SCR          THRU              
00567                   83000-DELETE-TSQ-SCR-EXIT                       
00568      PERFORM      88000-DELETE-TSQ-DB2          THRU              
00569                   88000-DELETE-TSQ-DB2-EXIT                       
00570      IF WS-DISPLAY-DATA                                           
00571 *                                                                 
00572 * WHEN THE PROGRAM IS INVOKED THE FIRST TIME , THIS SW WILL BE SPA
00573 *                                                                 
00574         PERFORM   60000-ACCESS-TABL             THRU              
00575                   60000-ACCESS-TABL-EXIT                          
00576         MOVE WS-YES                     TO WS-NON-KEY-FLD         
00577         MOVE SPACES                     TO CA-DB2-READ            
00578         MOVE WS-YES                     TO WS-FIRST-FLD           
00579         IF WS-ERR-CDE  =  WS-NO-RECORD AND                        
00580            WS-CTR = 0                                             
00581            MOVE  WS-NO-RECORD           TO WS-ERR-CDE             
00582            PERFORM  69000-READ-MSG              THRU              
00583                     69000-READ-MSG-EXIT                           
00584            MOVE 1                       TO WS-ITEM-CTR            
00585            PERFORM  82000-UPDT-TSQ-SCR          THRU              
00586                     82000-UPDT-TSQ-SCR-EXIT                       
00587         ELSE                                                      
00588            PERFORM  82000-UPDT-TSQ-SCR          THRU              
00589                     82000-UPDT-TSQ-SCR-EXIT                       
00590            PERFORM  73000-PROT-UNPROT-FLDS      THRU              
00591                     73000-PROT-UNPROT-FLDS-EXIT                   
00592            PERFORM  72000-FILL-MAP              THRU              
00593                     72000-FILL-MAP-EXIT                           
00594         END-IF                                                    
00595      ELSE                                                         
00596         MOVE WS-YES TO CA-DB2-READ                                
00597         MOVE 1                       TO WS-ITEM-CTR               
00598         MOVE WS-EARLY-DATE           TO                           
00599                   WS-TS-REQ-STRT-DTE(WS-ITEM-CTR)                 
00600                   WS-REQ-STRT-DTE                                 
00601         MOVE WS-DT-DATE              TO                           
00602                   WS-TS-REQ-END-DTE(WS-ITEM-CTR)                  
00603                   WS-REQ-END-DTE                                  
00604         MOVE WS-KEY-AREA             TO CA-KEY-AREA               
00605         PERFORM  82000-UPDT-TSQ-SCR             THRU              
00606                  82000-UPDT-TSQ-SCR-EXIT                          
00607         PERFORM  73000-PROT-UNPROT-FLDS         THRU              
00608                  73000-PROT-UNPROT-FLDS-EXIT                      
00609         PERFORM  72000-FILL-MAP                 THRU              
00610                  72000-FILL-MAP-EXIT                              
00611      END-IF                                                       
00612      PERFORM      79000-DISP-MAP                THRU              
00613                   79000-DISP-MAP-EXIT                             
00614      PERFORM      97000-RETN-PARA               THRU              
00615                   97000-RETN-PARA-EXIT.                           
00616                                                                   
00617  22000-DISP-PARA-EXIT.                                            
00618      EXIT.                                                        
00619                                                                   
00620 *-----------------------------------------------------------------
00621 *                                                                 
00622 *                                                                 
00623 * PARAGRAPH : 23000-RDISP-PARA.                                   
00624 *                                                                 
00625 * LINKAGE TO : 80400-MOVE-HELP-FLDS          82000-UPDT-TSQ-SCR   
00626 *              72000-FILL-MAP                73000-PROT-UNPROT-FLD
00627 *              97000-RETN-PARA                                    
00628 *                                                                 
00629 * DESCRIPTION : This routine is performed when the user presses   
00630 *               PF04 for transfering but does not really get      
00631 *               transferred and returns back to the same pgm.     
00632 *               But when he has pressed PF04, the control goes    
00633 *               to another program thereby all the communication  
00634 *               area values pertaining to your program are over-  
00635 *               written by the called program. So as the first    
00636 *               thing retrieve all those parameters required from 
00637 *               the TSQ( earlier read by 40000-INIT_SETUP) into   
00638 *               CA variables. After retrieving, assemble the map  
00639 *               and display.                                      
00640 *-----------------------------------------------------------------
00641                                                                   
00642  23000-RDISP-PARA.                                                
00643                                                                   
00644      MOVE SPACES                         TO CA-REDISPLAY-FN       
00645      MOVE CA-KEY-AREA                    TO WS-KEY-AREA           
00646      MOVE WS-RETN-DB2-READ(1)            TO CA-DB2-READ.          
00647      MOVE WS-RETN-TOT-PAGE(1)            TO CA-TOT-PAGE-NO.       
00648      MOVE WS-RETN-CUR-PAGE(1)            TO CA-PAGE-NO.           
00649                                                                   
00650      IF CA-READ-DB2                                               
00651         MOVE WS-YES                      TO WS-FIRST-FLD          
00652      ELSE                                                         
00653         MOVE WS-YES                      TO WS-NON-KEY-FLD.       
00654                                                                   
00655      IF CA-LOOKUP OR                                              
00656         CA-HELP                                                   
00657         PERFORM   80400-MOVE-HELP-FLDS          THRU              
00658                   80400-MOVE-HELP-FLDS-EXIT                       
00659         IF CA-LOOKUP                                              
00660            MOVE WS-KEY-AREA              TO CA-KEY-AREA           
00661            PERFORM   82000-UPDT-TSQ-SCR         THRU              
00662                      82000-UPDT-TSQ-SCR-EXIT                      
00663         END-IF                                                    
00664      END-IF.                                                      
00665                                                                   
00666      PERFORM      72000-FILL-MAP                THRU              
00667                   72000-FILL-MAP-EXIT                             
00668      MOVE WS-YES                         TO WS-SCROLL-ACTIVE      
00669      PERFORM      73000-PROT-UNPROT-FLDS        THRU              
00670                   73000-PROT-UNPROT-FLDS-EXIT                     
00671      MOVE SPACES                         TO CA-DB2-COMMIT         
00672                                             CA-KEY-HELP           
00673                                             CA-DELETE-FN          
00674                                             CA-CONFIRM-FN         
00675                                             CA-LOOK-SW            
00676                                             CA-HELP-TYP           
00677                                             CA-HELP-FLD           
00678                                             CA-HELP-VAL           
00679                                             CA-EXIT-FN            
00680      MOVE WS-YES                         TO CA-SCREEN-CLR         
00681                                                                   
00682      IF CA-VIEW-ONLY OR CA-READ-DB2                               
00683         PERFORM   73000-PROT-UNPROT-FLDS        THRU              
00684                   73000-PROT-UNPROT-FLDS-EXIT                     
00685      END-IF.                                                      
00686      PERFORM      79000-DISP-MAP                THRU              
00687                   79000-DISP-MAP-EXIT.                            
00688      MOVE SPACES                         TO CA-HELP-FN            
00689      PERFORM      97000-RETN-PARA               THRU              
00690                   97000-RETN-PARA-EXIT.                           
00691                                                                   
00692  23000-RDISP-PARA-EXIT.                                           
00693      EXIT.                                                        
00694                                                                   
00695 *-----------------------------------------------------------------
00696 *                                                                 
00697 * PARAGRAPH : 24000-RECEIVE-PARA.                                 
00698 *                                                                 
00699 * LINKAGE TO : 78000-RECEIVE-MAP             80000-MAP-TO-TSQ     
00700 *              82000-UPDT-TSQ-SCR            54000-EDIT-FLDS      
00701 *              69000-READ-MSG                72000-FILL-MAP       
00702 *              73000-PROT-UNPROT-FLDS        79000-DISP-MAP       
00703 *              97000-RETN-PARA               69000-READ-MSG       
00704 *              72000-FILL-MAP                73000-PROT-UNPROT-FLD
00705 *              79000-DISP-MAP                97000-RETN-PARA      
00706 *              50000-EDIT-PARA               69000-READ-MSG       
00707 *              72000-FILL-MAP                79000-DISP-MAP       
00708 *              97000-RETN-PARA                                    
00709 *                                                                 
00710 * DESCRIPTION : This routine is done when the user presses the    
00711 *                CTRL key. On sensing the CTRL key, update the    
00712 *                TSQ (the user could have changed some information
00713 *                on the second attempt), and check if the user is 
00714 *                in the Header/Detail portion. If the user is in  
00715 *                Header portion, process the data. Otherwise displ
00716 *                -ay an error.                                    
00717 *-----------------------------------------------------------------
00718                                                                   
00719  24000-RECEIVE-PARA.                                              
00720                                                                   
00721      MOVE SPACES                         TO CA-ERR-SW             
00722                                             CA-ERR-DSC            
00723                                             CA-HELP-FLD           
00724                                             CA-LOOK-SW            
00725                                             CA-HELP-VAL           
00726      IF CA-MAP-LEN > 0                                            
00727         PERFORM   78000-RECEIVE-MAP             THRU              
00728                   78000-RECEIVE-MAP-EXIT                          
00729         PERFORM   80000-MAP-TO-TSQ              THRU              
00730                   80000-MAP-TO-TSQ-EXIT                           
00731         PERFORM   82000-UPDT-TSQ-SCR            THRU              
00732                   82000-UPDT-TSQ-SCR-EXIT                         
00733      END-IF                                                       
00734                                                                   
00735      IF CA-PAGE-NO = ZEROS                                        
00736         MOVE 1                                TO WS-ITEM-CTR      
00737      END-IF                                                       
00738      MOVE WS-ITEM-CTR                         TO I                
00739                                                                   
00740      IF NOT CA-READ-DB2                                           
00741         PERFORM 54000-EDIT-FLDS                  THRU             
00742                 54000-EDIT-FLDS-EXIT                              
00743                 VARYING WS-CTR                                    
00744                 FROM 1 BY 1                                       
00745                 UNTIL WS-CTR > WS-LINE-CTR                        
00746         IF WS-ERR-CDE   NOT             = SPACES                  
00747            MOVE WS-YES                   TO WS-SCROLL-ACTIVE      
00748            MOVE WS-YES                   TO TABLE-ACCESSED        
00749            PERFORM  69000-READ-MSG              THRU              
00750                     69000-READ-MSG-EXIT                           
00751            PERFORM  72000-FILL-MAP              THRU              
00752                     72000-FILL-MAP-EXIT                           
00753            PERFORM  73000-PROT-UNPROT-FLDS      THRU              
00754                     73000-PROT-UNPROT-FLDS-EXIT                   
00755            PERFORM  79000-DISP-MAP              THRU              
00756                     79000-DISP-MAP-EXIT                           
00757            PERFORM  97000-RETN-PARA             THRU              
00758                     97000-RETN-PARA-EXIT                          
00759         END-IF                                                    
00760      END-IF.                                                      
00761                                                                   
00762                                                                   
00763      IF NOT CA-READ-DB2                                           
00764         MOVE WS-INV-ENTER                TO WS-ERR-CDE            
00765         MOVE WS-YES                      TO WS-SCROLL-ACTIVE      
00766         PERFORM 69000-READ-MSG                  THRU              
00767                 69000-READ-MSG-EXIT                               
00768         PERFORM 72000-FILL-MAP                  THRU              
00769                 72000-FILL-MAP-EXIT                               
00770         PERFORM 73000-PROT-UNPROT-FLDS          THRU              
00771                 73000-PROT-UNPROT-FLDS-EXIT                       
00772         PERFORM 79000-DISP-MAP                  THRU              
00773                 79000-DISP-MAP-EXIT                               
00774         PERFORM 97000-RETN-PARA                 THRU              
00775                 97000-RETN-PARA-EXIT                              
00776      END-IF                                                       
00777                                                                   
00778      MOVE SPACES                         TO CA-RECEIVE-FN         
00779                                                                   
00780      IF CA-MAP-LEN > 0 OR ATLEAST-ONE-EDITED OR CA-READ-DB2       
00781         COMPUTE WS-SKIP-CTR = WS-LINE-CTR * CA-PAGE-NO            
00782         PERFORM   50000-EDIT-PARA            THRU                 
00783                   50000-EDIT-PARA-EXIT                            
00784                   VARYING WS-ITEM-CTR                             
00785                   FROM 1 BY 1                                     
00786                   UNTIL WS-ITEM-CTR > WS-TOT-PAGE-CTR             
00787                                                                   
00788         MOVE WS-ITEM-CTR1             TO WS-ITEM-CTR              
00789                                                                   
00790         MOVE WS-YES                   TO WS-NON-KEY-FLD           
00791                                          CA-EDIT-SW               
00792         PERFORM      69000-READ-MSG          THRU                 
00793                      69000-READ-MSG-EXIT                          
00794         PERFORM      72000-FILL-MAP          THRU                 
00795                      72000-FILL-MAP-EXIT                          
00796      END-IF                                                       
00797      PERFORM      79000-DISP-MAP                THRU              
00798                   79000-DISP-MAP-EXIT.                            
00799      PERFORM      97000-RETN-PARA               THRU              
00800                   97000-RETN-PARA-EXIT.                           
00801                                                                   
00802  24000-RECEIVE-PARA-EXIT.                                         
00803      EXIT.                                                        
00804                                                                   
00805                                                                   
00806 *-----------------------------------------------------------------
00807 *                                                                 
00808 *                                                                 
00809 * PARAGRAPH : 26000-SCRFWD-PARA.                                  
00810 *                                                                 
00811 * LINKAGE TO : 81000-READ-TSQ-SCR            78000-RECEIVE-MAP    
00812 *              80000-MAP-TO-TSQ              54000-EDIT-FLDS      
00813 *              82000-UPDT-TSQ-SCR            69000-READ-MSG       
00814 *              72000-FILL-MAP                73000-PROT-UNPROT-FLD
00815 *              79000-DISP-MAP                97000-RETN-PARA      
00816 *              81000-READ-TSQ-SCR            72000-FILL-MAP       
00817 *              73000-PROT-UNPROT-FLDS        60000-ACCESS-TABL    
00818 *              69000-READ-MSG                72000-FILL-MAP       
00819 *              73000-PROT-UNPROT-FLDS        82000-UPDT-TSQ-SCR   
00820 *              87000-UPDT-TSQ-DB2            72000-FILL-MAP       
00821 *              73000-PROT-UNPROT-FLDS        79000-DISP-MAP       
00822 *              97000-RETN-PARA                                    
00823 *                                                                 
00824 * DESCRIPTION : This routine is performed when the user presses   
00825 *               PF08 key. On pressing PF08 key, check if there    
00826 *               more pages in the TSQ(user would have pressed     
00827 *               PF07/PF08 several times). If the data exists in   
00828 *               the TSQ, increment the page number by 1, read the 
00829 *               TSQ into the map and display. If data does not    
00830 *               exist, read it from the DB2 table. If atleast one 
00831 *               data was found, create the map and display. If no 
00832 *               further data exists, display 'Last Page' error msg
00833 *-----------------------------------------------------------------
00834                                                                   
00835  26000-SCRFWD-PARA.                                               
00836                                                                   
00837      MOVE SPACES                         TO CA-SCRLFWD-FN         
00838                                             CA-ERR-SW             
00839                                             CA-ERR-MSG            
00840      IF CA-PAGE-NO > 0                                            
00841         COMPUTE WS-SKIP-CTR = WS-LINE-CTR * CA-PAGE-NO.           
00842                                                                   
00843      MOVE WS-YES                         TO WS-NON-KEY-FLD        
00844                                                                   
00845      IF CA-MAP-LEN > 0                                            
00846         PERFORM   78000-RECEIVE-MAP             THRU              
00847                   78000-RECEIVE-MAP-EXIT                          
00848         PERFORM   80000-MAP-TO-TSQ              THRU              
00849                   80000-MAP-TO-TSQ-EXIT                           
00850      END-IF                                                       
00851      MOVE WS-ITEM-CTR                 TO I                        
00852      PERFORM   54000-EDIT-FLDS                  THRU              
00853                54000-EDIT-FLDS-EXIT                               
00854                VARYING WS-CTR                                     
00855                FROM 1 BY 1                                        
00856                UNTIL WS-CTR > WS-LINE-CTR                         
00857      PERFORM   82000-UPDT-TSQ-SCR               THRU              
00858                82000-UPDT-TSQ-SCR-EXIT                            
00859      IF WS-ERR-CDE                   = SPACES                     
00860         IF WS-SELECT-CNT > 1                                      
00861            MOVE WS-ONE-SEL-ONLY       TO WS-ERR-CDE               
00862         ELSE                                                      
00863            IF WS-SELECT-CNT = 1                                   
00864               MOVE WS-PF11-VIEW-ADDR  TO WS-ERR-CDE               
00865            END-IF                                                 
00866         END-IF                                                    
00867      END-IF                                                       
00868      IF WS-ERR-CDE NOT = SPACES                                   
00869         PERFORM 69000-READ-MSG                  THRU              
00870                 69000-READ-MSG-EXIT                               
00871         MOVE WS-YES                      TO WS-SCROLL-ACTIVE      
00872         MOVE WS-YES                      TO TABLE-ACCESSED        
00873         PERFORM  72000-FILL-MAP                 THRU              
00874                  72000-FILL-MAP-EXIT                              
00875         PERFORM 73000-PROT-UNPROT-FLDS         THRU               
00876                 73000-PROT-UNPROT-FLDS-EXIT                       
00877         PERFORM 79000-DISP-MAP                  THRU              
00878                 79000-DISP-MAP-EXIT                               
00879         PERFORM 97000-RETN-PARA                 THRU              
00880                 97000-RETN-PARA-EXIT                              
00881      END-IF                                                       
00882                                                                   
00883      IF WS-ITEM-CTR < WS-TOT-PAGE-CTR                             
00884         ADD 1                            TO CA-PAGE-NO            
00885         MOVE CA-PAGE-NO                  TO WS-ITEM-CTR           
00886         MOVE WS-YES                      TO CA-SCREEN-CLR         
00887         IF CA-PAGE-NO = WS-PAGE-LIMIT                             
00888            MOVE WS-PAGE-LIMIT-END        TO WS-ERR-CDE            
00889            PERFORM  69000-READ-MSG              THRU              
00890                     69000-READ-MSG-EXIT                           
00891         END-IF                                                    
00892         PERFORM  72000-FILL-MAP                 THRU              
00893                  72000-FILL-MAP-EXIT                              
00894         MOVE WS-YES                      TO WS-SCROLL-ACTIVE      
00895         PERFORM  73000-PROT-UNPROT-FLDS         THRU              
00896                  73000-PROT-UNPROT-FLDS-EXIT                      
00897      ELSE                                                         
00898         IF CA-PAGE-NO NOT = WS-PAGE-LIMIT                         
00899            PERFORM  60000-ACCESS-TABL           THRU              
00900                     60000-ACCESS-TABL-EXIT                        
00901            IF WS-CTR = 0                                          
00902               MOVE WS-LAST-PAGE             TO WS-ERR-CDE         
00903               MOVE WS-YES                   TO WS-SCROLL-ACTIVE   
00904               PERFORM  69000-READ-MSG              THRU           
00905                        69000-READ-MSG-EXIT                        
00906               PERFORM  72000-FILL-MAP              THRU           
00907                        72000-FILL-MAP-EXIT                        
00908               PERFORM  73000-PROT-UNPROT-FLDS      THRU           
00909                        73000-PROT-UNPROT-FLDS-EXIT                
00910            ELSE                                                   
00911               ADD 1                         TO CA-PAGE-NO         
00912               ADD 1                         TO CA-TOT-PAGE-NO     
00913               MOVE CA-PAGE-NO               TO WS-ITEM-CTR        
00914               MOVE CA-TOT-PAGE-NO           TO WS-TOT-PAGE-CTR    
00915               PERFORM    82000-UPDT-TSQ-SCR     THRU              
00916                          82000-UPDT-TSQ-SCR-EXIT                  
00917               PERFORM    72000-FILL-MAP         THRU              
00918                          72000-FILL-MAP-EXIT                      
00919               PERFORM    73000-PROT-UNPROT-FLDS THRU              
00920                          73000-PROT-UNPROT-FLDS-EXIT              
00921         ELSE                                                      
00922               MOVE WS-PAGE-LIMIT-END        TO WS-ERR-CDE         
00923               MOVE WS-YES                   TO WS-SCROLL-ACTIVE   
00924               PERFORM  69000-READ-MSG              THRU           
00925                        69000-READ-MSG-EXIT                        
00926               PERFORM  72000-FILL-MAP              THRU           
00927                        72000-FILL-MAP-EXIT                        
00928               PERFORM  73000-PROT-UNPROT-FLDS      THRU           
00929                        73000-PROT-UNPROT-FLDS-EXIT                
00930         END-IF                                                    
00931      END-IF                                                       
00932      PERFORM    79000-DISP-MAP                  THRU              
00933                 79000-DISP-MAP-EXIT                               
00934      PERFORM    97000-RETN-PARA                 THRU              
00935                 97000-RETN-PARA-EXIT.                             
00936                                                                   
00937  26000-SCRFWD-PARA-EXIT.                                          
00938      EXIT.                                                        
00939                                                                   
00940 *-----------------------------------------------------------------
00941 *                                                                 
00942 *                                                                 
00943 * PARAGRAPH : 27000-SCRBWD-PARA.                                  
00944 *                                                                 
00945 * LINKAGE TO : 81000-READ-TSQ-SCR            78000-RECEIVE-MAP    
00946 *              80000-MAP-TO-TSQ              54000-EDIT-FLDS      
00947 *              82000-UPDT-TSQ-SCR            69000-READ-MSG       
00948 *              72000-FILL-MAP                73000-PROT-UNPROT-FLD
00949 *              79000-DISP-MAP                97000-RETN-PARA      
00950 *              81000-READ-TSQ-SCR            82000-UPDT-TSQ-SCR   
00951 *              87000-UPDT-TSQ-DB2            70000-PROT-KEYS      
00952 *              72000-FILL-MAP                73000-PROT-UNPROT-FLD
00953 *              69000-READ-MSG                72000-FILL-MAP       
00954 *              73000-PROT-UNPROT-FLDS        79000-DISP-MAP       
00955 *              97000-RETN-PARA                                    
00956 *                                                                 
00957 * DESCRIPTION : This routine is performed when the user presses   
00958 *               PF07 key. On pressing PF07 key, check if there    
00959 *               more pages in the TSQ(user would have pressed     
00960 *               PF07/PF08 several times). If the data exists in   
00961 *               the TSQ, decrement the page number by 1, read the 
00962 *               TSQ into the map and display. If data does not    
00963 *               exist, display 'First Page' error msg             
00964 *-----------------------------------------------------------------
00965                                                                   
00966  27000-SCRBWD-PARA.                                               
00967                                                                   
00968      MOVE SPACES                         TO CA-SCRLBWD-FN         
00969                                             CA-ERR-SW             
00970                                             CA-ERR-MSG            
00971                                                                   
00972      IF CA-MAP-LEN > 0                                            
00973         PERFORM   78000-RECEIVE-MAP             THRU              
00974                   78000-RECEIVE-MAP-EXIT                          
00975         PERFORM   80000-MAP-TO-TSQ              THRU              
00976                   80000-MAP-TO-TSQ-EXIT                           
00977      END-IF                                                       
00978      MOVE WS-ITEM-CTR                 TO I                        
00979      PERFORM   54000-EDIT-FLDS                  THRU              
00980                54000-EDIT-FLDS-EXIT                               
00981                VARYING WS-CTR                                     
00982                FROM 1 BY 1                                        
00983                UNTIL WS-CTR > WS-LINE-CTR                         
00984      PERFORM   82000-UPDT-TSQ-SCR               THRU              
00985                82000-UPDT-TSQ-SCR-EXIT                            
00986      IF WS-ERR-CDE                   = SPACES                     
00987         IF WS-SELECT-CNT > 1                                      
00988            MOVE WS-ONE-SEL-ONLY       TO WS-ERR-CDE               
00989         ELSE                                                      
00990            IF WS-SELECT-CNT = 1                                   
00991               MOVE WS-PF11-VIEW-ADDR   TO WS-ERR-CDE              
00992            END-IF                                                 
00993         END-IF                                                    
00994      END-IF                                                       
00995      IF WS-ERR-CDE NOT = SPACES                                   
00996         PERFORM 69000-READ-MSG                 THRU               
00997                 69000-READ-MSG-EXIT                               
00998         MOVE WS-YES                      TO TABLE-ACCESSED        
00999         MOVE WS-YES                      TO WS-SCROLL-ACTIVE      
01000         PERFORM 72000-FILL-MAP                 THRU               
01001                 72000-FILL-MAP-EXIT                               
01002         PERFORM 73000-PROT-UNPROT-FLDS         THRU               
01003                 73000-PROT-UNPROT-FLDS-EXIT                       
01004         PERFORM 79000-DISP-MAP                 THRU               
01005                 79000-DISP-MAP-EXIT                               
01006         PERFORM 97000-RETN-PARA                THRU               
01007                 97000-RETN-PARA-EXIT                              
01008      END-IF                                                       
01009      IF CA-PAGE-NO > 2                                            
01010         COMPUTE WS-SKIP-CTR = WS-LINE-CTR * (CA-PAGE-NO - 2)      
01011      ELSE                                                         
01012         MOVE ZEROS                       TO WS-SKIP-CTR.          
01013                                                                   
01014      IF CA-PAGE-NO > 1                                            
01015         SUBTRACT 1                       FROM CA-PAGE-NO          
01016         MOVE CA-PAGE-NO                  TO WS-ITEM-CTR           
01017         MOVE WS-YES                      TO CA-SCREEN-CLR         
01018         PERFORM   82000-UPDT-TSQ-SCR            THRU              
01019                   82000-UPDT-TSQ-SCR-EXIT                         
01020         MOVE WS-YES                      TO WS-SCROLL-ACTIVE      
01021         PERFORM   72000-FILL-MAP                THRU              
01022                   72000-FILL-MAP-EXIT                             
01023         PERFORM   73000-PROT-UNPROT-FLDS        THRU              
01024                   73000-PROT-UNPROT-FLDS-EXIT.                    
01025                                                                   
01026      MOVE WS-YES                         TO WS-NON-KEY-FLD        
01027                                                                   
01028      IF CA-PAGE-NO = 1                                            
01029         MOVE WS-FIRST-PAGE               TO WS-ERR-CDE            
01030         PERFORM   69000-READ-MSG                THRU              
01031                   69000-READ-MSG-EXIT                             
01032            MOVE WS-YES                   TO WS-SCROLL-ACTIVE      
01033            PERFORM   72000-FILL-MAP             THRU              
01034                      72000-FILL-MAP-EXIT                          
01035            PERFORM  73000-PROT-UNPROT-FLDS      THRU              
01036                     73000-PROT-UNPROT-FLDS-EXIT.                  
01037                                                                   
01038      PERFORM      79000-DISP-MAP                THRU              
01039                   79000-DISP-MAP-EXIT                             
01040      PERFORM      97000-RETN-PARA               THRU              
01041                   97000-RETN-PARA-EXIT.                           
01042                                                                   
01043  27000-SCRBWD-PARA-EXIT.                                          
01044      EXIT.                                                        
01045                                                                   
01046 *-----------------------------------------------------------------
01047 *                                                                 
01048 *                                                                 
01049 * PARAGRAPH : 28000-TRAN-PARA.                                    
01050 *                                                                 
01051 * LINKAGE TO : 78000-RECEIVE-MAP             80000-MAP-TO-TSQ     
01052 *              54000-EDIT-FLDS               82000-UPDT-TSQ-SCR   
01053 *              69000-READ-MSG                72000-FILL-MAP       
01054 *              73000-PROT-UNPROT-FLDS        79000-DISP-MAP       
01055 *              97000-RETN-PARA               82000-UPDT-TSQ-SCR   
01056 *              98000-XCTL-PARA                                    
01057 *                                                                 
01058 * DESCRIPTION : This routine is performed if the user presses PF04
01059 *               key to transfer to another program. As the Commun 
01060 *               area is disturbed on executing the transfer prog, 
01061 *               save those variables into the 1st array before    
01062 *               XFERing control. This saved value is used by the  
01063 *               program on REDISPLAY path                         
01064 *-----------------------------------------------------------------
01065                                                                   
01066  28000-TRAN-PARA.                                                 
01067                                                                   
01068      IF CA-PAGE-NO = 0                                            
01069         MOVE 1                           TO WS-ITEM-CTR           
01070      ELSE                                                         
01071         MOVE CA-PAGE-NO                  TO WS-ITEM-CTR           
01072      END-IF.                                                      
01073                                                                   
01074      IF CA-MAP-LEN > 0                                            
01075         PERFORM   78000-RECEIVE-MAP             THRU              
01076                   78000-RECEIVE-MAP-EXIT                          
01077         PERFORM   80000-MAP-TO-TSQ              THRU              
01078                   80000-MAP-TO-TSQ-EXIT                           
01079      END-IF                                                       
01080      MOVE WS-ITEM-CTR                    TO I                     
01081      PERFORM   82000-UPDT-TSQ-SCR               THRU              
01082                82000-UPDT-TSQ-SCR-EXIT                            
01083      IF WS-ERR-CDE   NOT             = SPACES                     
01084            MOVE WS-YES                   TO WS-SCROLL-ACTIVE      
01085            MOVE WS-YES                   TO TABLE-ACCESSED        
01086            PERFORM  69000-READ-MSG              THRU              
01087                     69000-READ-MSG-EXIT                           
01088            PERFORM  72000-FILL-MAP              THRU              
01089                     72000-FILL-MAP-EXIT                           
01090            PERFORM  73000-PROT-UNPROT-FLDS      THRU              
01091                     73000-PROT-UNPROT-FLDS-EXIT                   
01092            PERFORM  79000-DISP-MAP              THRU              
01093                     79000-DISP-MAP-EXIT                           
01094            PERFORM  97000-RETN-PARA             THRU              
01095                     97000-RETN-PARA-EXIT                          
01096      END-IF.                                                      
01097      PERFORM  73100-PROT-ALL-FLDS               THRU              
01098               73100-PROT-ALL-FLDS-EXIT                            
01099                                                                   
01100      MOVE CA-PAGE-NO                     TO WS-RETN-CUR-PAGE(1)   
01101      MOVE CA-TOT-PAGE-NO                 TO WS-RETN-TOT-PAGE(1)   
01102      MOVE CA-DB2-READ                    TO WS-RETN-DB2-READ(1)   
01103      MOVE WS-ITEM-CTR                    TO WS-CTR                
01104      PERFORM 82000-UPDT-TSQ-SCR                 THRU              
01105              82000-UPDT-TSQ-SCR-EXIT                              
01106              VARYING WS-ITEM-CTR FROM 1 BY 1                      
01107              UNTIL WS-ITEM-CTR > WS-CTR                           
01108      MOVE WS-XCTL-SW                     TO CA-XCTL-SW            
01109      PERFORM  79000-DISP-MAP                    THRU              
01110               79000-DISP-MAP-EXIT                                 
01111      PERFORM  98000-XCTL-PARA                   THRU              
01112               98000-XCTL-PARA-EXIT.                               
01113                                                                   
01114  28000-TRAN-PARA-EXIT.                                            
01115      EXIT.                                                        
01116                                                                   
01117                                                                   
01118 *-----------------------------------------------------------------
01119 *                                                                 
01120 *                                                                 
01121 * PARAGRAPH : 30000-REFRESH-PARA.                                 
01122 *                                                                 
01123 * LINKAGE TO :                               71000-PROT-FLDS      
01124 *              73000-PROT-UNPROT-FLDS        83000-DELETE-TSQ-SCR 
01125 *              88000-DELETE-TSQ-DB2          69000-READ-MSG       
01126 *              79000-DISP-MAP                97000-RETN-PARA      
01127 *                                                                 
01128 * DESCRIPTION : This routine is performed when the user presses   
01129 *               the CLEAR key. On pressing the CLEAR key, delete  
01130 *               all DB2 and SCREEN TSQ's, and set the user to     
01131 *               the NOTICE ID CODE entry field                    
01132 *-----------------------------------------------------------------
01133                                                                   
01134  30000-REFRESH-PARA.                                              
01135                                                                   
01136      MOVE SPACES                         TO CA-REFRESH-FN         
01137      MOVE WS-YES                         TO WS-FIRST-FLD          
01138                                             CA-DB2-READ           
01139      MOVE LOW-VALUES                     TO MAP-INPUT-AREA        
01140      MOVE WS-KEY-MSG                     TO WS-ERR-CDE            
01141      MOVE SPACES                         TO CA-KEY-AREA           
01142      MOVE SPACES                         TO WS-KEY-AREA           
01143      MOVE ZEROS                          TO CA-PAGE-NO            
01144                                             CA-TOT-PAGE-NO        
01145                                             CA-DB2-TSQ-CTR        
01146                                             CA-SCR-TSQ-CTR        
01147      PERFORM      83000-DELETE-TSQ-SCR          THRU              
01148                   83000-DELETE-TSQ-SCR-EXIT                       
01149      PERFORM      88000-DELETE-TSQ-DB2          THRU              
01150                   88000-DELETE-TSQ-DB2-EXIT                       
01151      MOVE 1                       TO WS-ITEM-CTR                  
01152      MOVE WS-EARLY-DATE           TO                              
01153                WS-TS-REQ-STRT-DTE(WS-ITEM-CTR)                    
01154                WS-REQ-STRT-DTE                                    
01155      MOVE WS-DT-DATE              TO                              
01156                WS-TS-REQ-END-DTE(WS-ITEM-CTR)                     
01157                WS-REQ-END-DTE                                     
01158      MOVE WS-KEY-AREA             TO CA-KEY-AREA                  
01159      PERFORM      82000-UPDT-TSQ-SCR            THRU              
01160                   82000-UPDT-TSQ-SCR-EXIT                         
01161      PERFORM      72000-FILL-MAP                THRU              
01162                   72000-FILL-MAP-EXIT                             
01163      PERFORM      73000-PROT-UNPROT-FLDS        THRU              
01164                   73000-PROT-UNPROT-FLDS-EXIT                     
01165                                                                   
01166      PERFORM      69000-READ-MSG                THRU              
01167                   69000-READ-MSG-EXIT                             
01168      PERFORM      79000-DISP-MAP                THRU              
01169                   79000-DISP-MAP-EXIT                             
01170      PERFORM      97000-RETN-PARA               THRU              
01171                   97000-RETN-PARA-EXIT.                           
01172                                                                   
01173  30000-REFRESH-PARA-EXIT.                                         
01174      EXIT.                                                        
01175 *-----------------------------------------------------------------
01176 *                                                                 
01177 *                                                                 
01178 * PARAGRAPH : 31000-HELP-PARA.                                    
01179 *                                                                 
01180 * LINKAGE TO : 82000-UPDT-TSQ-SCR            81000-READ-TSQ-SCR   
01181 *              78000-RECEIVE-MAP             80000-MAP-TO-TSQ     
01182 *              50000-EDIT-PARA               70000-PROT-KEYS      
01183 *              72000-FILL-MAP                73000-PROT-UNPROT-FLD
01184 *              71000-PROT-FLDS               79000-DISP-MAP       
01185 *              98000-XCTL-PARA                                    
01186 *                                                                 
01187 * DESCRIPTION : This routine is performed when the user presses   
01188 *               PF01. As in the case of 28000-TRAN-PARA, the      
01189 *               values are to be saved before doing a transfer.   
01190 *-----------------------------------------------------------------
01191  31000-HELP-PARA.                                                 
01192                                                                   
01193      MOVE WS-SCR-HELP                    TO CA-HELP-TYP           
01194      MOVE SPACES                         TO CA-HELP-FLD           
01195                                             CA-LOOK-SW            
01196                                             CA-HELP-VAL           
01197                                                                   
01198      MOVE CA-PAGE-NO                     TO WS-RETN-CUR-PAGE(1)   
01199      MOVE CA-TOT-PAGE-NO                 TO WS-RETN-TOT-PAGE(1)   
01200      MOVE CA-DB2-READ                    TO WS-RETN-DB2-READ(1)   
01201      MOVE WS-ITEM-CTR                    TO WS-ITEM-CTR1          
01202      MOVE 1                              TO WS-ITEM-CTR           
01203 * Update the 1st array of the tsq with the CA critical details    
01204      PERFORM 82000-UPDT-TSQ-SCR                 THRU              
01205              82000-UPDT-TSQ-SCR-EXIT                              
01206      MOVE WS-ITEM-CTR1                   TO WS-ITEM-CTR           
01207      IF CA-MAP-LEN > 0                                            
01208         PERFORM   78000-RECEIVE-MAP             THRU              
01209                   78000-RECEIVE-MAP-EXIT                          
01210         PERFORM   80000-MAP-TO-TSQ              THRU              
01211                   80000-MAP-TO-TSQ-EXIT                           
01212 * Update the current screen entries into the TSQ                  
01213         PERFORM   82000-UPDT-TSQ-SCR            THRU              
01214                   82000-UPDT-TSQ-SCR-EXIT                         
01215      END-IF                                                       
01216      MOVE WS-KEY-AREA                    TO CA-KEY-AREA           
01217      MOVE WS-XCTL-SW                     TO CA-XCTL-SW            
01218      MOVE BMS-PROT                       TO MAP-FAST-PATH-ATR     
01219                                                                   
01220      MOVE WS-YES                         TO WS-SCROLL-ACTIVE      
01221      PERFORM  73100-PROT-ALL-FLDS               THRU              
01222               73100-PROT-ALL-FLDS-EXIT                            
01223      PERFORM  79000-DISP-MAP                    THRU              
01224               79000-DISP-MAP-EXIT                                 
01225      PERFORM  98000-XCTL-PARA                   THRU              
01226               98000-XCTL-PARA-EXIT.                               
01227                                                                   
01228  31000-HELP-PARA-EXIT.                                            
01229      EXIT.                                                        
01230                                                                   
01231 *-----------------------------------------------------------------
01232 *                                                                 
01233 *                                                                 
01234 * PARAGRAPH : 32000-ADDRESS-PARA.                                 
01235 *                                                                 
01236 * LINKAGE TO : 72000-FILL-MAP                73000-PROT-UNPROT-FLD
01237 *              78000-RECEIVE-MAP             80000-MAP-TO-TSQ     
01238 *              82000-UPDT-TSQ-SCR            54000-EDIT-FLDS      
01239 *              69000-READ-MSG                82000-UPDT-TSQ-SCR   
01240 *              87000-UPDT-TSQ-DB2            79000-DISP-MAP       
01241 *              97000-RETN-PARA               82000-UPDT-TSQ-SCR   
01242 *              98000-XCTL-PARA                                    
01243 *                                                                 
01244 * DESCRIPTION : This routine is performed when the user selects a 
01245 *               record by a '/' to view the Addresses. As in the  
01246 *               case of TRANSFER rtn, save the parameters. Also   
01247 *               pass the row number and column number in which    
01248 *               the records existed in the TSQ through the COMM   
01249 *               AREA so as to enable the ADDRESS program to update
01250 *               the RESEND ACTY SEQ NUM                           
01251 *-----------------------------------------------------------------
01252                                                                   
01253  32000-ADDRESS-PARA.                                              
01254                                                                   
01255                                                                   
01256      IF CA-READ-DB2                                               
01257         MOVE WS-INV-PF11                 TO WS-ERR-CDE            
01258         MOVE WS-YES                      TO WS-SCROLL-ACTIVE      
01259         PERFORM 72000-FILL-MAP                  THRU              
01260                 72000-FILL-MAP-EXIT                               
01261         PERFORM 73000-PROT-UNPROT-FLDS          THRU              
01262                 73000-PROT-UNPROT-FLDS-EXIT                       
01263      ELSE                                                         
01264         IF CA-MAP-LEN > 0                                         
01265            PERFORM   78000-RECEIVE-MAP          THRU              
01266                      78000-RECEIVE-MAP-EXIT                       
01267            PERFORM   80000-MAP-TO-TSQ           THRU              
01268                      80000-MAP-TO-TSQ-EXIT                        
01269            PERFORM   82000-UPDT-TSQ-SCR         THRU              
01270                      82000-UPDT-TSQ-SCR-EXIT                      
01271         END-IF                                                    
01272         MOVE CA-PAGE-NO               TO I                        
01273         PERFORM   54000-EDIT-FLDS               THRU              
01274                   54000-EDIT-FLDS-EXIT                            
01275                   VARYING WS-CTR                                  
01276                   FROM 1 BY 1                                     
01277                   UNTIL WS-CTR > WS-LINE-CTR                      
01278      END-IF                                                       
01279      IF WS-ERR-CDE                   = SPACES                     
01280         IF WS-SELECT-CNT > 1                                      
01281            MOVE WS-ONE-SEL-ONLY       TO WS-ERR-CDE               
01282         ELSE                                                      
01283            IF WS-SELECT-CNT NOT      = 1                          
01284               MOVE WS-ONE-SEL-PF11    TO WS-ERR-CDE               
01285            END-IF                                                 
01286         END-IF                                                    
01287      END-IF                                                       
01288      PERFORM    69000-READ-MSG             THRU                   
01289                 69000-READ-MSG-EXIT                               
01290      PERFORM    82000-UPDT-TSQ-SCR            THRU                
01291                 82000-UPDT-TSQ-SCR-EXIT                           
01292      IF WS-ERR-CDE NOT = SPACES                                   
01293         PERFORM    79000-DISP-MAP             THRU                
01294                    79000-DISP-MAP-EXIT                            
01295         PERFORM    97000-RETN-PARA            THRU                
01296                    97000-RETN-PARA-EXIT                           
01297      END-IF                                                       
01298      MOVE 1                               TO WS-ITEM-CTR          
01299      MOVE CA-PAGE-NO                      TO WS-RETN-CUR-PAGE(1)  
01300      MOVE CA-TOT-PAGE-NO                  TO WS-RETN-TOT-PAGE(1)  
01301      MOVE CA-DB2-READ                     TO WS-RETN-DB2-READ(1)  
01302                                                                   
01303      PERFORM    82000-UPDT-TSQ-SCR            THRU                
01304                 82000-UPDT-TSQ-SCR-EXIT                           
01305                                                                   
01306      MOVE SPACES                          TO CA-PF-KEY            
01307      PERFORM 32100-SET-ADDR-PARM    THRU                          
01308              32100-SET-ADDR-PARM-EXIT.                            
01309                                                                   
01310                                                                   
01311  32000-ADDRESS-PARA-EXIT.                                         
01312      EXIT.                                                        
01313                                                                   
01314  32100-SET-ADDR-PARM.                                             
01315                                                                   
01316      MOVE I                               TO WS-USER-PAGE-NUM     
01317      MOVE WS-CTR4                         TO WS-USER-ROW-NUM      
01318      MOVE WS-NOTI-ACTY-SEQ-N(I, WS-CTR4)  TO WS-ACTY-SEQ-NUM      
01319      PERFORM 69000-SELECT-LNTA22SP              THRU              
01320              69000-SELECT-LNTA22SP-EXIT                           
01321      IF NOT SQL-SUCCESS                                           
01322         MOVE WS-ERR-TYP-DB2              TO WS-ERR-TYP            
01323         MOVE WS-SQLCODE                  TO WS-ERR-ERR            
01324         PERFORM   69000-READ-MSG                THRU              
01325                   69000-READ-MSG-EXIT                             
01326         PERFORM   72000-FILL-MAP                THRU              
01327                   72000-FILL-MAP-EXIT                             
01328         PERFORM   73000-PROT-UNPROT-FLDS        THRU              
01329                   73000-PROT-UNPROT-FLDS-EXIT                     
01330         PERFORM   79000-DISP-MAP                THRU              
01331                   79000-DISP-MAP-EXIT                             
01332         PERFORM   97000-RETN-PARA               THRU              
01333                   97000-RETN-PARA-EXIT                            
01334      ELSE                                                         
01335         MOVE DCLT-NOTI-ACTY     TO WS-USER-NOTI-ACTY              
01336      END-IF                                                       
01337      MOVE NTA-GEN-STAT-IND      TO WS-GEN-STAT-IND(I, WS-CTR4)    
01338      MOVE NTA-RSND-ACTY-SEQ-NUM TO WS-RESEND-ACTY                 
01339      MOVE WS-RESEND-ACTY        TO WS-RSND-ACTY-SEQ-N(I, WS-CTR4) 
01340      MOVE 'NOT1'                TO CA-FAST-NEXT-ID                
01341      MOVE 'F'                   TO CA-TRANSFER-FN                 
01342      MOVE WS-XCTL-SW            TO CA-XCTL-SW                     
01343      MOVE I                     TO WS-ITEM-CTR                    
01344      PERFORM    82000-UPDT-TSQ-SCR            THRU                
01345                 82000-UPDT-TSQ-SCR-EXIT                           
01346      MOVE WS-USER-AREA          TO CA-USER-AREA                   
01347      PERFORM   98000-XCTL-PARA               THRU                 
01348                98000-XCTL-PARA-EXIT.                              
01349                                                                   
01350  32100-SET-ADDR-PARM-EXIT.                                        
01351      EXIT.                                                        
01352                                                                   
01353 *-----------------------------------------------------------------
01354 *                                                                 
01355 *                                                                 
01356 * PARAGRAPH : 40000-INIT-SETUP.                                   
01357 *                                                                 
01358 * LINKAGE TO : 69000-READ-MSG                79000-DISP-MAP       
01359 *              97000-RETN-PARA               69000-READ-MSG       
01360 *              41000-USR-AREA                42000-INIT-DET-AREA  
01361 *              73000-PROT-UNPROT-FLDS        81000-READ-TSQ-SCR   
01362 *              81000-READ-TSQ-SCR                                 
01363 *                                                                 
01364 * DESCRIPTION : This routine is performed everytime. This routine 
01365 *               sets up the environment variables, reads the TSQ  
01366 *               into WORKING STORAGE and sets the LIOA Address    
01367 *-----------------------------------------------------------------
01368                                                                   
01369  40000-INIT-SETUP.                                                
01370      MOVE SPACES                         TO WS-NON-KEY-FLD        
01371      MOVE SPACES                         TO WS-ERR-CDE            
01372      MOVE LOW-VALUES                     TO MAP-INPUT-AREA        
01373      IF EIBCALEN > 0                                              
01374         MOVE DFHCOMMAREA                 TO WS-COMM-AREA          
01375      ELSE                                                         
01376         MOVE WS-INVLD-COMM               TO WS-ERR-CDE            
01377         MOVE WS-YES                      TO WS-FIRST-FLD          
01378         MOVE 'NOTI'                      TO CA-TRANID             
01379         PERFORM   69000-READ-MSG           THRU                   
01380                   69000-READ-MSG-EXIT                             
01381         PERFORM   79000-DISP-MAP           THRU                   
01382                   79000-DISP-MAP-EXIT                             
01383         PERFORM   97000-RETN-PARA          THRU                   
01384                   97000-RETN-PARA-EXIT.                           
01385                                                                   
01386      SET ADDRESS OF LS-TIOA-AREA         TO CA-TIOA-PTR           
01387      IF CA-PAGE-NO = 0 AND                                        
01388         (CA-SCRL-FWD    OR                                        
01389          CA-SCRL-BWD)                                             
01390         MOVE 'Y'                         TO CA-ERR-SW             
01391         MOVE SPACES                      TO CA-SCRLBWD-FN         
01392         MOVE SPACES                      TO CA-SCRLFWD-FN         
01393         MOVE WS-INVLD-KEY                TO WS-ERR-CDE            
01394         PERFORM   69000-READ-MSG           THRU                   
01395                   69000-READ-MSG-EXIT                             
01396         MOVE WS-ERR-AREA                 TO CA-ERR-MSG-1          
01397      END-IF                                                       
01398      MOVE CA-MNTH-S                      TO WS-DT-MNTH            
01399                                             WS-DTR-CUR-MNTH       
01400      MOVE CA-DATE-S                      TO WS-DT-DAY             
01401                                             WS-DTR-CUR-DATE       
01402      MOVE CA-YEAR-S                      TO WS-DT-YEAR            
01403                                             WS-DTR-CUR-YEAR       
01404      MOVE CA-CENTURY                     TO WS-DT-CENTURY         
01405                                             WS-DTR-CUR-CENT       
01406                                                                   
01407      MOVE CA-KEY-AREA                    TO WS-KEY-AREA           
01408      MOVE WS-MAP-NAM                     TO CA-MAP-NAM            
01409      MOVE WS-RETURN-SW                   TO CA-XCTL-SW            
01410      MOVE CA-PAGE-NO                     TO WS-ITEM-CTR           
01411      MOVE WS-LUW-CTR                     TO CA-LAST-LUW-CTR       
01412                                                                   
01413      IF CA-TOT-PAGE-NO              = ZEROS                       
01414         MOVE 1                           TO WS-TOT-PAGE-CTR       
01415      ELSE                                                         
01416         MOVE CA-TOT-PAGE-NO              TO WS-TOT-PAGE-CTR       
01417      END-IF                                                       
01418      IF CA-USER-AREA NOT = SPACES     AND                         
01419         CA-USER-AREA NOT = LOW-VALUES                             
01420         PERFORM   41000-USR-AREA           THRU                   
01421                   41000-USR-AREA-EXIT.                            
01422                                                                   
01423      INITIALIZE   WS-SCR-HDR-AREA(WS-TOT-PAGE-CTR)                
01424      PERFORM      73000-PROT-UNPROT-FLDS   THRU                   
01425                   73000-PROT-UNPROT-FLDS-EXIT                     
01426                                                                   
01427      IF CA-REDISPLAY                                              
01428         MOVE 1                             TO WS-ITEM-CTR         
01429         PERFORM   81000-READ-TSQ-SCR       THRU                   
01430                   81000-READ-TSQ-SCR-EXIT                         
01431         MOVE WS-RETN-DB2-READ(1)            TO CA-DB2-READ        
01432         MOVE WS-RETN-TOT-PAGE(1)            TO CA-TOT-PAGE-NO     
01433         MOVE WS-RETN-CUR-PAGE(1)            TO CA-PAGE-NO         
01434      END-IF                                                       
01435                                                                   
01436      PERFORM      81000-READ-TSQ-SCR       THRU                   
01437                   81000-READ-TSQ-SCR-EXIT                         
01438                   VARYING WS-ITEM-CTR                             
01439                   FROM 1 BY 1                                     
01440                   UNTIL WS-ITEM-CTR > WS-TOT-PAGE-CTR             
01441                                                                   
01442      IF CA-PAGE-NO                  = ZEROS                       
01443         MOVE 1                           TO WS-ITEM-CTR           
01444      ELSE                                                         
01445         MOVE CA-PAGE-NO                  TO WS-ITEM-CTR           
01446      END-IF                                                       
01447                                                                   
01448      EXEC CICS                                                    
01449           HANDLE CONDITION                                        
01450           MAPFAIL (95000-MAP-FAIL)                                
01451           ERROR   (96000-ERR-PARA)                                
01452      END-EXEC.                                                    
01453                                                                   
01454  40000-INIT-SETUP-EXIT.                                           
01455      EXIT.                                                        
01456                                                                   
01457 *-----------------------------------------------------------------
01458 *                                                                 
01459 * PARAGRAPH : 41000-USR-AREA.                                     
01460 *                                                                 
01461 * LINKAGE TO : NONE                                               
01462 *                                                                 
01463 * DESCRIPTION : This routine is not used by this program. Because 
01464 *               it is part of the template, the routine name is   
01465 *               being retained                                    
01466 *-----------------------------------------------------------------
01467                                                                   
01468  41000-USR-AREA.                                                  
01469      CONTINUE.                                                    
01470 *                                                                 
01471 *    If the program has to execute a DB/2 read and display        
01472 *    the map with all fields filled in using the 'Keys' passed    
01473 *    from the calling program. >                                  
01474 *                                                                 
01475 *    IF CA-DISPLAY AND                                            
01476 *       <Key passed thru CA-USR-AREA is not spaces, and not equal 
01477 *        to CA-KEY-AREA, Set WS-DISP-DAT-SW to 'Y' and initialize 
01478 *        CA-KEY-AREA with keys from CA-USR-AREA.>                 
01479 *                                                                 
01480  41000-USR-AREA-EXIT.                                             
01481      EXIT.                                                        
01482                                                                   
01483 *-----------------------------------------------------------------
01484 *                                                                 
01485 * PARAGRAPH : 42000-INIT-DET-AREA.                                
01486 *                                                                 
01487 * LINKAGE TO : NONE                                               
01488 *                                                                 
01489 * DESCRIPTION : This routine initializes the TSQ map area and     
01490 *               protects all selection fields. The detail validn  
01491 *               routine will unprotect the fields on which data   
01492 *               exists thereby leaving non-existent data fields   
01493 *               protected.                                        
01494 *-----------------------------------------------------------------
01495                                                                   
01496  42000-INIT-DET-AREA.                                             
01497                                                                   
01498      INITIALIZE WS-SCR-DET-AREA(WS-TOT-PAGE-CTR + 1, WS-CTR).     
01499      MOVE BMS-PROT                       TO                       
01500           MAP-SEL-FLD-ATR(WS-CTR).                                
01501                                                                   
01502  42000-INIT-DET-AREA-EXIT.                                        
01503      EXIT.                                                        
01504                                                                   
01505 *-----------------------------------------------------------------
01506 *                                                                 
01507 *                                                                 
01508 * PARAGRAPH : 50000-EDIT-PARA.                                    
01509 *                                                                 
01510 * LINKAGE TO : 52000-EDIT-KEYS               82000-UPDT-TSQ-SCR   
01511 *              69000-READ-MSG                72000-FILL-MAP       
01512 *              79000-DISP-MAP                97000-RETN-PARA      
01513 *                                                                 
01514 * DESCRIPTION : This routine validates the Header entry fields.   
01515 *               If all fields are valid, the map is sent with the 
01516 *               data. Otherwise Error is reported back to the user
01517 *-----------------------------------------------------------------
01518                                                                   
01519  50000-EDIT-PARA.                                                 
01520                                                                   
01521      MOVE SPACES                         TO WS-EDIT-ERR           
01522                                             WS-ERR-CDE            
01523                                             CA-ENTER-FN           
01524      MOVE CA-KEY-AREA                    TO WS-KEY-AREA           
01525                                                                   
01526      PERFORM    52000-EDIT-KEYS            THRU                   
01527                 52000-EDIT-KEYS-EXIT.                             
01528                                                                   
01529      IF WS-EDIT-ERROR     AND                                     
01530         NOT CA-HELP       AND                                     
01531         NOT CA-PREVIOUS                                           
01532         MOVE SPACES                      TO CA-CONFIRM-FN         
01533         MOVE WS-YES                      TO CA-SCREEN-CLR         
01534         MOVE WS-ITEM-CTR                 TO CA-PAGE-NO            
01535         PERFORM    69000-READ-MSG          THRU                   
01536                    69000-READ-MSG-EXIT                            
01537         PERFORM    72000-FILL-MAP          THRU                   
01538                    72000-FILL-MAP-EXIT                            
01539         PERFORM    79000-DISP-MAP          THRU                   
01540                    79000-DISP-MAP-EXIT                            
01541         PERFORM    97000-RETN-PARA         THRU                   
01542                    97000-RETN-PARA-EXIT.                          
01543                                                                   
01544      IF NOT WS-EDIT-ERROR AND                                     
01545         WS-ERR-CDE = SPACES AND                                   
01546         NOT CA-HELP       AND                                     
01547         NOT CA-PREVIOUS                                           
01548             MOVE WS-PF-KEY-SEL           TO WS-ERR-CDE.           
01549                                                                   
01550  50000-EDIT-PARA-EXIT.                                            
01551      EXIT.                                                        
01552                                                                   
01553 *-----------------------------------------------------------------
01554 *                                                                 
01555 *                                                                 
01556 * PARAGRAPH : 52000-EDIT-KEYS.                                    
01557 *                                                                 
01558 * LINKAGE TO : 52100-EDIT-END-DTE            52110-EDIT-STRT-DTE  
01559 *              52120-ALL-SPACE               82000-UPDT-TSQ-SCR   
01560 *              69000-READ-MSG                79000-DISP-MAP       
01561 *              97000-RETN-PARA               42000-INIT-DET-AREA  
01562 *              60000-ACCESS-TABL             73000-PROT-UNPROT-FLD
01563 *              72000-FILL-MAP                69000-READ-MSG       
01564 *              82000-UPDT-TSQ-SCR            87000-UPDT-TSQ-DB2   
01565 *              79000-DISP-MAP                97000-RETN-PARA      
01566 *              81200-UPDT-HDRS                                    
01567 *                                                                 
01568 * DESCRIPTION : This routine checks for the validity of dates and 
01569 *               date range. It also checks if no data was entered.
01570 *               On encountering error in date, displays appropriat
01571 *               error                                             
01572 *-----------------------------------------------------------------
01573                                                                   
01574  52000-EDIT-KEYS.                                                 
01575                                                                   
01576       MOVE WS-ITEM-CTR                    TO I                    
01577       MOVE WS-NO                          TO WS-EDIT-ERR          
01578                                                                   
01579       IF CA-READ-DB2                                              
01580         PERFORM 52100-EDIT-END-DTE         THRU                   
01581                 52100-EDIT-END-DTE-EXIT                           
01582         PERFORM 52110-EDIT-STRT-DTE        THRU                   
01583                 52110-EDIT-STRT-DTE-EXIT                          
01584         PERFORM 52120-ALL-SPACE            THRU                   
01585                 52120-ALL-SPACE-EXIT                              
01586         IF WS-ERR-CDE = SPACES                                    
01587            IF WS-TS-REQ-STRT-DTE(I) NOT = SPACES     AND          
01588               WS-TS-REQ-STRT-DTE(I) NOT = LOW-VALUES AND          
01589               WS-TS-REQ-END-DTE(I)  NOT = SPACES AND              
01590               WS-TS-REQ-END-DTE(I)  NOT = LOW-VALUES              
01591                 MOVE WS-TS-REQ-STRT-DTE(I) TO WS-NUM-DATE-1       
01592                 MOVE WS-TS-REQ-END-DTE(I) TO WS-NUM-DATE-2        
01593                 MOVE WS-NUM-YYYY1          TO WS-ONUM-YYYY1       
01594                 MOVE WS-NUM-DD1            TO WS-ONUM-DD1         
01595                 MOVE WS-NUM-MM1            TO WS-ONUM-MM1         
01596                 MOVE WS-NUM-YYYY2          TO WS-ONUM-YYYY2       
01597                 MOVE WS-NUM-DD2            TO WS-ONUM-DD2         
01598                 MOVE WS-NUM-MM2            TO WS-ONUM-MM2         
01599                 IF WS-ONLY-NUM-DATE1 > WS-ONLY-NUM-DATE2          
01600                    MOVE WS-ERROR           TO                     
01601                         WS-TS-REQ-STRT-DTE-EDSW(I)                
01602                    MOVE WS-YES             TO WS-EDIT-ERR         
01603                    MOVE BMS-RED            TO                     
01604                                              MAP-REQ-STRT-DTE-ATR 
01605                    MOVE WS-INIT-CSR        TO                     
01606                                              MAP-REQ-STRT-DTE-LEN 
01607                    MOVE WS-STRT-GRTR-END   TO WS-ERR-CDE          
01608                 END-IF                                            
01609         END-IF                                                    
01610      END-IF                                                       
01611 *                                                                 
01612      IF WS-EDIT-ERROR AND NOT CA-HELP AND NOT CA-PREVIOUS         
01613         MOVE WS-KEY-AREA                 TO CA-KEY-AREA           
01614         PERFORM    82000-UPDT-TSQ-SCR      THRU                   
01615                    82000-UPDT-TSQ-SCR-EXIT                        
01616         PERFORM    69000-READ-MSG          THRU                   
01617                    69000-READ-MSG-EXIT                            
01618         PERFORM    79000-DISP-MAP          THRU                   
01619                    79000-DISP-MAP-EXIT                            
01620         PERFORM    97000-RETN-PARA         THRU                   
01621                    97000-RETN-PARA-EXIT                           
01622      END-IF                                                       
01623                                                                   
01624      IF CA-READ-DB2 AND NOT CA-HELP                               
01625         MOVE WS-KEY-AREA                 TO CA-KEY-AREA           
01626         MOVE 0                           TO WS-TOT-PAGE-CTR       
01627         MOVE 1                           TO WS-ITEM-CTR           
01628         INITIALIZE WS-SCR-HDR-AREA(1)                             
01629         PERFORM    42000-INIT-DET-AREA     THRU                   
01630                    42000-INIT-DET-AREA-EXIT                       
01631                    VARYING WS-CTR                                 
01632                    FROM 1 BY 1                                    
01633                    UNTIL WS-CTR > WS-LINE-CTR                     
01634                                                                   
01635         MOVE WS-NOTI-ID-SEL              TO WS-TS-NOTI-ID-SEL(I)  
01636         MOVE WS-REQ-UID                  TO WS-TS-REQ-UID-SEL(I)  
01637         MOVE WS-REQ-STRT-DTE             TO WS-TS-REQ-STRT-DTE(I) 
01638         MOVE WS-REQ-END-DTE              TO WS-TS-REQ-END-DTE(I)  
01639                                                                   
01640         PERFORM    60000-ACCESS-TABL       THRU                   
01641                    60000-ACCESS-TABL-EXIT                         
01642         IF WS-ERR-CDE = SPACES                                    
01643            MOVE SPACES                   TO CA-DB2-READ           
01644         END-IF                                                    
01645      END-IF                                                       
01646                                                                   
01647      IF WS-ERR-CDE =  WS-NO-RECORD                                
01648         IF WS-CTR = 0                                             
01649           MOVE 0                           TO CA-TOT-PAGE-NO      
01650                                               CA-PAGE-NO          
01651           MOVE  WS-NO-MATCH                TO WS-ERR-CDE          
01652         ELSE                                                      
01653           MOVE  WS-PF-KEY-SEL              TO WS-ERR-CDE          
01654         END-IF                                                    
01655      END-IF                                                       
01656                                                                   
01657      IF WS-CTR > 0                                                
01658         MOVE WS-YES                      TO WS-NON-KEY-FLD        
01659         MOVE WS-YES                      TO TABLE-ACCESSED        
01660         PERFORM    73000-PROT-UNPROT-FLDS  THRU                   
01661                    73000-PROT-UNPROT-FLDS-EXIT                    
01662      END-IF                                                       
01663      PERFORM    72000-FILL-MAP             THRU                   
01664                 72000-FILL-MAP-EXIT                               
01665      PERFORM    69000-READ-MSG             THRU                   
01666                 69000-READ-MSG-EXIT                               
01667      PERFORM    82000-UPDT-TSQ-SCR         THRU                   
01668                 82000-UPDT-TSQ-SCR-EXIT                           
01669      PERFORM    79000-DISP-MAP             THRU                   
01670                 79000-DISP-MAP-EXIT                               
01671      PERFORM    97000-RETN-PARA            THRU                   
01672                 97000-RETN-PARA-EXIT.                             
01673                                                                   
01674 * If the user has changed the Header Field, the program should pop
01675                                                                   
01676 * header fields in all pages in the Temporary Storage.            
01677                                                                   
01678      IF ATLEAST-ONE-EDITED                                        
01679         MOVE WS-KEY-AREA                TO CA-KEY-AREA            
01680         MOVE WS-ITEM-CTR                TO WS-ITEM-CTR1           
01681         PERFORM   81200-UPDT-HDRS          THRU                   
01682                   81200-UPDT-HDRS-EXIT                            
01683                   VARYING WS-ITEM-CTR                             
01684                   FROM 1 BY 1                                     
01685                   UNTIL WS-ITEM-CTR > CA-TOT-PAGE-NO              
01686         MOVE WS-ITEM-CTR1               TO WS-ITEM-CTR            
01687      END-IF.                                                      
01688                                                                   
01689  52000-EDIT-KEYS-EXIT.                                            
01690      EXIT.                                                        
01691                                                                   
01692 *-----------------------------------------------------------------
01693 *                                                                 
01694 *                                                                 
01695 * PARAGRAPH : 52100-EDIT-END-DTE.                                 
01696 *                                                                 
01697 * LINKAGE TO : 52200-VALIDATE-DATE                                
01698 *                                                                 
01699 * DESCRIPTION : This routine validates the END Date entered by the
01700 * user.                                                           
01701 *-----------------------------------------------------------------
01702                                                                   
01703  52100-EDIT-END-DTE.                                              
01704                                                                   
01705      IF WS-TS-REQ-END-DTE(I)    NOT = SPACES AND                  
01706         WS-TS-REQ-END-DTE(I)    NOT = LOW-VALUES                  
01707         MOVE WS-TS-REQ-END-DTE(I)     TO WS-DTR-DATE-1            
01708         MOVE  1                       TO WS-DTR-TYPE              
01709         MOVE  1                       TO WS-DTR-PRC-OPT           
01710         PERFORM 52200-VALIDATE-DATE     THRU                      
01711                 52200-VALIDATE-DATE-EXIT                          
01712         IF WS-DTR-ERR-FLG = SPACES                                
01713           MOVE WS-DTR-DATE-RET       TO                           
01714                                         WS-TS-REQ-END-DTE(I)      
01715                                         WS-NUM-DATE-1             
01716                                         WS-REQ-END-DTE            
01717           INSPECT WS-TS-REQ-END-DTE(I) REPLACING ALL '/' BY '-'   
01718           MOVE WS-DT-DATE            TO WS-NUM-DATE-2             
01719           MOVE WS-NUM-YYYY1          TO WS-ONUM-YYYY1             
01720           MOVE WS-NUM-DD1            TO WS-ONUM-DD1               
01721           MOVE WS-NUM-MM1            TO WS-ONUM-MM1               
01722           MOVE WS-NUM-YYYY2          TO WS-ONUM-YYYY2             
01723           MOVE WS-NUM-DD2            TO WS-ONUM-DD2               
01724           MOVE WS-NUM-MM2            TO WS-ONUM-MM2               
01725           IF WS-ONLY-NUM-DATE1 > WS-ONLY-NUM-DATE2                
01726              MOVE WS-END-LESS-CUR    TO WS-ERR-CDE                
01727              MOVE BMS-RED            TO MAP-REQ-END-DTE-ATR       
01728              MOVE WS-ERROR           TO                           
01729                 WS-TS-REQ-END-DTE-EDSW(I)                         
01730              MOVE WS-YES             TO WS-EDIT-ERR               
01731              MOVE WS-INIT-CSR        TO MAP-REQ-END-DTE-LEN       
01732           ELSE                                                    
01733               MOVE WS-NO                 TO                       
01734                    WS-TS-REQ-END-DTE-EDSW(I)                      
01735               MOVE BMS-GREEN  TO MAP-REQ-END-DTE-ATR              
01736               IF WS-TS-REQ-END-DTE(I) NOT = WS-REQ-END-DTE        
01737                  MOVE WS-TS-REQ-END-DTE(I)                        
01738                       TO WS-REQ-END-DTE                           
01739               END-IF                                              
01740           END-IF                                                  
01741         ELSE                                                      
01742            MOVE WS-ERROR              TO                          
01743                 WS-TS-REQ-END-DTE-EDSW(I)                         
01744            MOVE WS-YES                TO WS-EDIT-ERR              
01745            MOVE WS-INV-DATE-SPEC      TO WS-ERR-CDE               
01746            MOVE BMS-RED               TO MAP-REQ-END-DTE-ATR      
01747            MOVE WS-INIT-CSR           TO MAP-REQ-END-DTE-LEN      
01748         END-IF                                                    
01749      ELSE                                                         
01750         MOVE WS-DT-DATE            TO WS-REQ-END-DTE              
01751         MOVE WS-NO                 TO                             
01752                WS-TS-REQ-END-DTE-EDSW(I)                          
01753         MOVE BMS-GREEN  TO MAP-REQ-END-DTE-ATR                    
01754      END-IF.                                                      
01755                                                                   
01756  52100-EDIT-END-DTE-EXIT.                                         
01757      EXIT.                                                        
01758                                                                   
01759 *-----------------------------------------------------------------
01760 *                                                                 
01761 *                                                                 
01762 * PARAGRAPH : 52110-EDIT-STRT-DTE.                                
01763 *                                                                 
01764 * LINKAGE TO : 52200-VALIDATE-DATE                                
01765 *                                                                 
01766 * DESCRIPTION : This routine validates the START Date entered by  
01767 *               the user                                          
01768 *-----------------------------------------------------------------
01769                                                                   
01770  52110-EDIT-STRT-DTE.                                             
01771                                                                   
01772      IF WS-TS-REQ-STRT-DTE(I)  NOT = SPACES AND                   
01773         WS-TS-REQ-STRT-DTE(I)  NOT = LOW-VALUES                   
01774         MOVE WS-TS-REQ-STRT-DTE(I)    TO WS-DTR-DATE-1            
01775         MOVE  1                       TO WS-DTR-TYPE              
01776         MOVE  1                       TO WS-DTR-PRC-OPT           
01777         PERFORM 52200-VALIDATE-DATE     THRU                      
01778                 52200-VALIDATE-DATE-EXIT                          
01779         IF WS-DTR-ERR-FLG          = SPACES                       
01780           MOVE WS-DTR-DATE-RET       TO                           
01781                                         WS-TS-REQ-STRT-DTE(I)     
01782                                         WS-NUM-DATE-1             
01783                                         WS-REQ-STRT-DTE           
01784           INSPECT WS-TS-REQ-STRT-DTE(I) REPLACING ALL '/' BY '-'  
01785           MOVE WS-DT-DATE            TO WS-NUM-DATE-2             
01786           MOVE WS-NUM-YYYY1          TO WS-ONUM-YYYY1             
01787           MOVE WS-NUM-DD1            TO WS-ONUM-DD1               
01788           MOVE WS-NUM-MM1            TO WS-ONUM-MM1               
01789           MOVE WS-NUM-YYYY2          TO WS-ONUM-YYYY2             
01790           MOVE WS-NUM-DD2            TO WS-ONUM-DD2               
01791           MOVE WS-NUM-MM2            TO WS-ONUM-MM2               
01792           IF WS-ONLY-NUM-DATE1 > WS-ONLY-NUM-DATE2                
01793              MOVE WS-STRT-LESS-CUR   TO WS-ERR-CDE                
01794              MOVE BMS-RED            TO MAP-REQ-STRT-DTE-ATR      
01795              MOVE WS-ERROR           TO                           
01796                 WS-TS-REQ-STRT-DTE-EDSW(I)                        
01797              MOVE WS-YES             TO WS-EDIT-ERR               
01798              MOVE WS-INIT-CSR        TO MAP-REQ-STRT-DTE-LEN      
01799           ELSE                                                    
01800             MOVE WS-NO                 TO                         
01801                  WS-TS-REQ-STRT-DTE-EDSW(I)                       
01802             MOVE BMS-GREEN             TO MAP-REQ-STRT-DTE-ATR    
01803             IF WS-TS-REQ-STRT-DTE(I) NOT = WS-REQ-STRT-DTE        
01804                 MOVE WS-TS-REQ-STRT-DTE(I) TO                     
01805                        WS-REQ-STRT-DTE                            
01806             END-IF                                                
01807           END-IF                                                  
01808         ELSE                                                      
01809            MOVE WS-ERROR              TO                          
01810                 WS-TS-REQ-STRT-DTE-EDSW(I)                        
01811            MOVE WS-YES                TO WS-EDIT-ERR              
01812            MOVE WS-INV-DATE-SPEC      TO WS-ERR-CDE               
01813            MOVE BMS-RED               TO MAP-REQ-STRT-DTE-ATR     
01814            MOVE WS-INIT-CSR           TO MAP-REQ-STRT-DTE-LEN     
01815         END-IF                                                    
01816      ELSE                                                         
01817         MOVE WS-EARLY-DATE           TO WS-REQ-STRT-DTE           
01818         MOVE WS-NO                   TO                           
01819               WS-TS-REQ-STRT-DTE-EDSW(I)                          
01820         MOVE BMS-GREEN               TO MAP-REQ-STRT-DTE-ATR      
01821      END-IF.                                                      
01822                                                                   
01823  52110-EDIT-STRT-DTE-EXIT.                                        
01824      EXIT.                                                        
01825                                                                   
01826 *-----------------------------------------------------------------
01827 *                                                                 
01828 *                                                                 
01829 * PARAGRAPH : 52120-ALL-SPACE.                                    
01830 *                                                                 
01831 * LINKAGE TO : NONE                                               
01832 *                                                                 
01833 * DESCRIPTION : This routine checks if at least one header field  
01834 *               was entered by the user. If not, sets the error   
01835 *               code.                                             
01836 *-----------------------------------------------------------------
01837                                                                   
01838  52120-ALL-SPACE.                                                 
01839      IF WS-TS-REQ-STRT-DTE(I)   = SPACES AND                      
01840         WS-TS-REQ-END-DTE(I)    = SPACES AND                      
01841         WS-TS-NOTI-ID-SEL(I)    = SPACES AND                      
01842         WS-TS-REQ-UID-SEL(I)    = SPACES                          
01843            MOVE WS-ERROR                 TO                       
01844                 WS-TS-NOTI-ID-SEL-EDSW(I)                         
01845            MOVE WS-YES                   TO WS-EDIT-ERR           
01846            MOVE BMS-RED                  TO MAP-NOTI-ID-SEL-ATR   
01847            IF WS-ERR-CDE        = SPACES                          
01848               MOVE WS-INIT-CSR           TO MAP-NOTI-ID-SEL-LEN   
01849               MOVE WS-ONE-SEARCH-REQD    TO WS-ERR-CDE            
01850            END-IF                                                 
01851      END-IF.                                                      
01852                                                                   
01853  52120-ALL-SPACE-EXIT.                                            
01854      EXIT.                                                        
01855                                                                   
01856 *-----------------------------------------------------------------
01857 *                                                                 
01858 *                                                                 
01859 * PARAGRAPH : 52200-VALIDATE-DATE.                                
01860 *                                                                 
01861 * LINKAGE TO : NONE                                               
01862 *                                                                 
01863 * DESCRIPTION : This routine calls the DATE VALIDATION program to 
01864 *               validate the date.                                
01865 *-----------------------------------------------------------------
01866                                                                   
01867  52200-VALIDATE-DATE.                                             
01868      MOVE SPACES                   TO WS-DTR-ERR-FLG              
01869      INSPECT WS-DTR-DATE-1         REPLACING ALL '/' BY '-'       
01870      CALL WS-DATE-RTN USING WS-DTR-FLDS.                          
01871                                                                   
01872  52200-VALIDATE-DATE-EXIT.                                        
01873      EXIT.                                                        
01874                                                                   
01875 *-----------------------------------------------------------------
01876 *                                                                 
01877 *                                                                 
01878 * PARAGRAPH : 54000-EDIT-FLDS.                                    
01879 *                                                                 
01880 * LINKAGE TO : NONE                                               
01881 *                                                                 
01882 * DESCRIPTION : This routine validates the SELECTION field entered
01883 *               by the user for '/' and <SPACE>. Any violation to 
01884 *               the above 2 characters are treated and reported as
01885 *               errors.                                           
01886 *-----------------------------------------------------------------
01887                                                                   
01888  54000-EDIT-FLDS.                                                 
01889                                                                   
01890      MOVE WS-CTR                         TO J                     
01891                                                                   
01892      IF WS-SEL-FLD(I, J)           = WS-SELECT-SW OR              
01893         WS-SEL-FLD(I, J)           = SPACES    OR                 
01894         WS-SEL-FLD(I, J)           = LOW-VALUES                   
01895            MOVE WS-NO                 TO WS-SEL-FLD-EDSW(I, J)    
01896            MOVE BMS-GREEN             TO MAP-SEL-FLD-ATR(J)       
01897            IF WS-SEL-FLD(I, J)     = WS-SELECT-SW                 
01898               ADD   1                 TO WS-SELECT-CNT            
01899               MOVE WS-YES             TO WS-ROW-SELECTED-SW       
01900               MOVE J                  TO WS-CTR4                  
01901            END-IF                                                 
01902      ELSE                                                         
01903        MOVE WS-ERROR                TO WS-SEL-FLD-EDSW(I, J)      
01904        MOVE WS-YES                  TO WS-EDIT-ERR                
01905        MOVE BMS-RED                 TO MAP-SEL-FLD-ATR(J)         
01906        IF WS-ERR-CDE              = SPACES                        
01907           MOVE WS-INIT-CSR          TO MAP-SEL-FLD-LEN(J)         
01908           MOVE WS-INV-SEL-CDE       TO WS-ERR-CDE.                
01909                                                                   
01910  54000-EDIT-FLDS-EXIT.                                            
01911      EXIT.                                                        
01912                                                                   
01913                                                                   
01914 *-----------------------------------------------------------------
01915 *                                                                 
01916 *                                                                 
01917 * PARAGRAPH : 60000-ACCESS-TABL.                                  
01918 *                                                                 
01919 * LINKAGE TO : 61000-OPEN-LNTA21CD                                
01920 *                                                                 
01921 * DESCRIPTION : This routine sets the EARLY date and END date for 
01922 *               the date fields to fetch information( if date flds
01923 *               were not entered by the user)                     
01924 *-----------------------------------------------------------------
01925                                                                   
01926  60000-ACCESS-TABL.                                               
01927                                                                   
01928      MOVE CA-KEY-AREA                    TO WS-KEY-AREA           
01929                                                                   
01930      IF WS-NOTI-ID-SEL                 = SPACES OR                
01931         WS-NOTI-ID-SEL                 = LOW-VALUES               
01932         MOVE WS-NOTI-ID-UND              TO WS-DB-NOTI-ID-SEL     
01933      ELSE                                                         
01934         MOVE WS-NOTI-ID-SEL              TO WS-DB-NOTI-ID-SEL     
01935      END-IF                                                       
01936                                                                   
01937      IF WS-REQ-UID                     = SPACES OR                
01938         WS-REQ-UID                     = LOW-VALUES               
01939         MOVE WS-REQ-UID-UND              TO WS-DB-REQ-UID         
01940      ELSE                                                         
01941         MOVE WS-REQ-UID                  TO WS-DB-REQ-UID         
01942      END-IF                                                       
01943                                                                   
01944      IF WS-REQ-STRT-DTE        NOT     = SPACES AND               
01945         WS-REQ-STRT-DTE        NOT     = LOW-VALUES               
01946         MOVE WS-REQ-STRT-DTE             TO WS-DB-REQ-STRT-DTE    
01947      ELSE                                                         
01948         MOVE WS-EARLY-DATE               TO WS-DB-REQ-STRT-DTE    
01949                                             WS-REQ-STRT-DTE       
01950      END-IF                                                       
01951                                                                   
01952      IF WS-REQ-END-DTE         NOT     = SPACES AND               
01953         WS-REQ-END-DTE         NOT     = LOW-VALUES               
01954         MOVE WS-REQ-END-DTE              TO WS-DB-REQ-END-DTE     
01955      ELSE                                                         
01956         MOVE WS-DT-DATE                  TO WS-DB-REQ-END-DTE     
01957                                             WS-REQ-END-DTE        
01958      END-IF                                                       
01959      INSPECT WS-DB-REQ-END-DTE  REPLACING ALL '-' BY '/'.         
01960      INSPECT WS-DB-REQ-STRT-DTE REPLACING ALL '-' BY '/'.         
01961                                                                   
01962      PERFORM 61000-OPEN-LNTA21CD           THRU                   
01963              61000-OPEN-LNTA21CD-EXIT.                            
01964                                                                   
01965  60000-ACCESS-TABL-EXIT.                                          
01966      EXIT.                                                        
01967                                                                   
01968 *-----------------------------------------------------------------
01969 *                                                                 
01970 *                                                                 
01971 * PARAGRAPH : 61000-OPEN-LNTA21CD.                                
01972 *                                                                 
01973 * LINKAGE TO : 69000-OPEN-LNTA21CD           69000-READ-MSG       
01974 *              61200-FECH-LNTA21CD           61400-CLOS-LNTA21CD  
01975 *                                                                 
01976 * DESCRIPTION : This routine OPENS the DB2 table for processing.  
01977 *-----------------------------------------------------------------
01978                                                                   
01979  61000-OPEN-LNTA21CD.                                             
01980                                                                   
01981      MOVE ZEROS                          TO WS-CTR                
01982                                                                   
01983      PERFORM 69000-OPEN-LNTA21CD           THRU                   
01984              69000-OPEN-LNTA21CD-EXIT.                            
01985                                                                   
01986      IF NOT SQL-SUCCESS                                           
01987         MOVE WS-ERR-TYP-DB2              TO WS-ERR-TYP            
01988         MOVE WS-SQLCODE                  TO WS-ERR-ERR            
01989         MOVE WS-YES                      TO WS-FIRST-FLD          
01990         PERFORM    69000-READ-MSG          THRU                   
01991                    69000-READ-MSG-EXIT                            
01992      ELSE                                                         
01993      IF SQL-SUCCESS                                               
01994         PERFORM    61200-FECH-LNTA21CD     THRU                   
01995                    61200-FECH-LNTA21CD-EXIT                       
01996                    UNTIL WS-NOT-FND  OR                           
01997                    WS-CTR = WS-LINE-CTR                           
01998         PERFORM    61400-CLOS-LNTA21CD     THRU                   
01999                    61400-CLOS-LNTA21CD-EXIT.                      
02000                                                                   
02001  61000-OPEN-LNTA21CD-EXIT.                                        
02002      EXIT.                                                        
02003                                                                   
02004                                                                   
02005 *-----------------------------------------------------------------
02006 *                                                                 
02007 *                                                                 
02008 * PARAGRAPH : 61200-FECH-LNTA21CD.                                
02009 *                                                                 
02010 * LINKAGE TO : 69000-FETCH-LNTA21CD          69000-READ-MSG       
02011 *              69000-READ-MSG                42000-INIT-DET-AREA  
02012 *              80600-MOVE-TO-TSQ                                  
02013 *                                                                 
02014 * DESCRIPTION : This routine fetches records for the criteria     
02015 *               specified by the user. The fetching is stopped    
02016 *               when there are no more information available or   
02017 *               when 10 records have been read (the screen can    
02018 *               hold only 10 records)                             
02019 *-----------------------------------------------------------------
02020                                                                   
02021  61200-FECH-LNTA21CD.                                             
02022                                                                   
02023      PERFORM 69000-FETCH-LNTA21CD          THRU                   
02024              69000-FETCH-LNTA21CD-EXIT.                           
02025                                                                   
02026      IF SQL-NOTFND                                                
02027          MOVE WS-YES                   TO WS-NO-ROWS              
02028         IF WS-CTR  = 0                                            
02029            MOVE WS-ERR-TYP-DB2           TO WS-ERR-TYP            
02030            MOVE WS-SQLCODE               TO WS-ERR-ERR            
02031            MOVE WS-YES                   TO WS-FIRST-FLD          
02032            PERFORM    69000-READ-MSG       THRU                   
02033                       69000-READ-MSG-EXIT                         
02034         END-IF                                                    
02035      ELSE                                                         
02036      IF NOT SQL-SUCCESS                                           
02037         MOVE WS-ERR-TYP-DB2              TO WS-ERR-TYP            
02038         MOVE WS-YES                      TO WS-NO-ROWS            
02039         MOVE WS-SQLCODE                  TO WS-ERR-ERR            
02040         MOVE WS-YES                      TO WS-FIRST-FLD          
02041         PERFORM       69000-READ-MSG       THRU                   
02042                       69000-READ-MSG-EXIT                         
02043      ELSE                                                         
02044      IF SQL-SUCCESS                                               
02045         MOVE SPACES                      TO CA-DB2-READ           
02046         ADD 1                            TO WS-CTR1               
02047         IF WS-CTR1 > WS-SKIP-CTR                                  
02048            ADD 1                         TO WS-CTR                
02049            MOVE WS-YES                   TO CA-SCREEN-CLR         
02050            MOVE WS-YES                   TO TABLE-ACCESSED        
02051            MOVE 'NTA '                   TO WS-TMP-TBL-CDE        
02052            IF WS-CTR = 1                                          
02053               IF CA-PAGE-NO = ZEROS                               
02054                  MOVE 1                  TO CA-PAGE-NO            
02055                                             CA-TOT-PAGE-NO        
02056               ELSE                                                
02057                  ADD  1                  TO WS-ITEM-CTR           
02058               END-IF                                              
02059               INITIALIZE   WS-SCR-HDR-AREA(WS-TOT-PAGE-CTR + 1)   
02060               PERFORM 42000-INIT-DET-AREA  THRU                   
02061                       42000-INIT-DET-AREA-EXIT                    
02062                       VARYING WS-CTR                              
02063                       FROM 1 BY 1                                 
02064                       UNTIL WS-CTR > WS-LINE-CTR                  
02065               MOVE 1                     TO WS-CTR                
02066            END-IF                                                 
02067            PERFORM    80600-MOVE-TO-TSQ    THRU                   
02068                       80600-MOVE-TO-TSQ-EXIT.                     
02069                                                                   
02070  61200-FECH-LNTA21CD-EXIT.                                        
02071      EXIT.                                                        
02072                                                                   
02073                                                                   
02074 *-----------------------------------------------------------------
02075 *                                                                 
02076 *                                                                 
02077 * PARAGRAPH : 61400-CLOS-LNTA21CD.                                
02078 *                                                                 
02079 * LINKAGE TO : 69000-CLOSE-LNTA21CD          69000-READ-MSG       
02080 *                                                                 
02081 * DESCRIPTION : This routine CLOSES the DB2 table opened earlier. 
02082 *-----------------------------------------------------------------
02083                                                                   
02084  61400-CLOS-LNTA21CD.                                             
02085                                                                   
02086      PERFORM 69000-CLOSE-LNTA21CD          THRU                   
02087              69000-CLOSE-LNTA21CD-EXIT                            
02088      IF NOT SQL-SUCCESS                                           
02089         MOVE WS-ERR-TYP-DB2              TO WS-ERR-TYP            
02090         MOVE WS-SQLCODE                  TO WS-ERR-ERR            
02091         MOVE WS-YES                      TO WS-FIRST-FLD          
02092         PERFORM    69000-READ-MSG          THRU                   
02093                    69000-READ-MSG-EXIT.                           
02094                                                                   
02095  61400-CLOS-LNTA21CD-EXIT.                                        
02096      EXIT.                                                        
02097                                                                   
02098                                                                   
02099                                                                   
02100 *-----------------------------------------------------------------
02101 *                                                                 
02102 *                                                                 
02103 * PARAGRAPH : 69000-READ-MSG.                                     
02104 *                                                                 
02105 * LINKAGE TO : NONE                                               
02106 *                                                                 
02107 * DESCRIPTION : This routine reads the error message by making    
02108 *               use of error code set by different routines       
02109 *-----------------------------------------------------------------
02110                                                                   
02111  69000-READ-MSG.                                                  
02112      IF WS-ERR-CDE NOT = SPACES                                   
02113         MOVE WS-ERR-CDE                     TO EMT-ERR-MSG-CDE    
02114         EXEC SQL                                                  
02115              SELECT ERR_MSG_DSC                                   
02116              INTO   :EMT-ERR-MSG-DSC                              
02117              FROM   T_ERR_MSG_TBL                                 
02118              WHERE  ERR_MSG_CDE = :EMT-ERR-MSG-CDE                
02119         END-EXEC                                                  
02120                                                                   
02121         MOVE SQLCODE                       TO SQL-SQLCODE         
02122         IF SQL-NOTFND                                             
02123            MOVE 'Undefined Error'          TO WS-ERR-DSC          
02124         ELSE                                                      
02125            IF NOT SQL-SUCCESS                                     
02126               MOVE 'D'                        TO WS-ERR-TYP       
02127               MOVE SQL-SQLCODE                TO WS-ERR-ERR       
02128               MOVE 'Error in ERR_MSG_TBL'     TO WS-ERR-DSC       
02129            ELSE                                                   
02130               IF SQL-SUCCESS                                      
02131                  MOVE EMT-ERR-MSG-DSC            TO WS-ERR-DSC    
02132               END-IF                                              
02133            END-IF                                                 
02134         END-IF                                                    
02135         MOVE WS-ERR-AREA                   TO MAP-MSG-DAT-O       
02136      END-IF.                                                      
02137                                                                   
02138  69000-READ-MSG-EXIT.                                             
02139      EXIT.                                                        
02140                                                                   
02141                                                                   
02142 *       EXEC SQL                                                  
02143 *            INCLUDE LNTA21CP                                     
02144  ++INCLUDE LNTA21CP                                               
02145 *2                                                                
02146 *       END-EXEC.                                                 
02147                                                                   
02148 *       EXEC SQL                                                  
02149 *            INCLUDE LNTA22SP                                     
02150  ++INCLUDE LNTA22SP                                               
02151 *2                                                                
02152 *       END-EXEC.                                                 
02153                                                                   
02154 *-----------------------------------------------------------------
02155 *                                                                 
02156 *                                                                 
02157 * PARAGRAPH : 72000-FILL-MAP.                                     
02158 *                                                                 
02159 * LINKAGE TO : 72200-FILL-MAP-1                                   
02160 *                                                                 
02161 * DESCRIPTION : This routine fills the map from the TSQ before    
02162 *               displaying the screen with updated information.   
02163 *-----------------------------------------------------------------
02164                                                                   
02165  72000-FILL-MAP.                                                  
02166                                                                   
02167      MOVE WS-ITEM-CTR                    TO I                     
02168                                                                   
02169      MOVE WS-NOTI-ID-SEL                 TO MAP-NOTI-ID-SEL-O     
02170      MOVE WS-REQ-UID                     TO MAP-REQ-UID-O         
02171      MOVE WS-REQ-STRT-DTE                TO MAP-REQ-STRT-DTE-O    
02172      IF WS-TS-REQ-STRT-DTE-EDSW(I) NOT = WS-ERROR                 
02173         INSPECT MAP-REQ-STRT-DTE-O REPLACING ALL '/' BY '-'       
02174      END-IF                                                       
02175      MOVE WS-REQ-END-DTE                 TO MAP-REQ-END-DTE-O     
02176      IF WS-TS-REQ-END-DTE-EDSW(I) NOT = WS-ERROR                  
02177         INSPECT MAP-REQ-END-DTE-O REPLACING ALL '/' BY '-'        
02178      END-IF                                                       
02179      IF NOT CA-READ-DB2                                           
02180         MOVE I                           TO MAP-D-PAGE-O          
02181      ELSE                                                         
02182         MOVE SPACES                      TO MAP-D-PAGE-O          
02183      END-IF                                                       
02184      IF CA-PAGE-NO NOT = 0                                        
02185         PERFORM  72200-FILL-MAP-1        THRU                     
02186                  72200-FILL-MAP-1-EXIT                            
02187                  VARYING WS-CTR                                   
02188                  FROM WS-LINE-CTR BY -1                           
02189                  UNTIL WS-CTR < 1                                 
02190      END-IF.                                                      
02191                                                                   
02192  72000-FILL-MAP-EXIT.                                             
02193      EXIT.                                                        
02194                                                                   
02195 *-----------------------------------------------------------------
02196 *                                                                 
02197 *                                                                 
02198 * PARAGRAPH : 72200-FILL-MAP-1.                                   
02199 *                                                                 
02200 * LINKAGE TO : NONE                                               
02201 *                                                                 
02202 * DESCRIPTION : This routine fills the detail portion of the map  
02203 *               with data from the TSQ.                           
02204 *-----------------------------------------------------------------
02205                                                                   
02206  72200-FILL-MAP-1.                                                
02207                                                                   
02208      MOVE WS-CTR                         TO J                     
02209                                                                   
02210      MOVE WS-SEL-FLD(I, J)               TO MAP-SEL-FLD-O(J)      
02211      MOVE WS-NOTI-ID-CDE(I, J)           TO MAP-NOTI-ID-CDE-O(J)  
02212      MOVE WS-NOTI-ACTY-SEQ-N(I, J)       TO                       
02213           MAP-NOTI-ACTY-SEQ-N-O(J)                                
02214      MOVE WS-NOTI-REQ-UID(I, J)          TO MAP-NOTI-REQ-UID-O(J) 
02215      MOVE WS-NOTI-REQ-DTE(I, J)          TO MAP-NOTI-REQ-DTE-O(J) 
02216      INSPECT MAP-NOTI-REQ-DTE-O(J) REPLACING ALL '/' BY '-'       
02217      MOVE WS-GEN-STAT-IND(I, J)          TO MAP-GEN-STAT-IND-O(J) 
02218      MOVE WS-RSND-ACTY-SEQ-N(I, J)       TO                       
02219           MAP-RSND-ACTY-SEQ-N-O(J).                               
02220      IF MAP-RSND-ACTY-SEQ-N-O(J) = '000000'                       
02221         MOVE LOW-VALUES                  TO                       
02222              MAP-RSND-ACTY-SEQ-N-O(J).                            
02223                                                                   
02224                                                                   
02225  72200-FILL-MAP-1-EXIT.                                           
02226      EXIT.                                                        
02227                                                                   
02228                                                                   
02229 *-----------------------------------------------------------------
02230 *                                                                 
02231 *                                                                 
02232 * PARAGRAPH : 73000-PROT-UNPROT-FLDS.                             
02233 *                                                                 
02234 * LINKAGE TO : 73200-PROT-DET-FLDS           73300-PROT-HDR-FLDS  
02235 *                                                                 
02236 * DESCRIPTION : This routine protects/unprotects the fields in the
02237 *               map depending on whether the user is in the header
02238 *               or the detail portion                             
02239 *-----------------------------------------------------------------
02240                                                                   
02241  73000-PROT-UNPROT-FLDS.                                          
02242                                                                   
02243      IF CA-READ-DB2                                               
02244         MOVE WS-YES                      TO WS-FIRST-FLD          
02245         PERFORM   73200-PROT-DET-FLDS      THRU                   
02246                   73200-PROT-DET-FLDS-EXIT                        
02247                   VARYING WS-CTR                                  
02248                   FROM 1 BY 1                                     
02249                   UNTIL WS-CTR > WS-LINE-CTR                      
02250      ELSE                                                         
02251         MOVE WS-YES                      TO WS-NON-KEY-FLD        
02252         PERFORM   73300-PROT-HDR-FLDS      THRU                   
02253                   73300-PROT-HDR-FLDS-EXIT                        
02254                   VARYING WS-CTR                                  
02255                   FROM 1 BY 1                                     
02256                   UNTIL WS-CTR > WS-LINE-CTR                      
02257      END-IF.                                                      
02258                                                                   
02259  73000-PROT-UNPROT-FLDS-EXIT.                                     
02260      EXIT.                                                        
02261                                                                   
02262  73100-PROT-ALL-FLDS.                                             
02263                                                                   
02264      MOVE BMS-PROT                       TO MAP-NOTI-ID-SEL-ATR   
02265                                             MAP-REQ-UID-ATR       
02266                                             MAP-REQ-STRT-DTE-ATR  
02267                                             MAP-FAST-PATH-ATR     
02268                                             MAP-REQ-END-DTE-ATR.  
02269      PERFORM 73110-PROT-ALL-DET         THRU                      
02270              73110-PROT-ALL-DET-EXIT                              
02271              VARYING WS-CTR FROM 1 BY 1 UNTIL                     
02272              WS-CTR > WS-LINE-CTR.                                
02273                                                                   
02274  73100-PROT-ALL-FLDS-EXIT.                                        
02275       EXIT.                                                       
02276                                                                   
02277  73110-PROT-ALL-DET.                                              
02278      MOVE BMS-PROT                      TO                        
02279           MAP-NOTI-ID-CDE-ATR(WS-CTR)                             
02280           MAP-NOTI-ACTY-SEQ-N-ATR(WS-CTR)                         
02281           MAP-NOTI-REQ-UID-ATR(WS-CTR)                            
02282           MAP-NOTI-REQ-DTE-ATR(WS-CTR)                            
02283           MAP-GEN-STAT-IND-ATR(WS-CTR)                            
02284           MAP-RSND-ACTY-SEQ-N-ATR(WS-CTR)                         
02285           MAP-SEL-FLD-ATR(WS-CTR).                                
02286                                                                   
02287  73110-PROT-ALL-DET-EXIT.                                         
02288       EXIT.                                                       
02289                                                                   
02290 *-----------------------------------------------------------------
02291 *                                                                 
02292 *                                                                 
02293 * PARAGRAPH : 73200-PROT-DET-FLDS.                                
02294 *                                                                 
02295 * LINKAGE TO : NONE                                               
02296 *                                                                 
02297 * DESCRIPTION : This routine protects/Unprotects the Detail fields
02298 *               depending on whether the user is in the header or 
02299 *               detail portion                                    
02300 *-----------------------------------------------------------------
02301                                                                   
02302  73200-PROT-DET-FLDS.                                             
02303                                                                   
02304      IF WS-CTR                           =  1                     
02305         MOVE BMS-UNPROT                  TO MAP-NOTI-ID-SEL-ATR   
02306                                             MAP-REQ-UID-ATR       
02307                                             MAP-REQ-STRT-DTE-ATR  
02308                                             MAP-REQ-END-DTE-ATR   
02309      END-IF                                                       
02310      MOVE BMS-PROT                       TO                       
02311           MAP-SEL-FLD-ATR(WS-CTR)                                 
02312           MAP-NOTI-ID-CDE-ATR(WS-CTR)                             
02313           MAP-NOTI-ACTY-SEQ-N-ATR(WS-CTR)                         
02314           MAP-NOTI-REQ-UID-ATR(WS-CTR)                            
02315           MAP-NOTI-REQ-DTE-ATR(WS-CTR)                            
02316           MAP-GEN-STAT-IND-ATR(WS-CTR)                            
02317           MAP-RSND-ACTY-SEQ-N-ATR(WS-CTR).                        
02318                                                                   
02319  73200-PROT-DET-FLDS-EXIT.                                        
02320      EXIT.                                                        
02321                                                                   
02322                                                                   
02323 *-----------------------------------------------------------------
02324 *                                                                 
02325 *                                                                 
02326 * PARAGRAPH : 73300-PROT-HDR-FLDS.                                
02327 *                                                                 
02328 * LINKAGE TO : NONE                                               
02329 *                                                                 
02330 * DESCRIPTION : This routine protects the Header flds when the    
02331 *               user is in the detail portion                     
02332 *-----------------------------------------------------------------
02333                                                                   
02334  73300-PROT-HDR-FLDS.                                             
02335                                                                   
02336      IF WS-CTR = 1                                                
02337         MOVE BMS-PROT                   TO MAP-NOTI-ID-SEL-ATR    
02338                                            MAP-REQ-UID-ATR        
02339                                            MAP-REQ-STRT-DTE-ATR   
02340                                            MAP-REQ-END-DTE-ATR    
02341      END-IF                                                       
02342                                                                   
02343      MOVE BMS-PROT                      TO                        
02344           MAP-NOTI-ID-CDE-ATR(WS-CTR)                             
02345           MAP-NOTI-ACTY-SEQ-N-ATR(WS-CTR)                         
02346           MAP-NOTI-REQ-UID-ATR(WS-CTR)                            
02347           MAP-NOTI-REQ-DTE-ATR(WS-CTR)                            
02348           MAP-GEN-STAT-IND-ATR(WS-CTR)                            
02349           MAP-RSND-ACTY-SEQ-N-ATR(WS-CTR).                        
02350                                                                   
02351      IF TABLE-ACCESSED                  NOT = WS-YES              
02352         MOVE BMS-PROT                   TO                        
02353              MAP-SEL-FLD-ATR(WS-CTR).                             
02354                                                                   
02355      IF WS-SCROLL-ACTIVE                 = WS-YES                 
02356         IF MAP-NOTI-ID-CDE-O(WS-CTR) NOT = SPACES AND             
02357            MAP-NOTI-ID-CDE-O(WS-CTR) NOT = LOW-VALUES             
02358               IF MAP-SEL-FLD-ATR(WS-CTR) NOT = BMS-RED            
02359                  MOVE BMS-UNPROT         TO                       
02360                       MAP-SEL-FLD-ATR(WS-CTR)                     
02361               END-IF                                              
02362         ELSE                                                      
02363                  MOVE BMS-PROT           TO                       
02364                       MAP-SEL-FLD-ATR(WS-CTR)                     
02365         END-IF.                                                   
02366                                                                   
02367  73300-PROT-HDR-FLDS-EXIT.                                        
02368      EXIT.                                                        
02369                                                                   
02370                                                                   
02371 *-----------------------------------------------------------------
02372 *                                                                 
02373 *                                                                 
02374 * PARAGRAPH : 78000-RECEIVE-MAP.                                  
02375 *                                                                 
02376 * LINKAGE TO : NONE                                               
02377 *                                                                 
02378 * DESCRIPTION : This routine receives the map and sets it ready   
02379 *               for processing.                                   
02380 *-----------------------------------------------------------------
02381                                                                   
02382  78000-RECEIVE-MAP.                                               
02383                                                                   
02384      EXEC CICS                                                    
02385           RECEIVE                                                 
02386           MAP     (WS-MAP-NAM)                                    
02387           INTO    (MAP-INPUT-AREA)                                
02388           FROM    (LS-TIOA-AREA)                                  
02389           LENGTH  (CA-MAP-LEN)                                    
02390      END-EXEC.                                                    
02391                                                                   
02392  78000-RECEIVE-MAP-EXIT.                                          
02393      EXIT.                                                        
02394                                                                   
02395                                                                   
02396 *-----------------------------------------------------------------
02397 *                                                                 
02398 *                                                                 
02399 * PARAGRAPH : 79000-DISP-MAP.                                     
02400 *                                                                 
02401 * LINKAGE TO : NONE                                               
02402 *                                                                 
02403 * DESCRIPTION : This routine displays the updated map             
02404 *-----------------------------------------------------------------
02405                                                                   
02406  79000-DISP-MAP.                                                  
02407      IF CA-HELP-FN = SPACES                                       
02408         IF NOT CA-READ-DB2                                        
02409            IF WS-EDIT-ERR NOT = WS-YES                            
02410               MOVE WS-INIT-CSR              TO MAP-SEL-FLD-LEN(1) 
02411            END-IF                                                 
02412         ELSE                                                      
02413            IF WS-EDIT-ERR NOT = WS-YES                            
02414               MOVE WS-INIT-CSR              TO MAP-NOTI-ID-SEL-LEN
02415            END-IF                                                 
02416         END-IF                                                    
02417      END-IF                                                       
02418      MOVE CA-DATE                        TO MAP-DAT-O             
02419      MOVE CA-TRMID                       TO WS-TRMID              
02420      MOVE CA-USERID                      TO WS-USERID             
02421      MOVE WS-USER-TRM                    TO MAP-USR-ID-O          
02422                                                                   
02423                                                                   
02424                                                                   
02425      IF CA-SCREEN-ERASE                                           
02426         MOVE SPACES                      TO CA-SCREEN-CLR         
02427         EXEC CICS                                                 
02428              SEND                                                 
02429              MAP (WS-MAP-NAM)                                     
02430              FROM (MAP-OUTPUT-AREA)                               
02431              CURSOR                                               
02432              ERASE                                                
02433         END-EXEC                                                  
02434      ELSE                                                         
02435         EXEC CICS                                                 
02436              SEND                                                 
02437              MAP (WS-MAP-NAM)                                     
02438              FROM (MAP-OUTPUT-AREA)                               
02439              CURSOR                                               
02440              DATAONLY                                             
02441         END-EXEC.                                                 
02442                                                                   
02443  79000-DISP-MAP-EXIT.                                             
02444      EXIT.                                                        
02445                                                                   
02446                                                                   
02447                                                                   
02448 *-----------------------------------------------------------------
02449 *                                                                 
02450 *                                                                 
02451 * PARAGRAPH : 80000-MAP-TO-TSQ.                                   
02452 *                                                                 
02453 * LINKAGE TO : 80100-CHECK-HDR               80200-CHECK-DET      
02454 *              81200-UPDT-HDRS                                    
02455 *                                                                 
02456 * DESCRIPTION : This routine compares the data received from the  
02457 *               user against the TSQ information and decides which
02458 *               are the fields have been edited for further       
02459 *               processing on those fields                        
02460 *-----------------------------------------------------------------
02461                                                                   
02462  80000-MAP-TO-TSQ.                                                
02463                                                                   
02464      MOVE WS-ITEM-CTR                    TO I                     
02465                                             WS-ITEM-CTR1          
02466      MOVE SPACES                         TO WS-EDITED-SW.         
02467      IF CA-READ-DB2                                               
02468         PERFORM 80100-CHECK-HDR            THRU                   
02469                 80100-CHECK-HDR-EXIT                              
02470        IF ATLEAST-ONE-EDITED                                      
02471           MOVE WS-KEY-AREA               TO CA-KEY-AREA           
02472        END-IF                                                     
02473      ELSE                                                         
02474         PERFORM 80200-CHECK-DET            THRU                   
02475                 80200-CHECK-DET-EXIT                              
02476                 VARYING WS-CTR                                    
02477                 FROM 1 BY 1                                       
02478                 UNTIL WS-CTR > WS-LINE-CTR                        
02479      END-IF                                                       
02480      PERFORM   81200-UPDT-HDRS             THRU                   
02481                81200-UPDT-HDRS-EXIT.                              
02482      MOVE WS-ITEM-CTR1                   TO WS-ITEM-CTR.          
02483                                                                   
02484  80000-MAP-TO-TSQ-EXIT.                                           
02485      EXIT.                                                        
02486                                                                   
02487 *-----------------------------------------------------------------
02488 *                                                                 
02489 *                                                                 
02490 * PARAGRAPH : 80100-CHECK-HDR.                                    
02491 *                                                                 
02492 * LINKAGE TO : NONE                                               
02493 *                                                                 
02494 * DESCRIPTION : This routine compares the Header fields received  
02495 *               against the TSQ information                       
02496 *-----------------------------------------------------------------
02497                                                                   
02498  80100-CHECK-HDR.                                                 
02499                                                                   
02500      IF MAP-NOTI-ID-SEL-LEN              > ZERO     OR            
02501         MAP-NOTI-ID-SEL-ATR              = WS-EOL                 
02502         MOVE MAP-NOTI-ID-SEL-I           TO WS-HELP-IND           
02503                                                                   
02504         IF WS-HELP-IND                   = '?'     AND            
02505            CA-HELP                                                
02506            MOVE 'NOTI_ID_CDE'            TO CA-HELP-FLD           
02507            MOVE 'F'                      TO CA-HELP-TYP           
02508            MOVE WS-YES                   TO CA-KEY-HELP           
02509         ELSE                                                      
02510         IF WS-HELP-IND                   = '*'      AND           
02511            CA-HELP                                                
02512            MOVE 'NOTI_ID_CDE'            TO CA-HELP-FLD           
02513            MOVE 'Y'                      TO CA-LOOK-SW            
02514            MOVE WS-YES                   TO CA-KEY-HELP           
02515         ELSE                                                      
02516            MOVE MAP-NOTI-ID-SEL-I        TO WS-NOTI-ID-SEL        
02517                                             WS-TS-NOTI-ID-SEL(I)  
02518            MOVE WS-YES     TO WS-TS-NOTI-ID-SEL-EDSW(I)           
02519                               WS-EDITED-SW.                       
02520                                                                   
02521      IF MAP-REQ-UID-LEN                  > ZERO      OR           
02522         MAP-REQ-UID-ATR                  = WS-EOL                 
02523         MOVE MAP-REQ-UID-I               TO WS-HELP-IND           
02524                                                                   
02525         IF WS-HELP-IND                   = '?'      AND           
02526            CA-HELP                                                
02527            MOVE 'REQ_UID'                TO CA-HELP-FLD           
02528            MOVE 'F'                      TO CA-HELP-TYP           
02529            MOVE WS-YES                   TO CA-KEY-HELP           
02530         ELSE                                                      
02531         IF WS-HELP-IND                   = '*'     AND            
02532            CA-HELP                                                
02533            MOVE 'REQ_UID'                TO CA-HELP-FLD           
02534            MOVE 'F'                      TO CA-HELP-TYP           
02535            MOVE WS-YES                   TO CA-KEY-HELP           
02536         ELSE                                                      
02537            MOVE MAP-REQ-UID-I            TO WS-REQ-UID            
02538                                             WS-TS-REQ-UID-SEL(I)  
02539            MOVE WS-YES                   TO                       
02540                 WS-TS-REQ-UID-SEL-EDSW(I)                         
02541                 WS-EDITED-SW.                                     
02542                                                                   
02543      IF MAP-REQ-STRT-DTE-LEN             > ZERO      OR           
02544         MAP-REQ-STRT-DTE-ATR             = WS-EOL                 
02545         MOVE MAP-REQ-STRT-DTE-I          TO WS-HELP-IND           
02546                                                                   
02547         IF WS-HELP-IND                   = '?'      AND           
02548            CA-HELP                                                
02549            MOVE 'NOTI_REQ_STRT_DTE'      TO CA-HELP-FLD           
02550            MOVE 'F'                      TO CA-HELP-TYP           
02551            MOVE WS-YES                   TO CA-KEY-HELP           
02552         ELSE                                                      
02553         IF WS-HELP-IND                   = '*'      AND           
02554            CA-HELP                                                
02555            MOVE 'NOTI_REQ_STRT_DTE'      TO CA-HELP-FLD           
02556            MOVE 'F'                      TO CA-HELP-TYP           
02557            MOVE WS-YES                   TO CA-KEY-HELP           
02558         ELSE                                                      
02559            MOVE MAP-REQ-STRT-DTE-I       TO WS-REQ-STRT-DTE       
02560                                             WS-TS-REQ-STRT-DTE(I) 
02561            MOVE WS-YES                   TO                       
02562                 WS-TS-REQ-STRT-DTE-EDSW(I)                        
02563                 WS-EDITED-SW.                                     
02564                                                                   
02565      IF MAP-REQ-END-DTE-LEN              > ZERO     OR            
02566         MAP-REQ-END-DTE-ATR              = WS-EOL                 
02567         MOVE MAP-REQ-END-DTE-I           TO WS-HELP-IND           
02568                                                                   
02569         IF WS-HELP-IND                   = '?'     AND            
02570            CA-HELP                                                
02571            MOVE 'NOTI_REQ_END_DTE'       TO CA-HELP-FLD           
02572            MOVE 'F'                      TO CA-HELP-TYP           
02573            MOVE WS-YES                   TO CA-KEY-HELP           
02574         ELSE                                                      
02575         IF WS-HELP-IND                   = '*'     AND            
02576            CA-HELP                                                
02577            MOVE 'NOTI_REQ_END_DTE'       TO CA-HELP-FLD           
02578            MOVE 'F'                      TO CA-HELP-TYP           
02579            MOVE WS-YES                   TO CA-KEY-HELP           
02580         ELSE                                                      
02581            MOVE MAP-REQ-END-DTE-I        TO WS-REQ-END-DTE        
02582                                             WS-TS-REQ-END-DTE(I)  
02583            MOVE WS-YES                   TO                       
02584                 WS-TS-REQ-END-DTE-EDSW(I)                         
02585                 WS-EDITED-SW.                                     
02586                                                                   
02587  80100-CHECK-HDR-EXIT.                                            
02588        EXIT.                                                      
02589                                                                   
02590 *-----------------------------------------------------------------
02591 *                                                                 
02592 *                                                                 
02593 * PARAGRAPH : 80200-CHECK-DET.                                    
02594 *                                                                 
02595 * LINKAGE TO : NONE                                               
02596 *                                                                 
02597 * DESCRIPTION : This routine compares the Detail fields received  
02598 *               against the TSQ information                       
02599 *                                                                 
02600 *-----------------------------------------------------------------
02601                                                                   
02602  80200-CHECK-DET.                                                 
02603                                                                   
02604      MOVE WS-CTR                         TO J                     
02605                                                                   
02606      IF MAP-SEL-FLD-LEN(J)               > 0       OR             
02607         MAP-SEL-FLD-ATR(J)               = WS-EOL                 
02608         MOVE MAP-SEL-FLD-I(J)            TO WS-HELP-IND           
02609                                                                   
02610         IF ( WS-HELP-IND                 = '?'     AND            
02611            CA-HELP)                      OR                       
02612           ( WS-HELP-IND                  = '*'     AND            
02613            CA-HELP )                                              
02614            MOVE 'SEL_FLD'                TO CA-HELP-FLD           
02615            MOVE 'F'                      TO CA-HELP-TYP           
02616            MOVE WS-YES                   TO CA-KEY-HELP           
02617            MOVE WS-CTR                   TO CA-HELP-IDX           
02618         ELSE                                                      
02619            MOVE MAP-SEL-FLD-I(J)         TO WS-SEL-FLD(I, J)      
02620            MOVE WS-YES                   TO                       
02621                 WS-SEL-FLD-EDSW(I, J)                             
02622                 WS-EDITED-SW.                                     
02623                                                                   
02624  80200-CHECK-DET-EXIT.                                            
02625      EXIT.                                                        
02626                                                                   
02627                                                                   
02628 *-----------------------------------------------------------------
02629 *                                                                 
02630 *                                                                 
02631 * PARAGRAPH : 80400-MOVE-HELP-FLDS.                               
02632 *                                                                 
02633 * LINKAGE TO : NONE                                               
02634 *                                                                 
02635 * DESCRIPTION :                                                   
02636 *                                                                 
02637 *-----------------------------------------------------------------
02638                                                                   
02639  80400-MOVE-HELP-FLDS.                                            
02640      MOVE CA-HELP-IDX                    TO WS-CTR                
02641                                                                   
02642      EVALUATE TRUE                                                
02643         WHEN CA-HELP-FLD = 'NOTI_ID_CDE'                          
02644            MOVE WS-INIT-CSR              TO MAP-NOTI-ID-SEL-LEN   
02645            IF CA-HELP-VAL                NOT = SPACES AND         
02646               CA-HELP-VAL                NOT = LOW-VALUES AND     
02647               CA-LOOKUP                                           
02648                 MOVE CA-HELP-VAL         TO WS-HELP-VALUE         
02649                 MOVE WS-HELP-CDE         TO WS-NOTI-ID-SEL        
02650                 MOVE WS-HELP-CDE         TO                       
02651                      WS-TS-NOTI-ID-SEL(WS-ITEM-CTR)               
02652            END-IF                                                 
02653         WHEN CA-HELP-FLD = 'REQ_UID'                              
02654            MOVE WS-INIT-CSR              TO MAP-REQ-UID-LEN       
02655         WHEN CA-HELP-FLD = 'NOTI_REQ_STRT_DTE'                    
02656            MOVE WS-INIT-CSR              TO MAP-REQ-STRT-DTE-LEN  
02657         WHEN CA-HELP-FLD = 'NOTI_REQ_END_DTE'                     
02658            MOVE WS-INIT-CSR              TO MAP-REQ-END-DTE-LEN   
02659         WHEN CA-HELP-FLD = 'SEL_FLD'                              
02660            MOVE WS-INIT-CSR    TO MAP-SEL-FLD-LEN(CA-HELP-IDX)    
02661         WHEN CA-HELP-FLD = SPACES                                 
02662             IF CA-READ-DB2                                        
02663                MOVE WS-INIT-CSR           TO MAP-NOTI-ID-SEL-LEN  
02664             ELSE                                                  
02665                MOVE WS-INIT-CSR          TO MAP-SEL-FLD-LEN(1)    
02666             END-IF                                                
02667      END-EVALUATE                                                 
02668                                                                   
02669      IF CA-HELP-FLD = 'NOTI_ID_CDE'                               
02670         MOVE BMS-UNPROT                  TO MAP-NOTI-ID-SEL-ATR   
02671      END-IF.                                                      
02672      MOVE ZEROS                          TO CA-HELP-IDX.          
02673                                                                   
02674 * * Only for the Detail Line Fields.                              
02675                                                                   
02676  80400-MOVE-HELP-FLDS-EXIT.                                       
02677      EXIT.                                                        
02678                                                                   
02679                                                                   
02680 *-----------------------------------------------------------------
02681 *                                                                 
02682 *                                                                 
02683 * PARAGRAPH : 80600-MOVE-TO-TSQ.                                  
02684 *                                                                 
02685 * LINKAGE TO : NONE                                               
02686 *                                                                 
02687 * DESCRIPTION : This routine copies the latest data into the scrn 
02688 *               TSQ.                                              
02689 *-----------------------------------------------------------------
02690                                                                   
02691  80600-MOVE-TO-TSQ.                                               
02692                                                                   
02693      MOVE WS-ITEM-CTR                    TO I                     
02694      MOVE WS-CTR                         TO J                     
02695      IF WS-CTR = 1                                                
02696         IF WS-NOTI-ID-SEL                = SPACES                 
02697            MOVE WS-TS-NOTI-ID-SEL(I)     TO WS-NOTI-ID-SEL        
02698         END-IF                                                    
02699         IF WS-REQ-UID                    = SPACES                 
02700            MOVE WS-TS-REQ-UID-SEL(I)     TO WS-REQ-UID            
02701         END-IF                                                    
02702         IF WS-REQ-STRT-DTE               = SPACES                 
02703            MOVE WS-TS-REQ-STRT-DTE(I)    TO WS-REQ-STRT-DTE       
02704         END-IF                                                    
02705         IF WS-REQ-END-DTE                = SPACES                 
02706            MOVE WS-TS-REQ-END-DTE(I)     TO WS-REQ-END-DTE        
02707         END-IF                                                    
02708                                                                   
02709         MOVE WS-KEY-AREA                 TO CA-KEY-AREA           
02710         MOVE WS-NOTI-ID-SEL              TO                       
02711              WS-TS-NOTI-ID-SEL(I)                                 
02712         MOVE WS-REQ-UID                  TO                       
02713              WS-TS-REQ-UID-SEL(I)                                 
02714         MOVE WS-REQ-STRT-DTE             TO                       
02715              WS-TS-REQ-STRT-DTE(I)                                
02716         MOVE WS-REQ-END-DTE              TO                       
02717              WS-TS-REQ-END-DTE(I).                                
02718      IF NOT CA-READ-DB2                                           
02719         MOVE BMS-UNPROT                  TO MAP-SEL-FLD-ATR(J)    
02720      END-IF                                                       
02721      MOVE NTA-NOTI-ID-CDE                TO                       
02722           WS-NOTI-ID-CDE (I, J)                                   
02723      MOVE NTA-NOTI-REQ-UID               TO                       
02724           WS-NOTI-REQ-UID(I, J)                                   
02725      MOVE NTA-NOTI-REQ-DTE               TO                       
02726           WS-NOTI-REQ-DTE(I, J)                                   
02727      MOVE NTA-NOTI-PRT-ID                TO                       
02728           WS-NOTI-PRT-ID (I, J)                                   
02729      MOVE NTA-NOTI-ACTY-SEQ-NUM          TO                       
02730           WS-NUM-ACTY-SEQ-N                                       
02731      MOVE WS-NUM-ACTY-SEQ-N              TO                       
02732           WS-NOTI-ACTY-SEQ-N(I, J)                                
02733      MOVE NTA-GEN-STAT-IND               TO                       
02734           WS-GEN-STAT-IND(I, J)                                   
02735      MOVE NTA-RSND-ACTY-SEQ-NUM          TO                       
02736           WS-NUM-RSND-SEQ-N                                       
02737      MOVE WS-NUM-RSND-SEQ-N              TO                       
02738           WS-RSND-ACTY-SEQ-N(I, J).                               
02739                                                                   
02740  80600-MOVE-TO-TSQ-EXIT.                                          
02741      EXIT.                                                        
02742                                                                   
02743                                                                   
02744 *-----------------------------------------------------------------
02745 *                                                                 
02746 *                                                                 
02747 * PARAGRAPH : 81000-READ-TSQ-SCR.                                 
02748 *                                                                 
02749 * LINKAGE TO : 89000-CHECK-TSQ-ERR                                
02750 *                                                                 
02751 * DESCRIPTION :                                                   
02752 *                                                                 
02753 *-----------------------------------------------------------------
02754                                                                   
02755  81000-READ-TSQ-SCR.                                              
02756                                                                   
02757      MOVE CA-TRMID                       TO WS-SCR-TRMID          
02758      MOVE CA-FAST-PATH-ID                TO WS-SCR-FASTID         
02759                                                                   
02760      EXEC CICS                                                    
02761           READQ   TS                                              
02762           QUEUE   (WS-SCR-TSQ)                                    
02763           INTO    (WS-SCR-TSAREA(WS-ITEM-CTR))                    
02764           ITEM    (WS-ITEM-CTR)                                   
02765           RESP    (TSQ-RESP)                                      
02766      END-EXEC.                                                    
02767                                                                   
02768      IF (CA-TSQ-SW =  WS-NO   AND                                 
02769          NOT TSQ-ID-ERR)       OR                                 
02770          CA-TSQ-SW =  WS-YES                                      
02771          PERFORM  89000-CHECK-TSQ-ERR      THRU                   
02772                   89000-CHECK-TSQ-ERR-EXIT.                       
02773                                                                   
02774  81000-READ-TSQ-SCR-EXIT.                                         
02775      EXIT.                                                        
02776                                                                   
02777 *-----------------------------------------------------------------
02778 *                                                                 
02779 *                                                                 
02780 * PARAGRAPH : 81200-UPDT-HDRS.                                    
02781 *                                                                 
02782 * LINKAGE TO : NONE                                               
02783 *                                                                 
02784 * DESCRIPTION : This routine updates the Header fields to the TSQ 
02785 *-----------------------------------------------------------------
02786                                                                   
02787  81200-UPDT-HDRS.                                                 
02788                                                                   
02789      MOVE WS-NOTI-ID-SEL      TO                                  
02790           WS-TS-NOTI-ID-SEL(WS-ITEM-CTR)                          
02791      MOVE WS-REQ-UID          TO                                  
02792           WS-TS-REQ-UID-SEL(WS-ITEM-CTR)                          
02793      MOVE WS-REQ-STRT-DTE     TO                                  
02794           WS-TS-REQ-STRT-DTE(WS-ITEM-CTR)                         
02795      MOVE WS-REQ-END-DTE      TO                                  
02796           WS-TS-REQ-END-DTE(WS-ITEM-CTR).                         
02797      IF CA-PF11                                                   
02798         MOVE CA-PAGE-NO       TO WS-RETN-CUR-PAGE(1)              
02799         MOVE CA-TOT-PAGE-NO   TO WS-RETN-TOT-PAGE(1)              
02800         MOVE CA-DB2-READ      TO WS-RETN-DB2-READ(1).             
02801                                                                   
02802  81200-UPDT-HDRS-EXIT.                                            
02803      EXIT.                                                        
02804                                                                   
02805 *-----------------------------------------------------------------
02806 *                                                                 
02807 *                                                                 
02808 * PARAGRAPH : 82000-UPDT-TSQ-SCR.                                 
02809 *                                                                 
02810 * LINKAGE TO : 89000-CHECK-TSQ-ERR                                
02811 *                                                                 
02812 * DESCRIPTION : This routine writes the WORKING STORAGE TSQ data  
02813 *               into the QUEUE.                                   
02814 *-----------------------------------------------------------------
02815                                                                   
02816  82000-UPDT-TSQ-SCR.                                              
02817                                                                   
02818      MOVE CA-TRMID                       TO WS-SCR-TRMID          
02819      MOVE CA-FAST-PATH-ID                TO WS-SCR-FASTID         
02820                                                                   
02821      IF WS-ITEM-CTR > CA-SCR-TSQ-CTR                              
02822         EXEC CICS                                                 
02823              WRITEQ TS                                            
02824              QUEUE   (WS-SCR-TSQ)                                 
02825              FROM    (WS-SCR-TSAREA(WS-ITEM-CTR))                 
02826              RESP    (TSQ-RESP)                                   
02827         END-EXEC                                                  
02828      ELSE                                                         
02829         EXEC CICS                                                 
02830              WRITEQ TS                                            
02831              QUEUE   (WS-SCR-TSQ)                                 
02832              FROM    (WS-SCR-TSAREA(WS-ITEM-CTR))                 
02833              ITEM    (WS-ITEM-CTR)                                
02834              REWRITE                                              
02835              RESP    (TSQ-RESP)                                   
02836         END-EXEC.                                                 
02837                                                                   
02838                                                                   
02839      PERFORM      89000-CHECK-TSQ-ERR      THRU                   
02840                   89000-CHECK-TSQ-ERR-EXIT                        
02841                                                                   
02842      IF WS-ITEM-CTR > CA-SCR-TSQ-CTR                              
02843         MOVE WS-ITEM-CTR                 TO CA-SCR-TSQ-CTR        
02844      END-IF                                                       
02845      MOVE WS-YES                         TO CA-TSQ-SW.            
02846                                                                   
02847  82000-UPDT-TSQ-SCR-EXIT.                                         
02848      EXIT.                                                        
02849                                                                   
02850 *-----------------------------------------------------------------
02851 *                                                                 
02852 *                                                                 
02853 * PARAGRAPH : 83000-DELETE-TSQ-SCR.                               
02854 *                                                                 
02855 * LINKAGE TO : 89000-CHECK-TSQ-ERR                                
02856 *                                                                 
02857 * DESCRIPTION : This routine DELETE's the SCREEN TSQ required     
02858 *               during REFRESH processing (CLEAR key)             
02859 *-----------------------------------------------------------------
02860                                                                   
02861  83000-DELETE-TSQ-SCR.                                            
02862                                                                   
02863      MOVE CA-TRMID                       TO WS-SCR-TRMID          
02864      MOVE CA-FAST-PATH-ID                TO WS-SCR-FASTID         
02865                                                                   
02866      EXEC CICS                                                    
02867           DELETEQ TS                                              
02868           QUEUE   (WS-SCR-TSQ)                                    
02869           RESP    (TSQ-RESP)                                      
02870      END-EXEC.                                                    
02871                                                                   
02872      IF NOT TSQ-ID-ERR                                            
02873         PERFORM  89000-CHECK-TSQ-ERR       THRU                   
02874                  89000-CHECK-TSQ-ERR-EXIT.                        
02875      MOVE SPACES                         TO CA-TSQ-SW.            
02876                                                                   
02877  83000-DELETE-TSQ-SCR-EXIT.                                       
02878      EXIT.                                                        
02879                                                                   
02880 *-----------------------------------------------------------------
02881 *                                                                 
02882 *                                                                 
02883 * PARAGRAPH : 87000-UPDT-TSQ-DB2.                                 
02884 *                                                                 
02885 * LINKAGE TO : 89000-CHECK-TSQ-ERR                                
02886 *                                                                 
02887 * DESCRIPTION : This routine updates the DB2 TSQ for Update       
02888 *-----------------------------------------------------------------
02889                                                                   
02890 *-----------------------------------------------------------------
02891 *                                                                 
02892 *                                                                 
02893 * PARAGRAPH : 88000-DELETE-TSQ-DB2.                               
02894 *                                                                 
02895 * LINKAGE TO : 89000-CHECK-TSQ-ERR                                
02896 *                                                                 
02897 * DESCRIPTION : This routine DELETEs the DB2 TSQ                  
02898 *-----------------------------------------------------------------
02899                                                                   
02900  88000-DELETE-TSQ-DB2.                                            
02901                                                                   
02902      MOVE CA-TRMID                       TO WS-DB2-TRMID          
02903      MOVE CA-DB2-QID                     TO WS-DB2-FASTID         
02904                                                                   
02905      EXEC CICS                                                    
02906           DELETEQ TS                                              
02907           QUEUE   (WS-DB2-TSQ)                                    
02908           RESP    (TSQ-RESP)                                      
02909      END-EXEC.                                                    
02910                                                                   
02911      IF NOT TSQ-ID-ERR                                            
02912         PERFORM  89000-CHECK-TSQ-ERR       THRU                   
02913                  89000-CHECK-TSQ-ERR-EXIT.                        
02914                                                                   
02915  88000-DELETE-TSQ-DB2-EXIT.                                       
02916      EXIT.                                                        
02917                                                                   
02918 *-----------------------------------------------------------------
02919 *                                                                 
02920 *                                                                 
02921 * PARAGRAPH : 89000-CHECK-TSQ-ERR.                                
02922 *                                                                 
02923 * LINKAGE TO : 69000-READ-MSG                79000-DISP-MAP       
02924 *              97000-RETN-PARA                                    
02925 *                                                                 
02926 * DESCRIPTION : This routine checks for TSQ errors and displays   
02927 *               errors accordingly.                               
02928 *-----------------------------------------------------------------
02929                                                                   
02930  89000-CHECK-TSQ-ERR.                                             
02931                                                                   
02932      IF TSQ-ITEM-ERR                                              
02933         MOVE TSQ-RESP                    TO WS-ERR-ERR            
02934      ELSE                                                         
02935      IF TSQ-LEN-ERR                                               
02936         MOVE TSQ-RESP                    TO WS-ERR-ERR            
02937      ELSE                                                         
02938      IF TSQ-ID-ERR                                                
02939         MOVE TSQ-RESP                    TO WS-ERR-ERR            
02940      ELSE                                                         
02941      IF TSQ-NOSPACE                                               
02942         MOVE TSQ-RESP                    TO WS-ERR-ERR            
02943      ELSE                                                         
02944      IF NOT TSQ-SUCCESS                                           
02945         MOVE WS-TSQ-FAIL                 TO WS-ERR-CDE.           
02946                                                                   
02947      IF NOT TSQ-SUCCESS                                           
02948         MOVE WS-ERR-TYP-TSQ              TO WS-ERR-TYP            
02949         PERFORM    69000-READ-MSG          THRU                   
02950                    69000-READ-MSG-EXIT                            
02951         PERFORM    79000-DISP-MAP          THRU                   
02952                    79000-DISP-MAP-EXIT                            
02953         PERFORM    97000-RETN-PARA         THRU                   
02954                    97000-RETN-PARA-EXIT.                          
02955                                                                   
02956  89000-CHECK-TSQ-ERR-EXIT.                                        
02957      EXIT.                                                        
02958                                                                   
02959                                                                   
02960 *-----------------------------------------------------------------
02961 *                                                                 
02962 *                                                                 
02963 * PARAGRAPH : 95000-MAP-FAIL.                                     
02964 *                                                                 
02965 * LINKAGE TO : 69000-READ-MSG                79000-DISP-MAP       
02966 *              97000-RETN-PARA                                    
02967 *                                                                 
02968 * DESCRIPTION : This routine issues a MAP FAIL command should a   
02969 *               map fail condition arise.                         
02970 *-----------------------------------------------------------------
02971                                                                   
02972  95000-MAP-FAIL.                                                  
02973                                                                   
02974      MOVE WS-MAP-FAIL                    TO WS-ERR-CDE            
02975      MOVE WS-YES                         TO WS-FIRST-FLD          
02976      PERFORM       69000-READ-MSG          THRU                   
02977                    69000-READ-MSG-EXIT                            
02978      PERFORM       79000-DISP-MAP          THRU                   
02979                    79000-DISP-MAP-EXIT                            
02980      PERFORM       97000-RETN-PARA         THRU                   
02981                    97000-RETN-PARA-EXIT.                          
02982                                                                   
02983  95000-MAP-FAIL-EXIT.                                             
02984      EXIT.                                                        
02985                                                                   
02986                                                                   
02987 *-----------------------------------------------------------------
02988 *                                                                 
02989 *                                                                 
02990 * PARAGRAPH : 96000-ERR-PARA.                                     
02991 *                                                                 
02992 * LINKAGE TO : 69000-READ-MSG                79000-DISP-MAP       
02993 *              97000-RETN-PARA                                    
02994 *                                                                 
02995 * DESCRIPTION : This routine displays an UNDEFINED CICS ERROR     
02996 *-----------------------------------------------------------------
02997                                                                   
02998  96000-ERR-PARA.                                                  
02999                                                                   
03000      MOVE WS-UNDF-CICS-ERR               TO WS-ERR-CDE            
03001      MOVE WS-YES                         TO WS-FIRST-FLD          
03002      PERFORM       69000-READ-MSG          THRU                   
03003                    69000-READ-MSG-EXIT                            
03004      PERFORM       79000-DISP-MAP          THRU                   
03005                    79000-DISP-MAP-EXIT                            
03006      PERFORM       97000-RETN-PARA         THRU                   
03007                    97000-RETN-PARA-EXIT.                          
03008                                                                   
03009  96000-ERR-PARA-EXIT.                                             
03010      EXIT.                                                        
03011                                                                   
03012                                                                   
03013                                                                   
03014 *-----------------------------------------------------------------
03015 *                                                                 
03016 *                                                                 
03017 * PARAGRAPH : 97000-RETN-PARA.                                    
03018 *                                                                 
03019 * LINKAGE TO : NONE                                               
03020 *                                                                 
03021 * DESCRIPTION : This routine returns control to the same program  
03022 *               (Pseudo-conversational)                           
03023 *-----------------------------------------------------------------
03024                                                                   
03025  97000-RETN-PARA.                                                 
03026                                                                   
03027      EXEC CICS                                                    
03028           RETURN                                                  
03029           TRANSID  (CA-TRANID)                                    
03030           COMMAREA (WS-COMM-AREA)                                 
03031      END-EXEC.                                                    
03032                                                                   
03033  97000-RETN-PARA-EXIT.                                            
03034      EXIT.                                                        
03035                                                                   
03036 *-----------------------------------------------------------------
03037 *                                                                 
03038 *                                                                 
03039 * PARAGRAPH : 98000-XCTL-PARA.                                    
03040 *                                                                 
03041 * LINKAGE TO : NONE                                               
03042 *                                                                 
03043 * DESCRIPTION : This routine transfers control to the ACMP        
03044 *                                                                 
03045 *-----------------------------------------------------------------
03046                                                                   
03047  98000-XCTL-PARA.                                                 
03048                                                                   
03049      EXEC CICS                                                    
03050           XCTL                                                    
03051           PROGRAM ('OGXOACMP')                                    
03052           COMMAREA (WS-COMM-AREA)                                 
03053      END-EXEC.                                                    
03054                                                                   
03055  98000-XCTL-PARA-EXIT.                                            
03056      EXIT.                                                        
