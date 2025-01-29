       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.                           UTCONTR.                           
       AUTHOR.                               TATA CONSULTANCY SERVICES.         
       DATE-WRITTEN.                         08/25/2000.                        
       DATE-COMPILED.                                                           
      *---------------------------------------------------------------          
      *                       OVERVIEW                                          
      *---------------------------------------------------------------          
      *                                                                         
      * THIS IS A MAIN CONTROL PROGRAM FOR THE ADB SYSTEM.                      
      * ALL TRANSACTIONS OF THE SYSTEM WILL BE LINKED TO THIS PROGRAM           
      *         SYSTEM          : AGENCIES DATABASE                             
      *         DATE CREATED    : 08/25/2000                                    
      *                                                                         
      * THIS PROGRAM WILL ALWAYS BE INVOKED FIRST FOR THE ADB SYSTEM            
      * IF IT IS A FIRST TIME CALL, APPROPRIATE PROGRAMS ARE CALLED             
      *    BASED ON THE TRANSACTION ID USED.                                    
      * IF IT IS A RETURN FROM A PSEUDO-CONVERSE PROGRAM, THE                   
      *    DIALOG ISSUING THE RETURN TO CICS WILL BE CALLED.                    
      *                                                                         
      *---------------------------------------------------------------          
      *                       GENERAL                                           
      *---------------------------------------------------------------          
      *                                                                         
      * PROGRAM TYPE     : COBOL-CICS                                           
      * LIST OF TRANSACTION IDS ASSIGNED TO THIS PROGRAM                        
      * TRANS ID    FUNCTION   DIALOG           DESCRIPTION                     
      *  UTAP       UTFAPT01   UTDAPT01   APPOINTMENT SUB MENU                  
      *  UTDA       UTFORO01   UTDORO01   OFFICE/DETACHED                       
      *  UTDE       UTFDE001   UTDGEN01   DEMOTION TRANSACTION                  
      *  UTIG       UTFIQG01   UTDGEN01   INQUIRY AGENT INFO                    
      *  UTIP       UTFIQP01   UTDGEN01   INQUIRY - PERSONAL INFO               
      *  UTMI       UTFMI001   UTDGEN01   MISC CHNG GENERAL PURPOSE             
      *  UTMS       UTFMS001   UTDGEN01   MARKETER SPLIT                        
      *  UTMU       UTFMENU1   UTDMENU1   AGENCIES DATABASE MAIN MENU           
      *  UTOA       UTFORP01   UTDORP01   AGENCY/DETACHED                       
      *  UTOR       UTFOR001   UTDOR001   ORGANIZATIONS MAPLESS DIALOG          
DAPOS *  UTPC       UTFPCD01   UTDGEN01   COMPUTER INFO GEN                     
      *  UTPH       UTFPH001   UTDPH001   REP PHONE INFORMATION                 
      *  UTPR       UTFPR001   UTDGEN01   PROMOTION TRANSACTION                 
      *  UTRA       UTFRA001   UTDRA001   AGENCIES DATABASE REAPPOINTM          
      *  UTRC       UTFRCL01   UTDGEN01   RECLASS GEN PURPOSE                   
      *  UTRD       UTFRED01   UTDGEN01   REVERSE DEMOTE GENL ENTRY             
      *  UTRE       UTFREM01   UTDREM01   AGENCIES DATABASE REVERSAL M          
      *  UTRP       UTFREP01   UTDGEN01   REVERSE PROMOTIONS                    
      *  UTTE       UTFTE001   UTDGEN01   TERMINATIONS GENERAL PURPOSE          
      *  UTTR       UTFTR001   UTDTR001   TRANSFERS SUBMENU                     
SWAPCH*  UTSI       UTSI       UTSI       SWAP ROUTINE                          
ADB801*  UTCG       UTFLGRD1   UTDLGRD1   UTCG - FIC ONLINE LICENSE GRID        
      *                                                                         
      * MAPSET USED      : NONE                                                 
      * MAPS USED        : NONE                                                 
      * INIATIATED BY    : ALL ABOVE TRANSACTIONS                               
      * INITIATES        : ALL ABOVE PROGRAMS                                   
      *                                                                         
      *                                                                         
       ENVIRONMENT DIVISION.                                                    
      *                                                                         
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER.                      IBM-370.                           
       OBJECT-COMPUTER.                      IBM-370.                           
      *                                                                         
       INPUT-OUTPUT SECTION.                                                    
      *                                                                         
TCSD  *IDMS-CONTROL SECTION.                                                    
TCSD  *PROTOCOL.    MODE IS  CICS-EXEC DEBUG.                                   
      *                                                                         
       DATA DIVISION.                                                           
      *                                                                         
       WORKING-STORAGE SECTION.                                                 
      *                                                                         
      ** MARKER FOR THE START OF THE WORKING STORAGE SECTION.                   
      *                                                                         
              EXEC SQL                                                          
                INCLUDE CPFVARS                                                 
              END-EXEC                                                          
TCSTST        EXEC SQL                                                          
TCSTST          INCLUDE CWSVARSC                                                
TCSTST        END-EXEC                                                          
              EXEC SQL                                                          
                INCLUDE CADSCONV                                                
              END-EXEC                                                          
       01 FILLER                                 PIC X(27)                      
                             VALUE 'WORKING STORAGE STARTS HERE'.               
      *                                                                         
       01 WS-COMMAREA.                                                          
      *                                                                         
      ** COMMAREA COPYBOOKS FOR GLOBAL RECORDS                                  
      *                                                                         
           02 WS-GLOBAL-RECS.                                                   
      ** COPYBOOK FOR ADSO-APPLICATION-GLOBAL-RECORD                            
SATHYA*       EXEC SQL                                                          
SATHYA*         INCLUDE CADSAGRC                                                
SATHYA*       END-EXEC                                                          
SATHYA        EXEC SQL                                                          
SATHYA          INCLUDE CGLOBVAR                                                
SATHYA        END-EXEC                                                          
TCSDB2        EXEC SQL                                                          
TCSDB2          INCLUDE CDBSTR                                                  
TCSDB2        END-EXEC                                                          
TCSSEC        EXEC SQL                                                          
TCSSEC          INCLUDE CLICATCD                                                
TCSSEC        END-EXEC                                                          
      ** COPYBOOK FOR UTDGEN01-GR-1                                             
              EXEC SQL                                                          
                INCLUDE CGEN01G1                                                
              END-EXEC                                                          
      ** COPYBOOK FOR UTDGEN01-GR-2                                             
              EXEC SQL                                                          
                INCLUDE CGEN01G2                                                
              END-EXEC                                                          
      ** COPYBOOK FOR WK-SOLICITOR-REC                                          
              EXEC SQL                                                          
                INCLUDE CSOLICSR                                                
              END-EXEC                                                          
      ** COPYBOOK FOR ABS-TIME                                                  
              EXEC SQL                                                          
                INCLUDE CABSTIME                                                
              END-EXEC                                                          
      ** COPYBOOK FOR SUBSCHEMA-CTRL                                            
              EXEC SQL                                                          
                INCLUDE CSCHEMC                                                 
              END-EXEC                                                          
      *                                                                         
      ** COPYBOOK FOR DB-STATISTICS                                             
              EXEC SQL                                                          
                INCLUDE CSTATIS                                                 
              END-EXEC                                                          
      *                                                                         
      ** FILLER FOR WORK RECORDS BEING PASSED                                   
      *                                                                         
      *    02 WS-WORK-RECS.                                                     
      *       03 FILLER                        PIC X(25000).                    
      *                                                                         
TCSTST* 02 WS-WORK-RECS.                                                        
TCSTST*    03 WS-COMM-RECORD-REDEF.                                             
TCSTST*      04 WS-COMM-RECORD-TABLE        OCCURS 25 TIMES.                    
TCSTST*       05 WS-COMM-RECORD-NAME         PIC X(8).                          
TCSTST*    EXEC SQL                                                             
TCSTST*        INCLUDE  CLICATCD                                                
TCSTST*    END-EXEC.                                                            
      *                                                                         
      *                                                                         
      *                                                                         
      ** WORKING-STORAGE VARIABLES                                              
      *                                                                         
       01  WS-PROG-VARS.                                                        
           02  WS-XCTL-PGM                     PIC  X(08).
           02  WS-NEXT-PGM-NAME                PIC  X(08).        
      *                                                                         
SWAPCH 01  PRSWAP-LEN                           PIC S9(4) COMP.                 
SWAPCH 01  WS-PRUSWAP-DATA.                                                     
SWAPCH       02  PRSWAP-DATA-1.                                                 
SWAPCH       EXEC SQL                                                           
SWAPCH           INCLUDE  CPRSWDA1                                              
SWAPCH       END-EXEC.                                                          
SWAPCH 01  WS-SWAP-QUE                        PIC X(8).                         
      ** WORKING-STORAGE DECLARATION FOR RECORD TABLE                           
      *                                                                         
TCSSEC*01 WS-PROGRAM-RECORDS.                                                   
TCSSEC*   02 WS-PROGRAM-RECORD-VAL.                                             
TCSSEC*      03 FILLER                         PIC X(8)                         
TCSSEC*         VALUE 'CLICATCD'.                                               
TCSSEC*   02 WS-RECORD-TABLE                                                    
TCSSEC*                REDEFINES WS-PROGRAM-RECORD-VAL.                         
TCSSEC*      03 WS-RECORD-NAME                 PIC X(8).                        
      *                                                                         
      *                                                                         
      ** ASSEMBLER PROGRAMS TO WHICH THE PROGRAM IS GETTING LINKED.             
      *                                                                         
TCSTST 01 WS-ASSEMB-PROGS.                                                      
TCSTST    02 WS-ASSEMB-PGM-NAME                PIC X(8).                        
TCSTST       88 C-ASSEMB-PGM                   VALUE 'WS107'                    
TCSTST                                               'UTDACD01'                 
TCSTST                                                     'WS115'.             
      *                                                                         
      ** WORKING-STORAGE CONSTANTS                                              
      *                                                                         
      *                                                                         
       01  WC-PROG-VARS.                                                        
           02  WC-CURR-PGM-ID                  PIC  X(08)                       
                              VALUE 'UTCONTR'.                                  
       01  WS-SECR-PGM                     PIC  X(08).                          
      *                                                                         
      ** ABEND ROUTINE COPYBOOK WORKING STORAGE                                 
      *                                                                         
           EXEC SQL                                                             
                INCLUDE UTIABWS0                                                
           END-EXEC.                                                            
           EXEC SQL                                                             
                INCLUDE SQLCA                                                   
           END-EXEC                                                             
      *                                                                         
      ** MARKER FOR THE END OF THE WORKING STORAGE SECTION                      
      *                                                                         
      *01  ACD-OPERATOR-OFFICE-TYPE        PIC  X.                              
      *                                                                         
       01 FILLER                                 PIC  X(22)                     
                             VALUE 'END OF WORKING STORAGE'.                    
      *                                                                         
       LINKAGE SECTION.                                                         
       01  DFHCOMMAREA.                                                         
           02  LK-FILLER.                                                       
              03  LK-FILL                          PIC X(1)                     
                  OCCURS 1 TO 25000 TIMES DEPENDING ON EIBCALEN.                
      *                                                                         
      ************************************************************              
000001************************************************************              
000002*                                                                         
000003** HANDLE ALL ABENDS WS-CURR-PGM-ID IS DEFINED IN THE                     
000004** COPYBOOK UTIABWS0 THAT HAS VARIABLES USED FOR ABEND                    
000005** HANDLING PROCEDURE DIVISION COPY BOOK UTIABAA0                         
000006*                                                                         
       PROCEDURE DIVISION USING DFHCOMMAREA .                                   
           MOVE WC-CURR-PGM-ID                         TO                       
                WS-CURR-PGM-ID                                                  
           .                                                                    
           EXEC SQL                                                             
                INCLUDE UTIABAA0                                                
           END-EXEC                                                             
       P-MAIN-PARA .                                                            
           MOVE 'P-MAIN-PARA'                          TO                       
                ABND-PRGH-NAME                                                  
           MOVE 'P'                                    TO                       
                ABND-ERROR-TYPE                                                 
           IF NOT EIBCALEN = 0                                                  
             MOVE DFHCOMMAREA ( 1:LENGTH OF WS-GLOBAL-RECS )                    
                                                       TO                       
                  WS-GLOBAL-RECS                                                
             IF AGR-TYPE-CTRL = 'N'                                             
               MOVE 1                                  TO                       
                    AGR-CUR-SUB                                                 
               MOVE 'T'                                TO                       
                    AGR-TYPE-CTRL                                               
               MOVE WS-GLOBAL-RECS                     TO                       
                    LK-FILLER ( 1:LENGTH OF WS-GLOBAL-RECS )                    
               MOVE LK-FILLER                          TO                       
                    DFHCOMMAREA                                                 
               EXEC CICS XCTL                                                   
                   PROGRAM ( AGR-PGM-NAME ( AGR-CUR-SUB ) )                     
                   COMMAREA ( DFHCOMMAREA )                                     
               END-EXEC                                                         
             END-IF                                                             
           END-IF                                                               
           IF EIBCALEN = 0                                                      
             INITIALIZE WS-COMMAREA                                             
             MOVE 0                                    TO                       
S                 AGR-POPQ-CNT                                                  
RETTST*      MOVE 2668                                 TO                       
RETTST       MOVE 2669                                 TO                       
                  AGR-LEN-GLOBAL-RECS                                           
             MOVE 'UTDACD01'                           TO                       
                  WS-NEXT-PGM-NAME                                              
             EXEC CICS LINK                                                     
                 PROGRAM ( WS-NEXT-PGM-NAME )                                   
                 COMMAREA ( APPLICATION-CONTROL-DATA )                          
             END-EXEC                                                           
             EVALUATE EIBTRNID                                                  
               WHEN 'UTSI'                                                      
                 MOVE 'S'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTSI'                           TO                       
                      WS-XCTL-PGM                                               
                 MOVE +75                              TO                       
                      PRSWAP-LEN                                                
                 EXEC CICS                                                      
                     RETRIEVE                                                   
                     INTO ( PRSWAP-DATA )                                       
                     LENGTH ( PRSWAP-LEN )                                      
                 END-EXEC                                                       
                 STRING EIBTRMID 'SWAP'                                         
                   DELIMITED BY SIZE                                            
                   INTO WS-SWAP-QUE                                             
                 PERFORM P-HANDLE-Q-ERROR              THRU                     
                         P-HANDLE-Q-ERROR-X                                     
                 EXEC CICS DELETEQ TS                                           
                     QUEUE ( WS-SWAP-QUE )                                      
                 END-EXEC                                                       
                 PERFORM P-DELETEQ-ERROR-FLAG          THRU                     
                         P-DELETEQ-ERROR-FLAG-X                                 
                 PERFORM P-REMOVE-HANDLE-Q             THRU                     
                         P-REMOVE-HANDLE-Q-X                                    
                 PERFORM P-HANDLE-Q-ERROR              THRU                     
                         P-HANDLE-Q-ERROR-X                                     
                 EXEC CICS WRITEQ TS                                            
                     QUEUE ( WS-SWAP-QUE )                                      
                     FROM ( PRSWAP-DATA )                                       
                 END-EXEC                                                       
                 PERFORM P-WRITEQ-ERROR-FLAG           THRU                     
                         P-WRITEQ-ERROR-FLAG-X                                  
                 PERFORM P-REMOVE-HANDLE-Q             THRU                     
                         P-REMOVE-HANDLE-Q-X                                    
               WHEN 'UTST'                                                      
                 MOVE 'P'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFIQ002'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDIQ002'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTAP'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFAPT01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDAPT01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTDA'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFORO01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDORO01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTDE'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFDE001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTIG'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFIQG01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTFD'                                                      
                 MOVE 'P'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFFL001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDFL001'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTIQ'                                                      
                 MOVE 'P'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFIQ001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTIP'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFIQP01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTMI'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFMI001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTMS'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFMS001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTMU'                                                      
                 IF ACD-OPERATOR-OFFICE-TYPE = 'D'  OR                          
                    'O'                                                         
                   MOVE 'P'                            TO                       
                        AGR-APPLICATION-TYPE                                    
                   MOVE 'UTFMENU2'                     TO                       
                        AGR-CURRENT-FUNCTION                                    
                   MOVE 'UTDMENU2'                     TO                       
                        WS-XCTL-PGM                                             
                 ELSE                                                           
                   MOVE 'A'                            TO                       
                        AGR-APPLICATION-TYPE                                    
                   MOVE 'UTFMENU1'                     TO                       
                        AGR-CURRENT-FUNCTION                                    
                   MOVE 'UTDMENU1'                     TO                       
                        WS-XCTL-PGM                                             
                 END-IF                                                         
               WHEN 'UTOA'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFORP01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDORP01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTOR'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFOR001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDOR001'                       TO                       
                      WS-XCTL-PGM                                               
DAPOS          WHEN 'UTPC'                                                      
DAPOS            MOVE 'P'                              TO                       
DAPOS                 AGR-APPLICATION-TYPE                                      
DAPOS            MOVE 'UTFPCD01'                       TO                       
DAPOS                 AGR-CURRENT-FUNCTION                                      
DAPOS            MOVE 'UTDGEN01'                       TO                       
DAPOS                 WS-XCTL-PGM                                               
               WHEN 'UTPH'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFPH001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDPH001'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTPR'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFPR001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTRA'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFRA001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDRA001'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTRC'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFRCL01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTRD'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFRED01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTRE'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFREM01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDREM01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTRI'                                                      
                 MOVE 'P'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFRIM01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDRIM01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTRP'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFREP01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTTE'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFTE001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTTR'                                                      
                 MOVE 'A'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFTR001'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDTR001'                       TO                       
                      WS-XCTL-PGM                                               
               WHEN 'UTHA'                                                      
                 MOVE 'P'                              TO                       
                      AGR-APPLICATION-TYPE                                      
                 MOVE 'UTFAHD01'                       TO                       
                      AGR-CURRENT-FUNCTION                                      
                 MOVE 'UTDGEN01'                       TO                       
                      WS-XCTL-PGM                                               
ADB801         WHEN 'UTCG'                                                      
ADB801           MOVE 'A'                              TO                       
ADB801                AGR-APPLICATION-TYPE                                      
ADB801           MOVE 'UTFLGRD1'                       TO                       
ADB801                AGR-CURRENT-FUNCTION                                      
ADB801           MOVE 'UTDLGRD1'                       TO                       
ADB801                WS-XCTL-PGM                                               
             END-EVALUATE                                                       
             MOVE AGR-CURRENT-FUNCTION                 TO                       
                  AGR-PRESENT-FUNCTION                                          
             MOVE 1                                    TO                       
                  AGR-PGM-SUB                                                   
             MOVE 1                                    TO                       
                  AGR-CUR-SUB                                                   
             MOVE WS-XCTL-PGM                          TO                       
                  AGR-PGM-NAME ( AGR-CUR-SUB )                                  
             MOVE 'UTTS'                               TO                       
                  AGR-DBNAME                                                    
             PERFORM P-XCTL-PARA                       THRU                     
                     P-XCTL-PARA-X                                              
           ELSE                                                                 
             MOVE LK-FILLER                            TO                       
                  WS-COMMAREA                                                   
             IF AGR-TYPE-CTRL = 'R'                                             
SATHYA        MOVE '#SELECT ' TO WS-MSG-OUT                                    
SATHYA        EXEC CICS SEND TEXT                                               
SATHYA            FROM (WS-MSG-OUT)                                             
SATHYA            LENGTH(9)                                                     
SATHYA            CURSOR(9)                                                     
SATHYA            ERASE                                                         
SATHYA         END-EXEC                                                         
               EXEC CICS RETURN                                                 
               END-EXEC                                                         
             END-IF                                                             
             MOVE 1                                    TO                       
                  AGR-CUR-SUB                                                   
             MOVE AGR-PGM-NAME ( AGR-CUR-SUB )         TO                       
                  WS-XCTL-PGM                                                   
             EXEC CICS XCTL                                                     
                 PROGRAM ( WS-XCTL-PGM )                                        
                 COMMAREA ( DFHCOMMAREA )                                       
             END-EXEC                                                           
           END-IF                                                               
           .                                                                    
       P-MAIN-PARA-X .                                                          
      *-------------------------------------------------------------*           
      ** TRANSFERS CONTROL TO THE NEXT LEVEL                       **           
      *-------------------------------------------------------------*           
           EXIT                                                                 
           .                                                                    
       P-LINK-PARA .                                                            
           MOVE 'P-LINK-PARA'                          TO                       
                ABND-PRGH-NAME                                                  
           MOVE 'C'                                    TO                       
                ABND-ERROR-TYPE                                                 
           MOVE WS-NEXT-PGM-NAME                       TO                       
                WS-ASSEMB-PGM-NAME                                              
           IF C-ASSEMB-PGM                                                      
             IF WS-ASSEMB-PGM-NAME = 'UTDACD01'                                 
               EXEC CICS LINK                                                   
                   PROGRAM ( WS-ASSEMB-PGM-NAME )                               
                   COMMAREA ( APPLICATION-CONTROL-DATA )                        
               END-EXEC                                                         
             END-IF                                                             
           ELSE                                                                 
             ADD 1                                     TO                       
                 AGR-CUR-SUB                                                    
             ADD 1                                     TO                       
                 AGR-PGM-SUB                                                    
             MOVE WS-NEXT-PGM-NAME                     TO                       
                  AGR-PGM-NAME ( AGR-CUR-SUB )                                  
             MOVE ZERO                                 TO                       
                  AGR-PGM-ENTRY-CNT ( AGR-CUR-SUB )                             
             STRING EIBTRMID 'GEN1'                                             
               DELIMITED BY SIZE                                                
               INTO WS-GEN-QUE                                                  
             EXEC SQL                                                           
                  INCLUDE PWRITEQ                                               
             END-EXEC                                                           
             EXEC CICS LINK                                                     
                 PROGRAM ( AGR-PGM-NAME ( AGR-CUR-SUB ) )                       
                 COMMAREA ( WS-COMMAREA )                                       
             END-EXEC                                                           
           END-IF                                                               
           .                                                                    
       P-LINK-PARA-X .                                                          
           EXIT                                                                 
           .                                                                    
       P-XCTL-PARA .                                                            
           MOVE 'P-XCTL-PARA'                          TO                       
                ABND-PRGH-NAME                                                  
           MOVE 'C'                                    TO                       
                ABND-ERROR-TYPE                                                 
           EXEC CICS XCTL                                                       
               PROGRAM ( WS-XCTL-PGM )                                          
               COMMAREA ( WS-COMMAREA )                                         
           END-EXEC                                                             
           .                                                                    
       P-XCTL-PARA-X .                                                          
           EXIT                                                                 
           .                                                                    
       P-HANDLE-Q-ERROR .                                                       
           EXEC CICS IGNORE                                                     
               CONDITION                                                        
               QIDERR                                                           
               ITEMERR                                                          
               IOERR                                                            
               INVREQ                                                           
               LENGERR                                                          
           END-EXEC                                                             
           .                                                                    
       P-HANDLE-Q-ERROR-X .                                                     
           EXIT                                                                 
           .                                                                    
       P-REMOVE-HANDLE-Q .                                                      
           EXEC CICS HANDLE CONDITION                                           
               QIDERR ( Z999-ABEND-RTN )                                        
               ITEMERR ( Z999-ABEND-RTN )                                       
               IOERR ( Z999-ABEND-RTN )                                         
               INVREQ ( Z999-ABEND-RTN )                                        
               LENGERR ( Z999-ABEND-RTN )                                       
           END-EXEC                                                             
           .                                                                    
       P-REMOVE-HANDLE-Q-X .                                                    
           EXIT                                                                 
           .                                                                    
       P-DELETEQ-ERROR-FLAG .                                                   
           MOVE '0000'                                 TO                       
                ERROR-STATUS                                                    
           EVALUATE EIBRESP                                                     
             WHEN 44                                                            
               MOVE '4303'                             TO                       
                    ERROR-STATUS                                                
             WHEN 26                                                            
               MOVE '4305'                             TO                       
                    ERROR-STATUS                                                
             WHEN 17                                                            
               MOVE '4307'                             TO                       
                    ERROR-STATUS                                                
             WHEN 16                                                            
               MOVE '4331'                             TO                       
                    ERROR-STATUS                                                
           END-EVALUATE                                                         
           .                                                                    
       P-DELETEQ-ERROR-FLAG-X .                                                 
           EXIT                                                                 
           .                                                                    
       P-WRITEQ-ERROR-FLAG .                                                    
           MOVE '0000'                                 TO                       
                ERROR-STATUS                                                    
           EVALUATE EIBRESP                                                     
             WHEN 17                                                            
               MOVE '4307'                             TO                       
                    ERROR-STATUS                                                
             WHEN 26                                                            
               MOVE '4322'                             TO                       
                    ERROR-STATUS                                                
             WHEN 16                                                            
               MOVE '4331'                             TO                       
                    ERROR-STATUS                                                
             WHEN 22                                                            
               MOVE '4332'                             TO                       
                    ERROR-STATUS                                                
           END-EVALUATE                                                         
           .                                                                    
       P-WRITEQ-ERROR-FLAG-X .                                                  
      ******************************************************************        
      ** TO HANDLE ALL ABENDS - COPYBOOK UTIABPD0                               
      ** HERE UTIABPD1 IS INTRODUCED BECAUSE WE NEED NOT DO A FINISH            
      ******************************************************************        
           EXIT                                                                 
           .                                                                    
           EXEC SQL                                                             
                INCLUDE UTIABPD1                                                
           END-EXEC                                                             
