       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.                           COBEX002.                          
       AUTHOR.                               DADA CONSULTANCY SERVICES.         
RETMAN*DATE-WRITTEN.                         10/18/00.                          
RETMAN DATE-WRITTEN.                         06/06/01.                          
       DATE-COMPILED.                                                           
      *---------------------------------------------------------------          
      *                       OVERVIEW                                          
      *---------------------------------------------------------------          
      *                                                                         
      * PROGRAM GENERATED FROM ADS/O BY COBGEN CONVERTER                        
      *         SYSTEM          : AGENCIES DATABASE                             
RETMAN*         CONVERSION DATE : 10/18/00.                                     
RETMAN*         CONVERSION DATE : 06/06/01.                                     
      *                                                                         
      *----------------------------------------------------------------*        
      *       BRIEF DESCRIPTION OF FUNCTIONALITY                                
      *----------------------------------------------------------------*        
R51617* THIS PROGRAM DISPLAYS THE INQUIRY SUB-MENUS FOR FIRM CONTRACTS          
R51617* ALONG WITH THE PROFILE INFORMATION LIKE CONTRACT#,STATUS,RMO/           
R51617* REGION/OFFICE ETC.THE USER CAN SELECT ONE OF THE SUB-MENUS TO           
R51617* VIEW THE FIRM'S LICENSES/APPOINTMENTS,ADDRESS,CONTRACT                  
R51617* INFORMATION,FUNCTIONAL MANAGEMENT,AGREEMENT INFORMATION ETC.            
      *                                                                         
      *----------------------------------------------------------------*        
      *                      GENERAL INFORMATION                       *        
      *----------------------------------------------------------------*        
      *PROGRAM NAME              :   UTDIQF02                          *        
      *PROGRAM TYPE              :   COBOL-CICS-DB2                    *        
      *TYPE                      :   MAP PROGRAM                       *        
      *MAPSET USED               :   UTIQ002                           *        
      *MAPS USED                 :   UMIQ002                           *        
      *PREDECESSOR               :   UTDGEN01                          *        
      *SUCCESSOR                 :   UTDSWAP1                          *        
      *                          :   UTDIQF06                          *        
      *                          :   UTDIQF12                          *        
      *                          :   UTDIQF08                          *        
      *                          :   UTDIQO18                          *        
      *                          :   UTDIQ009                          *        
      *                          :   UTDIQF10                          *        
      *                          :   UTDIQS20                          *        
      *                          :   UTDGEN01                          *        
      *                          :   UTDIQO16                          *        
      *                          :   UTDIQC01                          *        
      *                          :   UTDADDRA                          *        
      *TRANSFER/LINK             :   TRANSFER                          *        
      *DATABASE CALLS (Y/N)      :   Y                                 *        
      *TYPE OF CHANNELS          :   PRUSEL                            *        
      *                          :   PSI                               *        
      *                          :   ADDITIONAL CHANNELS               *        
      *TYPE OF AGENTS            :   FIRM                              *        
      *FUNCTIONALITY             :   SUB-MENU PROGRAM WHICH TRANSFERS  *        
      *                              CONTROL TO THE RESPECTIVE PROGRAM *        
      *                              FOR THE INQUIRY TRANSACTION       *        
      *                              FOR  BROKER FIRM / PSI FINANCIAL  *        
      *                              ADVISOR / ADITIONAL CHANNELS      *        
      *                              ACCOUNTS EXECUTIVE /              *        
      *                              M - FINANCIAL FIRM                *        
      *----------------------------------------------------------------*        
      *                SUPPORTED FUNCTION KEYS                                  
      *---------------------------------------------------------------          
      *-----------------------------------------------------------------        
      *                                                                         
      ******************************************************************        
      * S.NO.  DATE.          DESCRIPTION OF CHANGES MADE.                   
      *----------------------------------------------------------------*        
      *  1    082102  CHANGES MADE AS PART OF SUB-PRODUCER PHASE1.           
      *               THE CHANGE TAG IS SBPROD.                              
      *  2    052203  CHANGES MADE AS PART OF POLICY OUTPUT ADDRESS.         
      *               PROJECT NUMBER - xyz02-213                             
      *               THE CHANGE TAG IS 2003B1.                              
      *  3    110504  CHANGES DONE AS A PART OF xyz04-408                    
      *               SECURITY PROPOSAL - CHANGES DONE FOR                   
      *               DBAM AND COPYBOOK CLONING                              
      *               CHANGE TAG : COB390                                    
      *  4    072005  CHANGES DONE AS A PART OF xyz05-504                    
      *               UPDATE FOR SELLING AGREEMENT,BROKER                    
      *               AGREEMENT AND ERROR & OMISSION                         
      *               CHANGE TAG : 2005C1                                    
      *  5    111005  CHANGES MADE AS PART OF xyz504 CCF#0509 - TO           
      *               STORE THE BD SELLING AGREEMENT INFORMATION AT          
      *               TIN LEVEL.                                             
      *               THE CHANGE TAG IS CCF509.                              
      *  6    041206  CHANGES MADE AS PART OF xyz504 CCF#512                 
      *               SELLING AGREEMENTS FOR LLE                             
      *               THE CHANGE TAG IS CCF512.                              
      *  7    051706  CHANGES DONE AS A PART OF xyz06-603                    
      *               TO POPULATE THE PARTICIPANT NUMBER AND                 
      *               AND FIRM TYPE FOR ANNUITY FIRMS                        
      *               ASSCOCIATED TO OFFICES HAVING VALID                    
      *               PARTICIPANT NUMBER AND FIRM TYPE                       
      *               CHANGE TAG : xyz603                                    
      *  8    090106  CHANGES DONE AS A PART OF xyz06-612                    
      *               GA LINKAGE AND ENHANCED GA PURPOSE SCREENS.            
      *               CHANGE TAG : xyz612                                    
      *  9    112006  CHANGES DONE AS A PART OF xyz06-612 CCF604             
      *               GA LINKAGE AND ENHANCED GA PURPOSE SCREENS.            
      *               CHANGE TAG : CCF604                                    
      *  10   041206  CHANGES DONE AS A PART OF xyz06-612 CCF604             
      *               REMEDY 47562                                           
      *               CHANGE TAG : R47562                                    
      *  11   070309  CHANGES DONE AS A PART OF REMEDY 51646 CRD NO.         
      *               AND BD NAME DISPLAY                                    
      *               CHANGE TAG : R51646                                    
      *  11   060309  CHANGES DONE AS A PART OF xyz08-817,                    
      *               TO POPULATE THE NPN FOR FIRM CONTRACT                   
      *               BEING INQUIRED                                          
      *               CHANGE TAG : xyz817                                     
R51617*  12   082809 THE CHANGES WERE MADE TO DISTINGUISH BETWEEN THE|        
R51617*              FIRM RECORD AND PERSON RECORD IN THE TABLE              
R51617*              UTT_3RD_PARTY_INFO.                                     
R51617*              CHANGE TAG : R51617                                     
2015C1*  13  | 091515 |CHANGES MADE AS PART OF STARTEGIC HIERACHY PH6  |        
2015C1*      |        |TO ADD A NEW CATEGORY 'BROKER INFORMATION' FOR  |        
2015C1*      |        |BROKER PERSON CONTRACTS.                        |        
2015C1*      |        |FUNCTIONAL MANAGEMENT SUB-MENU IS RENAMED TO             
2015C1*      |        |RELATIONSHIP INFORMATION AND SOLICITOR DISPLAY           
2015C1*      |        |MENU GETTING DARKENED.                                   
2015C1*      |        |FOR MANAGEMENT CONTARCTS FUNCTIONAL MANAGEMENT           
2015C1*      |        |SUB-MENU REPLACE WITH MANAGEMENT DISPLAY.                
2015C1*      |        |CHANGE TAG  : 2015C1                            |        
      *  14  |09202016|Y2K CHANGES - PHASE 3                           |        
      *      |        |CHANGE TAG  : Y2K16                             |        
      ******************************************************************        
      *---------------------------------------------------------------          
      ******************************************************************        
      *---------------------------------------------------------------          
      *                   IDMS RECORDS ACCESSED                                 
      *---------------------------------------------------------------          
      *                                                                         
      * RECORD-NAME             ACCESS-TYPE                                     
      *                                                                         
      * CNPORG                                                                  
      * CORDAGNT                                                                
I     * CPRUSINF                                                                
      * CORDDET                                                                 
      * CORDPLAN                                                                
      * CPERSON                                                                 
      * COACDTBL                                                                
      *---------------------------------------------------------------          
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
TCSD  *PROTOCOL.    MODE IS  CICS-EXEC DEBUG                                    
TCSD  *             IDMS-RECORDS MANUAL.                                        
      *                                                                         
       DATA DIVISION.                                                           
      *                                                                         
TCSD  *SCHEMA SECTION.                                                          
TCSD  *DB UTSS0001 WITHIN UTSC0001 VERSION 1.                                   
      *                                                                         
       WORKING-STORAGE SECTION.                                                 
      *                                                                         
      ** MARKER FOR THE START OF THE WORKING STORAGE SECTION.                   
      *                                                                         
       01 FILLER                               PIC X(27)                        
                             VALUE 'WORKING STORAGE STARTS HERE'.               
      *                                                                         
       01 WS-COMMAREA.                                                          
          02 WS-GLOBAL-RECS.                                                    
             EXEC SQL                                                           
                 INCLUDE CGLOBVAR                                               
             END-EXEC                                                           
TCSDB2       EXEC SQL                                                           
TCSDB2           INCLUDE CDBSTR                                                 
TCSDB2       END-EXEC                                                           
             EXEC SQL                                                           
                  INCLUDE CLICATCD                                              
             END-EXEC                                                           
                                                                                
       LINKAGE SECTION.                                                         
       01  DFHCOMMAREA.                                                         
           02  LK-FILLER.                                                       
               03  LK-FILLER03                     PIC X(1)                     
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
           MOVE WC-CURR-PROG-NAME                      TO                       
                WS-CURR-PGM-ID                                                  
                INCLUDE UTSETFLG                                                
