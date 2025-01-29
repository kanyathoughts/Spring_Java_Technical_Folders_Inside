       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. UTPSPA66.                                                    
      ******************************************************************        
      * AUTHOR             TATA CONSULTANCY SERVICES                   *        
      * TITLE              ADB10-020 MQ FOR CONTRACT - HIERARCHY FETCH *        
      * DATE WRITTEN       08/18/2010                                  *        
      * INSTALLATION       Dec'11                                      *        
      * FUNCTION           The program UTPSPA66 will take the Contract *        
      *                    Number, Date and System ID as the input.    *        
      *                    It retrieves all the contract related infor-*        
      *                    mation and the hierarchical information.    *        
      ******************************************************************        
      *             P R O G R A M   C H A N G E   L O G                *        
      ******************************************************************        
      *                                                                *        
      *   LOG #    DATE      NAME      CHANGE DESCRIPTION              *        
      *   -----   ------   ---------   ----------------------------    *        
      *                                                                *        
      ******************************************************************        
                                                                                
       ENVIRONMENT DIVISION.                                                    
                                                                                
       DATA DIVISION.                                                           
                                                                                
      *-------------- WORKING STORAGE SECTION BEGINS ------------------*        
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      *                                                                         
       01 WS-MQ-FIELDS.                                                         
          05 HCONN                             PIC S9(09) BINARY.               
          05 Q-HANDLE                          PIC S9(09) BINARY.               
          05 OPEN-OPTIONS                      PIC S9(09) BINARY.               
          05 CLOSE-OPTIONS                     PIC S9(09) BINARY.               
          05 COMPLETION-CODE                   PIC S9(09) BINARY.               
          05 G-OPEN-CODE                       PIC S9(09) BINARY.               
          05 REASON                            PIC S9(09) BINARY.               
          05 BUFFER-LENGTH                     PIC S9(09) BINARY.               
          05 DATA-LENGTH                       PIC S9(09) BINARY.               
          05 MESSAGE-ID                        PIC X(24).                       
          05 WS-MQTM-ENVDATA.                                                   
             07 WS-MQTM-TRANSID                PIC X(04).                       
             07 WS-MQTM-MSGID                  PIC X(24).                       
             07 WS-MQTM-CORRELID               PIC X(24).                       
      *                                                                         
       01 WORK-FIELDS.                                                          
          05 COMM-LENGTH                       PIC S9(04) COMP.                 
          05 WS-RESP                           PIC S9(08) COMP                  
                                                    VALUE ZEROS.                
          05 WS-QUEUE-OPEN-SW                  PIC X(01).                       
             88 QUEUE-OPEN                          VALUE 'Y'.                  
             88 QUEUE-CLOSE                         VALUE 'N'.                  
          05 WS-RES-QUEUE-SW                   PIC X(01).                       
             88 RES-QUEUE-OPEN                      VALUE 'Y'.                  
             88 RES-QUEUE-CLOSE                     VALUE 'N'.                  
      *                                                                         
       01 WS-OP-RESPONSE-AREA.                                                  
          05 WS-OP-REPLY-CODE              PIC X(04).                           
          05 WS-OP-REPLY-MSG               PIC X(80).                           
          05 WS-OP-CNTR-NUM                PIC X(06).                           
          05 WS-OP-GA-IND-TYPE             PIC X(05).                           
          05 WS-OP-PERS-FIRM-IND           PIC X(01).                           
          05 WS-OP-CNTR-TAG                PIC X(03).                           
          05 WS-OP-FIRST-NAME              PIC X(20).                           
          05 WS-OP-MID-NAME                PIC X(25).                           
          05 WS-OP-LAST-NAME               PIC X(30).                           
          05 WS-OP-FIRM-NAME               PIC X(50).                           
          05 WS-OP-TAX-ID-NUM              PIC X(09).                           
          05 WS-OP-OFFICE.                                                      
             10 WS-OP-OFFICE-CD            PIC X(04).                           
             10 WS-OP-DETACH-CD            PIC X(01).                           
          05 WS-OP-OFFICE-NAME             PIC X(50).                           
          05 WS-OP-CNTR-FINAL-CD           PIC X(01).                           
          05 WS-OP-CNTR-SUBFINAL-CD        PIC X(02).                           
          05 WS-OP-CNTR-STATUS-DESC        PIC X(50).                           
          05 WS-OP-CNTR-PREFIX             PIC X(01).                           
          05 WS-OP-CNTR-TYP-CD             PIC X(01).                           
          05 WS-OP-CNTR-PLN-CD             PIC X(02).                           
          05 WS-OP-CNTR-SBTYP-CD           PIC X(02).                           
          05 WS-OP-CNTR-START-DT           PIC X(10).                           
          05 WS-OP-CNTR-END-DT             PIC X(10).                           
          05 WS-OP-ADDR-INFO     OCCURS 5 TIMES.                                
             10 WS-OP-ADDR-TYPE            PIC X(01).                           
             10 WS-OP-ADDR-L1              PIC X(40).                           
             10 WS-OP-ADDR-L2              PIC X(40).                           
             10 WS-OP-ADDR-L3              PIC X(40).                           
             10 WS-OP-CITY-NAME            PIC X(20).                           
             10 WS-OP-STATE-CD             PIC X(02).                           
             10 WS-OP-ZIP-CD               PIC X(10).                           
             10 WS-OP-CNTRY-CD             PIC X(02).                           
             10 WS-OP-PHONE-NUM            PIC X(10).                           
             10 WS-OP-FAX-NUM              PIC X(10).                           
          05 WS-OP-SOL-FIRM-CNTR-NUM       PIC X(06).                           
          05 WS-OP-CNTR-REL-DATA      OCCURS 25 TIMES.                          
             10 WS-OP-CHLD-CNTR-NUM        PIC X(06).                           
             10 WS-OP-PARNT-CNTR-NUM       PIC X(06).                           
             10 WS-OP-DIR-IND-REL-IND      PIC X(01).                           
             10 WS-OP-PARNT-PERS-FIRM-IND  PIC X(01).                           
             10 WS-OP-PARNT-CNTR-TAG       PIC X(03).                           
             10 WS-OP-PARNT-CNTR-FINAL-CD  PIC X(01).                           
             10 WS-OP-PARNT-CNTR-PFX-ID    PIC X(01).                           
             10 WS-OP-PARNT-CNTR-TYP-CD    PIC X(01).                           
             10 WS-OP-PARNT-CNTR-PLN-CD    PIC X(02).                           
             10 WS-OP-PARNT-CNTR-SBTYP-CD  PIC X(02).                           
             10 WS-OP-REL-START-DT         PIC X(10).                           
             10 WS-OP-REL-END-DT           PIC X(10).                           
             10 WS-OP-REL-TYP-CD           PIC X(01).                           
      *                                                                         
       01 ARR-REL-DETAILS.                                                      
          05 ARR-REL-DATA   OCCURS 25 TIMES.                                    
             10 ARR-CHLD-CNTR-NUM        PIC X(06).                             
             10 ARR-PARNT-CNTR-NUM       PIC X(06).                             
             10 ARR-DIR-IND-REL-IND      PIC X(01).                             
             10 ARR-PARNT-PERS-FIRM-IND  PIC X(01).                             
             10 ARR-PARNT-CNTR-TAG       PIC X(03).                             
             10 ARR-PARNT-CNTR-FINAL-CD  PIC X(01).                             
             10 ARR-PARNT-CNTR-PFX-ID    PIC X(01).                             
             10 ARR-PARNT-CNTR-TYP-CD    PIC X(01).                             
             10 ARR-PARNT-CNTR-PLN-CD    PIC X(02).                             
             10 ARR-PARNT-CNTR-SBTYP-CD  PIC X(02).                             
             10 ARR-REL-START-DT         PIC X(10).                             
             10 ARR-REL-END-DT           PIC X(10).                             
             10 ARR-REL-TYP-CD           PIC X(01).                             
      *                                                                         
       01 WS-ERROR-DETAILS.                                                     
          05 WS-ERROR-REPLY-IND                PIC X(01) VALUE 'N'.             
             88 ERROR-REPLY                         VALUE 'Y'.                  
             88 ERROR-NO-REPLY                      VALUE 'N'.                  
                                                                                
          05 WS-ERROR-OUT.                                                      
             07 WS-ERR-RET-CODE                PIC 9(08).                       
             07 WS-ERR-MSG                     PIC X(50) VALUE SPACES.          
      *                                                                         
       01 WS-TSQ.                                                               
          05 WS-TSQ-LEN                        PIC S9(04) COMP.                 
          05 WS-TSQ-NAME                       PIC X(08).                       
          05 WS-TSQ-DATA.                                                       
             07 WS-TSQ-DATE                    PIC X(10).                       
             07 FILLER                         PIC X(01) VALUE SPACES.          
             07 WS-TSQ-TIME                    PIC 9(06).                       
             07 FILLER                         PIC X(01) VALUE SPACES.          
             07 WS-TSQ-RET-CODE                PIC ZZZZZ9999+.                  
             07 WS-TSQ-MSG                     PIC X(50).                       
      *                                                                         
       01 WS-VSAM-TABLE.                                                        
          02 WS-VSAM-TABLE-KEY.                                                 
             03 WS-TABLE-NAME PIC X(8).                                         
             03 WS-TABLE-TYPE PIC X(1).                                         
             03 WS-ENCODE-VALUE PIC X(30).                                      
          02 WS-DECODE-VALUE PIC X(80).                                         
          02 FILLER          PIC X(14).                                         
      *                                                                         
       01 WS-NULL-IND.                                                  00019700
          05 WS-NULL-IND-FNAME                  PIC S9(4) COMP.         00019800
          05 WS-NULL-IND-MNAME                  PIC S9(4) COMP.         00019900
          05 WS-NULL-IND-LNAME                  PIC S9(4) COMP.         00020000
          05 WS-NULL-IND-SSN                    PIC S9(4) COMP.         00020000
      *                                                                         
       01  WS-MISC-VARS.                                                        
           05 WS-CNTR-NUM                     PIC X(6) VALUE SPACES.            
           05 WS-GA-CNTR                      PIC X(6) VALUE SPACES.            
           05 WS-TEMP-CNTR                    PIC X(6) VALUE SPACES.            
           05 WS-INP-DATE                     PIC X(10) VALUE SPACES.           
           05 BRK-DIR-REL-CTR                 PIC S9(4) USAGE IS COMP-4.        
           05 SOL-IND-REL-CTR                 PIC S9(4) USAGE IS COMP-4.        
           05 ARRAY-CTR                       PIC S9(4) USAGE IS COMP-4.        
           05 ADDR-CTR                        PIC S9(4) USAGE IS COMP-4.        
           05 WS-GEORVP-ST-CD                 PIC X(02) VALUE SPACES.           
           05 WS-GEORVP-ZIP-CD                PIC X(10) VALUE SPACES.           
           05 WS-GEORVP-ZIP-CD-TMP            PIC X(05) VALUE SPACES.           
           05 WS-INVALID-CNTR                 PIC X(1)  VALUE 'N'.              
           05 WS-DATE-PROC                    PIC X(10) VALUE SPACES.           
           05 WS-TRC-DATE2                    PIC X(08) VALUE SPACES.           
           05 WS-TMP-PARNT-PERS-FIRM-IND      PIC X(01) VALUE SPACES.           
           05 WS-SYS-ID                       PIC X(10) VALUE SPACES.           
           05 WS-SQLCODE                      PIC 9(04).                        
           05 WS-COMMIT                       PIC X(01) VALUE 'N'.      00007300
           05 SUB                             PIC S9(4) COMP.                   
           05 WS-COUNT                        PIC S9(4) USAGE IS COMP-4.        
           05 WS-CNT                          PIC S9(4) COMP.                   
           05 CICS-TIME                       PIC S9(15) COMP-3.                
           05 CURRENT-PROCESSING-DATE         PIC X(10).                        
           05 WS-TIME.                                                          
             07 WS-HH                         PIC 9(02).                        
             07 WS-MM                         PIC 9(02).                        
             07 WS-SS                         PIC 9(02).                        
      *                                                                         
       01 WS-FIRST-TIME                     PIC X(01) VALUE 'N'.                
          88 FIRST-TIME                               VALUE 'Y'.                
       01 WS-SOLICITOR                      PIC X(01) VALUE 'N'.                
          88 SOLICITOR                                VALUE 'Y'.                
       01 WS-BROKER                         PIC X(01) VALUE 'N'.                
          88 BROKER                                   VALUE 'Y'.                
       01 WS-GA                             PIC X(01) VALUE 'N'.                
          88 GA                                       VALUE 'Y'.                
       01 WS-RVP                            PIC X(01) VALUE 'N'.                
          88 RVP                                      VALUE 'Y'.                
       01 WS-INPUT-TYPE-INDICATOR           PIC X(01) VALUE 'N'.                
          88 INPUT-IS-FIRM                            VALUE '1'.                
          88 INPUT-IS-PERSON                          VALUE '2'.                
       01 WS-SOLICITOR-BUS-ADDR             PIC X(01) VALUE 'N'.                
          88 SOLICITOR-BUS-ADDR                       VALUE 'Y'.                
       01 WS-SOL-BFIRM-ACTIVE               PIC X(01) VALUE 'N'.                
          88 SOL-BFIRM-ACTIVE                         VALUE 'Y'.                
       01 WS-SOL-PARENT-IS-GA               PIC X(01) VALUE 'N'.                
          88 SOL-PARENT-IS-GA                         VALUE 'Y'.                
       01 WS-BFIRM-RELS-END                 PIC X(01) VALUE 'N'.                
          88 BFIRM-RELS-END                           VALUE 'Y'.                
       01 WS-GA-RELS-END                    PIC X(01) VALUE 'N'.                
          88 GA-RELS-END                              VALUE 'Y'.                
       01 WS-CPC-FOUND                      PIC X(01) VALUE 'N'.                
          88 CPC-FOUND                                VALUE 'Y'.                
      ** Input Layout from the Request queue.                                   
      *                                                                         
       01 ADB-INPUT-AREA.                                                       
          EXEC SQL                                                              
               INCLUDE ICPSPA66                                                 
          END-EXEC.                                                             
      *                                                                         
      ** Output Record Layout to the Response queue.                            
      *                                                                         
       01 ADB-OUTPUT-AREA.                                                      
          EXEC SQL                                                              
               INCLUDE OCPSPA66                                                 
          END-EXEC.                                                             
      *                                                                         
      *                                                                         
      ** DCLGEN COPYBOOKS                                                       
      *                                                                         
            EXEC SQL                                                            
                 INCLUDE SQLCA                                                  
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDOARL                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDOAGT                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDOADL                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDOAPL                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDMRKR                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDNORG                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDPERS                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDPORG                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDPOOA                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDPRSI                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDOSRL                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDPSFL                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDNOSI                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDMULS                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTGAIND                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
                 INCLUDE UTTDRVGE                                               
            END-EXEC.                                                           
            EXEC SQL                                                            
               INCLUDE TRCCONVR                                                 
            END-EXEC.                                                           
      *                                                                         
      *INDICATORS.                                                              
      *                                                                         
       01 WS-TRCSMALL                      PIC X(8)  VALUE                      
                                                         'TRCSMALL'.            
      *                                                                         
      ** MQ COPYLIBS.                                                           
      *                                                                         
       01 MQ-CONSTANTS.                                                         
          COPY CMQV.                                                            
       01 OBJECT-DESCRIPTOR.                                                    
          COPY CMQODV.                                                          
       01 MESSAGE-DESCRIPTOR.                                                   
          COPY CMQMDV.                                                          
       01 GMOPTIONS.                                                            
          COPY CMQGMOV.                                                         
       01 PMOPTIONS.                                                            
          COPY CMQPMOV.                                                         
       01 MQM-TRIGGER-MESSAGE.                                                  
          COPY CMQTML.                                                          
      *                                                                         
      * This cursor will fetch all active relationships for the Broker          
      * Firm or Broker Individual.                                              
            EXEC SQL                                                            
               DECLARE BFIRM_RELS CURSOR FOR                                    
               SELECT A.PARNT_CNTR_NUM                                          
                     ,A.REL_BG_DT                                               
                     ,A.REL_EN_DT                                               
                     ,A.REL_TYP_CD                                              
                     ,B.CNTR_PFX_ID                                             
                     ,B.CNTR_TYP_CD                                             
                     ,B.CNTR_SBTYP_CD                                           
                     ,C.PLN_CD                                                  
                     ,D.FINAL_CD                                                
                     ,(CASE WHEN E.SSN_NUM IS NULL THEN 'F'                     
                              ELSE 'P' END)                                     
                 FROM UTT_OA_REL A                                              
                     ,UTT_OA_DTL B                                              
                     ,UTT_OA_PLN C                                              
                     ,UTT_OA D                                                  
                 LEFT OUTER JOIN                                                
                      UTT_PERS E                                                
                      ON D.MKTR_FOCAL_SUBJ_ID = E.SUBJ_ID                       
               WHERE A.CHLD_CNTR_NUM = :DCLUTT-OA-REL.CHLD-CNTR-NUM             
                 AND A.REL_TYP_CD NOT IN ('G','F')                              
                 AND (:WS-INP-DATE BETWEEN A.REL_BG_DT AND A.REL_EN_DT          
                       OR (A.REL_FINAL_CD = 'A' AND                             
                           :WS-INP-DATE >= A.REL_BG_DT))                        
                 AND D.CNTR_NUM = A.PARNT_CNTR_NUM                              
                 AND B.OA_CNTR_NUM = A.PARNT_CNTR_NUM                           
                 AND B.CRT_TS IN                                                
                    (SELECT MAX(Z.CRT_TS) FROM UTT_OA_DTL Z                     
                      WHERE Z.OA_CNTR_NUM = B.OA_CNTR_NUM                       
                        AND (Z.PRIR_CONTR_IND IS NULL                           
                         OR Z.PRIR_CONTR_IND = ' '))                            
                 AND C.CNTR_NUM = B.OA_CNTR_NUM                                 
                 AND C.ORDAGT_DTL_TS = B.CRT_TS                                 
                ORDER BY B.PRIR_CONTR_IND DESC                                  
                       ,B.CRT_TS DESC                                           
                       ,C.CRT_TS DESC                                           
            END-EXEC.                                                           
      *                                                                         
      * This cursor will fetch all active relationships for GA Firm or          
      * GA Person.                                                              
            EXEC SQL                                                            
               DECLARE GA_RELS CURSOR FOR                                       
               SELECT A.PARNT_CNTR_NUM                                          
                     ,A.REL_BG_DT                                               
                     ,A.REL_EN_DT                                               
                     ,A.REL_TYP_CD                                              
                     ,B.CNTR_PFX_ID                                             
                     ,B.CNTR_TYP_CD                                             
                     ,B.CNTR_SBTYP_CD                                           
                     ,C.PLN_CD                                                  
                     ,D.FINAL_CD                                                
                     ,(CASE WHEN E.SSN_NUM IS NULL THEN 'F'                     
                              ELSE 'P' END)                                     
                 FROM UTT_OA_REL A                                              
                     ,UTT_OA_DTL B                                              
                     ,UTT_OA_PLN C                                              
                     ,UTT_OA D                                                  
                 LEFT OUTER JOIN                                                
                      UTT_PERS E                                                
                      ON D.MKTR_FOCAL_SUBJ_ID = E.SUBJ_ID                       
               WHERE A.CHLD_CNTR_NUM = :DCLUTT-OA-REL.CHLD-CNTR-NUM             
                 AND A.REL_TYP_CD NOT IN ('G','F','B')                          
                 AND (:WS-INP-DATE BETWEEN A.REL_BG_DT AND A.REL_EN_DT          
                       OR (A.REL_FINAL_CD = 'A' AND                             
                           :WS-INP-DATE >= A.REL_BG_DT))                        
                 AND D.CNTR_NUM = A.PARNT_CNTR_NUM                              
                 AND B.OA_CNTR_NUM = A.PARNT_CNTR_NUM                           
                 AND B.CRT_TS IN                                                
                    (SELECT MAX(Z.CRT_TS) FROM UTT_OA_DTL Z                     
                      WHERE Z.OA_CNTR_NUM = B.OA_CNTR_NUM                       
                        AND (Z.PRIR_CONTR_IND IS NULL                           
                         OR Z.PRIR_CONTR_IND = ' '))                            
                 AND C.CNTR_NUM = B.OA_CNTR_NUM                                 
                 AND C.ORDAGT_DTL_TS = B.CRT_TS                                 
                ORDER BY B.PRIR_CONTR_IND DESC                                  
                       ,B.CRT_TS DESC                                           
                       ,C.CRT_TS DESC                                           
            END-EXEC.                                                           
      *                                                                         
      *================================================================*        
       PROCEDURE DIVISION.                                                      
      *================================================================*        
                                                                                
      *----------------------------------------------------------------*        
       0000-MAINPARA.                                                           
      *----------------------------------------------------------------*        
      *MAIN PARAGRAPH TO PROCESS THIS NEW MQ APPLICATION               *        
      *----------------------------------------------------------------*        
                                                                                
           PERFORM 1000-GET-TRIGGER-PARA                                        
              THRU 1000-GET-TRIGGER-EXIT                                        
                                                                                
           PERFORM 2000-GET-REQUEST-PARA                                        
              THRU 2000-GET-REQUEST-EXIT                                        
                                                                                
           PERFORM 3000-PROCESS-REQUEST-PARA                                    
              THRU 3000-PROCESS-REQUEST-EXIT                                    
                                                                                
           PERFORM 7000-SET-RESPONSE-PARA                                       
              THRU 7000-SET-RESPONSE-EXIT                                       
                                                                                
           PERFORM 9999-RETURN                                                  
           .                                                                    
                                                                                                                                                      
      *----------------------------------------------------------------*        
       3222-FETCH-STATUS-DESC-PARA.                                             
      *----------------------------------------------------------------*        
      *THIS PARA RETRIEVES THE FINAL CODE DESCRIPTION FROM VSAMTABL.   *        
      *THE EXISTING LOGIC CODETBL HAS THE LOGIC TO FETCH STATUS DESCRI-*        
      *PTION FROM VSAMTABL.                                            *        
      *----------------------------------------------------------------*        
           MOVE SPACES TO WS-VSAM-TABLE                                         
           MOVE 'UTTSTACD' TO WS-TABLE-NAME                                     
           MOVE 'C'        TO WS-TABLE-TYPE                                     
           MOVE 'O' TO WS-ENCODE-VALUE(1:1)                                     
           MOVE WS-OP-CNTR-FINAL-CD    TO WS-ENCODE-VALUE(2:1)                  
           MOVE WS-OP-CNTR-SUBFINAL-CD TO WS-ENCODE-VALUE(3:2)                  
           IF WS-ENCODE-VALUE(1:1) = LOW-VALUE                                  
              MOVE 'LOW-VALUES' TO WS-ENCODE-VALUE                              
           END-IF                                                               
           EXEC CICS IGNORE CONDITION                                           
                NOTFND                                                          
                DUPKEY                                                          
           END-EXEC                                                             
           EXEC CICS READ                                                       
                FILE('VSAMTABL')                                                
                INTO(WS-VSAM-TABLE)                                             
                RIDFLD(WS-VSAM-TABLE-KEY)                                       
           END-EXEC                                                             
           EVALUATE EIBRESP                                                     
               WHEN ZERO                                                        
                  MOVE WS-DECODE-VALUE TO WS-OP-CNTR-STATUS-DESC                
               WHEN DFHRESP(NOTFND)                                             
                  MOVE SPACES TO WS-OP-CNTR-STATUS-DESC                         
               WHEN OTHER                                                       
                  MOVE '909D' TO OP-REPLY-CODE                                  
                  MOVE 'ERROR READING VSAM TABLE VSAMTABL FOR STATUS'           
                              TO OP-REPLY-MSG                                   
                  INSPECT OP-RESPONSE-AREA                                      
                  CONVERTING X'00' TO X'40'                                     
                  PERFORM 7000-SET-RESPONSE-PARA                                
                     THRU 7000-SET-RESPONSE-EXIT                                
                  PERFORM 9999-RETURN                                           
           END-EVALUATE                                                         
           .                                                                    
      *                                                                         
       3222-FETCH-STATUS-DESC-PARA-X.                                           
           EXIT.                                                                
      *                                                                                                                                  
