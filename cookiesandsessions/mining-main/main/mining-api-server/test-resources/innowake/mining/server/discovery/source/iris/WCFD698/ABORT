       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.  ABORT.                                                      
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
             SELECT OPTIONAL ABORTFA  ASSIGN TO DFSVSAMP.                       
                                                                                
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  ABORTFA                                                              
           RECORDING   MODE IS F                                                
           DATA RECORD IS   ABORTFA-REC.                                        
                                                                                
       01  ABORTFA-REC             PIC X(80).                                   
                                                                                
       WORKING-STORAGE SECTION.                                                 
       01 WSC-CONSTANTS.                                                        
          05 WSC-IMS         PIC X(03) VALUE 'IMS'.                             
          05 WSC-NON-IMS     PIC X(03) VALUE 'NON'.                             
          05 WSC-IO-PCB-NAME PIC X(08) VALUE 'IOPCB  '.                         
       01 WSH-HOLD-FIELDS.                                                      
          05 WSH-RUN-TYPE    PIC X(03) VALUE SPACES.                            
          05 WSH-RETURN-CODE PIC 9(07) VALUE ZERO.                              
          05 WSH-REASON-CODE PIC 9(07) VALUE ZERO.                              
       01 WSC-ROLL           PIC X(04) VALUE 'ROLB'.                            
                                                                                
       01 WS-AIB-MASK.                                                          
          05 WS-AIB-EYECATCHER      PIC X(08) VALUE 'DFSAIB  '.                 
      *   NEEDS 4                                                               
          05 WS-AIB-ALLOC-LENGTH    PIC 9(09) COMP VALUE 0.                     
          05 WS-AIB-SUB-FUNC-CDE    PIC X(08) VALUE SPACES.                     
          05 WS-AIB-RESOURCE-NAME   PIC X(08) VALUE SPACES.                     
          05 FILLER                 PIC X(16).                                  
      *   NEEDS 4                                                               
          05 WS-AIB-MAX-OUTPUT-LEN  PIC 9(09) COMP VALUE 0.                     
      *   NEEDS 4                                                               
          05 WS-AIB-OUTPUT-LEN-USED PIC 9(09) COMP VALUE 0.                     
          05 FILLER                 PIC X(12).                                  
          05 WS-AIB-RETURN-CODE.                                                
             10 WS-AIB-RETURN-CODE-NBR PIC 9(7) COMP VALUE 0.                   
          05 WS-AIB-REASON-CODE.                                                
             10 WS-AIB-REASON-CODE-NBR PIC 9(7) COMP VALUE 0.                   
          05 FILLER                 PIC X(04).                                  
      *   NEEDS 4                                                               
          05 WS-AIB-RESOURCE-ADDR   PIC 9(09) COMP VALUE 0.                     
          05 FILLER                 PIC X(48).                                  
      ******************************************************************        
      **   LINKAGE SECTION                                            **        
      ******************************************************************        
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01  WSP-ABORT-PARMS.                                                     
           05  WSP-ABORT-CODE                 PIC S9(4) COMP.                   
                                                                                
      ******************************************************************        
      **                                                              **        
      **   PROCEDURE DIVISION                                         **        
      **                                                              **        
      ******************************************************************        
                                                                                
       PROCEDURE DIVISION USING WSP-ABORT-PARMS.                                
                                                                                
       00000-MAIN-CONTROL.                                                      
                                                                                
      *                                                                         
      * BEGIN TIR 23439                                                         
      *                                                                         
                                                                                
      *********************************************************                 
      *   PRE-LOAD THE RUN TYPE AS IMS                        *                 
      *********************************************************                 
           MOVE WSC-IMS TO WSH-RUN-TYPE.                                        
                                                                                
           OPEN INPUT ABORTFA.                                                  
      *********************************************************                 
      *  IF THE FIRST READ GETS AN AT END, THEN THE IMS BUFFER*                 
      *  DD IS NOT AVAILABLE.  THAT MAKES THIS A NON-IMS RUN. *                 
      *********************************************************                 
           READ ABORTFA                                                         
             AT END                                                             
                 MOVE WSC-NON-IMS TO WSH-RUN-TYPE.                              
                                                                                
           IF WSH-RUN-TYPE = WSC-IMS                                            
               CLOSE ABORTFA                                                    
               MOVE WSC-IO-PCB-NAME TO WS-AIB-RESOURCE-NAME                     
               MOVE 128 TO WS-AIB-ALLOC-LENGTH                                  
               CALL 'AIBTDLI' USING WSC-ROLL WS-AIB-MASK                        
               DISPLAY 'ROLLBACK HAS BEEN CALLED'                               
               IF WS-AIB-RETURN-CODE NOT = SPACES                               
                   MOVE WS-AIB-RETURN-CODE-NBR TO WSH-RETURN-CODE               
                   IF WSH-RETURN-CODE = ZEROES                                  
                       DISPLAY 'BACKOUT HAS BEEN PERFORMED'                     
                   END-IF                                                       
               END-IF                                                           
               IF WS-AIB-REASON-CODE NOT = SPACES                               
                   MOVE WS-AIB-REASON-CODE-NBR TO WSH-REASON-CODE               
               END-IF                                                           
               DISPLAY 'RETURN CODE: ' WSH-RETURN-CODE                          
               DISPLAY 'REASON CODE: ' WSH-REASON-CODE                          
               CALL 'CEE3DMP' USING WSP-ABORT-CODE                              
               CALL 'CEE3ABD' USING WSP-ABORT-CODE                              
           ELSE                                                                 
               CALL 'CEE3DMP' USING WSP-ABORT-CODE                              
               CALL 'CEE3ABD' USING WSP-ABORT-CODE                              
           END-IF.                                                              
                                                                                
      *                                                                         
      * END TIR 23439                                                           
      *                                                                         
           GOBACK.                                                              
                                                                               
