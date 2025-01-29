       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.             DNA2.
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       01  WS-COPYRIGHT PIC X(57) VALUE 'Hello'.
       01  DEP-RLSELIT.                                                 
       01  MIC-RLSELIT.                                                 
       01  SRW105-AREAS.                                                
       01  WS-RGNMIM3013AREAS.                                          
       01  MIMST-RECORD.                                                
       01  MIWPRI-AREAS.                                                
       01  DPWPRI-AREAS.                                                
       01  MI3013-RECORD.                                               
       01  MIWMAST-AREAS.                                               
       01  WS-ABEND.                                                    
       01  WS-WORKAREAS.                                                
           03  WS-SUB1                 PIC S9(04)      COMP             
                                                       VALUE ZEROS.     
           03  WS-SUB2                 PIC S9(04)      COMP             
                                                       VALUE ZEROS.     
           03  WS-FUNCTION             PIC X(01).                       
           03  WS-MIMKEYHOLD.                                           
               05  WS-HOLDAPPL         PIC 9(02).                       
               05  WS-HOLDPARM         PIC 9(03).                       
               05  WS-HOLDACCUM        PIC 9(02).                       
           03  WS-MODELFOUND           PIC X(01).                       
      ******************************************************************
       LINKAGE SECTION.                                                 
       01  LS-RGNMIM3013AREAS.                                          
      ******************************************************************
       PROCEDURE DIVISION                                               
               USING                                                    
               LS-RGNMIM3013AREAS.                                      
      ******************************************************************
       MAIN-LOGIC SECTION.                                              
       ML-START.                                                        
           MOVE ZEROS TO WS-MIMKEYHOLD.                                 
           MOVE SPACE TO LS-RGNMIM3013ERROR.                            
           IF LS-RGNMIM3013FUNC IS EQUAL TO 'B'                         
               PERFORM MIM-BUILD3013.                                   
           IF LS-RGNMIM3013ERROR IS NOT EQUAL TO 'Y'                    
               MOVE WS-RGNMIM3013DATA TO LS-RGNMIM3013DATA.             
           GOBACK.                                                      
       ML-EXIT.                                                         
           EXIT.                                                        
      ******************************************************************
       MIM-BUILD3013 SECTION.                                           
       MB-START.                                                        
           MOVE LOW-VALUES TO WS-RGNMIM3013DATA.                        
           MOVE ZEROS TO WS-SUB1.                                       
       MB-INITPOINTERSLOOP.                                             
           ADD +1 TO WS-SUB1.                                           
           IF WS-SUB1 IS LESS THAN +1001                                
               MOVE ZEROS TO WS-RGNMIM3013P (WS-SUB1)                   
               GO TO MB-INITPOINTERSLOOP.                               
           MOVE ZEROS TO WS-SUB1.                                       
           MOVE SPACES TO MI3013-RECORD.                                
           MOVE LS-RGNMIM3013INST TO MIM-3013KINST.                     
           MOVE 3013 TO MIM-3013KREC.                                   
           MOVE LS-RGNMIM3013NBR TO MIM-3013KREGION.                    
           MOVE LS-RGNMIM3013APPL TO MIM-3013KAPPL.                     
           MOVE ZEROS TO MIM-3013KPARM.                                 
           MOVE ZEROS TO MIM-3013KACCUM.                                
           MOVE ZEROS TO MIM-3013KEFFDT.                                
           MOVE LS-RGNMIM3013MODEL TO MIM-3013KMODEL.                   
           MOVE MIC-DPS-3013-PRI TO MIC-MST-REC-PRI.                    
           MOVE MI3013-RECORD TO MIMST-RECORD.                          
           PERFORM MIC-MST-GET-EQUAL-REQUEST.                           
           GO TO MB-MIM3013LOOP1.                                       
       MB-MIM3013LOOP.                                                  
           PERFORM MIC-MST-GET-NEXT-REQUEST.                            
       MB-MIM3013LOOP1.                                                 
           IF WS-FUNCTION IS EQUAL TO 'E'                               
               GO TO MB-EXIT.                                           
           IF WS-FUNCTION IS NOT EQUAL TO SPACES                        
               GO TO MB-ERROR.                                          
           MOVE MIMST-RECORD TO MI3013-RECORD.                          
           IF MIM-3013KINST IS NOT EQUAL TO LS-RGNMIM3013INST           
               OR MIM-3013KREGION IS NOT EQUAL TO LS-RGNMIM3013NBR      
               OR MIM-3013KAPPL IS NOT EQUAL TO LS-RGNMIM3013APPL       
               GO TO MB-EXIT.                                           
           IF MIM-3013KEFFDT IS GREATER THAN LS-RGNMIM3013EFFDT         
               GO TO MB-MIM3013LOOP.                                    
           IF LS-RGNMIM3013MODEL IS EQUAL TO SPACE                      
               AND (MIM-3013KMODEL IS NOT EQUAL TO LS-RGNMIM3013MODEL   
               OR (MIM-3013KAPPL IS EQUAL TO WS-HOLDAPPL                
               AND MIM-3013KPARM IS EQUAL TO WS-HOLDPARM                
               AND MIM-3013KACCUM IS EQUAL TO WS-HOLDACCUM))            
               GO TO MB-MIM3013LOOP.                                    
           IF MIM-3013KAPPL IS NOT EQUAL TO WS-HOLDAPPL                 
               OR MIM-3013KPARM IS NOT EQUAL TO WS-HOLDPARM             
               OR MIM-3013KACCUM IS NOT EQUAL TO WS-HOLDACCUM           
               MOVE MIM-3013KAPPL TO WS-HOLDAPPL                        
               MOVE MIM-3013KPARM TO WS-HOLDPARM                        
               MOVE MIM-3013KACCUM TO WS-HOLDACCUM                      
               MOVE SPACE TO WS-MODELFOUND.                             
       MB-CHECKMODEL.                                                   
      **THIS PARAGRAPH KEEPS THE ROUTINE FROM OVERLAYING A CURRENT      
      **RECORD WITH PAST-DATED RECORD WHILE LOOKING FOR A MODEL RECORD. 
           IF LS-RGNMIM3013MODEL IS NOT EQUAL TO 'M'                    
               GO TO MB-3013MODEL.                                      
           MOVE WS-RGNMIM3013P (MIM-3013KPARM) TO WS-SUB2.              
           IF WS-SUB2 IS EQUAL TO ZEROS                                 
               GO TO MB-3013MODEL.                                      
           IF WS-RGNMIM3013A (WS-SUB2 MIM-3013KACCUM)                   
                   IS NOT EQUAL TO LOW-VALUES                           
               AND MIM-3013KMODEL IS EQUAL TO SPACE                     
               GO TO MB-MIM3013LOOP.                                    
       MB-3013MODEL.                                                    
      **THIS PARAGRAPH IS USED TO BYPASS LOADING OF ANY SUBSEQUENT      
      **PAST-DATED RECORDS ONCE A MODEL RECORD HAS BEEN LOADED.         
           IF LS-RGNMIM3013MODEL IS EQUAL TO 'M'                        
               AND WS-MODELFOUND IS NOT EQUAL TO SPACE                  
               GO TO MB-MIM3013LOOP.                                    
           IF MIM-3013KMODEL IS EQUAL TO 'M'                            
               MOVE 'Y' TO WS-MODELFOUND.                               
           IF (WS-RGNMIM3013P (MIM-3013KPARM) IS EQUAL TO ZERO          
               AND MIM-3013KPARM IS NOT EQUAL TO ZERO)                  
               OR (WS-RGNMIM3013P (1000) IS EQUAL TO ZERO               
               AND MIM-3013KPARM IS EQUAL TO ZERO)                      
               ADD +1 TO WS-SUB1.                                       
           IF WS-SUB1 IS GREATER THAN WS-RGNMIM3013MAXSIZE              
               GO TO MB-ERROR.                                          
           MOVE MIM-3013DATA TO WS-RGNMIM3013A (WS-SUB1 MIM-3013KACCUM).
           IF MIM-3013KPARM IS EQUAL TO ZEROS                           
               MOVE WS-SUB1 TO WS-RGNMIM3013P (1000)                    
           ELSE                                                         
               MOVE WS-SUB1 TO WS-RGNMIM3013P (MIM-3013KPARM).          
           GO TO MB-MIM3013LOOP.                                        
       MB-ERROR.                                                        
           MOVE 'Y' TO LS-RGNMIM3013ERROR.                              
       MB-EXIT.                                                         
           EXIT.                                                        
      ******************************************************************
       MIC-MST-API SECTION.                                             
       COPY MIPMSTA.                                                    
      ******************************************************************
