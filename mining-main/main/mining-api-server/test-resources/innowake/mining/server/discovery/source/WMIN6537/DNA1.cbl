       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.             DNA1.
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       01  WS-COPYRIGHT PIC X(57) VALUE 'Hello'.
       01  DEP-RLSELIT.                                                 
       01  MIC-RLSELIT.                                                 
       01  SRW105-AREAS.                                                
       01  WS-RGNMIM3010AREAS.                                          
       01  MIMST-RECORD.                                                
       01  MIWPRI-AREAS.                                                
       01  DPWPRI-AREAS.                                                
       01  MI3010-RECORD.                                               
       01  MIWMSTA-AREAS.                                               
       01  WS-ABEND.                                                    
       01  WS-WORKAREAS.                                                
           03  WS-SUB1                 PIC S9(04)      COMP             
                                                       VALUE ZEROS.     
           03  WS-FUNCTION             PIC X(01).                       
           03  WS-MIMKEYHOLD.                                           
               05  WS-HOLDAPPL         PIC 9(02).                       
               05  WS-HOLDPLAN         PIC X(10).                       
      ******************************************************************
       LINKAGE SECTION.                                                 
       01  LS-RGNMIM3010AREAS.                                          
      ******************************************************************
       PROCEDURE DIVISION                                               
               USING                                                    
               LS-RGNMIM3010AREAS.                                      
      ******************************************************************
       MAIN-LOGIC SECTION.                                              
       ML-START.                                                        
           MOVE ZEROS TO WS-HOLDAPPL.                                   
           MOVE SPACES TO WS-HOLDPLAN.                                  
           MOVE SPACE TO LS-RGNMIM3010ERROR.                            
           IF LS-RGNMIM3010FUNC IS EQUAL TO 'B'                         
               PERFORM MIM-BUILD3010.                                   
           IF LS-RGNMIM3010ERROR IS NOT EQUAL TO 'Y'                    
               MOVE WS-RGNMIM3010DATA TO LS-RGNMIM3010DATA.             
           GOBACK.                                                      
       ML-EXIT.                                                         
           EXIT.                                                        
      ******************************************************************
       MIM-BUILD3010 SECTION.                                           
       MB-START.                                                        
           MOVE LOW-VALUES TO WS-RGNMIM3010DATA.                        
           MOVE ZEROS TO WS-SUB1.                                       
           MOVE LS-RGNMIM3010INST TO MIM-3010KINST.                     
           MOVE +3010 TO MIM-3010KREC.                                  
           MOVE LS-RGNMIM3010NBR TO MIM-3010KREGION.                    
           MOVE LS-RGNMIM3010APPL TO MIM-3010KAPPL.                     
           MOVE SPACES TO MIM-3010KCHRGPLAN.                            
           MOVE ZEROS TO MIM-3010KEFFDT.                                
           MOVE MIC-DPS-3010-PRI TO MIC-MST-REC-PRI.                    
           MOVE MI3010-RECORD TO MIMST-RECORD.                          
           PERFORM MIC-MST-GET-EQUAL-REQUEST.                           
           GO TO MB-MIM3010LOOP1.                                       
       MB-MIM3010LOOP.                                                  
           PERFORM MIC-MST-GET-NEXT-REQUEST.                            
       MB-MIM3010LOOP1.                                                 
           IF WS-FUNCTION IS EQUAL TO 'E'                               
               GO TO MB-EXIT.                                           
           IF WS-FUNCTION IS NOT EQUAL TO SPACES                        
               GO TO MB-ERROR.                                          
           MOVE MIMST-RECORD TO MI3010-RECORD.                          
           IF MIM-3010KINST IS NOT EQUAL TO LS-RGNMIM3010INST           
               OR MIM-3010KREGION IS NOT EQUAL TO LS-RGNMIM3010NBR      
               OR MIM-3010KAPPL IS NOT EQUAL TO LS-RGNMIM3010APPL       
               GO TO MB-EXIT.                                           
           IF MIM-3010KEFFDT IS GREATER THAN LS-RGNMIM3010EFFDT         
               OR (MIM-3010KAPPL IS EQUAL TO WS-HOLDAPPL                
               AND MIM-3010KCHRGPLAN IS EQUAL TO WS-HOLDPLAN)           
               GO TO MB-MIM3010LOOP.                                    
           MOVE MIM-3010KAPPL TO WS-HOLDAPPL.                           
           MOVE MIM-3010KCHRGPLAN TO WS-HOLDPLAN.                       
           ADD +1 TO WS-SUB1.                                           
           IF WS-SUB1 IS GREATER THAN WS-RGNMIM3010MAXSIZE              
               GO TO MB-ERROR.                                          
           MOVE MIM-3010DATA TO WS-RGNMIM3010 (WS-SUB1).                
           MOVE MIM-3010KCHRGPLAN TO WS-RGNMIM3010K (WS-SUB1).          
           GO TO MB-MIM3010LOOP.                                        
       MB-ERROR.                                                        
           MOVE 'Y' TO LS-RGNMIM3010ERROR.                              
       MB-EXIT.                                                         
           EXIT.                                                        
      ******************************************************************
       MIC-MST-API SECTION.                                             
       COPY MIPMSTA.                                                    
      ******************************************************************
