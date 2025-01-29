       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. UTPSPA66.                                                        
                                                                                
       ENVIRONMENT DIVISION.                                                    
                                                                                
       DATA DIVISION.                                                           
                                                                                
      *-------------- WORKING STORAGE SECTION BEGINS ------------------*        
                                                                                
       WORKING-STORAGE SECTION.                                                 
                      
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
      *                                                                         
       3222-FETCH-STATUS-DESC-PARA-X.                                           
           EXIT.                                                                
      *                                                                                                                                  
