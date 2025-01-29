       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.  SIMPCOND.                                           
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.                                              
                                                                        
       MAIN0100-CONTROL.                                                
                                                                        
           IF WS-NO-MORE-RECORDS = 'Y'                                  
              CONTINUE                                                  
           ELSE                                                         
              PERFORM PROC0100-PROCESS THRU PROC0100-EXIT               
                   UNTIL WS-NO-MORE-RECORDS = 'Y'                    
           END-IF
                                                                        
       MAIN0100-EXIT.                                                   
           GOBACK.                                                      
                                                                        