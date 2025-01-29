       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.  NESTIF.                                           
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.                                              
                                                                        
       MAIN0100-CONTROL.                                                
                                                                        
           IF WS-NO-MORE-RECORDS = 'Y'                                  
               IF COM-NAME-RETURN = ZERO                                
                   MOVE COM-NAM(1:1)      TO DOR-REQ-FNAME              
               ELSE                                                     
                   MOVE SPACES            TO DOR-REQ-FNAME
               END-IF             
           ELSE                                                         
              PERFORM PROC0100-PROCESS THRU PROC0100-EXIT               
                   UNTIL WS-NO-MORE-RECORDS = 'Y'.                      
                                                                        
       MAIN0100-EXIT.                                                   
           GOBACK.                                                      
                                                                        