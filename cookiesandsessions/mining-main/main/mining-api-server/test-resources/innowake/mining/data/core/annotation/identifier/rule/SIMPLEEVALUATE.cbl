       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.  SIMPCOND.                                           
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.                                              
                                                                        
       MAIN0100-CONTROL.                                                
                                                                        
           EVALUATE CARPOOL-SIZE
             WHEN 1
               MOVE "SINGLE" TO PRINT-CARPOOL-STATUS
             WHEN 2
               MOVE "COUPLE" TO PRINT-CARPOOL-STATUS
             WHEN 3 THRU 6
               MOVE "SMALL GROUP" TO PRINT-CARPOOL STATUS
             WHEN OTHER
               MOVE "BIG GROUP" TO PRINT-CARPOOL STATUS
           END-EVALUATE                                                 

       MAIN0100-EXIT.                                                   
           GOBACK.                                                      
                                                                        