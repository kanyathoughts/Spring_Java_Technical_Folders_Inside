       IDENTIFICATION DIVISION.
       PROGRAM-ID. A.
       FILE-CONTROL.                                                                                                                    
       DATA DIVISION.                                                                                                                                  
       FILE SECTION.        
                  EXEC SQL                                                             
                DECLARE CUROFMR CURSOR FOR                                      
                 SELECT                                                         
                    ABC                                       
                 FROM                                                           
                       CCC                                                   
                 WHERE                                                          
                      ABC =                                                 
                      :AAA.ABC                                              
           END-EXEC.   
       WORKING-STORAGE SECTION.                                                 
          EXEC SQL                                                             
                INCLUDE SQLCPY                                                
           END-EXEC.                    
       PROCEDURE DIVISION.
       