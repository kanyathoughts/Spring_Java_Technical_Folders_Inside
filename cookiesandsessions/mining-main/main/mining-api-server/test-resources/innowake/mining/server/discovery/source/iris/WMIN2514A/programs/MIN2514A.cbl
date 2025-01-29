       IDENTIFICATION DIVISION.  
       PROGRAM-ID.     PROG1.
       ENVIRONMENT DIVISION.     
       CONFIGURATION SECTION.    
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
                                 
       INPUT-OUTPUT SECTION.     
       FILE-CONTROL.             
       DATA DIVISION.            
       WORKING-STORAGE SECTION.  
           REPLACE ==DB_TABLE== BY ==PRG1_TABLE==.
       ++INCLUDE SQLSTM1         
           REPLACE OFF.          
           REPLACE ==PRGX== BY =='Callee1'==.
       ++INCLUDE CALLSTM1        
           REPLACE OFF.          
           
       END PROGRAM PROG1.