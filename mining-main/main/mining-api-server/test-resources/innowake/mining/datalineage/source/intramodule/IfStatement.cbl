       IDENTIFICATION DIVISION.
       PROGRAM-ID. IFSTATEMENT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01  APPL-RESULT             PIC S9(9)   COMP.                            
           88  APPL-AOK            VALUE 0.                                     
           88  APPL-EOF            VALUE 16.
         01  END-OF-FILE             PIC X(01)    VALUE 'N'.
         01  ACCTFILE-STATUS.                                                     
           05  ACCTFILE-STAT1      PIC X.                                       
           05  ACCTFILE-STAT2      PIC X.                                       
                                                                                
         01  IO-STATUS.                                                           
           05  IO-STAT1            PIC X.         

       PROCEDURE DIVISION.
          IF  APPL-AOK                                                         
               CONTINUE                                                         
          ELSE                                                                 
               IF  APPL-EOF                                                     
                   MOVE 'Y' TO END-OF-FILE                                      
               ELSE                                                             
                   DISPLAY 'ERROR READING ACCOUNT FILE'                         
                   MOVE ACCTFILE-STATUS TO IO-STATUS                                                            
               END-IF                                                           
          END-IF  

       STOP RUN.
