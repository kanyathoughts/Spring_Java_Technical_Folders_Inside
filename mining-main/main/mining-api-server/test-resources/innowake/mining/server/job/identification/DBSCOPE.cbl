******** A                                                
        IDENTIFICATION DIVISION.                           
        PROGRAM-ID. A.                                     
        ENVIRONMENT DIVISION.                              
        CONFIGURATION SECTION.                             
        DATA DIVISION.                                     
        WORKING-STORAGE SECTION.                           
        01  DCL.                                           
            10 COORDINATOR-TYPE     PIC X(1).              
            10 ANNUAL-DELIV-FEE     PIC S9(4) USAGE COMP.  
            10 FREE-DELIVERY-IND    PIC X(1).              
         01  W-SUBJECT-ID           PIC S9(9) COMP.        
        LINKAGE SECTION.                                   
        PROCEDURE DIVISION.                                
        0000-INITIALIZE.                                   
            EXEC SQL                                       
                SELECT TABLE1.COORDINATOR_TYPE,            
                   TABLE1.ANNUAL_DELIV_FEE,                
                   TABLE1.FREE_DELIVERY_IND                
                   INTO :DCL.COORDINATOR-TYPE,             
                        :DCL.ANNUAL-DELIV-FEE,             
                        :DCL.FREE-DELIVERY-IND             
                   FROM TABLE1                             
                   WHERE TABLE1.SUBJECT_ID = :W-SUBJECT-ID 
            END-EXEC.                                      
                                                           
 