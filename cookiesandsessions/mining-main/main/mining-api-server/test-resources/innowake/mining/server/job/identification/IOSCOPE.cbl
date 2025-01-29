******** A                                       
        IDENTIFICATION DIVISION.                  
        PROGRAM-ID. A.                            
        ENVIRONMENT DIVISION.                     
        CONFIGURATION SECTION.                    
        INPUT-OUTPUT SECTION.                     
        FILE-CONTROL.                             
            SELECT KSDSFILE ASSIGN TO  \"KSD\"    
                ORGANIZATION  IS  INDEXED         
                ACCESS  MODE  IS  DYNAMIC         
                RECORD  KEY   IS  PKEY            
                FILE STATUS   IS  WS-FS.          
        DATA DIVISION.                            
        FILE SECTION.                             
        FD KSDSFILE RECORD CONTAINS 80 CHARACTERS.
        01  KSDSFILE-REC.                         
            05  PKEY                PIC X(6).     
            05  INFO                PIC X(74).    
        WORKING-STORAGE SECTION.                  
        01  MY-PROGRAM-NAME PIC X(10) VALUE 'A'.  
        LINKAGE SECTION.                          
        PROCEDURE DIVISION.                       
                                                  
                                                  
