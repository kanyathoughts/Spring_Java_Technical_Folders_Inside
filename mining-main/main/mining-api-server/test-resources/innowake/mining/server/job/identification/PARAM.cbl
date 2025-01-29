******** A                                                       
        IDENTIFICATION DIVISION.                                  
        PROGRAM-ID. A.                                            
        ENVIRONMENT DIVISION.                                     
        CONFIGURATION SECTION.                                    
        DATA DIVISION.                                            
        WORKING-STORAGE SECTION.                                  
         01  W-SUBJECT-ID           PIC S9(9) COMP.               
        LINKAGE SECTION.                                          
        01  DFHCOMMAREA.                                          
            02  LK-FILLER.                                        
                03  LK-FILLER03                     PIC X(1)      
                   OCCURS 1 TO 25000 TIMES DEPENDING ON EIBCALEN. 
        PROCEDURE DIVISION.                                       
        0000-INITIALIZE.                                          
                                                                  
 