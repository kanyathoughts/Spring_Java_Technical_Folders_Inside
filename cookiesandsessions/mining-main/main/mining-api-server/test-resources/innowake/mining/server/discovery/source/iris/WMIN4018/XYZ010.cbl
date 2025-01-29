       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.  XYZ010.                                                     
       AUTHOR.         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
                                                                                
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  FILLER                            PIC X(20)                          
                                             VALUE 'ACCUMULATORS START'.        
       01  WSA-ACCUMULATORS.                                                    
           05  WSA-PROGRAM-ACCUMULATORS.                                        
       01  FILLER                            PIC X(20)                          
                                             VALUE 'CONSTANTS START'.           
       01  WSC-CONSTANTS.                                                       
           05  WSC-ZERO                      PIC X(01) VALUE '0'.               
           05  WSC-NINE                      PIC X(01) VALUE '9'.               
           05  WSC-TXT-00                    PIC X(02) VALUE '00'.              
           05  WSC-TXT-248                   PIC X(03) VALUE '248'.             
                        
                                                                                 
       PROCEDURE DIVISION.                                                      
                                                                                
       IMS-LINKAGE SECTION.                                                     
           ENTRY 'DLITCBL' USING IO-PCB                                                                        
                                 WQPC67G0-PCB.                                  
                                                                                
      *************************************************************** **        
      **                                                              **        
      **   00000-MAIN-CONTROL                                         **        
      **                                                              **        
      *************************************************************** **        
                                                                                
       00000-MAIN-CONTROL.                                                      
                                                                                
                                                           
            EXIT.                                                               
                                                                                
