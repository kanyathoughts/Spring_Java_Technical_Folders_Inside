       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. QCU018.                                                      
       AUTHOR.     D. ELLERS.                                                   
       DATE-WRITTEN. NOVEMBER 1987.                                             
       DATE-COMPILED.                                                           
                                                                                
      ***************************************************************           
      **                                                                        
      **  THIS SUBROUTINE PREPARES TO INITIATE THE ONLINE FATAL ERROR           
      **  PROGRAM (QCU010), MOVING DATA INTO THE XFER AREA FROM INFO            
      **  PASSED FROM TELON MODULE 'ADMAABT'.                                   
      **                                                                        
      ***************************************************************           
      **  REMARKS                                                               
      **                                                                        
      ** 05/27/2008 * HOLSINGER * WR_7884: RECOMPILE FOR E-COBOL                
      ***************************************************************           
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       DATA DIVISION.                                                           
           EJECT                                                                
       WORKING-STORAGE SECTION.                                                 
           SKIP3                                                                
       01  FILLER                                PIC X(43) VALUE                
           '*****  QCU018 WORKING STORAGE BEGINS  *****'.                       
                                                                                
       01  WSC-CONSTANTS.                                                       
           05  WSC-ONE                           PIC X(01) VALUE '1'.           
           05  WSC-TWO                           PIC X(01) VALUE '2'.           
           05  WSC-THREE                         PIC X(01) VALUE '3'.           
                                                                                
           05  WSC-ABEND-CODE-1        PIC S9(04) COMP VALUE +1018.             
                                                                                
           05  WSC-PURG-FUNCTION                 PIC X(04) VALUE 'PURG'.        
           05  WSC-CHNG-FUNCTION                 PIC X(04) VALUE 'CHNG'.        
           05  WSC-ISRT-FUNCTION                 PIC X(04) VALUE 'ISRT'.        
                                                                                
           05  WSC-CBLTDLI                       PIC X(08)                      
                                                       VALUE 'CBLTDLI '.        
           05  WSC-CEE3ABD                       PIC X(08)                      
                                                       VALUE 'CEE3ABD '.        
           05  WSC-TRANSACTION-ID                PIC X(08)                      
                                                       VALUE 'CUERR   '.        
                                                                                
           05  WSC-THREE-ZERO-FIVE               PIC 9(04) VALUE 0305.          
           05  FILLER  PIC X(17)  VALUE 'WQPCALT1 BEFORE: '.                    
           05  WSH-WQPCALT1-BEFORE            PIC X(20) VALUE SPACES.           
           05  FILLER  PIC X(16)  VALUE 'WQPCALT1 AFTER: '.                     
           05  WSH-WQPCALT1-AFTER             PIC X(20) VALUE SPACES.           
           SKIP3                                                                
       01  FILLER                                PIC X(41) VALUE                
           '*****  QCU018 WORKING STORAGE ENDS  *****'.                         
           EJECT                                                                
                                                                                
      **  2 DUMMY PARMS ARE HERE IN LINKAGE FOR COMPATIBILITY                   
      **  WITH CALLING PROGRAM "ADMAABT"  -  SAME PARMS THAT                    
      **  TELON USES IN "Z-980-ABNORMAL-TERM" TO CALL ADMAABT                   
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
           COPY ADDCABTA.                                                       
                                                                                
       01  IO-PCB.                                                              
           03  IO-PCB-LTERM    PIC X(8).                                        
           03  FILLER          PIC X(32).                                       
                                                                                
           COPY WQPCALT1.                                                       
                                                                                
       01  DUMMY-SECOND-ABT-AREA.                                               
           03  FILLER                PIC X(01).                                 
                                                                                
      **  THE FOLLOWING SPA DEFINITION _MUST_ EXACTLY MATCH THAT OF             
      **  FATAL ERROR PROGRAM QCU010.  CHANGES TO THAT PROGRAM'S SPA            
      **  AREA WHICH MODIFY THE SPA LENGTH  MUST BE REPLICATED HERE.            
                                                                                
       01  SPA-AREA.                                                            
      *    03  FILLER                PIC X(06).                                 
      *    03  SPA-TRANSACTION-CODE  PIC X(08).                                 
           COPY QS00XFER.                                                       
           COPY QS00XFHP.                                                       
           03  FILLER          PIC X(857).                                      
           COPY QS00XFFE.                                                       
                                                                                
       01  TP-OUTPUT-BUFFER.                                                    
           03  FILLER                PIC X(01).                                 
                                                                                
       01  TPO-ERRMSG1         PIC X(79).                                       
           EJECT                                                                
       PROCEDURE DIVISION USING ABNORMAL-TERMINATION-AREA                       
                                IO-PCB                                          
                                WQPCALT1-PCB                                    
                                DUMMY-SECOND-ABT-AREA                           
                                SPA-AREA                                        
                                TP-OUTPUT-BUFFER                                
                                TPO-ERRMSG1.                                    
                                                                                
           MOVE WQPCALT1-PCB  TO WSH-WQPCALT1-BEFORE.                           
           CALL WSC-CBLTDLI USING WSC-PURG-FUNCTION                             
                                  WQPCALT1-PCB.                                 
                                                                                
           IF WQPCALT1-STAT-CODE  = SPACES OR 'AZ'                              
               NEXT SENTENCE                                                    
           ELSE                                                                 
               MOVE WQPCALT1-PCB  TO WSH-WQPCALT1-AFTER                         
               CALL WSC-CEE3ABD USING WSC-ABEND-CODE-1.                         
                                                                                
      **  MOVE THE IMPORTANT SPA, ERRMSG & IO-PCB FIELDS TO THE                 
      **  QS00XFER SPA AREA, FOR EVENTUAL USE BY CUERR.                         
                                                                                
           MOVE SPACES   TO XFER00-FATAL-ERROR-SECTION.                         
           MOVE ABT-PGM-NAME          TO XFER00-ABT-PGM-NAME.                   
           MOVE ABT-PGM-TRAN-CODE     TO XFER00-ABT-PGM-TRAN-CODE.              
           MOVE ABT-DLI-STATUS        TO XFER00-ABT-DLI-STATUS.                 
           MOVE ABT-DA-FUNC-DLI       TO XFER00-ABT-DA-FUNC-DLI.                
           MOVE ABT-DA-FUNC-PCB-TYPE  TO XFER00-ABT-DA-FUNC-PCB-TYPE.           
           MOVE ABT-DA-ACCESS-NAME    TO XFER00-ABT-DA-ACCESS-NAME.             
           MOVE ABT-ERROR-ABEND-CODE  TO XFER00-ABT-ERROR-ABEND-CODE.           
           MOVE ABT-ERROR-SECTION     TO XFER00-ABT-ERROR-SECTION.              
      *                                                                         
           MOVE TPO-ERRMSG1           TO XFER00-TPO-ERRMSG.                     
      *                                                                         
           MOVE IO-PCB                TO XFER00-IO-PCB.                         
                                                                                
       PROGRAM-EXIT.                                                            
                                                                                
           GOBACK.                                                              
                                                                                
                                                                               
