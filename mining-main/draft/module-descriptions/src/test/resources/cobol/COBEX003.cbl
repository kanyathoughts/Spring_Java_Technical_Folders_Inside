       IDENTIFICATION  DIVISION.                                                
       PROGRAM-ID. COBEX003.                                                    
       AUTHOR. TCS.                                                             
       DATE-WRITTEN. 2010-04-20.                                                
      *----------------------------------------------------------------*        
      *AUTHOR. DADA CONSULTANCY SERVICES.                              *        
      *----------------------------------------------------------------*        
      *INSTALLATION.  PRUDENTIAL.                                      *        
      *----------------------------------------------------------------*        
      *           P R O G R A M        I N F O R M A T I O N           *        
      *----------------------------------------------------------------*        
      *  ADB10-004 - L&A IMAGING WORKFLOW & CASE MANAGEMENT            *        
      *----------------------------------------------------------------*        
      * A NEW MQ MODULE SERVICE WILL UPDATE THE BROKER AGREEMENT INFO          
      * IN ADB. ONCE THE BROKER AGREEMENT IS EXECUTED, L&A CASE MGMT           
      * WILL SEND THE DATE TO ADB ABOUT THE NEWLY EXECUTED BROKER AGMT         
      * TO ADB - BROKER AGREEMENT VERSION DATE, DATE EXECUTED AND SPEC-        
      * IALIZED INDICATOR (IF APPLICABLE).  ONCE THE BROKER AGREEMENT          
      * INFORMATION IS CAPTURED, ADB WILL AUTOMATE THE CREATION OF THE         
      * BROKER AGREEMENT. IF A BROKER AGREEMENT ALREADY EXISTS, THEN IT        
      * WILL BE TERMINATED AND A NEW BROKER AGREEMENT WILL BE CREATED.         
      * IF A BROKER AGREEMENT DOES NOT EXIST, THEN A NEW BROKER AGMT           
      * WILL BE CREATED.                                                       
      *----------------------------------------------------------------*        
      *           C H A N G E   H I S T O R Y   L O G                  *        
      *--------------------------------------------------------------- *        
      * SR.NO.    DATE.          DESCRIPTION OF CHANGES MADE.                
      *----------------------------------------------------------------*        
      *         |        |                                             |        
      *--------------------------------------------------------------- *        
       ENVIRONMENT DIVISION.                                                    
      *-----------------------------------------------------------------        
                                                                                
       DATA DIVISION.                                                           
      *-----------------------------------------------------------------        
                                                                                
       WORKING-STORAGE SECTION.                                                 
      *-----------------------------------------------------------------        
      * GENERAL VARIABLES                                              *        
      *-----------------------------------------------------------------        
       01 LINKAGE-IP.                                                           
          05 ADB-INPUT-AREA.                                                    
             10 IP-SSN-NUM                 PIC X(09).                           
             10 IP-BRK-AGMT-VER-DT         PIC X(07).                           
             10 IP-EXEC-DT                 PIC X(10).                           
             10 IP-SPL-INDICATOR           PIC X(01).                           
             10 IP-PRODUCT-TYPE            PIC X(02).                           
             10 IP-PERS-FIRM-IND           PIC X(01).                           
             10 IP-FILLER                  PIC X(170).                          
      *-----------------------------------------------------------------        
      *  DECLARATIONS OF CURSORS                                       *        
      *-----------------------------------------------------------------        
      * CURSOR TO FETCH THE CURRENT OLDCO OFFICE FOR A NEWCO OFFICE             
      *-----------------------------------------------------------------        
      *-----------------------------------------------------------------        
       LINKAGE SECTION.                                                         
       01 DFHCOMMAREA.                                                          
        02 LK-UTPSPA64.                                                         
          05 ADB-INPUT-AREA.                                                    
             10 IP-SSN-NUM                 PIC X(09).                           
             10 IP-BRK-AGMT-VER-DT         PIC X(07).                           
             10 IP-EXEC-DT                 PIC X(10).                           
             10 IP-SPL-INDICATOR           PIC X(01).                           
             10 IP-PRODUCT-TYPE            PIC X(02).                           
             10 IP-PERS-FIRM-IND           PIC X(01).                           
             10 IP-FILLER                  PIC X(170).                          
          05 ADB-OUTPUT-AREA.                                                   
             10 OP-REPLY-CODE              PIC X(04).                           
             10 OP-REPLY-MSG               PIC X(80).                           
             10 OP-SSN-NUM                 PIC X(09).                           
             10 OP-FILLER                  PIC X(107).                          
                                                                                
      *-----------------------------------------------------------------        
       PROCEDURE DIVISION USING DFHCOMMAREA.                                    
      *-----------------------------------------------------------------        
                                                                                
       0000-MAINLINE-PARA.                                                      
            PERFORM 1000-HOUSE-KEEPING                                          
               THRU 1000-EXIT                                                   
            PERFORM 2000-PROCESS-PARA                                           
               THRU 2000-EXIT                                                   
            .                                                                   
