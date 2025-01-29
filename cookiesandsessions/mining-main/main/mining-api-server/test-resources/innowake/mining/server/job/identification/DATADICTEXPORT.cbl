       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATADICTEXPORT.
       
       AUTHOR.
       DATE-WRITTEN. AUG 2023.
       DATE-COMPILED.
       ENVIRONMENT DIVISION. 
       DATA DIVISION.
       FILE SECTION. 
       
       WORKING-STORAGE SECTION.
       
       01  WS-RETURN.
           05  WS-LAST-UNIT-CODE           PIC X(05)  VALUE 'XXXXX'.
           05  FINAL-REC                   PIC X(05)  .
           05  CURR-REC                    PIC X(05)  .
           05  NON-FINAL-REC               PIC X(05)  .
           
012802***  WORK AREAS                                                   00710000
012803                                                                  00720000
012804 01  WS-WORK-FIELDS.                                              00730000
012805     05  WS-MAX-LINES            PIC 9(03)     VALUE 60.          00740000
012806     05  WS-LINE-COUNT           PIC 9(03)     VALUE 99.          00750000
012807         88  WS-REC-CNT          PIC 9(05)     VALUE ZEROES.      00760000
012808     05  WS-PAGE-NO              PIC 9(03)     VALUE ZEROES.      00770000
012809     05  WS-HOLD-SEQNUM          PIC X(03)     VALUE SPACES.      00780000
       
       PROCEDURE DIVISION. 
       
       0000-MAIN-PROCESS.
      
       IF WS-REC-CNT NOT = SPACES                                          
                MOVE WS-LAST-UNIT-CODE TO FINAL-REC                       
                SET  NON-FINAL-REC     TO TRUE                                   
       END-IF.
           
       IF NOT FINAL-REC                                               
           SET  NON-FINAL-REC   TO TRUE                              
           MOVE CURR-REC        TO WS-LAST-UNIT-CODE                                                
       ELSE                                                                 
           MOVE NON-FINAL-REC    TO FALSE                       
           MOVE CURR-REC         TO FINAL-REC 
           SET  WS-PAGE-NO       TO ZEROES 
           SET  WS-MAX-LINES     TO ZEROES              
       END-IF. 
       
       0000-EXIT. EXIT.
       
       