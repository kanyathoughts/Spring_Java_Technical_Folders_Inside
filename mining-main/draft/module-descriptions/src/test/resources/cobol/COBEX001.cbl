       ID DIVISION.
       PROGRAM-ID.   COBEX001.
      *
      ******************************************************************
      *  BLS-LAUS DISQUALIFICATION REPORT PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.
      
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
      
       01  SUB1                        PIC 9(3)    VALUE ZERO  COMP-3.  
      
       01  WS-HEAD-LINE-4.
           05  FILLER              PIC X(30)  VALUE '  FIPS    COUNTY'.
           05  FILLER              PIC X(102) VALUE SPACES.
      *****05**FILLER**************PIC*X(18)**VALUE*'CODE*10-19'.*******
      *****05**FILLER**************PIC*X(21)**VALUE*'CODE*20-28'.*******
      *****05**FILLER**************PIC*X(18)**VALUE*'CODE*29'.**********
      *****05**FILLER**************PIC*X(16)**VALUE*'CODE*06'.**********
      *****05**FILLER**************PIC*X(20)**VALUE*'06,*10-29'.********
      *****05**FILLER**************PIC*X(09)**VALUE*'CODE*30'.**********
      
       01  WS-HEAD-LINE-5.
           05  FILLER              PIC X(26)  VALUE '  CODE    NAME'.
           05  FILLER              PIC X(22)  VALUE 'VOLUNTARY QUIT'.
           05  FILLER              PIC X(12)  VALUE 'MISCONDUCT'.
           05  FILLER              PIC X(27)  VALUE 'GROSS MISCONDUCT'.
           05  FILLER              PIC X(20)  VALUE 'OTHER  '.
           05  FILLER              PIC X(10)  VALUE 'TOTAL'.
           05  FILLER              PIC X(15)  VALUE 'LABOR DISPUTE'.
      
       LINKAGE SECTION.
      
       01  RET-CODE                    PIC 9(1).
      
       COPY BPICDR                                                 
      
       01  LK-CONTROL-TOTALS.
           05  LK-MODULE-ID            PIC X(8).
           05  LK-CONTROL-TOTAL        OCCURS 9 TIMES
                                       PIC 9(9)    COMP-3.
           EJECT
       PROCEDURE DIVISION USING RET-CODE
                                BPS-COMMON-RECORD
                                LK-CONTROL-TOTALS.
      
       MAIN0100.
           IF  RET-CODE = 9
               MOVE ZERO TO RET-CODE
               PERFORM PROC0090-REPORT THRU PROC0090-EXIT
                   VARYING DISQ FROM 1 BY 1
                   UNTIL DISQ GREATER THAN 2
               MOVE CONTROL-TOTALS TO LK-CONTROL-TOTALS
               MOVE 'BPS50240' TO LK-MODULE-ID
               MOVE 9 TO RET-CODE
               GO TO MAIN0100-GOBACK.
                                                                        
           IF FIRST-TIME                                                
              MOVE 'N'               TO WS-FIRST-TIME                   
              MOVE ZERO TO WS-PAGE-CTR                                  
                           WS-LAST-DISQUAL-IND                          
              PERFORM INIT0100-CONTROL THRU INIT0100-EXIT 9 TIMES       
              PERFORM INIT0200-EDIT THRU INIT0200-EXIT 6 TIMES          
              PERFORM INIT0300-REPORT THRU INIT0300-EXIT 2 TIMES        
              PERFORM INIT0400-DATE THRU INIT0400-EXIT                  
              GO TO MAIN0100-GOBACK.                                    
                                                                        
           ADD 1 TO CONTROL-TOTAL (1)
                    CONTROL-TOTAL (3).
           IF SC-SUMMARY
               ADD 1 TO CONTROL-TOTAL (7)
               MOVE SR-COUNT
                    TO COL-TBL (SC-CLAIM-CENTER SR-LINE SR-COLUMN)
           ELSE
              IF SC-DETAIL
                 ADD 1 TO CONTROL-TOTAL (5)
                 PERFORM PROC0125-INA-DETAIL THRU PROC0125-EXIT.
      
