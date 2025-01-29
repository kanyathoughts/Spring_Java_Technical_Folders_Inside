       IDENTIFICATION DIVISION.
       PROGRAM-ID. ONENEST.
       
       AUTHOR.
       DATE-WRITTEN. AUG 2023.
       DATE-COMPILED.
       ENVIRONMENT DIVISION. 
       DATA DIVISION.
       FILE SECTION. 
       
       WORKING-STORAGE SECTION.
       
00001  01  LK414-LINKAGE.                                               07/22/10
00017      05  LK414-AGRMT-TYPE-CD                   PIC X.                CL**9                                                                      CL**9
00121      05  LK414-ERROR-SUB                 PIC 9(2).                   CL**9
00122      05  LK414-ERROR-RECORD.                                         CL**9
00123          10  LK414-ERROR-ENTRY OCCURS 50 TIMES.                      CL**9
00124              15  LK414-ERROR-SYS-CD      PIC X(02).                  CL**9
00125              15  LK414-ERROR-NBR         PIC X(06).                  CL**9
00126              15  LK414-ERROR-COLUMN-NAME PIC X(18).                  CL**9
00127              15  LK414-ERROR-LOCATION    PIC X(30).                  CL**9
00128              15  LK414-ERROR-CRITICAL-SW PIC X.                      CL**9
00129              15  FILLER                  PIC X(9).                   CL**9
00022      05  LK414-INVALID-GRP-IND                 PIC X.                CL**9
00023          88 LK414-GROUP-INVALID                VALUE 'Y'.            CL**9
00024      05  LK414-NEW-GROUP-CHECK                 PIC X.                CL**9
00025          88 LK414-NEW-GROUP                    VALUE 'Y'.            CL**9
       
       PROCEDURE DIVISION. 
       
       0000-MAIN-PROCESS.
      
      IF LK414-GROUP-INVALID AND NOT LK414-NEW-GROUP THEN         
           ADD +1 TO LK414-ERROR-SUB                              
           IF LK414-AGRMT-TYPE-CD = 'I' THEN                      
              MOVE '015W00' TO LK414-ERROR-NBR(LK414-ERROR-SUB)   
           ELSE                                                   
              MOVE '013E00' TO LK414-ERROR-NBR(LK414-ERROR-SUB)   
           END-IF                                                 
           MOVE 'SF' TO LK414-ERROR-SYS-CD(LK414-ERROR-SUB)       
           MOVE 'COV-AGRMT-ID' TO                                 
                 LK414-ERROR-COLUMN-NAME(LK414-ERROR-SUB)         
           MOVE 'PARAGRAPH 1000-EDIT-ORG-PERS' TO                 
                 LK414-ERROR-LOCATION(LK414-ERROR-SUB)            
           MOVE 'Y' TO  LK414-ERROR-CRITICAL-SW(LK414-ERROR-SUB)  
      END-IF
           
       0000-EXIT. EXIT.
       
       