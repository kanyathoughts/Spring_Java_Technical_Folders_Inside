       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID.    PBPS0013.                                         00020000
       DATE-WRITTEN.  01/22/10.                                         00050000
       DATE-COMPILED.                                                   00060000
               SKIP3                                                    00070000
       ENVIRONMENT DIVISION.                                            00260000
                                                                        00270000
       CONFIGURATION SECTION.                                           00280000
                                                                        00290000
       SOURCE-COMPUTER. IBM-370.                                        00300000
       OBJECT-COMPUTER. IBM-370.                                        00310000
                                                                        00320000
       INPUT-OUTPUT SECTION.                                            00330000
                                                                        00340000
       FILE-CONTROL.                                                    00350000
       DATA DIVISION.                                                   00400000
       FILE  SECTION.                                                   00410000
       WORKING-STORAGE SECTION.                                         00460000
                                                                        00470000
       01  PROGRAM-YCERR000             PIC X(13) VALUE '$PGM=YCERR000'.        
                                                                                
005800 01  PROGRAM-QLCVG000             PIC X(13) VALUE '$PGM=QLCVG000'.        
005900                                                                          
006200 01  PROGRAM-OMSSDATC             PIC X(13) VALUE '$PGM=OMSSDATC'.        
006300                                                                          
       01  PROGRAM-PBPSM000       PIC X(13)                             00620000
                                  VALUE '$PGM=PBPSM000'.                00630000
                                                                        00640000
                                                                                
006200 01  STATUS-CODE                  PIC X(10) VALUE SPACES.                 
006200 01  STATUS-SUMMARY               PIC X(10) VALUE SPACES.                 
                                                                        00650000
                                                                        01010000
       PROCEDURE DIVISION.                                                      
       0000-MAINLINE.                                                   01050000
                                                                        01060000
024000     MOVE 'OKAY'                 TO  STATUS-CODE                          
024100     MOVE ' '                    TO  STATUS-SUMMARY                       
018400                                                                          
           PERFORM  1000-GET-KEYWORD-VALUE                              01160000
                                                                        01180000    
           GOBACK.                                                      01190000
                                                                        01200000
       1000-GET-KEYWORD-VALUE.                                          01390000
                                                                        01430000                                                                            
023000     IF STATUS-CODE NOT = 'OKAY'                                              
                                                                        01517000
033100        PERFORM 9999-CALL-YCERR000                            
032100                                                                          
033300     END-IF                                                               
032500     .                                                                                                                                                 
       9999-CALL-YCERR000.                                                      
                                                                                                                                                              
           CALL PROGRAM-YCERR000 (6:8)                                         
           END-CALL                                                             
           .                                                                    
       IDENTIFICATION DIVISION.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01  DAWS.
         05  DAWS-CALLED-PGM   PIC X(08)
                               VALUE SPACES.
                               01  WS-PGM.
        05 WS-SUBROUTINE              PIC X(08).
           88 WS-SUBPGM01             VALUE 'ABCDEFGH'.
           88 WS-SUBPGM02             VALUE 'SUBPGMxy'.
           88 WS-SUBPGM03             VALUE 'SUBPGMxz'.
           88 WS-SUBPGM04             VALUE 'SUBPGMxa'.
        01 PROGRAM-DUMMY       PIC X(08) VALUE 'DUMMY-PROGRAM'.
       PROCEDURE DIVISION.          
           MOVE PROGRAM-DUMMY (2:4) TO DAWS-CALLED-PGM.
           CALL DAWS-CALLED-PGM USING ARCH-AREA
                         EE858-AREA.
           SET WS-SUBPGM01 (2:3) TO TRUE.
           CALL WS-SUBROUTINE.                                                               
