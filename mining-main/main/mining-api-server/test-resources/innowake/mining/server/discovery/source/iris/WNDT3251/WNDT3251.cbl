000010 IDENTIFICATION DIVISION.                                         00001000
000020 PROGRAM-ID. WDIS127A.                                            00002000
000320 ENVIRONMENT DIVISION.                                            00032000
000330 CONFIGURATION SECTION.                                           00033000
000340 DATA DIVISION.                                                   00034000
000360 WORKING-STORAGE SECTION.                                         00036000
002850 LINKAGE SECTION.                                                 00285000
004050 PROCEDURE DIVISION                 .                             00405000
           EXEC SQL                                                             
              VALUES IDENTITY_VAL_LOCAL()                                       
              INTO : ID912-CLNT-GR-RULE-ID                                      
           END-EXEC                            
