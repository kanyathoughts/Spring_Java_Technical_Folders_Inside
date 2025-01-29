000010 IDENTIFICATION DIVISION.                                         00001000
000020 PROGRAM-ID. WDIS129A.                                            00002000
000320 ENVIRONMENT DIVISION.                                            00032000
000330 CONFIGURATION SECTION.                                           00033000
000340 DATA DIVISION.                                                   00034000
000360 WORKING-STORAGE SECTION.                                         00036000
004170 LINKAGE SECTION.                                                 00417000
006360 PROCEDURE DIVISION.                                              00636000
013650 FOR-READ-ONLY.                                                   01365000
013670         EXEC SQL                                                 01367000
013680           DECLARE my-cursor CURSOR                               01368000
013690           FOR SELECT                                             01369000
013700              AAAAAAAA,                                           01370000
013710              BBBBBBBB,                                           01371000
013720              CCCCCCCC,                                           01438000
013730              NXENTERPRISE                                        01438000
014390           FROM  MYTABLE                                          01439000
014400           WHERE                                                  01440000
014410              AAAAAAAA =                                          01441000
014420               'AAAAAAAA' AND                                     01442000
014430              BBBBBBBB =                                          01443000
014440               'BBBBBBBB' AND                                     01446000
014450              NXENTERPRISE =                                      01446000
014460               'NXENTERPRISE'                                     01446000
014470           ORDER BY  CCCCCCCC  ASC,                               01447000
014480              BBBBBBBB  ASC,                                      01448000
014490              AAAAAAAA  ASC,                                      01449000
014500              NXENTERPRISE DESC                                   01449000
014510           OPTIMIZE FOR 1 ROW FOR READ ONLY                       01450000
014520         END-EXEC.                                                01451000
014530         EXEC SQL OPEN my-cursor                                  01452000
014540         END-EXEC.                                                01453000