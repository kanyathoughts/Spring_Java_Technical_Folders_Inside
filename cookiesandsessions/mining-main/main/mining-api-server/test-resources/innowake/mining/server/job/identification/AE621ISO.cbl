000010 IDENTIFICATION DIVISION.                                         00001000
000020 PROGRAM-ID. AE621.                                               00002000
002000*---------------------------------------------------------------* 00200000
002010 ENVIRONMENT DIVISION.                                            00201000
002020 CONFIGURATION SECTION.                                           00202000
002030*SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.                      00203000
002040 SOURCE-COMPUTER.   IBM.                                          00204000
002050 OBJECT-COMPUTER.   IBM.                                          00205000
002620 DATA DIVISION.                                                   00262000
004410 WORKING-STORAGE SECTION.                                         00441000
013040 01  WS-CLOB-COLUMN USAGE IS SQL TYPE IS CLOB(80000).             01304000
468990     EXEC SQL                                                     46899000
469000       SELECT GRS_DATA                                            46900000
469010         INTO :WS-CLOB-COLUMN                                     46901000
469020         FROM A_AEGRS                                             46902000
469030        WHERE GROUP_ID       = :AEGRS-GROUP-ID                    46903000
469040          AND CATEGORY_ID    = :AEGRS-CATEGORY-ID                 46904000
469050          AND BENEFIT_ID     = :AEGRS-BENEFIT-ID                  46905000
469060     END-EXEC                                                     46906000
