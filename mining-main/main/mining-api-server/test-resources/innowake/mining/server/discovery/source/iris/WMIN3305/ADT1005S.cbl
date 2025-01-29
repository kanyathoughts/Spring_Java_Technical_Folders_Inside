000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.     ADT1005S.                                        00020000
003100 EJECT                                                            00470000
003200 ENVIRONMENT DIVISION.                                            00480000
003600                                                                  00490000
003700 INPUT-OUTPUT SECTION.                                            00500000
003800 FILE-CONTROL.                                                    00510000
004900 EJECT                                                            00560000
005000 DATA DIVISION.                                                   00570000
005100 FILE SECTION.                                                    00580000
007700     EJECT                                                        00780000
007800 WORKING-STORAGE SECTION.                                         00790000
008000     EJECT                                                        00980000
025400                                                                  03170000
026500     EJECT                                                        03420000
049400 LINKAGE SECTION.                                                 05790000
050000                                                                  05850000
050100     EJECT                                                        05860000
050200 PROCEDURE DIVISION.                                              05870000
050300     EJECT                                                        05880000
051800*    MAINLINE CONTROL                                             06030000
052060                                                                  06070000
052100     PERFORM WITH TEST BEFORE                                     06080000
052200       UNTIL END-OF-INPUT                                         06090000
052300                                                                  06100000
052400         PERFORM 0100-TRANSACTION-DRIVER   THRU 0100-EXIT         06110000
052500                                                                  06120000
052800     END-PERFORM.                                                 06150000
052900                                                                  06160000
056400     GOBACK.                                                      06800000
056500     EJECT                                                        06810000
070900 0100-RECORD-RESTART-DATA.                                        09380000
071000                                                                  09390000
      *TPC101395 - END                                                  09810000
073700                                                                  09820000
073800     EXEC SQL    COMMIT    END-EXEC.                              09830000
073900                                                                  09840000
074000 0100-EXIT.        EXIT.                                          09850000
074100     EJECT                                                        09860000
