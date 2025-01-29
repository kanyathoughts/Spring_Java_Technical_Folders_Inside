000100 IDENTIFICATION DIVISION.                                         00010000
000300 PROGRAM-ID. ABCD   .                                             00030002
004300 ENVIRONMENT DIVISION.                                            00430000
004400 CONFIGURATION SECTION.                                           00440000
004500 SOURCE-COMPUTER. IBM-370.                                        00450000
004600 OBJECT-COMPUTER.  IBM-370.                                       00460000
004700*                                                                 00470000
004800 INPUT-OUTPUT SECTION.                                            00480000
004900 FILE-CONTROL.                                                    00490000
005000     SELECT POLMBK                                                00500000
005100         ASSIGN TO UT-S-POLMBK.                                   00510000
005200     SELECT POLMBKO                                               00520000
005300         ASSIGN TO UT-S-POLMBKO.                                  00530000
005400     SELECT DATEFILE                                              00540000
005500         ASSIGN TO DATEFILE.                                      00550000
005600     EJECT.                                                       00560000
005700*                                                                 00570000
005800 DATA DIVISION.                                                   00580000
005900 FILE SECTION.                                                    00590000
006000 FD  POLMBK                                                       00600000
006100     RECORDING MODE IS F                                          00610000
006200     BLOCK CONTAINS 0 RECORDS.                                    00620000
006500 01  POLMBK-REC-IN.                                               00650000
006600     02  FILLER                  PIC X(273).                      00660000
006800*                                                                 00680000
006900 FD  POLMBKO                                                      00690000
007000     RECORDING MODE IS F                                          00700000
007100     BLOCK CONTAINS 0 RECORDS.                                    00710000
007400 01  POLMBK-REC-OUT.                                              00740000
007500     02  FILLER                  PIC X(274).                      00750000
007600*                                                                 00760000
007700 FD  DATEFILE                                                     00770000
007800     RECORDING MODE IS F.                                         00780000
008200 01  DATEFILE-REC.                                                00820000
008300     02  FILLER                  PIC X(80).                       00830000
008400*                                                                 00840000
008500 WORKING-STORAGE SECTION.                                         00850000
011000*                                                                 01100000
011100 PROCEDURE DIVISION.                                              01110000
011200 PRG-BGN.                                                         01120000
011300     OPEN INPUT DATEFILE.                                           01130000
011300     OPEN INPUT POLMBK.                                           01130000
011300     OPEN INPUT POLMBKO.                                           01130000
011400     OPEN OUTPUT FILEOUT                                          01140000
011500                 FILECHK.                                         01150000
012400 BEGIN.                                                           01240000
012500 BGN.                                                             01250000
014700     GOBACK.                                                      01470000
 