000100 IDENTIFICATION DIVISION.                                         00010000
000300 PROGRAM-ID. AF10H01.                                             00030002
004300 ENVIRONMENT DIVISION.                                            00430000
004400 CONFIGURATION SECTION.                                           00440000
004500 SOURCE-COMPUTER. IBM-370.                                        00450000
004600 OBJECT-COMPUTER.  IBM-370.                                       00460000
004700*                                                                 00470000
004800 INPUT-OUTPUT SECTION.                                            00480000
004900 FILE-CONTROL.                                                    00490000
005000     SELECT FILEIN                                                00500000
005100         ASSIGN TO INFILE.                                        00510000
005200     SELECT FILEOUT                                               00520000
005300         ASSIGN TO OUTFILE.                                       00530000
005400     SELECT FILECHK                                               00540000
005500         ASSIGN TO CHKFILE.                                       00550000
005600*                                                                 00560000
005700 I-O-CONTROL.                                                     00570000
005800 DATA DIVISION.                                                   00580000
005900 FILE SECTION.                                                    00590000
006000 FD  FILEIN                                                       00600000
006100     BLOCK CONTAINS 0 RECORDS                                     00610000
006200     RECORD CONTAINS 274 CHARACTERS                               00620000
006300     LABEL RECORDS ARE STANDARD                                   00630000
006400     DATA RECORD IS INREC.                                        00640000
006500 01  INREC.                                                       00650000
006600     02  FILLER                  PIC X(273).                      00660000
006700     02  RECZ                    PIC X.                           00670000
006800*                                                                 00680000
006900 FD  FILEOUT                                                      00690000
007000     BLOCK CONTAINS 0 RECORDS                                     00700000
007100     RECORD CONTAINS 274 CHARACTERS                               00710000
007200     LABEL RECORDS ARE STANDARD                                   00720000
007300     DATA RECORD IS OUTREC.                                       00730000
007400 01  OUTREC.                                                      00740000
007500     02  FILLER                  PIC X(274).                      00750000
007600*                                                                 00760000
007700 FD  FILECHK                                                      00770000
007800     BLOCK CONTAINS 0 RECORDS                                     00780000
007900     RECORD CONTAINS 80 CHARACTERS                                00790000
008000     LABEL RECORDS ARE STANDARD                                   00800000
008100     DATA RECORD IS CHKREC.                                       00810000
008200 01  CHKREC.                                                      00820000
008300     02  FILLER                  PIC X(80).                       00830000
008400*                                                                 00840000
008500 WORKING-STORAGE SECTION.                                         00850000
011000*                                                                 01100000
011100 PROCEDURE DIVISION.                                              01110000
011200 PRG-BGN.                                                         01120000
011300     OPEN INPUT FILEIN.                                           01130000
011400     OPEN OUTPUT FILEOUT                                          01140000
011500                 FILECHK.                                         01150000
012400 BEGIN.                                                           01240000
012500 BGN.                                                             01250000
014700     GOBACK.                                                      01470000
