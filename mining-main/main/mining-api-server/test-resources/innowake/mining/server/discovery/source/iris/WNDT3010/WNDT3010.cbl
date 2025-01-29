00001 ******************************************************************06/25/13
00002  IDENTIFICATION DIVISION.                                         P442288
00003 ******************************************************************   LV024
00004                                                                   P442288
00005  PROGRAM-ID.    P442288.                                          P442288
00006  AUTHOR.        ROY MILLS AND WALLY LUKASZUK.                     P442288
00007  DATE-WRITTEN.  AUGUST 2006.                                      P442288
00008  INSTALLATION.  ( TXDOT D-19 ).                                   P442288
00520 ***************************************************************** P442288
00552      EJECT                                                        P442288
00553  DATA DIVISION.                                                   P442288
00554 ***********************                                           P442288
00555                                                                   P442288
00616  WORKING-STORAGE SECTION.                                         P442288
00951      05  WS-RTS-DOCNO.                                            P442288
00955      05  WS-RTS-DOCNO-NUM REDEFINES WS-RTS-DOCNO                  P442288
00956                                    PIC  9(17).                    P442288
01219                                                                   P442288
01220  01  WS-SCRT                       PIC X(08)  VALUE '........'.   P442288
01227       04  WS-INQUIRY-ADABAS-DBID   PIC 9(3) COMP VALUE   0.       P442288
01229                                                                   P442288
01271                                                                   P442288
01272      EXEC ADABAS                                                  P442288
01273           FIND                                                    P442288
01274           DECLARE ARDR CURSOR FOR                                 P442288
01275           SELECT DOCNO,                                           P442288
01300                 REGPLTOWNRNAME                                    P442288
01301              FROM RTS-REGIS      RTS-ARDR                         P442288
01302                 WHERE DOCNO   = :WS-RTS-DOCNO                     P442288
01303              OPTIONS PASSWORD = :WS-SCRT                          P442288
01304                 PREFIX        = :RTS-ARDR-                        P442288
01305                 DBID          = :WS-INQUIRY-ADABAS-DBID           P442288
01306      END-EXEC.                                                    P442288
08268                                                                   P442288
