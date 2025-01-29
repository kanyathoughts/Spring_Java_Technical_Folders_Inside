000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.                        UTPBDI01.                     00000200
000300 AUTHOR.                            TATA CONSULTANCY SERVICES     00000300
000400 DATE-WRITTEN.                      NOV 2000.                     00000400
000500 DATE-COMPILED.                                                   00000500
007000 ENVIRONMENT DIVISION.                                            00007100
007700 DATA DIVISION.                                                   00007800
008400 WORKING-STORAGE SECTION.                                         00008500
024600**********************************************************        00025400
024700 PROCEDURE DIVISION.                                              00025500
024800**********************************************************        00025600
027900     EXEC CICS ASSIGN                                             00028900
028100*    Set USER ID                                                  00029000
028100          USERID(WS-USER-ID)                                      00029100
028200     END-EXEC                                                     00029200
125300     EXIT                                                         00157300
