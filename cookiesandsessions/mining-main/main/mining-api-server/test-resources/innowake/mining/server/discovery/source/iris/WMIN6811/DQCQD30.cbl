000100 IDENTIFICATION DIVISION.                                         00010003
000200 PROGRAM-ID.     DQCQD30.                                         00020003
003800 ENVIRONMENT DIVISION.                                            00380003
004600 DATA DIVISION.                                                   00460003
005000 WORKING-STORAGE SECTION.                                         00500003
046400           MOVE '   RETRIEVE AGAIN TO VIEW'  TO  MESS1O           04640003
046500           MOVE 'INFORMATION'                TO  MESS2O           04650003
046600           MOVE EIBTRMID TO TERMO                                 04660003
046700           EXEC CICS SEND MAP('MAP4')                             04670003
046800                          MAPSET('DQBQD30')                       04680003
046900                          ERASE                                   04690003
047000                          ACCUM                                   04700003
047100                          FREEKB                                  04710003
047200                          END-EXEC                                04720003
051900     SKIP3                                                        05190003
