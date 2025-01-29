       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID.                           UTDACTVA.                  00000200
       AUTHOR.                               TATA CONSULTANCY SERVICES. 00000300
       DATE-WRITTEN.                         01/20/15.                  00000400
       DATE-COMPILED.                                                   00000500
       ENVIRONMENT DIVISION.                                            00003000
       CONFIGURATION SECTION.                                           00003100
       SOURCE-COMPUTER.                      IBM-370.                   00003200
       OBJECT-COMPUTER.                      IBM-370.                   00003300
       INPUT-OUTPUT SECTION.                                            00003400
       DATA DIVISION.                                                   00003900
       WORKING-STORAGE SECTION.                                         00004400
       LINKAGE SECTION.                                                 00031800
       PROCEDURE DIVISION USING DFHCOMMAREA .                           00032500
               EXEC CICS SEND                                           00165200
                   MAP ( 'UMACTVA'  )                                   00165300
                   MAPSET ( 'UTACTVA'  )                                00165400
                   FROM ( UMACTVAO )                                    00165500
                   ERASE                                                00165600
               END-EXEC                                                 00165700
                                                                        00213500
