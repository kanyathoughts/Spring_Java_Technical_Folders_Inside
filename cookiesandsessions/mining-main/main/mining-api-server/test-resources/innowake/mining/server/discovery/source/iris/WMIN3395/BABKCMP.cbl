       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID. 'BABKCMP'.                                           00000200
      ******************************************************************00000300
       ENVIRONMENT DIVISION.                                            00002100
       CONFIGURATION SECTION.                                           00002200
       INPUT-OUTPUT SECTION.                                            00002300
           SKIP2                                                        00002400
       FILE-CONTROL.                                                    00002500
       DATA DIVISION.                                                   00003000
           SKIP2                                                        00003100
       FILE SECTION.                                                    00003200
           SKIP1                                                        00003300
       WORKING-STORAGE SECTION.                                         00003900
       PROCEDURE DIVISION.                                              00039200
           SKIP2                                                        00039300
       A-100-BODY.                                                      00039400
661010     CALL 'BABKREU'.                                              00039401
           GOBACK.                                                      00040100
           EJECT                                                        00132000
       IDENTIFICATION DIVISION.                                         00132100
       PROGRAM-ID. 'BABKREU'.                                           00132200
       DATA DIVISION.                                                   00132300
       WORKING-STORAGE SECTION.                                         00132400
       01  FILLER.                                                      00132500
           05  SUB-REC               PIC S9(4) BINARY VALUE ZERO.       00132600
           SKIP3                                                        00132700
       LINKAGE SECTION.                                                 00133500
           SKIP2                                                        00133600
       PROCEDURE DIVISION.                                              00134500
           IF  LITREC-OBJ-SUB-TYPE (SUB-REC) = BKFIL-YY                 00141700
               NEXT SENTENCE                                            00141800
           ELSE                                                         00141900
               PERFORM P-125-LOOKUP                                     00142000
                  VARYING SUB-REC FROM 1 BY 1                           00142100
                   UNTIL LITREC-OBJ-SUB-TYPE (SUB-REC) = BKFIL-YY       00142200
                     OR SPACES.                                         00142300
           GOBACK.                                                      00135300
       P-125-LOOKUP.                                                    00143300
WFIX       SKIP2                                                        00143400
