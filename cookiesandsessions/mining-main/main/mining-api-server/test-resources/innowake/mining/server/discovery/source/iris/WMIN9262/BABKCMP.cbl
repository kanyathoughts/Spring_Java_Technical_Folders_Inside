       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID. 'BABKCMP'.                                           00000200
      ******************************************************************00000300
       ENVIRONMENT DIVISION.                                            00002100
       CONFIGURATION SECTION.                                           00002200
       PROCEDURE DIVISION.                                              00039200
           SKIP2                                                        00039300
       A-100-BODY.                                                      00039400
661010     CALL 'BABKREU1'.                                              00039401
           CALL 'BABKREU2'.
           GOBACK.                                                      00040100
           EJECT                                                        00132000
       IDENTIFICATION DIVISION.                                         00132100
       PROGRAM-ID. 'BABKREU1'.                                           00132200
       DATA DIVISION.                                                   00132300
       PROCEDURE DIVISION.                                              00134500
       P-125-LOOKUP.                                                    00143300
WFIX       SKIP2                                                        00143400
       IDENTIFICATION DIVISION.                                         00132100
       PROGRAM-ID. "BABKREU2".                                           00132200
       DATA DIVISION.                                                   00132300
       PROCEDURE DIVISION.                                              00134500
       P-125-LOOKUP.                                                    00143300
WFIX       SKIP2                                                        00143400
