       IDENTIFICATION DIVISION.                                         00010003
       PROGRAM-ID. CX00540.                                             00020003
       ENVIRONMENT DIVISION.                                            00030003
       DATA DIVISION.                                                   00040003
       WORKING-STORAGE SECTION.                                         00050003
       01 WS-DATA.                                                      00280005
          05 WS-FILE-NAME     PIC  X(8) VALUE 'EZACONFG'.               00320033
          05 WS-LENGTH           PIC  9(4) COMP.                        00490024
          05 WS-RECORD-KEY.                                             00520026
              10 WS-EZAJOB-ID    PIC  X(8).                             00530019
              10 WS-RECORD-TYPE  PIC  X(1) VALUE 'L'.                   00540007
              10 FILLER          PIC  X(3).                             00550007
              10 WS-TRAN-ID.                                            00560007
                15 WS-TRAN-X1    PIC  X(2).                             00570012
                15 WS-TRAN-X2    PIC  X(2).                             00580012
      *===============================================================* 00810041
       LINKAGE SECTION.                                                 00820003
      *===============================================================* 00830038
       01 EZAKOPI-RECORD.                                               00880008
          05 RECORD-KEY.                                                00890007
              10 CJOB-ID      PIC  X(8).                                00900007
              10 RECORD-TYPE  PIC  X(1).                                00910007
              10 FILLER       PIC  X(3).                                00920007
              10 LISTENER     PIC  X(4).                                00930008
          05 FILLER           PIC  X(132).                              00940014
      *                                                                 00950008
      *===============================================================* 00960038
       PROCEDURE DIVISION.                                              00970003
      *===============================================================* 00980038
       LAES-EZAKOPI-NEXT SECTION.                                       02850026
           EXEC CICS READNEXT DATASET(WS-FILE-NAME)                     02920028
                          INTO(EZAKOPI-RECORD)                          02930026
                          LENGTH(WS-LENGTH)                             02940026
                          RIDFLD(WS-RECORD-KEY)                         02950026
                          REQID(EIBTASKN)                               02960026
           END-EXEC.                                                    02970026
      *                                                                 02980026
       X-LAES-EZAKOPI-NEXT.                                             02990029
           EXIT.                                                        03000026
      *                                                                 03010026