000100*01  PA0791I-MESSAGE.                                             00000100
000200         05  E2325-LL                PIC S9(04) COMP.             00000200
000300         05  E2326-ZZ                PIC S9(04) COMP.             00000300
000400         05  E72-TRAN-CD             PIC  X(08).                  00000400
000500         05  MID-PFKEYS              PIC  X(06).                  00000500
                   88 MID-MF0616           VALUE 'PF10'.                00000600
000600     02  PA0791I-INPUT-AREA.                                      00000700
000700         05  I-ACTION                PIC   X(3).                  00000800
000800             88  I-OK-ACTION                        VALUE 'ADD'   00000900
000900                                                          'CHG'   00001000
001000                                                          'LIS'.  00001100
001100         05  I-ACCESSORY             PIC   X(2).                  00001200
001200         05  I-MDLYR                 PIC   X(2).                  00001300
001300         05  I-PHASE                 PIC   X(1).                  00001400
001400             88  I-OK-PHASE                         VALUE         00001500
001500                                                    '0' THRU '9'  00001600
001600                                                    'A' THRU 'Z'. 00001700
      *PCPS CHANGES START                                               00001800
001700*        05  I-MODELS                PIC   X(36).                 00001900
001700*        05  I-MODELS                PIC   X(156).                00002000
001700         05  I-MODELS                PIC   X(104).                00002100
001800         05  I-MODEL REDEFINES I-MODELS                           00002200
001900*                         OCCURS  9 TIMES                         00002300
001900*                         OCCURS 39 TIMES                         00002400
001900                          OCCURS 26 TIMES                         00002500
      *PCPS CHANGES END                                                 00002600
002000                              INDEXED BY I-MODEL-INDX             00002700
002100                                     PIC   X(04).                 00002800
002200                                                                  00002900
               05  I-SERVCO                PIC   X(13).                 00003000
002300         05  I-UPDATE-ALL-SW         PIC   X(01).                 00003100
002400         05  I-MODL1-PROCESSED-SW    PIC   X(01).                 00003200
002500         05  I-MODL2-PROCESSED-SW    PIC   X(01).                 00003300
002600         05  I-MODL3-PROCESSED-SW    PIC   X(01).                 00003400
002700         05  I-MODL4-PROCESSED-SW    PIC   X(01).                 00003500
002800         05  I-MODL5-PROCESSED-SW    PIC   X(01).                 00003600
002900         05  I-MODL6-PROCESSED-SW    PIC   X(01).                 00003700
003000         05  I-MODL7-PROCESSED-SW    PIC   X(01).                 00003800
003100         05  I-MODL8-PROCESSED-SW    PIC   X(01).                 00003900
003200         05  I-MODL9-PROCESSED-SW    PIC   X(01).                 00004000
      *PCPS CHANGES START                                               00004100
003200         05  I-MODL10-PROCESSED-SW   PIC   X(01).                 00004200
003200         05  I-MODL11-PROCESSED-SW   PIC   X(01).                 00004300
003200         05  I-MODL12-PROCESSED-SW   PIC   X(01).                 00004400
003200         05  I-MODL13-PROCESSED-SW   PIC   X(01).                 00004500
003200         05  I-MODL14-PROCESSED-SW   PIC   X(01).                 00004600
003200         05  I-MODL15-PROCESSED-SW   PIC   X(01).                 00004700
003200         05  I-MODL16-PROCESSED-SW   PIC   X(01).                 00004800
003200         05  I-MODL17-PROCESSED-SW   PIC   X(01).                 00004900
003200         05  I-MODL18-PROCESSED-SW   PIC   X(01).                 00005000
003200         05  I-MODL19-PROCESSED-SW   PIC   X(01).                 00005100
003200         05  I-MODL20-PROCESSED-SW   PIC   X(01).                 00005200
003200         05  I-MODL21-PROCESSED-SW   PIC   X(01).                 00005300
003200         05  I-MODL22-PROCESSED-SW   PIC   X(01).                 00005400
003200         05  I-MODL23-PROCESSED-SW   PIC   X(01).                 00005500
003200         05  I-MODL24-PROCESSED-SW   PIC   X(01).                 00005600
003200         05  I-MODL25-PROCESSED-SW   PIC   X(01).                 00005700
003200         05  I-MODL26-PROCESSED-SW   PIC   X(01).                 00005800
003200*        05  I-MODL27-PROCESSED-SW   PIC   X(01).                 00005900
003200*        05  I-MODL28-PROCESSED-SW   PIC   X(01).                 00006000
003200*        05  I-MODL29-PROCESSED-SW   PIC   X(01).                 00006100
003200*        05  I-MODL30-PROCESSED-SW   PIC   X(01).                 00006200
003200*        05  I-MODL31-PROCESSED-SW   PIC   X(01).                 00006300
003200*        05  I-MODL32-PROCESSED-SW   PIC   X(01).                 00006400
003200*        05  I-MODL33-PROCESSED-SW   PIC   X(01).                 00006500
003200*        05  I-MODL34-PROCESSED-SW   PIC   X(01).                 00006600
003200*        05  I-MODL35-PROCESSED-SW   PIC   X(01).                 00006700
003200*        05  I-MODL36-PROCESSED-SW   PIC   X(01).                 00006800
003200*        05  I-MODL37-PROCESSED-SW   PIC   X(01).                 00006900
003200*        05  I-MODL38-PROCESSED-SW   PIC   X(01).                 00007000
003200*        05  I-MODL39-PROCESSED-SW   PIC   X(01).                 00007100
      *PCPS CHANGES END                                                 00007200
003300         05  I-SELECTED-MODEL        PIC   X(04).                 00007300
003400                                                                  00007400
003500         05  I-PART-MAINTENANCE OCCURS 8  TIMES INDEXED BY        00007500
003600                                     I-PART-INDX.                 00007600
003700             10  I-HIDDEN-BLD-TXROOT PIC   X(01).                 00007700
003800             10  I-SEL-ACTION        PIC   X(03).                 00007800
003900             10  I-REG-PIO-CODE      PIC   X(01).                 00007900
004000             10  I-E1-PART-NO.                                    00008000
004100                 15  I-PART-NO-5     PIC   X(5).                  00008100
004200                 15  I-PART-NO-10    PIC   X(5).                  00008200
004300                 15  I-PART-NO-15    PIC   X(5).                  00008300
004400             10  I-V5598-PART-CMT    PIC   X(3).                  00008400
004500             10  I-V5599-PART-CLR    PIC   9(2).                  00008500
004600             10  I-V5600-COMP-TYPE   PIC   X(1).                  00008600
004700* THE BELOW FIELDS WERE ADDED 12-21-83 TO ALLOW FOR PROCESSING    00008700
004800* REGULAR MAJOR COMPONENT PARTS.                                  00008800
004900         05  I-AREA-FOR-MAJOR-COMP-REG.                           00008900
005000             10  I-MAJ-SEL-ACTION        PIC   X(03).             00009000
005010             10  I-MAJ-PIO-CODE          PIC   X(01).             00009100
005100             10  I-MAJ-E1-PART-NO.                                00009200
005200                 15  I-MAJ-PART-NO-5     PIC   X(5).              00009300
005300                 15  I-MAJ-PART-NO-10    PIC   X(5).              00009400
005400                 15  I-MAJ-PART-NO-15    PIC   X(5).              00009500
005500             10  I-MAJ-V5598-PART-CMT    PIC   X(3).              00009600
005600             10  I-MAJ-V5599-PART-CLR    PIC   9(2).              00009700
005700             10  I-MAJ-V5599-PART-CLRX REDEFINES                  00009800
005800                 I-MAJ-V5599-PART-CLR    PIC   X(2).              00009900
005900             10  I-MAJ-V5600-MAJ-COMP    PIC   X(1).              00010000
006000* THE BELOW FIELDS WERE ADDED 12-21-83 TO ALLOW FOR PROCESSING    00010100
006100* 'CAL' MAJOR COMPONENT PARTS.                                    00010200
006200         05  I-AREA-FOR-MAJOR-COMP-CAL.                           00010300
006300             10  I-CAL-SEL-ACTION        PIC   X(03).             00010400
006310             10  I-CAL-PIO-CODE          PIC   X(01).             00010500
006400             10  I-CAL-E1-PART-NO.                                00010600
006500                 15  I-CAL-PART-NO-5     PIC   X(5).              00010700
006600                 15  I-CAL-PART-NO-10    PIC   X(5).              00010800
006700                 15  I-CAL-PART-NO-15    PIC   X(5).              00010900
006800             10  I-CAL-V5598-PART-CMT    PIC   X(3).              00011000
006900             10  I-CAL-V5599-PART-CLR    PIC   9(2).              00011100
007000             10  I-CAL-V5599-PART-CLRX REDEFINES                  00011200
007100                 I-CAL-V5599-PART-CLR    PIC   X(2).              00011300
007200             10  I-CAL-V5600-MAJ-COMP    PIC   X(1).              00011400
007300* THE BELOW FIELDS WERE ADDED 11-01-83 TO ALLOW THE 11TH PART     00011500
007400* ON THE SCREEN TO BE AN ADD OPTION WHEN PROCESSING CHANGES.      00011600
007500         05  I-ADD-HIDDEN-BLD-TXROOT PIC   X(01).                 00011700
007600         05  I-ADD-PIO-CODE          PIC   X(01).                 00011800
007700         05  I-ADD-E1-PART-NO.                                    00011900
007800             10  I-ADD-PART-NO-5     PIC   X(5).                  00012000
007900             10  I-ADD-PART-NO-10    PIC   X(5).                  00012100
008000             10  I-ADD-PART-NO-15    PIC   X(5).                  00012200
008100         05  I-ADD-V5598-PART-CMT    PIC   X(3).                  00012300
008200         05  I-ADD-V5599-PART-CLR    PIC   9(2).                  00012400
008300         05  I-ADD-V5599-PART-CLRX REDEFINES I-ADD-V5599-PART-CLR 00012500
008400                                     PIC   X(2).                  00012600
008500         05  I-ADD-V5600-COMP-TYPE   PIC   X(1).                  00012700
008600         05  I-HIDDEN-PART-NUM       PIC   X(15).                 00012800
008700         05  I-HIDDEN-MID-FIELD.                                  00012900
008800             10  I-HMODEL            PIC   X(4).                  00013000
008900             10  I-HMDLYR            PIC   X(2).                  00013100
009000             10  I-HACC              PIC   X(2).                  00013200
009100             10  I-HPHASE            PIC   X(1).                  00013300
009200         05  I-E5737-UPDATE-SW       PIC   X(1).                  00013400
009300         05  I-LAST-MODEL-PROCESSED  PIC   X(1).                  00013500
009400         05  I-LAST-PART-FOUND-FOR-MDL-ACC                        00013600
009500                                     PIC   X(1).                  00013700
009600             88 LAST-PART-FOUND-FOR-MDL-ACC         VALUE 'Y'.    00013800
009700         05  I-HIDDEN-1-MAJ-IND      PIC   X(1).                  00013900
009800         05  I-HIDDEN-1-CAL-IND      PIC   X(1).                  00014000
009900         05  I-HIDDEN-WARNING-NON-PIO-PART                        00014100
010000                                     PIC   X(1).                  00014200
010100         05  I-SECOND-SCREEN-SW      PIC   X(1).                  00014300
010200             88 SECOND-SCREEN                       VALUE 'Y'.    00014400
      *PCPS CHANGES START                                               00014500
               05  I-HIDDEN-VMF008.                                     00014600
                   15  I-H-MODEL            PIC X(4).                   00014700
                   15  I-H-MDLYR            PIC X(2).                   00014800
                   15  I-H-ACC              PIC X(2).                   00014900
                   15  I-H-PHASE            PIC X(1).                   00015000
                   15  I-H-PWD              PIC X(8).                   00015100
                   15  I-H-FLG              PIC X(1).                   00015200
      *PCPS CHANGES END                                                 00015300
