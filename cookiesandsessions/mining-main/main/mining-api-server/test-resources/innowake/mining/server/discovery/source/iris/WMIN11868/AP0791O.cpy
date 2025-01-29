000100*01  PA0791O-MOD-AREA.                                            00000100
000200     05  MOD-LL                       PIC S9(4) COMP.             00000200
000300     05  MOD-ZZ                       PIC S9(4) COMP.             00000300
000400     05  PA0791O-AREA.                                            00000400
000500         10  O-ACTION-ATTR            PIC X(2).                   00000500
000600         10  O-ACTION                 PIC X(3).                   00000600
000700         10  O-ACTION-E               PIC X(1).                   00000700
000800         10  O-ACCESSORY              PIC X(2).                   00000800
000900         10  O-MDLYR                  PIC X(2).                   00000900
001000         10  O-PHASE                  PIC X(1).                   00001000
      *PCPS CHANGES START                                               00001100
001100*        10  O-MODELS                 PIC X(36).                  00001200
001100*        10  O-MODELS                 PIC X(156).                 00001300
001100         10  O-MODELS                 PIC X(104).                 00001400
      *PCPS CHANGES END                                                 00001500
001200         10  O-MODEL REDEFINES O-MODELS                           00001600
      *PCPS CHANGES START                                               00001700
001300*                         OCCURS  9 TIMES                         00001800
001300*                         OCCURS 39 TIMES                         00001900
001300                          OCCURS 26 TIMES                         00002000
      *PCPS CHANGES END                                                 00002100
001400                              INDEXED BY O-MODEL-INDX             00002200
001500                                      PIC X(4).                   00002300
001600                                                                  00002400
               10  O-SERVCO                 PIC X(13).                  00002500
001700         10  O-UPDATE-ALL-SW          PIC X(01).                  00002600
001800         10  O-MODL1-PROCESSED-SW     PIC X(01).                  00002700
001900         10  O-MODL2-PROCESSED-SW     PIC X(01).                  00002800
002000         10  O-MODL3-PROCESSED-SW     PIC X(01).                  00002900
002100         10  O-MODL4-PROCESSED-SW     PIC X(01).                  00003000
002200         10  O-MODL5-PROCESSED-SW     PIC X(01).                  00003100
002300         10  O-MODL6-PROCESSED-SW     PIC X(01).                  00003200
002400         10  O-MODL7-PROCESSED-SW     PIC X(01).                  00003300
002500         10  O-MODL8-PROCESSED-SW     PIC X(01).                  00003400
002600         10  O-MODL9-PROCESSED-SW     PIC X(01).                  00003500
      *PCPS CHANGES START                                               00003600
002600         10  O-MODL10-PROCESSED-SW    PIC X(01).                  00003700
002600         10  O-MODL11-PROCESSED-SW    PIC X(01).                  00003800
002600         10  O-MODL12-PROCESSED-SW    PIC X(01).                  00003900
002600         10  O-MODL13-PROCESSED-SW    PIC X(01).                  00004000
002600         10  O-MODL14-PROCESSED-SW    PIC X(01).                  00004100
002600         10  O-MODL15-PROCESSED-SW    PIC X(01).                  00004200
002600         10  O-MODL16-PROCESSED-SW    PIC X(01).                  00004300
002600         10  O-MODL17-PROCESSED-SW    PIC X(01).                  00004400
002600         10  O-MODL18-PROCESSED-SW    PIC X(01).                  00004500
002600         10  O-MODL19-PROCESSED-SW    PIC X(01).                  00004600
002600         10  O-MODL20-PROCESSED-SW    PIC X(01).                  00004700
002600         10  O-MODL21-PROCESSED-SW    PIC X(01).                  00004800
002600         10  O-MODL22-PROCESSED-SW    PIC X(01).                  00004900
002600         10  O-MODL23-PROCESSED-SW    PIC X(01).                  00005000
002600         10  O-MODL24-PROCESSED-SW    PIC X(01).                  00005100
002600         10  O-MODL25-PROCESSED-SW    PIC X(01).                  00005200
002600         10  O-MODL26-PROCESSED-SW    PIC X(01).                  00005300
002600*        10  O-MODL27-PROCESSED-SW    PIC X(01).                  00005400
002600*        10  O-MODL28-PROCESSED-SW    PIC X(01).                  00005500
002600*        10  O-MODL29-PROCESSED-SW    PIC X(01).                  00005600
002600*        10  O-MODL30-PROCESSED-SW    PIC X(01).                  00005700
002600*        10  O-MODL31-PROCESSED-SW    PIC X(01).                  00005800
002600*        10  O-MODL32-PROCESSED-SW    PIC X(01).                  00005900
002600*        10  O-MODL33-PROCESSED-SW    PIC X(01).                  00006000
002600*        10  O-MODL34-PROCESSED-SW    PIC X(01).                  00006100
002600*        10  O-MODL35-PROCESSED-SW    PIC X(01).                  00006200
002600*        10  O-MODL36-PROCESSED-SW    PIC X(01).                  00006300
002600*        10  O-MODL37-PROCESSED-SW    PIC X(01).                  00006400
002600*        10  O-MODL38-PROCESSED-SW    PIC X(01).                  00006500
002600*        10  O-MODL39-PROCESSED-SW    PIC X(01).                  00006600
      *PCPS CHANGES END                                                 00006700
                                                                        00006800
002700                                                                  00006900
002800         10  O-SELECTED-MODEL-ATTR    PIC X(02).                  00007000
002900         10  O-SELECTED-MODEL         PIC X(04).                  00007100
003000         10  O-SELECTED-MODEL-E       PIC X(01).                  00007200
003100         10  O-PART-MAINTENANCE OCCURS  8 TIMES INDEXED BY        00007300
003200                         O-PART-INDX.                             00007400
003300             15  O-HIDDEN-BLD-TXROOT   PIC X(01).                 00007500
003400             15  O-SEL-ACTION-ATTR     PIC X(02).                 00007600
003500             15  O-SEL-ACTION          PIC X(03).                 00007700
003600             15  O-SEL-ACTION-E        PIC X(01).                 00007800
003700             15  O-REG-PIO-CODE-ATTR   PIC X(02).                 00007900
003800             15  O-REG-PIO-CODE        PIC X(01).                 00008000
003810             15  O-REG-PIO-CODE-E      PIC X(01).                 00008100
003900             15  O-E1-PART-NO-5-ATTR   PIC X(02).                 00008200
004000             15  O-E1-PART-NO-5        PIC X(05).                 00008300
004100             15  O-E1-PART-NO-10-ATTR  PIC X(02).                 00008400
004200             15  O-E1-PART-NO-10       PIC X(05).                 00008500
004300             15  O-E1-PART-NO-15-ATTR  PIC X(02).                 00008600
004400             15  O-E1-PART-NO-15       PIC X(05).                 00008700
004500             15  O-E1-PART-NO-E        PIC X(01).                 00008800
004600             15  O-V5598-PART-CMT-ATTR PIC X(02).                 00008900
004700             15  O-V5598-PART-CMT      PIC X(03).                 00009000
004800             15  O-V5598-PART-CMT-E    PIC X(01).                 00009100
004900             15  O-V5599-PART-CLR-ATTR PIC X(02).                 00009200
005000             15  O-V5599-PART-CLR      PIC 9(02).                 00009300
005100             15  O-V5599-PART-CLRX REDEFINES O-V5599-PART-CLR     00009400
005200                                       PIC X(02).                 00009500
005300             15  O-V5599-PART-CLR-E    PIC X(01).                 00009600
005400             15  O-V5600-COMP-TYPE-ATTR PIC X(02).                00009700
005500             15  O-V5600-COMP-TYPE     PIC X(01).                 00009800
005600             15  O-V5600-COMP-TYPE-E   PIC X(01).                 00009900
005700* THE FOLLOWING FIELDS WERE ADDED SO THAT REGULAR MAJOR COMPONENT 00010000
005800* PARTS COULD BE PROCESSED.                                       00010100
005900         10  O-AREA-FOR-MAJOR-COMP-REG.                           00010200
006000             15  O-MAJ-SEL-ACTION-ATTR      PIC X(02).            00010300
006100             15  O-MAJ-SEL-ACTION           PIC X(03).            00010400
006200             15  O-MAJ-SEL-ACTION-E         PIC X(01).            00010500
006300             15  O-MAJ-PIO-CODE-ATTR        PIC X(02).            00010600
006400             15  O-MAJ-PIO-CODE             PIC X(01).            00010700
006410             15  O-MAJ-PIO-CODE-E           PIC X(01).            00010800
006500             15  O-MAJ-E1-PART-NO-5-ATTR    PIC X(02).            00010900
006600             15  O-MAJ-E1-PART-NO-5         PIC X(05).            00011000
006700             15  O-MAJ-E1-PART-NO-10-ATTR   PIC X(02).            00011100
006800             15  O-MAJ-E1-PART-NO-10        PIC X(05).            00011200
006900             15  O-MAJ-E1-PART-NO-15-ATTR   PIC X(02).            00011300
007000             15  O-MAJ-E1-PART-NO-15        PIC X(05).            00011400
007100             15  O-MAJ-E1-PART-NO-E         PIC X(01).            00011500
007200             15  O-MAJ-V5598-PART-CMT-ATTR  PIC X(02).            00011600
007300             15  O-MAJ-V5598-PART-CMT       PIC X(03).            00011700
007400             15  O-MAJ-V5598-PART-CMT-E     PIC X(01).            00011800
007500             15  O-MAJ-V5599-PART-CLR-ATTR  PIC X(02).            00011900
007600             15  O-MAJ-V5599-PART-CLR       PIC 9(02).            00012000
007700             15  O-MAJ-V5599-PART-CLRX REDEFINES                  00012100
007800                 O-MAJ-V5599-PART-CLR       PIC X(02).            00012200
007900             15  O-MAJ-V5599-PART-CLR-E     PIC X(01).            00012300
008000             15  O-MAJ-V5600-MAJ-COMP-ATTR  PIC X(02).            00012400
008100             15  O-MAJ-V5600-MAJ-COMP       PIC X(01).            00012500
008200             15  O-MAJ-V5600-MAJ-COMP-E     PIC X(01).            00012600
008500         10  O-AREA-FOR-MAJOR-COMP-CAL.                           00012900
008600             15  O-CAL-SEL-ACTION-ATTR      PIC X(02).            00013000
008700             15  O-CAL-SEL-ACTION           PIC X(03).            00013100
008800             15  O-CAL-SEL-ACTION-E         PIC X(01).            00013200
008900             15  O-CAL-PIO-CODE-ATTR        PIC X(02).            00013300
009000             15  O-CAL-PIO-CODE             PIC X(01).            00013400
009010             15  O-CAL-PIO-CODE-E           PIC X(01).            00013500
009100             15  O-CAL-E1-PART-NO-5-ATTR    PIC X(02).            00013600
009200             15  O-CAL-E1-PART-NO-5         PIC X(05).            00013700
009300             15  O-CAL-E1-PART-NO-10-ATTR   PIC X(02).            00013800
009400             15  O-CAL-E1-PART-NO-10        PIC X(05).            00013900
009500             15  O-CAL-E1-PART-NO-15-ATTR   PIC X(02).            00014000
009600             15  O-CAL-E1-PART-NO-15        PIC X(05).            00014100
009700             15  O-CAL-E1-PART-NO-E         PIC X(01).            00014200
009800             15  O-CAL-V5598-PART-CMT-ATTR  PIC X(02).            00014300
009900             15  O-CAL-V5598-PART-CMT       PIC X(03).            00014400
010000             15  O-CAL-V5598-PART-CMT-E     PIC X(01).            00014500
010100             15  O-CAL-V5599-PART-CLR-ATTR  PIC X(02).            00014600
010200             15  O-CAL-V5599-PART-CLR       PIC 9(02).            00014700
010300             15  O-CAL-V5599-PART-CLRX REDEFINES                  00014800
010400                 O-CAL-V5599-PART-CLR       PIC X(02).            00014900
010500             15  O-CAL-V5599-PART-CLR-E     PIC X(01).            00015000
010600             15  O-CAL-V5600-MAJ-COMP-ATTR  PIC X(02).            00015100
010700             15  O-CAL-V5600-MAJ-COMP       PIC X(01).            00015200
010800             15  O-CAL-V5600-MAJ-COMP-E     PIC X(01).            00015300
011100         10  O-ADD-HIDDEN-BLD-TXROOT   PIC X(01).                 00015600
011200         10  O-ADD-PIO-CODE-ATTR       PIC X(02).                 00015700
011300         10  O-ADD-PIO-CODE            PIC X(01).                 00015800
011310         10  O-ADD-PIO-CODE-E          PIC X(01).                 00015900
011400         10  O-ADD-E1-PART-NO-5-ATTR   PIC X(02).                 00016000
011500         10  O-ADD-E1-PART-NO-5        PIC X(05).                 00016100
011600         10  O-ADD-PART-DASH-1-ATTR    PIC X(02).                 00016200
011700         10  O-ADD-PART-DASH-1         PIC X(01).                 00016300
011800         10  O-ADD-E1-PART-NO-10-ATTR  PIC X(02).                 00016400
011900         10  O-ADD-E1-PART-NO-10       PIC X(05).                 00016500
012000         10  O-ADD-PART-DASH-2-ATTR    PIC X(02).                 00016600
012100         10  O-ADD-PART-DASH-2         PIC X(01).                 00016700
012200         10  O-ADD-E1-PART-NO-15-ATTR  PIC X(02).                 00016800
012300         10  O-ADD-E1-PART-NO-15       PIC X(05).                 00016900
012400         10  O-ADD-E1-PART-NO-E        PIC X(01).                 00017000
012500         10  O-ADD-V5598-PART-CMT-ATTR PIC X(02).                 00017100
012600         10  O-ADD-V5598-PART-CMT      PIC X(03).                 00017200
012700         10  O-ADD-V5598-PART-CMT-E    PIC X(01).                 00017300
012800         10  O-ADD-V5599-PART-CLR-ATTR PIC X(02).                 00017400
012900         10  O-ADD-V5599-PART-CLR      PIC 9(02).                 00017500
013000         10  O-ADD-V5599-PART-CLRX REDEFINES O-ADD-V5599-PART-CLR 00017600
013100                                       PIC X(02).                 00017700
013200         10  O-ADD-V5599-PART-CLR-E    PIC X(01).                 00017800
013300         10  O-ADD-V5600-COMP-TYPE-ATTR                           00017900
013400                                       PIC X(02).                 00018000
013500         10  O-ADD-V5600-COMP-TYPE     PIC X(01).                 00018100
013600         10  O-ADD-V5600-COMP-TYPE-E   PIC X(01).                 00018200
013700*                                                                 00018300
013800         10  O-HIDDEN-PART-NUM        PIC X(15).                  00018400
013900         10  O-HIDDEN-MOD-FIELD.                                  00018500
014000             15  O-HMODEL             PIC X(4).                   00018600
014100             15  O-HMDLYR             PIC X(2).                   00018700
014200             15  O-HACC               PIC X(2).                   00018800
014300             15  O-HPHASE             PIC X(1).                   00018900
014400         10  O-E5737-UPDATE-SW        PIC X(1).                   00019000
014500         10  O-LAST-MODEL-PROCESSED   PIC X(1).                   00019100
014600         10  O-LAST-PART-FOUND-FOR-MDL-ACC                        00019200
014700                                      PIC X(1).                   00019300
014800         10  O-HIDDEN-1-MAJ-IND       PIC X(1).                   00019400
014900         10  O-HIDDEN-1-CAL-IND       PIC X(1).                   00019500
015000         10  O-HIDDEN-WARNING-NON-PIO-PART                        00019600
015100                                      PIC X(1).                   00019700
015200         10  O-SECOND-SCREEN-SW       PIC X(1).                   00019800
      * PCPS CHANGES START                                              00019900
               10  O-HIDDEN-VMF008.                                     00020000
                   15  O-H-MODEL            PIC X(4).                   00020100
                   15  O-H-MDLYR            PIC X(2).                   00020200
                   15  O-H-ACC              PIC X(2).                   00020300
                   15  O-H-PHASE            PIC X(1).                   00020400
                   15  O-H-PWD              PIC X(8).                   00020500
                   15  O-H-FLG              PIC X(1).                   00020600
      * PCPS CHANGES END                                                00020700
015300         10  PA0791-IO-PGMMSG         PIC X(79).                  00020800
