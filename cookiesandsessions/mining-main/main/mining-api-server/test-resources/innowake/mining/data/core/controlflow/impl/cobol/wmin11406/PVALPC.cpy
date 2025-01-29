000100******************************************************************00000100
000200*   COPYLIB MEMBER: PVALPC                                       *00000200
000300*   THIS COPYLIB IS USED TO RETREIVE AND VALIDATE "PART CENTER"  *00000300
000400*   VALUES. THE PART CENTER TABLE CONTAINS VALUES VERY SIMILAR   *00000400
000500*   TO THE TPNASUPP.TWHINFO TABLE AND RETREIVED AND LOADED TO    *00000500
000600*   LINKAGE COPYLIBS BY SUB PROGRAM YSP059.                      *00000600
000700*                                                                *00000700
000800*   NOTE: ANY UPDATES TO THE TPNASUPP.TWHINFO TABLE MUST ALSO    *00000800
000900*         BE CONSIDERED HERE.                                    *00000900
001000******************************************************************00001000
001100 01 PART-CENTER-VALUES.                                           00001100
001200    05 PART-CENTER-AREA.                                          00001200
001300      10 FILLER          PIC X(100) VALUE                         00001300
001400    'OONTARIO             9104051203NAOWC80820NAPCCONTCAN01000999900001400
001500-       '57300ONTE02                            '.                00001500
001600      10 FILLER          PIC X(100) VALUE                         00001600
001700    'TKENTUCKY            9104095202NAOEC34968NAPCKTOLKYN02000998800001700
001800-       '57301MWPE02                            '.                00001800
001900                                                                  00001900
002000    05 BLANK-AREA.                                                00002000
002100       10 FUTURE-PART-CENTER-AREA OCCURS 3 TIMES.                 00002100
002200          15 FILLER           PIC X(100) VALUE SPACES.            00002200
002300                                                                  00002300
002400 01 PART-CENTER-TABLE REDEFINES PART-CENTER-VALUES.               00002400
002500    05 PART-CENTER-ENTRIES OCCURS 5 TIMES INDEXED BY PC-INDEX.    00002500
002600       10 PCT-WH                PIC X(01).                        00002600
002700       10 PCT-WH-NAME           PIC X(20).                        00002700
002800       10 PCT-DISTFD            PIC X(07).                        00002800
002900       10 PCT-SAC-CD            PIC X(03).                        00002900
003000       10 PCT-VDR-CD            PIC X(05).                        00003000
003100       10 PCT-DLR-NUM           PIC X(05).                        00003100
003200       10 PCT-WH-DESC1          PIC X(05).                        00003200
003300       10 PCT-WH-ABBR           PIC X(03).                        00003300
003400       10 PCT-WH-STATE          PIC X(02).                        00003400
003500       10 PCT-WH-CAL-ID         PIC X(03).                        00003500
003600       10 PCT-SCRAP-DISTFD      PIC X(07).                        00003600
003700       10 PCT-TMEX-BUYBACK-DLR  PIC X(05).                        00003700
003800       10 PCT-AS400-LTERM       PIC X(04).                        00003800
003800       10 PCT-CUSTOMS-FLAG      PIC X(02).                        00003900
003800       10 FILLER                PIC X(28).                        00004000
003900                                                                  00004100
004000 01  PVALPC-VALID-PC-CHECK             PIC X(01).                 00004200
004100     88 VALID-PC                VALUE 'O', 'T'.                   00004300
004200                                                                  00004400
004300 01 PVALPC-VALID-DISTFD-CHECK   PIC X(07).                        00004500
004400    88 VALID-PC-DISTFD          VALUE '9104051','9104095'.        00004600
004400    88 VALID-SCRAP-DISTFD       VALUE '0009966','0009977',        00004700
004500                                      '0009986','0009987',        00004800
004600                                      '0009988','0009997',        00004900
004700                                      '0009998','0009999'.        00005000
004900                                                                  00005100
005000 01 PVALPC-VALID-PC-DLR-CHECK   PIC X(05).                        00005200
005100       88 VALID-PC-DLR-NUM      VALUE '80820','34968'.            00005300
005200                                                                  00005400
005300 01 PART-CENTER-VENDOR-VALUES.                                    00005500
005400    05 PART-CENTER-VENDOR-AREA.                                   00005600
005500      10 FILLER          PIC X(10) VALUE                          00005700
005600    'JO        '.                                                 00005800
005700      10 FILLER          PIC X(10) VALUE                          00005900
005800    'NT        '.                                                 00006000
005700      10 FILLER          PIC X(10) VALUE                          00006010
005800    'AO        '.                                                 00006020
005900                                                                  00006100
006000    05 BLANK-AREA.                                                00006200
006100       10 FUTURE-PART-CENTER-VENDOR-AREA OCCURS 3 TIMES.          00006300
006200          15 FILLER           PIC X(70).                          00006400
006300                                                                  00006500
006400 01 PART-CENTER-VENDOR-TABLE REDEFINES PART-CENTER-VENDOR-VALUES. 00006600
006500    05 PART-CENTER-VDR-STK OCCURS 5 TIMES INDEXED BY PC-VS-INDEX. 00006700
006600       10 PCVS-SRCE-CD          PIC X(01).                        00006800
006700       10 PCVS-STK-WH           PIC X(01).                        00006900
006800       10 FILLER                PIC X(08).                        00007000
006900                                                                  00007100
007000 01 NMP-DEFAULT-STK-WH          PIC X(01) VALUE 'O'.              00007200
007100                                                                  00007300
007200 01 NMP-NA-DEFAULT-STK-WH       PIC X(01) VALUE 'T'.              00007400
007300                                                                  00007500
007400 01 SUPPLY-DEFAULT-SHIP-WH      PIC X(01) VALUE 'O'.              00007600
007500                                                                  00007700
007600 01 PART-ACTV-DFLT-STK-WH       PIC X(01) VALUE 'O'.              00007800
007700                                                                  00007900
007800 01 SUPPLY-DEFAULT-REC-WH       PIC X(01) VALUE 'O'.              00008000
