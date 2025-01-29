000010 IDENTIFICATION DIVISION.                                         00001000
000020 PROGRAM-ID.             DPB031.                                  00002000
000000 COPY DPW007.                                                     00003000
000040******************************************************************00004000
000050*    DPB031 -- ROUTINE WHICH BUILDS/LOADS MICM REGIONAL PRICING  *00005000
000060*              RECORD 3013 FOR USE BY CALLING PROGRAM.  THIS     *00006000
000070*              ROUTINE CONTAINS INFORMATION FOR 4 REGIONS.       *00007000
000080*                                                                *00008000
000090*    CALLING PARAMATERS:                                         *00009000
000100*        CALL 'DPB031' USING                                     *00010000
000110*            WS-RGNMIM3013AREAS.                                 *00011000
000120*                                                                *00012000
000130*    LS-RGNMIM3013NBR CONTAINS THE REGION NUMBER OF THE DESIRED  *00013000
000140*    INFORMATION.                                                *00014000
000150*                                                                *00015000
000160*    LS-RGNMIM3013APPL CONTAINS THE APPLICATION NUMBER OF THE    *00016000
000170*    DESIRED INFORMATION.                                        *00017000
000180*                                                                *00018000
000190*    LS-RGNMIM3013MODEL IDENTIFIES THE MODELING PARAMETER OF     *00019000
000200*    THE DESIRED INFORMATION.                                    *00020000
000210*        M  - LOAD MODELING PARAMETERS. IF NO MODELING PARAMETER *00021000
000220*             EXISTS FOR A S/C PARAMETER, SUBSTITUTE A LIVE ONE  *00022000
000230*       ' ' - LOAD LIVE PARAMETERS                               *00023000
000240*                                                                *00024000
000250*    LS-RGNMIM3013LOC IDENTIFIES WHICH OCCURANCE OF THE MICM     *00025000
000260*    DATA IS REQUIRED.                                           *00026000
000270*                                                                *00027000
000280*    LS-RGNMIM3013FUNC IDENTIFIES THE ACTION TO BE TAKEN BY      *00028000
000290*    THIS ROUTINE:                                               *00029000
000300*        B - BUILD THE DESIRED MICM RECORD, THEN LOAD IT         *00030000
000310*        L - LOAD THE DESIRED MICM RECORD                        *00031000
000320*                                                                *00032000
000330*    LS-RGNMIM3013ERROR IDENTIFIES TO THE CALLING PROGRAM IF     *00033000
000340*    AN ERROR HAS BEEN ENCOUNTERED.  IF SO, A 'Y' WILL BE PASSED *00034000
000350*    IN THIS FIELD.                                              *00035000
000360******************************************************************00036000
000370 ENVIRONMENT DIVISION.                                            00037000
000380 INPUT-OUTPUT SECTION.                                            00038000
000390 FILE-CONTROL.                                                    00039000
000400 DATA DIVISION.                                                   00040000
000410 WORKING-STORAGE SECTION.                                         00041000
000420 01  WS-COPYRIGHT                PIC X(57)       VALUE            00042000
000430     'COPYRIGHT(C) 2017 INFOR. ALL RIGHTS RESERVED.            '. 00043000
000440 01  DEP-RLSELIT.                                                 00044000
000000 COPY DPWRLSE.                                                    00045000
000460 01  MIC-RLSELIT.                                                 00046000
000000 COPY MIWRLSE.                                                    00047000
000480 01  SRW105-AREAS.                                                00048000
000000 COPY SRW105.                                                     00049000
000500*01  BICRSRB.                                                     00050000
000000 COPY BICRSRB.                                                    00051000
000520*01  BICRVERB.                                                    00052000
000000 COPY BICRVERB.                                                   00053000
000540*01  BICRSTAT.                                                    00054000
000000 COPY BICRSTAT.                                                   00055000
000560*01  BICRDBS.                                                     00056000
000000 COPY BICRDBS.                                                    00057000
000580*01  BICRPSB.                                                     00058000
000000 COPY BICRPSB.                                                    00059000
000600*01  BICPBPRM.                                                    00060000
000000 COPY BICPBPRM.                                                   00061000
000620 01  FILLER                      PIC X(08)       VALUE '**RGN1**'.00062000
000630 01  WS-RGN1.                                                     00063000
000000 COPY DPW030.                                                     00064000
000650 01  FILLER                      PIC X(08)       VALUE '**RGN2**'.00065000
000660 01  WS-RGN2.                                                     00066000
000000 COPY DPW030.                                                     00067000
000680 01  FILLER                      PIC X(08)       VALUE '**RGN3**'.00068000
000690 01  WS-RGN3.                                                     00069000
000000 COPY DPW030.                                                     00070000
000710 01  FILLER                      PIC X(08)       VALUE '**RGN4**'.00071000
000720 01  WS-RGN4.                                                     00072000
000000 COPY DPW030.                                                     00073000
000740 01  MIMST-RECORD.                                                00074000
000000 COPY MISMST.                                                     00075000
000760 01  MIWPRI-AREAS.                                                00076000
000000 COPY MIWPRI.                                                     00077000
000780 01  DPWPRI-AREAS.                                                00078000
000000 COPY DPWPRI.                                                     00079000
000800 01  MI3013-RECORD.                                               00080000
000000 COPY MIS3013.                                                    00081000
000820 01  MIWMAST-AREAS.                                               00082000
000000 COPY MIWMSTA.                                                    00083000
000840 01  WS-ABEND.                                                    00084000
000000 COPY SRW028.                                                     00085000
000860 01  WS-WORKAREAS.                                                00086000
000870     03  WS-SUB1                 PIC S9(04)      COMP             00087000
000880                                                 VALUE ZEROS.     00088000
000890     03  WS-NXTAVL               PIC S9(04)      COMP             00089000
000900                                                 VALUE ZEROS.     00090000
000910     03  WS-DATAOC               PIC S9(04)      COMP             00091000
000920                                                 VALUE ZEROS.     00092000
000930     03  WS-FUNCTION             PIC X(01).                       00093000
000940     03  WS-3013SAVEAREA         PIC X(71).                       00094000
000950     03  WS-MIMKEYHOLD.                                           00095000
000960         05  WS-HOLDAPPL         PIC 9(02).                       00096000
000970         05  WS-HOLDPARM         PIC 9(03).                       00097000
000980         05  WS-HOLDACCUM        PIC 9(02).                       00098000
000990     03  WS-MODELFOUND           PIC X(01).                       00099000
001000     03  WS-PROCESSMODELSW       PIC X(01).                       00100000
001010******************************************************************00101000
001020 LINKAGE SECTION.                                                 00102000
001030 01  LS-RGNMIM3013AREAS.                                          00103000
000000 COPY DPW031.                                                     00104000
001050******************************************************************00105000
001060 PROCEDURE DIVISION                                               00106000
001070         USING                                                    00107000
001080         LS-RGNMIM3013AREAS.                                      00108000
001090******************************************************************00109000
001100 MAIN-LOGIC SECTION.                                              00110000
001110 ML-START.                                                        00111000
001120     MOVE 'N' TO WS-PROCESSMODELSW.                               00112000
001130     MOVE ZEROS TO WS-MIMKEYHOLD.                                 00113000
001140     MOVE SPACE TO LS-RGNMIM3013ERROR.                            00114000
001150     IF LS-RGNMIM3013LOC IS LESS THAN 1                           00115000
001160         OR LS-RGNMIM3013LOC IS GREATER THAN 4                    00116000
001170         MOVE 'Y' TO LS-RGNMIM3013ERROR                           00117000
001180         GO TO ML-END.                                            00118000
001190     IF LS-RGNMIM3013FUNC IS EQUAL TO 'B'                         00119000
001200         PERFORM MIM-BUILD3013.                                   00120000
001210     IF LS-RGNMIM3013ERROR IS EQUAL TO 'Y'                        00121000
001220         GO TO ML-END.                                            00122000
001230     IF LS-RGNMIM3013LOC IS EQUAL TO 1                            00123000
001240         MOVE WS-RGNMIM3013DATA OF WS-RGN1 TO LS-RGNMIM3013DATA   00124000
001250         GO TO ML-END.                                            00125000
001260     IF LS-RGNMIM3013LOC IS EQUAL TO 2                            00126000
001270         MOVE WS-RGNMIM3013DATA OF WS-RGN2 TO LS-RGNMIM3013DATA   00127000
001280         GO TO ML-END.                                            00128000
001290     IF LS-RGNMIM3013LOC IS EQUAL TO 3                            00129000
001300         MOVE WS-RGNMIM3013DATA OF WS-RGN3 TO LS-RGNMIM3013DATA   00130000
001310         GO TO ML-END.                                            00131000
001320     MOVE WS-RGNMIM3013DATA OF WS-RGN4 TO LS-RGNMIM3013DATA.      00132000
001330 ML-END.                                                          00133000
001340     GOBACK.                                                      00134000
001350 ML-EXIT.                                                         00135000
001360     EXIT.                                                        00136000
001370******************************************************************00137000
001380 MIM-BUILD3013 SECTION.                                           00138000
001390 MB-START.                                                        00139000
001400     IF LS-RGNMIM3013LOC IS EQUAL TO 1                            00140000
001410         MOVE LS-RGNMIM3013DATA TO WS-RGNMIM3013DATA OF WS-RGN1.  00141000
001420     IF LS-RGNMIM3013LOC IS EQUAL TO 2                            00142000
001430         MOVE LS-RGNMIM3013DATA TO WS-RGNMIM3013DATA OF WS-RGN2.  00143000
001440     IF LS-RGNMIM3013LOC IS EQUAL TO 3                            00144000
001450         MOVE LS-RGNMIM3013DATA TO WS-RGNMIM3013DATA OF WS-RGN3.  00145000
001460     IF LS-RGNMIM3013LOC IS EQUAL TO 4                            00146000
001470         MOVE LS-RGNMIM3013DATA TO WS-RGNMIM3013DATA OF WS-RGN4.  00147000
001480     MOVE ZEROS TO WS-SUB1.                                       00148000
001490     MOVE ZEROS TO WS-NXTAVL.                                     00149000
001500 MB-FINDNXTAVLLOOP.                                               00150000
001510     ADD +1 TO WS-SUB1.                                           00151000
001520     IF WS-SUB1 IS EQUAL TO +1001                                 00152000
001530         GO TO MB-FINDNXTAVLEND.                                  00153000
001540     IF LS-RGNMIM3013LOC IS EQUAL TO 1                            00154000
001550         MOVE WS-RGNMIM3013P OF WS-RGN1 (WS-SUB1) TO WS-DATAOC.   00155000
001560     IF LS-RGNMIM3013LOC IS EQUAL TO 2                            00156000
001570         MOVE WS-RGNMIM3013P OF WS-RGN2 (WS-SUB1) TO WS-DATAOC.   00157000
001580     IF LS-RGNMIM3013LOC IS EQUAL TO 3                            00158000
001590         MOVE WS-RGNMIM3013P OF WS-RGN3 (WS-SUB1) TO WS-DATAOC.   00159000
001600     IF LS-RGNMIM3013LOC IS EQUAL TO 4                            00160000
001610         MOVE WS-RGNMIM3013P OF WS-RGN4 (WS-SUB1) TO WS-DATAOC.   00161000
001620     IF WS-DATAOC IS GREATER THAN WS-NXTAVL                       00162000
001630         MOVE WS-DATAOC TO WS-NXTAVL.                             00163000
001640     GO TO MB-FINDNXTAVLLOOP.                                     00164000
001650 MB-FINDNXTAVLEND.                                                00165000
001660     MOVE ZEROS TO WS-SUB1.                                       00166000
001670     MOVE SPACES TO MI3013-RECORD.                                00167000
001680     MOVE LS-RGNMIM3013INST TO MIM-3013KINST.                     00168000
001690     MOVE 3013 TO MIM-3013KREC.                                   00169000
001700     MOVE LS-RGNMIM3013NBR TO MIM-3013KREGION.                    00170000
001710     MOVE LS-RGNMIM3013APPL TO MIM-3013KAPPL.                     00171000
001720     MOVE ZEROS TO MIM-3013KPARM.                                 00172000
001730     MOVE ZEROS TO MIM-3013KACCUM.                                00173000
001740     MOVE ZEROS TO MIM-3013KEFFDT.                                00174000
001750     MOVE MIC-DPS-3013-PRI TO MIC-MST-REC-PRI.                    00175000
001760     MOVE MI3013-RECORD TO MIMST-RECORD.                          00176000
001770     PERFORM MIC-MST-GET-EQUAL-REQUEST.                           00177000
001780     IF WS-FUNCTION IS EQUAL TO 'N'                               00178000
001790         OR WS-FUNCTION IS EQUAL TO 'E'                           00179000
001800         GO TO MB-EXIT.                                           00180000
001810     IF WS-FUNCTION IS NOT EQUAL TO SPACES                        00181000
001820         GO TO MB-ERROR.                                          00182000
001830     GO TO MB-MIM3013LOOP1.                                       00183000
001840 MB-MIM3013LOOP.                                                  00184000
001850     IF WS-PROCESSMODELSW IS EQUAL TO 'Y'                         00185000
001860         MOVE WS-3013SAVEAREA TO MIM-3013DATA                     00186000
001870         MOVE 'N' TO WS-PROCESSMODELSW                            00187000
001880         GO TO MB-MIM3013CONT.                                    00188000
001890     IF WS-PROCESSMODELSW IS EQUAL TO 'E'                         00189000
001900         GO TO MB-EXIT.                                           00190000
001910     PERFORM MIC-MST-GET-NEXT-REQUEST.                            00191000
001920     IF WS-FUNCTION IS EQUAL TO 'E'                               00192000
001930         AND LS-RGNMIM3013MODEL IS EQUAL TO 'M'                   00193000
001940         AND WS-MODELFOUND IS EQUAL TO SPACE                      00194000
001950         MOVE WS-3013SAVEAREA TO MIM-3013DATA                     00195000
001960         MOVE 'E' TO WS-PROCESSMODELSW                            00196000
001970         GO TO MB-MIM3013LOAD.                                    00197000
001980     IF WS-FUNCTION IS EQUAL TO 'E'                               00198000
001990         GO TO MB-EXIT.                                           00199000
002000     IF WS-FUNCTION IS NOT EQUAL TO SPACES                        00200000
002010         GO TO MB-ERROR.                                          00201000
002020 MB-MIM3013LOOP1.                                                 00202000
002030     MOVE MIMST-RECORD TO MI3013-RECORD.                          00203000
002040     IF (MIM-3013KINST IS NOT EQUAL TO LS-RGNMIM3013INST          00204000
002050         OR MIM-3013KREGION IS NOT EQUAL TO LS-RGNMIM3013NBR      00205000
002060         OR MIM-3013KAPPL IS NOT EQUAL TO LS-RGNMIM3013APPL)      00206000
002070         AND LS-RGNMIM3013MODEL IS EQUAL TO 'M'                   00207000
002080         AND WS-MODELFOUND IS EQUAL TO SPACE                      00208000
002090         MOVE WS-3013SAVEAREA TO MIM-3013DATA                     00209000
002100         MOVE 'E' TO WS-PROCESSMODELSW                            00210000
002110         GO TO MB-MIM3013LOAD.                                    00211000
002120     IF MIM-3013KINST IS NOT EQUAL TO LS-RGNMIM3013INST           00212000
002130         OR MIM-3013KREGION IS NOT EQUAL TO LS-RGNMIM3013NBR      00213000
002140         OR MIM-3013KAPPL IS NOT EQUAL TO LS-RGNMIM3013APPL       00214000
002150         GO TO MB-EXIT.                                           00215000
002160     IF MIM-3013KEFFDT IS GREATER THAN LS-RGNMIM3013EFFDT         00216000
002170         GO TO MB-MIM3013LOOP.                                    00217000
002180     IF LS-RGNMIM3013MODEL IS EQUAL TO SPACE                      00218000
002190         AND (MIM-3013KMODEL IS NOT EQUAL TO LS-RGNMIM3013MODEL   00219000
002200         OR (MIM-3013KAPPL IS EQUAL TO WS-HOLDAPPL                00220000
002210         AND MIM-3013KPARM IS EQUAL TO WS-HOLDPARM                00221000
002220         AND MIM-3013KACCUM IS EQUAL TO WS-HOLDACCUM))            00222000
002230         GO TO MB-MIM3013LOOP.                                    00223000
002240* IF NO MODEL RECORD WAS FOUND, LOAD THE FIRST LIVE RECORD        00224000
002250* FOR THIS S/C PARAMETER, WHICH WAS SAVED IN WS-SAVEMODELAREA.    00225000
002260     IF LS-RGNMIM3013MODEL IS NOT EQUAL TO SPACE                  00226000
002270         AND (MIM-3013KAPPL IS NOT EQUAL TO WS-HOLDAPPL           00227000
002280         OR MIM-3013KPARM IS NOT EQUAL TO WS-HOLDPARM             00228000
002290         OR MIM-3013KACCUM IS NOT EQUAL TO WS-HOLDACCUM)          00229000
002300         AND WS-MIMKEYHOLD IS NOT EQUAL TO ZERO                   00230000
002310         AND WS-MODELFOUND IS EQUAL TO SPACE                      00231000
002320         MOVE WS-3013SAVEAREA TO MIM-3013DATA                     00232000
002330         MOVE MIM-3013DATA TO WS-3013SAVEAREA                     00233000
002340         MOVE 'Y' TO WS-PROCESSMODELSW                            00234000
002350         GO TO MB-MIM3013LOAD.                                    00235000
002360 MB-MIM3013CONT.                                                  00236000
002370     IF MIM-3013KAPPL IS NOT EQUAL TO WS-HOLDAPPL                 00237000
002380         OR MIM-3013KPARM IS NOT EQUAL TO WS-HOLDPARM             00238000
002390         OR MIM-3013KACCUM IS NOT EQUAL TO WS-HOLDACCUM           00239000
002400         MOVE MIM-3013KAPPL TO WS-HOLDAPPL                        00240000
002410         MOVE MIM-3013KPARM TO WS-HOLDPARM                        00241000
002420         MOVE MIM-3013KACCUM TO WS-HOLDACCUM                      00242000
002430         MOVE MIM-3013DATA TO WS-3013SAVEAREA                     00243000
002440         MOVE SPACE TO WS-MODELFOUND.                             00244000
002450     IF LS-RGNMIM3013MODEL IS EQUAL TO 'M'                        00245000
002460         AND WS-MODELFOUND IS NOT EQUAL TO SPACE                  00246000
002470         GO TO MB-MIM3013LOOP.                                    00247000
002480     IF MIM-3013KMODEL IS EQUAL TO 'M'                            00248000
002490         MOVE 'Y' TO WS-MODELFOUND.                               00249000
002500 MB-MIM3013LOAD.                                                  00250000
002510     IF LS-RGNMIM3013LOC IS EQUAL TO 2                            00251000
002520         GO TO MB-REGION2.                                        00252000
002530     IF LS-RGNMIM3013LOC IS EQUAL TO 3                            00253000
002540         GO TO MB-REGION3.                                        00254000
002550     IF LS-RGNMIM3013LOC IS EQUAL TO 4                            00255000
002560         GO TO MB-REGION4.                                        00256000
002570 MB-REGION1.                                                      00257000
002580     MOVE MIM-3013KPARM TO WS-DATAOC.                             00258000
002590     IF MIM-3013KPARM IS EQUAL TO ZEROS                           00259000
002600         MOVE +1000 TO WS-DATAOC.                                 00260000
002610     IF WS-RGNMIM3013P OF WS-RGN1(WS-DATAOC) IS NOT EQUAL TO ZEROS00261000
002620         MOVE WS-RGNMIM3013P OF WS-RGN1 (WS-DATAOC) TO WS-SUB1    00262000
002630         GO TO MB-REGION1MOVE.                                    00263000
002640     ADD +1 TO WS-NXTAVL.                                         00264000
002650     MOVE WS-NXTAVL TO WS-SUB1.                                   00265000
002660     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3013MAXSIZE OF WS-RGN1   00266000
002670         GO TO MB-ERROR.                                          00267000
002680     MOVE WS-SUB1 TO WS-RGNMIM3013P OF WS-RGN1 (WS-DATAOC).       00268000
002690 MB-REGION1MOVE.                                                  00269000
002700     MOVE MIM-3013DATA                                            00270000
002710         TO WS-RGNMIM3013A OF WS-RGN1 (WS-SUB1 MIM-3013KACCUM).   00271000
002720     GO TO MB-MIM3013LOOP.                                        00272000
002730 MB-REGION2.                                                      00273000
002740     MOVE MIM-3013KPARM TO WS-DATAOC.                             00274000
002750     IF MIM-3013KPARM IS EQUAL TO ZEROS                           00275000
002760         MOVE +1000 TO WS-DATAOC.                                 00276000
002770     IF WS-RGNMIM3013P OF WS-RGN2(WS-DATAOC) IS NOT EQUAL TO ZEROS00277000
002780         MOVE WS-RGNMIM3013P OF WS-RGN2 (WS-DATAOC) TO WS-SUB1    00278000
002790         GO TO MB-REGION2MOVE.                                    00279000
002800     ADD +1 TO WS-NXTAVL.                                         00280000
002810     MOVE WS-NXTAVL TO WS-SUB1.                                   00281000
002820     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3013MAXSIZE OF WS-RGN2   00282000
002830         GO TO MB-ERROR.                                          00283000
002840     MOVE WS-SUB1 TO WS-RGNMIM3013P OF WS-RGN2 (WS-DATAOC).       00284000
002850 MB-REGION2MOVE.                                                  00285000
002860     MOVE MIM-3013DATA                                            00286000
002870         TO WS-RGNMIM3013A OF WS-RGN2 (WS-SUB1 MIM-3013KACCUM).   00287000
002880     GO TO MB-MIM3013LOOP.                                        00288000
002890 MB-REGION3.                                                      00289000
002900     MOVE MIM-3013KPARM TO WS-DATAOC.                             00290000
002910     IF MIM-3013KPARM IS EQUAL TO ZEROS                           00291000
002920         MOVE +1000 TO WS-DATAOC.                                 00292000
002930     IF WS-RGNMIM3013P OF WS-RGN3(WS-DATAOC) IS NOT EQUAL TO ZEROS00293000
002940         MOVE WS-RGNMIM3013P OF WS-RGN3 (WS-DATAOC) TO WS-SUB1    00294000
002950         GO TO MB-REGION3MOVE.                                    00295000
002960     ADD +1 TO WS-NXTAVL.                                         00296000
002970     MOVE WS-NXTAVL TO WS-SUB1.                                   00297000
002980     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3013MAXSIZE OF WS-RGN3   00298000
002990         GO TO MB-ERROR.                                          00299000
003000     MOVE WS-SUB1 TO WS-RGNMIM3013P OF WS-RGN3 (WS-DATAOC).       00300000
003010 MB-REGION3MOVE.                                                  00301000
003020     MOVE MIM-3013DATA                                            00302000
003030         TO WS-RGNMIM3013A OF WS-RGN3 (WS-SUB1 MIM-3013KACCUM).   00303000
003040     GO TO MB-MIM3013LOOP.                                        00304000
003050 MB-REGION4.                                                      00305000
003060     MOVE MIM-3013KPARM TO WS-DATAOC.                             00306000
003070     IF MIM-3013KPARM IS EQUAL TO ZEROS                           00307000
003080         MOVE +1000 TO WS-DATAOC.                                 00308000
003090     IF WS-RGNMIM3013P OF WS-RGN4(WS-DATAOC) IS NOT EQUAL TO ZEROS00309000
003100         MOVE WS-RGNMIM3013P OF WS-RGN4 (WS-DATAOC) TO WS-SUB1    00310000
003110         GO TO MB-REGION4MOVE.                                    00311000
003120     ADD +1 TO WS-NXTAVL.                                         00312000
003130     MOVE WS-NXTAVL TO WS-SUB1.                                   00313000
003140     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3013MAXSIZE OF WS-RGN4   00314000
003150         GO TO MB-ERROR.                                          00315000
003160     MOVE WS-SUB1 TO WS-RGNMIM3013P OF WS-RGN4 (WS-DATAOC).       00316000
003170 MB-REGION4MOVE.                                                  00317000
003180     MOVE MIM-3013DATA                                            00318000
003190         TO WS-RGNMIM3013A OF WS-RGN4 (WS-SUB1 MIM-3013KACCUM).   00319000
003200     GO TO MB-MIM3013LOOP.                                        00320000
003210 MB-ERROR.                                                        00321000
003220     MOVE 'Y' TO LS-RGNMIM3013ERROR.                              00322000
003230 MB-EXIT.                                                         00323000
003240     EXIT.                                                        00324000
003250******************************************************************00325000
003260 MIC-MST-API SECTION.                                             00326000
000000 COPY MIPMSTA.                                                    00327000
003280******************************************************************00328000
