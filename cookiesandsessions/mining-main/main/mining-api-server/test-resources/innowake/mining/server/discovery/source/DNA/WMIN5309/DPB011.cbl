000010 IDENTIFICATION DIVISION.                                         00001000
000020 PROGRAM-ID.             DPB011.                                  00002000
000000 COPY DPW007.                                                     00003000
000040******************************************************************00004000
000050*    DPB011 -- ROUTINE WHICH BUILDS/LOADS MICM REGIONAL PRICING  *00005000
000060*              RECORD 3010 FOR USE BY CALLING PROGRAM.  THIS     *00006000
000070*              ROUTINE CONTAINS INFORMATION FOR 4 REGIONS.       *00007000
000080*                                                                *00008000
000090*    CALLING PARAMATERS:                                         *00009000
000100*        CALL 'DPB011' USING                                     *00010000
000110*            WS-RGNMIM3010AREAS.                                 *00011000
000120*                                                                *00012000
000130*    LS-RGNMIM3010NBR CONTAINS THE REGION NUMBER OF THE DESIRED  *00013000
000140*    INFORMATION.                                                *00014000
000150*                                                                *00015000
000160*    LS-RGNMIM3010APPL CONTAINS THE APPLICATION NUMBER OF THE    *00016000
000170*    DESIRED INFORMATION.                                        *00017000
000180*                                                                *00018000
000190*    LS-RGNMIM3010LOC IDENTIFIES WHICH OCCURANCE OF THE MICM     *00019000
000200*    DATA IS REQUIRED.                                           *00020000
000210*                                                                *00021000
000220*    LS-RGNMIM3010FUNC IDENTIFIES THE ACTION TO BE TAKEN BY      *00022000
000230*    THIS ROUTINE:                                               *00023000
000240*        B - BUILD THE DESIRED MICM RECORD, THEN LOAD IT         *00024000
000250*        L - LOAD THE DESIRED MICM RECORD                        *00025000
000260*                                                                *00026000
000270*    LS-RGNMIM3010ERROR IDENTIFIES TO THE CALLING PROGRAM IF     *00027000
000280*    AN ERROR HAS BEEN ENCOUNTERED.  IF SO, A 'Y' WILL BE PASSED *00028000
000290*    IN THIS FIELD.                                              *00029000
000300******************************************************************00030000
000310 ENVIRONMENT DIVISION.                                            00031000
000320 INPUT-OUTPUT SECTION.                                            00032000
000330 FILE-CONTROL.                                                    00033000
000340 DATA DIVISION.                                                   00034000
000350 WORKING-STORAGE SECTION.                                         00035000
000360 01  WS-COPYRIGHT                PIC X(57)       VALUE            00036000
000370     'COPYRIGHT(C) 2017 INFOR. ALL RIGHTS RESERVED.            '. 00037000
000380 01  DEP-RLSELIT.                                                 00038000
000000 COPY DPWRLSE.                                                    00039000
000400 01  MIC-RLSELIT.                                                 00040000
000000 COPY MIWRLSE.                                                    00041000
000420 01  SRW105-AREAS.                                                00042000
000000 COPY SRW105.                                                     00043000
000440*01  BICRSRB.                                                     00044000
000000 COPY BICRSRB.                                                    00045000
000460*01  BICRVERB.                                                    00046000
000000 COPY BICRVERB.                                                   00047000
000480*01  BICRSTAT.                                                    00048000
000000 COPY BICRSTAT.                                                   00049000
000500*01  BICRDBS.                                                     00050000
000000 COPY BICRDBS.                                                    00051000
000520*01  BICRPSB.                                                     00052000
000000 COPY BICRPSB.                                                    00053000
000540*01  BICPBPRM.                                                    00054000
000000 COPY BICPBPRM.                                                   00055000
000560 01  FILLER                      PIC X(08)       VALUE '**RGN1**'.00056000
000570 01  WS-RGN1.                                                     00057000
000000 COPY DPW010.                                                     00058000
000590 01  FILLER                      PIC X(08)       VALUE '**RGN2**'.00059000
000600 01  WS-RGN2.                                                     00060000
000000 COPY DPW010.                                                     00061000
000620 01  FILLER                      PIC X(08)       VALUE '**RGN3**'.00062000
000630 01  WS-RGN3.                                                     00063000
000000 COPY DPW010.                                                     00064000
000650 01  FILLER                      PIC X(08)       VALUE '**RGN4**'.00065000
000660 01  WS-RGN4.                                                     00066000
000000 COPY DPW010.                                                     00067000
000680 01  MIWPRI-AREAS.                                                00068000
000000 COPY MIWPRI.                                                     00069000
000700 01  DPWPRI-AREAS.                                                00070000
000000 COPY DPWPRI.                                                     00071000
000720 01  MIMST-RECORD.                                                00072000
000000 COPY MISMST.                                                     00073000
000740 01  MI3010-RECORD.                                               00074000
000000 COPY MIS3010.                                                    00075000
000760 01  MIWMSTA-AREAS.                                               00076000
000000 COPY MIWMSTA.                                                    00077000
000780 01  WS-ABEND.                                                    00078000
000000 COPY SRW028.                                                     00079000
000800 01  WS-WORKAREAS.                                                00080000
000810     03  WS-SUB1                 PIC S9(04)      COMP             00081000
000820                                                 VALUE ZEROS.     00082000
000830     03  WS-NXTAVL               PIC S9(04)      COMP             00083000
000840                                                 VALUE ZEROS.     00084000
000850     03  WS-DATAOC               PIC S9(04)      COMP             00085000
000860                                                 VALUE ZEROS.     00086000
000870     03  WS-FUNCTION             PIC X(01).                       00087000
000880     03  WS-MIMKEYHOLD.                                           00088000
000890         05  WS-HOLDAPPL         PIC 9(02).                       00089000
000900         05  WS-HOLDPLAN         PIC X(10).                       00090000
000910     03  WS-TEMPKEY              PIC X(10).                       00091000
000920     03  WS-TEMP                 PIC X(79).                       00092000
000930******************************************************************00093000
000940 LINKAGE SECTION.                                                 00094000
000950 01  LS-RGNMIM3010AREAS.                                          00095000
000000 COPY DPW011.                                                     00096000
000970******************************************************************00097000
000980 PROCEDURE DIVISION                                               00098000
000990         USING                                                    00099000
001000         LS-RGNMIM3010AREAS.                                      00100000
001010******************************************************************00101000
001020 MAIN-LOGIC SECTION.                                              00102000
001030 ML-START.                                                        00103000
001040     MOVE ZEROS TO WS-HOLDAPPL.                                   00104000
001050     MOVE SPACES TO WS-HOLDPLAN.                                  00105000
001060     MOVE SPACE TO LS-RGNMIM3010ERROR.                            00106000
001070     IF LS-RGNMIM3010LOC IS LESS THAN 1                           00107000
001080         OR LS-RGNMIM3010LOC IS GREATER THAN 4                    00108000
001090         MOVE 'Y' TO LS-RGNMIM3010ERROR                           00109000
001100         GO TO ML-END.                                            00110000
001110     IF LS-RGNMIM3010FUNC IS EQUAL TO 'B'                         00111000
001120         PERFORM MIM-BUILD3010.                                   00112000
001130     IF LS-RGNMIM3010ERROR IS EQUAL TO 'Y'                        00113000
001140         GO TO ML-END.                                            00114000
001150     IF LS-RGNMIM3010LOC IS EQUAL TO 1                            00115000
001160         MOVE WS-RGNMIM3010DATA OF WS-RGN1 TO LS-RGNMIM3010DATA   00116000
001170         GO TO ML-END.                                            00117000
001180     IF LS-RGNMIM3010LOC IS EQUAL TO 2                            00118000
001190         MOVE WS-RGNMIM3010DATA OF WS-RGN2 TO LS-RGNMIM3010DATA   00119000
001200         GO TO ML-END.                                            00120000
001210     IF LS-RGNMIM3010LOC IS EQUAL TO 3                            00121000
001220         MOVE WS-RGNMIM3010DATA OF WS-RGN3 TO LS-RGNMIM3010DATA   00122000
001230         GO TO ML-END.                                            00123000
001240     MOVE WS-RGNMIM3010DATA OF WS-RGN4 TO LS-RGNMIM3010DATA.      00124000
001250 ML-END.                                                          00125000
001260     GOBACK.                                                      00126000
001270 ML-EXIT.                                                         00127000
001280     EXIT.                                                        00128000
001290******************************************************************00129000
001300 MIM-BUILD3010 SECTION.                                           00130000
001310 MB-START.                                                        00131000
001320     IF LS-RGNMIM3010LOC IS EQUAL TO 1                            00132000
001330         MOVE LS-RGNMIM3010DATA TO WS-RGNMIM3010DATA OF WS-RGN1.  00133000
001340     IF LS-RGNMIM3010LOC IS EQUAL TO 2                            00134000
001350         MOVE LS-RGNMIM3010DATA TO WS-RGNMIM3010DATA OF WS-RGN2.  00135000
001360     IF LS-RGNMIM3010LOC IS EQUAL TO 3                            00136000
001370         MOVE LS-RGNMIM3010DATA TO WS-RGNMIM3010DATA OF WS-RGN3.  00137000
001380     IF LS-RGNMIM3010LOC IS EQUAL TO 4                            00138000
001390         MOVE LS-RGNMIM3010DATA TO WS-RGNMIM3010DATA OF WS-RGN4.  00139000
001400     MOVE ZEROS TO WS-SUB1.                                       00140000
001410     MOVE ZEROS TO WS-NXTAVL.                                     00141000
001420 MB-FINDNXTAVLEND.                                                00142000
001430     MOVE ZEROS TO WS-SUB1.                                       00143000
001440     MOVE SPACES TO MI3010-RECORD.                                00144000
001450     MOVE LS-RGNMIM3010INST TO MIM-3010KINST.                     00145000
001460     MOVE 3010 TO MIM-3010KREC.                                   00146000
001470     MOVE LS-RGNMIM3010NBR TO MIM-3010KREGION.                    00147000
001480     MOVE LS-RGNMIM3010APPL TO MIM-3010KAPPL.                     00148000
001490     MOVE SPACES TO MIM-3010KCHRGPLAN.                            00149000
001500     MOVE ZEROS TO MIM-3010KEFFDT.                                00150000
001510     MOVE MIC-DPS-3010-PRI TO MIC-MST-REC-PRI.                    00151000
001520     MOVE MI3010-RECORD TO MIMST-RECORD.                          00152000
001530     PERFORM MIC-MST-GET-EQUAL-REQUEST.                           00153000
001540     GO TO MB-MIM3010LOOP1.                                       00154000
001550 MB-MIM3010LOOP.                                                  00155000
001560     PERFORM MIC-MST-GET-NEXT-REQUEST.                            00156000
001570 MB-MIM3010LOOP1.                                                 00157000
001580     IF WS-FUNCTION IS EQUAL TO 'E'                               00158000
001590         GO TO MB-EXIT.                                           00159000
001600     IF WS-FUNCTION IS NOT EQUAL TO SPACES                        00160000
001610         GO TO MB-ERROR.                                          00161000
001620     MOVE MIMST-RECORD TO MI3010-RECORD.                          00162000
001630     IF MIM-3010KINST IS NOT EQUAL TO LS-RGNMIM3010INST           00163000
001640         OR MIM-3010KREGION IS NOT EQUAL TO LS-RGNMIM3010NBR      00164000
001650         OR MIM-3010KAPPL IS NOT EQUAL TO LS-RGNMIM3010APPL       00165000
001660         GO TO MB-EXIT.                                           00166000
001670     IF MIM-3010KEFFDT IS GREATER THAN LS-RGNMIM3010EFFDT         00167000
001680         OR (MIM-3010KAPPL IS EQUAL TO WS-HOLDAPPL                00168000
001690         AND MIM-3010KCHRGPLAN IS EQUAL TO WS-HOLDPLAN)           00169000
001700         GO TO MB-MIM3010LOOP.                                    00170000
001710     MOVE MIM-3010KAPPL TO WS-HOLDAPPL.                           00171000
001720     MOVE MIM-3010KCHRGPLAN TO WS-HOLDPLAN.                       00172000
001730     IF LS-RGNMIM3010LOC IS EQUAL TO 2                            00173000
001740         GO TO MB-REGION2.                                        00174000
001750     IF LS-RGNMIM3010LOC IS EQUAL TO 3                            00175000
001760         GO TO MB-REGION3.                                        00176000
001770     IF LS-RGNMIM3010LOC IS EQUAL TO 4                            00177000
001780         GO TO MB-REGION4.                                        00178000
001790* WHEN LOADING 3010 REGION RECORDS, THEY ARE BUBBLE SORTED        00179000
001800* INTO THE CORRECT ALPHANUMERIC SEQUENCE.                         00180000
001810 MB-REGION1.                                                      00181000
001820     MOVE +0 TO WS-SUB1.                                          00182000
001830 MB-REGION1LOOP.                                                  00183000
001840     ADD +1 TO WS-SUB1.                                           00184000
001850     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3010MAXSIZE OF WS-RGN1   00185000
001860         GO TO MB-ERROR.                                          00186000
001870     IF WS-RGNMIM3010K OF WS-RGN1 (WS-SUB1) IS EQUAL TO LOW-VALUES00187000
001880         GO TO MB-REGION1MOVE.                                    00188000
001890     IF WS-RGNMIM3010K OF WS-RGN1 (WS-SUB1)                       00189000
001900         IS LESS THAN MIM-3010KCHRGPLAN                           00190000
001910         GO TO MB-REGION1LOOP.                                    00191000
001920     IF WS-RGNMIM3010K OF WS-RGN1 (WS-SUB1)                       00192000
001930         IS EQUAL TO MIM-3010KCHRGPLAN                            00193000
001940         GO TO MB-REGION1MOVE.                                    00194000
001950     MOVE WS-RGNMIM3010K OF WS-RGN1 (WS-SUB1) TO WS-TEMPKEY.      00195000
001960     MOVE WS-RGNMIM3010 OF WS-RGN1 (WS-SUB1) TO WS-TEMP.          00196000
001970     MOVE MIM-3010KCHRGPLAN                                       00197000
001980         TO WS-RGNMIM3010K OF WS-RGN1 (WS-SUB1).                  00198000
001990     MOVE MIM-3010DATA TO WS-RGNMIM3010 OF WS-RGN1 (WS-SUB1).     00199000
002000     MOVE WS-TEMPKEY TO MIM-3010KCHRGPLAN.                        00200000
002010     MOVE WS-TEMP TO MIM-3010DATA.                                00201000
002020     GO TO MB-REGION1LOOP.                                        00202000
002030 MB-REGION1MOVE.                                                  00203000
002040     MOVE MIM-3010KCHRGPLAN                                       00204000
002050         TO WS-RGNMIM3010K OF WS-RGN1 (WS-SUB1).                  00205000
002060     MOVE MIM-3010DATA TO WS-RGNMIM3010 OF WS-RGN1 (WS-SUB1).     00206000
002070     GO TO MB-MIM3010LOOP.                                        00207000
002080 MB-REGION2.                                                      00208000
002090     MOVE +0 TO WS-SUB1.                                          00209000
002100 MB-REGION2LOOP.                                                  00210000
002110     ADD +1 TO WS-SUB1.                                           00211000
002120     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3010MAXSIZE OF WS-RGN2   00212000
002130         GO TO MB-ERROR.                                          00213000
002140     IF WS-RGNMIM3010K OF WS-RGN2 (WS-SUB1) IS EQUAL TO LOW-VALUES00214000
002150         GO TO MB-REGION2MOVE.                                    00215000
002160     IF WS-RGNMIM3010K OF WS-RGN2 (WS-SUB1)                       00216000
002170         IS LESS THAN MIM-3010KCHRGPLAN                           00217000
002180         GO TO MB-REGION2LOOP.                                    00218000
002190     IF WS-RGNMIM3010K OF WS-RGN2 (WS-SUB1)                       00219000
002200         IS EQUAL TO MIM-3010KCHRGPLAN                            00220000
002210         GO TO MB-REGION2MOVE.                                    00221000
002220     MOVE WS-RGNMIM3010K OF WS-RGN2 (WS-SUB1) TO WS-TEMPKEY.      00222000
002230     MOVE WS-RGNMIM3010 OF WS-RGN2 (WS-SUB1) TO WS-TEMP.          00223000
002240     MOVE MIM-3010KCHRGPLAN                                       00224000
002250         TO WS-RGNMIM3010K OF WS-RGN2 (WS-SUB1).                  00225000
002260     MOVE MIM-3010DATA TO WS-RGNMIM3010 OF WS-RGN2 (WS-SUB1).     00226000
002270     MOVE WS-TEMPKEY TO MIM-3010KCHRGPLAN.                        00227000
002280     MOVE WS-TEMP TO MIM-3010DATA.                                00228000
002290     GO TO MB-REGION1LOOP.                                        00229000
002300 MB-REGION2MOVE.                                                  00230000
002310     MOVE MIM-3010KCHRGPLAN                                       00231000
002320         TO WS-RGNMIM3010K OF WS-RGN2 (WS-SUB1).                  00232000
002330     MOVE MIM-3010DATA TO WS-RGNMIM3010 OF WS-RGN2 (WS-SUB1).     00233000
002340     GO TO MB-MIM3010LOOP.                                        00234000
002350 MB-REGION3.                                                      00235000
002360     MOVE +0 TO WS-SUB1.                                          00236000
002370 MB-REGION3LOOP.                                                  00237000
002380     ADD +1 TO WS-SUB1.                                           00238000
002390     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3010MAXSIZE OF WS-RGN3   00239000
002400         GO TO MB-ERROR.                                          00240000
002410     IF WS-RGNMIM3010K OF WS-RGN3 (WS-SUB1) IS EQUAL TO LOW-VALUES00241000
002420         GO TO MB-REGION3MOVE.                                    00242000
002430     IF WS-RGNMIM3010K OF WS-RGN3 (WS-SUB1)                       00243000
002440         IS LESS THAN MIM-3010KCHRGPLAN                           00244000
002450         GO TO MB-REGION3LOOP.                                    00245000
002460     IF WS-RGNMIM3010K OF WS-RGN3 (WS-SUB1)                       00246000
002470         IS EQUAL TO MIM-3010KCHRGPLAN                            00247000
002480         GO TO MB-REGION3MOVE.                                    00248000
002490     MOVE WS-RGNMIM3010K OF WS-RGN3 (WS-SUB1) TO WS-TEMPKEY.      00249000
002500     MOVE WS-RGNMIM3010 OF WS-RGN3 (WS-SUB1) TO WS-TEMP.          00250000
002510     MOVE MIM-3010KCHRGPLAN                                       00251000
002520         TO WS-RGNMIM3010K OF WS-RGN3 (WS-SUB1).                  00252000
002530     MOVE MIM-3010DATA TO WS-RGNMIM3010 OF WS-RGN3 (WS-SUB1).     00253000
002540     MOVE WS-TEMPKEY TO MIM-3010KCHRGPLAN.                        00254000
002550     MOVE WS-TEMP TO MIM-3010DATA.                                00255000
002560     GO TO MB-REGION1LOOP.                                        00256000
002570 MB-REGION3MOVE.                                                  00257000
002580     MOVE MIM-3010KCHRGPLAN                                       00258000
002590         TO WS-RGNMIM3010K OF WS-RGN3 (WS-SUB1).                  00259000
002600     MOVE MIM-3010DATA TO WS-RGNMIM3010 OF WS-RGN3 (WS-SUB1).     00260000
002610     GO TO MB-MIM3010LOOP.                                        00261000
002620 MB-REGION4.                                                      00262000
002630     MOVE +0 TO WS-SUB1.                                          00263000
002640 MB-REGION4LOOP.                                                  00264000
002650     ADD +1 TO WS-SUB1.                                           00265000
002660     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3010MAXSIZE OF WS-RGN4   00266000
002670         GO TO MB-ERROR.                                          00267000
002680     IF WS-RGNMIM3010K OF WS-RGN4 (WS-SUB1) IS EQUAL TO LOW-VALUES00268000
002690         GO TO MB-REGION4MOVE.                                    00269000
002700     IF WS-RGNMIM3010K OF WS-RGN4 (WS-SUB1)                       00270000
002710         IS LESS THAN MIM-3010KCHRGPLAN                           00271000
002720         GO TO MB-REGION4LOOP.                                    00272000
002730     IF WS-RGNMIM3010K OF WS-RGN4 (WS-SUB1)                       00273000
002740         IS EQUAL TO MIM-3010KCHRGPLAN                            00274000
002750         GO TO MB-REGION4MOVE.                                    00275000
002760     MOVE WS-RGNMIM3010K OF WS-RGN4 (WS-SUB1) TO WS-TEMPKEY.      00276000
002770     MOVE WS-RGNMIM3010 OF WS-RGN4 (WS-SUB1) TO WS-TEMP.          00277000
002780     MOVE MIM-3010KCHRGPLAN                                       00278000
002790         TO WS-RGNMIM3010K OF WS-RGN4 (WS-SUB1).                  00279000
002800     MOVE MIM-3010DATA TO WS-RGNMIM3010 OF WS-RGN4 (WS-SUB1).     00280000
002810     MOVE WS-TEMPKEY TO MIM-3010KCHRGPLAN.                        00281000
002820     MOVE WS-TEMP TO MIM-3010DATA.                                00282000
002830     GO TO MB-REGION1LOOP.                                        00283000
002840 MB-REGION4MOVE.                                                  00284000
002850     MOVE MIM-3010KCHRGPLAN                                       00285000
002860         TO WS-RGNMIM3010K OF WS-RGN4 (WS-SUB1).                  00286000
002870     MOVE MIM-3010DATA TO WS-RGNMIM3010 OF WS-RGN4 (WS-SUB1).     00287000
002880     GO TO MB-MIM3010LOOP.                                        00288000
002890 MB-ERROR.                                                        00289000
002900     MOVE 'Y' TO LS-RGNMIM3010ERROR.                              00290000
002910 MB-EXIT.                                                         00291000
002920     EXIT.                                                        00292000
002930******************************************************************00293000
002940 MIC-MST-API SECTION.                                             00294000
000000 COPY MIPMSTA.                                                    00295000
002960******************************************************************00296000
