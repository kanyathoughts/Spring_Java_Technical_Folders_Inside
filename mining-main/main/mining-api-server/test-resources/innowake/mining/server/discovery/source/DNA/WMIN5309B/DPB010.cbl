000010 IDENTIFICATION DIVISION.                                         00001000
000020 PROGRAM-ID.             DPB010.                                  00002000
000000 COPY DPW007.                                                     00003000
000040******************************************************************00004000
000050*    DPB010 -- ROUTINE WHICH BUILDS/LOADS MICM REGIONAL PRICING  *00005000
000060*              RECORD 3010 FOR USE BY CALLING PROGRAM.  THIS     *00006000
000070*              ROUTINE CONTAINS THE REGION 00 INFORMATION.       *00007000
000080*                                                                *00008000
000090*    CALLING PARAMATERS:                                         *00009000
000100*        CALL 'DPB010' USING                                     *00010000
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
000250*        M - LOAD THE DESIRED MICM RECORD                        *00025000
000260*                                                                *00026000
000270*    LS-RGNMIM3010ERROR IDENTIFIES TO THE CALLING PROGRAM IF     *00027000
000280*    AN ERROR HAS BEEN ENCOUNTERED.  IF SO, A 'Y' WILL BE PASSED *00028000
000290*    IN THIS FIELD.                                              *00029000
000300******************************************************************00030000
000310 ENVIRONMENT DIVISION.                                            00031000
000320 INPUT-OUTPUT SECTION.                                            00032000
000330 DATA DIVISION.                                                   00033000
000340 WORKING-STORAGE SECTION.                                         00034000
000350 01  WS-COPYRIGHT                PIC X(57)       VALUE            00035000
000360     'COPYRIGHT(C) 2017 INFOR. ALL RIGHTS RESERVED.            '. 00036000
000370 01  DEP-RLSELIT.                                                 00037000
000000 COPY DPWRLSE.                                                    00038000
000390 01  MIC-RLSELIT.                                                 00039000
000000 COPY MIWRLSE.                                                    00040000
000410 01  SRW105-AREAS.                                                00041000
000000 COPY SRW105.                                                     00042000
000430*01  BICRSRB.                                                     00043000
000000 COPY BICRSRB.                                                    00044000
000450*01  BICRVERB.                                                    00045000
000000 COPY BICRVERB.                                                   00046000
000470*01  BICRSTAT.                                                    00047000
000000 COPY BICRSTAT.                                                   00048000
000490*01  BICRDBS.                                                     00049000
000000 COPY BICRDBS.                                                    00050000
000510*01  BICRPSB.                                                     00051000
000000 COPY BICRPSB.                                                    00052000
000530*01  BICPBPRM.                                                    00053000
000000 COPY BICPBPRM.                                                   00054000
000550 01  WS-RGNMIM3010AREAS.                                          00055000
000000 COPY DPW010.                                                     00056000
000570 01  MIMST-RECORD.                                                00057000
000000 COPY MISMST.                                                     00058000
000590 01  MIWPRI-AREAS.                                                00059000
000000 COPY MIWPRI.                                                     00060000
000610 01  DPWPRI-AREAS.                                                00061000
000000 COPY DPWPRI.                                                     00062000
000630 01  MI3010-RECORD.                                               00063000
000000 COPY MIS3010.                                                    00064000
000650 01  MIWMSTA-AREAS.                                               00065000
000000 COPY MIWMSTA.                                                    00066000
000670 01  WS-ABEND.                                                    00067000
000000 COPY SRW028.                                                     00068000
000690 01  WS-WORKAREAS.                                                00069000
000700     03  WS-SUB1                 PIC S9(04)      COMP             00070000
000710                                                 VALUE ZEROS.     00071000
000720     03  WS-FUNCTION             PIC X(01).                       00072000
000730     03  WS-MIMKEYHOLD.                                           00073000
000740         05  WS-HOLDAPPL         PIC 9(02).                       00074000
000750         05  WS-HOLDPLAN         PIC X(10).                       00075000
000760******************************************************************00076000
000770 LINKAGE SECTION.                                                 00077000
000780 01  LS-RGNMIM3010AREAS.                                          00078000
000000 COPY DPW011.                                                     00079000
000800******************************************************************00080000
000810 PROCEDURE DIVISION                                               00081000
000820         USING                                                    00082000
000830         LS-RGNMIM3010AREAS.                                      00083000
000840******************************************************************00084000
000850 MAIN-LOGIC SECTION.                                              00085000
000860 ML-START.                                                        00086000
000870     MOVE ZEROS TO WS-HOLDAPPL.                                   00087000
000880     MOVE SPACES TO WS-HOLDPLAN.                                  00088000
000890     MOVE SPACE TO LS-RGNMIM3010ERROR.                            00089000
000900     IF LS-RGNMIM3010FUNC IS EQUAL TO 'B'                         00090000
000910         PERFORM MIM-BUILD3010.                                   00091000
000920     IF LS-RGNMIM3010ERROR IS NOT EQUAL TO 'Y'                    00092000
000930         MOVE WS-RGNMIM3010DATA TO LS-RGNMIM3010DATA.             00093000
000940     GOBACK.                                                      00094000
000950 ML-EXIT.                                                         00095000
000960     EXIT.                                                        00096000
000970******************************************************************00097000
000980 MIM-BUILD3010 SECTION.                                           00098000
000990 MB-START.                                                        00099000
001000     MOVE LOW-VALUES TO WS-RGNMIM3010DATA.                        00100000
001010     MOVE ZEROS TO WS-SUB1.                                       00101000
001020     MOVE LS-RGNMIM3010INST TO MIM-3010KINST.                     00102000
001030     MOVE +3010 TO MIM-3010KREC.                                  00103000
001040     MOVE LS-RGNMIM3010NBR TO MIM-3010KREGION.                    00104000
001050     MOVE LS-RGNMIM3010APPL TO MIM-3010KAPPL.                     00105000
001060     MOVE SPACES TO MIM-3010KCHRGPLAN.                            00106000
001070     MOVE ZEROS TO MIM-3010KEFFDT.                                00107000
001080     MOVE MIC-DPS-3010-PRI TO MIC-MST-REC-PRI.                    00108000
001090     MOVE MI3010-RECORD TO MIMST-RECORD.                          00109000
001100     PERFORM MIC-MST-GET-EQUAL-REQUEST.                           00110000
001110     GO TO MB-MIM3010LOOP1.                                       00111000
001120 MB-MIM3010LOOP.                                                  00112000
001130     PERFORM MIC-MST-GET-NEXT-REQUEST.                            00113000
001140 MB-MIM3010LOOP1.                                                 00114000
001150     IF WS-FUNCTION IS EQUAL TO 'E'                               00115000
001160         GO TO MB-EXIT.                                           00116000
001170     IF WS-FUNCTION IS NOT EQUAL TO SPACES                        00117000
001180         GO TO MB-ERROR.                                          00118000
001190     MOVE MIMST-RECORD TO MI3010-RECORD.                          00119000
001200     IF MIM-3010KINST IS NOT EQUAL TO LS-RGNMIM3010INST           00120000
001210         OR MIM-3010KREGION IS NOT EQUAL TO LS-RGNMIM3010NBR      00121000
001220         OR MIM-3010KAPPL IS NOT EQUAL TO LS-RGNMIM3010APPL       00122000
001230         GO TO MB-EXIT.                                           00123000
001240     IF MIM-3010KEFFDT IS GREATER THAN LS-RGNMIM3010EFFDT         00124000
001250         OR (MIM-3010KAPPL IS EQUAL TO WS-HOLDAPPL                00125000
001260         AND MIM-3010KCHRGPLAN IS EQUAL TO WS-HOLDPLAN)           00126000
001270         GO TO MB-MIM3010LOOP.                                    00127000
001280     MOVE MIM-3010KAPPL TO WS-HOLDAPPL.                           00128000
001290     MOVE MIM-3010KCHRGPLAN TO WS-HOLDPLAN.                       00129000
001300     ADD +1 TO WS-SUB1.                                           00130000
001310     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3010MAXSIZE              00131000
001320         GO TO MB-ERROR.                                          00132000
001330     MOVE MIM-3010DATA TO WS-RGNMIM3010 (WS-SUB1).                00133000
001340     MOVE MIM-3010KCHRGPLAN TO WS-RGNMIM3010K (WS-SUB1).          00134000
001350     GO TO MB-MIM3010LOOP.                                        00135000
001360 MB-ERROR.                                                        00136000
001370     MOVE 'Y' TO LS-RGNMIM3010ERROR.                              00137000
001380 MB-EXIT.                                                         00138000
001390     EXIT.                                                        00139000
001400******************************************************************00140000
001410 MIC-MST-API SECTION.                                             00141000
000000 COPY MIPMSTA.                                                    00142000
001430******************************************************************00143000
