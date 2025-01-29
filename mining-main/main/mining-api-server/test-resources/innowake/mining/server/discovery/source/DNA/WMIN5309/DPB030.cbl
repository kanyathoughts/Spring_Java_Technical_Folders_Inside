000010 IDENTIFICATION DIVISION.                                         00001000
000020 PROGRAM-ID.             DPB030.                                  00002000
000000 COPY DPW007.                                                     00003000
000040******************************************************************00004000
000050*    DPB030 -- ROUTINE WHICH BUILDS/LOADS MICM REGIONAL PRICING  *00005000
000060*              RECORDS 3013 FOR USE BY THE CALLING PROGRAM.      *00006000
000070*              THIS ROUTINE CONTAINS THE REGION 00 INFORMATION   *00007000
000080*                                                                *00008000
000090*    CALLING PARAMATERS:                                         *00009000
000100*        CALL 'DPB030' USING                                     *00010000
000110*            WS-RGNMIM3013AREAS                                  *00011000
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
000390 DATA DIVISION.                                                   00039000
000400 WORKING-STORAGE SECTION.                                         00040000
000410 01  WS-COPYRIGHT                PIC X(57)       VALUE            00041000
000420     'COPYRIGHT(C) 2017 INFOR. ALL RIGHTS RESERVED.            '. 00042000
000430 01  DEP-RLSELIT.                                                 00043000
000000 COPY DPWRLSE.                                                    00044000
000450 01  MIC-RLSELIT.                                                 00045000
000000 COPY MIWRLSE.                                                    00046000
000470 01  SRW105-AREAS.                                                00047000
000000 COPY SRW105.                                                     00048000
000490*01  BICRSRB.                                                     00049000
000000 COPY BICRSRB.                                                    00050000
000510*01  BICRVERB.                                                    00051000
000000 COPY BICRVERB.                                                   00052000
000530*01  BICRSTAT.                                                    00053000
000000 COPY BICRSTAT.                                                   00054000
000550*01  BICRDBS.                                                     00055000
000000 COPY BICRDBS.                                                    00056000
000570*01  BICRPSB.                                                     00057000
000000 COPY BICRPSB.                                                    00058000
000590*01  BICPBPRM.                                                    00059000
000000 COPY BICPBPRM.                                                   00060000
000610 01  WS-RGNMIM3013AREAS.                                          00061000
000000 COPY DPW030.                                                     00062000
000630 01  MIMST-RECORD.                                                00063000
000000 COPY MISMST.                                                     00064000
000650 01  MIWPRI-AREAS.                                                00065000
000000 COPY MIWPRI.                                                     00066000
000670 01  DPWPRI-AREAS.                                                00067000
000000 COPY DPWPRI.                                                     00068000
000690 01  MI3013-RECORD.                                               00069000
000000 COPY MIS3013.                                                    00070000
000710 01  MIWMAST-AREAS.                                               00071000
000000 COPY MIWMSTA.                                                    00072000
000730 01  WS-ABEND.                                                    00073000
000000 COPY SRW028.                                                     00074000
000750 01  WS-WORKAREAS.                                                00075000
000760     03  WS-SUB1                 PIC S9(04)      COMP             00076000
000770                                                 VALUE ZEROS.     00077000
000780     03  WS-SUB2                 PIC S9(04)      COMP             00078000
000790                                                 VALUE ZEROS.     00079000
000800     03  WS-FUNCTION             PIC X(01).                       00080000
000810     03  WS-MIMKEYHOLD.                                           00081000
000820         05  WS-HOLDAPPL         PIC 9(02).                       00082000
000830         05  WS-HOLDPARM         PIC 9(03).                       00083000
000840         05  WS-HOLDACCUM        PIC 9(02).                       00084000
000850     03  WS-MODELFOUND           PIC X(01).                       00085000
000860******************************************************************00086000
000870 LINKAGE SECTION.                                                 00087000
000880 01  LS-RGNMIM3013AREAS.                                          00088000
000000 COPY DPW031.                                                     00089000
000900******************************************************************00090000
000910 PROCEDURE DIVISION                                               00091000
000920         USING                                                    00092000
000930         LS-RGNMIM3013AREAS.                                      00093000
000940******************************************************************00094000
000950 MAIN-LOGIC SECTION.                                              00095000
000960 ML-START.                                                        00096000
000970     MOVE ZEROS TO WS-MIMKEYHOLD.                                 00097000
000980     MOVE SPACE TO LS-RGNMIM3013ERROR.                            00098000
000990     IF LS-RGNMIM3013FUNC IS EQUAL TO 'B'                         00099000
001000         PERFORM MIM-BUILD3013.                                   00100000
001010     IF LS-RGNMIM3013ERROR IS NOT EQUAL TO 'Y'                    00101000
001020         MOVE WS-RGNMIM3013DATA TO LS-RGNMIM3013DATA.             00102000
001030     GOBACK.                                                      00103000
001040 ML-EXIT.                                                         00104000
001050     EXIT.                                                        00105000
001060******************************************************************00106000
001070 MIM-BUILD3013 SECTION.                                           00107000
001080 MB-START.                                                        00108000
001090     MOVE LOW-VALUES TO WS-RGNMIM3013DATA.                        00109000
001100     MOVE ZEROS TO WS-SUB1.                                       00110000
001110 MB-INITPOINTERSLOOP.                                             00111000
001120     ADD +1 TO WS-SUB1.                                           00112000
001130     IF WS-SUB1 IS LESS THAN +1001                                00113000
001140         MOVE ZEROS TO WS-RGNMIM3013P (WS-SUB1)                   00114000
001150         GO TO MB-INITPOINTERSLOOP.                               00115000
001160     MOVE ZEROS TO WS-SUB1.                                       00116000
001170     MOVE SPACES TO MI3013-RECORD.                                00117000
001180     MOVE LS-RGNMIM3013INST TO MIM-3013KINST.                     00118000
001190     MOVE 3013 TO MIM-3013KREC.                                   00119000
001200     MOVE LS-RGNMIM3013NBR TO MIM-3013KREGION.                    00120000
001210     MOVE LS-RGNMIM3013APPL TO MIM-3013KAPPL.                     00121000
001220     MOVE ZEROS TO MIM-3013KPARM.                                 00122000
001230     MOVE ZEROS TO MIM-3013KACCUM.                                00123000
001240     MOVE ZEROS TO MIM-3013KEFFDT.                                00124000
001250     MOVE LS-RGNMIM3013MODEL TO MIM-3013KMODEL.                   00125000
001260     MOVE MIC-DPS-3013-PRI TO MIC-MST-REC-PRI.                    00126000
001270     MOVE MI3013-RECORD TO MIMST-RECORD.                          00127000
001280     PERFORM MIC-MST-GET-EQUAL-REQUEST.                           00128000
001290     GO TO MB-MIM3013LOOP1.                                       00129000
001300 MB-MIM3013LOOP.                                                  00130000
001310     PERFORM MIC-MST-GET-NEXT-REQUEST.                            00131000
001320 MB-MIM3013LOOP1.                                                 00132000
001330     IF WS-FUNCTION IS EQUAL TO 'E'                               00133000
001340         GO TO MB-EXIT.                                           00134000
001350     IF WS-FUNCTION IS NOT EQUAL TO SPACES                        00135000
001360         GO TO MB-ERROR.                                          00136000
001370     MOVE MIMST-RECORD TO MI3013-RECORD.                          00137000
001380     IF MIM-3013KINST IS NOT EQUAL TO LS-RGNMIM3013INST           00138000
001390         OR MIM-3013KREGION IS NOT EQUAL TO LS-RGNMIM3013NBR      00139000
001400         OR MIM-3013KAPPL IS NOT EQUAL TO LS-RGNMIM3013APPL       00140000
001410         GO TO MB-EXIT.                                           00141000
001420     IF MIM-3013KEFFDT IS GREATER THAN LS-RGNMIM3013EFFDT         00142000
001430         GO TO MB-MIM3013LOOP.                                    00143000
001440     IF LS-RGNMIM3013MODEL IS EQUAL TO SPACE                      00144000
001450         AND (MIM-3013KMODEL IS NOT EQUAL TO LS-RGNMIM3013MODEL   00145000
001460         OR (MIM-3013KAPPL IS EQUAL TO WS-HOLDAPPL                00146000
001470         AND MIM-3013KPARM IS EQUAL TO WS-HOLDPARM                00147000
001480         AND MIM-3013KACCUM IS EQUAL TO WS-HOLDACCUM))            00148000
001490         GO TO MB-MIM3013LOOP.                                    00149000
001500     IF MIM-3013KAPPL IS NOT EQUAL TO WS-HOLDAPPL                 00150000
001510         OR MIM-3013KPARM IS NOT EQUAL TO WS-HOLDPARM             00151000
001520         OR MIM-3013KACCUM IS NOT EQUAL TO WS-HOLDACCUM           00152000
001530         MOVE MIM-3013KAPPL TO WS-HOLDAPPL                        00153000
001540         MOVE MIM-3013KPARM TO WS-HOLDPARM                        00154000
001550         MOVE MIM-3013KACCUM TO WS-HOLDACCUM                      00155000
001560         MOVE SPACE TO WS-MODELFOUND.                             00156000
001570 MB-CHECKMODEL.                                                   00157000
001580**THIS PARAGRAPH KEEPS THE ROUTINE FROM OVERLAYING A CURRENT      00158000
001590**RECORD WITH PAST-DATED RECORD WHILE LOOKING FOR A MODEL RECORD. 00159000
001600     IF LS-RGNMIM3013MODEL IS NOT EQUAL TO 'M'                    00160000
001610         GO TO MB-3013MODEL.                                      00161000
001620     MOVE WS-RGNMIM3013P (MIM-3013KPARM) TO WS-SUB2.              00162000
001630     IF WS-SUB2 IS EQUAL TO ZEROS                                 00163000
001640         GO TO MB-3013MODEL.                                      00164000
001650     IF WS-RGNMIM3013A (WS-SUB2 MIM-3013KACCUM)                   00165000
001660             IS NOT EQUAL TO LOW-VALUES                           00166000
001670         AND MIM-3013KMODEL IS EQUAL TO SPACE                     00167000
001680         GO TO MB-MIM3013LOOP.                                    00168000
001690 MB-3013MODEL.                                                    00169000
001700**THIS PARAGRAPH IS USED TO BYPASS LOADING OF ANY SUBSEQUENT      00170000
001710**PAST-DATED RECORDS ONCE A MODEL RECORD HAS BEEN LOADED.         00171000
001720     IF LS-RGNMIM3013MODEL IS EQUAL TO 'M'                        00172000
001730         AND WS-MODELFOUND IS NOT EQUAL TO SPACE                  00173000
001740         GO TO MB-MIM3013LOOP.                                    00174000
001750     IF MIM-3013KMODEL IS EQUAL TO 'M'                            00175000
001760         MOVE 'Y' TO WS-MODELFOUND.                               00176000
001770     IF (WS-RGNMIM3013P (MIM-3013KPARM) IS EQUAL TO ZERO          00177000
001780         AND MIM-3013KPARM IS NOT EQUAL TO ZERO)                  00178000
001790         OR (WS-RGNMIM3013P (1000) IS EQUAL TO ZERO               00179000
001800         AND MIM-3013KPARM IS EQUAL TO ZERO)                      00180000
001810         ADD +1 TO WS-SUB1.                                       00181000
001820     IF WS-SUB1 IS GREATER THAN WS-RGNMIM3013MAXSIZE              00182000
001830         GO TO MB-ERROR.                                          00183000
001840     MOVE MIM-3013DATA TO WS-RGNMIM3013A (WS-SUB1 MIM-3013KACCUM).00184000
001850     IF MIM-3013KPARM IS EQUAL TO ZEROS                           00185000
001860         MOVE WS-SUB1 TO WS-RGNMIM3013P (1000)                    00186000
001870     ELSE                                                         00187000
001880         MOVE WS-SUB1 TO WS-RGNMIM3013P (MIM-3013KPARM).          00188000
001890     GO TO MB-MIM3013LOOP.                                        00189000
001900 MB-ERROR.                                                        00190000
001910     MOVE 'Y' TO LS-RGNMIM3013ERROR.                              00191000
001920 MB-EXIT.                                                         00192000
001930     EXIT.                                                        00193000
001940******************************************************************00194000
001950 MIC-MST-API SECTION.                                             00195000
000000 COPY MIPMSTA.                                                    00196000
001970******************************************************************00197000
