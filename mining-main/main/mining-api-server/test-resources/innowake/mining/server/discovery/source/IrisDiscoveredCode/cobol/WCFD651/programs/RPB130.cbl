000010 IDENTIFICATION DIVISION.                                         00001000
000020 PROGRAM-ID.             RPB130.                                  00002000
000000***************************************************************   00003000
000000*                                                             *   00003001
000000*                           NOTICE                            *   00003002
000000*                                                             *   00003003
000000*   THIS SOFTWARE IS THE PROPERTY OF AND CONTAINS             *   00003004
000000*   CONFIDENTIAL INFORMATION OF INFOR AND/OR ITS AFFILIATES   *   00003005
000000*   OR SUBSIDIARIES AND SHALL NOT BE DISCLOSED WITHOUT PRIOR  *   00003006
000000*   WRITTEN PERMISSION. LICENSED CUSTOMERS MAY COPY AND       *   00003007
000000*   ADAPT THIS SOFTWARE FOR THEIR OWN USE IN ACCORDANCE WITH  *   00003008
000000*   THE TERMS OF THEIR SOFTWARE LICENSE AGREEMENT.            *   00003009
000000*   ALL OTHER RIGHTS RESERVED.                                *   00003010
000000*                                                             *   00003011
000000*   (C) COPYRIGHT 2017 INFOR.  ALL RIGHTS RESERVED.           *   00003012
000000*   THE WORD AND DESIGN MARKS SET FORTH HEREIN ARE            *   00003013
000000*   TRADEMARKS AND/OR REGISTERED TRADEMARKS OF INFOR          *   00003014
000000*   AND/OR ITS AFFILIATES AND SUBSIDIARIES. ALL RIGHTS        *   00003015
000000*   RESERVED.  ALL OTHER TRADEMARKS LISTED HEREIN ARE         *   00003016
000000*   THE PROPERTY OF THEIR RESPECTIVE OWNERS.                  *   00003017
000000*                                                             *   00003018
000000***************************************************************   00003019
000000*     Infopoint Relationship Pricing                          *   00003020
000000*     RP 5.0.01                                               *   00003021
000000***************************************************************   00003022
000040*----------------------------------------------------------------*00004000
000050*    RPB130 - USER MODIFIABLE ACCOUNT KEY VERIFICATION ROUTINE   *00005000
000060*    AND REFORMATER USED BY INFOPOINT APPLICATIONS.              *00006000
000070*----------------------------------------------------------------*00007000
000080 ENVIRONMENT DIVISION.                                            00008000
000090 DATA DIVISION.                                                   00009000
000100******************************************************************00010000
000110 WORKING-STORAGE SECTION.                                         00011000
000000 01  WS-COPYRIGHT                PIC X(057)      VALUE            00012000
000000     'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. WWW.INFOR.COM '. 00012001
000130 01  RPS-RLSELIT.                                                 00013000
000000 COPY RPWRLSE.                                                    00014000
000150 01  WORK-AREAS.                                                  00015000
000160     03  WS-FUNCTION             PIC X(01)       VALUE SPACES.    00016000
000170     03  WS-LEADING              PIC S9(04)      COMP.            00017000
000180     03  WS-NOTSIGDIGITS         PIC S9(04)      COMP.            00018000
000190     03  WS-FOUNDZEROS           PIC X(01).                       00019000
000200******************************************************************00020000
000210 01  WS-USEROPTIONS.                                              00021000
000220     03  FILLER                  PIC X(08)       VALUE '*USROPT*'.00022000
000230*    CHANGE THE FOLLOWING VALUE TO 'Y' IF ALPHA KEYS ARE TO BE    00023000
000240*    ZERO FILLED.                                                 00024000
000250     03  WS-ALPHAZEROFILL        PIC X(01)       VALUE 'N'.       00025000
000260*    CHANGE THE FOLLOWING VALUE TO 'N' IF ALPHA KEYS ARE NOT TO BE00026000
000270*    DISPLAYED WITH LEADING ZEROES.                               00027000
000280     03  WS-ALPHAZEROSHOW        PIC X(01)       VALUE 'Y'.       00028000
000290******************************************************************00029000
000300 01  SRW025-AREAS.                                                00030000
000000 COPY SRW025.                                                     00031000
000320 01  SRW166-AREAS.                                                00032000
000000 COPY SRW166.                                                     00033000
000340 01  RPW033-AREAS.                                                00034000
000000 COPY RPW033.                                                     00035000
000360******************************************************************00036000
000370 LINKAGE SECTION.                                                 00037000
000380 01  RPW130-AREAS.                                                00038000
000000 COPY RPW130.                                                     00039000
000400 01  LS-FUNCTION                 PIC X(01).                       00040000
000410******************************************************************00041000
000420 PROCEDURE DIVISION                                               00042000
000430         USING                                                    00043000
000440         RPW130-AREAS                                             00044000
000450         LS-FUNCTION.                                             00045000
000460******************************************************************00046000
000470 MAIN-LOGIC SECTION.                                              00047000
000480 ML-START.                                                        00048000
000490     MOVE SPACES TO LS-FUNCTION.                                  00049000
000500     MOVE SPACES TO RPW130-FORMATTEDACCT.                         00050000
000510     IF RPW130-FUNCTION IS EQUAL TO 'D'                           00051000
000520         GO TO ML-OUTPUTFORMAT.                                   00052000
000530     IF RPW130-FUNCTION IS EQUAL TO 'K'                           00053000
000540         PERFORM BUILD-FILEKEY                                    00054000
000550         GO TO ML-OUTPUTFORMAT.                                   00055000
000560     IF RPW130-FUNCTION IS NOT EQUAL TO 'V'                       00056000
000570         MOVE 'I' TO LS-FUNCTION                                  00057000
000580         GO TO ML-EXIT.                                           00058000
000590     IF RPW130-2023ACCTALPHA IS EQUAL TO 'Y'                      00059000
000600         PERFORM VERIFY-ACCT-ALPHA                                00060000
000610     ELSE                                                         00061000
000620         PERFORM VERIFY-ACCT-NUMERIC.                             00062000
000630     IF LS-FUNCTION IS NOT EQUAL TO SPACE                         00063000
000640         MOVE RPW130-ACCOUNTINPUT TO RPW130-FORMATTEDACCT         00064000
000650         GO TO ML-EXIT.                                           00065000
000660 ML-OUTPUTFORMAT.                                                 00066000
000670     MOVE SPACE TO LS-FUNCTION.                                   00067000
000680     IF RPW130-2023ACCTLN IS NOT NUMERIC                          00068000
000690         OR RPW130-2023ACCTLN IS EQUAL TO ZERO                    00069000
000700         OR RPW130-2023ACCTLN IS GREATER THAN 18                  00070000
000710         MOVE RPW130-ACCOUNTKEY TO RPW130-FORMATTEDACCT           00071000
000720         GO TO ML-EXIT.                                           00072000
000730     MOVE RPW130-ACCOUNTKEY TO RPW033-ACCOUNT.                    00073000
000740     MOVE RPW130-2023ACCTLN TO RPW033-ACCTLN.                     00074000
000750     IF RPW130-2023ACCTED IS NUMERIC                              00075000
000760         MOVE RPW130-2023ACCTED TO RPW033-ACCTED                  00076000
000770     ELSE                                                         00077000
000780         MOVE ZERO TO RPW033-ACCTED.                              00078000
000790     PERFORM FORMAT-ACCOUNT-LEFTJUST.                             00079000
000800     MOVE RPW033-FORMATTEDACCT TO RPW130-FORMATTEDACCT.           00080000
000810 ML-EXIT.                                                         00081000
000820     GOBACK.                                                      00082000
000830******************************************************************00083000
000840 BUILD-FILEKEY SECTION.                                           00084000
000850 BFK-START.                                                       00085000
000860     COMPUTE WS-LEADING = 18 - RPW130-2023ACCTLN.                 00086000
000870     MOVE +18 TO SR-JLGTH.                                        00087000
000880     MOVE RPW130-ACCOUNTINPUT TO SR-JUSTIFYIN.                    00088000
000890     PERFORM RIGHT-JUSTIFY.                                       00089000
000900 BFK-CHECKDIGIT.                                                  00090000
000910     IF RPW130-2023ACCTALPHA IS NOT EQUAL TO 'Y'                  00091000
000920         GO TO BFK-MOVE.                                          00092000
000930     MOVE SR-JUSTIFYOUT TO SR-JUSTIFYIN.                          00093000
000940     MOVE +18 TO SR-JLGTH.                                        00094000
000950     PERFORM LEFT-JUSTIFY.                                        00095000
000960 BFK-MOVE.                                                        00096000
000970     MOVE SR-JUSTIFYOUT TO RPW130-ACCOUNTKEY.                     00097000
000980 BFK-EXIT.                                                        00098000
000990     EXIT.                                                        00099000
001000******************************************************************00100000
001010 VERIFY-ACCT-NUMERIC SECTION.                                     00101000
001020 VAN-START.                                                       00102000
001030     MOVE +0 TO WS-NOTSIGDIGITS.                                  00103000
001040     COMPUTE WS-LEADING = 18 - RPW130-2023ACCTLN.                 00104000
001050     MOVE +18 TO SR-JLGTH.                                        00105000
001060     MOVE RPW130-ACCOUNTINPUT TO SR-JUSTIFYIN.                    00106000
001070     PERFORM RIGHT-JUSTIFY.                                       00107000
001080     MOVE +0 TO SR-JOSUB.                                         00108000
001090 VAN-CHECKCONTENTS.                                               00109000
001100     ADD +1 TO SR-JOSUB.                                          00110000
001110     IF SR-JOSUB IS GREATER THAN +18                              00111000
001120         GO TO VAN-CHECKDIGIT.                                    00112000
001130     IF SR-JOUT (SR-JOSUB) IS NOT EQUAL TO '0'                    00113000
001140         AND SR-JOSUB IS NOT GREATER THAN WS-LEADING              00114000
001150         MOVE 'L' TO LS-FUNCTION                                  00115000
001160         GO TO VAN-EXIT.                                          00116000
001170     IF SR-JOUT (SR-JOSUB) IS EQUAL TO '0'                        00117000
001180         ADD +1 TO WS-NOTSIGDIGITS.                               00118000
001190     IF SR-JOUT (SR-JOSUB) IS NUMERIC                             00119000
001200         GO TO VAN-CHECKCONTENTS.                                 00120000
001210     MOVE 'N' TO LS-FUNCTION.                                     00121000
001220     GO TO VAN-EXIT.                                              00122000
001230 VAN-CHECKDIGIT.                                                  00123000
001240     IF WS-NOTSIGDIGITS IS GREATER THAN +17                       00124000
001250         MOVE 'Z' TO LS-FUNCTION                                  00125000
001260         GO TO VAN-EXIT.                                          00126000
001270     MOVE SR-JUSTIFYOUT TO RPW130-ACCOUNTKEY.                     00127000
001280 VAN-EXIT.                                                        00128000
001290     EXIT.                                                        00129000
001300******************************************************************00130000
001310 VERIFY-ACCT-ALPHA SECTION.                                       00131000
001320 VAA-START.                                                       00132000
001330     MOVE +0 TO WS-NOTSIGDIGITS.                                  00133000
001340     MOVE SPACES TO WS-FOUNDZEROS.                                00134000
001350     COMPUTE WS-LEADING = 18 - RPW130-2023ACCTLN.                 00135000
001360     MOVE +18 TO SR-JLGTH.                                        00136000
001370     MOVE RPW130-ACCOUNTINPUT TO SR-JUSTIFYIN.                    00137000
001380     PERFORM RIGHT-JUSTIFY.                                       00138000
001390     MOVE +0 TO SR-JOSUB.                                         00139000
001400 VAA-CHECKCONTENTS.                                               00140000
001410     ADD +1 TO SR-JOSUB.                                          00141000
001420     IF SR-JOSUB IS GREATER THAN +18                              00142000
001430         GO TO VAA-CHECKDIGIT.                                    00143000
001440     IF SR-JOUT (SR-JOSUB) IS NOT EQUAL TO '0'                    00144000
001450         AND SR-JOUT (SR-JOSUB) IS NOT EQUAL TO ' '               00145000
001460         AND SR-JOSUB IS NOT GREATER THAN WS-LEADING              00146000
001470         MOVE 'L' TO LS-FUNCTION                                  00147000
001480         GO TO VAA-EXIT.                                          00148000
001490     IF SR-JOUT (SR-JOSUB) IS EQUAL TO '0'                        00149000
001500         MOVE 'Y' TO WS-FOUNDZEROS.                               00150000
001510     IF SR-JOUT (SR-JOSUB) IS EQUAL TO SPACE                      00151000
001520         AND WS-FOUNDZEROS IS EQUAL TO 'Y'                        00152000
001530         MOVE 'N' TO LS-FUNCTION                                  00153000
001540         GO TO VAA-EXIT.                                          00154000
001550     IF SR-JOUT (SR-JOSUB) IS EQUAL TO '0'                        00155000
001560         OR SR-JOUT (SR-JOSUB) IS EQUAL TO SPACE                  00156000
001570         ADD +1 TO WS-NOTSIGDIGITS.                               00157000
001580     IF SR-JOUT (SR-JOSUB) IS NUMERIC                             00158000
001590         OR SR-JOUT (SR-JOSUB) IS ALPHABETIC                      00159000
001600         GO TO VAA-CHECKCONTENTS.                                 00160000
001610     MOVE 'N' TO LS-FUNCTION.                                     00161000
001620     GO TO VAA-EXIT.                                              00162000
001630 VAA-CHECKDIGIT.                                                  00163000
001640     IF WS-NOTSIGDIGITS IS GREATER THAN +17                       00164000
001650         MOVE 'Z' TO LS-FUNCTION                                  00165000
001660         GO TO VAA-EXIT.                                          00166000
001670     MOVE SR-JUSTIFYOUT TO SR-JUSTIFYIN.                          00167000
001680     MOVE +18 TO SR-JLGTH.                                        00168000
001690     PERFORM LEFT-JUSTIFY.                                        00169000
001700     MOVE SR-JUSTIFYOUT TO RPW130-ACCOUNTKEY.                     00170000
001710 VAA-EXIT.                                                        00171000
001720     EXIT.                                                        00172000
001730******************************************************************00173000
001740 LEFT-JUSTIFY SECTION.                                            00174000
001750 LJ-START.                                                        00175000
001760     MOVE SPACES TO SR-JUSTIFYOUT.                                00176000
001770     MOVE +1 TO SR-JISUB.                                         00177000
001780     MOVE +1 TO SR-JOSUB.                                         00178000
001790 LJ-SUPPRESS.                                                     00179000
001800     IF SR-JISUB IS GREATER THAN SR-JLGTH                         00180000
001810         GO TO LJ-EXIT.                                           00181000
001820     IF SR-JIN (SR-JISUB) IS EQUAL TO SPACE                       00182000
001830         ADD +1 TO SR-JISUB                                       00183000
001840         GO TO LJ-SUPPRESS.                                       00184000
001850     IF SR-JIN (SR-JISUB) IS NOT EQUAL TO '0'                     00185000
001860         OR RPW130-2023ACCTALPHA IS NOT EQUAL TO 'Y'              00186000
001870         GO TO LJ-MOVE.                                           00187000
001880     IF WS-ALPHAZEROSHOW IS NOT EQUAL TO 'Y'                      00188000
001890         ADD +1 TO SR-JISUB                                       00189000
001900         GO TO LJ-SUPPRESS.                                       00190000
001910 LJ-MOVE.                                                         00191000
001920     MOVE SR-JIN (SR-JISUB) TO SR-JOUT (SR-JOSUB).                00192000
001930     ADD +1 TO SR-JISUB.                                          00193000
001940     ADD +1 TO SR-JOSUB.                                          00194000
001950     IF SR-JISUB IS NOT GREATER THAN SR-JLGTH                     00195000
001960         GO TO LJ-MOVE.                                           00196000
001970 LJ-EXIT.                                                         00197000
001980     EXIT.                                                        00198000
001990******************************************************************00199000
002000 RIGHT-JUSTIFY SECTION.                                           00200000
002010 RJ-START.                                                        00201000
002020     MOVE SPACES TO SR-JUSTIFYOUT.                                00202000
002030* EXAMINE FIELD ALL LOW-VALUES BY SPACES.                         00203000
002040     MOVE SR-JUSTIFYIN TO SRW166-FIELD.                           00204000
002050     MOVE +18 TO SRW166-LGTH.                                     00205000
002060     MOVE LOW-VALUE TO SRW166-IN.                                 00206000
002070     MOVE SPACE TO SRW166-OUT.                                    00207000
002080     PERFORM EXAMINE-FIELD.                                       00208000
002090     MOVE SRW166-FIELD TO SR-JUSTIFYIN.                           00209000
002100                                                                  00210000
002110* EXAMINE FIELD REPLACING LEADING SPACES BY ZERO.                 00211000
002120     MOVE SR-JUSTIFYIN TO SRW166-FIELD.                           00212000
002130     MOVE +18 TO SRW166-LGTH.                                     00213000
002140     MOVE 'X' TO SRW166-LEAD.                                     00214000
002150     MOVE SPACE TO SRW166-IN.                                     00215000
002160     MOVE ZERO TO SRW166-OUT.                                     00216000
002170     PERFORM EXAMINE-FIELD.                                       00217000
002180     MOVE SRW166-FIELD TO SR-JUSTIFYIN.                           00218000
002190                                                                  00219000
002200     MOVE +18 TO SR-JISUB.                                        00220000
002210     MOVE SR-JLGTH TO SR-JOSUB.                                   00221000
002220     IF RPW130-2023ACCTALPHA IS NOT EQUAL TO 'Y'                  00222000
002230         OR WS-ALPHAZEROFILL IS EQUAL TO 'Y'                      00223000
002240         MOVE ALL '0' TO SR-JUSTIFYOUT.                           00224000
002250 RJ-FIND.                                                         00225000
002260     IF SR-JISUB IS LESS THAN +1                                  00226000
002270         GO TO RJ-LEADING.                                        00227000
002280     IF SR-JIN (SR-JISUB) IS EQUAL TO SPACES                      00228000
002290         SUBTRACT +1 FROM SR-JISUB                                00229000
002300         GO TO RJ-FIND.                                           00230000
002310 RJ-MOVE.                                                         00231000
002320     MOVE SR-JIN (SR-JISUB) TO SR-JOUT (SR-JOSUB).                00232000
002330     SUBTRACT +1 FROM SR-JISUB.                                   00233000
002340     SUBTRACT +1 FROM SR-JOSUB.                                   00234000
002350     IF SR-JISUB IS GREATER THAN +0                               00235000
002360         AND SR-JOSUB IS GREATER THAN +0                          00236000
002370         GO TO RJ-MOVE.                                           00237000
002380 RJ-LEADING.                                                      00238000
002390     IF RPW130-2023ACCTALPHA IS NOT EQUAL TO 'Y'                  00239000
002400         OR WS-ALPHAZEROFILL IS EQUAL TO 'Y'                      00240000
002410         GO TO RJ-EXIT.                                           00241000
002420                                                                  00242000
002430* EXAMINE FIELD REPLACING LEADING ZERO BY SPACES.                 00243000
002440     MOVE SR-JUSTIFYIN TO SRW166-FIELD.                           00244000
002450     MOVE +18 TO SRW166-LGTH.                                     00245000
002460     MOVE 'X' TO SRW166-LEAD.                                     00246000
002470     MOVE ZERO TO SRW166-IN.                                      00247000
002480     MOVE SPACE TO SRW166-OUT.                                    00248000
002490     PERFORM EXAMINE-FIELD.                                       00249000
002500     MOVE SRW166-FIELD TO SR-JUSTIFYIN.                           00250000
002510 RJ-EXIT.                                                         00251000
002520     EXIT.                                                        00252000
002530******************************************************************00253000
002540 EXAMINE-FIELD SECTION.                                           00254000
000000 COPY SRP166.                                                     00255000
002560******************************************************************00256000
002570 FORMAT-ACCOUNT-LEFTJUST SECTION.                                 00257000
000000 COPY RPP033.                                                     00258000
002590******************************************************************00259000
