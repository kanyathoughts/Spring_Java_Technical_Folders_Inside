#FIS******************************************************************* 00000001
#FIS***                                                               * 00000002
#FIS***                   COPYRIGHT 1993-2019                         * 00000003
#FIS***                                                               * 00000004
#FIS***      Fidelity National Information Services, Inc.             * 00000005
#FIS***      and/or its subsidiaries - All Rights Reserved            * 00000006
#FIS***      worldwide.                                               * 00000007
#FIS***                                                               * 00000008
#FIS***      This document is protected under the trade secret        * 00000009
#FIS***      and copyright laws as the property of Fidelity           * 00000010
#FIS***      National Information Services, Inc. and/or its           * 00000011
#FIS***      subsidiaries.                                            * 00000012
#FIS***                                                               * 00000013
#FIS***      Copying, reproduction or distribution should be          * 00000014
#FIS***      limited and only to employees with a "need to know"      * 00000015
#FIS***      to do their job. Any disclosure of this document to      * 00000016
#FIS***      third parties is strictly prohibited.                    * 00000017
#FIS***                                                               * 00000018
#FIS******************************************************************* 00000019
       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID. 'BABKCMP'.                                           00000200
      ******************************************************************00000300
      **  APPLICATION  SUNGARD EBS OMNIPLUS                             00000400
      ******************************************************************00000500
      **  AUTHOR       W. WHITE, Jul 2004                               00000600
      **  PURPOSE      Compare OmniPlus Backups                         00000700
      ******************************************************************00000800
      **  REVISIONS:                                                    00000900
      ******************************************************************00001000
      **  Input                                                         00001100
      **   . BKUPI   - Reference Backup File                            00001200
      **   . BKUPI2  - Referant  Backup File                            00001300
660014**   . IN110   - Control File                                     00001301
      ******************************************************************00001500
      **  Output                                                        00001600
      **   . SYSOUT  - Displayed Information                            00001700
      **   . OT200   - File of differences                              00001800
      **   . REPORT  - Report summarizing processing                    00001900
      ******************************************************************00002000
       ENVIRONMENT DIVISION.                                            00002100
       CONFIGURATION SECTION.                                           00002200
       INPUT-OUTPUT SECTION.                                            00002300
           SKIP2                                                        00002400
       FILE-CONTROL.                                                    00002500
           SELECT PRINT-FILE                                            00002600
               ASSIGN TO UT-S-REPORT                                    00002700
&MFCY          ORGANIZATION IS LINE SEQUENTIAL                          00002800
               FILE STATUS WPRINT-FS.                                   00002900
       DATA DIVISION.                                                   00003000
           SKIP2                                                        00003100
       FILE SECTION.                                                    00003200
           SKIP1                                                        00003300
       FD  PRINT-FILE                                                   00003400
           BLOCK CONTAINS 0 RECORDS                                     00003500
           LABEL RECORDS ARE STANDARD.                                  00003600
           SKIP2                                                        00003700
       01 PRINT-LINE                PIC X(133).                         00003800
       WORKING-STORAGE SECTION.                                         00003900
       77  WQUIT-FLG             PIC X            VALUE 'N'.            00004000
           88  WQUIT-FLG-YES VALUE 'Y'.                                 00004100
           SKIP2                                                        00004200
       77  WK-PGM-NAME           PIC X(8)     VALUE SPACES.             00004300
       77  WK-LAB                PIC XX       VALUE SPACES.             00004400
       77  SUB-STRECS            PIC S9(4)    VALUE ZERO.               00004500
WFIX   77  SUB-CHAR              PIC S9(4)    VALUE ZERO.               00004600
WFIX   77  SUB-START             PIC S9(4)    VALUE ZERO.               00004700
       77  WDUMP-FLG             PIC X        VALUE SPACES.             00004800
           88  WDUMP-FLG-REC VALUE ' '.                                 00004900
           88  WDUMP-FLG-REC2 VALUE '2'.                                00005000
       77  WPREV-XXYY                PIC X(4)  VALUE SPACES.            00005100
       77  WDISP-XXYY                PIC X(4)  VALUE SPACES.            00005200
       77  WDISP-DATE                PIC 99B99B99B99 VALUE SPACES.      00005300
       77  WDISP-TIME                PIC 99B99B99B99 VALUE SPACES.      00005400
       01  WPRINT-AREA.                                                 00005500
           05  WPRINT-FS                 PIC XX  VALUE '00'.            00005600
               88  WPRINT-FS-OK VALUE '00'.                             00005700
           05  WPRINT-PAGE               PIC 99999  VALUE ZERO.         00005800
           05  WPRINT-TITLE1             PIC X(133) VALUE SPACES.       00005900
           05  WPRINT-TITLE2             PIC X(133) VALUE SPACES.       00006000
           05  WPRINT-LINE               PIC X(133) VALUE SPACES.       00006100
           05  WPRINT-LINES              PIC S9(5)  VALUE ZERO.         00006200
               88  WPRINT-LINES-OVER VALUE 51 THRU 1000.                00006300
           05  WPRINT-BLANK-LINES        PIC 9      VALUE ZERO.         00006400
           05  WPRINT-TOT-LINES          PIC S9(5)  VALUE ZERO.         00006500
           SKIP2                                                        00006600
       01  FILLER.                                                      00006700
           05  WDISP-1            PIC ZZZ,ZZZ,ZZ9  VALUE ZERO.          00006800
           05  WDISP-2            PIC ZZZ,ZZZ,ZZ9  VALUE ZERO.          00006900
           05  WDISP-3            PIC ZZZ,ZZZ,ZZ9  VALUE ZERO.          00007000
           05  WDISP-4            PIC ZZZ,ZZZ,ZZ9  VALUE ZERO.          00007100
           05  WDISP-5            PIC ZZZ,ZZZ,ZZ9  VALUE ZERO.          00007200
           05  WDISP-6            PIC ZZZ,ZZZ,ZZ9  VALUE ZERO.          00007300
           05  WDISP-7            PIC ZZZ,ZZZ,ZZ9  VALUE ZERO.          00007400
           05  WDISP-A-1          PIC ZZ,ZZ9  VALUE ZERO.               00007500
           05  WDISP-POS          PIC ZZZ9     VALUE ZERO.              00007600
           SKIP2                                                        00007700
       01  WCTR-AREA.                                                   00007800
           05  WCTR-CYCLE         PIC 9(9) COMP-3 VALUE ZERO.           00007900
           05  WCTR-BKPI          PIC 9(9) COMP-3 VALUE ZERO.           00008000
           05  WCTR-BKPI2         PIC 9(9) COMP-3 VALUE ZERO.           00008100
           05  WCTR-PLANS         PIC 9(5) COMP-3 VALUE ZERO.           00008200
WFIX       05  WCTR-DIFF-CHARS    PIC 9(5) COMP-3 VALUE ZERO.           00008300
WFIX       05  WCTR-DEDIFFS       PIC 9(5) COMP-3 VALUE ZERO.           00008400
           SKIP2                                                        00008500
       01  WTEMP-SHOW.                                                  00008600
           05  WTEMP-VAL            PIC X(4) VALUE SPACES.              00008700
           05  WTEMP-VAL-SEP        PIC X    VALUE SPACES.              00008800
           05  WTEMP-VAL-NAME       PIC X(35) VALUE SPACES.             00008900
           SKIP2                                                        00009000
       01  WPREVTX-AREA.                                                00009100
           05  WPREVTX-FILE-NAME    PIC X(50) VALUE SPACES.             00009200
           05  WPREVTX-BKPI-CYCLE   PIC S9(9) COMP-3 VALUE ZERO.        00009300
           05  WPREVTX-BKPI2-CYCLE   PIC S9(9) COMP-3 VALUE ZERO.       00009400
           05  WPREVTX-MATCH-CYCLE   PIC S9(9) COMP-3 VALUE ZERO.       00009500
           SKIP2                                                        00009600
       01  FILLER.                                                      00009700
           05  SUB-DE                PIC S9(4) BINARY VALUE ZERO.       00009800
           05  SUB-SAVE              PIC S9(4) BINARY VALUE ZERO.       00009900
           SKIP2                                                        00010000
       01  WSHOW-LINE.                                                  00010100
           05  FILLER          PIC X VALUE SPACES.                      00010200
           05  WSHOW-DE.                                                00010300
               10  WSHOW-DE-PREFIX   PIC X(5) VALUE SPACES.             00010400
                   88  WSHOW-DE-PREFIX-KEY   VALUE 'KeyDE'.             00010500
                   88  WSHOW-DE-PREFIX-DATA  VALUE '   DE'.             00010600
               10  WSHOW-DE-NUM      PIC 999 VALUE ZERO.                00010700
           05  FILLER          PIC X VALUE SPACES.                      00010800
           05  WSHOW-DE-NAME   PIC X(20)  VALUE SPACES.                 00010900
           05  FILLER          PIC X VALUE SPACES.                      00011000
           05  WSHOW-VALUES.                                            00011100
               10  WSHOW-VAL1      PIC X(40)  VALUE SPACES.             00011200
               10  FILLER          PIC X      VALUE SPACES.             00011300
               10  WSHOW-VAL2      PIC X(40)  VALUE SPACES.             00011400
24828S     05  FILLER          PIC X(91)  VALUE SPACES.                 00011401
       01  FILLER REDEFINES WSHOW-LINE.                                 00011500
20992S     05  WSHOW-TEXT      PIC X(200).                              00011501
           SKIP2                                                        00011700
           SKIP2                                                        00011800
       01  WRECHDR-LINE.                                                00011900
           05  FILLER           PIC X(13)  VALUE '<RECORD type='.       00012000
           05  WRECHDR-XXYY     PIC X(4)   VALUE SPACES.                00012100
           05  FILLER           PIC XX     VALUE '>'.                   00012200
           05  WRECHDR-MSG.                                             00012300
               10  FILLER           PIC X(12)  VALUE SPACES.            00012400
               10  WRECHDR-HDG1     PIC X(10)  VALUE SPACES.            00012500
               10  WRECHDR-RECNUM   PIC ZZZZ,ZZZ,ZZ9 VALUE SPACES.      00012600
               10  FILLER           PIC X(18)  VALUE SPACES.            00012700
               10  FILLER           PIC X      VALUE SPACES.            00012800
               10  WRECHDR-HDG2     PIC X(12)  VALUE SPACES.            00012900
               10  WRECHDR-RECNUM2  PIC ZZZZ,ZZZ,ZZ9 VALUE SPACES.      00013000
               10  FILLER           PIC X(8)   VALUE SPACES.            00013100
           SKIP2                                                        00013200
       01  WCTL-AREA.                                                   00013300
           05  WCTL-CMD                PIC X(20) VALUE SPACES.          00013400
               88  WCTL-CMD-RECTRACE   VALUE 'RECTRACE'.                00013500
               88  WCTL-CMD-TRACE      VALUE 'TRACE'.                   00013600
               88  WCTL-CMD-BRSEQ      VALUE 'BRSEQ'.                   00013700
               88  WCTL-CMD-ALLDES     VALUE 'ALLDES'.                  00013800
               88  WCTL-CMD-NORUNDATE  VALUE 'NORUNDATE'.               00013900
W              88  WCTL-CMD-DEDETAIL   VALUE 'DEDETAIL'.                00014000
W              88  WCTL-CMD-NODEDETAIL VALUE 'NODEDETAIL'.              00014100
           05  WCTL-VAL                PIC X(20) VALUE SPACES.          00014200
           05  WCTL-SHOW-EXTRA-REC-DES PIC X VALUE SPACES.              00014300
               88  WCTL-SHOW-EXTRA-REC-DES-YES VALUE 'Y' 'y'.           00014400
           05  WCTL-MAX-DIFFS          PIC 9(8) VALUE ZERO.             00014500
           05  WCTL-SEL-PLAN           PIC X(6) VALUE SPACES.           00014600
           05  WCTL-REC-TRACE          PIC X    VALUE 'N'.              00014700
               88  WCTL-REC-TRACE-YES VALUE 'Y' 'y'.                    00014800
           05  WCTL-TRACE              PIC X    VALUE 'N'.              00014900
               88  WCTL-TRACE-YES VALUE 'Y' 'y'.                        00015000
           05  WCTL-BRSEQ              PIC X    VALUE 'N'.              00015100
               88  WCTL-BRSEQ-YES VALUE 'Y' 'y'.                        00015200
           05  WCTL-ALLDES             PIC X    VALUE 'N'.              00015300
               88  WCTL-ALLDES-YES VALUE 'Y' 'y'.                       00015400
           05  WCTL-NORUNDATE          PIC X    VALUE 'N'.              00015500
               88  WCTL-NORUNDATE-YES VALUE 'Y' 'y'.                    00015600
W          05  WCTL-DEDETAIL           PIC X    VALUE 'Y'.              00015700
W              88  WCTL-DEDETAIL-YES VALUE 'Y' 'y'.                     00015800
W2             88  WCTL-DEDETAIL-NO  VALUE 'N' 'n'.                     00015900
           SKIP2                                                        00016000
       01  WPLAN-AREA.                                                  00016100
           05  WPLAN-PLAN-NUM    PIC X(6)  VALUE SPACES.                00016200
           05  WPLAN-CTR-AREA.                                          00016300
               10  WPLAN-CTR-BKPI        PIC 9(8)  VALUE ZERO.          00016400
               10  WPLAN-CTR-BKPI2       PIC 9(8)  VALUE ZERO.          00016500
               10  WPLAN-CTR-EXTRA-BKPI  PIC 9(8)  VALUE ZERO.          00016600
               10  WPLAN-CTR-EXTRA-BKPI2 PIC 9(8)  VALUE ZERO.          00016700
               10  WPLAN-CTR-MATCHED     PIC 9(8)  VALUE ZERO.          00016800
               10  WPLAN-CTR-DIFF-RECS   PIC 9(8)  VALUE ZERO.          00016900
               10  WPLAN-CTR-DIFF-DES    PIC 9(8)  VALUE ZERO.          00017000
               10  WPLAN-CTR-DATA-SAME   PIC 9(8)  VALUE ZERO.          00017100
               10  WPLAN-CTR-DATA-DIFF   PIC 9(8)  VALUE ZERO.          00017200
           SKIP2                                                        00017300
       01  WRUN-AREA.                                                   00017400
           05  WRUN-CTR-AREA.                                           00017500
               10  WRUN-CTR-BKPI         PIC 9(8)  VALUE ZERO.          00017600
               10  WRUN-CTR-BKPI2        PIC 9(8)  VALUE ZERO.          00017700
               10  WRUN-CTR-EXTRA-BKPI   PIC 9(8)  VALUE ZERO.          00017800
               10  WRUN-CTR-EXTRA-BKPI2  PIC 9(8)  VALUE ZERO.          00017900
               10  WRUN-CTR-MATCHED      PIC 9(8)  VALUE ZERO.          00018000
               10  WRUN-CTR-DIFF-RECS    PIC 9(8)  VALUE ZERO.          00018100
               10  WRUN-CTR-DIFF-DES     PIC 9(8)  VALUE ZERO.          00018200
               10  WRUN-CTR-DATA-SAME    PIC 9(8)  VALUE ZERO.          00018300
               10  WRUN-CTR-DATA-DIFF    PIC 9(8)  VALUE ZERO.          00018400
               SKIP2                                                    00018500
           SKIP3                                                        00018600
       01  STRECS-AREA.                                                 00018700
           05  STRECS-MAX-ENTRIES    PIC S9(4) VALUE 200.               00018800
           05  STRECS-ENTRY OCCURS 200 TIMES.                           00018900
               10  STRECS-XXYY           PIC X(4)  VALUE SPACES.        00019000
               10  STRECS-DESC           PIC X(20) VALUE SPACES.        00019100
               10  STRECS-REQSKIP        PIC X     VALUE SPACES.        00019200
                   88  STRECS-REQSKIP-YES VALUE 'Y'.                    00019300
               10  STRECS-BKPI        PIC S9(9) COMP-3  VALUE ZERO.     00019400
               10  STRECS-BKPI2       PIC S9(9) COMP-3  VALUE ZERO.     00019500
               10  STRECS-MATCHED     PIC S9(9) COMP-3  VALUE ZERO.     00019600
               10  STRECS-EXTRA1      PIC S9(9) COMP-3  VALUE ZERO.     00019700
               10  STRECS-EXTRA2      PIC S9(9) COMP-3  VALUE ZERO.     00019800
               10  STRECS-DIFFER      PIC S9(9) COMP-3  VALUE ZERO.     00019900
               10  STRECS-SAME        PIC S9(9) COMP-3  VALUE ZERO.     00020000
               10  STRECS-DIFF-DES    PIC S9(9) COMP-3  VALUE ZERO.     00020100
           EJECT                                                        00020200
20992S 01  RPTHDR-LINE.                                                 00020201
               05   FILLER     PIC X(02) VALUE                          00020400
                 ' '.                                                   00020500
               05   FILLER     PIC X(27) VALUE                          00020600
                 'OmniPlus Backup Compare'.                             00020700
               05   RPTHDR-SUBTITLE    PIC X(50)  VALUE SPACES.         00020800
               05   RPTHDR-DATE PIC 99/99/99 VALUE ZERO.                00020900
               05   FILLER     PIC X(6)  VALUE                          00021000
                 '  Page'.                                              00021100
               05   RPTHDR-PAGE PIC ZZZ9  VALUE ZERO.                   00021200
           SKIP3                                                        00021300
20992S 01  RPTITLE1-LINE.                                               00021301
               05   FILLER     PIC X(10) VALUE                          00021500
                 '  Plan# '.                                            00021600
     ***            XXXXXX                                              00021700
               05   FILLER     PIC X(28) VALUE                          00021800
                 '    Matched   Different'.                             00021900
               05   FILLER     PIC X(25) VALUE                          00022000
                 '...BackupFile1.....'.                                 00022100
               05   FILLER     PIC X(22) VALUE                          00022200
                 '...BackupFile2.....'.                                 00022300
                                                                        00022400
20992S 01  RPTITLE2-LINE.                                               00022401
               05   FILLER     PIC X(10) VALUE                          00022600
                 '       '.                                             00022700
               05   FILLER     PIC X(25) VALUE                          00022800
                 '       '.                                             00022900
     ***          zzz,zzz,zzz zzz,zzz,zzz                               00023000
               05   FILLER     PIC X(25) VALUE                          00023100
                 '   Records       Extra'.                              00023200
     ***          zzz,zzz,zzz zzz,zzz,zzz                               00023300
               05   FILLER     PIC X(25) VALUE                          00023400
                 '   Records       Extra'.                              00023500
     ***          zzz,zzz,zzz zzz,zzz,zzz                               00023600
     ***            XXXXXX zzz,zzz,zzz zzz,zzz,zzz                      00023700
                                                                        00023800
20992S 01  RPDAT1-LINE.                                                 00023801
               05  FILLER               PIC X(1) VALUE SPACES.          00024000
               05  RPDAT1-PLAN-NUM      PIC X(6) VALUE SPACES.          00024100
               05  FILLER               PIC X(1) VALUE SPACES.          00024200
               05  RPDAT1-MATCHED       PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00024300
               05  RPDAT1-DIFFERENT     PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00024400
               05  FILLER               PIC X(1) VALUE SPACES.          00024500
               05  RPDAT1-BKP1          PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00024600
               05  RPDAT1-EXTRA1        PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00024700
               05  FILLER               PIC X(1) VALUE SPACES.          00024800
               05  RPDAT1-BKP2          PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00024900
               05  RPDAT1-EXTRA2        PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00025000
           SKIP3                                                        00025100
20992S 01  RTTITLE1-LINE.                                               00025101
               05   FILLER     PIC X(05) VALUE                          00025300
                 'Type'.                                                00025400
               05  FILLER              PIC X(21) VALUE SPACES.          00025500
     ***            XXXXXX                                              00025600
               05   FILLER     PIC X(29) VALUE                          00025700
                 '    Matched    Different'.                            00025800
               05   FILLER     PIC X(24) VALUE                          00025900
                 '...BackupFile1.....'.                                 00026000
               05   FILLER     PIC X(22) VALUE                          00026100
                 '...BackupFile2.....'.                                 00026200
                                                                        00026300
20992S 01  RTTITLE2-LINE.                                               00026301
               05   FILLER     PIC X(05) VALUE                          00026500
                 '  '.                                                  00026600
               05  FILLER              PIC X(21) VALUE SPACES.          00026700
               05   FILLER     PIC X(26) VALUE                          00026800
                 '       '.                                             00026900
     ***          zzz,zzz,zzz zzz,zzz,zzz                               00027000
               05   FILLER     PIC X(24) VALUE                          00027100
                 '   Records       Extra'.                              00027200
     ***          zzz,zzz,zzz zzz,zzz,zzz                               00027300
               05   FILLER     PIC X(24) VALUE                          00027400
                 '   Records       Extra'.                              00027500
     ***          zzz,zzz,zzz zzz,zzz,zzz                               00027600
     ***            XXXXXX zzz,zzz,zzz zzz,zzz,zzz                      00027700
                                                                        00027800
20992S 01  RTDAT1-LINE.                                                 00027801
               05  RTDAT1-XXYY          PIC X(5) VALUE SPACES.          00028000
               05  RTDAT1-REC-NAME     PIC X(21) VALUE SPACES.          00028100
               05  RTDAT1-MATCHED       PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00028200
               05  RTDAT1-DIFFERENT     PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00028300
               05  RTDAT1-BKP1          PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00028400
               05  RTDAT1-EXTRA1        PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00028500
               05  RTDAT1-BKP2          PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00028600
               05  RTDAT1-EXTRA2        PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00028700
           SKIP3                                                        00028800
20992S 01  RDTITLE1-LINE.                                               00028801
               05   FILLER     PIC X(06) VALUE                          00029000
                 '  Type'.                                              00029100
     ***          XXXX                                                  00029200
               05   FILLER     PIC X(32) VALUE                          00029300
                 '  DE#  Name     '.                                    00029400
               05   FILLER     PIC X(22) VALUE                          00029500
                 '    Count'.                                           00029600
                                                                        00029700
20992S 01  RDTITLE2-LINE.                                               00029701
               05   FILLER     PIC X(06) VALUE                          00029900
                 '    '.                                                00030000
                                                                        00030100
20992S 01  RDDAT1-LINE.                                                 00030101
               05  FILLER               PIC X(2) VALUE SPACES.          00030300
               05  RDDAT1-XXYY          PIC X(6) VALUE SPACES.          00030400
               05  RDDAT1-DENUM         PIC X(5)         VALUE SPACES.  00030500
               05  RDDAT1-DENAME        PIC X(20)        VALUE SPACES.  00030600
               05  RDDAT1-DIFFERENT     PIC ZZZZ,ZZZ,ZZ9 VALUE ZERO.    00030700
               SKIP2                                                    00030800
20992S 01  HDTITLE1-LINE.                                               00030801
               05   FILLER     PIC X(50) VALUE                          00031000
                 'File Date   Time  Release  Path'.                     00031100
     *****         X   99/999 99:99 99.99.99 xxxx'                      00031200
20992S 01  HDDAT1-LINE.                                                 00031201
               05  FILLER               PIC X(1) VALUE SPACES.          00031400
               05  HDDAT1-FILE-NUM      PIC X(4) VALUE SPACES.          00031500
               05  HDDAT1-DATE          PIC 99/999    VALUE SPACES.     00031600
               05  FILLER               PIC X    VALUE SPACES.          00031700
               05  HDDAT1-TIME          PIC 99/99   VALUE SPACES.       00031800
               05  FILLER               PIC X    VALUE SPACES.          00031900
               05  HDDAT1-RELEASE       PIC X(9)         VALUE SPACES.  00032000
               05  FILLER               PIC X    VALUE SPACES.          00032100
               05  HDDAT1-PATH          PIC X(100) VALUE SPACES.        00032200
               SKIP2                                                    00032300
       01  BKFIL-AREA. COPY BKFIL REPLACING ==:BKFIL:== BY ==BKFIL==.   00032400
               SKIP2                                                    00032500
       01  BKFIL2-AREA. COPY BKFIL REPLACING ==:BKFIL:== BY ==BKFIL2==. 00032600
           SKIP2                                                        00032700
       01  BWA-REC.  COPY BKUPREC.                                      00032800
             02  TXTX-REC REDEFINES BWA-DATA-REC. COPY MSTTXTX.         00032900
     ***     02  PL-REC REDEFINES BWA-DATA-REC. COPY MSTRPL.            00033000
     ***                                                                00033100
     ***     02  PT-REC REDEFINES BWA-DATA-REC. COPY MSTRPT.            00033200
             02  AH-REC REDEFINES BWA-DATA-REC. COPY AHDEF.             00033300
     ***     02  EK-REC REDEFINES BWA-DATA-REC. COPY EKDEF.             00033400
           SKIP2                                                        00033500
       01  BWA2-REC.  COPY BKUPREC2.                                    00033600
             02  TXTX2-REC REDEFINES BWA2-DATA-REC. COPY MSTTXTX2.      00033700
     ***     02  PT2-REC REDEFINES BWA2-DATA-REC. COPY MSTRPT2.         00033800
             02  AH2-REC REDEFINES BWA2-DATA-REC. COPY AHDEF2.          00033900
     ***     02  EK2-REC REDEFINES BWA2-DATA-REC. COPY EKDEF2.          00034000
           SKIP2                                                        00034100
       01  HDR-REC.  COPY BKUPHDR.                                      00034200
           SKIP2                                                        00034300
       01  HDR2-REC.  COPY BKUPHDR2.                                    00034400
           SKIP2                                                        00034500
       01  TLR-REC.  COPY BKUPTLR.                                      00034600
           SKIP2                                                        00034700
       01  CTL-REC     PIC X(80)  VALUE SPACES.                         00035000
           SKIP3                                                        00035100
       01  OT200-REC                 PIC X(200)           VALUE SPACES. 00035200
           SKIP3                                                        00035300
      **   COPY LITCR.                                                  00035400
           SKIP3                                                        00035500
       01  IO-BKPI-PARM.                                                00035600
             COPY IOBKPI REPLACING 'PROGRAM' BY 'BABKCMP'.              00035700
           SKIP3                                                        00035800
       01  IO-BKPI2-PARM.                                               00035900
             COPY IOBKPI2 REPLACING 'PROGRAM' BY 'BABKCMP'.             00036000
           SKIP3                                                        00036100
660014 01  IO-IN110-PARM.                                               00036101
660014       COPY IOIN110 REPLACING 'PROGRAM' BY 'BABKCMP'.             00036102
           SKIP3                                                        00036400
       01  IO-OT200-PARM.                                               00036500
             COPY IOOT200 REPLACING 'PROGRAM' BY 'BABKCMP'.             00036600
           SKIP3                                                        00036700
       01  DE-AREA.  COPY PRMDE.                                        00036800
           SKIP3                                                        00036900
       01  DE2-AREA.  COPY PRMDE2.                                      00037000
           EJECT                                                        00037100
WFIX   01  SD-AREA IS GLOBAL. COPY PRMSD.                               00037200
           EJECT                                                        00037300
     *** Areas related to the recid-de accumulator container            00037400
       01  STCOIA-AREA. COPY UTCOIA                                     00037500
             REPLACING ==:UTCOIA:== BY ==STCOIA==.                      00037600
           SKIP3                                                        00037700
       01  STCOHDR-AREA. COPY UTCOHDR                                   00037800
             REPLACING ==:UTCOHDR:== BY ==STCOHDR==.                    00037900
           SKIP3                                                        00038000
       01  STKEY-AREA.                                                  00038100
           05  STKEY-XXYY         PIC XXXX VALUE SPACES.                00038200
           05  STKEY-DENUM        PIC XXX  VALUE SPACES.                00038300
           SKIP3                                                        00038400
       01  STDATA-AREA.                                                 00038500
           05  STDATA-DENAME       PIC X(20)  VALUE SPACES.             00038600
           05  STDATA-REQSKIP        PIC X     VALUE SPACES.            00038700
               88  STDATA-REQSKIP-YES VALUE 'Y'.                        00038800
           05  STDATA-DIFFERENT   PIC S9(9) COMP-3  VALUE ZERO.         00038900
           05  STDATA-EXTRA1      PIC S9(9) COMP-3  VALUE ZERO.         00039000
           05  STDATA-EXTRA2      PIC S9(9) COMP-3  VALUE ZERO.         00039100
       PROCEDURE DIVISION.                                              00039200
           SKIP2                                                        00039300
       A-100-BODY.                                                      00039400
661010     PERFORM A-105-INIT.                                          00039401
           PERFORM A-200-INIT-OPEN.                                     00039500
           PERFORM B-100-PROC-PLAN                                      00039600
             UNTIL WQUIT-FLG-YES                                        00039700
               OR (IO-BKPI-ERROR-YES                                    00039800
                 AND IO-BKPI2-ERROR-YES).                               00039900
           PERFORM A-900-FINI-CLOSE.                                    00040000
           GOBACK.                                                      00040100
661010                                                                  00040101
661010 A-105-INIT.                                                      00040102
661010     CALL 'BASDIN' USING SD-AREA.                                 00040103
           SKIP2                                                        00040200
       A-200-INIT-OPEN.                                                 00040300
           DISPLAY 'BaBkCmp-998 V0.7A Program Starting'.                00040400
           ACCEPT RPTHDR-DATE FROM DATE.                                00040500
           ACCEPT WDISP-TIME FROM TIME.                                 00040600
           INSPECT WDISP-TIME REPLACING ALL ' ' BY ':'.                 00040700
           OPEN OUTPUT                                                  00040800
                     PRINT-FILE.                                        00040900
           IF  NOT WPRINT-FS-OK                                         00041000
               DISPLAY 'BaBkCmp-53 PRINT dd OPEN failed:'               00041100
                   WPRINT-FS                                            00041200
               STOP RUN.                                                00041300
           MOVE ' Environment Definition'                               00041400
             TO RPTHDR-SUBTITLE.                                        00041500
           PERFORM V-300-RPT-BREAK.                                     00041600
           STRING ' Processing at ' RPTHDR-DATE                         00041700
                  ', ' WDISP-TIME (1:7)                                 00041800
                  DELIMITED BY SIZE                                     00041900
                    INTO WPRINT-LINE.                                   00042000
           PERFORM V-100-PRINT-LINE.                                    00042100
654920     INITIALIZE STRECS-ENTRY (1).                                 00042101
           PERFORM A-205-INIT-STRECS                                    00042200
654920       VARYING SUB-STRECS FROM 2 BY 1                             00042201
               UNTIL SUB-STRECS > STRECS-MAX-ENTRIES.                   00042400
660014     PERFORM A-240-SETUP-IN110.                                   00042401
           MOVE 'Control Options Specified are'  TO WPRINT-LINE.        00042600
           PERFORM V-100-PRINT-LINE.                                    00042700
           PERFORM F-100-PROC-CTL                                       00042800
660014       UNTIL IO-IN110-ERROR-YES.                                  00042801
           MOVE HDTITLE1-LINE  TO WPRINT-LINE.                          00043000
           PERFORM V-100-PRINT-LINE.                                    00043100
           PERFORM A-210-SETUP-BKPI.                                    00043200
           PERFORM A-220-SETUP-BKPI2.                                   00043300
           PERFORM A-230-SETUP-OT200.                                   00043400
W          IF  WCTL-DEDETAIL-YES                                        00043500
               PERFORM T-800-SETUP-STCO.                                00043600
           MOVE RPTITLE1-LINE  TO WPRINT-TITLE1.                        00043700
           MOVE RPTITLE2-LINE  TO WPRINT-TITLE2.                        00043800
           MOVE ' Summary of Record Compares by Plan'                   00043900
             TO RPTHDR-SUBTITLE.                                        00044000
           PERFORM V-300-RPT-BREAK.                                     00044100
           SKIP2                                                        00044200
       A-205-INIT-STRECS.                                               00044300
654920     MOVE STRECS-ENTRY (1)       TO STRECS-ENTRY (SUB-STRECS).    00044301
           SKIP2                                                        00044500
       A-210-SETUP-BKPI.                                                00044600
           CALL 'BABKPI' USING SD-AREA                                  00044700
                                 IO-BKPI-PARM                           00044800
                                 IO-BKPI-OPEN                           00044900
                                 BWA-REC.                               00045000
           IF  IO-BKPI-ERROR-YES                                        00045100
               DISPLAY 'BABKCMP-102 BKPI file failed to open:'          00045200
                 IO-BKPI-FILE-STATUS                                    00045300
                 GOBACK.                                                00045400
           PERFORM N1-100-READ-BKPI.                                    00045500
           IF  IO-BKPI-ERROR-YES                                        00045600
               DISPLAY 'BABKCMP-103 BKPI file initial Read Failed:'     00045700
                 IO-BKPI-FILE-STATUS                                    00045800
                 GOBACK.                                                00045900
           IF  NOT BWA-TYPE-HDR-REC                                     00046000
               DISPLAY 'BABKCMP-104 BKPI2 file missing BkupHeader:'     00046100
                 IO-BKPI-FILE-STATUS                                    00046200
                 GOBACK.                                                00046300
           MOVE BWA-DATA-REC           TO HDR-REC.                      00046400
W          MOVE '1'            TO HDDAT1-FILE-NUM.                      00046500
           MOVE HDR-DATE       TO HDDAT1-DATE.                          00046600
           MOVE HDR-TIME       TO HDDAT1-TIME.                          00046700
           INSPECT HDDAT1-TIME REPLACING ALL ' ' BY ':'.                00046800
           MOVE HDR-SYSTEM-RELEASE TO HDDAT1-RELEASE.                   00046900
           MOVE SPACES         TO HDDAT1-PATH.                          00047000
&MFCY      DISPLAY 'BKUPIN'   UPON  ENVIRONMENT-NAME.                   00047100
&MFCY      ACCEPT  HDDAT1-PATH FROM ENVIRONMENT-VALUE.                  00047200
           MOVE HDDAT1-LINE    TO WPRINT-LINE.                          00047300
           PERFORM V-100-PRINT-LINE.                                    00047400
     ***                                                                00047500
     ***   DISPLAY 'BaBkCmp-109 File1 Header: '                         00047600
     ***      'Release:' HDR-SYSTEM-RELEASE                             00047700
     ***      'Date:' HDR-DATE                                          00047800
     ***      ', Time:' HDR-TIME                                        00047900
     ***      ', Path:' HDDAT1-PATH.                                    00048000
           PERFORM N1-100-READ-BKPI.                                    00048100
           SKIP2                                                        00048200
       A-220-SETUP-BKPI2.                                               00048300
           CALL 'BABKPI2' USING SD-AREA                                 00048400
                                 IO-BKPI2-PARM                          00048500
                                 IO-BKPI2-OPEN                          00048600
                                 BWA2-REC.                              00048700
           IF  IO-BKPI2-ERROR-YES                                       00048800
               DISPLAY 'BABKCMP-202 BKPI2 file failed to open:'         00048900
                 IO-BKPI2-FILE-STATUS                                   00049000
                 GOBACK.                                                00049100
           PERFORM N2-100-READ-BKPI2.                                   00049200
           IF  IO-BKPI2-ERROR-YES                                       00049300
               DISPLAY 'BABKCMP-203 BKPI2 file initial Read Failed:'    00049400
                 IO-BKPI2-FILE-STATUS                                   00049500
                 GOBACK.                                                00049600
           IF  NOT BWA2-TYPE-HDR-REC                                    00049700
               DISPLAY 'BABKCMP-204 BKPI2 file missing BkupHeader:'     00049800
                 IO-BKPI2-FILE-STATUS                                   00049900
                 GOBACK.                                                00050000
           MOVE BWA2-DATA-REC      TO HDR2-REC.                         00050100
W          MOVE '2'            TO HDDAT1-FILE-NUM.                      00050200
           MOVE HDR2-DATE      TO HDDAT1-DATE.                          00050300
           MOVE HDR2-TIME      TO HDDAT1-TIME.                          00050400
           INSPECT HDDAT1-TIME REPLACING ALL ' ' BY ':'.                00050500
           MOVE HDR2-SYSTEM-RELEASE TO HDDAT1-RELEASE.                  00050600
           MOVE SPACES         TO HDDAT1-PATH.                          00050700
&MFCY      DISPLAY 'BKUPIN2'  UPON  ENVIRONMENT-NAME.                   00050800
&MFCY      ACCEPT  HDDAT1-PATH FROM ENVIRONMENT-VALUE.                  00050900
           MOVE HDDAT1-LINE    TO WPRINT-LINE.                          00051000
           PERFORM V-100-PRINT-LINE.                                    00051100
     ***                                                                00051200
     ***   DISPLAY 'BaBkCmp-109 File2 Header: '                         00051300
     ***      'Release:' HDR2-SYSTEM-RELEASE                            00051400
     ***      'Date:' HDR2-DATE                                         00051500
     ***      ', Time:' HDR2-TIME                                       00051600
     ***      ', Path:' HDDAT1-PATH.                                    00051700
           PERFORM N2-100-READ-BKPI2.                                   00051800
           SKIP2                                                        00051900
       A-230-SETUP-OT200.                                               00052000
           CALL 'UTO200IO' USING SD-AREA                                00052100
                                 IO-OT200-PARM                          00052200
                                 IO-OT200-OPEN                          00052300
                                 OT200-REC.                             00052400
           IF  IO-OT200-ERROR = 'Y'                                     00052500
                OR IO-OT200-FILE-STATUS NOT = '00'                      00052600
               DISPLAY 'BABKCMP-301 OT200 file failed to open:'         00052700
                 IO-OT200-FILE-STATUS                                   00052800
                 GOBACK.                                                00052900
           SKIP2                                                        00053000
660014 A-240-SETUP-IN110.                                               00053001
659652     CALL 'UT110IN'  USING SD-AREA                                00053002
660014               IO-IN110-PARM                                      00053003
660014               IO-IN110-OPEN                                      00053004
                     CTL-REC.                                           00053500
660014     IF  IO-IN110-ERROR = 'Y'                                     00053501
660014          OR IO-IN110-FILE-STATUS NOT = '00'                      00053502
660014         DISPLAY 'BABKCMP-302 IN110 file failed to open:'         00053503
660014           IO-IN110-FILE-STATUS                                   00053504
                 GOBACK.                                                00054000
660014     PERFORM F-110-READ-IN110.                                    00054001
           SKIP2                                                        00054200
       A-900-FINI-CLOSE.                                                00054300
           DISPLAY ' '.                                                 00054400
                                                                        00054500
           CALL 'BABKPI' USING SD-AREA                                  00054600
                                 IO-BKPI-PARM                           00054700
                                 IO-BKPI-CLOSE                          00054800
                                 BWA-REC.                               00054900
           CALL 'BABKPI2' USING SD-AREA                                 00055000
                                 IO-BKPI2-PARM                          00055100
                                 IO-BKPI2-CLOSE                         00055200
                                 BWA2-REC.                              00055300
659652     CALL 'UT110IN'  USING SD-AREA                                00055301
660014               IO-IN110-PARM                                      00055302
660014               IO-IN110-CLOSE                                     00055303
                     CTL-REC.                                           00055700
           CALL 'UTO200IO' USING SD-AREA                                00055800
                                 IO-OT200-PARM                          00055900
                                 IO-OT200-CLOSE                         00056000
                                 OT200-REC.                             00056100
           PERFORM A-920-STATS.                                         00056200
W          DISPLAY 'BaBkCmp-946 Print File Lines:' WPRINT-TOT-LINES.    00056300
           CLOSE PRINT-FILE.                                            00056400
           DISPLAY ' '.                                                 00056500
           DISPLAY 'BaBkCmp-998 Program Ending'.                        00056600
           SKIP2                                                        00056700
       A-920-STATS.                                                     00056800
           MOVE ' Total'            TO RPDAT1-PLAN-NUM.                 00056900
           MOVE WRUN-CTR-MATCHED    TO RPDAT1-MATCHED.                  00057000
           MOVE WRUN-CTR-DATA-DIFF TO RPDAT1-DIFFERENT.                 00057100
           MOVE WRUN-CTR-BKPI       TO RPDAT1-BKP1.                     00057200
           MOVE WRUN-CTR-EXTRA-BKPI TO RPDAT1-EXTRA1.                   00057300
           MOVE WRUN-CTR-BKPI2      TO RPDAT1-BKP2.                     00057400
           MOVE WRUN-CTR-EXTRA-BKPI2 TO RPDAT1-EXTRA2.                  00057500
           MOVE RPDAT1-LINE   TO WPRINT-LINE.                           00057600
           PERFORM V-100-PRINT-LINE.                                    00057700
           DISPLAY ' '.                                                 00057800
           DISPLAY ' '.                                                 00057900
           MOVE WCTR-PLANS     TO WDISP-A-1.                            00058000
           DISPLAY 'BaBkCmp-444 Run Totals: '                           00058100
             ' Plans:' WDISP-A-1.                                       00058200
           MOVE WCTR-CYCLE          TO WDISP-1.                         00058300
           MOVE WRUN-CTR-MATCHED    TO WDISP-2.                         00058400
           MOVE WRUN-CTR-DATA-DIFF  TO WDISP-3.                         00058500
           DISPLAY ' Recs Compared:' WDISP-1                            00058600
             ', Matched:' WDISP-2                                       00058700
             ', Different:' WDISP-3.                                    00058800
           MOVE WRUN-CTR-BKPI       TO WDISP-1.                         00058900
           MOVE WRUN-CTR-EXTRA-BKPI TO WDISP-2.                         00059000
           DISPLAY '    Bkpi: Recs:' WDISP-1                            00059100
             ', Extra:' WDISP-2.                                        00059200
           MOVE WRUN-CTR-BKPI2       TO WDISP-1.                        00059300
           MOVE WRUN-CTR-EXTRA-BKPI2 TO WDISP-2.                        00059400
           DISPLAY '   Bkpi2: Recs:' WDISP-1                            00059500
             ', Extra:' WDISP-2.                                        00059600
           IF  WCTL-BRSEQ-YES                                           00059700
               MOVE BKFIL-DUPKEYS    TO WDISP-1                         00059800
               MOVE BKFIL2-DUPKEYS   TO WDISP-2                         00059900
               DISPLAY 'BaBkCmp-194 DupBrKeys'                          00060000
                    ' File1:' WDISP-1                                   00060100
                    ' File2:' WDISP-2.                                  00060200
           MOVE RTTITLE1-LINE  TO WPRINT-TITLE1.                        00060300
           MOVE RTTITLE2-LINE  TO WPRINT-TITLE2.                        00060400
           MOVE ' Summary of Record Compares by Type'                   00060500
             TO RPTHDR-SUBTITLE.                                        00060600
           PERFORM V-300-RPT-BREAK.                                     00060700
           DISPLAY ' '.                                                 00060800
           DISPLAY ' '.                                                 00060900
           DISPLAY ' Summary Report of Record Compares'.                00061000
           PERFORM A-950-RPT-STRECS                                     00061100
             VARYING SUB-STRECS FROM 1 BY 1                             00061200
               UNTIL SUB-STRECS > STRECS-MAX-ENTRIES.                   00061300
W          IF  WCTL-DEDETAIL-YES                                        00061400
               PERFORM T-200-REPORT-STCO.                               00061500
           SKIP2                                                        00061600
       A-950-RPT-STRECS.                                                00061700
           IF  STRECS-XXYY (SUB-STRECS) NOT = SPACES                    00061800
               PERFORM A-955-RPT-STRECS.                                00061900
           SKIP2                                                        00062000
       A-955-RPT-STRECS.                                                00062100
     ***                                                                00062200
     ***   DISPLAY ' '.                                                 00062300
     ***   DISPLAY ' RecordType ' STRECS-XXYY (SUB-STRECS)              00062400
     ***    ' ' STRECS-DESC (SUB-STRECS).                               00062500
     ***   MOVE  STRECS-BKPI (SUB-STRECS)     TO WDISP-1.               00062600
     ***   MOVE  STRECS-EXTRA1 (SUB-STRECS)   TO WDISP-2.               00062700
     ***   DISPLAY '   BkFile1 Read:' WDISP-1                           00062800
     ***         ',     Extra:' WDISP-2.                                00062900
     ***   MOVE  STRECS-BKPI2 (SUB-STRECS)    TO WDISP-1.               00063000
     ***   MOVE  STRECS-EXTRA2 (SUB-STRECS)   TO WDISP-2.               00063100
     ***   DISPLAY '   BkFile2 Read:' WDISP-1                           00063200
     ***         ',     Extra:' WDISP-2.                                00063300
     ***   MOVE  STRECS-MATCHED (SUB-STRECS)  TO WDISP-1.               00063400
     ***   MOVE  STRECS-SAME (SUB-STRECS)     TO WDISP-2.               00063500
     ***   DISPLAY '     KeyMatched:' WDISP-1                           00063600
W    ***           ', SameData:' WDISP-2.                               00063700
     ***   MOVE  STRECS-DIFFER (SUB-STRECS)   TO WDISP-3.               00063800
     ***   MOVE  STRECS-DIFF-DES (SUB-STRECS) TO WDISP-4.               00063900
W    ***   DISPLAY '       '                                            00064000
W    ***           '                '                                   00064100
     ***         'Differing Data:' WDISP-3                              00064200
     ***         ', #DEs:' WDISP-4.                                     00064300
           MOVE STRECS-XXYY (SUB-STRECS)     TO RTDAT1-XXYY.            00064400
W          MOVE STRECS-DESC (SUB-STRECS)     TO RTDAT1-REC-NAME.        00064500
           MOVE STRECS-BKPI (SUB-STRECS)     TO RTDAT1-BKP1.            00064600
           MOVE STRECS-EXTRA1 (SUB-STRECS)   TO RTDAT1-EXTRA1.          00064700
           MOVE STRECS-BKPI2 (SUB-STRECS)    TO RTDAT1-BKP2.            00064800
           MOVE STRECS-EXTRA2 (SUB-STRECS)   TO RTDAT1-EXTRA2.          00064900
           MOVE STRECS-MATCHED (SUB-STRECS)  TO RTDAT1-MATCHED.         00065000
           MOVE STRECS-DIFFER (SUB-STRECS)   TO RTDAT1-DIFFERENT.       00065100
           MOVE RTDAT1-LINE   TO WPRINT-LINE.                           00065200
           PERFORM V-100-PRINT-LINE.                                    00065300
           SKIP2                                                        00065400
     *** Code to process each plan                                      00065500
       B-100-PROC-PLAN.                                                 00065600
           ADD 1 TO WCTR-PLANS.                                         00065700
           INITIALIZE WPLAN-AREA.                                       00065800
           IF  BWA-PLAN-NUM < BWA2-PLAN-NUM                             00065900
               MOVE BWA-PLAN-NUM  TO WPLAN-PLAN-NUM                     00066000
           ELSE                                                         00066100
               MOVE BWA2-PLAN-NUM  TO WPLAN-PLAN-NUM.                   00066200
           IF  WCTL-TRACE-YES                                           00066300
               DISPLAY 'BaBkCmp-444 Starting Plan: ' WPLAN-PLAN-NUM.    00066400
           PERFORM B-110-PROC-RECS                                      00066500
             UNTIL WQUIT-FLG-YES                                        00066600
               OR ((IO-BKPI-ERROR-YES                                   00066700
                  OR BWA-PLAN-NUM NOT = WPLAN-PLAN-NUM)                 00066800
                 AND (IO-BKPI2-ERROR-YES                                00066900
                  OR BWA2-PLAN-NUM NOT = WPLAN-PLAN-NUM)).              00067000
           MOVE WPLAN-PLAN-NUM      TO RPDAT1-PLAN-NUM.                 00067100
           MOVE WPLAN-CTR-MATCHED   TO RPDAT1-MATCHED.                  00067200
           MOVE WPLAN-CTR-DATA-DIFF TO RPDAT1-DIFFERENT.                00067300
           MOVE WPLAN-CTR-BKPI      TO RPDAT1-BKP1.                     00067400
           MOVE WPLAN-CTR-EXTRA-BKPI TO RPDAT1-EXTRA1.                  00067500
           MOVE WPLAN-CTR-BKPI2     TO RPDAT1-BKP2.                     00067600
           MOVE WPLAN-CTR-EXTRA-BKPI2 TO RPDAT1-EXTRA2.                 00067700
           MOVE RPDAT1-LINE   TO WPRINT-LINE.                           00067800
           PERFORM V-100-PRINT-LINE.                                    00067900
     ***                                                                00068000
     ***   DISPLAY ' '.                                                 00068100
     ***   DISPLAY 'BaBkCmp-444 Plan: ' WPLAN-PLAN-NUM                  00068200
     ***     ',Recs Matched:' WPLAN-CTR-MATCHED                         00068300
     ***     ', Different:' WPLAN-CTR-DATA-DIFF.                        00068400
     ***   DISPLAY '    Bkpi: Recs:' WPLAN-CTR-BKPI                     00068500
     ***     ', Extra:' WPLAN-CTR-EXTRA-BKPI.                           00068600
     ***   DISPLAY '   Bkpi2: Recs:' WPLAN-CTR-BKPI2                    00068700
     ***     ', Extra:' WPLAN-CTR-EXTRA-BKPI2.                          00068800
           ADD WPLAN-CTR-BKPI       TO WRUN-CTR-BKPI.                   00068900
           ADD WPLAN-CTR-BKPI2      TO WRUN-CTR-BKPI2.                  00069000
           ADD WPLAN-CTR-EXTRA-BKPI TO WRUN-CTR-EXTRA-BKPI.             00069100
           ADD WPLAN-CTR-EXTRA-BKPI2 TO WRUN-CTR-EXTRA-BKPI2.           00069200
           ADD WPLAN-CTR-MATCHED    TO WRUN-CTR-MATCHED.                00069300
           ADD WPLAN-CTR-DIFF-RECS  TO WRUN-CTR-DIFF-RECS.              00069400
           ADD WPLAN-CTR-DIFF-DES   TO WRUN-CTR-DIFF-DES.               00069500
           ADD WPLAN-CTR-DATA-SAME  TO WRUN-CTR-DATA-SAME.              00069600
           ADD WPLAN-CTR-DATA-DIFF  TO WRUN-CTR-DATA-DIFF.              00069700
           SKIP2                                                        00069800
       B-110-PROC-RECS.                                                 00069900
           IF  BKFIL-KEY-GROUP < BKFIL2-KEY-GROUP                       00070000
               OR  IO-BKPI2-ERROR-YES                                   00070100
               OR  BWA2-PLAN-NUM NOT = WPLAN-PLAN-NUM                   00070200
               PERFORM B-200-EXTRA-BKPI                                 00070300
           ELSE IF  BKFIL2-KEY-GROUP < BKFIL-KEY-GROUP                  00070400
               OR  IO-BKPI-ERROR-YES                                    00070500
               OR  BWA-PLAN-NUM NOT = WPLAN-PLAN-NUM                    00070600
               PERFORM B-300-EXTRA-BKPI2                                00070700
           ELSE                                                         00070800
               PERFORM B-400-REC-KEYS-MATCH                             00070900
           END-IF.                                                      00071000
           ADD 1 TO WCTR-CYCLE.                                         00071100
           SKIP2                                                        00071200
       B-200-EXTRA-BKPI.                                                00071300
           ADD 1  TO STRECS-EXTRA1 (BKFIL-XXYY-SUB).                    00071400
           PERFORM N1-500-RUN-RESTORE.                                  00071500
           IF  BKFIL-XXYY = 'TXTX'                                      00071600
               AND WPREVTX-BKPI-CYCLE = (WCTR-CYCLE - 1)                00071700
               AND WPREVTX-FILE-NAME = TXTX-FILE-NAME                   00071800
               MOVE TXTX-DATA-CHAR-ALL TO WSHOW-VALUES                  00071900
               PERFORM Q-900-WRITE-WSHOW                                00072000
               MOVE WCTR-CYCLE   TO WPREVTX-BKPI-CYCLE                  00072100
           ELSE                                                         00072200
               PERFORM B-210-DUMP-REC.                                  00072300
           IF  BKFIL-XXYY = 'TXTX'                                      00072400
               MOVE WCTR-CYCLE      TO WPREVTX-BKPI-CYCLE               00072500
               MOVE TXTX-FILE-NAME  TO WPREVTX-FILE-NAME.               00072600
           ADD 1 TO WPLAN-CTR-EXTRA-BKPI.                               00072700
           PERFORM N1-100-READ-BKPI.                                    00072800
           SKIP2                                                        00072900
       B-210-DUMP-REC.                                                  00073000
           MOVE  BKFIL-XXYY           TO WRECHDR-XXYY.                  00073100
           MOVE 'on File1 only'       TO WRECHDR-MSG.                   00073200
           MOVE WCTR-BKPI             TO WRECHDR-RECNUM.                00073300
           PERFORM Q-905-WRITE-RECHDR.                                  00073400
           SET WDUMP-FLG-REC TO TRUE.                                   00073500
           PERFORM Q-100-DUMP-REC.                                      00073600
           SKIP2                                                        00073700
       B-300-EXTRA-BKPI2.                                               00073800
           ADD 1  TO STRECS-EXTRA2 (BKFIL2-XXYY-SUB).                   00073900
           PERFORM N2-500-RUN-RESTORE.                                  00074000
           IF  BKFIL-XXYY = 'TXTX'                                      00074100
               AND WPREVTX-BKPI2-CYCLE = (WCTR-CYCLE - 1)               00074200
               AND WPREVTX-FILE-NAME = TXTX2-FILE-NAME                  00074300
               MOVE TXTX2-DATA-CHAR-ALL TO WSHOW-VALUES                 00074400
               PERFORM Q-900-WRITE-WSHOW                                00074500
           ELSE                                                         00074600
               PERFORM B-310-DUMP-REC.                                  00074700
           IF  BKFIL-XXYY = 'TXTX'                                      00074800
               MOVE WCTR-CYCLE       TO WPREVTX-BKPI2-CYCLE             00074900
               MOVE TXTX2-FILE-NAME  TO WPREVTX-FILE-NAME.              00075000
           ADD 1 TO WPLAN-CTR-EXTRA-BKPI2.                              00075100
           PERFORM N2-100-READ-BKPI2.                                   00075200
           SKIP2                                                        00075300
       B-310-DUMP-REC.                                                  00075400
           MOVE  BKFIL2-XXYY           TO WRECHDR-XXYY.                 00075500
           MOVE 'on File2 only' TO WRECHDR-MSG.                         00075600
           MOVE WCTR-BKPI2             TO WRECHDR-RECNUM2.              00075700
           PERFORM Q-905-WRITE-RECHDR.                                  00075800
           SET WDUMP-FLG-REC2 TO TRUE.                                  00075900
           PERFORM Q-100-DUMP-REC.                                      00076000
           SKIP2                                                        00076100
       B-400-REC-KEYS-MATCH.                                            00076200
           ADD 1  TO STRECS-MATCHED (BKFIL-XXYY-SUB).                   00076300
           ADD 1 TO WPLAN-CTR-MATCHED.                                  00076400
           IF  BWA-REC (1:BKFIL-LENG)                                   00076500
                 = BWA2-REC (1:BKFIL2-LENG)                             00076600
               ADD 1 TO WPLAN-CTR-DATA-SAME                             00076700
               ADD 1  TO STRECS-SAME (BKFIL-XXYY-SUB)                   00076800
           ELSE                                                         00076900
               PERFORM B-410-DATA-IS-DIFF.                              00077000
           PERFORM N1-100-READ-BKPI.                                    00077100
           PERFORM N2-100-READ-BKPI2.                                   00077200
           SKIP2                                                        00077300
       B-410-DATA-IS-DIFF.                                              00077400
           ADD 1  TO STRECS-DIFFER (BKFIL-XXYY-SUB).                    00077500
           PERFORM N1-500-RUN-RESTORE.                                  00077600
           PERFORM N2-500-RUN-RESTORE.                                  00077700
           IF  BKFIL-XXYY = 'TXTX'                                      00077800
               AND WPREVTX-MATCH-CYCLE = (WCTR-CYCLE - 1)               00077900
               AND WPREVTX-FILE-NAME = TXTX-FILE-NAME                   00078000
               MOVE TXTX-DATA-CHAR-ALL TO WSHOW-VALUES                  00078100
               PERFORM Q-900-WRITE-WSHOW                                00078200
               MOVE TXTX2-DATA-CHAR-ALL   TO WSHOW-VALUES               00078300
               MOVE '        File2 Value:' TO WSHOW-DE-NAME             00078400
               PERFORM Q-900-WRITE-WSHOW                                00078500
           ELSE                                                         00078600
               PERFORM B-410-RPT-DIFFS.                                 00078700
           ADD 1 TO WPLAN-CTR-DATA-DIFF.                                00078800
           IF  BKFIL-XXYY = 'TXTX'                                      00078900
               MOVE WCTR-CYCLE      TO WPREVTX-MATCH-CYCLE              00079000
               MOVE  TXTX-FILE-NAME TO  WPREVTX-FILE-NAME.              00079100
           SKIP2                                                        00079200
       B-410-RPT-DIFFS.                                                 00079300
           MOVE  BKFIL-XXYY           TO WRECHDR-XXYY.                  00079400
           MOVE 'Differs'    TO WRECHDR-MSG.                            00079500
           MOVE 'File1     ' TO WRECHDR-HDG1.                           00079600
           MOVE 'File2     ' TO WRECHDR-HDG2.                           00079700
           MOVE WCTR-BKPI             TO WRECHDR-RECNUM.                00079800
           MOVE WCTR-BKPI2            TO WRECHDR-RECNUM2.               00079900
           PERFORM Q-905-WRITE-RECHDR.                                  00080000
           PERFORM Q-200-DEDIFF.                                        00080100
           SKIP2                                                        00080200
       F-100-PROC-CTL.                                                  00080300
           MOVE CTL-REC  TO WPRINT-LINE (4:).                           00080400
           IF  CTL-REC (1:1) = '*'                                      00080500
               DISPLAY 'BaBkCmp-500 Comment:' CTL-REC (1:50)            00080600
           ELSE                                                         00080700
               PERFORM F-105-CONTINUE.                                  00080800
           PERFORM V-100-PRINT-LINE.                                    00080900
660014     PERFORM F-110-READ-IN110.                                    00080901
       F-105-CONTINUE.                                                  00081100
           MOVE CTL-REC    TO WCTL-CMD.                                 00081200
           DISPLAY 'BaBkCmp-500 CtlCard:' CTL-REC (1:50).               00081300
           IF  WCTL-CMD-RECTRACE                                        00081400
               SET WCTL-REC-TRACE-YES TO TRUE                           00081500
           ELSE IF  WCTL-CMD-TRACE                                      00081600
               SET WCTL-TRACE-YES TO TRUE                               00081700
           ELSE IF  WCTL-CMD-BRSEQ                                      00081800
               SET WCTL-BRSEQ-YES TO TRUE                               00081900
           ELSE IF  WCTL-CMD-ALLDES                                     00082000
               SET WCTL-ALLDES-YES TO TRUE                              00082100
           ELSE IF  WCTL-CMD-NORUNDATE                                  00082200
               SET WCTL-NORUNDATE-YES TO TRUE                           00082300
W          ELSE IF  WCTL-CMD-DEDETAIL                                   00082400
W              SET WCTL-DEDETAIL-YES TO TRUE                            00082500
W          ELSE IF  WCTL-CMD-NODEDETAIL                                 00082600
W              SET WCTL-DEDETAIL-NO  TO TRUE                            00082700
           ELSE                                                         00082800
               MOVE '!Error, Invalid Control Card'                      00082900
                       TO WPRINT-LINE (30:)                             00083000
               DISPLAY ' Invalid Control Card:' CTL-REC (1:50).         00083100
           SKIP2                                                        00083200
660014 F-110-READ-IN110.                                                00083201
659652     CALL 'UT110IN'  USING SD-AREA                                00083202
660014                    IO-IN110-PARM                                 00083203
660014               IO-IN110-READ                                      00083204
                     CTL-REC.                                           00083700
           SKIP2                                                        00083800
     ** Various BKPI file/rec dependant routines                        00083900
       N1-100-READ-BKPI.                                                00084000
           INITIALIZE BKFIL-REC-AREA.                                   00084100
           IF  WCTL-BRSEQ-YES                                           00084200
               PERFORM N1-120-READ-PIS                                  00084300
           ELSE                                                         00084400
               PERFORM N1-110-READ-BKPI.                                00084500
           IF  IO-BKPI-ERROR-NO                                         00084600
               AND IO-BKPI-EOF-NO                                       00084700
               PERFORM N1-105-SETUP-REC                                 00084800
           ELSE                                                         00084900
               DISPLAY 'BABKCMP-992 End of BkPi'                        00085000
               SET IO-BKPI-ERROR-YES TO TRUE.                           00085100
           SKIP2                                                        00085200
       N1-105-SETUP-REC.                                                00085300
           MOVE IO-BKPI-REC-LENG  TO BKFIL-LENG                         00085400
           PERFORM N1-130-GET-XXYY.                                     00085500
           IF  WCTL-NORUNDATE-YES                                       00085600
               SET BKFIL-OPER-RUN-ZAP TO TRUE                           00085700
               CALL 'BABKREU' USING BKFIL-AREA                          00085800
                                    BWA-REC.                            00085900
           IF  WCTL-REC-TRACE-YES                                       00086000
               DISPLAY 'BaBkCmp File1: Type:' BWA-TYPE-CODE             00086100
                  ', Id:' BKFIL-XXYY                                    00086200
                  ', Leng:' BKFIL-LENG                                  00086300
                  ', Desc:' BKFIL-DESC.                                 00086400
           IF  BWA-TYPE-TLR-REC                                         00086500
               PERFORM N1-800-PROC-TLR.                                 00086600
           ADD 1 TO WPLAN-CTR-BKPI                                      00086700
WW                      WCTR-BKPI.                                      00086800
           SKIP2                                                        00086900
       N1-110-READ-BKPI.                                                00087000
           CALL 'BABKPI' USING SD-AREA                                  00087100
                                 IO-BKPI-PARM                           00087200
                                 IO-BKPI-READ                           00087300
                                 BWA-REC.                               00087400
           SKIP2                                                        00087500
WP     N1-120-READ-PIS.                                                 00087600
WP         CALL 'BABKPIS' USING SD-AREA                                 00087700
WP                               IO-BKPI-PARM                           00087800
WP                               BKFIL-AREA                             00087900
WP                               BWA-REC.                               00088000
           SKIP2                                                        00088100
       N1-130-GET-XXYY.                                                 00088200
           SET BKFIL-OPER-GET-XXYY TO TRUE                              00088300
           CALL 'BABKREU' USING BKFIL-AREA                              00088400
                                    BWA-REC.                            00088500
W          IF  BKFIL-XXYY-SUB < 1                                       00088600
W              OR > STRECS-MAX-ENTRIES                                  00088700
W              MOVE STRECS-MAX-ENTRIES TO BKFIL-XXYY-SUB                00088800
W              MOVE 'Other'    TO  STRECS-DESC (BKFIL-XXYY-SUB)         00088900
W              MOVE '????'     TO  STRECS-XXYY (BKFIL-XXYY-SUB)         00089000
W          ELSE                                                         00089100
               MOVE BKFIL-XXYY TO STRECS-XXYY (BKFIL-XXYY-SUB)          00089200
               MOVE BKFIL-DESC TO STRECS-DESC (BKFIL-XXYY-SUB).         00089300
           ADD 1  TO STRECS-BKPI (BKFIL-XXYY-SUB).                      00089400
           SKIP2                                                        00089500
       N1-500-RUN-RESTORE.                                              00089600
           IF  WCTL-NORUNDATE-YES                                       00089700
               SET BKFIL-OPER-RUN-RESTORE TO TRUE                       00089800
               CALL 'BABKREU' USING BKFIL-AREA                          00089900
                                    BWA-REC                             00090000
           END-IF.                                                      00090100
           SKIP2                                                        00090200
       N1-800-PROC-TLR.                                                 00090300
           DISPLAY 'BaBkCmp-995 File1 Trailer Found'                    00090400
           MOVE BWA-REC     TO TLR-REC                                  00090500
           SET IO-BKPI-ERROR-YES                                        00090600
               IO-BKPI-EOF-YES                                          00090700
                 TO TRUE.                                               00090800
           SKIP2                                                        00090900
     ** Various BKPI2 file/rec dependant routines                       00091000
       N2-100-READ-BKPI2.                                               00091100
           INITIALIZE BKFIL2-REC-AREA.                                  00091200
           IF  WCTL-BRSEQ-YES                                           00091300
               PERFORM N2-120-READ-PIS                                  00091400
           ELSE                                                         00091500
               PERFORM N2-110-READ-BKPI2.                               00091600
           IF  IO-BKPI2-ERROR-NO                                        00091700
               AND IO-BKPI2-EOF-NO                                      00091800
               PERFORM N2-105-SETUP-REC                                 00091900
           ELSE                                                         00092000
               DISPLAY 'BABKCMP-992 End of BKPI2'                       00092100
               SET IO-BKPI2-ERROR-YES TO TRUE.                          00092200
           SKIP2                                                        00092300
       N2-105-SETUP-REC.                                                00092400
           MOVE IO-BKPI2-REC-LENG TO BKFIL2-LENG                        00092500
           IF  BKFIL2-XXYY = SPACES                                     00092600
               PERFORM N2-130-GET-XXYY.                                 00092700
           IF  WCTL-NORUNDATE-YES                                       00092800
               SET BKFIL2-OPER-RUN-ZAP TO TRUE                          00092900
               CALL 'BABKREU' USING BKFIL2-AREA                         00093000
                                    BWA2-REC.                           00093100
           IF  WCTL-REC-TRACE-YES                                       00093200
               DISPLAY 'BaBkCmp File1: Type:' BWA2-TYPE-CODE            00093300
                  ', Id:' BKFIL2-XXYY                                   00093400
                  ', Leng:' BKFIL2-LENG                                 00093500
                  ', Desc:' BKFIL2-DESC.                                00093600
           IF  BWA2-TYPE-TLR-REC                                        00093700
               PERFORM N2-800-PROC-TLR.                                 00093800
           ADD 1 TO WPLAN-CTR-BKPI2                                     00093900
WW                      WCTR-BKPI2.                                     00094000
           SKIP2                                                        00094100
       N2-110-READ-BKPI2.                                               00094200
           CALL 'BABKPI2' USING SD-AREA                                 00094300
                                 IO-BKPI2-PARM                          00094400
                                 IO-BKPI2-READ                          00094500
                                 BWA2-REC.                              00094600
           SKIP2                                                        00094700
WP     N2-120-READ-PIS.                                                 00094800
WP         CALL 'BABKPIS2' USING SD-AREA                                00094900
WP                               IO-BKPI2-PARM                          00095000
WP                               BKFIL2-AREA                            00095100
WP                               BWA2-REC.                              00095200
           SKIP2                                                        00095300
       N2-130-GET-XXYY.                                                 00095400
           SET BKFIL2-OPER-GET-XXYY TO TRUE                             00095500
           CALL 'BABKREU' USING BKFIL2-AREA                             00095600
                                    BWA2-REC.                           00095700
W          IF  BKFIL2-XXYY-SUB < 1                                      00095800
W              OR > STRECS-MAX-ENTRIES                                  00095900
W              MOVE STRECS-MAX-ENTRIES TO BKFIL2-XXYY-SUB               00096000
W              MOVE 'Other'    TO  STRECS-DESC (BKFIL2-XXYY-SUB)        00096100
W              MOVE '????'     TO  STRECS-XXYY (BKFIL2-XXYY-SUB)        00096200
W          ELSE                                                         00096300
               MOVE BKFIL2-XXYY TO STRECS-XXYY (BKFIL2-XXYY-SUB)        00096400
               MOVE BKFIL2-DESC TO STRECS-DESC (BKFIL2-XXYY-SUB).       00096500
           ADD 1  TO STRECS-BKPI2 (BKFIL2-XXYY-SUB).                    00096600
           SKIP2                                                        00096700
       N2-500-RUN-RESTORE.                                              00096800
           IF  WCTL-NORUNDATE-YES                                       00096900
               SET BKFIL2-OPER-RUN-RESTORE TO TRUE                      00097000
               CALL 'BABKREU' USING BKFIL2-AREA                         00097100
                                    BWA2-REC                            00097200
           END-IF.                                                      00097300
           SKIP2                                                        00097400
       N2-800-PROC-TLR.                                                 00097500
           DISPLAY 'BaBkCmp-995 File2 Trailer Found'                    00097600
           MOVE BWA2-REC    TO TLR-REC                                  00097700
           SET IO-BKPI2-ERROR-YES                                       00097800
               IO-BKPI2-EOF-YES                                         00097900
                 TO TRUE.                                               00098000
           SKIP2                                                        00098100
     ***************************                                        00098200
     *** Routines to dump a record, or record differences               00098300
     ***************************                                        00098400
       Q-100-DUMP-REC.                                                  00098500
           MOVE 1     TO SUB-DE.                                        00098600
           PERFORM Q-110-EACH-DE                                        00098700
               UNTIL SUB-DE > 999.                                      00098800
           SKIP2                                                        00098900
     *** Code to format a line for each DE                              00099000
       Q-110-EACH-DE.                                                   00099100
           IF  WDUMP-FLG-REC                                            00099200
               MOVE SUB-DE                     TO DE-DENUM              00099300
               PERFORM R-100-FETCH-DE-DEF                               00099400
           ELSE                                                         00099500
               MOVE SUB-DE                     TO DE2-DENUM             00099600
               PERFORM S-100-FETCH-DE2-DEF                              00099700
               MOVE DE2-AREA  TO DE-AREA.                               00099800
           IF  DE-NAME = SPACES                                         00099900
               OR  DE-VAL-TYPE-READONLY                                 00100000
               NEXT SENTENCE                                            00100100
 W3        ELSE IF  DE-VAL-TYPE-KEY                                     00100200
 W3            PERFORM R-600-FMT-DE                                     00100300
 W3            PERFORM Q-900-WRITE-WSHOW                                00100400
           ELSE IF (DE-PIC-TYPE-9                                       00100500
                    AND DE-VAL-INTERNAL-N = ZERO)                       00100600
                 OR (DE-PIC-TYPE-X                                      00100700
                    AND (DE-VAL-INTERNAL = SPACES OR '0' OR '00'        00100800
                       OR '000' OR '00000' OR '000000000'               00100900
                       OR DE-VAL-INTERNAL (1:1) = LOW-VALUES))          00101000
               NEXT SENTENCE                                            00101100
           ELSE                                                         00101200
               PERFORM R-600-FMT-DE                                     00101300
               PERFORM Q-900-WRITE-WSHOW                                00101400
           END-IF.                                                      00101500
           IF  DE-NEXT-DE > SUB-DE                                      00101600
               MOVE DE-NEXT-DE                 TO SUB-DE                00101700
           ELSE                                                         00101800
               ADD 1 TO SUB-DE.                                         00101900
           SKIP3                                                        00102000
     *** Compare the 2 records, write out differences                   00102100
       Q-200-DEDIFF.                                                    00102200
           MOVE 1     TO SUB-DE.                                        00102300
WFIX       MOVE ZERO  TO WCTR-DEDIFFS.                                  00102400
           PERFORM Q-210-EACH-DE                                        00102500
               UNTIL SUB-DE > 999.                                      00102600
WFIX       IF  WCTR-DEDIFFS = ZERO                                      00102700
WFIX           PERFORM Q-300-RPT-DATA-DIFF.                             00102800
           SKIP3                                                        00102900
     ***   DE-AREA   - File1                                            00103000
     ***   DE2-AREA  - File2                                            00103100
       Q-210-EACH-DE.                                                   00103200
           MOVE SUB-DE                     TO DE-DENUM                  00103300
                                              DE2-DENUM.                00103400
           PERFORM R-100-FETCH-DE-DEF.                                  00103500
           PERFORM S-100-FETCH-DE2-DEF.                                 00103600
           IF  DE-NAME = SPACES                                         00103700
               OR  DE-VAL-TYPE-READONLY                                 00103800
               NEXT SENTENCE                                            00103900
           ELSE IF  DE-VAL-TYPE-KEY                                     00104000
               PERFORM R-600-FMT-DE                                     00104100
               PERFORM Q-900-WRITE-WSHOW                                00104200
           ELSE IF  DE-VAL-INTERNAL NOT = DE2-VAL-INTERNAL              00104300
               PERFORM Q-220-SHOW-DIFF                                  00104400
           ELSE IF  WCTL-ALLDES-YES                                     00104500
               PERFORM Q-215-SHOW-ALL-VALUED-DES                        00104600
           END-IF.                                                      00104700
                                                                        00104800
           MOVE SUB-DE TO SUB-SAVE.                                     00104900
           IF  DE-NEXT-DE < DE2-NEXT-DE                                 00105000
               MOVE DE-NEXT-DE                 TO SUB-DE                00105100
           ELSE                                                         00105200
               MOVE DE2-NEXT-DE                 TO SUB-DE.              00105300
           IF   SUB-DE <= SUB-SAVE                                      00105400
                ADD SUB-SAVE 1 GIVING SUB-DE.                           00105500
       Q-215-SHOW-ALL-VALUED-DES.                                       00105600
           IF (DE-PIC-TYPE-9                                            00105700
                    AND DE-VAL-INTERNAL-N = ZERO)                       00105800
                 OR (DE-PIC-TYPE-X                                      00105900
                    AND (DE-VAL-INTERNAL = SPACES OR '0' OR '00'        00106000
                       OR '000' OR '00000' OR '000000000'               00106100
                       OR DE-VAL-INTERNAL (1:1) = LOW-VALUES))          00106200
               NEXT SENTENCE                                            00106300
           ELSE                                                         00106400
               MOVE '*'         TO WSHOW-LINE                           00106500
               PERFORM R-605-FMT-DE                                     00106600
               PERFORM Q-900-WRITE-WSHOW                                00106700
           END-IF.                                                      00106800
     *** Show both des, pre then upd                                    00106900
       Q-220-SHOW-DIFF.                                                 00107000
           ADD 1  TO STRECS-DIFF-DES (BKFIL-XXYY-SUB)                   00107100
WFIX                 WCTR-DEDIFFS.                                      00107200
W          IF  WCTL-DEDETAIL-YES                                        00107300
               PERFORM T-100-UPDATE-STCO.                               00107400
           PERFORM R-600-FMT-DE.                                        00107500
           PERFORM S-600-FMT-DE2.                                       00107600
           PERFORM Q-900-WRITE-WSHOW.                                   00107700
           MOVE 1     TO WPLAN-CTR-DIFF-DES.                             00107800
           SKIP2                                                        00107900
     *** GROUP COMPARE DIFF, but all DES show the same.                 00108000
WFIX   Q-300-RPT-DATA-DIFF.                                             00108100
WFIX       MOVE 1  TO SUB-CHAR.                                         00108200
WFIX       PERFORM Q-310-PERCHAR                                        00108300
WFIX         VARYING SUB-CHAR FROM 1 BY 1                               00108400
WFIX           UNTIL SUB-CHAR > BKFIL-LENG.                             00108500
WFIX   Q-310-PERCHAR.                                                   00108600
WFIX       IF  BWA-REC (SUB-CHAR:1)                                     00108700
WFIX             = BWA2-REC (SUB-CHAR:1)                                00108800
WFIX           ADD 1 TO SUB-CHAR                                        00108900
WFIX       ELSE                                                         00109000
WFIX           PERFORM Q-320-DIFF-CHARS.                                00109100
WFIX   Q-320-DIFF-CHARS.                                                00109200
WFIX       MOVE SUB-CHAR   TO SUB-START.                                00109300
WFIX       MOVE 1          TO WCTR-DIFF-CHARS.                          00109400
WFIX       ADD 1           TO SUB-CHAR.                                 00109500
WFIX       PERFORM Q-390-COUNT-DIFF                                     00109600
WFIX         UNTIL SUB-CHAR >= BKFIL-LENG                               00109700
WFIX           OR  WCTR-DIFF-CHARS > 20                                 00109800
WFIX           OR  BWA-REC (SUB-CHAR:1)                                 00109900
WFIX             = BWA2-REC (SUB-CHAR:1).                               00110000
WFIX       MOVE SUB-CHAR   TO WDISP-POS.                                00110100
WFIX       STRING '*  DataDifference at Pos:' WDISP-POS                 00110200
WFIX          '; File1=' QUOTE                                          00110300
WFIX             BWA-REC (SUB-START:WCTR-DIFF-CHARS)                    00110400
WFIX          QUOTE ', File2=' QUOTE                                    00110500
WFIX             BWA2-REC (SUB-START:WCTR-DIFF-CHARS)                   00110600
WFIX          QUOTE                                                     00110700
WFIX2         DELIMITED BY SIZE                                         00110800
WFIX         INTO WSHOW-LINE.                                           00110900
WFIX       PERFORM Q-900-WRITE-WSHOW.                                   00111000
WFIX   Q-390-COUNT-DIFF.                                                00111100
WFIX       ADD 1 TO WCTR-DIFF-CHARS.                                    00111200
WFIX       ADD 1 TO SUB-CHAR.                                           00111300
           SKIP2                                                        00111400
       Q-900-WRITE-WSHOW.                                               00111500
           MOVE WSHOW-LINE   TO OT200-REC.                              00111600
           PERFORM Q-910-WRITE-OT200.                                   00111700
           MOVE SPACES   TO WSHOW-LINE.                                 00111800
           SKIP2                                                        00111900
       Q-905-WRITE-RECHDR.                                              00112000
           PERFORM Q-910-WRITE-OT200.                                   00112100
           MOVE WRECHDR-LINE   TO OT200-REC.                            00112200
           PERFORM Q-910-WRITE-OT200.                                   00112300
           SKIP2                                                        00112400
       Q-910-WRITE-OT200.                                               00112500
           CALL 'UTO200IO' USING SD-AREA                                00112600
                                 IO-OT200-PARM                          00112700
                                 IO-OT200-WRITE                         00112800
                                 OT200-REC.                             00112900
           MOVE SPACES   TO OT200-REC.                                  00113000
           SKIP2                                                        00113100
     ***************************                                        00113200
     *** Routines for the BWA- record and prmde                         00113300
     ***************************                                        00113400
       R-100-FETCH-DE-DEF.                                              00113500
           MOVE SPACES TO DE-NAME.                                      00113600
           SET DE-BYPASS-DED-YES TO TRUE.                               00113700
           SET DE-PROCESS-GET TO TRUE.                                  00113800
           PERFORM R-900-CALL-DEMOD.                                    00113900
           SKIP2                                                        00114000
       R-600-FMT-DE.                                                    00114100
           MOVE SPACES   TO WSHOW-LINE.                                 00114200
           PERFORM R-605-FMT-DE.                                        00114300
       R-605-FMT-DE.                                                    00114400
           CALL 'BADEINDP' USING SD-AREA                                00114500
                                 DE-AREA.                               00114600
           IF  DE-VAL-TYPE-KEY                                          00114700
               SET WSHOW-DE-PREFIX-KEY    TO TRUE                       00114800
           ELSE                                                         00114900
               SET WSHOW-DE-PREFIX-DATA   TO TRUE.                      00115000
           MOVE DE-DENUM   TO WSHOW-DE-NUM.                             00115100
           MOVE DE-NAME    TO WSHOW-DE-NAME.                            00115200
WW4        MOVE DE-VAL-DISPLAY TO WTEMP-SHOW.                           00115300
WW4        IF  WTEMP-VAL-NAME = SPACES                                  00115400
WW4            AND WTEMP-VAL-SEP = SPACES                               00115500
WW4            AND  DE-VAL-NAME NOT = SPACES                            00115600
WW4            MOVE ':'           TO WTEMP-VAL-SEP                      00115700
WW4            MOVE DE-VAL-NAME  TO WTEMP-VAL-NAME.                     00115800
WW4        MOVE WTEMP-SHOW     TO WSHOW-VALUES.                         00115900
           SKIP2                                                        00116000
       R-900-CALL-DEMOD.                                                00116100
           STRING BKFIL-XXYY 'DE'                                       00116200
              DELIMITED BY SIZE                                         00116300
                INTO WK-PGM-NAME.                                       00116400
           CALL WK-PGM-NAME USING SD-AREA                               00116500
                                 DE-AREA                                00116600
                                 BWA-DATA-REC                           00116700
                ON EXCEPTION                                            00116800
                  DISPLAY 'DE data not available for:' WK-PGM-NAME      00116900
             END-CALL.                                                  00117000
           SKIP2                                                        00117100
     ***************************                                        00117200
     *** Routines for the DE2 record and prmde                          00117300
     ***************************                                        00117400
       S-100-FETCH-DE2-DEF.                                             00117500
           MOVE SPACES TO DE2-NAME.                                     00117600
           SET DE2-BYPASS-DED-YES TO TRUE.                              00117700
           SET DE2-PROCESS-GET TO TRUE.                                 00117800
           PERFORM S-900-CALL-MOD-DE2.                                  00117900
           SKIP2                                                        00118000
       S-600-FMT-DE2.                                                   00118100
           CALL 'BADEINDP' USING SD-AREA                                00118200
                                 DE2-AREA.                              00118300
WW4        MOVE DE2-VAL-DISPLAY TO WTEMP-SHOW.                          00118400
WW4        IF  WTEMP-VAL-NAME = SPACES                                  00118500
WW4            AND WTEMP-VAL-SEP = SPACES                               00118600
WW4            AND  DE2-VAL-NAME NOT = SPACES                           00118700
WW4            MOVE ':'           TO WTEMP-VAL-SEP                      00118800
WW4            MOVE DE2-VAL-NAME  TO WTEMP-VAL-NAME.                    00118900
           IF  WSHOW-VAL2 NOT = SPACES                                  00119000
               PERFORM Q-900-WRITE-WSHOW                                00119100
               MOVE '        File2 Value:' TO WSHOW-DE-NAME             00119200
               MOVE WTEMP-SHOW      TO WSHOW-VALUES                     00119300
           ELSE                                                         00119400
               MOVE WTEMP-SHOW      TO WSHOW-VAL2.                      00119500
           SKIP2                                                        00119600
       S-900-CALL-MOD-DE2.                                              00119700
           STRING BKFIL2-XXYY 'DE'                                      00119800
              DELIMITED BY SIZE                                         00119900
                INTO WK-PGM-NAME.                                       00120000
           CALL WK-PGM-NAME USING SD-AREA                               00120100
                                 DE2-AREA                               00120200
                                 BWA2-DATA-REC                          00120300
                ON EXCEPTION                                            00120400
                  DISPLAY 'DE2 data not available for:' WK-PGM-NAME     00120500
             END-CALL.                                                  00120600
           EJECT                                                        00120700
       T-100-UPDATE-STCO.                                               00120800
           MOVE BKFIL-XXYY   TO STKEY-XXYY.                             00120900
           MOVE DE-DENUM     TO STKEY-DENUM.                            00121000
           SET STCOIA-OPER-GET    TO TRUE.                              00121100
           PERFORM T-900-CALL-STCO.                                     00121200
           IF  STCOIA-RC-ERROR                                          00121300
               INITIALIZE STDATA-AREA                                   00121400
               MOVE DE-NAME     TO STDATA-DENAME.                       00121500
           ADD 1                  TO STDATA-DIFFERENT.                  00121600
           SET STCOIA-OPER-UPDATE TO TRUE.                              00121700
           PERFORM T-900-CALL-STCO.                                     00121800
           SKIP3                                                        00121900
       T-200-REPORT-STCO.                                               00122000
           MOVE RDTITLE1-LINE  TO WPRINT-TITLE1.                        00122100
           MOVE RDTITLE2-LINE  TO WPRINT-TITLE2.                        00122200
           MOVE 'Summary of DE differences within RecordType'           00122300
             TO RPTHDR-SUBTITLE.                                        00122400
           PERFORM V-300-RPT-BREAK.                                     00122500
     ***                                                                00122600
     ***   DISPLAY ' '.                                                 00122700
           DISPLAY ' Total #DesDifferent:' STCOHDR-NUM-ITEMS.           00122800
     ***   DISPLAY ' '.                                                 00122900
     ***   DISPLAY '  Summary of DE differences by record type'.        00123000
           SET STCOIA-OPER-FIRST  TO TRUE.                              00123100
           PERFORM T-900-CALL-STCO.                                     00123200
           PERFORM T-210-REPORT-STCO-ENTRY                              00123300
             UNTIL STCOIA-RC-ERROR.                                     00123400
           SET STCOIA-OPER-CO-EMPTY  TO TRUE.                           00123500
           PERFORM T-900-CALL-STCO.                                     00123600
           SKIP3                                                        00123700
       T-210-REPORT-STCO-ENTRY.                                         00123800
           IF  STKEY-XXYY NOT = WPREV-XXYY                              00123900
               MOVE STKEY-XXYY TO WPREV-XXYY                            00124000
                                  WDISP-XXYY                            00124100
           ELSE                                                         00124200
               MOVE SPACES     TO WDISP-XXYY.                           00124300
           MOVE STDATA-DIFFERENT   TO WDISP-1.                          00124400
     ***                                                                00124500
     ***   DISPLAY '     ' WDISP-XXYY                                   00124600
     ***     ' DE' STKEY-DENUM                                          00124700
     ***     '   ' STDATA-DENAME                                        00124800
     ***     ', #Different:' WDISP-1.                                   00124900
           IF  WPRINT-LINES-OVER                                        00125000
               MOVE STKEY-XXYY      TO RDDAT1-XXYY                      00125100
           ELSE                                                         00125200
               MOVE WDISP-XXYY      TO RDDAT1-XXYY.                     00125300
           MOVE STKEY-DENUM         TO RDDAT1-DENUM.                    00125400
           MOVE STDATA-DENAME       TO RDDAT1-DENAME.                   00125500
           MOVE STDATA-DIFFERENT    TO RDDAT1-DIFFERENT.                00125600
           MOVE RDDAT1-LINE   TO WPRINT-LINE.                           00125700
           PERFORM V-100-PRINT-LINE.                                    00125800
           SET STCOIA-OPER-NEXT   TO TRUE.                              00125900
           PERFORM T-900-CALL-STCO.                                     00126000
           SKIP2                                                        00126100
       T-800-SETUP-STCO.                                                00126200
           MOVE 'RecTots'         TO STCOHDR-NAME.                      00126300
           SET STCOHDR-TYPE-ORDERED     TO TRUE.                        00126400
           SET STCOIA-MODE-MOVE         TO TRUE.                        00126500
     *** Check and setup the container                                  00126600
           SET STCOIA-OPER-CO-SETUP     TO TRUE.                        00126700
           PERFORM T-900-CALL-STCO.                                     00126800
           IF  STCOIA-RC-ERROR                                          00126900
               DISPLAY 'BaBkCmp-432 STCO setup failed'                  00127000
               STOP RUN.                                                00127100
           DISPLAY 'BaBkCmp-300 STCO DeDiff Detail is active'.          00127200
           SKIP2                                                        00127300
       T-900-CALL-STCO.                                                 00127400
           SET STCOIA-KEY-ADDR      TO ADDRESS OF STKEY-AREA.           00127500
           SET STCOIA-DATA-ADDR     TO ADDRESS OF STDATA-AREA.          00127600
           MOVE LENGTH OF STKEY-AREA    TO STCOHDR-KEY-LENG.            00127700
           MOVE LENGTH OF STDATA-AREA   TO STCOHDR-DATA-LENG.           00127800
           CALL 'UTCOINDX' USING SD-AREA                                00127900
                            STCOHDR-AREA                                00128000
                            STCOIA-AREA.                                00128100
       V-100-PRINT-LINE.                                                00128200
           IF  WPRINT-LINES-OVER                                        00128300
               PERFORM V-150-NEWPAGE                                    00128400
           ELSE                                                         00128500
               ADD 1 TO WPRINT-LINES.                                   00128600
           PERFORM V-200-BLANK-LINE                                     00128700
             WPRINT-BLANK-LINES TIMES.                                  00128800
           WRITE PRINT-LINE FROM WPRINT-LINE                            00128900
                 AFTER ADVANCING 1 LINE.                                00129000
           MOVE SPACES   TO WPRINT-LINE.                                00129100
           MOVE ZERO         TO WPRINT-BLANK-LINES.                     00129200
           ADD 1 TO WPRINT-TOT-LINES.                                   00129300
       V-150-NEWPAGE.                                                   00129400
           ADD 1 TO WPRINT-PAGE.                                        00129500
           MOVE WPRINT-PAGE  TO RPTHDR-PAGE.                            00129600
TH*****    WRITE PRINT-LINE FROM ' '                                    00129700
TH*****      AFTER ADVANCING PAGE.                                      00129800
TH         MOVE SPACES TO PRINT-LINE.                                   00129900
TH         WRITE PRINT-LINE                                             00130000
TH           AFTER ADVANCING PAGE.                                      00130100
           WRITE PRINT-LINE FROM RPTHDR-LINE                            00130200
             AFTER ADVANCING 1 LINE.                                    00130300
           WRITE PRINT-LINE FROM WPRINT-TITLE1                          00130400
             AFTER ADVANCING 1.                                         00130500
           WRITE PRINT-LINE FROM WPRINT-TITLE2                          00130600
             AFTER ADVANCING 1.                                         00130700
           MOVE 1            TO WPRINT-BLANK-LINES.                     00130800
           MOVE ZERO TO WPRINT-BLANK-LINES.                             00130900
           MOVE ZERO TO WPRINT-LINES.                                   00131000
           ADD 4 TO WPRINT-TOT-LINES.                                   00131100
       V-200-BLANK-LINE.                                                00131200
TH         MOVE SPACES TO PRINT-LINE.                                   00131300
TH         WRITE PRINT-LINE                                             00131400
TH           AFTER ADVANCING 1.                                         00131500
TH*****    WRITE PRINT-LINE FROM ' '                                    00131600
TH*****      AFTER ADVANCING 1.                                         00131700
       V-300-RPT-BREAK.                                                 00131800
           PERFORM V-150-NEWPAGE.                                       00131900
           EJECT                                                        00132000
