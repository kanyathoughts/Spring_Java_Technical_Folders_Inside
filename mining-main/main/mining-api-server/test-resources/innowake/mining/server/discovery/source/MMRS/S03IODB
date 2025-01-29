* REL=S03IODB.10                                                        00001000
**********************************************************************  00002000
*                                                                       00003000
*  S03IODB .... STATIC SQL I/O MODULE                                   00004000
*                                                                       00005000
*  CREATION DATE: 04/25/16                                              00006000
*                                                                       00007000
*  FUNCTIONAL DESCRIPTION: THIS PROGRAM CONTAINS THE STATIC SQL         00008000
*  VECTORS REQUIRED TO SUPPORT I/O TO THE S03 FILE.  IT IS LOADED       00009000
*  BY THE DB2 I/O MANAGER (BIPDB2X).  THIS PROGRAM WILL BE USED         00010000
*  IN BOTH A CICS AND BATCH ENVIRONMENT AND MUST BE RE-ENTRANT          00011000
*  AND VOID OF SVC CALLS.                                               00012000
*     ***************************************************************   00013000
*     *                                                             *   00013001
*     *                           NOTICE                            *   00013002
*     *                                                             *   00013003
*     *   THIS SOFTWARE IS THE PROPERTY OF AND CONTAINS             *   00013004
*     *   CONFIDENTIAL INFORMATION OF INFOR AND/OR ITS AFFILIATES   *   00013005
*     *   OR SUBSIDIARIES AND SHALL NOT BE DISCLOSED WITHOUT PRIOR  *   00013006
*     *   WRITTEN PERMISSION. LICENSED CUSTOMERS MAY COPY AND       *   00013007
*     *   ADAPT THIS SOFTWARE FOR THEIR OWN USE IN ACCORDANCE WITH  *   00013008
*     *   THE TERMS OF THEIR SOFTWARE LICENSE AGREEMENT.            *   00013009
*     *   ALL OTHER RIGHTS RESERVED.                                *   00013010
*     *                                                             *   00013011
*     *   (C) COPYRIGHT 2017 INFOR.  ALL RIGHTS RESERVED.           *   00013012
*     *   THE WORD AND DESIGN MARKS SET FORTH HEREIN ARE            *   00013013
*     *   TRADEMARKS AND/OR REGISTERED TRADEMARKS OF INFOR          *   00013014
*     *   AND/OR ITS AFFILIATES AND SUBSIDIARIES. ALL RIGHTS        *   00013015
*     *   RESERVED.  ALL OTHER TRADEMARKS LISTED HEREIN ARE         *   00013016
*     *   THE PROPERTY OF THEIR RESPECTIVE OWNERS.                  *   00013017
*     *                                                             *   00013018
*     ***************************************************************   00013019
*     *     Infopoint Relationship Pricing                          *   00013020
*     *     RP 5.0.01                                               *   00013021
*     ***************************************************************   00013022
**********************************************************************  00014000
* SQI COMMUNICATION AREA DSECT ADDRESSED BY REGISTER 11                 00015000
*   MUST NOT BE MODIFIED.                                               00016000
**********************************************************************  00017000
*                                                                       00018000
COM#AREA DSECT                         SQI COMMUNICATION AREA           00019000
SQW@CAF  DS    F                       ADDRESS OF ATTACH FACILITY       00020000
SQW@RET  DS    F                       RETURN ADDRESS TO SQI            00021000
SQWSQLCA DS    XL136                   GLOBAL SQLCA                     00022000
         ORG   SQWSQLCA                                                 00023000
         EXEC  SQL INCLUDE SQLCA                                        00024000
*                                                                       00025000
*                                                                       00026000
**********************************************************************  00027000
* HOST VARIABLE AREA DSECT ADDRESSED BY REGISTER 2                      00028000
*   MUST NOT BE MODIFIED.                                               00029000
**********************************************************************  00030000
*                                                                       00031000
HOST#VAR DSECT                         PROGRAM HOST VARIABLES           00032000
CSRPTR   DS    H                       CURSOR ROUTINE POINTER           00033000
         DS    CL6                     RESERVED                         00034000
ORG      DS    CL6                     RECORD ORG CODE                  00035000
COBREC   DS    CL251                   COBOL RECORD (UNALIGNED)         00036000
ASMREC   DS    0F                      ASSEMBLER RECORD (ALIGNED)       00037000
INST     DS    CL4                                                      00038000
RECNBR   DS    CL4                                                      00039000
RATETABL DS    CL6                                                      00040000
EFFDATE  DS    CL8                                                      00041000
AUDDATE  DS    PL5'0.'                                                  00042000
AUDTIME  DS    PL5'0.'                                                  00043000
AUDUSER  DS    CL8                                                      00044000
AUDORG   DS    CL6                                                      00045000
BALOPT   DS    CL1                                                      00046000
RATEOPT  DS    CL1                                                      00047000
BALORDR  DS    CL2                                                      00048000
TERMBAL  DS    CL6                                                      00049000
RATEAMT1 DS    PL5'0.'                                                  00050000
TERM1    DS    PL3'0.'                                                  00051000
RATPCT1  DS    PL5'0.000000000'                                         00052000
RATEAMT2 DS    PL5'0.'                                                  00053000
TERM2    DS    PL3'0.'                                                  00054000
RATPCT2  DS    PL5'0.000000000'                                         00055000
RATEAMT3 DS    PL5'0.'                                                  00056000
TERM3    DS    PL3'0.'                                                  00057000
RATPCT3  DS    PL5'0.000000000'                                         00058000
RATEAMT4 DS    PL5'0.'                                                  00059000
TERM4    DS    PL3'0.'                                                  00060000
RATPCT4  DS    PL5'0.000000000'                                         00061000
RATEAMT5 DS    PL5'0.'                                                  00062000
TERM5    DS    PL3'0.'                                                  00063000
RATPCT5  DS    PL5'0.000000000'                                         00064000
RATEAMT6 DS    PL5'0.'                                                  00065000
TERM6    DS    PL3'0.'                                                  00066000
RATPCT6  DS    PL5'0.000000000'                                         00067000
RATEAMT7 DS    PL5'0.'                                                  00068000
TERM7    DS    PL3'0.'                                                  00069000
RATPCT7  DS    PL5'0.000000000'                                         00070000
RATEAMT8 DS    PL5'0.'                                                  00071000
TERM8    DS    PL3'0.'                                                  00072000
RATPCT8  DS    PL5'0.000000000'                                         00073000
RATEAMT9 DS    PL5'0.'                                                  00074000
TERM9    DS    PL3'0.'                                                  00075000
RATPCT9  DS    PL5'0.000000000'                                         00076000
RATEAT10 DS    PL5'0.'                                                  00077000
TERM10   DS    PL3'0.'                                                  00078000
RATPCT10 DS    PL5'0.000000000'                                         00079000
RATAMT11 DS    PL5'0.'                                                  00080000
TERM11   DS    PL3'0.'                                                  00081000
RATPCT11 DS    PL5'0.000000000'                                         00082000
RATAMT12 DS    PL5'0.'                                                  00083000
TERM12   DS    PL3'0.'                                                  00084000
RATPCT12 DS    PL5'0.000000000'                                         00085000
RATAMT13 DS    PL5'0.'                                                  00086000
TERM13   DS    PL3'0.'                                                  00087000
RATPCT13 DS    PL5'0.000000000'                                         00088000
RATAMT14 DS    PL5'0.'                                                  00089000
TERM14   DS    PL3'0.'                                                  00090000
RATPCT14 DS    PL5'0.000000000'                                         00091000
RATAMT15 DS    PL5'0.'                                                  00092000
TERM15   DS    PL3'0.'                                                  00093000
RATPCT15 DS    PL5'0.000000000'                                         00094000
*                                                                       00095000
*                                                                       00096000
**********************************************************************  00097000
* PROGRAM TABLE HEADER SECTION:                                         00098000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00099000
*   THE CONTENTS OF THIS SECTION SHOULD NOT BE MODIFIED.                00100000
**********************************************************************  00101000
*                                                                       00102000
S03IODB  CSECT                         PROGRAM TABLE SECTION            00103000
S03IODB  AMODE ANY                                                      00104000
S03IODB  RMODE ANY                                                      00105000
         DC    CL9'S03IODB'            PROGRAM ID                       00106000
         DC    CL8'&SYSDATE',CL1' ',CL5'&SYSTIME',CL1' '                00107000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00108000
         DC    A(0)                    RESERVED                         00109000
         DC    A(0)                    RESERVED                         00110000
         DC    24CL1' '                RESERVED                         00111000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00112000
*                                                                       00113000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00114000
         DC    CL29'WWW.INFOR.COM                '                      00114001
*                                                                       00115000
*                                                                       00116000
**********************************************************************  00117000
* STATEMENT TABLE SECTION:                                              00118000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00119000
*   THE CONTENTS AND ORDER OF THE STATEMENTS IN THIS SECTION            00120000
*   SHOULD NOT BE MODIFIED.                                             00121000
**********************************************************************  00122000
*                                                                       00123000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00124000
STM#TAB  AMODE ANY                                                      00125000
STM#TAB  RMODE ANY                                                      00126000
         DC    A(SQL#0000)             SELECT INTO (KEY 0)              00127000
         DC    A(SQL#0001)             SELECT INTO (KEY 1)              00128000
         DC    A(SQL#0002)             SELECT INTO (KEY 2)              00129000
         DC    A(SQL#0003)             SELECT INTO (KEY 3)              00130000
         DC    A(SQL#0004)             SELECT UPDATE CURSOR (KEY 0)     00131000
         DC    A(SQL#0005)             SELECT UPDATE CURSOR (KEY 1)     00132000
         DC    A(SQL#0006)             SELECT UPDATE CURSOR (KEY 2)     00133000
         DC    A(SQL#0007)             SELECT UPDATE CURSOR (KEY 3)     00134000
         DC    A(SQL#0008)             FETCH UPDATE CURSOR (KEY 0)      00135000
         DC    A(SQL#0009)             FETCH UPDATE CURSOR (KEY 1)      00136000
         DC    A(SQL#0010)             FETCH UPDATE CURSOR (KEY 2)      00137000
         DC    A(SQL#0011)             FETCH UPDATE CURSOR (KEY 3)      00138000
         DC    A(SQL#0012)             CLOSE UPDATE CURSOR (KEY 0)      00139000
         DC    A(SQL#0013)             CLOSE UPDATE CURSOR (KEY 1)      00140000
         DC    A(SQL#0014)             CLOSE UPDATE CURSOR (KEY 2)      00141000
         DC    A(SQL#0015)             CLOSE UPDATE CURSOR (KEY 3)      00142000
         DC    A(SQL#0016)             SELECT SEQ CURSOR (KEY 0)        00143000
         DC    A(SQL#0017)             SELECT SEQ CURSOR (KEY 1)        00144000
         DC    A(SQL#0018)             SELECT SEQ CURSOR (KEY 2)        00145000
         DC    A(SQL#0019)             SELECT SEQ CURSOR (KEY 3)        00146000
         DC    A(SQL#0020)             FETCH SEQ CURSOR (KEY 0)         00147000
         DC    A(SQL#0021)             FETCH SEQ CURSOR (KEY 1)         00148000
         DC    A(SQL#0022)             FETCH SEQ CURSOR (KEY 2)         00149000
         DC    A(SQL#0023)             FETCH SEQ CURSOR (KEY 3)         00150000
         DC    A(SQL#0024)             FETCH SEQ CURSOR UPDATE (KEY 0)  00151000
         DC    A(SQL#0025)             FETCH SEQ CURSOR UPDATE (KEY 1)  00152000
         DC    A(SQL#0026)             FETCH SEQ CURSOR UPDATE (KEY 2)  00153000
         DC    A(SQL#0027)             FETCH SEQ CURSOR UPDATE (KEY 3)  00154000
         DC    A(SQL#0028)             CLOSE SEQ CURSOR (KEY 0)         00155000
         DC    A(SQL#0029)             CLOSE SEQ CURSOR (KEY 1)         00156000
         DC    A(SQL#0030)             CLOSE SEQ CURSOR (KEY 2)         00157000
         DC    A(SQL#0031)             CLOSE SEQ CURSOR (KEY 3)         00158000
         DC    A(SQL#0032)             SELECT KEY (KEY 0)               00159000
         DC    A(SQL#0033)             SELECT KEY (KEY 1)               00160000
         DC    A(SQL#0034)             SELECT KEY (KEY 2)               00161000
         DC    A(SQL#0035)             SELECT KEY (KEY 3)               00162000
         DC    A(SQL#0036)             INSERT STATEMENT                 00163000
         DC    A(SQL#0037)             UPDATE STATEMENT (KEY 0)         00164000
         DC    A(SQL#0038)             UPDATE STATEMENT (KEY 1)         00165000
         DC    A(SQL#0039)             UPDATE STATEMENT (KEY 2)         00166000
         DC    A(SQL#0040)             UPDATE STATEMENT (KEY 3)         00167000
         DC    A(SQL#0041)             DELETE STATEMENT (KEY 0)         00168000
         DC    A(SQL#0042)             DELETE STATEMENT (KEY 1)         00169000
         DC    A(SQL#0043)             DELETE STATEMENT (KEY 2)         00170000
         DC    A(SQL#0044)             DELETE STATEMENT (KEY 3)         00171000
         DC    A(SQL#0045)             DELETE ALL STATEMENT             00172000
         DC    4X'FF'                                                   00173000
*                                                                       00174000
*                                                                       00175000
**********************************************************************  00176000
* SQL STATEMENT SECTION:                                                00177000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00178000
*     TO SUPPORT THIS TABLE.                                            00179000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00180000
*     ARE EQUIVALENT.                                                   00181000
**********************************************************************  00182000
*                                                                       00183000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00184000
SQL#STMT AMODE ANY                                                      00185000
SQL#STMT RMODE ANY                                                      00186000
         USING HOST#VAR,2              ADDRESS HOST VARIABLES           00187000
         USING SQLDSECT,10,3           ADDRESS SQLDSECT                 00188000
         USING COM#AREA,11             ADDRESS COMMAREA                 00189000
*                                                                       00190000
*                                                                       00191000
**********************************************************************  00192000
* SELECT INTO STATEMENT BY PRIMARY KEY:                                 00193000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00194000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00195000
**********************************************************************  00196000
*                                                                       00197000
SQL#0000 DS    0H                                                       00198000
         USING SQL#0000,12             ESTABLISH BASE REGISTER          00199000
         B     *+6                     BRANCH AROUND ADCON              00200000
BASE0000 DC    AL2(4096)                                                00201000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00202000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00203000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00204000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00205000
         AH    3,BASE0000              ADD 4K                           00206000
         EXEC  SQL                                                     *00207000
               SELECT AUDIT_DATE,                                      *00208000
                   AUDIT_TIME,                                         *00209000
                   AUDIT_USER,                                         *00210000
                   AUDIT_ORG,                                          *00211000
                   BAL_OPT,                                            *00212000
                   RATE_OPTION,                                        *00213000
                   BAL_ORDER,                                          *00214000
                   TERM_BAL,                                           *00215000
                   RATE_AMT_1,                                         *00216000
                   TERM_1,                                             *00217000
                   RATE_PCT_1,                                         *00218000
                   RATE_AMT_2,                                         *00219000
                   TERM_2,                                             *00220000
                   RATE_PCT_2,                                         *00221000
                   RATE_AMT_3,                                         *00222000
                   TERM_3,                                             *00223000
                   RATE_PCT_3,                                         *00224000
                   RATE_AMT_4,                                         *00225000
                   TERM_4,                                             *00226000
                   RATE_PCT_4,                                         *00227000
                   RATE_AMT_5,                                         *00228000
                   TERM_5,                                             *00229000
                   RATE_PCT_5,                                         *00230000
                   RATE_AMT_6,                                         *00231000
                   TERM_6,                                             *00232000
                   RATE_PCT_6,                                         *00233000
                   RATE_AMT_7,                                         *00234000
                   TERM_7,                                             *00235000
                   RATE_PCT_7,                                         *00236000
                   RATE_AMT_8,                                         *00237000
                   TERM_8,                                             *00238000
                   RATE_PCT_8,                                         *00239000
                   RATE_AMT_9,                                         *00240000
                   TERM_9,                                             *00241000
                   RATE_PCT_9,                                         *00242000
                   RATE_AMT_10,                                        *00243000
                   TERM_10,                                            *00244000
                   RATE_PCT_10,                                        *00245000
                   RATE_AMT_11,                                        *00246000
                   TERM_11,                                            *00247000
                   RATE_PCT_11,                                        *00248000
                   RATE_AMT_12,                                        *00249000
                   TERM_12,                                            *00250000
                   RATE_PCT_12,                                        *00251000
                   RATE_AMT_13,                                        *00252000
                   TERM_13,                                            *00253000
                   RATE_PCT_13,                                        *00254000
                   RATE_AMT_14,                                        *00255000
                   TERM_14,                                            *00256000
                   RATE_PCT_14,                                        *00257000
                   RATE_AMT_15,                                        *00258000
                   TERM_15,                                            *00259000
                   RATE_PCT_15                                         *00260000
                 INTO :AUDDATE,                                        *00261000
                   :AUDTIME,                                           *00262000
                   :AUDUSER,                                           *00263000
                   :AUDORG,                                            *00264000
                   :BALOPT,                                            *00265000
                   :RATEOPT,                                           *00266000
                   :BALORDR,                                           *00267000
                   :TERMBAL,                                           *00268000
                   :RATEAMT1,                                          *00269000
                   :TERM1,                                             *00270000
                   :RATPCT1,                                           *00271000
                   :RATEAMT2,                                          *00272000
                   :TERM2,                                             *00273000
                   :RATPCT2,                                           *00274000
                   :RATEAMT3,                                          *00275000
                   :TERM3,                                             *00276000
                   :RATPCT3,                                           *00277000
                   :RATEAMT4,                                          *00278000
                   :TERM4,                                             *00279000
                   :RATPCT4,                                           *00280000
                   :RATEAMT5,                                          *00281000
                   :TERM5,                                             *00282000
                   :RATPCT5,                                           *00283000
                   :RATEAMT6,                                          *00284000
                   :TERM6,                                             *00285000
                   :RATPCT6,                                           *00286000
                   :RATEAMT7,                                          *00287000
                   :TERM7,                                             *00288000
                   :RATPCT7,                                           *00289000
                   :RATEAMT8,                                          *00290000
                   :TERM8,                                             *00291000
                   :RATPCT8,                                           *00292000
                   :RATEAMT9,                                          *00293000
                   :TERM9,                                             *00294000
                   :RATPCT9,                                           *00295000
                   :RATEAT10,                                          *00296000
                   :TERM10,                                            *00297000
                   :RATPCT10,                                          *00298000
                   :RATAMT11,                                          *00299000
                   :TERM11,                                            *00300000
                   :RATPCT11,                                          *00301000
                   :RATAMT12,                                          *00302000
                   :TERM12,                                            *00303000
                   :RATPCT12,                                          *00304000
                   :RATAMT13,                                          *00305000
                   :TERM13,                                            *00306000
                   :RATPCT13,                                          *00307000
                   :RATAMT14,                                          *00308000
                   :TERM14,                                            *00309000
                   :RATPCT14,                                          *00310000
                   :RATAMT15,                                          *00311000
                   :TERM15,                                            *00312000
                   :RATPCT15                                           *00313000
                 FROM S03                                              *00314000
                 WHERE INST_NBR = :INST AND                            *00315000
                   RECORD_NBR = :RECNBR AND                            *00316000
                   RATE_TABLE = :RATETABL AND                          *00317000
                   EFFECTIVE_DATE = :EFFDATE                           *00318000
                 FETCH FIRST 1 ROW ONLY                                 00319000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   00320000
         BALR  4,5                     MOVE INTO RECORD AREA            00321000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00322000
         BR    14                      RETURN TO CALLER                 00323000
         LTORG                                                          00324000
*                                                                       00325000
*                                                                       00326000
**********************************************************************  00327000
* SELECT INTO STATEMENT BY ALTERNATE KEY 1:                             00328000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00329000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00330000
**********************************************************************  00331000
*                                                                       00332000
SQL#0001 DS    0H                                                       00333000
         USING SQL#0001,12             ESTABLISH BASE REGISTER          00334000
         LA    15,255                  SET RETURN CODE                  00335000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00336000
         BR    14                      RETURN TO CALLER                 00337000
         LTORG                                                          00338000
*                                                                       00339000
*                                                                       00340000
**********************************************************************  00341000
* SELECT INTO STATEMENT BY ALTERNATE KEY 2:                             00342000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00343000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00344000
**********************************************************************  00345000
*                                                                       00346000
SQL#0002 DS    0H                                                       00347000
         USING SQL#0002,12             ESTABLISH BASE REGISTER          00348000
         LA    15,255                  SET RETURN CODE                  00349000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00350000
         BR    14                      RETURN TO CALLER                 00351000
         LTORG                                                          00352000
*                                                                       00353000
*                                                                       00354000
**********************************************************************  00355000
* SELECT INTO STATEMENT BY ALTERNATE KEY 3:                             00356000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00357000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00358000
**********************************************************************  00359000
*                                                                       00360000
SQL#0003 DS    0H                                                       00361000
         USING SQL#0003,12             ESTABLISH BASE REGISTER          00362000
         LA    15,255                  SET RETURN CODE                  00363000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00364000
         BR    14                      RETURN TO CALLER                 00365000
         LTORG                                                          00366000
*                                                                       00367000
*                                                                       00368000
**********************************************************************  00369000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY PRIMARY KEY:       00370000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00371000
*   THEY ARE ALSO USED AFTER A SUCCESSFUL SELECT SEQUENTIAL STATEMENT   00372000
*     FOR THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS.                      00373000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00374000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00375000
*     UPDATE STATEMENT TO MATCH.                                        00376000
**********************************************************************  00377000
*                                                                       00378000
SQL#0004 DS    0H                                                       00379000
         USING SQL#0004,12             ESTABLISH BASE REGISTER          00380000
         B     *+6                     BRANCH AROUND ADCON              00381000
BASE0004 DC    AL2(4096)                                                00382000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00383000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00384000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00385000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00386000
         AH    3,BASE0004              ADD 4K                           00387000
         EXEC  SQL                                                     *00388000
               DECLARE S03UPD0 CURSOR FOR                              *00389000
               SELECT AUDIT_DATE,                                      *00390000
                   AUDIT_TIME,                                         *00391000
                   AUDIT_USER,                                         *00392000
                   AUDIT_ORG,                                          *00393000
                   BAL_OPT,                                            *00394000
                   RATE_OPTION,                                        *00395000
                   BAL_ORDER,                                          *00396000
                   TERM_BAL,                                           *00397000
                   RATE_AMT_1,                                         *00398000
                   TERM_1,                                             *00399000
                   RATE_PCT_1,                                         *00400000
                   RATE_AMT_2,                                         *00401000
                   TERM_2,                                             *00402000
                   RATE_PCT_2,                                         *00403000
                   RATE_AMT_3,                                         *00404000
                   TERM_3,                                             *00405000
                   RATE_PCT_3,                                         *00406000
                   RATE_AMT_4,                                         *00407000
                   TERM_4,                                             *00408000
                   RATE_PCT_4,                                         *00409000
                   RATE_AMT_5,                                         *00410000
                   TERM_5,                                             *00411000
                   RATE_PCT_5,                                         *00412000
                   RATE_AMT_6,                                         *00413000
                   TERM_6,                                             *00414000
                   RATE_PCT_6,                                         *00415000
                   RATE_AMT_7,                                         *00416000
                   TERM_7,                                             *00417000
                   RATE_PCT_7,                                         *00418000
                   RATE_AMT_8,                                         *00419000
                   TERM_8,                                             *00420000
                   RATE_PCT_8,                                         *00421000
                   RATE_AMT_9,                                         *00422000
                   TERM_9,                                             *00423000
                   RATE_PCT_9,                                         *00424000
                   RATE_AMT_10,                                        *00425000
                   TERM_10,                                            *00426000
                   RATE_PCT_10,                                        *00427000
                   RATE_AMT_11,                                        *00428000
                   TERM_11,                                            *00429000
                   RATE_PCT_11,                                        *00430000
                   RATE_AMT_12,                                        *00431000
                   TERM_12,                                            *00432000
                   RATE_PCT_12,                                        *00433000
                   RATE_AMT_13,                                        *00434000
                   TERM_13,                                            *00435000
                   RATE_PCT_13,                                        *00436000
                   RATE_AMT_14,                                        *00437000
                   TERM_14,                                            *00438000
                   RATE_PCT_14,                                        *00439000
                   RATE_AMT_15,                                        *00440000
                   TERM_15,                                            *00441000
                   RATE_PCT_15                                         *00442000
                 FROM S03                                              *00443000
                 WHERE INST_NBR = :INST AND                            *00444000
                   RECORD_NBR = :RECNBR AND                            *00445000
                   RATE_TABLE = :RATETABL AND                          *00446000
                   EFFECTIVE_DATE = :EFFDATE                           *00447000
                 FOR UPDATE OF AUDIT_DATE,                             *00448000
                   AUDIT_TIME,                                         *00449000
                   AUDIT_USER,                                         *00450000
                   AUDIT_ORG,                                          *00451000
                   BAL_OPT,                                            *00452000
                   RATE_OPTION,                                        *00453000
                   BAL_ORDER,                                          *00454000
                   TERM_BAL,                                           *00455000
                   RATE_AMT_1,                                         *00456000
                   TERM_1,                                             *00457000
                   RATE_PCT_1,                                         *00458000
                   RATE_AMT_2,                                         *00459000
                   TERM_2,                                             *00460000
                   RATE_PCT_2,                                         *00461000
                   RATE_AMT_3,                                         *00462000
                   TERM_3,                                             *00463000
                   RATE_PCT_3,                                         *00464000
                   RATE_AMT_4,                                         *00465000
                   TERM_4,                                             *00466000
                   RATE_PCT_4,                                         *00467000
                   RATE_AMT_5,                                         *00468000
                   TERM_5,                                             *00469000
                   RATE_PCT_5,                                         *00470000
                   RATE_AMT_6,                                         *00471000
                   TERM_6,                                             *00472000
                   RATE_PCT_6,                                         *00473000
                   RATE_AMT_7,                                         *00474000
                   TERM_7,                                             *00475000
                   RATE_PCT_7,                                         *00476000
                   RATE_AMT_8,                                         *00477000
                   TERM_8,                                             *00478000
                   RATE_PCT_8,                                         *00479000
                   RATE_AMT_9,                                         *00480000
                   TERM_9,                                             *00481000
                   RATE_PCT_9,                                         *00482000
                   RATE_AMT_10,                                        *00483000
                   TERM_10,                                            *00484000
                   RATE_PCT_10,                                        *00485000
                   RATE_AMT_11,                                        *00486000
                   TERM_11,                                            *00487000
                   RATE_PCT_11,                                        *00488000
                   RATE_AMT_12,                                        *00489000
                   TERM_12,                                            *00490000
                   RATE_PCT_12,                                        *00491000
                   RATE_AMT_13,                                        *00492000
                   TERM_13,                                            *00493000
                   RATE_PCT_13,                                        *00494000
                   RATE_AMT_14,                                        *00495000
                   TERM_14,                                            *00496000
                   RATE_PCT_14,                                        *00497000
                   RATE_AMT_15,                                        *00498000
                   TERM_15,                                            *00499000
                   RATE_PCT_15                                         *00500000
                 FETCH FIRST 1 ROW ONLY                                 00501000
         EXEC  SQL                                                     *00502000
               OPEN S03UPD0                                             00503000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00504000
         BR    14                      RETURN TO CALLER                 00505000
         LTORG                                                          00506000
*                                                                       00507000
*                                                                       00508000
**********************************************************************  00509000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 1:   00510000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00511000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00512000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00513000
*     UPDATE STATEMENT TO MATCH.                                        00514000
**********************************************************************  00515000
*                                                                       00516000
SQL#0005 DS    0H                                                       00517000
         USING SQL#0005,12             ESTABLISH BASE REGISTER          00518000
         LA    15,255                  SET RETURN CODE                  00519000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00520000
         BR    14                      RETURN TO CALLER                 00521000
         LTORG                                                          00522000
*                                                                       00523000
*                                                                       00524000
**********************************************************************  00525000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 2:   00526000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00527000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00528000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00529000
*     UPDATE STATEMENT TO MATCH.                                        00530000
**********************************************************************  00531000
*                                                                       00532000
SQL#0006 DS    0H                                                       00533000
         USING SQL#0006,12             ESTABLISH BASE REGISTER          00534000
         LA    15,255                  SET RETURN CODE                  00535000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00536000
         BR    14                      RETURN TO CALLER                 00537000
         LTORG                                                          00538000
*                                                                       00539000
*                                                                       00540000
**********************************************************************  00541000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 3:   00542000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00543000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00544000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00545000
*     UPDATE STATEMENT TO MATCH.                                        00546000
**********************************************************************  00547000
*                                                                       00548000
SQL#0007 DS    0H                                                       00549000
         USING SQL#0007,12             ESTABLISH BASE REGISTER          00550000
         LA    15,255                  SET RETURN CODE                  00551000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00552000
         BR    14                      RETURN TO CALLER                 00553000
         LTORG                                                          00554000
*                                                                       00555000
*                                                                       00556000
**********************************************************************  00557000
* FETCH FROM UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                   00558000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00559000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00560000
*     THE ACTUAL ROW.                                                   00561000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00562000
**********************************************************************  00563000
*                                                                       00564000
SQL#0008 DS    0H                                                       00565000
         USING SQL#0008,12             ESTABLISH BASE REGISTER          00566000
         B     *+6                     BRANCH AROUND ADCON              00567000
BASE0008 DC    AL2(4096)                                                00568000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00569000
         AH    3,BASE0008              ADD 4K                           00570000
         EXEC  SQL                                                     *00571000
               FETCH S03UPD0                                           *00572000
                 INTO :AUDDATE,                                        *00573000
                   :AUDTIME,                                           *00574000
                   :AUDUSER,                                           *00575000
                   :AUDORG,                                            *00576000
                   :BALOPT,                                            *00577000
                   :RATEOPT,                                           *00578000
                   :BALORDR,                                           *00579000
                   :TERMBAL,                                           *00580000
                   :RATEAMT1,                                          *00581000
                   :TERM1,                                             *00582000
                   :RATPCT1,                                           *00583000
                   :RATEAMT2,                                          *00584000
                   :TERM2,                                             *00585000
                   :RATPCT2,                                           *00586000
                   :RATEAMT3,                                          *00587000
                   :TERM3,                                             *00588000
                   :RATPCT3,                                           *00589000
                   :RATEAMT4,                                          *00590000
                   :TERM4,                                             *00591000
                   :RATPCT4,                                           *00592000
                   :RATEAMT5,                                          *00593000
                   :TERM5,                                             *00594000
                   :RATPCT5,                                           *00595000
                   :RATEAMT6,                                          *00596000
                   :TERM6,                                             *00597000
                   :RATPCT6,                                           *00598000
                   :RATEAMT7,                                          *00599000
                   :TERM7,                                             *00600000
                   :RATPCT7,                                           *00601000
                   :RATEAMT8,                                          *00602000
                   :TERM8,                                             *00603000
                   :RATPCT8,                                           *00604000
                   :RATEAMT9,                                          *00605000
                   :TERM9,                                             *00606000
                   :RATPCT9,                                           *00607000
                   :RATEAT10,                                          *00608000
                   :TERM10,                                            *00609000
                   :RATPCT10,                                          *00610000
                   :RATAMT11,                                          *00611000
                   :TERM11,                                            *00612000
                   :RATPCT11,                                          *00613000
                   :RATAMT12,                                          *00614000
                   :TERM12,                                            *00615000
                   :RATPCT12,                                          *00616000
                   :RATAMT13,                                          *00617000
                   :TERM13,                                            *00618000
                   :RATPCT13,                                          *00619000
                   :RATAMT14,                                          *00620000
                   :TERM14,                                            *00621000
                   :RATPCT14,                                          *00622000
                   :RATAMT15,                                          *00623000
                   :TERM15,                                            *00624000
                   :RATPCT15                                            00625000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   00626000
         BALR  4,5                     MOVE INTO RECORD AREA            00627000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00628000
         BR    14                      RETURN TO CALLER                 00629000
         LTORG                                                          00630000
*                                                                       00631000
*                                                                       00632000
**********************************************************************  00633000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 1:               00634000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00635000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00636000
*     THE ACTUAL ROW.                                                   00637000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00638000
**********************************************************************  00639000
*                                                                       00640000
SQL#0009 DS    0H                                                       00641000
         USING SQL#0009,12             ESTABLISH BASE REGISTER          00642000
         LA    15,255                  SET RETURN CODE                  00643000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00644000
         BR    14                      RETURN TO CALLER                 00645000
         LTORG                                                          00646000
*                                                                       00647000
*                                                                       00648000
**********************************************************************  00649000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 2:               00650000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00651000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00652000
*     THE ACTUAL ROW.                                                   00653000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00654000
**********************************************************************  00655000
*                                                                       00656000
SQL#0010 DS    0H                                                       00657000
         USING SQL#0010,12             ESTABLISH BASE REGISTER          00658000
         LA    15,255                  SET RETURN CODE                  00659000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00660000
         BR    14                      RETURN TO CALLER                 00661000
         LTORG                                                          00662000
*                                                                       00663000
*                                                                       00664000
**********************************************************************  00665000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 3:               00666000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00667000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00668000
*     THE ACTUAL ROW.                                                   00669000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00670000
**********************************************************************  00671000
*                                                                       00672000
SQL#0011 DS    0H                                                       00673000
         USING SQL#0011,12             ESTABLISH BASE REGISTER          00674000
         LA    15,255                  SET RETURN CODE                  00675000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00676000
         BR    14                      RETURN TO CALLER                 00677000
         LTORG                                                          00678000
*                                                                       00679000
*                                                                       00680000
**********************************************************************  00681000
* CLOSE UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                        00682000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00683000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00684000
*     TO CLOSE THE UPDATE CURSOR.                                       00685000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00686000
**********************************************************************  00687000
*                                                                       00688000
SQL#0012 DS    0H                                                       00689000
         USING SQL#0012,12             ESTABLISH BASE REGISTER          00690000
         B     *+6                     BRANCH AROUND ADCON              00691000
BASE0012 DC    AL2(4096)                                                00692000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00693000
         AH    3,BASE0012              ADD 4K                           00694000
         EXEC  SQL                                                     *00695000
               CLOSE S03UPD0                                            00696000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00697000
         BR    14                      RETURN TO CALLER                 00698000
         LTORG                                                          00699000
*                                                                       00700000
*                                                                       00701000
**********************************************************************  00702000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 1:                    00703000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00704000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00705000
*     TO CLOSE THE UPDATE CURSOR.                                       00706000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00707000
**********************************************************************  00708000
*                                                                       00709000
SQL#0013 DS    0H                                                       00710000
         USING SQL#0013,12             ESTABLISH BASE REGISTER          00711000
         LA    15,255                  SET RETURN CODE                  00712000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00713000
         BR    14                      RETURN TO CALLER                 00714000
         LTORG                                                          00715000
*                                                                       00716000
*                                                                       00717000
**********************************************************************  00718000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 2:                    00719000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00720000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00721000
*     TO CLOSE THE UPDATE CURSOR.                                       00722000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00723000
**********************************************************************  00724000
*                                                                       00725000
SQL#0014 DS    0H                                                       00726000
         USING SQL#0014,12             ESTABLISH BASE REGISTER          00727000
         LA    15,255                  SET RETURN CODE                  00728000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00729000
         BR    14                      RETURN TO CALLER                 00730000
         LTORG                                                          00731000
*                                                                       00732000
*                                                                       00733000
**********************************************************************  00734000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 3:                    00735000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00736000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00737000
*     TO CLOSE THE UPDATE CURSOR.                                       00738000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00739000
**********************************************************************  00740000
*                                                                       00741000
SQL#0015 DS    0H                                                       00742000
         USING SQL#0015,12             ESTABLISH BASE REGISTER          00743000
         LA    15,255                  SET RETURN CODE                  00744000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00745000
         BR    14                      RETURN TO CALLER                 00746000
         LTORG                                                          00747000
*                                                                       00748000
*                                                                       00749000
**********************************************************************  00750000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY    00751000
* KEY:                                                                  00752000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00753000
*     AND GET-NEXT-LOCK VERBS.                                          00754000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00755000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00756000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00757000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     00758000
*     ORDER BY CLAUSE.                                                  00759000
**********************************************************************  00760000
*                                                                       00761000
SQL#0016 DS    0H                                                       00762000
         USING SQL#0016,12             ESTABLISH BASE REGISTER          00763000
         B     *+6                     BRANCH AROUND ADCON              00764000
BASE0016 DC    AL2(4096)                                                00765000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00766000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00767000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00768000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00769000
         AH    3,BASE0016              ADD 4K                           00770000
         EXEC  SQL                                                     *00771000
               DECLARE S03SQ001 CURSOR FOR                             *00772000
               SELECT INST_NBR,                                        *00773000
                   RECORD_NBR,                                         *00774000
                   RATE_TABLE,                                         *00775000
                   EFFECTIVE_DATE,                                     *00776000
                   AUDIT_DATE,                                         *00777000
                   AUDIT_TIME,                                         *00778000
                   AUDIT_USER,                                         *00779000
                   AUDIT_ORG,                                          *00780000
                   BAL_OPT,                                            *00781000
                   RATE_OPTION,                                        *00782000
                   BAL_ORDER,                                          *00783000
                   TERM_BAL,                                           *00784000
                   RATE_AMT_1,                                         *00785000
                   TERM_1,                                             *00786000
                   RATE_PCT_1,                                         *00787000
                   RATE_AMT_2,                                         *00788000
                   TERM_2,                                             *00789000
                   RATE_PCT_2,                                         *00790000
                   RATE_AMT_3,                                         *00791000
                   TERM_3,                                             *00792000
                   RATE_PCT_3,                                         *00793000
                   RATE_AMT_4,                                         *00794000
                   TERM_4,                                             *00795000
                   RATE_PCT_4,                                         *00796000
                   RATE_AMT_5,                                         *00797000
                   TERM_5,                                             *00798000
                   RATE_PCT_5,                                         *00799000
                   RATE_AMT_6,                                         *00800000
                   TERM_6,                                             *00801000
                   RATE_PCT_6,                                         *00802000
                   RATE_AMT_7,                                         *00803000
                   TERM_7,                                             *00804000
                   RATE_PCT_7,                                         *00805000
                   RATE_AMT_8,                                         *00806000
                   TERM_8,                                             *00807000
                   RATE_PCT_8,                                         *00808000
                   RATE_AMT_9,                                         *00809000
                   TERM_9,                                             *00810000
                   RATE_PCT_9,                                         *00811000
                   RATE_AMT_10,                                        *00812000
                   TERM_10,                                            *00813000
                   RATE_PCT_10,                                        *00814000
                   RATE_AMT_11,                                        *00815000
                   TERM_11,                                            *00816000
                   RATE_PCT_11,                                        *00817000
                   RATE_AMT_12,                                        *00818000
                   TERM_12,                                            *00819000
                   RATE_PCT_12,                                        *00820000
                   RATE_AMT_13,                                        *00821000
                   TERM_13,                                            *00822000
                   RATE_PCT_13,                                        *00823000
                   RATE_AMT_14,                                        *00824000
                   TERM_14,                                            *00825000
                   RATE_PCT_14,                                        *00826000
                   RATE_AMT_15,                                        *00827000
                   TERM_15,                                            *00828000
                   RATE_PCT_15                                         *00829000
                 FROM S03                                              *00830000
                 WHERE                                                 *00831000
                    INST_NBR = :INST AND                               *00832000
                    RECORD_NBR = :RECNBR AND                           *00833000
                    RATE_TABLE = :RATETABL AND                         *00834000
                    EFFECTIVE_DATE >= :EFFDATE                         *00835000
                 ORDER BY EFFECTIVE_DATE                               *00836000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00837000
         EXEC  SQL                                                     *00838000
               OPEN S03SQ001                                            00839000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00840000
         BR    14                      RETURN TO CALLER                 00841000
         LTORG                                                          00842000
*                                                                       00843000
*                                                                       00844000
**********************************************************************  00845000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  00846000
* KEY 1:                                                                00847000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00848000
*     AND GET-NEXT-LOCK VERBS.                                          00849000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00850000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00851000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00852000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     00853000
*     ORDER BY CLAUSE.                                                  00854000
**********************************************************************  00855000
*                                                                       00856000
SQL#0017 DS    0H                                                       00857000
         USING SQL#0017,12             ESTABLISH BASE REGISTER          00858000
         LA    15,255                  SET RETURN CODE                  00859000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00860000
         BR    14                      RETURN TO CALLER                 00861000
         LTORG                                                          00862000
*                                                                       00863000
*                                                                       00864000
**********************************************************************  00865000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  00866000
* KEY 2:                                                                00867000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00868000
*     AND GET-NEXT-LOCK VERBS.                                          00869000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00870000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00871000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00872000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     00873000
*     ORDER BY CLAUSE.                                                  00874000
**********************************************************************  00875000
*                                                                       00876000
SQL#0018 DS    0H                                                       00877000
         USING SQL#0018,12             ESTABLISH BASE REGISTER          00878000
         LA    15,255                  SET RETURN CODE                  00879000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00880000
         BR    14                      RETURN TO CALLER                 00881000
         LTORG                                                          00882000
*                                                                       00883000
*                                                                       00884000
**********************************************************************  00885000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  00886000
* KEY 3:                                                                00887000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00888000
*     AND GET-NEXT-LOCK VERBS.                                          00889000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00890000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00891000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00892000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     00893000
*     ORDER BY CLAUSE.                                                  00894000
**********************************************************************  00895000
*                                                                       00896000
SQL#0019 DS    0H                                                       00897000
         USING SQL#0019,12             ESTABLISH BASE REGISTER          00898000
         LA    15,255                  SET RETURN CODE                  00899000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00900000
         BR    14                      RETURN TO CALLER                 00901000
         LTORG                                                          00902000
*                                                                       00903000
*                                                                       00904000
**********************************************************************  00905000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:               00906000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              00907000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          00908000
*     RETRIEVE THE ACTUAL ROW.                                          00909000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             00910000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                00911000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00912000
**********************************************************************  00913000
*                                                                       00914000
SQL#0020 DS    0H                                                       00915000
         USING SQL#0020,12             ESTABLISH BASE REGISTER          00916000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      00917000
         LA    12,VECT0020(1)          LOAD POINTER TO FETCH ROUTINE    00918000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       00919000
         BR    12                      GO TO CURRENT FETCH ROUTINE      00920000
VECT0020 DC    A(SQL#0120)                                              00921000
         DC    A(SQL#0220)                                              00922000
         DC    A(SQL#0320)                                              00923000
         DC    A(SQL#0420)                                              00924000
         LTORG                                                          00925000
*                                                                       00926000
SQL#0120 DS    0H                                                       00927000
         USING SQL#0120,12             ESTABLISH BASE REGISTER          00928000
         B     *+6                     BRANCH AROUND ADCON              00929000
BASE0120 DC    AL2(4096)                                                00930000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00931000
         AH    3,BASE0120              ADD 4K                           00932000
         EXEC  SQL                                                     *00933000
               FETCH S03SQ001                                          *00934000
                 INTO :INST,                                           *00935000
                      :RECNBR,                                         *00936000
                      :RATETABL,                                       *00937000
                      :EFFDATE,                                        *00938000
                      :AUDDATE,                                        *00939000
                      :AUDTIME,                                        *00940000
                      :AUDUSER,                                        *00941000
                      :AUDORG,                                         *00942000
                      :BALOPT,                                         *00943000
                      :RATEOPT,                                        *00944000
                      :BALORDR,                                        *00945000
                      :TERMBAL,                                        *00946000
                      :RATEAMT1,                                       *00947000
                      :TERM1,                                          *00948000
                      :RATPCT1,                                        *00949000
                      :RATEAMT2,                                       *00950000
                      :TERM2,                                          *00951000
                      :RATPCT2,                                        *00952000
                      :RATEAMT3,                                       *00953000
                      :TERM3,                                          *00954000
                      :RATPCT3,                                        *00955000
                      :RATEAMT4,                                       *00956000
                      :TERM4,                                          *00957000
                      :RATPCT4,                                        *00958000
                      :RATEAMT5,                                       *00959000
                      :TERM5,                                          *00960000
                      :RATPCT5,                                        *00961000
                      :RATEAMT6,                                       *00962000
                      :TERM6,                                          *00963000
                      :RATPCT6,                                        *00964000
                      :RATEAMT7,                                       *00965000
                      :TERM7,                                          *00966000
                      :RATPCT7,                                        *00967000
                      :RATEAMT8,                                       *00968000
                      :TERM8,                                          *00969000
                      :RATPCT8,                                        *00970000
                      :RATEAMT9,                                       *00971000
                      :TERM9,                                          *00972000
                      :RATPCT9,                                        *00973000
                      :RATEAT10,                                       *00974000
                      :TERM10,                                         *00975000
                      :RATPCT10,                                       *00976000
                      :RATAMT11,                                       *00977000
                      :TERM11,                                         *00978000
                      :RATPCT11,                                       *00979000
                      :RATAMT12,                                       *00980000
                      :TERM12,                                         *00981000
                      :RATPCT12,                                       *00982000
                      :RATAMT13,                                       *00983000
                      :TERM13,                                         *00984000
                      :RATPCT13,                                       *00985000
                      :RATAMT14,                                       *00986000
                      :TERM14,                                         *00987000
                      :RATPCT14,                                       *00988000
                      :RATAMT15,                                       *00989000
                      :TERM15,                                         *00990000
                      :RATPCT15                                         00991000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          00992000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       00993000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      00994000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     00995000
         BR    12                      OPEN NEXT CURSOR                 00996000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   00997000
         BALR  4,5                     MOVE INTO RECORD AREA            00998000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00999000
         BR    14                      RETURN TO CALLER                 01000000
         LTORG                                                          01001000
*                                                                       01002000
SQL#0220 DS    0H                                                       01003000
         USING SQL#0220,12             ESTABLISH BASE REGISTER          01004000
         B     *+6                     BRANCH AROUND ADCON              01005000
BASE0220 DC    AL2(4096)                                                01006000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01007000
         AH    3,BASE0220              ADD 4K                           01008000
         EXEC  SQL                                                     *01009000
               FETCH S03SQ002                                          *01010000
                 INTO :INST,                                           *01011000
                      :RECNBR,                                         *01012000
                      :RATETABL,                                       *01013000
                      :EFFDATE,                                        *01014000
                      :AUDDATE,                                        *01015000
                      :AUDTIME,                                        *01016000
                      :AUDUSER,                                        *01017000
                      :AUDORG,                                         *01018000
                      :BALOPT,                                         *01019000
                      :RATEOPT,                                        *01020000
                      :BALORDR,                                        *01021000
                      :TERMBAL,                                        *01022000
                      :RATEAMT1,                                       *01023000
                      :TERM1,                                          *01024000
                      :RATPCT1,                                        *01025000
                      :RATEAMT2,                                       *01026000
                      :TERM2,                                          *01027000
                      :RATPCT2,                                        *01028000
                      :RATEAMT3,                                       *01029000
                      :TERM3,                                          *01030000
                      :RATPCT3,                                        *01031000
                      :RATEAMT4,                                       *01032000
                      :TERM4,                                          *01033000
                      :RATPCT4,                                        *01034000
                      :RATEAMT5,                                       *01035000
                      :TERM5,                                          *01036000
                      :RATPCT5,                                        *01037000
                      :RATEAMT6,                                       *01038000
                      :TERM6,                                          *01039000
                      :RATPCT6,                                        *01040000
                      :RATEAMT7,                                       *01041000
                      :TERM7,                                          *01042000
                      :RATPCT7,                                        *01043000
                      :RATEAMT8,                                       *01044000
                      :TERM8,                                          *01045000
                      :RATPCT8,                                        *01046000
                      :RATEAMT9,                                       *01047000
                      :TERM9,                                          *01048000
                      :RATPCT9,                                        *01049000
                      :RATEAT10,                                       *01050000
                      :TERM10,                                         *01051000
                      :RATPCT10,                                       *01052000
                      :RATAMT11,                                       *01053000
                      :TERM11,                                         *01054000
                      :RATPCT11,                                       *01055000
                      :RATAMT12,                                       *01056000
                      :TERM12,                                         *01057000
                      :RATPCT12,                                       *01058000
                      :RATAMT13,                                       *01059000
                      :TERM13,                                         *01060000
                      :RATPCT13,                                       *01061000
                      :RATAMT14,                                       *01062000
                      :TERM14,                                         *01063000
                      :RATPCT14,                                       *01064000
                      :RATAMT15,                                       *01065000
                      :TERM15,                                         *01066000
                      :RATPCT15                                         01067000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01068000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01069000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01070000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01071000
         BR    12                      OPEN NEXT CURSOR                 01072000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01073000
         BALR  4,5                     MOVE INTO RECORD AREA            01074000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01075000
         BR    14                      RETURN TO CALLER                 01076000
         LTORG                                                          01077000
*                                                                       01078000
SQL#0320 DS    0H                                                       01079000
         USING SQL#0320,12             ESTABLISH BASE REGISTER          01080000
         B     *+6                     BRANCH AROUND ADCON              01081000
BASE0320 DC    AL2(4096)                                                01082000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01083000
         AH    3,BASE0320              ADD 4K                           01084000
         EXEC  SQL                                                     *01085000
               FETCH S03SQ003                                          *01086000
                 INTO :INST,                                           *01087000
                      :RECNBR,                                         *01088000
                      :RATETABL,                                       *01089000
                      :EFFDATE,                                        *01090000
                      :AUDDATE,                                        *01091000
                      :AUDTIME,                                        *01092000
                      :AUDUSER,                                        *01093000
                      :AUDORG,                                         *01094000
                      :BALOPT,                                         *01095000
                      :RATEOPT,                                        *01096000
                      :BALORDR,                                        *01097000
                      :TERMBAL,                                        *01098000
                      :RATEAMT1,                                       *01099000
                      :TERM1,                                          *01100000
                      :RATPCT1,                                        *01101000
                      :RATEAMT2,                                       *01102000
                      :TERM2,                                          *01103000
                      :RATPCT2,                                        *01104000
                      :RATEAMT3,                                       *01105000
                      :TERM3,                                          *01106000
                      :RATPCT3,                                        *01107000
                      :RATEAMT4,                                       *01108000
                      :TERM4,                                          *01109000
                      :RATPCT4,                                        *01110000
                      :RATEAMT5,                                       *01111000
                      :TERM5,                                          *01112000
                      :RATPCT5,                                        *01113000
                      :RATEAMT6,                                       *01114000
                      :TERM6,                                          *01115000
                      :RATPCT6,                                        *01116000
                      :RATEAMT7,                                       *01117000
                      :TERM7,                                          *01118000
                      :RATPCT7,                                        *01119000
                      :RATEAMT8,                                       *01120000
                      :TERM8,                                          *01121000
                      :RATPCT8,                                        *01122000
                      :RATEAMT9,                                       *01123000
                      :TERM9,                                          *01124000
                      :RATPCT9,                                        *01125000
                      :RATEAT10,                                       *01126000
                      :TERM10,                                         *01127000
                      :RATPCT10,                                       *01128000
                      :RATAMT11,                                       *01129000
                      :TERM11,                                         *01130000
                      :RATPCT11,                                       *01131000
                      :RATAMT12,                                       *01132000
                      :TERM12,                                         *01133000
                      :RATPCT12,                                       *01134000
                      :RATAMT13,                                       *01135000
                      :TERM13,                                         *01136000
                      :RATPCT13,                                       *01137000
                      :RATAMT14,                                       *01138000
                      :TERM14,                                         *01139000
                      :RATPCT14,                                       *01140000
                      :RATAMT15,                                       *01141000
                      :TERM15,                                         *01142000
                      :RATPCT15                                         01143000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01144000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01145000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01146000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01147000
         BR    12                      OPEN NEXT CURSOR                 01148000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01149000
         BALR  4,5                     MOVE INTO RECORD AREA            01150000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01151000
         BR    14                      RETURN TO CALLER                 01152000
         LTORG                                                          01153000
*                                                                       01154000
SQL#0420 DS    0H                                                       01155000
         USING SQL#0420,12             ESTABLISH BASE REGISTER          01156000
         B     *+6                     BRANCH AROUND ADCON              01157000
BASE0420 DC    AL2(4096)                                                01158000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01159000
         AH    3,BASE0420              ADD 4K                           01160000
         EXEC  SQL                                                     *01161000
               FETCH S03SQ004                                          *01162000
                 INTO :INST,                                           *01163000
                      :RECNBR,                                         *01164000
                      :RATETABL,                                       *01165000
                      :EFFDATE,                                        *01166000
                      :AUDDATE,                                        *01167000
                      :AUDTIME,                                        *01168000
                      :AUDUSER,                                        *01169000
                      :AUDORG,                                         *01170000
                      :BALOPT,                                         *01171000
                      :RATEOPT,                                        *01172000
                      :BALORDR,                                        *01173000
                      :TERMBAL,                                        *01174000
                      :RATEAMT1,                                       *01175000
                      :TERM1,                                          *01176000
                      :RATPCT1,                                        *01177000
                      :RATEAMT2,                                       *01178000
                      :TERM2,                                          *01179000
                      :RATPCT2,                                        *01180000
                      :RATEAMT3,                                       *01181000
                      :TERM3,                                          *01182000
                      :RATPCT3,                                        *01183000
                      :RATEAMT4,                                       *01184000
                      :TERM4,                                          *01185000
                      :RATPCT4,                                        *01186000
                      :RATEAMT5,                                       *01187000
                      :TERM5,                                          *01188000
                      :RATPCT5,                                        *01189000
                      :RATEAMT6,                                       *01190000
                      :TERM6,                                          *01191000
                      :RATPCT6,                                        *01192000
                      :RATEAMT7,                                       *01193000
                      :TERM7,                                          *01194000
                      :RATPCT7,                                        *01195000
                      :RATEAMT8,                                       *01196000
                      :TERM8,                                          *01197000
                      :RATPCT8,                                        *01198000
                      :RATEAMT9,                                       *01199000
                      :TERM9,                                          *01200000
                      :RATPCT9,                                        *01201000
                      :RATEAT10,                                       *01202000
                      :TERM10,                                         *01203000
                      :RATPCT10,                                       *01204000
                      :RATAMT11,                                       *01205000
                      :TERM11,                                         *01206000
                      :RATPCT11,                                       *01207000
                      :RATAMT12,                                       *01208000
                      :TERM12,                                         *01209000
                      :RATPCT12,                                       *01210000
                      :RATAMT13,                                       *01211000
                      :TERM13,                                         *01212000
                      :RATPCT13,                                       *01213000
                      :RATAMT14,                                       *01214000
                      :TERM14,                                         *01215000
                      :RATPCT14,                                       *01216000
                      :RATAMT15,                                       *01217000
                      :TERM15,                                         *01218000
                      :RATPCT15                                         01219000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01220000
         BALR  4,5                     MOVE INTO RECORD AREA            01221000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01222000
         BR    14                      RETURN TO CALLER                 01223000
         LTORG                                                          01224000
*                                                                       01225000
*                                                                       01226000
**********************************************************************  01227000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 1:           01228000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01229000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01230000
*     RETRIEVE THE ACTUAL ROW.                                          01231000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01232000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01233000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01234000
**********************************************************************  01235000
*                                                                       01236000
SQL#0021 DS    0H                                                       01237000
         USING SQL#0021,12             ESTABLISH BASE REGISTER          01238000
         LA    15,255                  SET RETURN CODE                  01239000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01240000
         BR    14                      RETURN TO CALLER                 01241000
         LTORG                                                          01242000
*                                                                       01243000
*                                                                       01244000
**********************************************************************  01245000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 2:           01246000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01247000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01248000
*     RETRIEVE THE ACTUAL ROW.                                          01249000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01250000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01251000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01252000
**********************************************************************  01253000
*                                                                       01254000
SQL#0022 DS    0H                                                       01255000
         USING SQL#0022,12             ESTABLISH BASE REGISTER          01256000
         LA    15,255                  SET RETURN CODE                  01257000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01258000
         BR    14                      RETURN TO CALLER                 01259000
         LTORG                                                          01260000
*                                                                       01261000
*                                                                       01262000
**********************************************************************  01263000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 3:           01264000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01265000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01266000
*     RETRIEVE THE ACTUAL ROW.                                          01267000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01268000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01269000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01270000
**********************************************************************  01271000
*                                                                       01272000
SQL#0023 DS    0H                                                       01273000
         USING SQL#0023,12             ESTABLISH BASE REGISTER          01274000
         LA    15,255                  SET RETURN CODE                  01275000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01276000
         BR    14                      RETURN TO CALLER                 01277000
         LTORG                                                          01278000
*                                                                       01279000
*                                                                       01280000
**********************************************************************  01281000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01282000
* THE PRIMARY KEY:                                                      01283000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01284000
*     VERBS.                                                            01285000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01286000
*     RETRIEVE THE ACTUAL ROW.                                          01287000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01288000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01289000
*     WILL BE THRU THE UPDATE CURSOR.                                   01290000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01291000
**********************************************************************  01292000
*                                                                       01293000
SQL#0024 DS    0H                                                       01294000
         USING SQL#0024,12             ESTABLISH BASE REGISTER          01295000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01296000
         LA    12,VECT0024(1)          LOAD POINTER TO FETCH ROUTINE    01297000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01298000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01299000
VECT0024 DC    A(SQL#0124)                                              01300000
         DC    A(SQL#0224)                                              01301000
         DC    A(SQL#0324)                                              01302000
         DC    A(SQL#0424)                                              01303000
         LTORG                                                          01304000
*                                                                       01305000
SQL#0124 DS    0H                                                       01306000
         USING SQL#0124,12             ESTABLISH BASE REGISTER          01307000
         B     *+6                     BRANCH AROUND ADCON              01308000
BASE0124 DC    AL2(4096)                                                01309000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01310000
         AH    3,BASE0124              ADD 4K                           01311000
         EXEC  SQL                                                     *01312000
               FETCH S03SQ001                                          *01313000
                 INTO :INST,                                           *01314000
                      :RECNBR,                                         *01315000
                      :RATETABL,                                       *01316000
                      :EFFDATE                                          01317000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01318000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01319000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01320000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01321000
         BR    12                      OPEN NEXT CURSOR                 01322000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01323000
         BALR  4,5                     MOVE INTO RECORD AREA            01324000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01325000
         BR    14                      RETURN TO CALLER                 01326000
         LTORG                                                          01327000
*                                                                       01328000
SQL#0224 DS    0H                                                       01329000
         USING SQL#0224,12             ESTABLISH BASE REGISTER          01330000
         B     *+6                     BRANCH AROUND ADCON              01331000
BASE0224 DC    AL2(4096)                                                01332000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01333000
         AH    3,BASE0224              ADD 4K                           01334000
         EXEC  SQL                                                     *01335000
               FETCH S03SQ002                                          *01336000
                 INTO :INST,                                           *01337000
                      :RECNBR,                                         *01338000
                      :RATETABL,                                       *01339000
                      :EFFDATE                                          01340000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01341000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01342000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01343000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01344000
         BR    12                      OPEN NEXT CURSOR                 01345000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01346000
         BALR  4,5                     MOVE INTO RECORD AREA            01347000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01348000
         BR    14                      RETURN TO CALLER                 01349000
         LTORG                                                          01350000
*                                                                       01351000
SQL#0324 DS    0H                                                       01352000
         USING SQL#0324,12             ESTABLISH BASE REGISTER          01353000
         B     *+6                     BRANCH AROUND ADCON              01354000
BASE0324 DC    AL2(4096)                                                01355000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01356000
         AH    3,BASE0324              ADD 4K                           01357000
         EXEC  SQL                                                     *01358000
               FETCH S03SQ003                                          *01359000
                 INTO :INST,                                           *01360000
                      :RECNBR,                                         *01361000
                      :RATETABL,                                       *01362000
                      :EFFDATE                                          01363000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01364000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01365000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01366000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01367000
         BR    12                      OPEN NEXT CURSOR                 01368000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01369000
         BALR  4,5                     MOVE INTO RECORD AREA            01370000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01371000
         BR    14                      RETURN TO CALLER                 01372000
         LTORG                                                          01373000
*                                                                       01374000
SQL#0424 DS    0H                                                       01375000
         USING SQL#0424,12             ESTABLISH BASE REGISTER          01376000
         B     *+6                     BRANCH AROUND ADCON              01377000
BASE0424 DC    AL2(4096)                                                01378000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01379000
         AH    3,BASE0424              ADD 4K                           01380000
         EXEC  SQL                                                     *01381000
               FETCH S03SQ004                                          *01382000
                 INTO :INST,                                           *01383000
                      :RECNBR,                                         *01384000
                      :RATETABL,                                       *01385000
                      :EFFDATE                                          01386000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01387000
         BALR  4,5                     MOVE INTO RECORD AREA            01388000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01389000
         BR    14                      RETURN TO CALLER                 01390000
         LTORG                                                          01391000
*                                                                       01392000
*                                                                       01393000
**********************************************************************  01394000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01395000
* ALTERNATE KEY 1:                                                      01396000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01397000
*     VERBS.                                                            01398000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01399000
*     RETRIEVE THE ACTUAL ROW.                                          01400000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01401000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01402000
*     WILL BE THRU THE UPDATE CURSOR.                                   01403000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01404000
**********************************************************************  01405000
*                                                                       01406000
SQL#0025 DS    0H                                                       01407000
         USING SQL#0025,12             ESTABLISH BASE REGISTER          01408000
         LA    15,255                  SET RETURN CODE                  01409000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01410000
         BR    14                      RETURN TO CALLER                 01411000
         LTORG                                                          01412000
*                                                                       01413000
*                                                                       01414000
**********************************************************************  01415000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01416000
* ALTERNATE KEY 2:                                                      01417000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01418000
*     VERBS.                                                            01419000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01420000
*     RETRIEVE THE ACTUAL ROW.                                          01421000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01422000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01423000
*     WILL BE THRU THE UPDATE CURSOR.                                   01424000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01425000
**********************************************************************  01426000
*                                                                       01427000
SQL#0026 DS    0H                                                       01428000
         USING SQL#0026,12             ESTABLISH BASE REGISTER          01429000
         LA    15,255                  SET RETURN CODE                  01430000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01431000
         BR    14                      RETURN TO CALLER                 01432000
         LTORG                                                          01433000
*                                                                       01434000
*                                                                       01435000
**********************************************************************  01436000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01437000
* ALTERNATE KEY 3:                                                      01438000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01439000
*     VERBS.                                                            01440000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01441000
*     RETRIEVE THE ACTUAL ROW.                                          01442000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01443000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01444000
*     WILL BE THRU THE UPDATE CURSOR.                                   01445000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01446000
**********************************************************************  01447000
*                                                                       01448000
SQL#0027 DS    0H                                                       01449000
         USING SQL#0027,12             ESTABLISH BASE REGISTER          01450000
         LA    15,255                  SET RETURN CODE                  01451000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01452000
         BR    14                      RETURN TO CALLER                 01453000
         LTORG                                                          01454000
*                                                                       01455000
*                                                                       01456000
**********************************************************************  01457000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:                    01458000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01459000
*     AND GET-NEXT-LOCK VERBS.                                          01460000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01461000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01462000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01463000
**********************************************************************  01464000
*                                                                       01465000
SQL#0028 DS    0H                                                       01466000
         USING SQL#0028,12             ESTABLISH BASE REGISTER          01467000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01468000
         XC    CSRPTR,CSRPTR           CLEAR CURSOR ROUTINE POINTER     01469000
         LA    12,VECT0028(1)          LOAD POINTER TO CLOSE ROUTINE    01470000
         L     12,0(12)                LOAD CLOSE ROUTINE ADDRESS       01471000
         BR    12                      GO TO CURRENT CLOSE ROUTINE      01472000
VECT0028 DC    A(SQL#0128)                                              01473000
         DC    A(SQL#0228)                                              01474000
         DC    A(SQL#0328)                                              01475000
         DC    A(SQL#0428)                                              01476000
         LTORG                                                          01477000
*                                                                       01478000
SQL#0128 DS    0H                                                       01479000
         USING SQL#0128,12             ESTABLISH BASE REGISTER          01480000
         B     *+6                     BRANCH AROUND ADCON              01481000
BASE0128 DC    AL2(4096)                                                01482000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01483000
         AH    3,BASE0128              ADD 4K                           01484000
         EXEC  SQL                                                     *01485000
               CLOSE S03SQ001                                           01486000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01487000
         BR    14                      RETURN TO CALLER                 01488000
         LTORG                                                          01489000
*                                                                       01490000
SQL#0228 DS    0H                                                       01491000
         USING SQL#0228,12             ESTABLISH BASE REGISTER          01492000
         B     *+6                     BRANCH AROUND ADCON              01493000
BASE0228 DC    AL2(4096)                                                01494000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01495000
         AH    3,BASE0228              ADD 4K                           01496000
         EXEC  SQL                                                     *01497000
               CLOSE S03SQ002                                           01498000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01499000
         BR    14                      RETURN TO CALLER                 01500000
         LTORG                                                          01501000
*                                                                       01502000
SQL#0328 DS    0H                                                       01503000
         USING SQL#0328,12             ESTABLISH BASE REGISTER          01504000
         B     *+6                     BRANCH AROUND ADCON              01505000
BASE0328 DC    AL2(4096)                                                01506000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01507000
         AH    3,BASE0328              ADD 4K                           01508000
         EXEC  SQL                                                     *01509000
               CLOSE S03SQ003                                           01510000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01511000
         BR    14                      RETURN TO CALLER                 01512000
         LTORG                                                          01513000
*                                                                       01514000
SQL#0428 DS    0H                                                       01515000
         USING SQL#0428,12             ESTABLISH BASE REGISTER          01516000
         B     *+6                     BRANCH AROUND ADCON              01517000
BASE0428 DC    AL2(4096)                                                01518000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01519000
         AH    3,BASE0428              ADD 4K                           01520000
         EXEC  SQL                                                     *01521000
               CLOSE S03SQ004                                           01522000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01523000
         BR    14                      RETURN TO CALLER                 01524000
         LTORG                                                          01525000
*                                                                       01526000
*                                                                       01527000
**********************************************************************  01528000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 1:                01529000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01530000
*     AND GET-NEXT-LOCK VERBS.                                          01531000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01532000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01533000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01534000
**********************************************************************  01535000
*                                                                       01536000
SQL#0029 DS    0H                                                       01537000
         USING SQL#0029,12             ESTABLISH BASE REGISTER          01538000
         LA    15,255                  SET RETURN CODE                  01539000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01540000
         BR    14                      RETURN TO CALLER                 01541000
         LTORG                                                          01542000
*                                                                       01543000
*                                                                       01544000
**********************************************************************  01545000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 2:                01546000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01547000
*     AND GET-NEXT-LOCK VERBS.                                          01548000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01549000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01550000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01551000
**********************************************************************  01552000
*                                                                       01553000
SQL#0030 DS    0H                                                       01554000
         USING SQL#0030,12             ESTABLISH BASE REGISTER          01555000
         LA    15,255                  SET RETURN CODE                  01556000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01557000
         BR    14                      RETURN TO CALLER                 01558000
         LTORG                                                          01559000
*                                                                       01560000
*                                                                       01561000
**********************************************************************  01562000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 3:                01563000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01564000
*     AND GET-NEXT-LOCK VERBS.                                          01565000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01566000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01567000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01568000
**********************************************************************  01569000
*                                                                       01570000
SQL#0031 DS    0H                                                       01571000
         USING SQL#0031,12             ESTABLISH BASE REGISTER          01572000
         LA    15,255                  SET RETURN CODE                  01573000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01574000
         BR    14                      RETURN TO CALLER                 01575000
         LTORG                                                          01576000
*                                                                       01577000
*                                                                       01578000
**********************************************************************  01579000
* SELECT KEY STATEMENT BY PRIMARY KEY:                                  01580000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            01581000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01582000
**********************************************************************  01583000
*                                                                       01584000
SQL#0032 DS    0H                                                       01585000
         USING SQL#0032,12             ESTABLISH BASE REGISTER          01586000
         B     *+6                     BRANCH AROUND ADCON              01587000
BASE0032 DC    AL2(4096)                                                01588000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              01589000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      01590000
         BALR  4,5                     MOVE INTO HOST VARIABLES         01591000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01592000
         AH    3,BASE0032              ADD 4K                           01593000
         EXEC  SQL                                                     *01594000
               SELECT INST_NBR,                                        *01595000
                   RECORD_NBR,                                         *01596000
                   RATE_TABLE,                                         *01597000
                   EFFECTIVE_DATE                                      *01598000
                 INTO :INST,                                           *01599000
                   :RECNBR,                                            *01600000
                   :RATETABL,                                          *01601000
                   :EFFDATE                                            *01602000
                 FROM S03                                              *01603000
                 WHERE INST_NBR = :INST AND                            *01604000
                   RECORD_NBR = :RECNBR AND                            *01605000
                   RATE_TABLE = :RATETABL AND                          *01606000
                   EFFECTIVE_DATE = :EFFDATE                           *01607000
                 FETCH FIRST 1 ROW ONLY                                 01608000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01609000
         BR    14                      RETURN TO CALLER                 01610000
         LTORG                                                          01611000
*                                                                       01612000
*                                                                       01613000
**********************************************************************  01614000
* SELECT KEY STATEMENT BY ALTERNATE KEY 1:                              01615000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            01616000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01617000
**********************************************************************  01618000
*                                                                       01619000
SQL#0033 DS    0H                                                       01620000
         USING SQL#0033,12             ESTABLISH BASE REGISTER          01621000
         LA    15,255                  SET RETURN CODE                  01622000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01623000
         BR    14                      RETURN TO CALLER                 01624000
         LTORG                                                          01625000
*                                                                       01626000
*                                                                       01627000
**********************************************************************  01628000
* SELECT KEY STATEMENT BY ALTERNATE KEY 2:                              01629000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            01630000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01631000
**********************************************************************  01632000
*                                                                       01633000
SQL#0034 DS    0H                                                       01634000
         USING SQL#0034,12             ESTABLISH BASE REGISTER          01635000
         LA    15,255                  SET RETURN CODE                  01636000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01637000
         BR    14                      RETURN TO CALLER                 01638000
         LTORG                                                          01639000
*                                                                       01640000
*                                                                       01641000
**********************************************************************  01642000
* SELECT KEY STATEMENT BY ALTERNATE KEY 3:                              01643000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            01644000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01645000
**********************************************************************  01646000
*                                                                       01647000
SQL#0035 DS    0H                                                       01648000
         USING SQL#0035,12             ESTABLISH BASE REGISTER          01649000
         LA    15,255                  SET RETURN CODE                  01650000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01651000
         BR    14                      RETURN TO CALLER                 01652000
         LTORG                                                          01653000
*                                                                       01654000
*                                                                       01655000
**********************************************************************  01656000
* INSERT STATEMENT:                                                     01657000
*   THIS STATEMENT SUPPORTS THE PUT VERB.                               01658000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01659000
**********************************************************************  01660000
*                                                                       01661000
SQL#0036 DS    0H                                                       01662000
         USING SQL#0036,12             ESTABLISH BASE REGISTER          01663000
         B     *+6                     BRANCH AROUND ADCON              01664000
BASE0036 DC    AL2(4096)                                                01665000
         L     5,=A(IN#HOST)           LOAD INPUT CONVERSION ROUTINE    01666000
         BALR  4,5                     MOVE INTO HOST VARIABLES         01667000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01668000
         AH    3,BASE0036              ADD 4K                           01669000
         EXEC  SQL                                                     *01670000
               INSERT INTO S03                                         *01671000
                   (INST_NBR,                                          *01672000
                    RECORD_NBR,                                        *01673000
                    RATE_TABLE,                                        *01674000
                    EFFECTIVE_DATE,                                    *01675000
                    AUDIT_DATE,                                        *01676000
                    AUDIT_TIME,                                        *01677000
                    AUDIT_USER,                                        *01678000
                    AUDIT_ORG,                                         *01679000
                    BAL_OPT,                                           *01680000
                    RATE_OPTION,                                       *01681000
                    BAL_ORDER,                                         *01682000
                    TERM_BAL,                                          *01683000
                    RATE_AMT_1,                                        *01684000
                    TERM_1,                                            *01685000
                    RATE_PCT_1,                                        *01686000
                    RATE_AMT_2,                                        *01687000
                    TERM_2,                                            *01688000
                    RATE_PCT_2,                                        *01689000
                    RATE_AMT_3,                                        *01690000
                    TERM_3,                                            *01691000
                    RATE_PCT_3,                                        *01692000
                    RATE_AMT_4,                                        *01693000
                    TERM_4,                                            *01694000
                    RATE_PCT_4,                                        *01695000
                    RATE_AMT_5,                                        *01696000
                    TERM_5,                                            *01697000
                    RATE_PCT_5,                                        *01698000
                    RATE_AMT_6,                                        *01699000
                    TERM_6,                                            *01700000
                    RATE_PCT_6,                                        *01701000
                    RATE_AMT_7,                                        *01702000
                    TERM_7,                                            *01703000
                    RATE_PCT_7,                                        *01704000
                    RATE_AMT_8,                                        *01705000
                    TERM_8,                                            *01706000
                    RATE_PCT_8,                                        *01707000
                    RATE_AMT_9,                                        *01708000
                    TERM_9,                                            *01709000
                    RATE_PCT_9,                                        *01710000
                    RATE_AMT_10,                                       *01711000
                    TERM_10,                                           *01712000
                    RATE_PCT_10,                                       *01713000
                    RATE_AMT_11,                                       *01714000
                    TERM_11,                                           *01715000
                    RATE_PCT_11,                                       *01716000
                    RATE_AMT_12,                                       *01717000
                    TERM_12,                                           *01718000
                    RATE_PCT_12,                                       *01719000
                    RATE_AMT_13,                                       *01720000
                    TERM_13,                                           *01721000
                    RATE_PCT_13,                                       *01722000
                    RATE_AMT_14,                                       *01723000
                    TERM_14,                                           *01724000
                    RATE_PCT_14,                                       *01725000
                    RATE_AMT_15,                                       *01726000
                    TERM_15,                                           *01727000
                    RATE_PCT_15)                                       *01728000
                  VALUES (:INST,                                       *01729000
                   :RECNBR,                                            *01730000
                   :RATETABL,                                          *01731000
                   :EFFDATE,                                           *01732000
                   :AUDDATE,                                           *01733000
                   :AUDTIME,                                           *01734000
                   :AUDUSER,                                           *01735000
                   :AUDORG,                                            *01736000
                   :BALOPT,                                            *01737000
                   :RATEOPT,                                           *01738000
                   :BALORDR,                                           *01739000
                   :TERMBAL,                                           *01740000
                   :RATEAMT1,                                          *01741000
                   :TERM1,                                             *01742000
                   :RATPCT1,                                           *01743000
                   :RATEAMT2,                                          *01744000
                   :TERM2,                                             *01745000
                   :RATPCT2,                                           *01746000
                   :RATEAMT3,                                          *01747000
                   :TERM3,                                             *01748000
                   :RATPCT3,                                           *01749000
                   :RATEAMT4,                                          *01750000
                   :TERM4,                                             *01751000
                   :RATPCT4,                                           *01752000
                   :RATEAMT5,                                          *01753000
                   :TERM5,                                             *01754000
                   :RATPCT5,                                           *01755000
                   :RATEAMT6,                                          *01756000
                   :TERM6,                                             *01757000
                   :RATPCT6,                                           *01758000
                   :RATEAMT7,                                          *01759000
                   :TERM7,                                             *01760000
                   :RATPCT7,                                           *01761000
                   :RATEAMT8,                                          *01762000
                   :TERM8,                                             *01763000
                   :RATPCT8,                                           *01764000
                   :RATEAMT9,                                          *01765000
                   :TERM9,                                             *01766000
                   :RATPCT9,                                           *01767000
                   :RATEAT10,                                          *01768000
                   :TERM10,                                            *01769000
                   :RATPCT10,                                          *01770000
                   :RATAMT11,                                          *01771000
                   :TERM11,                                            *01772000
                   :RATPCT11,                                          *01773000
                   :RATAMT12,                                          *01774000
                   :TERM12,                                            *01775000
                   :RATPCT12,                                          *01776000
                   :RATAMT13,                                          *01777000
                   :TERM13,                                            *01778000
                   :RATPCT13,                                          *01779000
                   :RATAMT14,                                          *01780000
                   :TERM14,                                            *01781000
                   :RATPCT14,                                          *01782000
                   :RATAMT15,                                          *01783000
                   :TERM15,                                            *01784000
                   :RATPCT15)                                           01785000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01786000
         BR    14                      RETURN TO CALLER                 01787000
         LTORG                                                          01788000
*                                                                       01789000
*                                                                       01790000
**********************************************************************  01791000
* UPDATE STATEMENT BY PRIMARY KEY:                                      01792000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             01793000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        01794000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         01795000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             01796000
*     STATEMENT.                                                        01797000
**********************************************************************  01798000
*                                                                       01799000
SQL#0037 DS    0H                                                       01800000
         USING SQL#0037,12             ESTABLISH BASE REGISTER          01801000
         B     *+6                     BRANCH AROUND ADCON              01802000
BASE0037 DC    AL2(4096)                                                01803000
         L     5,=A(IN#HOST)           LOAD INPUT CONVERSION ROUTINE    01804000
         BALR  4,5                     MOVE INTO HOST VARIABLES         01805000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01806000
         AH    3,BASE0037              ADD 4K                           01807000
         EXEC  SQL                                                     *01808000
               UPDATE S03                                              *01809000
                   SET AUDIT_DATE = :AUDDATE,                          *01810000
                     AUDIT_TIME = :AUDTIME,                            *01811000
                     AUDIT_USER = :AUDUSER,                            *01812000
                     AUDIT_ORG = :AUDORG,                              *01813000
                     BAL_OPT = :BALOPT,                                *01814000
                     RATE_OPTION = :RATEOPT,                           *01815000
                     BAL_ORDER = :BALORDR,                             *01816000
                     TERM_BAL = :TERMBAL,                              *01817000
                     RATE_AMT_1 = :RATEAMT1,                           *01818000
                     TERM_1 = :TERM1,                                  *01819000
                     RATE_PCT_1 = :RATPCT1,                            *01820000
                     RATE_AMT_2 = :RATEAMT2,                           *01821000
                     TERM_2 = :TERM2,                                  *01822000
                     RATE_PCT_2 = :RATPCT2,                            *01823000
                     RATE_AMT_3 = :RATEAMT3,                           *01824000
                     TERM_3 = :TERM3,                                  *01825000
                     RATE_PCT_3 = :RATPCT3,                            *01826000
                     RATE_AMT_4 = :RATEAMT4,                           *01827000
                     TERM_4 = :TERM4,                                  *01828000
                     RATE_PCT_4 = :RATPCT4,                            *01829000
                     RATE_AMT_5 = :RATEAMT5,                           *01830000
                     TERM_5 = :TERM5,                                  *01831000
                     RATE_PCT_5 = :RATPCT5,                            *01832000
                     RATE_AMT_6 = :RATEAMT6,                           *01833000
                     TERM_6 = :TERM6,                                  *01834000
                     RATE_PCT_6 = :RATPCT6,                            *01835000
                     RATE_AMT_7 = :RATEAMT7,                           *01836000
                     TERM_7 = :TERM7,                                  *01837000
                     RATE_PCT_7 = :RATPCT7,                            *01838000
                     RATE_AMT_8 = :RATEAMT8,                           *01839000
                     TERM_8 = :TERM8,                                  *01840000
                     RATE_PCT_8 = :RATPCT8,                            *01841000
                     RATE_AMT_9 = :RATEAMT9,                           *01842000
                     TERM_9 = :TERM9,                                  *01843000
                     RATE_PCT_9 = :RATPCT9,                            *01844000
                     RATE_AMT_10 = :RATEAT10,                          *01845000
                     TERM_10 = :TERM10,                                *01846000
                     RATE_PCT_10 = :RATPCT10,                          *01847000
                     RATE_AMT_11 = :RATAMT11,                          *01848000
                     TERM_11 = :TERM11,                                *01849000
                     RATE_PCT_11 = :RATPCT11,                          *01850000
                     RATE_AMT_12 = :RATAMT12,                          *01851000
                     TERM_12 = :TERM12,                                *01852000
                     RATE_PCT_12 = :RATPCT12,                          *01853000
                     RATE_AMT_13 = :RATAMT13,                          *01854000
                     TERM_13 = :TERM13,                                *01855000
                     RATE_PCT_13 = :RATPCT13,                          *01856000
                     RATE_AMT_14 = :RATAMT14,                          *01857000
                     TERM_14 = :TERM14,                                *01858000
                     RATE_PCT_14 = :RATPCT14,                          *01859000
                     RATE_AMT_15 = :RATAMT15,                          *01860000
                     TERM_15 = :TERM15,                                *01861000
                     RATE_PCT_15 = :RATPCT15                           *01862000
                 WHERE CURRENT OF S03UPD0                               01863000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01864000
         BR    14                      RETURN TO CALLER                 01865000
         LTORG                                                          01866000
*                                                                       01867000
*                                                                       01868000
**********************************************************************  01869000
* UPDATE STATEMENT BY ALTERNATE KEY 1:                                  01870000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             01871000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        01872000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         01873000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             01874000
*     STATEMENT.                                                        01875000
**********************************************************************  01876000
*                                                                       01877000
SQL#0038 DS    0H                                                       01878000
         USING SQL#0038,12             ESTABLISH BASE REGISTER          01879000
         LA    15,255                  SET RETURN CODE                  01880000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01881000
         BR    14                      RETURN TO CALLER                 01882000
         LTORG                                                          01883000
*                                                                       01884000
*                                                                       01885000
**********************************************************************  01886000
* UPDATE STATEMENT BY ALTERNATE KEY 2:                                  01887000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             01888000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        01889000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         01890000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             01891000
*     STATEMENT.                                                        01892000
**********************************************************************  01893000
*                                                                       01894000
SQL#0039 DS    0H                                                       01895000
         USING SQL#0039,12             ESTABLISH BASE REGISTER          01896000
         LA    15,255                  SET RETURN CODE                  01897000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01898000
         BR    14                      RETURN TO CALLER                 01899000
         LTORG                                                          01900000
*                                                                       01901000
*                                                                       01902000
**********************************************************************  01903000
* UPDATE STATEMENT BY ALTERNATE KEY 3:                                  01904000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             01905000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        01906000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         01907000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             01908000
*     STATEMENT.                                                        01909000
**********************************************************************  01910000
*                                                                       01911000
SQL#0040 DS    0H                                                       01912000
         USING SQL#0040,12             ESTABLISH BASE REGISTER          01913000
         LA    15,255                  SET RETURN CODE                  01914000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01915000
         BR    14                      RETURN TO CALLER                 01916000
         LTORG                                                          01917000
*                                                                       01918000
*                                                                       01919000
**********************************************************************  01920000
* DELETE STATEMENT BY PRIMARY KEY:                                      01921000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            01922000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01923000
**********************************************************************  01924000
*                                                                       01925000
SQL#0041 DS    0H                                                       01926000
         USING SQL#0041,12             ESTABLISH BASE REGISTER          01927000
         B     *+6                     BRANCH AROUND ADCON              01928000
BASE0041 DC    AL2(4096)                                                01929000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01930000
         AH    3,BASE0041              ADD 4K                           01931000
         EXEC  SQL                                                     *01932000
               DELETE FROM S03                                         *01933000
                 WHERE CURRENT OF S03UPD0                               01934000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01935000
         BR    14                      RETURN TO CALLER                 01936000
         LTORG                                                          01937000
*                                                                       01938000
*                                                                       01939000
**********************************************************************  01940000
* DELETE STATEMENT BY ALTERNATE KEY 1:                                  01941000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            01942000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01943000
**********************************************************************  01944000
*                                                                       01945000
SQL#0042 DS    0H                                                       01946000
         USING SQL#0042,12             ESTABLISH BASE REGISTER          01947000
         LA    15,255                  SET RETURN CODE                  01948000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01949000
         BR    14                      RETURN TO CALLER                 01950000
         LTORG                                                          01951000
*                                                                       01952000
*                                                                       01953000
**********************************************************************  01954000
* DELETE STATEMENT BY ALTERNATE KEY 2:                                  01955000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            01956000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01957000
**********************************************************************  01958000
*                                                                       01959000
SQL#0043 DS    0H                                                       01960000
         USING SQL#0043,12             ESTABLISH BASE REGISTER          01961000
         LA    15,255                  SET RETURN CODE                  01962000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01963000
         BR    14                      RETURN TO CALLER                 01964000
         LTORG                                                          01965000
*                                                                       01966000
*                                                                       01967000
**********************************************************************  01968000
* DELETE STATEMENT BY ALTERNATE KEY 3:                                  01969000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            01970000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01971000
**********************************************************************  01972000
*                                                                       01973000
SQL#0044 DS    0H                                                       01974000
         USING SQL#0044,12             ESTABLISH BASE REGISTER          01975000
         LA    15,255                  SET RETURN CODE                  01976000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01977000
         BR    14                      RETURN TO CALLER                 01978000
         LTORG                                                          01979000
*                                                                       01980000
*                                                                       01981000
**********************************************************************  01982000
* DELETE ALL STATEMENT:                                                 01983000
*   THIS STATEMENT SUPPORTS THE DELETE-FILE VERB.                       01984000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01985000
**********************************************************************  01986000
*                                                                       01987000
SQL#0045 DS    0H                                                       01988000
         USING SQL#0045,12             ESTABLISH BASE REGISTER          01989000
         B     *+6                     BRANCH AROUND ADCON              01990000
BASE0045 DC    AL2(4096)                                                01991000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01992000
         AH    3,BASE0045              ADD 4K                           01993000
         EXEC  SQL                                                     *01994000
               DELETE FROM S03                                          01995000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01996000
         BR    14                      RETURN TO CALLER                 01997000
         LTORG                                                          01998000
*                                                                       01999000
*                                                                       02000000
**********************************************************************  02001000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY KEY:          02002000
*   THIS ROUTINE HANDLES PRIMARY KEY SEQUENTIAL CURSORS.                02003000
**********************************************************************  02004000
*                                                                       02005000
SQL#0046 DS    0H                                                       02006000
         USING SQL#0046,12             ESTABLISH BASE REGISTER          02007000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              02008000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      02009000
         BALR  4,5                     MOVE INTO HOST VARIABLES         02010000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      02011000
         LA    1,4(1)                  INCREMENT TO NEXT CURSOR         02012000
         STH   1,CSRPTR                SAVE FOR NEXT CALL               02013000
         LA    12,VECT0046(1)          LOAD POINTER TO NEXT CURSOR      02014000
         L     12,0(12)                LOAD CURSOR ROUTINE ADDRESS      02015000
         BR    12                      GO TO CURRENT CURSOR ROUTINE     02016000
VECT0046 DC    A(0)                                                     02017000
         DC    A(SQL#0246)                                              02018000
         DC    A(SQL#0346)                                              02019000
         DC    A(SQL#0446)                                              02020000
         LTORG                                                          02021000
*                                                                       02022000
SQL#0246 DS    0H                                                       02023000
         USING SQL#0246,12             ESTABLISH BASE REGISTER          02024000
         B     *+6                     BRANCH AROUND ADCON              02025000
BASE0246 DC    AL2(4096)                                                02026000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02027000
         AH    3,BASE0246              ADD 4K                           02028000
         EXEC  SQL                                                     *02029000
               CLOSE S03SQ001                                           02030000
         EXEC  SQL                                                     *02031000
               DECLARE S03SQ002 CURSOR FOR                             *02032000
               SELECT INST_NBR,                                        *02033000
                   RECORD_NBR,                                         *02034000
                   RATE_TABLE,                                         *02035000
                   EFFECTIVE_DATE,                                     *02036000
                   AUDIT_DATE,                                         *02037000
                   AUDIT_TIME,                                         *02038000
                   AUDIT_USER,                                         *02039000
                   AUDIT_ORG,                                          *02040000
                   BAL_OPT,                                            *02041000
                   RATE_OPTION,                                        *02042000
                   BAL_ORDER,                                          *02043000
                   TERM_BAL,                                           *02044000
                   RATE_AMT_1,                                         *02045000
                   TERM_1,                                             *02046000
                   RATE_PCT_1,                                         *02047000
                   RATE_AMT_2,                                         *02048000
                   TERM_2,                                             *02049000
                   RATE_PCT_2,                                         *02050000
                   RATE_AMT_3,                                         *02051000
                   TERM_3,                                             *02052000
                   RATE_PCT_3,                                         *02053000
                   RATE_AMT_4,                                         *02054000
                   TERM_4,                                             *02055000
                   RATE_PCT_4,                                         *02056000
                   RATE_AMT_5,                                         *02057000
                   TERM_5,                                             *02058000
                   RATE_PCT_5,                                         *02059000
                   RATE_AMT_6,                                         *02060000
                   TERM_6,                                             *02061000
                   RATE_PCT_6,                                         *02062000
                   RATE_AMT_7,                                         *02063000
                   TERM_7,                                             *02064000
                   RATE_PCT_7,                                         *02065000
                   RATE_AMT_8,                                         *02066000
                   TERM_8,                                             *02067000
                   RATE_PCT_8,                                         *02068000
                   RATE_AMT_9,                                         *02069000
                   TERM_9,                                             *02070000
                   RATE_PCT_9,                                         *02071000
                   RATE_AMT_10,                                        *02072000
                   TERM_10,                                            *02073000
                   RATE_PCT_10,                                        *02074000
                   RATE_AMT_11,                                        *02075000
                   TERM_11,                                            *02076000
                   RATE_PCT_11,                                        *02077000
                   RATE_AMT_12,                                        *02078000
                   TERM_12,                                            *02079000
                   RATE_PCT_12,                                        *02080000
                   RATE_AMT_13,                                        *02081000
                   TERM_13,                                            *02082000
                   RATE_PCT_13,                                        *02083000
                   RATE_AMT_14,                                        *02084000
                   TERM_14,                                            *02085000
                   RATE_PCT_14,                                        *02086000
                   RATE_AMT_15,                                        *02087000
                   TERM_15,                                            *02088000
                   RATE_PCT_15                                         *02089000
                 FROM S03                                              *02090000
                 WHERE                                                 *02091000
                    INST_NBR = :INST AND                               *02092000
                    RECORD_NBR = :RECNBR AND                           *02093000
                    RATE_TABLE > :RATETABL                             *02094000
                 ORDER BY RATE_TABLE,                                  *02095000
                   EFFECTIVE_DATE                                      *02096000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       02097000
         EXEC  SQL                                                     *02098000
               OPEN S03SQ002                                            02099000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            02100000
         BNZ   *+8                     NO - RETURN ERROR                02101000
         LR    12,9                    LOAD RETURN ADDRESS              02102000
         BR    12                      RETURN TO FETCH ROUTINE          02103000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02104000
         BR    14                      RETURN TO CALLER                 02105000
         LTORG                                                          02106000
*                                                                       02107000
SQL#0346 DS    0H                                                       02108000
         USING SQL#0346,12             ESTABLISH BASE REGISTER          02109000
         B     *+6                     BRANCH AROUND ADCON              02110000
BASE0346 DC    AL2(4096)                                                02111000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02112000
         AH    3,BASE0346              ADD 4K                           02113000
         EXEC  SQL                                                     *02114000
               CLOSE S03SQ002                                           02115000
         EXEC  SQL                                                     *02116000
               DECLARE S03SQ003 CURSOR FOR                             *02117000
               SELECT INST_NBR,                                        *02118000
                   RECORD_NBR,                                         *02119000
                   RATE_TABLE,                                         *02120000
                   EFFECTIVE_DATE,                                     *02121000
                   AUDIT_DATE,                                         *02122000
                   AUDIT_TIME,                                         *02123000
                   AUDIT_USER,                                         *02124000
                   AUDIT_ORG,                                          *02125000
                   BAL_OPT,                                            *02126000
                   RATE_OPTION,                                        *02127000
                   BAL_ORDER,                                          *02128000
                   TERM_BAL,                                           *02129000
                   RATE_AMT_1,                                         *02130000
                   TERM_1,                                             *02131000
                   RATE_PCT_1,                                         *02132000
                   RATE_AMT_2,                                         *02133000
                   TERM_2,                                             *02134000
                   RATE_PCT_2,                                         *02135000
                   RATE_AMT_3,                                         *02136000
                   TERM_3,                                             *02137000
                   RATE_PCT_3,                                         *02138000
                   RATE_AMT_4,                                         *02139000
                   TERM_4,                                             *02140000
                   RATE_PCT_4,                                         *02141000
                   RATE_AMT_5,                                         *02142000
                   TERM_5,                                             *02143000
                   RATE_PCT_5,                                         *02144000
                   RATE_AMT_6,                                         *02145000
                   TERM_6,                                             *02146000
                   RATE_PCT_6,                                         *02147000
                   RATE_AMT_7,                                         *02148000
                   TERM_7,                                             *02149000
                   RATE_PCT_7,                                         *02150000
                   RATE_AMT_8,                                         *02151000
                   TERM_8,                                             *02152000
                   RATE_PCT_8,                                         *02153000
                   RATE_AMT_9,                                         *02154000
                   TERM_9,                                             *02155000
                   RATE_PCT_9,                                         *02156000
                   RATE_AMT_10,                                        *02157000
                   TERM_10,                                            *02158000
                   RATE_PCT_10,                                        *02159000
                   RATE_AMT_11,                                        *02160000
                   TERM_11,                                            *02161000
                   RATE_PCT_11,                                        *02162000
                   RATE_AMT_12,                                        *02163000
                   TERM_12,                                            *02164000
                   RATE_PCT_12,                                        *02165000
                   RATE_AMT_13,                                        *02166000
                   TERM_13,                                            *02167000
                   RATE_PCT_13,                                        *02168000
                   RATE_AMT_14,                                        *02169000
                   TERM_14,                                            *02170000
                   RATE_PCT_14,                                        *02171000
                   RATE_AMT_15,                                        *02172000
                   TERM_15,                                            *02173000
                   RATE_PCT_15                                         *02174000
                 FROM S03                                              *02175000
                 WHERE                                                 *02176000
                    INST_NBR = :INST AND                               *02177000
                    RECORD_NBR > :RECNBR                               *02178000
                 ORDER BY RECORD_NBR,                                  *02179000
                   RATE_TABLE,                                         *02180000
                   EFFECTIVE_DATE                                      *02181000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       02182000
         EXEC  SQL                                                     *02183000
               OPEN S03SQ003                                            02184000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            02185000
         BNZ   *+8                     NO - RETURN ERROR                02186000
         LR    12,9                    LOAD RETURN ADDRESS              02187000
         BR    12                      RETURN TO FETCH ROUTINE          02188000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02189000
         BR    14                      RETURN TO CALLER                 02190000
         LTORG                                                          02191000
*                                                                       02192000
SQL#0446 DS    0H                                                       02193000
         USING SQL#0446,12             ESTABLISH BASE REGISTER          02194000
         B     *+6                     BRANCH AROUND ADCON              02195000
BASE0446 DC    AL2(4096)                                                02196000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02197000
         AH    3,BASE0446              ADD 4K                           02198000
         EXEC  SQL                                                     *02199000
               CLOSE S03SQ003                                           02200000
         EXEC  SQL                                                     *02201000
               DECLARE S03SQ004 CURSOR FOR                             *02202000
               SELECT INST_NBR,                                        *02203000
                   RECORD_NBR,                                         *02204000
                   RATE_TABLE,                                         *02205000
                   EFFECTIVE_DATE,                                     *02206000
                   AUDIT_DATE,                                         *02207000
                   AUDIT_TIME,                                         *02208000
                   AUDIT_USER,                                         *02209000
                   AUDIT_ORG,                                          *02210000
                   BAL_OPT,                                            *02211000
                   RATE_OPTION,                                        *02212000
                   BAL_ORDER,                                          *02213000
                   TERM_BAL,                                           *02214000
                   RATE_AMT_1,                                         *02215000
                   TERM_1,                                             *02216000
                   RATE_PCT_1,                                         *02217000
                   RATE_AMT_2,                                         *02218000
                   TERM_2,                                             *02219000
                   RATE_PCT_2,                                         *02220000
                   RATE_AMT_3,                                         *02221000
                   TERM_3,                                             *02222000
                   RATE_PCT_3,                                         *02223000
                   RATE_AMT_4,                                         *02224000
                   TERM_4,                                             *02225000
                   RATE_PCT_4,                                         *02226000
                   RATE_AMT_5,                                         *02227000
                   TERM_5,                                             *02228000
                   RATE_PCT_5,                                         *02229000
                   RATE_AMT_6,                                         *02230000
                   TERM_6,                                             *02231000
                   RATE_PCT_6,                                         *02232000
                   RATE_AMT_7,                                         *02233000
                   TERM_7,                                             *02234000
                   RATE_PCT_7,                                         *02235000
                   RATE_AMT_8,                                         *02236000
                   TERM_8,                                             *02237000
                   RATE_PCT_8,                                         *02238000
                   RATE_AMT_9,                                         *02239000
                   TERM_9,                                             *02240000
                   RATE_PCT_9,                                         *02241000
                   RATE_AMT_10,                                        *02242000
                   TERM_10,                                            *02243000
                   RATE_PCT_10,                                        *02244000
                   RATE_AMT_11,                                        *02245000
                   TERM_11,                                            *02246000
                   RATE_PCT_11,                                        *02247000
                   RATE_AMT_12,                                        *02248000
                   TERM_12,                                            *02249000
                   RATE_PCT_12,                                        *02250000
                   RATE_AMT_13,                                        *02251000
                   TERM_13,                                            *02252000
                   RATE_PCT_13,                                        *02253000
                   RATE_AMT_14,                                        *02254000
                   TERM_14,                                            *02255000
                   RATE_PCT_14,                                        *02256000
                   RATE_AMT_15,                                        *02257000
                   TERM_15,                                            *02258000
                   RATE_PCT_15                                         *02259000
                 FROM S03                                              *02260000
                 WHERE                                                 *02261000
                    INST_NBR > :INST                                   *02262000
                 ORDER BY INST_NBR,                                    *02263000
                   RECORD_NBR,                                         *02264000
                   RATE_TABLE,                                         *02265000
                   EFFECTIVE_DATE                                      *02266000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       02267000
         EXEC  SQL                                                     *02268000
               OPEN S03SQ004                                            02269000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            02270000
         BNZ   *+8                     NO - RETURN ERROR                02271000
         LR    12,9                    LOAD RETURN ADDRESS              02272000
         BR    12                      RETURN TO FETCH ROUTINE          02273000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02274000
         BR    14                      RETURN TO CALLER                 02275000
         LTORG                                                          02276000
*                                                                       02277000
*                                                                       02278000
**********************************************************************  02279000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 1:      02280000
*   THIS ROUTINE HANDLES ALTERNATE KEY 1 SEQUENTIAL CURSORS.            02281000
**********************************************************************  02282000
*                                                                       02283000
SQL#0047 DS    0H                                                       02284000
         USING SQL#0047,12             ESTABLISH BASE REGISTER          02285000
         LA    15,255                  SET RETURN CODE                  02286000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02287000
         BR    14                      RETURN TO CALLER                 02288000
         LTORG                                                          02289000
*                                                                       02290000
*                                                                       02291000
**********************************************************************  02292000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 2:      02293000
*   THIS ROUTINE HANDLES ALTERNATE KEY 2 SEQUENTIAL CURSORS.            02294000
**********************************************************************  02295000
*                                                                       02296000
SQL#0048 DS    0H                                                       02297000
         USING SQL#0048,12             ESTABLISH BASE REGISTER          02298000
         LA    15,255                  SET RETURN CODE                  02299000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02300000
         BR    14                      RETURN TO CALLER                 02301000
         LTORG                                                          02302000
*                                                                       02303000
*                                                                       02304000
**********************************************************************  02305000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 3:      02306000
*   THIS ROUTINE HANDLES ALTERNATE KEY 3 SEQUENTIAL CURSORS.            02307000
**********************************************************************  02308000
*                                                                       02309000
SQL#0049 DS    0H                                                       02310000
         USING SQL#0049,12             ESTABLISH BASE REGISTER          02311000
         LA    15,255                  SET RETURN CODE                  02312000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02313000
         BR    14                      RETURN TO CALLER                 02314000
         LTORG                                                          02315000
*                                                                       02316000
         DS    0H                      END OF SQL STATEMENTS            02317000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   02318000
*                                                                       02319000
*                                                                       02320000
**********************************************************************  02321000
* DUMMY ENTRY POINT DSNHLI                                              02322000
*   MUST NOT BE MODIFIED.                                               02323000
**********************************************************************  02324000
*                                                                       02325000
         ENTRY DSNHLI                                                   02326000
DSNHLI   DS    0H                                                       02327000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       02328000
         BR    15                      BRANCH TO ATTACH FACILITY        02329000
*                                                                       02330000
*                                                                       02331000
**********************************************************************  02332000
* CONVERSION ROUTINES INTO AND OUT OF ASSEMBLER HOST VARIABLES          02333000
*   REQUIRED IF THE COBOL RECORD STRUCTURE IS NOT ALIGNED.              02334000
*   MUST NOT BE MODIFIED.                                               02335000
**********************************************************************  02336000
*                                                                       02337000
CONV#RTN CSECT                         CONVERSION ROUTINES SECTION      02338000
CONV#RTN AMODE ANY                                                      02339000
CONV#RTN RMODE ANY                                                      02340000
IN#HOST  DS    0H                      MOVE INTO HOST VARIABLES         02341000
         USING IN#HOST,5               ESTABLISH BASE REGISTER          02342000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    02343000
IN#HOST1 CLI   0(6),X'FF'              END OF TABLE?                    02344000
         BER   4                         YES - RETURN TO SQL            02345000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         02346000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    02347000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  02348000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 02349000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              02350000
         LR    15,1                    SET BOTH LENGTHS EQUAL           02351000
         MVCL  14,0                    MOVE TO HOST VARIABLES           02352000
         LA    6,8(6)                  INCREMENT TO NEXT ENTRY          02353000
         B     IN#HOST1                LOOP UNTIL END OF TABLE          02354000
*                                                                       02355000
IN#KEY   DS    0H                      MOVE KEYS INTO HOST VARIABLES    02356000
         USING IN#KEY,5                ESTABLISH BASE REGISTER          02357000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    02358000
IN#KEY1  CLI   0(6),X'FF'              END OF TABLE?                    02359000
         BER   4                         YES - RETURN TO SQL            02360000
         SLR   0,0                     CLEAR FOR INSERT                 02361000
         IC    0,6(6)                  INSERT KEY FIELD ENTRY           02362000
         NR    0,3                     FOR THE CURRENT KEY?             02363000
         BZ    IN#KEY2                   NO - GO FOR NEXT ENTRY         02364000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         02365000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    02366000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  02367000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 02368000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              02369000
         LR    15,1                    SET BOTH LENGTHS EQUAL           02370000
         MVCL  14,0                    MOVE TO HOST VARIABLES           02371000
         TM    7(6),X'80'              SIGNED DECIMAL FIELD?            02372000
         BNO   IN#KEY2                   NO - GO FOR NEXT ENTRY         02373000
         BCTR  14,0                    DECREMENT TO SIGN BYTE           02374000
         TM    0(14),X'0A'             VALID SIGN BYTE?                 02375000
         BO    IN#KEY2                   YES - GO FOR NEXT ENTRY        02376000
         TM    0(14),X'0C'             VALID SIGN BYTE?                 02377000
         BO    IN#KEY2                   YES - GO FOR NEXT ENTRY        02378000
         OI    0(14),X'0F'               NO  - MAKE IT VALID            02379000
IN#KEY2  LA    6,8(6)                  INCREMENT TO NEXT ENTRY          02380000
         B     IN#KEY1                 LOOP UNTIL END OF TABLE          02381000
*                                                                       02382000
OUT#REC  DS    0H                      MOVE HOST VARIABLES INTO RECORD  02383000
         USING OUT#REC,5               ESTABLISH BASE REGISTER          02384000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    02385000
OUT#REC1 CLI   0(6),X'FF'              END OF TABLE?                    02386000
         BER   4                         YES - RETURN TO SQL            02387000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         02388000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    02389000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  02390000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 02391000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              02392000
         LR    15,1                    SET BOTH LENGTHS EQUAL           02393000
         MVCL  0,14                    MOVE TO RECORD AREA              02394000
         LA    6,8(6)                  INCREMENT TO NEXT ENTRY          02395000
         B     OUT#REC1                LOOP UNTIL END OF TABLE          02396000
*                                                                       02397000
OUT#KEY  DS    0H                      MOVE KEYS INTO RECORD            02398000
         USING OUT#KEY,5               ESTABLISH BASE REGISTER          02399000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    02400000
OUT#KEY1 CLI   0(6),X'FF'              END OF TABLE?                    02401000
         BER   4                         YES - RETURN TO SQL            02402000
         TM    6(6),X'80'              FOR THE PRIMARY KEY?             02403000
         BZ    OUT#KEY2                  NO - GO FOR NEXT ENTRY         02404000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         02405000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    02406000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  02407000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 02408000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              02409000
         LR    15,1                    SET BOTH LENGTHS EQUAL           02410000
         MVCL  0,14                    MOVE TO RECORD AREA              02411000
OUT#KEY2 LA    6,8(6)                  INCREMENT TO NEXT ENTRY          02412000
         B     OUT#KEY1                LOOP UNTIL END OF TABLE          02413000
*                                                                       02414000
*                                                                       02415000
**********************************************************************  02416000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  02417000
*   MUST NOT BE MODIFIED.                                               02418000
**********************************************************************  02419000
*                                                                       02420000
CONV#TAB DC    H'0000'                 COBOL RECORD AREA OFFSET         02421000
         DC    H'0000'                 HOST VARIABLE AREA OFFSET        02422000
         DC    H'0022'                 LENGTH TO MOVE                   02423000
         DC    X'80'                   80 = KEY 0 FIELD                 02424000
*                                      40 = KEY 1 FIELD                 02425000
*                                      20 = KEY 2 FIELD                 02426000
*                                      10 = KEY 3 FIELD                 02427000
         DC    X'00'                   80 = SIGNED DECIMAL FIELD        02428000
*                                                                       02429000
*                                      REST OF FIELD ENTRIES            02430000
         DC    H'0022',H'0022',H'0229',X'00',X'00'                      02431000
         DC    8X'FF'                  END OF FIELD ENTRIES             02432000
         LTORG                                                          02433000
         END                                                            02434000
