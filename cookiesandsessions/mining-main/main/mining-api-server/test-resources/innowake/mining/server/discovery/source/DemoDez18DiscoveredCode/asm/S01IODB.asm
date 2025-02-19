* REL=S01IODB.10                                                        00001000
**********************************************************************  00002000
*                                                                       00003000
*  S01IODB .... STATIC SQL I/O MODULE                                   00004000
*                                                                       00005000
*  CREATION DATE: 04/25/16                                              00006000
*                                                                       00007000
*  FUNCTIONAL DESCRIPTION: THIS PROGRAM CONTAINS THE STATIC SQL         00008000
*  VECTORS REQUIRED TO SUPPORT I/O TO THE S01 FILE.  IT IS LOADED       00009000
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
COBREC   DS    CL327                   COBOL RECORD (UNALIGNED)         00036000
ASMREC   DS    0F                      ASSEMBLER RECORD (ALIGNED)       00037000
INST     DS    CL4                                                      00038000
RECNBR   DS    CL4                                                      00039000
MODEL    DS    CL10                                                     00040000
EFFDATE  DS    CL8                                                      00041000
AUDDATE  DS    PL5'0.'                                                  00042000
AUDTIME  DS    PL5'0.'                                                  00043000
AUDUSER  DS    CL8                                                      00044000
AUDORG   DS    CL6                                                      00045000
ACTVCDE  DS    CL1                                                      00046000
SHRTDESC DS    CL20                                                     00047000
SCORE    DS    CL3                                                      00048000
AUENROLL DS    CL1                                                      00049000
INCTVBAL DS    CL1                                                      00050000
PROCOPT  DS    CL1                                                      00051000
CBALOPT  DS    CL1                                                      00052000
EXPFREQ  DS    CL1                                                      00053000
EXPTERM  DS    PL2'0.'                                                  00054000
ENDMNTH  DS    CL1                                                      00055000
CLSD     DS    PL5'0.'                                                  00056000
EXPRDATE DS    PL5'0.'                                                  00057000
EXRETDAY DS    PL2'0.'                                                  00058000
FIRSTRET DS    PL2'0.'                                                  00059000
DAYSEXT  DS    PL2'0.'                                                  00060000
EXTN     DS    CL2                                                      00061000
CASHOPT  DS    CL1                                                      00062000
CASHPCT  DS    PL5'0.000000000'                                         00063000
CASHBAL  DS    CL1                                                      00064000
CASHFREQ DS    CL1                                                      00065000
CASHTERM DS    PL2'0.'                                                  00066000
CASHDAY  DS    CL2                                                      00067000
NBRACCT  DS    CL2                                                      00068000
ACCTQLFY DS    CL1                                                      00069000
NBRPROD  DS    CL2                                                      00070000
PRODQLFY DS    CL1                                                      00071000
MINACCT  DS    CL2                                                      00072000
MINACCTQ DS    CL1                                                      00073000
MAXACCT  DS    CL2                                                      00074000
MAXACCTQ DS    CL1                                                      00075000
MINPROD  DS    CL2                                                      00076000
MINPRODQ DS    CL1                                                      00077000
MAXPROD  DS    CL2                                                      00078000
MAXPRODQ DS    CL1                                                      00079000
DEMOCODE DS    CL6                                                      00080000
DEMOQLFY DS    CL1                                                      00081000
CUSTCODE DS    CL6                                                      00082000
CUSTQLFY DS    CL1                                                      00083000
PBALOPT  DS    CL1                                                      00084000
PBALOPTQ DS    CL1                                                      00085000
PBALOPTI DS    CL1                                                      00086000
ACCTCDQ  DS    CL1                                                      00087000
ACCTCDI  DS    CL1                                                      00088000
ACTRELQ  DS    CL1                                                      00089000
ACINCEN  DS    CL1                                                      00090000
COMBCD1  DS    CL6                                                      00091000
CCQLFY1  DS    CL1                                                      00092000
CC1INCTV DS    CL1                                                      00093000
COMBCD2  DS    CL6                                                      00094000
CCQLFY2  DS    CL1                                                      00095000
CC2INCTV DS    CL1                                                      00096000
PRCOME1  DS    CL1                                                      00097000
PC1QLFY  DS    CL1                                                      00098000
PC1INCTV DS    CL1                                                      00099000
PRCOME2  DS    CL1                                                      00100000
PC2QLFY  DS    CL1                                                      00101000
PC2INCTV DS    CL1                                                      00102000
PGMIN1   DS    CL2                                                      00103000
PGMAX1   DS    CL2                                                      00104000
PRDGRP1Q DS    CL1                                                      00105000
PGMIN2   DS    CL2                                                      00106000
PGMAX2   DS    CL2                                                      00107000
PRDGRP2Q DS    CL1                                                      00108000
PGMIN3   DS    CL2                                                      00109000
PGMAX3   DS    CL2                                                      00110000
PRDGRP3Q DS    CL1                                                      00111000
PGMIN4   DS    CL2                                                      00112000
PGMAX4   DS    CL2                                                      00113000
PRDGRP4Q DS    CL1                                                      00114000
PGMIN5   DS    CL2                                                      00115000
PGMAX5   DS    CL2                                                      00116000
PRDGRP5Q DS    CL1                                                      00117000
INCENTOV DS    CL1                                                      00118000
DFLTMODL DS    CL10                                                     00119000
INCBALTR DS    CL1                                                      00120000
TRANPROF DS    CL6                                                      00121000
TRPINCEN DS    CL1                                                      00122000
CRWAMT   DS    PL6'0.00'                                                00123000
CRWMAXCB DS    PL8'0.00'                                                00124000
COMBCD3  DS    CL6                                                      00125000
CCQLFY3  DS    CL1                                                      00126000
CC3INCTV DS    CL1                                                      00127000
COMBCD4  DS    CL6                                                      00128000
CCQLFY4  DS    CL1                                                      00129000
CC4INCTV DS    CL1                                                      00130000
PRCOME3  DS    CL1                                                      00131000
PC3QLFY  DS    CL1                                                      00132000
PC3INCTV DS    CL1                                                      00133000
PRCOME4  DS    CL1                                                      00134000
PC4QLFY  DS    CL1                                                      00135000
PC4INCTV DS    CL1                                                      00136000
RSVFLAG1 DS    CL1                                                      00137000
CLACCOPT DS    CL1                                                      00138000
SECSVOPT DS    CL1                                                      00139000
RATEOPT  DS    CL1                                                      00140000
ENRATTP  DS    CL2                                                      00141000
MODELTYP DS    CL10                                                     00142000
LONGDESC DS    CL50                                                     00143000
PRIMCODE DS    CL6                                                      00144000
PRIMQLFY DS    CL1                                                      00145000
EXTENROL DS    CL1                                                      00146000
*                                                                       00147000
*                                                                       00148000
**********************************************************************  00149000
* PROGRAM TABLE HEADER SECTION:                                         00150000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00151000
*   THE CONTENTS OF THIS SECTION SHOULD NOT BE MODIFIED.                00152000
**********************************************************************  00153000
*                                                                       00154000
S01IODB  CSECT                         PROGRAM TABLE SECTION            00155000
S01IODB  AMODE ANY                                                      00156000
S01IODB  RMODE ANY                                                      00157000
         DC    CL9'S01IODB'            PROGRAM ID                       00158000
         DC    CL8'&SYSDATE',CL1' ',CL5'&SYSTIME',CL1' '                00159000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00160000
         DC    A(0)                    RESERVED                         00161000
         DC    A(0)                    RESERVED                         00162000
         DC    24CL1' '                RESERVED                         00163000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00164000
*                                                                       00165000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00166000
         DC    CL29'WWW.INFOR.COM                '                      00166001
*                                                                       00167000
*                                                                       00168000
**********************************************************************  00169000
* STATEMENT TABLE SECTION:                                              00170000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00171000
*   THE CONTENTS AND ORDER OF THE STATEMENTS IN THIS SECTION            00172000
*   SHOULD NOT BE MODIFIED.                                             00173000
**********************************************************************  00174000
*                                                                       00175000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00176000
STM#TAB  AMODE ANY                                                      00177000
STM#TAB  RMODE ANY                                                      00178000
         DC    A(SQL#0000)             SELECT INTO (KEY 0)              00179000
         DC    A(SQL#0001)             SELECT INTO (KEY 1)              00180000
         DC    A(SQL#0002)             SELECT INTO (KEY 2)              00181000
         DC    A(SQL#0003)             SELECT INTO (KEY 3)              00182000
         DC    A(SQL#0004)             SELECT UPDATE CURSOR (KEY 0)     00183000
         DC    A(SQL#0005)             SELECT UPDATE CURSOR (KEY 1)     00184000
         DC    A(SQL#0006)             SELECT UPDATE CURSOR (KEY 2)     00185000
         DC    A(SQL#0007)             SELECT UPDATE CURSOR (KEY 3)     00186000
         DC    A(SQL#0008)             FETCH UPDATE CURSOR (KEY 0)      00187000
         DC    A(SQL#0009)             FETCH UPDATE CURSOR (KEY 1)      00188000
         DC    A(SQL#0010)             FETCH UPDATE CURSOR (KEY 2)      00189000
         DC    A(SQL#0011)             FETCH UPDATE CURSOR (KEY 3)      00190000
         DC    A(SQL#0012)             CLOSE UPDATE CURSOR (KEY 0)      00191000
         DC    A(SQL#0013)             CLOSE UPDATE CURSOR (KEY 1)      00192000
         DC    A(SQL#0014)             CLOSE UPDATE CURSOR (KEY 2)      00193000
         DC    A(SQL#0015)             CLOSE UPDATE CURSOR (KEY 3)      00194000
         DC    A(SQL#0016)             SELECT SEQ CURSOR (KEY 0)        00195000
         DC    A(SQL#0017)             SELECT SEQ CURSOR (KEY 1)        00196000
         DC    A(SQL#0018)             SELECT SEQ CURSOR (KEY 2)        00197000
         DC    A(SQL#0019)             SELECT SEQ CURSOR (KEY 3)        00198000
         DC    A(SQL#0020)             FETCH SEQ CURSOR (KEY 0)         00199000
         DC    A(SQL#0021)             FETCH SEQ CURSOR (KEY 1)         00200000
         DC    A(SQL#0022)             FETCH SEQ CURSOR (KEY 2)         00201000
         DC    A(SQL#0023)             FETCH SEQ CURSOR (KEY 3)         00202000
         DC    A(SQL#0024)             FETCH SEQ CURSOR UPDATE (KEY 0)  00203000
         DC    A(SQL#0025)             FETCH SEQ CURSOR UPDATE (KEY 1)  00204000
         DC    A(SQL#0026)             FETCH SEQ CURSOR UPDATE (KEY 2)  00205000
         DC    A(SQL#0027)             FETCH SEQ CURSOR UPDATE (KEY 3)  00206000
         DC    A(SQL#0028)             CLOSE SEQ CURSOR (KEY 0)         00207000
         DC    A(SQL#0029)             CLOSE SEQ CURSOR (KEY 1)         00208000
         DC    A(SQL#0030)             CLOSE SEQ CURSOR (KEY 2)         00209000
         DC    A(SQL#0031)             CLOSE SEQ CURSOR (KEY 3)         00210000
         DC    A(SQL#0032)             SELECT KEY (KEY 0)               00211000
         DC    A(SQL#0033)             SELECT KEY (KEY 1)               00212000
         DC    A(SQL#0034)             SELECT KEY (KEY 2)               00213000
         DC    A(SQL#0035)             SELECT KEY (KEY 3)               00214000
         DC    A(SQL#0036)             INSERT STATEMENT                 00215000
         DC    A(SQL#0037)             UPDATE STATEMENT (KEY 0)         00216000
         DC    A(SQL#0038)             UPDATE STATEMENT (KEY 1)         00217000
         DC    A(SQL#0039)             UPDATE STATEMENT (KEY 2)         00218000
         DC    A(SQL#0040)             UPDATE STATEMENT (KEY 3)         00219000
         DC    A(SQL#0041)             DELETE STATEMENT (KEY 0)         00220000
         DC    A(SQL#0042)             DELETE STATEMENT (KEY 1)         00221000
         DC    A(SQL#0043)             DELETE STATEMENT (KEY 2)         00222000
         DC    A(SQL#0044)             DELETE STATEMENT (KEY 3)         00223000
         DC    A(SQL#0045)             DELETE ALL STATEMENT             00224000
         DC    4X'FF'                                                   00225000
*                                                                       00226000
*                                                                       00227000
**********************************************************************  00228000
* SQL STATEMENT SECTION:                                                00229000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00230000
*     TO SUPPORT THIS TABLE.                                            00231000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00232000
*     ARE EQUIVALENT.                                                   00233000
**********************************************************************  00234000
*                                                                       00235000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00236000
SQL#STMT AMODE ANY                                                      00237000
SQL#STMT RMODE ANY                                                      00238000
         USING HOST#VAR,2              ADDRESS HOST VARIABLES           00239000
         USING SQLDSECT,10,3,4         ADDRESS SQLDSECT                 00240000
         USING COM#AREA,11             ADDRESS COMMAREA                 00241000
*                                                                       00242000
*                                                                       00243000
**********************************************************************  00244000
* SELECT INTO STATEMENT BY PRIMARY KEY:                                 00245000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00246000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00247000
**********************************************************************  00248000
*                                                                       00249000
SQL#0000 DS    0H                                                       00250000
         USING SQL#0000,12,7           ESTABLISH BASE REGISTER          00251000
         B     *+6                     BRANCH AROUND ADCON              00252000
BASE0000 DC    AL2(4096)                                                00253000
         LR    7,12                    LOAD SECOND BASE                 00254000
         AH    7,BASE0000              ADD 4K                           00255000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00256000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00257000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00258000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00259000
         AH    3,BASE0000              ADD 4K                           00260000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00261000
         AH    4,BASE0000              ADD 4K                           00262000
         EXEC  SQL                                                     *00263000
               SELECT AUDIT_DATE,                                      *00264000
                   AUDIT_TIME,                                         *00265000
                   AUDIT_USER,                                         *00266000
                   AUDIT_ORG,                                          *00267000
                   ACTIVE_CODE,                                        *00268000
                   SHORT_DESC,                                         *00269000
                   SCORE,                                              *00270000
                   AUTO_ENROLL,                                        *00271000
                   INCEN_BAL_OPT,                                      *00272000
                   PROCESS_OPTION,                                     *00273000
                   CMB_BAL_OPT,                                        *00274000
                   EXPIRE_FREQ,                                        *00275000
                   EXPIRE_TERM,                                        *00276000
                   END_MONTH,                                          *00277000
                   CLOSE_DATE,                                         *00278000
                   EXPIRE_DATE,                                        *00279000
                   EXPIRE_RET_DAYS,                                    *00280000
                   FIRST_RETENTION,                                    *00281000
                   DAY_PER_EXTN,                                       *00282000
                   EXTENSION,                                          *00283000
                   CASH_OPT,                                           *00284000
                   CASH_PCT,                                           *00285000
                   CASH_BAL_OPT,                                       *00286000
                   CASH_FREQ,                                          *00287000
                   CASH_TERM,                                          *00288000
                   CASH_DAY,                                           *00289000
                   NBR_ACCOUNTS,                                       *00290000
                   ACCT_QLFY,                                          *00291000
                   NBR_PROD,                                           *00292000
                   PROD_QLFY,                                          *00293000
                   MIN_ACCT,                                           *00294000
                   MIN_ACCT_Q,                                         *00295000
                   MAX_ACCT,                                           *00296000
                   MAX_ACCT_Q,                                         *00297000
                   MIN_PROD,                                           *00298000
                   MIN_PROD_Q,                                         *00299000
                   MAX_PROD,                                           *00300000
                   MAX_PROD_Q,                                         *00301000
                   DEMOGR_CODE,                                        *00302000
                   DEMOGR_QLFY,                                        *00303000
                   CUST_REL_CODE,                                      *00304000
                   CUST_QLFY,                                          *00305000
                   PRIM_BAL_OPT,                                       *00306000
                   PRIM_BAL_OPT_Q,                                     *00307000
                   PRIM_BAL_OPT_I,                                     *00308000
                   ACCT_REL_CD_Q,                                      *00309000
                   ACCT_REL_CD_I,                                      *00310000
                   ACCT_REL_Q,                                         *00311000
                   ACCT_REL_INCEN,                                     *00312000
                   CMB_CATG_CD_1,                                      *00313000
                   CMB_CATG_1_Q,                                       *00314000
                   CMB_CATG_1_INC,                                     *00315000
                   CMB_CATG_CD_2,                                      *00316000
                   CMB_CATG_2_Q,                                       *00317000
                   CMB_CATG_2_INC,                                     *00318000
                   PRIME_CMB_OPT_1,                                    *00319000
                   PRIME_C_1_QLFY,                                     *00320000
                   PRIME_C_1_INCEN,                                    *00321000
                   PRIME_CMB_OPT_2,                                    *00322000
                   PRIME_C_2_QLFY,                                     *00323000
                   PRIME_C_2_INCEN,                                    *00324000
                   PROD_GRP_MIN_1,                                     *00325000
                   PROD_GRP_MAX_1,                                     *00326000
                   PROD_GRP_1_Q,                                       *00327000
                   PROD_GRP_MIN_2,                                     *00328000
                   PROD_GRP_MAX_2,                                     *00329000
                   PROD_GRP_2_Q,                                       *00330000
                   PROD_GRP_MIN_3,                                     *00331000
                   PROD_GRP_MAX_3,                                     *00332000
                   PROD_GRP_3_Q,                                       *00333000
                   PROD_GRP_MIN_4,                                     *00334000
                   PROD_GRP_MAX_4,                                     *00335000
                   PROD_GRP_4_Q,                                       *00336000
                   PROD_GRP_MIN_5,                                     *00337000
                   PROD_GRP_MAX_5,                                     *00338000
                   PROD_GRP_5_Q,                                       *00339000
                   ACCT_INCEN_OVRD,                                    *00340000
                   DEFAULT_MODEL,                                      *00341000
                   INCEN_BAL_TRN,                                      *00342000
                   TRAN_PROFILE,                                       *00343000
                   TRAN_INCENTIVE,                                     *00344000
                   CASH_MAX_AMT_PD,                                    *00345000
                   CASH_MAX_BAL,                                       *00346000
                   CMB_CATG_CD_3,                                      *00347000
                   CMB_CATG_3_Q,                                       *00348000
                   CMB_CATG_3_INC,                                     *00349000
                   CMB_CATG_CD_4,                                      *00350000
                   CMB_CATG_4_Q,                                       *00351000
                   CMB_CATG_4_INC,                                     *00352000
                   PRIME_CMB_OPT_3,                                    *00353000
                   PRIME_C_3_QLFY,                                     *00354000
                   PRIME_C_3_INCEN,                                    *00355000
                   PRIME_CMB_OPT_4,                                    *00356000
                   PRIME_C_4_QLFY,                                     *00357000
                   PRIME_C_4_INCEN,                                    *00358000
                   RSV_FLAG_1,                                         *00359000
                   CLOSED_ACCT_OPT,                                    *00360000
                   SEC_SVC_OPT,                                        *00361000
                   RATE_OPTION,                                        *00362000
                   ENROLL_LEFT,                                        *00363000
                   MODEL_TYPE,                                         *00364000
                   LONG_DESC,                                          *00365000
                   PRIM_REL_CODE,                                      *00366000
                   PRIM_QLFY,                                          *00367000
                   EXTL_ENROLL                                         *00368000
                 INTO :AUDDATE,                                        *00369000
                   :AUDTIME,                                           *00370000
                   :AUDUSER,                                           *00371000
                   :AUDORG,                                            *00372000
                   :ACTVCDE,                                           *00373000
                   :SHRTDESC,                                          *00374000
                   :SCORE,                                             *00375000
                   :AUENROLL,                                          *00376000
                   :INCTVBAL,                                          *00377000
                   :PROCOPT,                                           *00378000
                   :CBALOPT,                                           *00379000
                   :EXPFREQ,                                           *00380000
                   :EXPTERM,                                           *00381000
                   :ENDMNTH,                                           *00382000
                   :CLSD,                                              *00383000
                   :EXPRDATE,                                          *00384000
                   :EXRETDAY,                                          *00385000
                   :FIRSTRET,                                          *00386000
                   :DAYSEXT,                                           *00387000
                   :EXTN,                                              *00388000
                   :CASHOPT,                                           *00389000
                   :CASHPCT,                                           *00390000
                   :CASHBAL,                                           *00391000
                   :CASHFREQ,                                          *00392000
                   :CASHTERM,                                          *00393000
                   :CASHDAY,                                           *00394000
                   :NBRACCT,                                           *00395000
                   :ACCTQLFY,                                          *00396000
                   :NBRPROD,                                           *00397000
                   :PRODQLFY,                                          *00398000
                   :MINACCT,                                           *00399000
                   :MINACCTQ,                                          *00400000
                   :MAXACCT,                                           *00401000
                   :MAXACCTQ,                                          *00402000
                   :MINPROD,                                           *00403000
                   :MINPRODQ,                                          *00404000
                   :MAXPROD,                                           *00405000
                   :MAXPRODQ,                                          *00406000
                   :DEMOCODE,                                          *00407000
                   :DEMOQLFY,                                          *00408000
                   :CUSTCODE,                                          *00409000
                   :CUSTQLFY,                                          *00410000
                   :PBALOPT,                                           *00411000
                   :PBALOPTQ,                                          *00412000
                   :PBALOPTI,                                          *00413000
                   :ACCTCDQ,                                           *00414000
                   :ACCTCDI,                                           *00415000
                   :ACTRELQ,                                           *00416000
                   :ACINCEN,                                           *00417000
                   :COMBCD1,                                           *00418000
                   :CCQLFY1,                                           *00419000
                   :CC1INCTV,                                          *00420000
                   :COMBCD2,                                           *00421000
                   :CCQLFY2,                                           *00422000
                   :CC2INCTV,                                          *00423000
                   :PRCOME1,                                           *00424000
                   :PC1QLFY,                                           *00425000
                   :PC1INCTV,                                          *00426000
                   :PRCOME2,                                           *00427000
                   :PC2QLFY,                                           *00428000
                   :PC2INCTV,                                          *00429000
                   :PGMIN1,                                            *00430000
                   :PGMAX1,                                            *00431000
                   :PRDGRP1Q,                                          *00432000
                   :PGMIN2,                                            *00433000
                   :PGMAX2,                                            *00434000
                   :PRDGRP2Q,                                          *00435000
                   :PGMIN3,                                            *00436000
                   :PGMAX3,                                            *00437000
                   :PRDGRP3Q,                                          *00438000
                   :PGMIN4,                                            *00439000
                   :PGMAX4,                                            *00440000
                   :PRDGRP4Q,                                          *00441000
                   :PGMIN5,                                            *00442000
                   :PGMAX5,                                            *00443000
                   :PRDGRP5Q,                                          *00444000
                   :INCENTOV,                                          *00445000
                   :DFLTMODL,                                          *00446000
                   :INCBALTR,                                          *00447000
                   :TRANPROF,                                          *00448000
                   :TRPINCEN,                                          *00449000
                   :CRWAMT,                                            *00450000
                   :CRWMAXCB,                                          *00451000
                   :COMBCD3,                                           *00452000
                   :CCQLFY3,                                           *00453000
                   :CC3INCTV,                                          *00454000
                   :COMBCD4,                                           *00455000
                   :CCQLFY4,                                           *00456000
                   :CC4INCTV,                                          *00457000
                   :PRCOME3,                                           *00458000
                   :PC3QLFY,                                           *00459000
                   :PC3INCTV,                                          *00460000
                   :PRCOME4,                                           *00461000
                   :PC4QLFY,                                           *00462000
                   :PC4INCTV,                                          *00463000
                   :RSVFLAG1,                                          *00464000
                   :CLACCOPT,                                          *00465000
                   :SECSVOPT,                                          *00466000
                   :RATEOPT,                                           *00467000
                   :ENRATTP,                                           *00468000
                   :MODELTYP,                                          *00469000
                   :LONGDESC,                                          *00470000
                   :PRIMCODE,                                          *00471000
                   :PRIMQLFY,                                          *00472000
                   :EXTENROL                                           *00473000
                 FROM S01                                              *00474000
                 WHERE INST_NBR = :INST AND                            *00475000
                   RECORD_NBR = :RECNBR AND                            *00476000
                   MODEL = :MODEL AND                                  *00477000
                   EFFECTIVE_DATE = :EFFDATE                           *00478000
                 FETCH FIRST 1 ROW ONLY                                 00479000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   00480000
         BALR  4,5                     MOVE INTO RECORD AREA            00481000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00482000
         BR    14                      RETURN TO CALLER                 00483000
         LTORG                                                          00484000
*                                                                       00485000
*                                                                       00486000
**********************************************************************  00487000
* SELECT INTO STATEMENT BY ALTERNATE KEY 1:                             00488000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00489000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00490000
**********************************************************************  00491000
*                                                                       00492000
SQL#0001 DS    0H                                                       00493000
         USING SQL#0001,12,7           ESTABLISH BASE REGISTER          00494000
         LA    15,255                  SET RETURN CODE                  00495000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00496000
         BR    14                      RETURN TO CALLER                 00497000
         LTORG                                                          00498000
*                                                                       00499000
*                                                                       00500000
**********************************************************************  00501000
* SELECT INTO STATEMENT BY ALTERNATE KEY 2:                             00502000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00503000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00504000
**********************************************************************  00505000
*                                                                       00506000
SQL#0002 DS    0H                                                       00507000
         USING SQL#0002,12,7           ESTABLISH BASE REGISTER          00508000
         LA    15,255                  SET RETURN CODE                  00509000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00510000
         BR    14                      RETURN TO CALLER                 00511000
         LTORG                                                          00512000
*                                                                       00513000
*                                                                       00514000
**********************************************************************  00515000
* SELECT INTO STATEMENT BY ALTERNATE KEY 3:                             00516000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00517000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00518000
**********************************************************************  00519000
*                                                                       00520000
SQL#0003 DS    0H                                                       00521000
         USING SQL#0003,12,7           ESTABLISH BASE REGISTER          00522000
         LA    15,255                  SET RETURN CODE                  00523000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00524000
         BR    14                      RETURN TO CALLER                 00525000
         LTORG                                                          00526000
*                                                                       00527000
*                                                                       00528000
**********************************************************************  00529000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY PRIMARY KEY:       00530000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00531000
*   THEY ARE ALSO USED AFTER A SUCCESSFUL SELECT SEQUENTIAL STATEMENT   00532000
*     FOR THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS.                      00533000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00534000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00535000
*     UPDATE STATEMENT TO MATCH.                                        00536000
**********************************************************************  00537000
*                                                                       00538000
SQL#0004 DS    0H                                                       00539000
         USING SQL#0004,12,7           ESTABLISH BASE REGISTER          00540000
         B     *+6                     BRANCH AROUND ADCON              00541000
BASE0004 DC    AL2(4096)                                                00542000
         LR    7,12                    LOAD SECOND BASE                 00543000
         AH    7,BASE0004              ADD 4K                           00544000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00545000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00546000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00547000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00548000
         AH    3,BASE0004              ADD 4K                           00549000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00550000
         AH    4,BASE0004              ADD 4K                           00551000
         EXEC  SQL                                                     *00552000
               DECLARE S01UPD0 CURSOR FOR                              *00553000
               SELECT AUDIT_DATE,                                      *00554000
                   AUDIT_TIME,                                         *00555000
                   AUDIT_USER,                                         *00556000
                   AUDIT_ORG,                                          *00557000
                   ACTIVE_CODE,                                        *00558000
                   SHORT_DESC,                                         *00559000
                   SCORE,                                              *00560000
                   AUTO_ENROLL,                                        *00561000
                   INCEN_BAL_OPT,                                      *00562000
                   PROCESS_OPTION,                                     *00563000
                   CMB_BAL_OPT,                                        *00564000
                   EXPIRE_FREQ,                                        *00565000
                   EXPIRE_TERM,                                        *00566000
                   END_MONTH,                                          *00567000
                   CLOSE_DATE,                                         *00568000
                   EXPIRE_DATE,                                        *00569000
                   EXPIRE_RET_DAYS,                                    *00570000
                   FIRST_RETENTION,                                    *00571000
                   DAY_PER_EXTN,                                       *00572000
                   EXTENSION,                                          *00573000
                   CASH_OPT,                                           *00574000
                   CASH_PCT,                                           *00575000
                   CASH_BAL_OPT,                                       *00576000
                   CASH_FREQ,                                          *00577000
                   CASH_TERM,                                          *00578000
                   CASH_DAY,                                           *00579000
                   NBR_ACCOUNTS,                                       *00580000
                   ACCT_QLFY,                                          *00581000
                   NBR_PROD,                                           *00582000
                   PROD_QLFY,                                          *00583000
                   MIN_ACCT,                                           *00584000
                   MIN_ACCT_Q,                                         *00585000
                   MAX_ACCT,                                           *00586000
                   MAX_ACCT_Q,                                         *00587000
                   MIN_PROD,                                           *00588000
                   MIN_PROD_Q,                                         *00589000
                   MAX_PROD,                                           *00590000
                   MAX_PROD_Q,                                         *00591000
                   DEMOGR_CODE,                                        *00592000
                   DEMOGR_QLFY,                                        *00593000
                   CUST_REL_CODE,                                      *00594000
                   CUST_QLFY,                                          *00595000
                   PRIM_BAL_OPT,                                       *00596000
                   PRIM_BAL_OPT_Q,                                     *00597000
                   PRIM_BAL_OPT_I,                                     *00598000
                   ACCT_REL_CD_Q,                                      *00599000
                   ACCT_REL_CD_I,                                      *00600000
                   ACCT_REL_Q,                                         *00601000
                   ACCT_REL_INCEN,                                     *00602000
                   CMB_CATG_CD_1,                                      *00603000
                   CMB_CATG_1_Q,                                       *00604000
                   CMB_CATG_1_INC,                                     *00605000
                   CMB_CATG_CD_2,                                      *00606000
                   CMB_CATG_2_Q,                                       *00607000
                   CMB_CATG_2_INC,                                     *00608000
                   PRIME_CMB_OPT_1,                                    *00609000
                   PRIME_C_1_QLFY,                                     *00610000
                   PRIME_C_1_INCEN,                                    *00611000
                   PRIME_CMB_OPT_2,                                    *00612000
                   PRIME_C_2_QLFY,                                     *00613000
                   PRIME_C_2_INCEN,                                    *00614000
                   PROD_GRP_MIN_1,                                     *00615000
                   PROD_GRP_MAX_1,                                     *00616000
                   PROD_GRP_1_Q,                                       *00617000
                   PROD_GRP_MIN_2,                                     *00618000
                   PROD_GRP_MAX_2,                                     *00619000
                   PROD_GRP_2_Q,                                       *00620000
                   PROD_GRP_MIN_3,                                     *00621000
                   PROD_GRP_MAX_3,                                     *00622000
                   PROD_GRP_3_Q,                                       *00623000
                   PROD_GRP_MIN_4,                                     *00624000
                   PROD_GRP_MAX_4,                                     *00625000
                   PROD_GRP_4_Q,                                       *00626000
                   PROD_GRP_MIN_5,                                     *00627000
                   PROD_GRP_MAX_5,                                     *00628000
                   PROD_GRP_5_Q,                                       *00629000
                   ACCT_INCEN_OVRD,                                    *00630000
                   DEFAULT_MODEL,                                      *00631000
                   INCEN_BAL_TRN,                                      *00632000
                   TRAN_PROFILE,                                       *00633000
                   TRAN_INCENTIVE,                                     *00634000
                   CASH_MAX_AMT_PD,                                    *00635000
                   CASH_MAX_BAL,                                       *00636000
                   CMB_CATG_CD_3,                                      *00637000
                   CMB_CATG_3_Q,                                       *00638000
                   CMB_CATG_3_INC,                                     *00639000
                   CMB_CATG_CD_4,                                      *00640000
                   CMB_CATG_4_Q,                                       *00641000
                   CMB_CATG_4_INC,                                     *00642000
                   PRIME_CMB_OPT_3,                                    *00643000
                   PRIME_C_3_QLFY,                                     *00644000
                   PRIME_C_3_INCEN,                                    *00645000
                   PRIME_CMB_OPT_4,                                    *00646000
                   PRIME_C_4_QLFY,                                     *00647000
                   PRIME_C_4_INCEN,                                    *00648000
                   RSV_FLAG_1,                                         *00649000
                   CLOSED_ACCT_OPT,                                    *00650000
                   SEC_SVC_OPT,                                        *00651000
                   RATE_OPTION,                                        *00652000
                   ENROLL_LEFT,                                        *00653000
                   MODEL_TYPE,                                         *00654000
                   LONG_DESC,                                          *00655000
                   PRIM_REL_CODE,                                      *00656000
                   PRIM_QLFY,                                          *00657000
                   EXTL_ENROLL                                         *00658000
                 FROM S01                                              *00659000
                 WHERE INST_NBR = :INST AND                            *00660000
                   RECORD_NBR = :RECNBR AND                            *00661000
                   MODEL = :MODEL AND                                  *00662000
                   EFFECTIVE_DATE = :EFFDATE                           *00663000
                 FOR UPDATE OF AUDIT_DATE,                             *00664000
                   AUDIT_TIME,                                         *00665000
                   AUDIT_USER,                                         *00666000
                   AUDIT_ORG,                                          *00667000
                   ACTIVE_CODE,                                        *00668000
                   SHORT_DESC,                                         *00669000
                   SCORE,                                              *00670000
                   AUTO_ENROLL,                                        *00671000
                   INCEN_BAL_OPT,                                      *00672000
                   PROCESS_OPTION,                                     *00673000
                   CMB_BAL_OPT,                                        *00674000
                   EXPIRE_FREQ,                                        *00675000
                   EXPIRE_TERM,                                        *00676000
                   END_MONTH,                                          *00677000
                   CLOSE_DATE,                                         *00678000
                   EXPIRE_DATE,                                        *00679000
                   EXPIRE_RET_DAYS,                                    *00680000
                   FIRST_RETENTION,                                    *00681000
                   DAY_PER_EXTN,                                       *00682000
                   EXTENSION,                                          *00683000
                   CASH_OPT,                                           *00684000
                   CASH_PCT,                                           *00685000
                   CASH_BAL_OPT,                                       *00686000
                   CASH_FREQ,                                          *00687000
                   CASH_TERM,                                          *00688000
                   CASH_DAY,                                           *00689000
                   NBR_ACCOUNTS,                                       *00690000
                   ACCT_QLFY,                                          *00691000
                   NBR_PROD,                                           *00692000
                   PROD_QLFY,                                          *00693000
                   MIN_ACCT,                                           *00694000
                   MIN_ACCT_Q,                                         *00695000
                   MAX_ACCT,                                           *00696000
                   MAX_ACCT_Q,                                         *00697000
                   MIN_PROD,                                           *00698000
                   MIN_PROD_Q,                                         *00699000
                   MAX_PROD,                                           *00700000
                   MAX_PROD_Q,                                         *00701000
                   DEMOGR_CODE,                                        *00702000
                   DEMOGR_QLFY,                                        *00703000
                   CUST_REL_CODE,                                      *00704000
                   CUST_QLFY,                                          *00705000
                   PRIM_BAL_OPT,                                       *00706000
                   PRIM_BAL_OPT_Q,                                     *00707000
                   PRIM_BAL_OPT_I,                                     *00708000
                   ACCT_REL_CD_Q,                                      *00709000
                   ACCT_REL_CD_I,                                      *00710000
                   ACCT_REL_Q,                                         *00711000
                   ACCT_REL_INCEN,                                     *00712000
                   CMB_CATG_CD_1,                                      *00713000
                   CMB_CATG_1_Q,                                       *00714000
                   CMB_CATG_1_INC,                                     *00715000
                   CMB_CATG_CD_2,                                      *00716000
                   CMB_CATG_2_Q,                                       *00717000
                   CMB_CATG_2_INC,                                     *00718000
                   PRIME_CMB_OPT_1,                                    *00719000
                   PRIME_C_1_QLFY,                                     *00720000
                   PRIME_C_1_INCEN,                                    *00721000
                   PRIME_CMB_OPT_2,                                    *00722000
                   PRIME_C_2_QLFY,                                     *00723000
                   PRIME_C_2_INCEN,                                    *00724000
                   PROD_GRP_MIN_1,                                     *00725000
                   PROD_GRP_MAX_1,                                     *00726000
                   PROD_GRP_1_Q,                                       *00727000
                   PROD_GRP_MIN_2,                                     *00728000
                   PROD_GRP_MAX_2,                                     *00729000
                   PROD_GRP_2_Q,                                       *00730000
                   PROD_GRP_MIN_3,                                     *00731000
                   PROD_GRP_MAX_3,                                     *00732000
                   PROD_GRP_3_Q,                                       *00733000
                   PROD_GRP_MIN_4,                                     *00734000
                   PROD_GRP_MAX_4,                                     *00735000
                   PROD_GRP_4_Q,                                       *00736000
                   PROD_GRP_MIN_5,                                     *00737000
                   PROD_GRP_MAX_5,                                     *00738000
                   PROD_GRP_5_Q,                                       *00739000
                   ACCT_INCEN_OVRD,                                    *00740000
                   DEFAULT_MODEL,                                      *00741000
                   INCEN_BAL_TRN,                                      *00742000
                   TRAN_PROFILE,                                       *00743000
                   TRAN_INCENTIVE,                                     *00744000
                   CASH_MAX_AMT_PD,                                    *00745000
                   CASH_MAX_BAL,                                       *00746000
                   CMB_CATG_CD_3,                                      *00747000
                   CMB_CATG_3_Q,                                       *00748000
                   CMB_CATG_3_INC,                                     *00749000
                   CMB_CATG_CD_4,                                      *00750000
                   CMB_CATG_4_Q,                                       *00751000
                   CMB_CATG_4_INC,                                     *00752000
                   PRIME_CMB_OPT_3,                                    *00753000
                   PRIME_C_3_QLFY,                                     *00754000
                   PRIME_C_3_INCEN,                                    *00755000
                   PRIME_CMB_OPT_4,                                    *00756000
                   PRIME_C_4_QLFY,                                     *00757000
                   PRIME_C_4_INCEN,                                    *00758000
                   RSV_FLAG_1,                                         *00759000
                   CLOSED_ACCT_OPT,                                    *00760000
                   SEC_SVC_OPT,                                        *00761000
                   RATE_OPTION,                                        *00762000
                   ENROLL_LEFT,                                        *00763000
                   MODEL_TYPE,                                         *00764000
                   LONG_DESC,                                          *00765000
                   PRIM_REL_CODE,                                      *00766000
                   PRIM_QLFY,                                          *00767000
                   EXTL_ENROLL                                         *00768000
                 FETCH FIRST 1 ROW ONLY                                 00769000
         EXEC  SQL                                                     *00770000
               OPEN S01UPD0                                             00771000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00772000
         BR    14                      RETURN TO CALLER                 00773000
         LTORG                                                          00774000
*                                                                       00775000
*                                                                       00776000
**********************************************************************  00777000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 1:   00778000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00779000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00780000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00781000
*     UPDATE STATEMENT TO MATCH.                                        00782000
**********************************************************************  00783000
*                                                                       00784000
SQL#0005 DS    0H                                                       00785000
         USING SQL#0005,12,7           ESTABLISH BASE REGISTER          00786000
         LA    15,255                  SET RETURN CODE                  00787000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00788000
         BR    14                      RETURN TO CALLER                 00789000
         LTORG                                                          00790000
*                                                                       00791000
*                                                                       00792000
**********************************************************************  00793000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 2:   00794000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00795000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00796000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00797000
*     UPDATE STATEMENT TO MATCH.                                        00798000
**********************************************************************  00799000
*                                                                       00800000
SQL#0006 DS    0H                                                       00801000
         USING SQL#0006,12,7           ESTABLISH BASE REGISTER          00802000
         LA    15,255                  SET RETURN CODE                  00803000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00804000
         BR    14                      RETURN TO CALLER                 00805000
         LTORG                                                          00806000
*                                                                       00807000
*                                                                       00808000
**********************************************************************  00809000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 3:   00810000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00811000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00812000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00813000
*     UPDATE STATEMENT TO MATCH.                                        00814000
**********************************************************************  00815000
*                                                                       00816000
SQL#0007 DS    0H                                                       00817000
         USING SQL#0007,12,7           ESTABLISH BASE REGISTER          00818000
         LA    15,255                  SET RETURN CODE                  00819000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00820000
         BR    14                      RETURN TO CALLER                 00821000
         LTORG                                                          00822000
*                                                                       00823000
*                                                                       00824000
**********************************************************************  00825000
* FETCH FROM UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                   00826000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00827000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00828000
*     THE ACTUAL ROW.                                                   00829000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00830000
**********************************************************************  00831000
*                                                                       00832000
SQL#0008 DS    0H                                                       00833000
         USING SQL#0008,12,7           ESTABLISH BASE REGISTER          00834000
         B     *+6                     BRANCH AROUND ADCON              00835000
BASE0008 DC    AL2(4096)                                                00836000
         LR    7,12                    LOAD SECOND BASE                 00837000
         AH    7,BASE0008              ADD 4K                           00838000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00839000
         AH    3,BASE0008              ADD 4K                           00840000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00841000
         AH    4,BASE0008              ADD 4K                           00842000
         EXEC  SQL                                                     *00843000
               FETCH S01UPD0                                           *00844000
                 INTO :AUDDATE,                                        *00845000
                   :AUDTIME,                                           *00846000
                   :AUDUSER,                                           *00847000
                   :AUDORG,                                            *00848000
                   :ACTVCDE,                                           *00849000
                   :SHRTDESC,                                          *00850000
                   :SCORE,                                             *00851000
                   :AUENROLL,                                          *00852000
                   :INCTVBAL,                                          *00853000
                   :PROCOPT,                                           *00854000
                   :CBALOPT,                                           *00855000
                   :EXPFREQ,                                           *00856000
                   :EXPTERM,                                           *00857000
                   :ENDMNTH,                                           *00858000
                   :CLSD,                                              *00859000
                   :EXPRDATE,                                          *00860000
                   :EXRETDAY,                                          *00861000
                   :FIRSTRET,                                          *00862000
                   :DAYSEXT,                                           *00863000
                   :EXTN,                                              *00864000
                   :CASHOPT,                                           *00865000
                   :CASHPCT,                                           *00866000
                   :CASHBAL,                                           *00867000
                   :CASHFREQ,                                          *00868000
                   :CASHTERM,                                          *00869000
                   :CASHDAY,                                           *00870000
                   :NBRACCT,                                           *00871000
                   :ACCTQLFY,                                          *00872000
                   :NBRPROD,                                           *00873000
                   :PRODQLFY,                                          *00874000
                   :MINACCT,                                           *00875000
                   :MINACCTQ,                                          *00876000
                   :MAXACCT,                                           *00877000
                   :MAXACCTQ,                                          *00878000
                   :MINPROD,                                           *00879000
                   :MINPRODQ,                                          *00880000
                   :MAXPROD,                                           *00881000
                   :MAXPRODQ,                                          *00882000
                   :DEMOCODE,                                          *00883000
                   :DEMOQLFY,                                          *00884000
                   :CUSTCODE,                                          *00885000
                   :CUSTQLFY,                                          *00886000
                   :PBALOPT,                                           *00887000
                   :PBALOPTQ,                                          *00888000
                   :PBALOPTI,                                          *00889000
                   :ACCTCDQ,                                           *00890000
                   :ACCTCDI,                                           *00891000
                   :ACTRELQ,                                           *00892000
                   :ACINCEN,                                           *00893000
                   :COMBCD1,                                           *00894000
                   :CCQLFY1,                                           *00895000
                   :CC1INCTV,                                          *00896000
                   :COMBCD2,                                           *00897000
                   :CCQLFY2,                                           *00898000
                   :CC2INCTV,                                          *00899000
                   :PRCOME1,                                           *00900000
                   :PC1QLFY,                                           *00901000
                   :PC1INCTV,                                          *00902000
                   :PRCOME2,                                           *00903000
                   :PC2QLFY,                                           *00904000
                   :PC2INCTV,                                          *00905000
                   :PGMIN1,                                            *00906000
                   :PGMAX1,                                            *00907000
                   :PRDGRP1Q,                                          *00908000
                   :PGMIN2,                                            *00909000
                   :PGMAX2,                                            *00910000
                   :PRDGRP2Q,                                          *00911000
                   :PGMIN3,                                            *00912000
                   :PGMAX3,                                            *00913000
                   :PRDGRP3Q,                                          *00914000
                   :PGMIN4,                                            *00915000
                   :PGMAX4,                                            *00916000
                   :PRDGRP4Q,                                          *00917000
                   :PGMIN5,                                            *00918000
                   :PGMAX5,                                            *00919000
                   :PRDGRP5Q,                                          *00920000
                   :INCENTOV,                                          *00921000
                   :DFLTMODL,                                          *00922000
                   :INCBALTR,                                          *00923000
                   :TRANPROF,                                          *00924000
                   :TRPINCEN,                                          *00925000
                   :CRWAMT,                                            *00926000
                   :CRWMAXCB,                                          *00927000
                   :COMBCD3,                                           *00928000
                   :CCQLFY3,                                           *00929000
                   :CC3INCTV,                                          *00930000
                   :COMBCD4,                                           *00931000
                   :CCQLFY4,                                           *00932000
                   :CC4INCTV,                                          *00933000
                   :PRCOME3,                                           *00934000
                   :PC3QLFY,                                           *00935000
                   :PC3INCTV,                                          *00936000
                   :PRCOME4,                                           *00937000
                   :PC4QLFY,                                           *00938000
                   :PC4INCTV,                                          *00939000
                   :RSVFLAG1,                                          *00940000
                   :CLACCOPT,                                          *00941000
                   :SECSVOPT,                                          *00942000
                   :RATEOPT,                                           *00943000
                   :ENRATTP,                                           *00944000
                   :MODELTYP,                                          *00945000
                   :LONGDESC,                                          *00946000
                   :PRIMCODE,                                          *00947000
                   :PRIMQLFY,                                          *00948000
                   :EXTENROL                                            00949000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   00950000
         BALR  4,5                     MOVE INTO RECORD AREA            00951000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00952000
         BR    14                      RETURN TO CALLER                 00953000
         LTORG                                                          00954000
*                                                                       00955000
*                                                                       00956000
**********************************************************************  00957000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 1:               00958000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00959000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00960000
*     THE ACTUAL ROW.                                                   00961000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00962000
**********************************************************************  00963000
*                                                                       00964000
SQL#0009 DS    0H                                                       00965000
         USING SQL#0009,12,7           ESTABLISH BASE REGISTER          00966000
         LA    15,255                  SET RETURN CODE                  00967000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00968000
         BR    14                      RETURN TO CALLER                 00969000
         LTORG                                                          00970000
*                                                                       00971000
*                                                                       00972000
**********************************************************************  00973000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 2:               00974000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00975000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00976000
*     THE ACTUAL ROW.                                                   00977000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00978000
**********************************************************************  00979000
*                                                                       00980000
SQL#0010 DS    0H                                                       00981000
         USING SQL#0010,12,7           ESTABLISH BASE REGISTER          00982000
         LA    15,255                  SET RETURN CODE                  00983000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00984000
         BR    14                      RETURN TO CALLER                 00985000
         LTORG                                                          00986000
*                                                                       00987000
*                                                                       00988000
**********************************************************************  00989000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 3:               00990000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00991000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00992000
*     THE ACTUAL ROW.                                                   00993000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00994000
**********************************************************************  00995000
*                                                                       00996000
SQL#0011 DS    0H                                                       00997000
         USING SQL#0011,12,7           ESTABLISH BASE REGISTER          00998000
         LA    15,255                  SET RETURN CODE                  00999000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01000000
         BR    14                      RETURN TO CALLER                 01001000
         LTORG                                                          01002000
*                                                                       01003000
*                                                                       01004000
**********************************************************************  01005000
* CLOSE UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                        01006000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          01007000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          01008000
*     TO CLOSE THE UPDATE CURSOR.                                       01009000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01010000
**********************************************************************  01011000
*                                                                       01012000
SQL#0012 DS    0H                                                       01013000
         USING SQL#0012,12,7           ESTABLISH BASE REGISTER          01014000
         B     *+6                     BRANCH AROUND ADCON              01015000
BASE0012 DC    AL2(4096)                                                01016000
         LR    7,12                    LOAD SECOND BASE                 01017000
         AH    7,BASE0012              ADD 4K                           01018000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01019000
         AH    3,BASE0012              ADD 4K                           01020000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01021000
         AH    4,BASE0012              ADD 4K                           01022000
         EXEC  SQL                                                     *01023000
               CLOSE S01UPD0                                            01024000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01025000
         BR    14                      RETURN TO CALLER                 01026000
         LTORG                                                          01027000
*                                                                       01028000
*                                                                       01029000
**********************************************************************  01030000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 1:                    01031000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          01032000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          01033000
*     TO CLOSE THE UPDATE CURSOR.                                       01034000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01035000
**********************************************************************  01036000
*                                                                       01037000
SQL#0013 DS    0H                                                       01038000
         USING SQL#0013,12,7           ESTABLISH BASE REGISTER          01039000
         LA    15,255                  SET RETURN CODE                  01040000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01041000
         BR    14                      RETURN TO CALLER                 01042000
         LTORG                                                          01043000
*                                                                       01044000
*                                                                       01045000
**********************************************************************  01046000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 2:                    01047000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          01048000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          01049000
*     TO CLOSE THE UPDATE CURSOR.                                       01050000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01051000
**********************************************************************  01052000
*                                                                       01053000
SQL#0014 DS    0H                                                       01054000
         USING SQL#0014,12,7           ESTABLISH BASE REGISTER          01055000
         LA    15,255                  SET RETURN CODE                  01056000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01057000
         BR    14                      RETURN TO CALLER                 01058000
         LTORG                                                          01059000
*                                                                       01060000
*                                                                       01061000
**********************************************************************  01062000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 3:                    01063000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          01064000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          01065000
*     TO CLOSE THE UPDATE CURSOR.                                       01066000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01067000
**********************************************************************  01068000
*                                                                       01069000
SQL#0015 DS    0H                                                       01070000
         USING SQL#0015,12,7           ESTABLISH BASE REGISTER          01071000
         LA    15,255                  SET RETURN CODE                  01072000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01073000
         BR    14                      RETURN TO CALLER                 01074000
         LTORG                                                          01075000
*                                                                       01076000
*                                                                       01077000
**********************************************************************  01078000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY    01079000
* KEY:                                                                  01080000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         01081000
*     AND GET-NEXT-LOCK VERBS.                                          01082000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              01083000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                01084000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      01085000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     01086000
*     ORDER BY CLAUSE.                                                  01087000
**********************************************************************  01088000
*                                                                       01089000
SQL#0016 DS    0H                                                       01090000
         USING SQL#0016,12,7           ESTABLISH BASE REGISTER          01091000
         B     *+6                     BRANCH AROUND ADCON              01092000
BASE0016 DC    AL2(4096)                                                01093000
         LR    7,12                    LOAD SECOND BASE                 01094000
         AH    7,BASE0016              ADD 4K                           01095000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              01096000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      01097000
         BALR  4,5                     MOVE INTO HOST VARIABLES         01098000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01099000
         AH    3,BASE0016              ADD 4K                           01100000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01101000
         AH    4,BASE0016              ADD 4K                           01102000
         EXEC  SQL                                                     *01103000
               DECLARE S01SQ001 CURSOR FOR                             *01104000
               SELECT INST_NBR,                                        *01105000
                   RECORD_NBR,                                         *01106000
                   MODEL,                                              *01107000
                   EFFECTIVE_DATE,                                     *01108000
                   AUDIT_DATE,                                         *01109000
                   AUDIT_TIME,                                         *01110000
                   AUDIT_USER,                                         *01111000
                   AUDIT_ORG,                                          *01112000
                   ACTIVE_CODE,                                        *01113000
                   SHORT_DESC,                                         *01114000
                   SCORE,                                              *01115000
                   AUTO_ENROLL,                                        *01116000
                   INCEN_BAL_OPT,                                      *01117000
                   PROCESS_OPTION,                                     *01118000
                   CMB_BAL_OPT,                                        *01119000
                   EXPIRE_FREQ,                                        *01120000
                   EXPIRE_TERM,                                        *01121000
                   END_MONTH,                                          *01122000
                   CLOSE_DATE,                                         *01123000
                   EXPIRE_DATE,                                        *01124000
                   EXPIRE_RET_DAYS,                                    *01125000
                   FIRST_RETENTION,                                    *01126000
                   DAY_PER_EXTN,                                       *01127000
                   EXTENSION,                                          *01128000
                   CASH_OPT,                                           *01129000
                   CASH_PCT,                                           *01130000
                   CASH_BAL_OPT,                                       *01131000
                   CASH_FREQ,                                          *01132000
                   CASH_TERM,                                          *01133000
                   CASH_DAY,                                           *01134000
                   NBR_ACCOUNTS,                                       *01135000
                   ACCT_QLFY,                                          *01136000
                   NBR_PROD,                                           *01137000
                   PROD_QLFY,                                          *01138000
                   MIN_ACCT,                                           *01139000
                   MIN_ACCT_Q,                                         *01140000
                   MAX_ACCT,                                           *01141000
                   MAX_ACCT_Q,                                         *01142000
                   MIN_PROD,                                           *01143000
                   MIN_PROD_Q,                                         *01144000
                   MAX_PROD,                                           *01145000
                   MAX_PROD_Q,                                         *01146000
                   DEMOGR_CODE,                                        *01147000
                   DEMOGR_QLFY,                                        *01148000
                   CUST_REL_CODE,                                      *01149000
                   CUST_QLFY,                                          *01150000
                   PRIM_BAL_OPT,                                       *01151000
                   PRIM_BAL_OPT_Q,                                     *01152000
                   PRIM_BAL_OPT_I,                                     *01153000
                   ACCT_REL_CD_Q,                                      *01154000
                   ACCT_REL_CD_I,                                      *01155000
                   ACCT_REL_Q,                                         *01156000
                   ACCT_REL_INCEN,                                     *01157000
                   CMB_CATG_CD_1,                                      *01158000
                   CMB_CATG_1_Q,                                       *01159000
                   CMB_CATG_1_INC,                                     *01160000
                   CMB_CATG_CD_2,                                      *01161000
                   CMB_CATG_2_Q,                                       *01162000
                   CMB_CATG_2_INC,                                     *01163000
                   PRIME_CMB_OPT_1,                                    *01164000
                   PRIME_C_1_QLFY,                                     *01165000
                   PRIME_C_1_INCEN,                                    *01166000
                   PRIME_CMB_OPT_2,                                    *01167000
                   PRIME_C_2_QLFY,                                     *01168000
                   PRIME_C_2_INCEN,                                    *01169000
                   PROD_GRP_MIN_1,                                     *01170000
                   PROD_GRP_MAX_1,                                     *01171000
                   PROD_GRP_1_Q,                                       *01172000
                   PROD_GRP_MIN_2,                                     *01173000
                   PROD_GRP_MAX_2,                                     *01174000
                   PROD_GRP_2_Q,                                       *01175000
                   PROD_GRP_MIN_3,                                     *01176000
                   PROD_GRP_MAX_3,                                     *01177000
                   PROD_GRP_3_Q,                                       *01178000
                   PROD_GRP_MIN_4,                                     *01179000
                   PROD_GRP_MAX_4,                                     *01180000
                   PROD_GRP_4_Q,                                       *01181000
                   PROD_GRP_MIN_5,                                     *01182000
                   PROD_GRP_MAX_5,                                     *01183000
                   PROD_GRP_5_Q,                                       *01184000
                   ACCT_INCEN_OVRD,                                    *01185000
                   DEFAULT_MODEL,                                      *01186000
                   INCEN_BAL_TRN,                                      *01187000
                   TRAN_PROFILE,                                       *01188000
                   TRAN_INCENTIVE,                                     *01189000
                   CASH_MAX_AMT_PD,                                    *01190000
                   CASH_MAX_BAL,                                       *01191000
                   CMB_CATG_CD_3,                                      *01192000
                   CMB_CATG_3_Q,                                       *01193000
                   CMB_CATG_3_INC,                                     *01194000
                   CMB_CATG_CD_4,                                      *01195000
                   CMB_CATG_4_Q,                                       *01196000
                   CMB_CATG_4_INC,                                     *01197000
                   PRIME_CMB_OPT_3,                                    *01198000
                   PRIME_C_3_QLFY,                                     *01199000
                   PRIME_C_3_INCEN,                                    *01200000
                   PRIME_CMB_OPT_4,                                    *01201000
                   PRIME_C_4_QLFY,                                     *01202000
                   PRIME_C_4_INCEN,                                    *01203000
                   RSV_FLAG_1,                                         *01204000
                   CLOSED_ACCT_OPT,                                    *01205000
                   SEC_SVC_OPT,                                        *01206000
                   RATE_OPTION,                                        *01207000
                   ENROLL_LEFT,                                        *01208000
                   MODEL_TYPE,                                         *01209000
                   LONG_DESC,                                          *01210000
                   PRIM_REL_CODE,                                      *01211000
                   PRIM_QLFY,                                          *01212000
                   EXTL_ENROLL                                         *01213000
                 FROM S01                                              *01214000
                 WHERE                                                 *01215000
                    INST_NBR = :INST AND                               *01216000
                    RECORD_NBR = :RECNBR AND                           *01217000
                    MODEL = :MODEL AND                                 *01218000
                    EFFECTIVE_DATE >= :EFFDATE                         *01219000
                 ORDER BY EFFECTIVE_DATE                               *01220000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01221000
         EXEC  SQL                                                     *01222000
               OPEN S01SQ001                                            01223000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01224000
         BR    14                      RETURN TO CALLER                 01225000
         LTORG                                                          01226000
*                                                                       01227000
*                                                                       01228000
**********************************************************************  01229000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  01230000
* KEY 1:                                                                01231000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         01232000
*     AND GET-NEXT-LOCK VERBS.                                          01233000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              01234000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                01235000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      01236000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     01237000
*     ORDER BY CLAUSE.                                                  01238000
**********************************************************************  01239000
*                                                                       01240000
SQL#0017 DS    0H                                                       01241000
         USING SQL#0017,12,7           ESTABLISH BASE REGISTER          01242000
         LA    15,255                  SET RETURN CODE                  01243000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01244000
         BR    14                      RETURN TO CALLER                 01245000
         LTORG                                                          01246000
*                                                                       01247000
*                                                                       01248000
**********************************************************************  01249000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  01250000
* KEY 2:                                                                01251000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         01252000
*     AND GET-NEXT-LOCK VERBS.                                          01253000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              01254000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                01255000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      01256000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     01257000
*     ORDER BY CLAUSE.                                                  01258000
**********************************************************************  01259000
*                                                                       01260000
SQL#0018 DS    0H                                                       01261000
         USING SQL#0018,12,7           ESTABLISH BASE REGISTER          01262000
         LA    15,255                  SET RETURN CODE                  01263000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01264000
         BR    14                      RETURN TO CALLER                 01265000
         LTORG                                                          01266000
*                                                                       01267000
*                                                                       01268000
**********************************************************************  01269000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  01270000
* KEY 3:                                                                01271000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         01272000
*     AND GET-NEXT-LOCK VERBS.                                          01273000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              01274000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                01275000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      01276000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     01277000
*     ORDER BY CLAUSE.                                                  01278000
**********************************************************************  01279000
*                                                                       01280000
SQL#0019 DS    0H                                                       01281000
         USING SQL#0019,12,7           ESTABLISH BASE REGISTER          01282000
         LA    15,255                  SET RETURN CODE                  01283000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01284000
         BR    14                      RETURN TO CALLER                 01285000
         LTORG                                                          01286000
*                                                                       01287000
*                                                                       01288000
**********************************************************************  01289000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:               01290000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01291000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01292000
*     RETRIEVE THE ACTUAL ROW.                                          01293000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01294000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01295000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01296000
**********************************************************************  01297000
*                                                                       01298000
SQL#0020 DS    0H                                                       01299000
         USING SQL#0020,12,7           ESTABLISH BASE REGISTER          01300000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01301000
         LA    12,VECT0020(1)          LOAD POINTER TO FETCH ROUTINE    01302000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01303000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01304000
VECT0020 DC    A(SQL#0120)                                              01305000
         DC    A(SQL#0220)                                              01306000
         DC    A(SQL#0320)                                              01307000
         DC    A(SQL#0420)                                              01308000
         LTORG                                                          01309000
*                                                                       01310000
SQL#0120 DS    0H                                                       01311000
         USING SQL#0120,12,7           ESTABLISH BASE REGISTER          01312000
         B     *+6                     BRANCH AROUND ADCON              01313000
BASE0120 DC    AL2(4096)                                                01314000
         LR    7,12                    LOAD SECOND BASE                 01315000
         AH    7,BASE0120              ADD 4K                           01316000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01317000
         AH    3,BASE0120              ADD 4K                           01318000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01319000
         AH    4,BASE0120              ADD 4K                           01320000
         EXEC  SQL                                                     *01321000
               FETCH S01SQ001                                          *01322000
                 INTO :INST,                                           *01323000
                      :RECNBR,                                         *01324000
                      :MODEL,                                          *01325000
                      :EFFDATE,                                        *01326000
                      :AUDDATE,                                        *01327000
                      :AUDTIME,                                        *01328000
                      :AUDUSER,                                        *01329000
                      :AUDORG,                                         *01330000
                      :ACTVCDE,                                        *01331000
                      :SHRTDESC,                                       *01332000
                      :SCORE,                                          *01333000
                      :AUENROLL,                                       *01334000
                      :INCTVBAL,                                       *01335000
                      :PROCOPT,                                        *01336000
                      :CBALOPT,                                        *01337000
                      :EXPFREQ,                                        *01338000
                      :EXPTERM,                                        *01339000
                      :ENDMNTH,                                        *01340000
                      :CLSD,                                           *01341000
                      :EXPRDATE,                                       *01342000
                      :EXRETDAY,                                       *01343000
                      :FIRSTRET,                                       *01344000
                      :DAYSEXT,                                        *01345000
                      :EXTN,                                           *01346000
                      :CASHOPT,                                        *01347000
                      :CASHPCT,                                        *01348000
                      :CASHBAL,                                        *01349000
                      :CASHFREQ,                                       *01350000
                      :CASHTERM,                                       *01351000
                      :CASHDAY,                                        *01352000
                      :NBRACCT,                                        *01353000
                      :ACCTQLFY,                                       *01354000
                      :NBRPROD,                                        *01355000
                      :PRODQLFY,                                       *01356000
                      :MINACCT,                                        *01357000
                      :MINACCTQ,                                       *01358000
                      :MAXACCT,                                        *01359000
                      :MAXACCTQ,                                       *01360000
                      :MINPROD,                                        *01361000
                      :MINPRODQ,                                       *01362000
                      :MAXPROD,                                        *01363000
                      :MAXPRODQ,                                       *01364000
                      :DEMOCODE,                                       *01365000
                      :DEMOQLFY,                                       *01366000
                      :CUSTCODE,                                       *01367000
                      :CUSTQLFY,                                       *01368000
                      :PBALOPT,                                        *01369000
                      :PBALOPTQ,                                       *01370000
                      :PBALOPTI,                                       *01371000
                      :ACCTCDQ,                                        *01372000
                      :ACCTCDI,                                        *01373000
                      :ACTRELQ,                                        *01374000
                      :ACINCEN,                                        *01375000
                      :COMBCD1,                                        *01376000
                      :CCQLFY1,                                        *01377000
                      :CC1INCTV,                                       *01378000
                      :COMBCD2,                                        *01379000
                      :CCQLFY2,                                        *01380000
                      :CC2INCTV,                                       *01381000
                      :PRCOME1,                                        *01382000
                      :PC1QLFY,                                        *01383000
                      :PC1INCTV,                                       *01384000
                      :PRCOME2,                                        *01385000
                      :PC2QLFY,                                        *01386000
                      :PC2INCTV,                                       *01387000
                      :PGMIN1,                                         *01388000
                      :PGMAX1,                                         *01389000
                      :PRDGRP1Q,                                       *01390000
                      :PGMIN2,                                         *01391000
                      :PGMAX2,                                         *01392000
                      :PRDGRP2Q,                                       *01393000
                      :PGMIN3,                                         *01394000
                      :PGMAX3,                                         *01395000
                      :PRDGRP3Q,                                       *01396000
                      :PGMIN4,                                         *01397000
                      :PGMAX4,                                         *01398000
                      :PRDGRP4Q,                                       *01399000
                      :PGMIN5,                                         *01400000
                      :PGMAX5,                                         *01401000
                      :PRDGRP5Q,                                       *01402000
                      :INCENTOV,                                       *01403000
                      :DFLTMODL,                                       *01404000
                      :INCBALTR,                                       *01405000
                      :TRANPROF,                                       *01406000
                      :TRPINCEN,                                       *01407000
                      :CRWAMT,                                         *01408000
                      :CRWMAXCB,                                       *01409000
                      :COMBCD3,                                        *01410000
                      :CCQLFY3,                                        *01411000
                      :CC3INCTV,                                       *01412000
                      :COMBCD4,                                        *01413000
                      :CCQLFY4,                                        *01414000
                      :CC4INCTV,                                       *01415000
                      :PRCOME3,                                        *01416000
                      :PC3QLFY,                                        *01417000
                      :PC3INCTV,                                       *01418000
                      :PRCOME4,                                        *01419000
                      :PC4QLFY,                                        *01420000
                      :PC4INCTV,                                       *01421000
                      :RSVFLAG1,                                       *01422000
                      :CLACCOPT,                                       *01423000
                      :SECSVOPT,                                       *01424000
                      :RATEOPT,                                        *01425000
                      :ENRATTP,                                        *01426000
                      :MODELTYP,                                       *01427000
                      :LONGDESC,                                       *01428000
                      :PRIMCODE,                                       *01429000
                      :PRIMQLFY,                                       *01430000
                      :EXTENROL                                         01431000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01432000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01433000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01434000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01435000
         BR    12                      OPEN NEXT CURSOR                 01436000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01437000
         BALR  4,5                     MOVE INTO RECORD AREA            01438000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01439000
         BR    14                      RETURN TO CALLER                 01440000
         LTORG                                                          01441000
*                                                                       01442000
SQL#0220 DS    0H                                                       01443000
         USING SQL#0220,12,7           ESTABLISH BASE REGISTER          01444000
         B     *+6                     BRANCH AROUND ADCON              01445000
BASE0220 DC    AL2(4096)                                                01446000
         LR    7,12                    LOAD SECOND BASE                 01447000
         AH    7,BASE0220              ADD 4K                           01448000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01449000
         AH    3,BASE0220              ADD 4K                           01450000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01451000
         AH    4,BASE0220              ADD 4K                           01452000
         EXEC  SQL                                                     *01453000
               FETCH S01SQ002                                          *01454000
                 INTO :INST,                                           *01455000
                      :RECNBR,                                         *01456000
                      :MODEL,                                          *01457000
                      :EFFDATE,                                        *01458000
                      :AUDDATE,                                        *01459000
                      :AUDTIME,                                        *01460000
                      :AUDUSER,                                        *01461000
                      :AUDORG,                                         *01462000
                      :ACTVCDE,                                        *01463000
                      :SHRTDESC,                                       *01464000
                      :SCORE,                                          *01465000
                      :AUENROLL,                                       *01466000
                      :INCTVBAL,                                       *01467000
                      :PROCOPT,                                        *01468000
                      :CBALOPT,                                        *01469000
                      :EXPFREQ,                                        *01470000
                      :EXPTERM,                                        *01471000
                      :ENDMNTH,                                        *01472000
                      :CLSD,                                           *01473000
                      :EXPRDATE,                                       *01474000
                      :EXRETDAY,                                       *01475000
                      :FIRSTRET,                                       *01476000
                      :DAYSEXT,                                        *01477000
                      :EXTN,                                           *01478000
                      :CASHOPT,                                        *01479000
                      :CASHPCT,                                        *01480000
                      :CASHBAL,                                        *01481000
                      :CASHFREQ,                                       *01482000
                      :CASHTERM,                                       *01483000
                      :CASHDAY,                                        *01484000
                      :NBRACCT,                                        *01485000
                      :ACCTQLFY,                                       *01486000
                      :NBRPROD,                                        *01487000
                      :PRODQLFY,                                       *01488000
                      :MINACCT,                                        *01489000
                      :MINACCTQ,                                       *01490000
                      :MAXACCT,                                        *01491000
                      :MAXACCTQ,                                       *01492000
                      :MINPROD,                                        *01493000
                      :MINPRODQ,                                       *01494000
                      :MAXPROD,                                        *01495000
                      :MAXPRODQ,                                       *01496000
                      :DEMOCODE,                                       *01497000
                      :DEMOQLFY,                                       *01498000
                      :CUSTCODE,                                       *01499000
                      :CUSTQLFY,                                       *01500000
                      :PBALOPT,                                        *01501000
                      :PBALOPTQ,                                       *01502000
                      :PBALOPTI,                                       *01503000
                      :ACCTCDQ,                                        *01504000
                      :ACCTCDI,                                        *01505000
                      :ACTRELQ,                                        *01506000
                      :ACINCEN,                                        *01507000
                      :COMBCD1,                                        *01508000
                      :CCQLFY1,                                        *01509000
                      :CC1INCTV,                                       *01510000
                      :COMBCD2,                                        *01511000
                      :CCQLFY2,                                        *01512000
                      :CC2INCTV,                                       *01513000
                      :PRCOME1,                                        *01514000
                      :PC1QLFY,                                        *01515000
                      :PC1INCTV,                                       *01516000
                      :PRCOME2,                                        *01517000
                      :PC2QLFY,                                        *01518000
                      :PC2INCTV,                                       *01519000
                      :PGMIN1,                                         *01520000
                      :PGMAX1,                                         *01521000
                      :PRDGRP1Q,                                       *01522000
                      :PGMIN2,                                         *01523000
                      :PGMAX2,                                         *01524000
                      :PRDGRP2Q,                                       *01525000
                      :PGMIN3,                                         *01526000
                      :PGMAX3,                                         *01527000
                      :PRDGRP3Q,                                       *01528000
                      :PGMIN4,                                         *01529000
                      :PGMAX4,                                         *01530000
                      :PRDGRP4Q,                                       *01531000
                      :PGMIN5,                                         *01532000
                      :PGMAX5,                                         *01533000
                      :PRDGRP5Q,                                       *01534000
                      :INCENTOV,                                       *01535000
                      :DFLTMODL,                                       *01536000
                      :INCBALTR,                                       *01537000
                      :TRANPROF,                                       *01538000
                      :TRPINCEN,                                       *01539000
                      :CRWAMT,                                         *01540000
                      :CRWMAXCB,                                       *01541000
                      :COMBCD3,                                        *01542000
                      :CCQLFY3,                                        *01543000
                      :CC3INCTV,                                       *01544000
                      :COMBCD4,                                        *01545000
                      :CCQLFY4,                                        *01546000
                      :CC4INCTV,                                       *01547000
                      :PRCOME3,                                        *01548000
                      :PC3QLFY,                                        *01549000
                      :PC3INCTV,                                       *01550000
                      :PRCOME4,                                        *01551000
                      :PC4QLFY,                                        *01552000
                      :PC4INCTV,                                       *01553000
                      :RSVFLAG1,                                       *01554000
                      :CLACCOPT,                                       *01555000
                      :SECSVOPT,                                       *01556000
                      :RATEOPT,                                        *01557000
                      :ENRATTP,                                        *01558000
                      :MODELTYP,                                       *01559000
                      :LONGDESC,                                       *01560000
                      :PRIMCODE,                                       *01561000
                      :PRIMQLFY,                                       *01562000
                      :EXTENROL                                         01563000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01564000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01565000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01566000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01567000
         BR    12                      OPEN NEXT CURSOR                 01568000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01569000
         BALR  4,5                     MOVE INTO RECORD AREA            01570000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01571000
         BR    14                      RETURN TO CALLER                 01572000
         LTORG                                                          01573000
*                                                                       01574000
SQL#0320 DS    0H                                                       01575000
         USING SQL#0320,12,7           ESTABLISH BASE REGISTER          01576000
         B     *+6                     BRANCH AROUND ADCON              01577000
BASE0320 DC    AL2(4096)                                                01578000
         LR    7,12                    LOAD SECOND BASE                 01579000
         AH    7,BASE0320              ADD 4K                           01580000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01581000
         AH    3,BASE0320              ADD 4K                           01582000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01583000
         AH    4,BASE0320              ADD 4K                           01584000
         EXEC  SQL                                                     *01585000
               FETCH S01SQ003                                          *01586000
                 INTO :INST,                                           *01587000
                      :RECNBR,                                         *01588000
                      :MODEL,                                          *01589000
                      :EFFDATE,                                        *01590000
                      :AUDDATE,                                        *01591000
                      :AUDTIME,                                        *01592000
                      :AUDUSER,                                        *01593000
                      :AUDORG,                                         *01594000
                      :ACTVCDE,                                        *01595000
                      :SHRTDESC,                                       *01596000
                      :SCORE,                                          *01597000
                      :AUENROLL,                                       *01598000
                      :INCTVBAL,                                       *01599000
                      :PROCOPT,                                        *01600000
                      :CBALOPT,                                        *01601000
                      :EXPFREQ,                                        *01602000
                      :EXPTERM,                                        *01603000
                      :ENDMNTH,                                        *01604000
                      :CLSD,                                           *01605000
                      :EXPRDATE,                                       *01606000
                      :EXRETDAY,                                       *01607000
                      :FIRSTRET,                                       *01608000
                      :DAYSEXT,                                        *01609000
                      :EXTN,                                           *01610000
                      :CASHOPT,                                        *01611000
                      :CASHPCT,                                        *01612000
                      :CASHBAL,                                        *01613000
                      :CASHFREQ,                                       *01614000
                      :CASHTERM,                                       *01615000
                      :CASHDAY,                                        *01616000
                      :NBRACCT,                                        *01617000
                      :ACCTQLFY,                                       *01618000
                      :NBRPROD,                                        *01619000
                      :PRODQLFY,                                       *01620000
                      :MINACCT,                                        *01621000
                      :MINACCTQ,                                       *01622000
                      :MAXACCT,                                        *01623000
                      :MAXACCTQ,                                       *01624000
                      :MINPROD,                                        *01625000
                      :MINPRODQ,                                       *01626000
                      :MAXPROD,                                        *01627000
                      :MAXPRODQ,                                       *01628000
                      :DEMOCODE,                                       *01629000
                      :DEMOQLFY,                                       *01630000
                      :CUSTCODE,                                       *01631000
                      :CUSTQLFY,                                       *01632000
                      :PBALOPT,                                        *01633000
                      :PBALOPTQ,                                       *01634000
                      :PBALOPTI,                                       *01635000
                      :ACCTCDQ,                                        *01636000
                      :ACCTCDI,                                        *01637000
                      :ACTRELQ,                                        *01638000
                      :ACINCEN,                                        *01639000
                      :COMBCD1,                                        *01640000
                      :CCQLFY1,                                        *01641000
                      :CC1INCTV,                                       *01642000
                      :COMBCD2,                                        *01643000
                      :CCQLFY2,                                        *01644000
                      :CC2INCTV,                                       *01645000
                      :PRCOME1,                                        *01646000
                      :PC1QLFY,                                        *01647000
                      :PC1INCTV,                                       *01648000
                      :PRCOME2,                                        *01649000
                      :PC2QLFY,                                        *01650000
                      :PC2INCTV,                                       *01651000
                      :PGMIN1,                                         *01652000
                      :PGMAX1,                                         *01653000
                      :PRDGRP1Q,                                       *01654000
                      :PGMIN2,                                         *01655000
                      :PGMAX2,                                         *01656000
                      :PRDGRP2Q,                                       *01657000
                      :PGMIN3,                                         *01658000
                      :PGMAX3,                                         *01659000
                      :PRDGRP3Q,                                       *01660000
                      :PGMIN4,                                         *01661000
                      :PGMAX4,                                         *01662000
                      :PRDGRP4Q,                                       *01663000
                      :PGMIN5,                                         *01664000
                      :PGMAX5,                                         *01665000
                      :PRDGRP5Q,                                       *01666000
                      :INCENTOV,                                       *01667000
                      :DFLTMODL,                                       *01668000
                      :INCBALTR,                                       *01669000
                      :TRANPROF,                                       *01670000
                      :TRPINCEN,                                       *01671000
                      :CRWAMT,                                         *01672000
                      :CRWMAXCB,                                       *01673000
                      :COMBCD3,                                        *01674000
                      :CCQLFY3,                                        *01675000
                      :CC3INCTV,                                       *01676000
                      :COMBCD4,                                        *01677000
                      :CCQLFY4,                                        *01678000
                      :CC4INCTV,                                       *01679000
                      :PRCOME3,                                        *01680000
                      :PC3QLFY,                                        *01681000
                      :PC3INCTV,                                       *01682000
                      :PRCOME4,                                        *01683000
                      :PC4QLFY,                                        *01684000
                      :PC4INCTV,                                       *01685000
                      :RSVFLAG1,                                       *01686000
                      :CLACCOPT,                                       *01687000
                      :SECSVOPT,                                       *01688000
                      :RATEOPT,                                        *01689000
                      :ENRATTP,                                        *01690000
                      :MODELTYP,                                       *01691000
                      :LONGDESC,                                       *01692000
                      :PRIMCODE,                                       *01693000
                      :PRIMQLFY,                                       *01694000
                      :EXTENROL                                         01695000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01696000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01697000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01698000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01699000
         BR    12                      OPEN NEXT CURSOR                 01700000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01701000
         BALR  4,5                     MOVE INTO RECORD AREA            01702000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01703000
         BR    14                      RETURN TO CALLER                 01704000
         LTORG                                                          01705000
*                                                                       01706000
SQL#0420 DS    0H                                                       01707000
         USING SQL#0420,12,7           ESTABLISH BASE REGISTER          01708000
         B     *+6                     BRANCH AROUND ADCON              01709000
BASE0420 DC    AL2(4096)                                                01710000
         LR    7,12                    LOAD SECOND BASE                 01711000
         AH    7,BASE0420              ADD 4K                           01712000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01713000
         AH    3,BASE0420              ADD 4K                           01714000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01715000
         AH    4,BASE0420              ADD 4K                           01716000
         EXEC  SQL                                                     *01717000
               FETCH S01SQ004                                          *01718000
                 INTO :INST,                                           *01719000
                      :RECNBR,                                         *01720000
                      :MODEL,                                          *01721000
                      :EFFDATE,                                        *01722000
                      :AUDDATE,                                        *01723000
                      :AUDTIME,                                        *01724000
                      :AUDUSER,                                        *01725000
                      :AUDORG,                                         *01726000
                      :ACTVCDE,                                        *01727000
                      :SHRTDESC,                                       *01728000
                      :SCORE,                                          *01729000
                      :AUENROLL,                                       *01730000
                      :INCTVBAL,                                       *01731000
                      :PROCOPT,                                        *01732000
                      :CBALOPT,                                        *01733000
                      :EXPFREQ,                                        *01734000
                      :EXPTERM,                                        *01735000
                      :ENDMNTH,                                        *01736000
                      :CLSD,                                           *01737000
                      :EXPRDATE,                                       *01738000
                      :EXRETDAY,                                       *01739000
                      :FIRSTRET,                                       *01740000
                      :DAYSEXT,                                        *01741000
                      :EXTN,                                           *01742000
                      :CASHOPT,                                        *01743000
                      :CASHPCT,                                        *01744000
                      :CASHBAL,                                        *01745000
                      :CASHFREQ,                                       *01746000
                      :CASHTERM,                                       *01747000
                      :CASHDAY,                                        *01748000
                      :NBRACCT,                                        *01749000
                      :ACCTQLFY,                                       *01750000
                      :NBRPROD,                                        *01751000
                      :PRODQLFY,                                       *01752000
                      :MINACCT,                                        *01753000
                      :MINACCTQ,                                       *01754000
                      :MAXACCT,                                        *01755000
                      :MAXACCTQ,                                       *01756000
                      :MINPROD,                                        *01757000
                      :MINPRODQ,                                       *01758000
                      :MAXPROD,                                        *01759000
                      :MAXPRODQ,                                       *01760000
                      :DEMOCODE,                                       *01761000
                      :DEMOQLFY,                                       *01762000
                      :CUSTCODE,                                       *01763000
                      :CUSTQLFY,                                       *01764000
                      :PBALOPT,                                        *01765000
                      :PBALOPTQ,                                       *01766000
                      :PBALOPTI,                                       *01767000
                      :ACCTCDQ,                                        *01768000
                      :ACCTCDI,                                        *01769000
                      :ACTRELQ,                                        *01770000
                      :ACINCEN,                                        *01771000
                      :COMBCD1,                                        *01772000
                      :CCQLFY1,                                        *01773000
                      :CC1INCTV,                                       *01774000
                      :COMBCD2,                                        *01775000
                      :CCQLFY2,                                        *01776000
                      :CC2INCTV,                                       *01777000
                      :PRCOME1,                                        *01778000
                      :PC1QLFY,                                        *01779000
                      :PC1INCTV,                                       *01780000
                      :PRCOME2,                                        *01781000
                      :PC2QLFY,                                        *01782000
                      :PC2INCTV,                                       *01783000
                      :PGMIN1,                                         *01784000
                      :PGMAX1,                                         *01785000
                      :PRDGRP1Q,                                       *01786000
                      :PGMIN2,                                         *01787000
                      :PGMAX2,                                         *01788000
                      :PRDGRP2Q,                                       *01789000
                      :PGMIN3,                                         *01790000
                      :PGMAX3,                                         *01791000
                      :PRDGRP3Q,                                       *01792000
                      :PGMIN4,                                         *01793000
                      :PGMAX4,                                         *01794000
                      :PRDGRP4Q,                                       *01795000
                      :PGMIN5,                                         *01796000
                      :PGMAX5,                                         *01797000
                      :PRDGRP5Q,                                       *01798000
                      :INCENTOV,                                       *01799000
                      :DFLTMODL,                                       *01800000
                      :INCBALTR,                                       *01801000
                      :TRANPROF,                                       *01802000
                      :TRPINCEN,                                       *01803000
                      :CRWAMT,                                         *01804000
                      :CRWMAXCB,                                       *01805000
                      :COMBCD3,                                        *01806000
                      :CCQLFY3,                                        *01807000
                      :CC3INCTV,                                       *01808000
                      :COMBCD4,                                        *01809000
                      :CCQLFY4,                                        *01810000
                      :CC4INCTV,                                       *01811000
                      :PRCOME3,                                        *01812000
                      :PC3QLFY,                                        *01813000
                      :PC3INCTV,                                       *01814000
                      :PRCOME4,                                        *01815000
                      :PC4QLFY,                                        *01816000
                      :PC4INCTV,                                       *01817000
                      :RSVFLAG1,                                       *01818000
                      :CLACCOPT,                                       *01819000
                      :SECSVOPT,                                       *01820000
                      :RATEOPT,                                        *01821000
                      :ENRATTP,                                        *01822000
                      :MODELTYP,                                       *01823000
                      :LONGDESC,                                       *01824000
                      :PRIMCODE,                                       *01825000
                      :PRIMQLFY,                                       *01826000
                      :EXTENROL                                         01827000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01828000
         BALR  4,5                     MOVE INTO RECORD AREA            01829000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01830000
         BR    14                      RETURN TO CALLER                 01831000
         LTORG                                                          01832000
*                                                                       01833000
*                                                                       01834000
**********************************************************************  01835000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 1:           01836000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01837000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01838000
*     RETRIEVE THE ACTUAL ROW.                                          01839000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01840000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01841000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01842000
**********************************************************************  01843000
*                                                                       01844000
SQL#0021 DS    0H                                                       01845000
         USING SQL#0021,12,7           ESTABLISH BASE REGISTER          01846000
         LA    15,255                  SET RETURN CODE                  01847000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01848000
         BR    14                      RETURN TO CALLER                 01849000
         LTORG                                                          01850000
*                                                                       01851000
*                                                                       01852000
**********************************************************************  01853000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 2:           01854000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01855000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01856000
*     RETRIEVE THE ACTUAL ROW.                                          01857000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01858000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01859000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01860000
**********************************************************************  01861000
*                                                                       01862000
SQL#0022 DS    0H                                                       01863000
         USING SQL#0022,12,7           ESTABLISH BASE REGISTER          01864000
         LA    15,255                  SET RETURN CODE                  01865000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01866000
         BR    14                      RETURN TO CALLER                 01867000
         LTORG                                                          01868000
*                                                                       01869000
*                                                                       01870000
**********************************************************************  01871000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 3:           01872000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01873000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01874000
*     RETRIEVE THE ACTUAL ROW.                                          01875000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01876000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01877000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01878000
**********************************************************************  01879000
*                                                                       01880000
SQL#0023 DS    0H                                                       01881000
         USING SQL#0023,12,7           ESTABLISH BASE REGISTER          01882000
         LA    15,255                  SET RETURN CODE                  01883000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01884000
         BR    14                      RETURN TO CALLER                 01885000
         LTORG                                                          01886000
*                                                                       01887000
*                                                                       01888000
**********************************************************************  01889000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01890000
* THE PRIMARY KEY:                                                      01891000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01892000
*     VERBS.                                                            01893000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01894000
*     RETRIEVE THE ACTUAL ROW.                                          01895000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01896000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01897000
*     WILL BE THRU THE UPDATE CURSOR.                                   01898000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01899000
**********************************************************************  01900000
*                                                                       01901000
SQL#0024 DS    0H                                                       01902000
         USING SQL#0024,12,7           ESTABLISH BASE REGISTER          01903000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01904000
         LA    12,VECT0024(1)          LOAD POINTER TO FETCH ROUTINE    01905000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01906000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01907000
VECT0024 DC    A(SQL#0124)                                              01908000
         DC    A(SQL#0224)                                              01909000
         DC    A(SQL#0324)                                              01910000
         DC    A(SQL#0424)                                              01911000
         LTORG                                                          01912000
*                                                                       01913000
SQL#0124 DS    0H                                                       01914000
         USING SQL#0124,12,7           ESTABLISH BASE REGISTER          01915000
         B     *+6                     BRANCH AROUND ADCON              01916000
BASE0124 DC    AL2(4096)                                                01917000
         LR    7,12                    LOAD SECOND BASE                 01918000
         AH    7,BASE0124              ADD 4K                           01919000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01920000
         AH    3,BASE0124              ADD 4K                           01921000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01922000
         AH    4,BASE0124              ADD 4K                           01923000
         EXEC  SQL                                                     *01924000
               FETCH S01SQ001                                          *01925000
                 INTO :INST,                                           *01926000
                      :RECNBR,                                         *01927000
                      :MODEL,                                          *01928000
                      :EFFDATE                                          01929000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01930000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01931000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01932000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01933000
         BR    12                      OPEN NEXT CURSOR                 01934000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01935000
         BALR  4,5                     MOVE INTO RECORD AREA            01936000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01937000
         BR    14                      RETURN TO CALLER                 01938000
         LTORG                                                          01939000
*                                                                       01940000
SQL#0224 DS    0H                                                       01941000
         USING SQL#0224,12,7           ESTABLISH BASE REGISTER          01942000
         B     *+6                     BRANCH AROUND ADCON              01943000
BASE0224 DC    AL2(4096)                                                01944000
         LR    7,12                    LOAD SECOND BASE                 01945000
         AH    7,BASE0224              ADD 4K                           01946000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01947000
         AH    3,BASE0224              ADD 4K                           01948000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01949000
         AH    4,BASE0224              ADD 4K                           01950000
         EXEC  SQL                                                     *01951000
               FETCH S01SQ002                                          *01952000
                 INTO :INST,                                           *01953000
                      :RECNBR,                                         *01954000
                      :MODEL,                                          *01955000
                      :EFFDATE                                          01956000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01957000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01958000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01959000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01960000
         BR    12                      OPEN NEXT CURSOR                 01961000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01962000
         BALR  4,5                     MOVE INTO RECORD AREA            01963000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01964000
         BR    14                      RETURN TO CALLER                 01965000
         LTORG                                                          01966000
*                                                                       01967000
SQL#0324 DS    0H                                                       01968000
         USING SQL#0324,12,7           ESTABLISH BASE REGISTER          01969000
         B     *+6                     BRANCH AROUND ADCON              01970000
BASE0324 DC    AL2(4096)                                                01971000
         LR    7,12                    LOAD SECOND BASE                 01972000
         AH    7,BASE0324              ADD 4K                           01973000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01974000
         AH    3,BASE0324              ADD 4K                           01975000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01976000
         AH    4,BASE0324              ADD 4K                           01977000
         EXEC  SQL                                                     *01978000
               FETCH S01SQ003                                          *01979000
                 INTO :INST,                                           *01980000
                      :RECNBR,                                         *01981000
                      :MODEL,                                          *01982000
                      :EFFDATE                                          01983000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01984000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01985000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01986000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01987000
         BR    12                      OPEN NEXT CURSOR                 01988000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01989000
         BALR  4,5                     MOVE INTO RECORD AREA            01990000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01991000
         BR    14                      RETURN TO CALLER                 01992000
         LTORG                                                          01993000
*                                                                       01994000
SQL#0424 DS    0H                                                       01995000
         USING SQL#0424,12,7           ESTABLISH BASE REGISTER          01996000
         B     *+6                     BRANCH AROUND ADCON              01997000
BASE0424 DC    AL2(4096)                                                01998000
         LR    7,12                    LOAD SECOND BASE                 01999000
         AH    7,BASE0424              ADD 4K                           02000000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02001000
         AH    3,BASE0424              ADD 4K                           02002000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02003000
         AH    4,BASE0424              ADD 4K                           02004000
         EXEC  SQL                                                     *02005000
               FETCH S01SQ004                                          *02006000
                 INTO :INST,                                           *02007000
                      :RECNBR,                                         *02008000
                      :MODEL,                                          *02009000
                      :EFFDATE                                          02010000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   02011000
         BALR  4,5                     MOVE INTO RECORD AREA            02012000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02013000
         BR    14                      RETURN TO CALLER                 02014000
         LTORG                                                          02015000
*                                                                       02016000
*                                                                       02017000
**********************************************************************  02018000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        02019000
* ALTERNATE KEY 1:                                                      02020000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           02021000
*     VERBS.                                                            02022000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          02023000
*     RETRIEVE THE ACTUAL ROW.                                          02024000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    02025000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             02026000
*     WILL BE THRU THE UPDATE CURSOR.                                   02027000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02028000
**********************************************************************  02029000
*                                                                       02030000
SQL#0025 DS    0H                                                       02031000
         USING SQL#0025,12,7           ESTABLISH BASE REGISTER          02032000
         LA    15,255                  SET RETURN CODE                  02033000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02034000
         BR    14                      RETURN TO CALLER                 02035000
         LTORG                                                          02036000
*                                                                       02037000
*                                                                       02038000
**********************************************************************  02039000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        02040000
* ALTERNATE KEY 2:                                                      02041000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           02042000
*     VERBS.                                                            02043000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          02044000
*     RETRIEVE THE ACTUAL ROW.                                          02045000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    02046000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             02047000
*     WILL BE THRU THE UPDATE CURSOR.                                   02048000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02049000
**********************************************************************  02050000
*                                                                       02051000
SQL#0026 DS    0H                                                       02052000
         USING SQL#0026,12,7           ESTABLISH BASE REGISTER          02053000
         LA    15,255                  SET RETURN CODE                  02054000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02055000
         BR    14                      RETURN TO CALLER                 02056000
         LTORG                                                          02057000
*                                                                       02058000
*                                                                       02059000
**********************************************************************  02060000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        02061000
* ALTERNATE KEY 3:                                                      02062000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           02063000
*     VERBS.                                                            02064000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          02065000
*     RETRIEVE THE ACTUAL ROW.                                          02066000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    02067000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             02068000
*     WILL BE THRU THE UPDATE CURSOR.                                   02069000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02070000
**********************************************************************  02071000
*                                                                       02072000
SQL#0027 DS    0H                                                       02073000
         USING SQL#0027,12,7           ESTABLISH BASE REGISTER          02074000
         LA    15,255                  SET RETURN CODE                  02075000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02076000
         BR    14                      RETURN TO CALLER                 02077000
         LTORG                                                          02078000
*                                                                       02079000
*                                                                       02080000
**********************************************************************  02081000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:                    02082000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          02083000
*     AND GET-NEXT-LOCK VERBS.                                          02084000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      02085000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   02086000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02087000
**********************************************************************  02088000
*                                                                       02089000
SQL#0028 DS    0H                                                       02090000
         USING SQL#0028,12,7           ESTABLISH BASE REGISTER          02091000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      02092000
         XC    CSRPTR,CSRPTR           CLEAR CURSOR ROUTINE POINTER     02093000
         LA    12,VECT0028(1)          LOAD POINTER TO CLOSE ROUTINE    02094000
         L     12,0(12)                LOAD CLOSE ROUTINE ADDRESS       02095000
         BR    12                      GO TO CURRENT CLOSE ROUTINE      02096000
VECT0028 DC    A(SQL#0128)                                              02097000
         DC    A(SQL#0228)                                              02098000
         DC    A(SQL#0328)                                              02099000
         DC    A(SQL#0428)                                              02100000
         LTORG                                                          02101000
*                                                                       02102000
SQL#0128 DS    0H                                                       02103000
         USING SQL#0128,12,7           ESTABLISH BASE REGISTER          02104000
         B     *+6                     BRANCH AROUND ADCON              02105000
BASE0128 DC    AL2(4096)                                                02106000
         LR    7,12                    LOAD SECOND BASE                 02107000
         AH    7,BASE0128              ADD 4K                           02108000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02109000
         AH    3,BASE0128              ADD 4K                           02110000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02111000
         AH    4,BASE0128              ADD 4K                           02112000
         EXEC  SQL                                                     *02113000
               CLOSE S01SQ001                                           02114000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02115000
         BR    14                      RETURN TO CALLER                 02116000
         LTORG                                                          02117000
*                                                                       02118000
SQL#0228 DS    0H                                                       02119000
         USING SQL#0228,12,7           ESTABLISH BASE REGISTER          02120000
         B     *+6                     BRANCH AROUND ADCON              02121000
BASE0228 DC    AL2(4096)                                                02122000
         LR    7,12                    LOAD SECOND BASE                 02123000
         AH    7,BASE0228              ADD 4K                           02124000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02125000
         AH    3,BASE0228              ADD 4K                           02126000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02127000
         AH    4,BASE0228              ADD 4K                           02128000
         EXEC  SQL                                                     *02129000
               CLOSE S01SQ002                                           02130000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02131000
         BR    14                      RETURN TO CALLER                 02132000
         LTORG                                                          02133000
*                                                                       02134000
SQL#0328 DS    0H                                                       02135000
         USING SQL#0328,12,7           ESTABLISH BASE REGISTER          02136000
         B     *+6                     BRANCH AROUND ADCON              02137000
BASE0328 DC    AL2(4096)                                                02138000
         LR    7,12                    LOAD SECOND BASE                 02139000
         AH    7,BASE0328              ADD 4K                           02140000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02141000
         AH    3,BASE0328              ADD 4K                           02142000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02143000
         AH    4,BASE0328              ADD 4K                           02144000
         EXEC  SQL                                                     *02145000
               CLOSE S01SQ003                                           02146000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02147000
         BR    14                      RETURN TO CALLER                 02148000
         LTORG                                                          02149000
*                                                                       02150000
SQL#0428 DS    0H                                                       02151000
         USING SQL#0428,12,7           ESTABLISH BASE REGISTER          02152000
         B     *+6                     BRANCH AROUND ADCON              02153000
BASE0428 DC    AL2(4096)                                                02154000
         LR    7,12                    LOAD SECOND BASE                 02155000
         AH    7,BASE0428              ADD 4K                           02156000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02157000
         AH    3,BASE0428              ADD 4K                           02158000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02159000
         AH    4,BASE0428              ADD 4K                           02160000
         EXEC  SQL                                                     *02161000
               CLOSE S01SQ004                                           02162000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02163000
         BR    14                      RETURN TO CALLER                 02164000
         LTORG                                                          02165000
*                                                                       02166000
*                                                                       02167000
**********************************************************************  02168000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 1:                02169000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          02170000
*     AND GET-NEXT-LOCK VERBS.                                          02171000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      02172000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   02173000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02174000
**********************************************************************  02175000
*                                                                       02176000
SQL#0029 DS    0H                                                       02177000
         USING SQL#0029,12,7           ESTABLISH BASE REGISTER          02178000
         LA    15,255                  SET RETURN CODE                  02179000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02180000
         BR    14                      RETURN TO CALLER                 02181000
         LTORG                                                          02182000
*                                                                       02183000
*                                                                       02184000
**********************************************************************  02185000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 2:                02186000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          02187000
*     AND GET-NEXT-LOCK VERBS.                                          02188000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      02189000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   02190000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02191000
**********************************************************************  02192000
*                                                                       02193000
SQL#0030 DS    0H                                                       02194000
         USING SQL#0030,12,7           ESTABLISH BASE REGISTER          02195000
         LA    15,255                  SET RETURN CODE                  02196000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02197000
         BR    14                      RETURN TO CALLER                 02198000
         LTORG                                                          02199000
*                                                                       02200000
*                                                                       02201000
**********************************************************************  02202000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 3:                02203000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          02204000
*     AND GET-NEXT-LOCK VERBS.                                          02205000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      02206000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   02207000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02208000
**********************************************************************  02209000
*                                                                       02210000
SQL#0031 DS    0H                                                       02211000
         USING SQL#0031,12,7           ESTABLISH BASE REGISTER          02212000
         LA    15,255                  SET RETURN CODE                  02213000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02214000
         BR    14                      RETURN TO CALLER                 02215000
         LTORG                                                          02216000
*                                                                       02217000
*                                                                       02218000
**********************************************************************  02219000
* SELECT KEY STATEMENT BY PRIMARY KEY:                                  02220000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            02221000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02222000
**********************************************************************  02223000
*                                                                       02224000
SQL#0032 DS    0H                                                       02225000
         USING SQL#0032,12,7           ESTABLISH BASE REGISTER          02226000
         B     *+6                     BRANCH AROUND ADCON              02227000
BASE0032 DC    AL2(4096)                                                02228000
         LR    7,12                    LOAD SECOND BASE                 02229000
         AH    7,BASE0032              ADD 4K                           02230000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              02231000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      02232000
         BALR  4,5                     MOVE INTO HOST VARIABLES         02233000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02234000
         AH    3,BASE0032              ADD 4K                           02235000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02236000
         AH    4,BASE0032              ADD 4K                           02237000
         EXEC  SQL                                                     *02238000
               SELECT INST_NBR,                                        *02239000
                   RECORD_NBR,                                         *02240000
                   MODEL,                                              *02241000
                   EFFECTIVE_DATE                                      *02242000
                 INTO :INST,                                           *02243000
                   :RECNBR,                                            *02244000
                   :MODEL,                                             *02245000
                   :EFFDATE                                            *02246000
                 FROM S01                                              *02247000
                 WHERE INST_NBR = :INST AND                            *02248000
                   RECORD_NBR = :RECNBR AND                            *02249000
                   MODEL = :MODEL AND                                  *02250000
                   EFFECTIVE_DATE = :EFFDATE                           *02251000
                 FETCH FIRST 1 ROW ONLY                                 02252000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02253000
         BR    14                      RETURN TO CALLER                 02254000
         LTORG                                                          02255000
*                                                                       02256000
*                                                                       02257000
**********************************************************************  02258000
* SELECT KEY STATEMENT BY ALTERNATE KEY 1:                              02259000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            02260000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02261000
**********************************************************************  02262000
*                                                                       02263000
SQL#0033 DS    0H                                                       02264000
         USING SQL#0033,12,7           ESTABLISH BASE REGISTER          02265000
         LA    15,255                  SET RETURN CODE                  02266000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02267000
         BR    14                      RETURN TO CALLER                 02268000
         LTORG                                                          02269000
*                                                                       02270000
*                                                                       02271000
**********************************************************************  02272000
* SELECT KEY STATEMENT BY ALTERNATE KEY 2:                              02273000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            02274000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02275000
**********************************************************************  02276000
*                                                                       02277000
SQL#0034 DS    0H                                                       02278000
         USING SQL#0034,12,7           ESTABLISH BASE REGISTER          02279000
         LA    15,255                  SET RETURN CODE                  02280000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02281000
         BR    14                      RETURN TO CALLER                 02282000
         LTORG                                                          02283000
*                                                                       02284000
*                                                                       02285000
**********************************************************************  02286000
* SELECT KEY STATEMENT BY ALTERNATE KEY 3:                              02287000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            02288000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02289000
**********************************************************************  02290000
*                                                                       02291000
SQL#0035 DS    0H                                                       02292000
         USING SQL#0035,12,7           ESTABLISH BASE REGISTER          02293000
         LA    15,255                  SET RETURN CODE                  02294000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02295000
         BR    14                      RETURN TO CALLER                 02296000
         LTORG                                                          02297000
*                                                                       02298000
*                                                                       02299000
**********************************************************************  02300000
* INSERT STATEMENT:                                                     02301000
*   THIS STATEMENT SUPPORTS THE PUT VERB.                               02302000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02303000
**********************************************************************  02304000
*                                                                       02305000
SQL#0036 DS    0H                                                       02306000
         USING SQL#0036,12,7           ESTABLISH BASE REGISTER          02307000
         B     *+6                     BRANCH AROUND ADCON              02308000
BASE0036 DC    AL2(4096)                                                02309000
         LR    7,12                    LOAD SECOND BASE                 02310000
         AH    7,BASE0036              ADD 4K                           02311000
         L     5,=A(IN#HOST)           LOAD INPUT CONVERSION ROUTINE    02312000
         BALR  4,5                     MOVE INTO HOST VARIABLES         02313000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02314000
         AH    3,BASE0036              ADD 4K                           02315000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02316000
         AH    4,BASE0036              ADD 4K                           02317000
         EXEC  SQL                                                     *02318000
               INSERT INTO S01                                         *02319000
                   (INST_NBR,                                          *02320000
                    RECORD_NBR,                                        *02321000
                    MODEL,                                             *02322000
                    EFFECTIVE_DATE,                                    *02323000
                    AUDIT_DATE,                                        *02324000
                    AUDIT_TIME,                                        *02325000
                    AUDIT_USER,                                        *02326000
                    AUDIT_ORG,                                         *02327000
                    ACTIVE_CODE,                                       *02328000
                    SHORT_DESC,                                        *02329000
                    SCORE,                                             *02330000
                    AUTO_ENROLL,                                       *02331000
                    INCEN_BAL_OPT,                                     *02332000
                    PROCESS_OPTION,                                    *02333000
                    CMB_BAL_OPT,                                       *02334000
                    EXPIRE_FREQ,                                       *02335000
                    EXPIRE_TERM,                                       *02336000
                    END_MONTH,                                         *02337000
                    CLOSE_DATE,                                        *02338000
                    EXPIRE_DATE,                                       *02339000
                    EXPIRE_RET_DAYS,                                   *02340000
                    FIRST_RETENTION,                                   *02341000
                    DAY_PER_EXTN,                                      *02342000
                    EXTENSION,                                         *02343000
                    CASH_OPT,                                          *02344000
                    CASH_PCT,                                          *02345000
                    CASH_BAL_OPT,                                      *02346000
                    CASH_FREQ,                                         *02347000
                    CASH_TERM,                                         *02348000
                    CASH_DAY,                                          *02349000
                    NBR_ACCOUNTS,                                      *02350000
                    ACCT_QLFY,                                         *02351000
                    NBR_PROD,                                          *02352000
                    PROD_QLFY,                                         *02353000
                    MIN_ACCT,                                          *02354000
                    MIN_ACCT_Q,                                        *02355000
                    MAX_ACCT,                                          *02356000
                    MAX_ACCT_Q,                                        *02357000
                    MIN_PROD,                                          *02358000
                    MIN_PROD_Q,                                        *02359000
                    MAX_PROD,                                          *02360000
                    MAX_PROD_Q,                                        *02361000
                    DEMOGR_CODE,                                       *02362000
                    DEMOGR_QLFY,                                       *02363000
                    CUST_REL_CODE,                                     *02364000
                    CUST_QLFY,                                         *02365000
                    PRIM_BAL_OPT,                                      *02366000
                    PRIM_BAL_OPT_Q,                                    *02367000
                    PRIM_BAL_OPT_I,                                    *02368000
                    ACCT_REL_CD_Q,                                     *02369000
                    ACCT_REL_CD_I,                                     *02370000
                    ACCT_REL_Q,                                        *02371000
                    ACCT_REL_INCEN,                                    *02372000
                    CMB_CATG_CD_1,                                     *02373000
                    CMB_CATG_1_Q,                                      *02374000
                    CMB_CATG_1_INC,                                    *02375000
                    CMB_CATG_CD_2,                                     *02376000
                    CMB_CATG_2_Q,                                      *02377000
                    CMB_CATG_2_INC,                                    *02378000
                    PRIME_CMB_OPT_1,                                   *02379000
                    PRIME_C_1_QLFY,                                    *02380000
                    PRIME_C_1_INCEN,                                   *02381000
                    PRIME_CMB_OPT_2,                                   *02382000
                    PRIME_C_2_QLFY,                                    *02383000
                    PRIME_C_2_INCEN,                                   *02384000
                    PROD_GRP_MIN_1,                                    *02385000
                    PROD_GRP_MAX_1,                                    *02386000
                    PROD_GRP_1_Q,                                      *02387000
                    PROD_GRP_MIN_2,                                    *02388000
                    PROD_GRP_MAX_2,                                    *02389000
                    PROD_GRP_2_Q,                                      *02390000
                    PROD_GRP_MIN_3,                                    *02391000
                    PROD_GRP_MAX_3,                                    *02392000
                    PROD_GRP_3_Q,                                      *02393000
                    PROD_GRP_MIN_4,                                    *02394000
                    PROD_GRP_MAX_4,                                    *02395000
                    PROD_GRP_4_Q,                                      *02396000
                    PROD_GRP_MIN_5,                                    *02397000
                    PROD_GRP_MAX_5,                                    *02398000
                    PROD_GRP_5_Q,                                      *02399000
                    ACCT_INCEN_OVRD,                                   *02400000
                    DEFAULT_MODEL,                                     *02401000
                    INCEN_BAL_TRN,                                     *02402000
                    TRAN_PROFILE,                                      *02403000
                    TRAN_INCENTIVE,                                    *02404000
                    CASH_MAX_AMT_PD,                                   *02405000
                    CASH_MAX_BAL,                                      *02406000
                    CMB_CATG_CD_3,                                     *02407000
                    CMB_CATG_3_Q,                                      *02408000
                    CMB_CATG_3_INC,                                    *02409000
                    CMB_CATG_CD_4,                                     *02410000
                    CMB_CATG_4_Q,                                      *02411000
                    CMB_CATG_4_INC,                                    *02412000
                    PRIME_CMB_OPT_3,                                   *02413000
                    PRIME_C_3_QLFY,                                    *02414000
                    PRIME_C_3_INCEN,                                   *02415000
                    PRIME_CMB_OPT_4,                                   *02416000
                    PRIME_C_4_QLFY,                                    *02417000
                    PRIME_C_4_INCEN,                                   *02418000
                    RSV_FLAG_1,                                        *02419000
                    CLOSED_ACCT_OPT,                                   *02420000
                    SEC_SVC_OPT,                                       *02421000
                    RATE_OPTION,                                       *02422000
                    ENROLL_LEFT,                                       *02423000
                    MODEL_TYPE,                                        *02424000
                    LONG_DESC,                                         *02425000
                    PRIM_REL_CODE,                                     *02426000
                    PRIM_QLFY,                                         *02427000
                    EXTL_ENROLL)                                       *02428000
                  VALUES (:INST,                                       *02429000
                   :RECNBR,                                            *02430000
                   :MODEL,                                             *02431000
                   :EFFDATE,                                           *02432000
                   :AUDDATE,                                           *02433000
                   :AUDTIME,                                           *02434000
                   :AUDUSER,                                           *02435000
                   :AUDORG,                                            *02436000
                   :ACTVCDE,                                           *02437000
                   :SHRTDESC,                                          *02438000
                   :SCORE,                                             *02439000
                   :AUENROLL,                                          *02440000
                   :INCTVBAL,                                          *02441000
                   :PROCOPT,                                           *02442000
                   :CBALOPT,                                           *02443000
                   :EXPFREQ,                                           *02444000
                   :EXPTERM,                                           *02445000
                   :ENDMNTH,                                           *02446000
                   :CLSD,                                              *02447000
                   :EXPRDATE,                                          *02448000
                   :EXRETDAY,                                          *02449000
                   :FIRSTRET,                                          *02450000
                   :DAYSEXT,                                           *02451000
                   :EXTN,                                              *02452000
                   :CASHOPT,                                           *02453000
                   :CASHPCT,                                           *02454000
                   :CASHBAL,                                           *02455000
                   :CASHFREQ,                                          *02456000
                   :CASHTERM,                                          *02457000
                   :CASHDAY,                                           *02458000
                   :NBRACCT,                                           *02459000
                   :ACCTQLFY,                                          *02460000
                   :NBRPROD,                                           *02461000
                   :PRODQLFY,                                          *02462000
                   :MINACCT,                                           *02463000
                   :MINACCTQ,                                          *02464000
                   :MAXACCT,                                           *02465000
                   :MAXACCTQ,                                          *02466000
                   :MINPROD,                                           *02467000
                   :MINPRODQ,                                          *02468000
                   :MAXPROD,                                           *02469000
                   :MAXPRODQ,                                          *02470000
                   :DEMOCODE,                                          *02471000
                   :DEMOQLFY,                                          *02472000
                   :CUSTCODE,                                          *02473000
                   :CUSTQLFY,                                          *02474000
                   :PBALOPT,                                           *02475000
                   :PBALOPTQ,                                          *02476000
                   :PBALOPTI,                                          *02477000
                   :ACCTCDQ,                                           *02478000
                   :ACCTCDI,                                           *02479000
                   :ACTRELQ,                                           *02480000
                   :ACINCEN,                                           *02481000
                   :COMBCD1,                                           *02482000
                   :CCQLFY1,                                           *02483000
                   :CC1INCTV,                                          *02484000
                   :COMBCD2,                                           *02485000
                   :CCQLFY2,                                           *02486000
                   :CC2INCTV,                                          *02487000
                   :PRCOME1,                                           *02488000
                   :PC1QLFY,                                           *02489000
                   :PC1INCTV,                                          *02490000
                   :PRCOME2,                                           *02491000
                   :PC2QLFY,                                           *02492000
                   :PC2INCTV,                                          *02493000
                   :PGMIN1,                                            *02494000
                   :PGMAX1,                                            *02495000
                   :PRDGRP1Q,                                          *02496000
                   :PGMIN2,                                            *02497000
                   :PGMAX2,                                            *02498000
                   :PRDGRP2Q,                                          *02499000
                   :PGMIN3,                                            *02500000
                   :PGMAX3,                                            *02501000
                   :PRDGRP3Q,                                          *02502000
                   :PGMIN4,                                            *02503000
                   :PGMAX4,                                            *02504000
                   :PRDGRP4Q,                                          *02505000
                   :PGMIN5,                                            *02506000
                   :PGMAX5,                                            *02507000
                   :PRDGRP5Q,                                          *02508000
                   :INCENTOV,                                          *02509000
                   :DFLTMODL,                                          *02510000
                   :INCBALTR,                                          *02511000
                   :TRANPROF,                                          *02512000
                   :TRPINCEN,                                          *02513000
                   :CRWAMT,                                            *02514000
                   :CRWMAXCB,                                          *02515000
                   :COMBCD3,                                           *02516000
                   :CCQLFY3,                                           *02517000
                   :CC3INCTV,                                          *02518000
                   :COMBCD4,                                           *02519000
                   :CCQLFY4,                                           *02520000
                   :CC4INCTV,                                          *02521000
                   :PRCOME3,                                           *02522000
                   :PC3QLFY,                                           *02523000
                   :PC3INCTV,                                          *02524000
                   :PRCOME4,                                           *02525000
                   :PC4QLFY,                                           *02526000
                   :PC4INCTV,                                          *02527000
                   :RSVFLAG1,                                          *02528000
                   :CLACCOPT,                                          *02529000
                   :SECSVOPT,                                          *02530000
                   :RATEOPT,                                           *02531000
                   :ENRATTP,                                           *02532000
                   :MODELTYP,                                          *02533000
                   :LONGDESC,                                          *02534000
                   :PRIMCODE,                                          *02535000
                   :PRIMQLFY,                                          *02536000
                   :EXTENROL)                                           02537000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02538000
         BR    14                      RETURN TO CALLER                 02539000
         LTORG                                                          02540000
*                                                                       02541000
*                                                                       02542000
**********************************************************************  02543000
* UPDATE STATEMENT BY PRIMARY KEY:                                      02544000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             02545000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        02546000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         02547000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             02548000
*     STATEMENT.                                                        02549000
**********************************************************************  02550000
*                                                                       02551000
SQL#0037 DS    0H                                                       02552000
         USING SQL#0037,12,7           ESTABLISH BASE REGISTER          02553000
         B     *+6                     BRANCH AROUND ADCON              02554000
BASE0037 DC    AL2(4096)                                                02555000
         LR    7,12                    LOAD SECOND BASE                 02556000
         AH    7,BASE0037              ADD 4K                           02557000
         L     5,=A(IN#HOST)           LOAD INPUT CONVERSION ROUTINE    02558000
         BALR  4,5                     MOVE INTO HOST VARIABLES         02559000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02560000
         AH    3,BASE0037              ADD 4K                           02561000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02562000
         AH    4,BASE0037              ADD 4K                           02563000
         EXEC  SQL                                                     *02564000
               UPDATE S01                                              *02565000
                   SET AUDIT_DATE = :AUDDATE,                          *02566000
                     AUDIT_TIME = :AUDTIME,                            *02567000
                     AUDIT_USER = :AUDUSER,                            *02568000
                     AUDIT_ORG = :AUDORG,                              *02569000
                     ACTIVE_CODE = :ACTVCDE,                           *02570000
                     SHORT_DESC = :SHRTDESC,                           *02571000
                     SCORE = :SCORE,                                   *02572000
                     AUTO_ENROLL = :AUENROLL,                          *02573000
                     INCEN_BAL_OPT = :INCTVBAL,                        *02574000
                     PROCESS_OPTION = :PROCOPT,                        *02575000
                     CMB_BAL_OPT = :CBALOPT,                           *02576000
                     EXPIRE_FREQ = :EXPFREQ,                           *02577000
                     EXPIRE_TERM = :EXPTERM,                           *02578000
                     END_MONTH = :ENDMNTH,                             *02579000
                     CLOSE_DATE = :CLSD,                               *02580000
                     EXPIRE_DATE = :EXPRDATE,                          *02581000
                     EXPIRE_RET_DAYS = :EXRETDAY,                      *02582000
                     FIRST_RETENTION = :FIRSTRET,                      *02583000
                     DAY_PER_EXTN = :DAYSEXT,                          *02584000
                     EXTENSION = :EXTN,                                *02585000
                     CASH_OPT = :CASHOPT,                              *02586000
                     CASH_PCT = :CASHPCT,                              *02587000
                     CASH_BAL_OPT = :CASHBAL,                          *02588000
                     CASH_FREQ = :CASHFREQ,                            *02589000
                     CASH_TERM = :CASHTERM,                            *02590000
                     CASH_DAY = :CASHDAY,                              *02591000
                     NBR_ACCOUNTS = :NBRACCT,                          *02592000
                     ACCT_QLFY = :ACCTQLFY,                            *02593000
                     NBR_PROD = :NBRPROD,                              *02594000
                     PROD_QLFY = :PRODQLFY,                            *02595000
                     MIN_ACCT = :MINACCT,                              *02596000
                     MIN_ACCT_Q = :MINACCTQ,                           *02597000
                     MAX_ACCT = :MAXACCT,                              *02598000
                     MAX_ACCT_Q = :MAXACCTQ,                           *02599000
                     MIN_PROD = :MINPROD,                              *02600000
                     MIN_PROD_Q = :MINPRODQ,                           *02601000
                     MAX_PROD = :MAXPROD,                              *02602000
                     MAX_PROD_Q = :MAXPRODQ,                           *02603000
                     DEMOGR_CODE = :DEMOCODE,                          *02604000
                     DEMOGR_QLFY = :DEMOQLFY,                          *02605000
                     CUST_REL_CODE = :CUSTCODE,                        *02606000
                     CUST_QLFY = :CUSTQLFY,                            *02607000
                     PRIM_BAL_OPT = :PBALOPT,                          *02608000
                     PRIM_BAL_OPT_Q = :PBALOPTQ,                       *02609000
                     PRIM_BAL_OPT_I = :PBALOPTI,                       *02610000
                     ACCT_REL_CD_Q = :ACCTCDQ,                         *02611000
                     ACCT_REL_CD_I = :ACCTCDI,                         *02612000
                     ACCT_REL_Q = :ACTRELQ,                            *02613000
                     ACCT_REL_INCEN = :ACINCEN,                        *02614000
                     CMB_CATG_CD_1 = :COMBCD1,                         *02615000
                     CMB_CATG_1_Q = :CCQLFY1,                          *02616000
                     CMB_CATG_1_INC = :CC1INCTV,                       *02617000
                     CMB_CATG_CD_2 = :COMBCD2,                         *02618000
                     CMB_CATG_2_Q = :CCQLFY2,                          *02619000
                     CMB_CATG_2_INC = :CC2INCTV,                       *02620000
                     PRIME_CMB_OPT_1 = :PRCOME1,                       *02621000
                     PRIME_C_1_QLFY = :PC1QLFY,                        *02622000
                     PRIME_C_1_INCEN = :PC1INCTV,                      *02623000
                     PRIME_CMB_OPT_2 = :PRCOME2,                       *02624000
                     PRIME_C_2_QLFY = :PC2QLFY,                        *02625000
                     PRIME_C_2_INCEN = :PC2INCTV,                      *02626000
                     PROD_GRP_MIN_1 = :PGMIN1,                         *02627000
                     PROD_GRP_MAX_1 = :PGMAX1,                         *02628000
                     PROD_GRP_1_Q = :PRDGRP1Q,                         *02629000
                     PROD_GRP_MIN_2 = :PGMIN2,                         *02630000
                     PROD_GRP_MAX_2 = :PGMAX2,                         *02631000
                     PROD_GRP_2_Q = :PRDGRP2Q,                         *02632000
                     PROD_GRP_MIN_3 = :PGMIN3,                         *02633000
                     PROD_GRP_MAX_3 = :PGMAX3,                         *02634000
                     PROD_GRP_3_Q = :PRDGRP3Q,                         *02635000
                     PROD_GRP_MIN_4 = :PGMIN4,                         *02636000
                     PROD_GRP_MAX_4 = :PGMAX4,                         *02637000
                     PROD_GRP_4_Q = :PRDGRP4Q,                         *02638000
                     PROD_GRP_MIN_5 = :PGMIN5,                         *02639000
                     PROD_GRP_MAX_5 = :PGMAX5,                         *02640000
                     PROD_GRP_5_Q = :PRDGRP5Q,                         *02641000
                     ACCT_INCEN_OVRD = :INCENTOV,                      *02642000
                     DEFAULT_MODEL = :DFLTMODL,                        *02643000
                     INCEN_BAL_TRN = :INCBALTR,                        *02644000
                     TRAN_PROFILE = :TRANPROF,                         *02645000
                     TRAN_INCENTIVE = :TRPINCEN,                       *02646000
                     CASH_MAX_AMT_PD = :CRWAMT,                        *02647000
                     CASH_MAX_BAL = :CRWMAXCB,                         *02648000
                     CMB_CATG_CD_3 = :COMBCD3,                         *02649000
                     CMB_CATG_3_Q = :CCQLFY3,                          *02650000
                     CMB_CATG_3_INC = :CC3INCTV,                       *02651000
                     CMB_CATG_CD_4 = :COMBCD4,                         *02652000
                     CMB_CATG_4_Q = :CCQLFY4,                          *02653000
                     CMB_CATG_4_INC = :CC4INCTV,                       *02654000
                     PRIME_CMB_OPT_3 = :PRCOME3,                       *02655000
                     PRIME_C_3_QLFY = :PC3QLFY,                        *02656000
                     PRIME_C_3_INCEN = :PC3INCTV,                      *02657000
                     PRIME_CMB_OPT_4 = :PRCOME4,                       *02658000
                     PRIME_C_4_QLFY = :PC4QLFY,                        *02659000
                     PRIME_C_4_INCEN = :PC4INCTV,                      *02660000
                     RSV_FLAG_1 = :RSVFLAG1,                           *02661000
                     CLOSED_ACCT_OPT = :CLACCOPT,                      *02662000
                     SEC_SVC_OPT = :SECSVOPT,                          *02663000
                     RATE_OPTION = :RATEOPT,                           *02664000
                     ENROLL_LEFT = :ENRATTP,                           *02665000
                     MODEL_TYPE = :MODELTYP,                           *02666000
                     LONG_DESC = :LONGDESC,                            *02667000
                     PRIM_REL_CODE = :PRIMCODE,                        *02668000
                     PRIM_QLFY = :PRIMQLFY,                            *02669000
                     EXTL_ENROLL = :EXTENROL                           *02670000
                 WHERE CURRENT OF S01UPD0                               02671000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02672000
         BR    14                      RETURN TO CALLER                 02673000
         LTORG                                                          02674000
*                                                                       02675000
*                                                                       02676000
**********************************************************************  02677000
* UPDATE STATEMENT BY ALTERNATE KEY 1:                                  02678000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             02679000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        02680000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         02681000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             02682000
*     STATEMENT.                                                        02683000
**********************************************************************  02684000
*                                                                       02685000
SQL#0038 DS    0H                                                       02686000
         USING SQL#0038,12,7           ESTABLISH BASE REGISTER          02687000
         LA    15,255                  SET RETURN CODE                  02688000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02689000
         BR    14                      RETURN TO CALLER                 02690000
         LTORG                                                          02691000
*                                                                       02692000
*                                                                       02693000
**********************************************************************  02694000
* UPDATE STATEMENT BY ALTERNATE KEY 2:                                  02695000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             02696000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        02697000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         02698000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             02699000
*     STATEMENT.                                                        02700000
**********************************************************************  02701000
*                                                                       02702000
SQL#0039 DS    0H                                                       02703000
         USING SQL#0039,12,7           ESTABLISH BASE REGISTER          02704000
         LA    15,255                  SET RETURN CODE                  02705000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02706000
         BR    14                      RETURN TO CALLER                 02707000
         LTORG                                                          02708000
*                                                                       02709000
*                                                                       02710000
**********************************************************************  02711000
* UPDATE STATEMENT BY ALTERNATE KEY 3:                                  02712000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             02713000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        02714000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         02715000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             02716000
*     STATEMENT.                                                        02717000
**********************************************************************  02718000
*                                                                       02719000
SQL#0040 DS    0H                                                       02720000
         USING SQL#0040,12,7           ESTABLISH BASE REGISTER          02721000
         LA    15,255                  SET RETURN CODE                  02722000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02723000
         BR    14                      RETURN TO CALLER                 02724000
         LTORG                                                          02725000
*                                                                       02726000
*                                                                       02727000
**********************************************************************  02728000
* DELETE STATEMENT BY PRIMARY KEY:                                      02729000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            02730000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02731000
**********************************************************************  02732000
*                                                                       02733000
SQL#0041 DS    0H                                                       02734000
         USING SQL#0041,12,7           ESTABLISH BASE REGISTER          02735000
         B     *+6                     BRANCH AROUND ADCON              02736000
BASE0041 DC    AL2(4096)                                                02737000
         LR    7,12                    LOAD SECOND BASE                 02738000
         AH    7,BASE0041              ADD 4K                           02739000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02740000
         AH    3,BASE0041              ADD 4K                           02741000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02742000
         AH    4,BASE0041              ADD 4K                           02743000
         EXEC  SQL                                                     *02744000
               DELETE FROM S01                                         *02745000
                 WHERE CURRENT OF S01UPD0                               02746000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02747000
         BR    14                      RETURN TO CALLER                 02748000
         LTORG                                                          02749000
*                                                                       02750000
*                                                                       02751000
**********************************************************************  02752000
* DELETE STATEMENT BY ALTERNATE KEY 1:                                  02753000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            02754000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02755000
**********************************************************************  02756000
*                                                                       02757000
SQL#0042 DS    0H                                                       02758000
         USING SQL#0042,12,7           ESTABLISH BASE REGISTER          02759000
         LA    15,255                  SET RETURN CODE                  02760000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02761000
         BR    14                      RETURN TO CALLER                 02762000
         LTORG                                                          02763000
*                                                                       02764000
*                                                                       02765000
**********************************************************************  02766000
* DELETE STATEMENT BY ALTERNATE KEY 2:                                  02767000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            02768000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02769000
**********************************************************************  02770000
*                                                                       02771000
SQL#0043 DS    0H                                                       02772000
         USING SQL#0043,12,7           ESTABLISH BASE REGISTER          02773000
         LA    15,255                  SET RETURN CODE                  02774000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02775000
         BR    14                      RETURN TO CALLER                 02776000
         LTORG                                                          02777000
*                                                                       02778000
*                                                                       02779000
**********************************************************************  02780000
* DELETE STATEMENT BY ALTERNATE KEY 3:                                  02781000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            02782000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02783000
**********************************************************************  02784000
*                                                                       02785000
SQL#0044 DS    0H                                                       02786000
         USING SQL#0044,12,7           ESTABLISH BASE REGISTER          02787000
         LA    15,255                  SET RETURN CODE                  02788000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02789000
         BR    14                      RETURN TO CALLER                 02790000
         LTORG                                                          02791000
*                                                                       02792000
*                                                                       02793000
**********************************************************************  02794000
* DELETE ALL STATEMENT:                                                 02795000
*   THIS STATEMENT SUPPORTS THE DELETE-FILE VERB.                       02796000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02797000
**********************************************************************  02798000
*                                                                       02799000
SQL#0045 DS    0H                                                       02800000
         USING SQL#0045,12,7           ESTABLISH BASE REGISTER          02801000
         B     *+6                     BRANCH AROUND ADCON              02802000
BASE0045 DC    AL2(4096)                                                02803000
         LR    7,12                    LOAD SECOND BASE                 02804000
         AH    7,BASE0045              ADD 4K                           02805000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02806000
         AH    3,BASE0045              ADD 4K                           02807000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02808000
         AH    4,BASE0045              ADD 4K                           02809000
         EXEC  SQL                                                     *02810000
               DELETE FROM S01                                          02811000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02812000
         BR    14                      RETURN TO CALLER                 02813000
         LTORG                                                          02814000
*                                                                       02815000
*                                                                       02816000
**********************************************************************  02817000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY KEY:          02818000
*   THIS ROUTINE HANDLES PRIMARY KEY SEQUENTIAL CURSORS.                02819000
**********************************************************************  02820000
*                                                                       02821000
SQL#0046 DS    0H                                                       02822000
         USING SQL#0046,12,7           ESTABLISH BASE REGISTER          02823000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              02824000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      02825000
         BALR  4,5                     MOVE INTO HOST VARIABLES         02826000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      02827000
         LA    1,4(1)                  INCREMENT TO NEXT CURSOR         02828000
         STH   1,CSRPTR                SAVE FOR NEXT CALL               02829000
         LA    12,VECT0046(1)          LOAD POINTER TO NEXT CURSOR      02830000
         L     12,0(12)                LOAD CURSOR ROUTINE ADDRESS      02831000
         BR    12                      GO TO CURRENT CURSOR ROUTINE     02832000
VECT0046 DC    A(0)                                                     02833000
         DC    A(SQL#0246)                                              02834000
         DC    A(SQL#0346)                                              02835000
         DC    A(SQL#0446)                                              02836000
         LTORG                                                          02837000
*                                                                       02838000
SQL#0246 DS    0H                                                       02839000
         USING SQL#0246,12,7           ESTABLISH BASE REGISTER          02840000
         B     *+6                     BRANCH AROUND ADCON              02841000
BASE0246 DC    AL2(4096)                                                02842000
         LR    7,12                    LOAD SECOND BASE                 02843000
         AH    7,BASE0246              ADD 4K                           02844000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02845000
         AH    3,BASE0246              ADD 4K                           02846000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02847000
         AH    4,BASE0246              ADD 4K                           02848000
         EXEC  SQL                                                     *02849000
               CLOSE S01SQ001                                           02850000
         EXEC  SQL                                                     *02851000
               DECLARE S01SQ002 CURSOR FOR                             *02852000
               SELECT INST_NBR,                                        *02853000
                   RECORD_NBR,                                         *02854000
                   MODEL,                                              *02855000
                   EFFECTIVE_DATE,                                     *02856000
                   AUDIT_DATE,                                         *02857000
                   AUDIT_TIME,                                         *02858000
                   AUDIT_USER,                                         *02859000
                   AUDIT_ORG,                                          *02860000
                   ACTIVE_CODE,                                        *02861000
                   SHORT_DESC,                                         *02862000
                   SCORE,                                              *02863000
                   AUTO_ENROLL,                                        *02864000
                   INCEN_BAL_OPT,                                      *02865000
                   PROCESS_OPTION,                                     *02866000
                   CMB_BAL_OPT,                                        *02867000
                   EXPIRE_FREQ,                                        *02868000
                   EXPIRE_TERM,                                        *02869000
                   END_MONTH,                                          *02870000
                   CLOSE_DATE,                                         *02871000
                   EXPIRE_DATE,                                        *02872000
                   EXPIRE_RET_DAYS,                                    *02873000
                   FIRST_RETENTION,                                    *02874000
                   DAY_PER_EXTN,                                       *02875000
                   EXTENSION,                                          *02876000
                   CASH_OPT,                                           *02877000
                   CASH_PCT,                                           *02878000
                   CASH_BAL_OPT,                                       *02879000
                   CASH_FREQ,                                          *02880000
                   CASH_TERM,                                          *02881000
                   CASH_DAY,                                           *02882000
                   NBR_ACCOUNTS,                                       *02883000
                   ACCT_QLFY,                                          *02884000
                   NBR_PROD,                                           *02885000
                   PROD_QLFY,                                          *02886000
                   MIN_ACCT,                                           *02887000
                   MIN_ACCT_Q,                                         *02888000
                   MAX_ACCT,                                           *02889000
                   MAX_ACCT_Q,                                         *02890000
                   MIN_PROD,                                           *02891000
                   MIN_PROD_Q,                                         *02892000
                   MAX_PROD,                                           *02893000
                   MAX_PROD_Q,                                         *02894000
                   DEMOGR_CODE,                                        *02895000
                   DEMOGR_QLFY,                                        *02896000
                   CUST_REL_CODE,                                      *02897000
                   CUST_QLFY,                                          *02898000
                   PRIM_BAL_OPT,                                       *02899000
                   PRIM_BAL_OPT_Q,                                     *02900000
                   PRIM_BAL_OPT_I,                                     *02901000
                   ACCT_REL_CD_Q,                                      *02902000
                   ACCT_REL_CD_I,                                      *02903000
                   ACCT_REL_Q,                                         *02904000
                   ACCT_REL_INCEN,                                     *02905000
                   CMB_CATG_CD_1,                                      *02906000
                   CMB_CATG_1_Q,                                       *02907000
                   CMB_CATG_1_INC,                                     *02908000
                   CMB_CATG_CD_2,                                      *02909000
                   CMB_CATG_2_Q,                                       *02910000
                   CMB_CATG_2_INC,                                     *02911000
                   PRIME_CMB_OPT_1,                                    *02912000
                   PRIME_C_1_QLFY,                                     *02913000
                   PRIME_C_1_INCEN,                                    *02914000
                   PRIME_CMB_OPT_2,                                    *02915000
                   PRIME_C_2_QLFY,                                     *02916000
                   PRIME_C_2_INCEN,                                    *02917000
                   PROD_GRP_MIN_1,                                     *02918000
                   PROD_GRP_MAX_1,                                     *02919000
                   PROD_GRP_1_Q,                                       *02920000
                   PROD_GRP_MIN_2,                                     *02921000
                   PROD_GRP_MAX_2,                                     *02922000
                   PROD_GRP_2_Q,                                       *02923000
                   PROD_GRP_MIN_3,                                     *02924000
                   PROD_GRP_MAX_3,                                     *02925000
                   PROD_GRP_3_Q,                                       *02926000
                   PROD_GRP_MIN_4,                                     *02927000
                   PROD_GRP_MAX_4,                                     *02928000
                   PROD_GRP_4_Q,                                       *02929000
                   PROD_GRP_MIN_5,                                     *02930000
                   PROD_GRP_MAX_5,                                     *02931000
                   PROD_GRP_5_Q,                                       *02932000
                   ACCT_INCEN_OVRD,                                    *02933000
                   DEFAULT_MODEL,                                      *02934000
                   INCEN_BAL_TRN,                                      *02935000
                   TRAN_PROFILE,                                       *02936000
                   TRAN_INCENTIVE,                                     *02937000
                   CASH_MAX_AMT_PD,                                    *02938000
                   CASH_MAX_BAL,                                       *02939000
                   CMB_CATG_CD_3,                                      *02940000
                   CMB_CATG_3_Q,                                       *02941000
                   CMB_CATG_3_INC,                                     *02942000
                   CMB_CATG_CD_4,                                      *02943000
                   CMB_CATG_4_Q,                                       *02944000
                   CMB_CATG_4_INC,                                     *02945000
                   PRIME_CMB_OPT_3,                                    *02946000
                   PRIME_C_3_QLFY,                                     *02947000
                   PRIME_C_3_INCEN,                                    *02948000
                   PRIME_CMB_OPT_4,                                    *02949000
                   PRIME_C_4_QLFY,                                     *02950000
                   PRIME_C_4_INCEN,                                    *02951000
                   RSV_FLAG_1,                                         *02952000
                   CLOSED_ACCT_OPT,                                    *02953000
                   SEC_SVC_OPT,                                        *02954000
                   RATE_OPTION,                                        *02955000
                   ENROLL_LEFT,                                        *02956000
                   MODEL_TYPE,                                         *02957000
                   LONG_DESC,                                          *02958000
                   PRIM_REL_CODE,                                      *02959000
                   PRIM_QLFY,                                          *02960000
                   EXTL_ENROLL                                         *02961000
                 FROM S01                                              *02962000
                 WHERE                                                 *02963000
                    INST_NBR = :INST AND                               *02964000
                    RECORD_NBR = :RECNBR AND                           *02965000
                    MODEL > :MODEL                                     *02966000
                 ORDER BY MODEL,                                       *02967000
                   EFFECTIVE_DATE                                      *02968000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       02969000
         EXEC  SQL                                                     *02970000
               OPEN S01SQ002                                            02971000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            02972000
         BNZ   *+8                     NO - RETURN ERROR                02973000
         LR    12,9                    LOAD RETURN ADDRESS              02974000
         BR    12                      RETURN TO FETCH ROUTINE          02975000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02976000
         BR    14                      RETURN TO CALLER                 02977000
         LTORG                                                          02978000
*                                                                       02979000
SQL#0346 DS    0H                                                       02980000
         USING SQL#0346,12,7           ESTABLISH BASE REGISTER          02981000
         B     *+6                     BRANCH AROUND ADCON              02982000
BASE0346 DC    AL2(4096)                                                02983000
         LR    7,12                    LOAD SECOND BASE                 02984000
         AH    7,BASE0346              ADD 4K                           02985000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02986000
         AH    3,BASE0346              ADD 4K                           02987000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    02988000
         AH    4,BASE0346              ADD 4K                           02989000
         EXEC  SQL                                                     *02990000
               CLOSE S01SQ002                                           02991000
         EXEC  SQL                                                     *02992000
               DECLARE S01SQ003 CURSOR FOR                             *02993000
               SELECT INST_NBR,                                        *02994000
                   RECORD_NBR,                                         *02995000
                   MODEL,                                              *02996000
                   EFFECTIVE_DATE,                                     *02997000
                   AUDIT_DATE,                                         *02998000
                   AUDIT_TIME,                                         *02999000
                   AUDIT_USER,                                         *03000000
                   AUDIT_ORG,                                          *03001000
                   ACTIVE_CODE,                                        *03002000
                   SHORT_DESC,                                         *03003000
                   SCORE,                                              *03004000
                   AUTO_ENROLL,                                        *03005000
                   INCEN_BAL_OPT,                                      *03006000
                   PROCESS_OPTION,                                     *03007000
                   CMB_BAL_OPT,                                        *03008000
                   EXPIRE_FREQ,                                        *03009000
                   EXPIRE_TERM,                                        *03010000
                   END_MONTH,                                          *03011000
                   CLOSE_DATE,                                         *03012000
                   EXPIRE_DATE,                                        *03013000
                   EXPIRE_RET_DAYS,                                    *03014000
                   FIRST_RETENTION,                                    *03015000
                   DAY_PER_EXTN,                                       *03016000
                   EXTENSION,                                          *03017000
                   CASH_OPT,                                           *03018000
                   CASH_PCT,                                           *03019000
                   CASH_BAL_OPT,                                       *03020000
                   CASH_FREQ,                                          *03021000
                   CASH_TERM,                                          *03022000
                   CASH_DAY,                                           *03023000
                   NBR_ACCOUNTS,                                       *03024000
                   ACCT_QLFY,                                          *03025000
                   NBR_PROD,                                           *03026000
                   PROD_QLFY,                                          *03027000
                   MIN_ACCT,                                           *03028000
                   MIN_ACCT_Q,                                         *03029000
                   MAX_ACCT,                                           *03030000
                   MAX_ACCT_Q,                                         *03031000
                   MIN_PROD,                                           *03032000
                   MIN_PROD_Q,                                         *03033000
                   MAX_PROD,                                           *03034000
                   MAX_PROD_Q,                                         *03035000
                   DEMOGR_CODE,                                        *03036000
                   DEMOGR_QLFY,                                        *03037000
                   CUST_REL_CODE,                                      *03038000
                   CUST_QLFY,                                          *03039000
                   PRIM_BAL_OPT,                                       *03040000
                   PRIM_BAL_OPT_Q,                                     *03041000
                   PRIM_BAL_OPT_I,                                     *03042000
                   ACCT_REL_CD_Q,                                      *03043000
                   ACCT_REL_CD_I,                                      *03044000
                   ACCT_REL_Q,                                         *03045000
                   ACCT_REL_INCEN,                                     *03046000
                   CMB_CATG_CD_1,                                      *03047000
                   CMB_CATG_1_Q,                                       *03048000
                   CMB_CATG_1_INC,                                     *03049000
                   CMB_CATG_CD_2,                                      *03050000
                   CMB_CATG_2_Q,                                       *03051000
                   CMB_CATG_2_INC,                                     *03052000
                   PRIME_CMB_OPT_1,                                    *03053000
                   PRIME_C_1_QLFY,                                     *03054000
                   PRIME_C_1_INCEN,                                    *03055000
                   PRIME_CMB_OPT_2,                                    *03056000
                   PRIME_C_2_QLFY,                                     *03057000
                   PRIME_C_2_INCEN,                                    *03058000
                   PROD_GRP_MIN_1,                                     *03059000
                   PROD_GRP_MAX_1,                                     *03060000
                   PROD_GRP_1_Q,                                       *03061000
                   PROD_GRP_MIN_2,                                     *03062000
                   PROD_GRP_MAX_2,                                     *03063000
                   PROD_GRP_2_Q,                                       *03064000
                   PROD_GRP_MIN_3,                                     *03065000
                   PROD_GRP_MAX_3,                                     *03066000
                   PROD_GRP_3_Q,                                       *03067000
                   PROD_GRP_MIN_4,                                     *03068000
                   PROD_GRP_MAX_4,                                     *03069000
                   PROD_GRP_4_Q,                                       *03070000
                   PROD_GRP_MIN_5,                                     *03071000
                   PROD_GRP_MAX_5,                                     *03072000
                   PROD_GRP_5_Q,                                       *03073000
                   ACCT_INCEN_OVRD,                                    *03074000
                   DEFAULT_MODEL,                                      *03075000
                   INCEN_BAL_TRN,                                      *03076000
                   TRAN_PROFILE,                                       *03077000
                   TRAN_INCENTIVE,                                     *03078000
                   CASH_MAX_AMT_PD,                                    *03079000
                   CASH_MAX_BAL,                                       *03080000
                   CMB_CATG_CD_3,                                      *03081000
                   CMB_CATG_3_Q,                                       *03082000
                   CMB_CATG_3_INC,                                     *03083000
                   CMB_CATG_CD_4,                                      *03084000
                   CMB_CATG_4_Q,                                       *03085000
                   CMB_CATG_4_INC,                                     *03086000
                   PRIME_CMB_OPT_3,                                    *03087000
                   PRIME_C_3_QLFY,                                     *03088000
                   PRIME_C_3_INCEN,                                    *03089000
                   PRIME_CMB_OPT_4,                                    *03090000
                   PRIME_C_4_QLFY,                                     *03091000
                   PRIME_C_4_INCEN,                                    *03092000
                   RSV_FLAG_1,                                         *03093000
                   CLOSED_ACCT_OPT,                                    *03094000
                   SEC_SVC_OPT,                                        *03095000
                   RATE_OPTION,                                        *03096000
                   ENROLL_LEFT,                                        *03097000
                   MODEL_TYPE,                                         *03098000
                   LONG_DESC,                                          *03099000
                   PRIM_REL_CODE,                                      *03100000
                   PRIM_QLFY,                                          *03101000
                   EXTL_ENROLL                                         *03102000
                 FROM S01                                              *03103000
                 WHERE                                                 *03104000
                    INST_NBR = :INST AND                               *03105000
                    RECORD_NBR > :RECNBR                               *03106000
                 ORDER BY RECORD_NBR,                                  *03107000
                   MODEL,                                              *03108000
                   EFFECTIVE_DATE                                      *03109000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       03110000
         EXEC  SQL                                                     *03111000
               OPEN S01SQ003                                            03112000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            03113000
         BNZ   *+8                     NO - RETURN ERROR                03114000
         LR    12,9                    LOAD RETURN ADDRESS              03115000
         BR    12                      RETURN TO FETCH ROUTINE          03116000
         L     14,SQW@RET              LOAD RETURN ADDRESS              03117000
         BR    14                      RETURN TO CALLER                 03118000
         LTORG                                                          03119000
*                                                                       03120000
SQL#0446 DS    0H                                                       03121000
         USING SQL#0446,12,7           ESTABLISH BASE REGISTER          03122000
         B     *+6                     BRANCH AROUND ADCON              03123000
BASE0446 DC    AL2(4096)                                                03124000
         LR    7,12                    LOAD SECOND BASE                 03125000
         AH    7,BASE0446              ADD 4K                           03126000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    03127000
         AH    3,BASE0446              ADD 4K                           03128000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    03129000
         AH    4,BASE0446              ADD 4K                           03130000
         EXEC  SQL                                                     *03131000
               CLOSE S01SQ003                                           03132000
         EXEC  SQL                                                     *03133000
               DECLARE S01SQ004 CURSOR FOR                             *03134000
               SELECT INST_NBR,                                        *03135000
                   RECORD_NBR,                                         *03136000
                   MODEL,                                              *03137000
                   EFFECTIVE_DATE,                                     *03138000
                   AUDIT_DATE,                                         *03139000
                   AUDIT_TIME,                                         *03140000
                   AUDIT_USER,                                         *03141000
                   AUDIT_ORG,                                          *03142000
                   ACTIVE_CODE,                                        *03143000
                   SHORT_DESC,                                         *03144000
                   SCORE,                                              *03145000
                   AUTO_ENROLL,                                        *03146000
                   INCEN_BAL_OPT,                                      *03147000
                   PROCESS_OPTION,                                     *03148000
                   CMB_BAL_OPT,                                        *03149000
                   EXPIRE_FREQ,                                        *03150000
                   EXPIRE_TERM,                                        *03151000
                   END_MONTH,                                          *03152000
                   CLOSE_DATE,                                         *03153000
                   EXPIRE_DATE,                                        *03154000
                   EXPIRE_RET_DAYS,                                    *03155000
                   FIRST_RETENTION,                                    *03156000
                   DAY_PER_EXTN,                                       *03157000
                   EXTENSION,                                          *03158000
                   CASH_OPT,                                           *03159000
                   CASH_PCT,                                           *03160000
                   CASH_BAL_OPT,                                       *03161000
                   CASH_FREQ,                                          *03162000
                   CASH_TERM,                                          *03163000
                   CASH_DAY,                                           *03164000
                   NBR_ACCOUNTS,                                       *03165000
                   ACCT_QLFY,                                          *03166000
                   NBR_PROD,                                           *03167000
                   PROD_QLFY,                                          *03168000
                   MIN_ACCT,                                           *03169000
                   MIN_ACCT_Q,                                         *03170000
                   MAX_ACCT,                                           *03171000
                   MAX_ACCT_Q,                                         *03172000
                   MIN_PROD,                                           *03173000
                   MIN_PROD_Q,                                         *03174000
                   MAX_PROD,                                           *03175000
                   MAX_PROD_Q,                                         *03176000
                   DEMOGR_CODE,                                        *03177000
                   DEMOGR_QLFY,                                        *03178000
                   CUST_REL_CODE,                                      *03179000
                   CUST_QLFY,                                          *03180000
                   PRIM_BAL_OPT,                                       *03181000
                   PRIM_BAL_OPT_Q,                                     *03182000
                   PRIM_BAL_OPT_I,                                     *03183000
                   ACCT_REL_CD_Q,                                      *03184000
                   ACCT_REL_CD_I,                                      *03185000
                   ACCT_REL_Q,                                         *03186000
                   ACCT_REL_INCEN,                                     *03187000
                   CMB_CATG_CD_1,                                      *03188000
                   CMB_CATG_1_Q,                                       *03189000
                   CMB_CATG_1_INC,                                     *03190000
                   CMB_CATG_CD_2,                                      *03191000
                   CMB_CATG_2_Q,                                       *03192000
                   CMB_CATG_2_INC,                                     *03193000
                   PRIME_CMB_OPT_1,                                    *03194000
                   PRIME_C_1_QLFY,                                     *03195000
                   PRIME_C_1_INCEN,                                    *03196000
                   PRIME_CMB_OPT_2,                                    *03197000
                   PRIME_C_2_QLFY,                                     *03198000
                   PRIME_C_2_INCEN,                                    *03199000
                   PROD_GRP_MIN_1,                                     *03200000
                   PROD_GRP_MAX_1,                                     *03201000
                   PROD_GRP_1_Q,                                       *03202000
                   PROD_GRP_MIN_2,                                     *03203000
                   PROD_GRP_MAX_2,                                     *03204000
                   PROD_GRP_2_Q,                                       *03205000
                   PROD_GRP_MIN_3,                                     *03206000
                   PROD_GRP_MAX_3,                                     *03207000
                   PROD_GRP_3_Q,                                       *03208000
                   PROD_GRP_MIN_4,                                     *03209000
                   PROD_GRP_MAX_4,                                     *03210000
                   PROD_GRP_4_Q,                                       *03211000
                   PROD_GRP_MIN_5,                                     *03212000
                   PROD_GRP_MAX_5,                                     *03213000
                   PROD_GRP_5_Q,                                       *03214000
                   ACCT_INCEN_OVRD,                                    *03215000
                   DEFAULT_MODEL,                                      *03216000
                   INCEN_BAL_TRN,                                      *03217000
                   TRAN_PROFILE,                                       *03218000
                   TRAN_INCENTIVE,                                     *03219000
                   CASH_MAX_AMT_PD,                                    *03220000
                   CASH_MAX_BAL,                                       *03221000
                   CMB_CATG_CD_3,                                      *03222000
                   CMB_CATG_3_Q,                                       *03223000
                   CMB_CATG_3_INC,                                     *03224000
                   CMB_CATG_CD_4,                                      *03225000
                   CMB_CATG_4_Q,                                       *03226000
                   CMB_CATG_4_INC,                                     *03227000
                   PRIME_CMB_OPT_3,                                    *03228000
                   PRIME_C_3_QLFY,                                     *03229000
                   PRIME_C_3_INCEN,                                    *03230000
                   PRIME_CMB_OPT_4,                                    *03231000
                   PRIME_C_4_QLFY,                                     *03232000
                   PRIME_C_4_INCEN,                                    *03233000
                   RSV_FLAG_1,                                         *03234000
                   CLOSED_ACCT_OPT,                                    *03235000
                   SEC_SVC_OPT,                                        *03236000
                   RATE_OPTION,                                        *03237000
                   ENROLL_LEFT,                                        *03238000
                   MODEL_TYPE,                                         *03239000
                   LONG_DESC,                                          *03240000
                   PRIM_REL_CODE,                                      *03241000
                   PRIM_QLFY,                                          *03242000
                   EXTL_ENROLL                                         *03243000
                 FROM S01                                              *03244000
                 WHERE                                                 *03245000
                    INST_NBR > :INST                                   *03246000
                 ORDER BY INST_NBR,                                    *03247000
                   RECORD_NBR,                                         *03248000
                   MODEL,                                              *03249000
                   EFFECTIVE_DATE                                      *03250000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       03251000
         EXEC  SQL                                                     *03252000
               OPEN S01SQ004                                            03253000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            03254000
         BNZ   *+8                     NO - RETURN ERROR                03255000
         LR    12,9                    LOAD RETURN ADDRESS              03256000
         BR    12                      RETURN TO FETCH ROUTINE          03257000
         L     14,SQW@RET              LOAD RETURN ADDRESS              03258000
         BR    14                      RETURN TO CALLER                 03259000
         LTORG                                                          03260000
*                                                                       03261000
*                                                                       03262000
**********************************************************************  03263000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 1:      03264000
*   THIS ROUTINE HANDLES ALTERNATE KEY 1 SEQUENTIAL CURSORS.            03265000
**********************************************************************  03266000
*                                                                       03267000
SQL#0047 DS    0H                                                       03268000
         USING SQL#0047,12,7           ESTABLISH BASE REGISTER          03269000
         LA    15,255                  SET RETURN CODE                  03270000
         L     14,SQW@RET              LOAD RETURN ADDRESS              03271000
         BR    14                      RETURN TO CALLER                 03272000
         LTORG                                                          03273000
*                                                                       03274000
*                                                                       03275000
**********************************************************************  03276000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 2:      03277000
*   THIS ROUTINE HANDLES ALTERNATE KEY 2 SEQUENTIAL CURSORS.            03278000
**********************************************************************  03279000
*                                                                       03280000
SQL#0048 DS    0H                                                       03281000
         USING SQL#0048,12,7           ESTABLISH BASE REGISTER          03282000
         LA    15,255                  SET RETURN CODE                  03283000
         L     14,SQW@RET              LOAD RETURN ADDRESS              03284000
         BR    14                      RETURN TO CALLER                 03285000
         LTORG                                                          03286000
*                                                                       03287000
*                                                                       03288000
**********************************************************************  03289000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 3:      03290000
*   THIS ROUTINE HANDLES ALTERNATE KEY 3 SEQUENTIAL CURSORS.            03291000
**********************************************************************  03292000
*                                                                       03293000
SQL#0049 DS    0H                                                       03294000
         USING SQL#0049,12,7           ESTABLISH BASE REGISTER          03295000
         LA    15,255                  SET RETURN CODE                  03296000
         L     14,SQW@RET              LOAD RETURN ADDRESS              03297000
         BR    14                      RETURN TO CALLER                 03298000
         LTORG                                                          03299000
*                                                                       03300000
         DS    0H                      END OF SQL STATEMENTS            03301000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   03302000
*                                                                       03303000
*                                                                       03304000
**********************************************************************  03305000
* DUMMY ENTRY POINT DSNHLI                                              03306000
*   MUST NOT BE MODIFIED.                                               03307000
**********************************************************************  03308000
*                                                                       03309000
         ENTRY DSNHLI                                                   03310000
DSNHLI   DS    0H                                                       03311000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       03312000
         BR    15                      BRANCH TO ATTACH FACILITY        03313000
*                                                                       03314000
*                                                                       03315000
**********************************************************************  03316000
* CONVERSION ROUTINES INTO AND OUT OF ASSEMBLER HOST VARIABLES          03317000
*   REQUIRED IF THE COBOL RECORD STRUCTURE IS NOT ALIGNED.              03318000
*   MUST NOT BE MODIFIED.                                               03319000
**********************************************************************  03320000
*                                                                       03321000
CONV#RTN CSECT                         CONVERSION ROUTINES SECTION      03322000
CONV#RTN AMODE ANY                                                      03323000
CONV#RTN RMODE ANY                                                      03324000
IN#HOST  DS    0H                      MOVE INTO HOST VARIABLES         03325000
         USING IN#HOST,5               ESTABLISH BASE REGISTER          03326000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    03327000
IN#HOST1 CLI   0(6),X'FF'              END OF TABLE?                    03328000
         BER   4                         YES - RETURN TO SQL            03329000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         03330000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    03331000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  03332000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 03333000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              03334000
         LR    15,1                    SET BOTH LENGTHS EQUAL           03335000
         MVCL  14,0                    MOVE TO HOST VARIABLES           03336000
         LA    6,8(6)                  INCREMENT TO NEXT ENTRY          03337000
         B     IN#HOST1                LOOP UNTIL END OF TABLE          03338000
*                                                                       03339000
IN#KEY   DS    0H                      MOVE KEYS INTO HOST VARIABLES    03340000
         USING IN#KEY,5                ESTABLISH BASE REGISTER          03341000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    03342000
IN#KEY1  CLI   0(6),X'FF'              END OF TABLE?                    03343000
         BER   4                         YES - RETURN TO SQL            03344000
         SLR   0,0                     CLEAR FOR INSERT                 03345000
         IC    0,6(6)                  INSERT KEY FIELD ENTRY           03346000
         NR    0,3                     FOR THE CURRENT KEY?             03347000
         BZ    IN#KEY2                   NO - GO FOR NEXT ENTRY         03348000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         03349000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    03350000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  03351000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 03352000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              03353000
         LR    15,1                    SET BOTH LENGTHS EQUAL           03354000
         MVCL  14,0                    MOVE TO HOST VARIABLES           03355000
         TM    7(6),X'80'              SIGNED DECIMAL FIELD?            03356000
         BNO   IN#KEY2                   NO - GO FOR NEXT ENTRY         03357000
         BCTR  14,0                    DECREMENT TO SIGN BYTE           03358000
         TM    0(14),X'0A'             VALID SIGN BYTE?                 03359000
         BO    IN#KEY2                   YES - GO FOR NEXT ENTRY        03360000
         TM    0(14),X'0C'             VALID SIGN BYTE?                 03361000
         BO    IN#KEY2                   YES - GO FOR NEXT ENTRY        03362000
         OI    0(14),X'0F'               NO  - MAKE IT VALID            03363000
IN#KEY2  LA    6,8(6)                  INCREMENT TO NEXT ENTRY          03364000
         B     IN#KEY1                 LOOP UNTIL END OF TABLE          03365000
*                                                                       03366000
OUT#REC  DS    0H                      MOVE HOST VARIABLES INTO RECORD  03367000
         USING OUT#REC,5               ESTABLISH BASE REGISTER          03368000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    03369000
OUT#REC1 CLI   0(6),X'FF'              END OF TABLE?                    03370000
         BER   4                         YES - RETURN TO SQL            03371000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         03372000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    03373000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  03374000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 03375000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              03376000
         LR    15,1                    SET BOTH LENGTHS EQUAL           03377000
         MVCL  0,14                    MOVE TO RECORD AREA              03378000
         LA    6,8(6)                  INCREMENT TO NEXT ENTRY          03379000
         B     OUT#REC1                LOOP UNTIL END OF TABLE          03380000
*                                                                       03381000
OUT#KEY  DS    0H                      MOVE KEYS INTO RECORD            03382000
         USING OUT#KEY,5               ESTABLISH BASE REGISTER          03383000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    03384000
OUT#KEY1 CLI   0(6),X'FF'              END OF TABLE?                    03385000
         BER   4                         YES - RETURN TO SQL            03386000
         TM    6(6),X'80'              FOR THE PRIMARY KEY?             03387000
         BZ    OUT#KEY2                  NO - GO FOR NEXT ENTRY         03388000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         03389000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    03390000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  03391000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 03392000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              03393000
         LR    15,1                    SET BOTH LENGTHS EQUAL           03394000
         MVCL  0,14                    MOVE TO RECORD AREA              03395000
OUT#KEY2 LA    6,8(6)                  INCREMENT TO NEXT ENTRY          03396000
         B     OUT#KEY1                LOOP UNTIL END OF TABLE          03397000
*                                                                       03398000
*                                                                       03399000
**********************************************************************  03400000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  03401000
*   MUST NOT BE MODIFIED.                                               03402000
**********************************************************************  03403000
*                                                                       03404000
CONV#TAB DC    H'0000'                 COBOL RECORD AREA OFFSET         03405000
         DC    H'0000'                 HOST VARIABLE AREA OFFSET        03406000
         DC    H'0026'                 LENGTH TO MOVE                   03407000
         DC    X'80'                   80 = KEY 0 FIELD                 03408000
*                                      40 = KEY 1 FIELD                 03409000
*                                      20 = KEY 2 FIELD                 03410000
*                                      10 = KEY 3 FIELD                 03411000
         DC    X'00'                   80 = SIGNED DECIMAL FIELD        03412000
*                                                                       03413000
*                                      REST OF FIELD ENTRIES            03414000
         DC    H'0026',H'0026',H'0301',X'00',X'00'                      03415000
         DC    8X'FF'                  END OF FIELD ENTRIES             03416000
         LTORG                                                          03417000
         END                                                            03418000
