**********************************************************************  00001000
*                                                                       00002000
*  S01RDB .... STATIC SQL READ MODULE                                   00003000
*                                                                       00004000
*  CREATION DATE: 04/25/16                                              00005000
*                                                                       00006000
*  FUNCTIONAL DESCRIPTION: THIS PROGRAM CONTAINS THE STATIC SQL         00007000
*  VECTORS REQUIRED TO SUPPORT I/O TO THE S01 TABLE.  IT IS LOADED      00008000
*  BY THE DB2 I/O MANAGER (BIPDB2X).  THIS PROGRAM WILL BE USED         00009000
*  IN BOTH A CICS AND BATCH ENVIRONMENT AND MUST BE RE-ENTRANT          00010000
*  AND VOID OF SVC CALLS.                                               00011000
*                                                                       00012000
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
*                                                                       00014000
**********************************************************************  00015000
* SQI COMMUNICATION AREA DSECT ADDRESSED BY REGISTER 11                 00016000
**********************************************************************  00017000
*                                                                       00018000
COM#AREA DSECT                         SQI COMMUNICATION AREA           00019000
SQW@CAF  DS    F                       ADDRESS OF CALL ATTACH FACILITY  00020000
SQW@RET  DS    F                       RETURN TO THIS SQI ADDRESS       00021000
SQWSQLCA DS    XL136                   SQLCA FOR I/O MODULE USE         00022000
         ORG   SQWSQLCA                                                 00023000
         EXEC  SQL INCLUDE SQLCA                                        00024000
SQWSQLDL DS    F                       LENGTH OF DYNAMIC SQLDSECT       00025000
SQWSQLD  DS    F                       DYNAMIC SQLDSECT                 00026000
SQW@SQRX DS    F                       SQI INTERFACE ROUTINE ADDRESS    00027000
SQWPARM1 DS    F                       SQI PARAMETER 1 (MODULE ADDRESS) 00028000
SQWINTHV DS    F                       INTEGER HOST VARIABLE            00029000
SQWCSRBR DS    F                       CURSOR ROUTINE BASE REGISTER     00030000
SQWCSRRA DS    F                       CURSOR ROUTINE RETURN ADDRESS    00031000
SQWRSFVC DS    H                       ROWSET FETCH SQLVAR COUNT        00032000
SQWKMRP  DS    CL1                     SQI KEY MASK / ROUTINE POINTER   00033000
         DS    CL5                     RESERVED                         00034000
SQWIOSI  DS    0CL24                   I/O MODULE STATE INFORMATION     00035000
SQWSQLDA DS    F                       ROWSET SQLDA STORAGE ADDRESS     00036000
SQWRSRMX DS    H                       ROWSET MAX NUMBER OF ROWS        00037000
SQWRSRCT DS    H                       ROWSET RETURNED ROW COUNT        00038000
SQWRSROC DS    H                       ROWSET ROW OCCURS COUNT          00039000
SQWRSFCD DS    H                       ROWSET FETCH SQLCODE             00040000
SQWCSRCA DS    F                       READ CURSOR CLOSE ROUTINE ADDR   00041000
SQWCSUCA DS    F                       UPDATE CURSOR CLOSE ROUTINE ADDR 00042000
SQWCSRLV DS    CL1                     CURSOR CASCADE LEVEL LIMIT       00043000
SQWCSRCC DS    CL1                     CURSOR CASCADE COUNT             00044000
SQWCSRSP DS    H                       CURSOR ROUTINE POINTER           00045000
         DS    CL4                     RESERVED                         00046000
SQWCMD   DS    CL2                     DBS COMMAND CODE                 00047000
SQWORG   DS    CL6                     RECORD ORG CODE                  00048000
ASMREC   DS    0F                      ASSEMBLER RECORD (ALIGNED)       00049000
INST     DS    CL4                                                      00050000
RECNBR   DS    CL4                                                      00051000
MODEL    DS    CL10                                                     00052000
EFFDATE  DS    CL8                                                      00053000
AUDDATE  DS    PL5'0.'                                                  00054000
AUDTIME  DS    PL5'0.'                                                  00055000
AUDUSER  DS    CL8                                                      00056000
AUDORG   DS    CL6                                                      00057000
ACTVCDE  DS    CL1                                                      00058000
SHRTDESC DS    CL20                                                     00059000
SCORE    DS    CL3                                                      00060000
AUENROLL DS    CL1                                                      00061000
INCTVBAL DS    CL1                                                      00062000
PROCOPT  DS    CL1                                                      00063000
CBALOPT  DS    CL1                                                      00064000
EXPFREQ  DS    CL1                                                      00065000
EXPTERM  DS    PL2'0.'                                                  00066000
ENDMNTH  DS    CL1                                                      00067000
CLSD     DS    PL5'0.'                                                  00068000
EXPRDATE DS    PL5'0.'                                                  00069000
EXRETDAY DS    PL2'0.'                                                  00070000
FIRSTRET DS    PL2'0.'                                                  00071000
DAYSEXT  DS    PL2'0.'                                                  00072000
EXTN     DS    CL2                                                      00073000
CASHOPT  DS    CL1                                                      00074000
CASHPCT  DS    PL5'0.000000000'                                         00075000
CASHBAL  DS    CL1                                                      00076000
CASHFREQ DS    CL1                                                      00077000
CASHTERM DS    PL2'0.'                                                  00078000
CASHDAY  DS    CL2                                                      00079000
NBRACCT  DS    CL2                                                      00080000
ACCTQLFY DS    CL1                                                      00081000
NBRPROD  DS    CL2                                                      00082000
PRODQLFY DS    CL1                                                      00083000
MINACCT  DS    CL2                                                      00084000
MINACCTQ DS    CL1                                                      00085000
MAXACCT  DS    CL2                                                      00086000
MAXACCTQ DS    CL1                                                      00087000
MINPROD  DS    CL2                                                      00088000
MINPRODQ DS    CL1                                                      00089000
MAXPROD  DS    CL2                                                      00090000
MAXPRODQ DS    CL1                                                      00091000
DEMOCODE DS    CL6                                                      00092000
DEMOQLFY DS    CL1                                                      00093000
CUSTCODE DS    CL6                                                      00094000
CUSTQLFY DS    CL1                                                      00095000
PBALOPT  DS    CL1                                                      00096000
PBALOPTQ DS    CL1                                                      00097000
PBALOPTI DS    CL1                                                      00098000
ACCTCDQ  DS    CL1                                                      00099000
ACCTCDI  DS    CL1                                                      00100000
ACTRELQ  DS    CL1                                                      00101000
ACINCEN  DS    CL1                                                      00102000
COMBCD1  DS    CL6                                                      00103000
CCQLFY1  DS    CL1                                                      00104000
CC1INCTV DS    CL1                                                      00105000
COMBCD2  DS    CL6                                                      00106000
CCQLFY2  DS    CL1                                                      00107000
CC2INCTV DS    CL1                                                      00108000
PRCOME1  DS    CL1                                                      00109000
PC1QLFY  DS    CL1                                                      00110000
PC1INCTV DS    CL1                                                      00111000
PRCOME2  DS    CL1                                                      00112000
PC2QLFY  DS    CL1                                                      00113000
PC2INCTV DS    CL1                                                      00114000
PGMIN1   DS    CL2                                                      00115000
PGMAX1   DS    CL2                                                      00116000
PRDGRP1Q DS    CL1                                                      00117000
PGMIN2   DS    CL2                                                      00118000
PGMAX2   DS    CL2                                                      00119000
PRDGRP2Q DS    CL1                                                      00120000
PGMIN3   DS    CL2                                                      00121000
PGMAX3   DS    CL2                                                      00122000
PRDGRP3Q DS    CL1                                                      00123000
PGMIN4   DS    CL2                                                      00124000
PGMAX4   DS    CL2                                                      00125000
PRDGRP4Q DS    CL1                                                      00126000
PGMIN5   DS    CL2                                                      00127000
PGMAX5   DS    CL2                                                      00128000
PRDGRP5Q DS    CL1                                                      00129000
INCENTOV DS    CL1                                                      00130000
DFLTMODL DS    CL10                                                     00131000
INCBALTR DS    CL1                                                      00132000
TRANPROF DS    CL6                                                      00133000
TRPINCEN DS    CL1                                                      00134000
CRWAMT   DS    PL6'0.00'                                                00135000
CRWMAXCB DS    PL8'0.00'                                                00136000
COMBCD3  DS    CL6                                                      00137000
CCQLFY3  DS    CL1                                                      00138000
CC3INCTV DS    CL1                                                      00139000
COMBCD4  DS    CL6                                                      00140000
CCQLFY4  DS    CL1                                                      00141000
CC4INCTV DS    CL1                                                      00142000
PRCOME3  DS    CL1                                                      00143000
PC3QLFY  DS    CL1                                                      00144000
PC3INCTV DS    CL1                                                      00145000
PRCOME4  DS    CL1                                                      00146000
PC4QLFY  DS    CL1                                                      00147000
PC4INCTV DS    CL1                                                      00148000
RSVFLAG1 DS    CL1                                                      00149000
CLACCOPT DS    CL1                                                      00150000
SECSVOPT DS    CL1                                                      00151000
RATEOPT  DS    CL1                                                      00152000
ENRATTP  DS    CL2                                                      00153000
MODELTYP DS    CL10                                                     00154000
LONGDESC DS    CL50                                                     00155000
PRIMCODE DS    CL6                                                      00156000
PRIMQLFY DS    CL1                                                      00157000
EXTENROL DS    CL1                                                      00158000
*                                                                       00159000
         ORG   ASMREC+(2000-L'SQWADATA) POINT TO ADDITIONAL DATA        00160000
SQWADATA DS    0CL400                  ADDITIONAL DATA PASSED TO MODULE 00161000
SQWSEGF  DS    CL102                   SEGMENTED FROM KEY VALUE         00162000
SQWSEGT  DS    CL102                   SEGMENTED TO KEY VALUE           00163000
SQWAUDIT DS    CL99                    CALLERS AUDIT DATA               00164000
         DS    CL97                    RESERVED                         00165000
*                                                                       00166000
INDVARS  DS    0H                      NULL INDICATOR VARIABLES         00167000
INDVARX  DS    0H                                                       00168000
INDVARL  EQU   INDVARX-INDVARS         NULL INDICATOR AREA LENGTH       00169000
*                                                                       00170000
*                                                                       00171000
**********************************************************************  00172000
* ROWSET SQLDA AREA ADDRESSED BY REGISTER 2                             00173000
**********************************************************************  00174000
*                                                                       00175000
SQDSQLDA DSECT                         ROWSET SQLDA AREA                00176000
*                                                                       00177000
**********************************************************************  00178000
* PROGRAM TABLE HEADER SECTION:                                         00179000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00180000
**********************************************************************  00181000
*                                                                       00182000
S01RDB   CSECT                         PROGRAM TABLE SECTION            00183000
S01RDB   AMODE ANY                                                      00184000
S01RDB   RMODE ANY                                                      00185000
         DC    CL8'S01RDB  '           PROGRAM ID                       00186000
         DC    CL1' '                                                   00187000
         DC    CL8'&SYSDATE'           ASSEMBLY DATE                    00188000
         DC    CL1' '                                                   00189000
         DC    CL5'&SYSTIME'           ASSEMBLY TIME                    00190000
         DC    CL1' '                                                   00191000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00192000
         DC    5A(0)                   RESERVED                         00193000
         DC    AL2(0)                  RESERVED                         00194000
         DC    AL2(INDVARL)            NULL INDICATOR AREA LENGTH       00195000
         DC    A(CONVTAB1)             RECORD/HOST CONVERSION TABLE     00196000
         DC    A(CONVTAB2)             SQLDA DATA TYPE/LENGTH TABLE     00197000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00198000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00199000
         DC    CL29'WWW.INFOR.COM                '                      00199001
*                                                                       00200000
**********************************************************************  00201000
* STATEMENT TABLE SECTION:                                              00202000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00203000
**********************************************************************  00204000
*                                                                       00205000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00206000
STM#TAB  AMODE ANY                                                      00207000
STM#TAB  RMODE ANY                                                      00208000
         DC    A(SELIN0)               SELECT INTO (KEY 0)              00209000
         DC    A(SELIN1)               SELECT INTO (KEY 1)              00210000
         DC    A(SELIN2)               SELECT INTO (KEY 2)              00211000
         DC    A(SELIN3)               SELECT INTO (KEY 3)              00212000
         DC    12A(0)                  UDB MODULE VECTORS               00213000
         DC    A(SELXC0)               SELECT SEQ CURSOR (KEY 0)        00214000
         DC    A(SELXC1)               SELECT SEQ CURSOR (KEY 1)        00215000
         DC    A(SELXC2)               SELECT SEQ CURSOR (KEY 2)        00216000
         DC    A(SELXC3)               SELECT SEQ CURSOR (KEY 3)        00217000
         DC    A(FETDC0)               FETCH SEQ DATA CURSOR (KEY 0)    00218000
         DC    A(FETDC1)               FETCH SEQ DATA CURSOR (KEY 1)    00219000
         DC    A(FETDC2)               FETCH SEQ DATA CURSOR (KEY 2)    00220000
         DC    A(FETDC3)               FETCH SEQ DATA CURSOR (KEY 3)    00221000
         DC    A(FETKC0)               FETCH SEQ KEY CURSOR (KEY 0)     00222000
         DC    A(FETKC1)               FETCH SEQ KEY CURSOR (KEY 1)     00223000
         DC    A(FETKC2)               FETCH SEQ KEY CURSOR (KEY 2)     00224000
         DC    A(FETKC3)               FETCH SEQ KEY CURSOR (KEY 3)     00225000
         DC    A(CLSXC0)               CLOSE SEQ CURSOR (KEY 0)         00226000
         DC    A(CLSXC1)               CLOSE SEQ CURSOR (KEY 1)         00227000
         DC    A(CLSXC2)               CLOSE SEQ CURSOR (KEY 2)         00228000
         DC    A(CLSXC3)               CLOSE SEQ CURSOR (KEY 3)         00229000
         DC    A(SELKY0)               SELECT KEY (KEY 0)               00230000
         DC    A(SELKY1)               SELECT KEY (KEY 1)               00231000
         DC    A(SELKY2)               SELECT KEY (KEY 2)               00232000
         DC    A(SELKY3)               SELECT KEY (KEY 3)               00233000
         DC    10A(0)                  UDB MODULE VECTORS               00234000
         DC    4X'FF'                                                   00235000
*                                                                       00236000
**********************************************************************  00237000
* SQL STATEMENT SECTION:                                                00238000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00239000
*     TO SUPPORT THIS TABLE.                                            00240000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00241000
*     ARE EQUIVALENT.                                                   00242000
**********************************************************************  00243000
*                                                                       00244000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00245000
SQL#STMT AMODE ANY                                                      00246000
SQL#STMT RMODE ANY                                                      00247000
         USING SQDSQLDA,2              ADDRESS ROWSET SQLDA AREA        00248000
         USING SQLDSECT,10,3,4         ADDRESS SQLDSECT                 00249000
         USING COM#AREA,11             ADDRESS COMMAREA                 00250000
*                                                                       00251000
**********************************************************************  00252000
* SELECT INTO STATEMENT BY PRIMARY KEY:                                 00253000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00254000
**********************************************************************  00255000
*                                                                       00256000
SELIN0   DS    0H                                                       00257000
         USING SELIN0,12,8             ESTABLISH BASE REGISTER          00258000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00259000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00260000
         BALR  14,15                   MOVE REQUESTED DATA              00261000
         B     *+6                     BRANCH AROUND ADCON              00262000
BASIN0   DC    AL2(4096)                                                00263000
         LR    8,12                    LOAD SECOND BASE                 00264000
         AH    8,BASIN0                ADD 4K                           00265000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00266000
         AH    3,BASIN0                ADD 4K                           00267000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00268000
         AH    4,BASIN0                ADD 4K                           00269000
         EXEC  SQL SELECT                                              *00270000
                   AUDIT_DATE,                                         *00271000
                   AUDIT_TIME,                                         *00272000
                   AUDIT_USER,                                         *00273000
                   AUDIT_ORG,                                          *00274000
                   ACTIVE_CODE,                                        *00275000
                   SHORT_DESC,                                         *00276000
                   SCORE,                                              *00277000
                   AUTO_ENROLL,                                        *00278000
                   INCEN_BAL_OPT,                                      *00279000
                   PROCESS_OPTION,                                     *00280000
                   CMB_BAL_OPT,                                        *00281000
                   EXPIRE_FREQ,                                        *00282000
                   EXPIRE_TERM,                                        *00283000
                   END_MONTH,                                          *00284000
                   CLOSE_DATE,                                         *00285000
                   EXPIRE_DATE,                                        *00286000
                   EXPIRE_RET_DAYS,                                    *00287000
                   FIRST_RETENTION,                                    *00288000
                   DAY_PER_EXTN,                                       *00289000
                   EXTENSION,                                          *00290000
                   CASH_OPT,                                           *00291000
                   CASH_PCT,                                           *00292000
                   CASH_BAL_OPT,                                       *00293000
                   CASH_FREQ,                                          *00294000
                   CASH_TERM,                                          *00295000
                   CASH_DAY,                                           *00296000
                   NBR_ACCOUNTS,                                       *00297000
                   ACCT_QLFY,                                          *00298000
                   NBR_PROD,                                           *00299000
                   PROD_QLFY,                                          *00300000
                   MIN_ACCT,                                           *00301000
                   MIN_ACCT_Q,                                         *00302000
                   MAX_ACCT,                                           *00303000
                   MAX_ACCT_Q,                                         *00304000
                   MIN_PROD,                                           *00305000
                   MIN_PROD_Q,                                         *00306000
                   MAX_PROD,                                           *00307000
                   MAX_PROD_Q,                                         *00308000
                   DEMOGR_CODE,                                        *00309000
                   DEMOGR_QLFY,                                        *00310000
                   CUST_REL_CODE,                                      *00311000
                   CUST_QLFY,                                          *00312000
                   PRIM_BAL_OPT,                                       *00313000
                   PRIM_BAL_OPT_Q,                                     *00314000
                   PRIM_BAL_OPT_I,                                     *00315000
                   ACCT_REL_CD_Q,                                      *00316000
                   ACCT_REL_CD_I,                                      *00317000
                   ACCT_REL_Q,                                         *00318000
                   ACCT_REL_INCEN,                                     *00319000
                   CMB_CATG_CD_1,                                      *00320000
                   CMB_CATG_1_Q,                                       *00321000
                   CMB_CATG_1_INC,                                     *00322000
                   CMB_CATG_CD_2,                                      *00323000
                   CMB_CATG_2_Q,                                       *00324000
                   CMB_CATG_2_INC,                                     *00325000
                   PRIME_CMB_OPT_1,                                    *00326000
                   PRIME_C_1_QLFY,                                     *00327000
                   PRIME_C_1_INCEN,                                    *00328000
                   PRIME_CMB_OPT_2,                                    *00329000
                   PRIME_C_2_QLFY,                                     *00330000
                   PRIME_C_2_INCEN,                                    *00331000
                   PROD_GRP_MIN_1,                                     *00332000
                   PROD_GRP_MAX_1,                                     *00333000
                   PROD_GRP_1_Q,                                       *00334000
                   PROD_GRP_MIN_2,                                     *00335000
                   PROD_GRP_MAX_2,                                     *00336000
                   PROD_GRP_2_Q,                                       *00337000
                   PROD_GRP_MIN_3,                                     *00338000
                   PROD_GRP_MAX_3,                                     *00339000
                   PROD_GRP_3_Q,                                       *00340000
                   PROD_GRP_MIN_4,                                     *00341000
                   PROD_GRP_MAX_4,                                     *00342000
                   PROD_GRP_4_Q,                                       *00343000
                   PROD_GRP_MIN_5,                                     *00344000
                   PROD_GRP_MAX_5,                                     *00345000
                   PROD_GRP_5_Q,                                       *00346000
                   ACCT_INCEN_OVRD,                                    *00347000
                   DEFAULT_MODEL,                                      *00348000
                   INCEN_BAL_TRN,                                      *00349000
                   TRAN_PROFILE,                                       *00350000
                   TRAN_INCENTIVE,                                     *00351000
                   CASH_MAX_AMT_PD,                                    *00352000
                   CASH_MAX_BAL,                                       *00353000
                   CMB_CATG_CD_3,                                      *00354000
                   CMB_CATG_3_Q,                                       *00355000
                   CMB_CATG_3_INC,                                     *00356000
                   CMB_CATG_CD_4,                                      *00357000
                   CMB_CATG_4_Q,                                       *00358000
                   CMB_CATG_4_INC,                                     *00359000
                   PRIME_CMB_OPT_3,                                    *00360000
                   PRIME_C_3_QLFY,                                     *00361000
                   PRIME_C_3_INCEN,                                    *00362000
                   PRIME_CMB_OPT_4,                                    *00363000
                   PRIME_C_4_QLFY,                                     *00364000
                   PRIME_C_4_INCEN,                                    *00365000
                   RSV_FLAG_1,                                         *00366000
                   CLOSED_ACCT_OPT,                                    *00367000
                   SEC_SVC_OPT,                                        *00368000
                   RATE_OPTION,                                        *00369000
                   ENROLL_LEFT,                                        *00370000
                   MODEL_TYPE,                                         *00371000
                   LONG_DESC,                                          *00372000
                   PRIM_REL_CODE,                                      *00373000
                   PRIM_QLFY,                                          *00374000
                   EXTL_ENROLL                                         *00375000
                 INTO                                                  *00376000
                   :AUDDATE,                                           *00377000
                   :AUDTIME,                                           *00378000
                   :AUDUSER,                                           *00379000
                   :AUDORG,                                            *00380000
                   :ACTVCDE,                                           *00381000
                   :SHRTDESC,                                          *00382000
                   :SCORE,                                             *00383000
                   :AUENROLL,                                          *00384000
                   :INCTVBAL,                                          *00385000
                   :PROCOPT,                                           *00386000
                   :CBALOPT,                                           *00387000
                   :EXPFREQ,                                           *00388000
                   :EXPTERM,                                           *00389000
                   :ENDMNTH,                                           *00390000
                   :CLSD,                                              *00391000
                   :EXPRDATE,                                          *00392000
                   :EXRETDAY,                                          *00393000
                   :FIRSTRET,                                          *00394000
                   :DAYSEXT,                                           *00395000
                   :EXTN,                                              *00396000
                   :CASHOPT,                                           *00397000
                   :CASHPCT,                                           *00398000
                   :CASHBAL,                                           *00399000
                   :CASHFREQ,                                          *00400000
                   :CASHTERM,                                          *00401000
                   :CASHDAY,                                           *00402000
                   :NBRACCT,                                           *00403000
                   :ACCTQLFY,                                          *00404000
                   :NBRPROD,                                           *00405000
                   :PRODQLFY,                                          *00406000
                   :MINACCT,                                           *00407000
                   :MINACCTQ,                                          *00408000
                   :MAXACCT,                                           *00409000
                   :MAXACCTQ,                                          *00410000
                   :MINPROD,                                           *00411000
                   :MINPRODQ,                                          *00412000
                   :MAXPROD,                                           *00413000
                   :MAXPRODQ,                                          *00414000
                   :DEMOCODE,                                          *00415000
                   :DEMOQLFY,                                          *00416000
                   :CUSTCODE,                                          *00417000
                   :CUSTQLFY,                                          *00418000
                   :PBALOPT,                                           *00419000
                   :PBALOPTQ,                                          *00420000
                   :PBALOPTI,                                          *00421000
                   :ACCTCDQ,                                           *00422000
                   :ACCTCDI,                                           *00423000
                   :ACTRELQ,                                           *00424000
                   :ACINCEN,                                           *00425000
                   :COMBCD1,                                           *00426000
                   :CCQLFY1,                                           *00427000
                   :CC1INCTV,                                          *00428000
                   :COMBCD2,                                           *00429000
                   :CCQLFY2,                                           *00430000
                   :CC2INCTV,                                          *00431000
                   :PRCOME1,                                           *00432000
                   :PC1QLFY,                                           *00433000
                   :PC1INCTV,                                          *00434000
                   :PRCOME2,                                           *00435000
                   :PC2QLFY,                                           *00436000
                   :PC2INCTV,                                          *00437000
                   :PGMIN1,                                            *00438000
                   :PGMAX1,                                            *00439000
                   :PRDGRP1Q,                                          *00440000
                   :PGMIN2,                                            *00441000
                   :PGMAX2,                                            *00442000
                   :PRDGRP2Q,                                          *00443000
                   :PGMIN3,                                            *00444000
                   :PGMAX3,                                            *00445000
                   :PRDGRP3Q,                                          *00446000
                   :PGMIN4,                                            *00447000
                   :PGMAX4,                                            *00448000
                   :PRDGRP4Q,                                          *00449000
                   :PGMIN5,                                            *00450000
                   :PGMAX5,                                            *00451000
                   :PRDGRP5Q,                                          *00452000
                   :INCENTOV,                                          *00453000
                   :DFLTMODL,                                          *00454000
                   :INCBALTR,                                          *00455000
                   :TRANPROF,                                          *00456000
                   :TRPINCEN,                                          *00457000
                   :CRWAMT,                                            *00458000
                   :CRWMAXCB,                                          *00459000
                   :COMBCD3,                                           *00460000
                   :CCQLFY3,                                           *00461000
                   :CC3INCTV,                                          *00462000
                   :COMBCD4,                                           *00463000
                   :CCQLFY4,                                           *00464000
                   :CC4INCTV,                                          *00465000
                   :PRCOME3,                                           *00466000
                   :PC3QLFY,                                           *00467000
                   :PC3INCTV,                                          *00468000
                   :PRCOME4,                                           *00469000
                   :PC4QLFY,                                           *00470000
                   :PC4INCTV,                                          *00471000
                   :RSVFLAG1,                                          *00472000
                   :CLACCOPT,                                          *00473000
                   :SECSVOPT,                                          *00474000
                   :RATEOPT,                                           *00475000
                   :ENRATTP,                                           *00476000
                   :MODELTYP,                                          *00477000
                   :LONGDESC,                                          *00478000
                   :PRIMCODE,                                          *00479000
                   :PRIMQLFY,                                          *00480000
                   :EXTENROL                                           *00481000
                 FROM S01                                              *00482000
                 WHERE                                                 *00483000
                   INST_NBR = :INST AND                                *00484000
                   RECORD_NBR = :RECNBR AND                            *00485000
                   MODEL = :MODEL AND                                  *00486000
                   EFFECTIVE_DATE = :EFFDATE                           *00487000
                 FETCH FIRST 1 ROW ONLY                                 00488000
         MVI   SQWKMRP,X'03'           MOVE HOST VARIABLES TO RECORD    00489000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00490000
         BALR  14,15                   MOVE REQUESTED DATA              00491000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00492000
         BR    14                      RETURN TO CALLER                 00493000
         LTORG                                                          00494000
*                                                                       00495000
**********************************************************************  00496000
* SELECT COUNT STATEMENT BY PRIMARY KEY:                                00497000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            00498000
**********************************************************************  00499000
*                                                                       00500000
SELKY0   DS    0H                                                       00501000
         USING SELKY0,12,8             ESTABLISH BASE REGISTER          00502000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00503000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00504000
         BALR  14,15                   MOVE REQUESTED DATA              00505000
         B     *+6                     BRANCH AROUND ADCON              00506000
BASKY0   DC    AL2(4096)                                                00507000
         LR    8,12                    LOAD SECOND BASE                 00508000
         AH    8,BASKY0                ADD 4K                           00509000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00510000
         AH    3,BASKY0                ADD 4K                           00511000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00512000
         AH    4,BASKY0                ADD 4K                           00513000
         EXEC  SQL SELECT                                              *00514000
                   COUNT(*)                                            *00515000
                 INTO                                                  *00516000
                   :SQWINTHV                                           *00517000
                 FROM S01                                              *00518000
                 WHERE                                                 *00519000
                   INST_NBR = :INST AND                                *00520000
                   RECORD_NBR = :RECNBR AND                            *00521000
                   MODEL = :MODEL AND                                  *00522000
                   EFFECTIVE_DATE = :EFFDATE                           *00523000
                 FETCH FIRST 1 ROW ONLY                                 00524000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00525000
         CLC   SQLCODE,=F'0'           GOOD RETURN FROM SQL?            00526000
         BNER  14                      NO - RETURN TO CALLER            00527000
         CLC   SQWINTHV,=F'0'          ANY ROWS MATCH WHERE CLAUSE?     00528000
         BNER  14                      YES - RETURN ZERO SQLCODE        00529000
         MVC   SQLCODE,=F'+100'        SET SQLCODE TO ROW NOT FOUND     00530000
         BR    14                      RETURN TO CALLER                 00531000
         LTORG                                                          00532000
*                                                                       00533000
**********************************************************************  00534000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY    00535000
* KEY:                                                                  00536000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00537000
*     AND GET-NEXT-LOCK VERBS.                                          00538000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00539000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00540000
**********************************************************************  00541000
*                                                                       00542000
SELXC0   DS    0H                                                       00543000
         USING SELXC0,12,8             ESTABLISH BASE REGISTER          00544000
         MVI   SQWKMRP,X'86'           SET HOST KEY 0 & CURSOR POINTER  00545000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00546000
         BALR  14,15                   MOVE DATA & SET CURSOR POINTER   00547000
         LA    12,SELXC0P              LOAD VECTOR TABLE ADDRESS        00548000
         AH    12,SQWCSRSP             COMPUTE POINTER TO OPEN ROUTINE  00549000
         L     12,0(12)                LOAD OPEN ROUTINE ADDRESS        00550000
         BR    12                      GO TO CURSOR OPEN ROUTINE        00551000
SELXC0P  DC    A(SELGE001)                                              00552000
         DC    (KY0COLMS-01)A(0)                                        00553000
         DC    A(SELGE002)                                              00554000
         DC    (KY0COLMS-02)A(0)                                        00555000
         DC    A(SELGE003)                                              00556000
         DC    (KY0COLMS-03)A(0)                                        00557000
         DC    A(SELGE004)                                              00558000
         LTORG                                                          00559000
*                                                                       00560000
SELGE001 DS    0H                                                       00561000
         USING SELGE001,12,8           ESTABLISH BASE REGISTER          00562000
         B     *+6                     BRANCH AROUND ADCON              00563000
BASGE001 DC    AL2(4096)                                                00564000
         LR    8,12                    LOAD SECOND BASE                 00565000
         AH    8,BASGE001              ADD 4K                           00566000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00567000
         AH    3,BASGE001              ADD 4K                           00568000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00569000
         AH    4,BASGE001              ADD 4K                           00570000
         EXEC  SQL DECLARE S01GE001 CURSOR                             *00571000
               WITH ROWSET POSITIONING                                 *00572000
               FOR SELECT                                              *00573000
                   INST_NBR,                                           *00574000
                   RECORD_NBR,                                         *00575000
                   MODEL,                                              *00576000
                   EFFECTIVE_DATE,                                     *00577000
                   AUDIT_DATE,                                         *00578000
                   AUDIT_TIME,                                         *00579000
                   AUDIT_USER,                                         *00580000
                   AUDIT_ORG,                                          *00581000
                   ACTIVE_CODE,                                        *00582000
                   SHORT_DESC,                                         *00583000
                   SCORE,                                              *00584000
                   AUTO_ENROLL,                                        *00585000
                   INCEN_BAL_OPT,                                      *00586000
                   PROCESS_OPTION,                                     *00587000
                   CMB_BAL_OPT,                                        *00588000
                   EXPIRE_FREQ,                                        *00589000
                   EXPIRE_TERM,                                        *00590000
                   END_MONTH,                                          *00591000
                   CLOSE_DATE,                                         *00592000
                   EXPIRE_DATE,                                        *00593000
                   EXPIRE_RET_DAYS,                                    *00594000
                   FIRST_RETENTION,                                    *00595000
                   DAY_PER_EXTN,                                       *00596000
                   EXTENSION,                                          *00597000
                   CASH_OPT,                                           *00598000
                   CASH_PCT,                                           *00599000
                   CASH_BAL_OPT,                                       *00600000
                   CASH_FREQ,                                          *00601000
                   CASH_TERM,                                          *00602000
                   CASH_DAY,                                           *00603000
                   NBR_ACCOUNTS,                                       *00604000
                   ACCT_QLFY,                                          *00605000
                   NBR_PROD,                                           *00606000
                   PROD_QLFY,                                          *00607000
                   MIN_ACCT,                                           *00608000
                   MIN_ACCT_Q,                                         *00609000
                   MAX_ACCT,                                           *00610000
                   MAX_ACCT_Q,                                         *00611000
                   MIN_PROD,                                           *00612000
                   MIN_PROD_Q,                                         *00613000
                   MAX_PROD,                                           *00614000
                   MAX_PROD_Q,                                         *00615000
                   DEMOGR_CODE,                                        *00616000
                   DEMOGR_QLFY,                                        *00617000
                   CUST_REL_CODE,                                      *00618000
                   CUST_QLFY,                                          *00619000
                   PRIM_BAL_OPT,                                       *00620000
                   PRIM_BAL_OPT_Q,                                     *00621000
                   PRIM_BAL_OPT_I,                                     *00622000
                   ACCT_REL_CD_Q,                                      *00623000
                   ACCT_REL_CD_I,                                      *00624000
                   ACCT_REL_Q,                                         *00625000
                   ACCT_REL_INCEN,                                     *00626000
                   CMB_CATG_CD_1,                                      *00627000
                   CMB_CATG_1_Q,                                       *00628000
                   CMB_CATG_1_INC,                                     *00629000
                   CMB_CATG_CD_2,                                      *00630000
                   CMB_CATG_2_Q,                                       *00631000
                   CMB_CATG_2_INC,                                     *00632000
                   PRIME_CMB_OPT_1,                                    *00633000
                   PRIME_C_1_QLFY,                                     *00634000
                   PRIME_C_1_INCEN,                                    *00635000
                   PRIME_CMB_OPT_2,                                    *00636000
                   PRIME_C_2_QLFY,                                     *00637000
                   PRIME_C_2_INCEN,                                    *00638000
                   PROD_GRP_MIN_1,                                     *00639000
                   PROD_GRP_MAX_1,                                     *00640000
                   PROD_GRP_1_Q,                                       *00641000
                   PROD_GRP_MIN_2,                                     *00642000
                   PROD_GRP_MAX_2,                                     *00643000
                   PROD_GRP_2_Q,                                       *00644000
                   PROD_GRP_MIN_3,                                     *00645000
                   PROD_GRP_MAX_3,                                     *00646000
                   PROD_GRP_3_Q,                                       *00647000
                   PROD_GRP_MIN_4,                                     *00648000
                   PROD_GRP_MAX_4,                                     *00649000
                   PROD_GRP_4_Q,                                       *00650000
                   PROD_GRP_MIN_5,                                     *00651000
                   PROD_GRP_MAX_5,                                     *00652000
                   PROD_GRP_5_Q,                                       *00653000
                   ACCT_INCEN_OVRD,                                    *00654000
                   DEFAULT_MODEL,                                      *00655000
                   INCEN_BAL_TRN,                                      *00656000
                   TRAN_PROFILE,                                       *00657000
                   TRAN_INCENTIVE,                                     *00658000
                   CASH_MAX_AMT_PD,                                    *00659000
                   CASH_MAX_BAL,                                       *00660000
                   CMB_CATG_CD_3,                                      *00661000
                   CMB_CATG_3_Q,                                       *00662000
                   CMB_CATG_3_INC,                                     *00663000
                   CMB_CATG_CD_4,                                      *00664000
                   CMB_CATG_4_Q,                                       *00665000
                   CMB_CATG_4_INC,                                     *00666000
                   PRIME_CMB_OPT_3,                                    *00667000
                   PRIME_C_3_QLFY,                                     *00668000
                   PRIME_C_3_INCEN,                                    *00669000
                   PRIME_CMB_OPT_4,                                    *00670000
                   PRIME_C_4_QLFY,                                     *00671000
                   PRIME_C_4_INCEN,                                    *00672000
                   RSV_FLAG_1,                                         *00673000
                   CLOSED_ACCT_OPT,                                    *00674000
                   SEC_SVC_OPT,                                        *00675000
                   RATE_OPTION,                                        *00676000
                   ENROLL_LEFT,                                        *00677000
                   MODEL_TYPE,                                         *00678000
                   LONG_DESC,                                          *00679000
                   PRIM_REL_CODE,                                      *00680000
                   PRIM_QLFY,                                          *00681000
                   EXTL_ENROLL                                         *00682000
                 FROM S01                                              *00683000
                 WHERE                                                 *00684000
                   INST_NBR = :INST AND                                *00685000
                   RECORD_NBR = :RECNBR AND                            *00686000
                   MODEL = :MODEL AND                                  *00687000
                   EFFECTIVE_DATE >=                                   *00688000
                     :EFFDATE                                          *00689000
                 ORDER BY EFFECTIVE_DATE ASC                           *00690000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00691000
         EXEC  SQL OPEN S01GE001                                        00692000
         MVC   SQWCSRCA,=A(CLSGE001)   SET CURSOR CLOSE ROUTINE ADDRESS 00693000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00694000
         BR    14                      RETURN TO CALLER                 00695000
         LTORG                                                          00696000
*                                                                       00697000
SELGE002 DS    0H                                                       00698000
         USING SELGE002,12,8           ESTABLISH BASE REGISTER          00699000
         B     *+6                     BRANCH AROUND ADCON              00700000
BASGE002 DC    AL2(4096)                                                00701000
         LR    8,12                    LOAD SECOND BASE                 00702000
         AH    8,BASGE002              ADD 4K                           00703000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00704000
         AH    3,BASGE002              ADD 4K                           00705000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00706000
         AH    4,BASGE002              ADD 4K                           00707000
         EXEC  SQL DECLARE S01GE002 CURSOR                             *00708000
               WITH ROWSET POSITIONING                                 *00709000
               FOR SELECT                                              *00710000
                   INST_NBR,                                           *00711000
                   RECORD_NBR,                                         *00712000
                   MODEL,                                              *00713000
                   EFFECTIVE_DATE,                                     *00714000
                   AUDIT_DATE,                                         *00715000
                   AUDIT_TIME,                                         *00716000
                   AUDIT_USER,                                         *00717000
                   AUDIT_ORG,                                          *00718000
                   ACTIVE_CODE,                                        *00719000
                   SHORT_DESC,                                         *00720000
                   SCORE,                                              *00721000
                   AUTO_ENROLL,                                        *00722000
                   INCEN_BAL_OPT,                                      *00723000
                   PROCESS_OPTION,                                     *00724000
                   CMB_BAL_OPT,                                        *00725000
                   EXPIRE_FREQ,                                        *00726000
                   EXPIRE_TERM,                                        *00727000
                   END_MONTH,                                          *00728000
                   CLOSE_DATE,                                         *00729000
                   EXPIRE_DATE,                                        *00730000
                   EXPIRE_RET_DAYS,                                    *00731000
                   FIRST_RETENTION,                                    *00732000
                   DAY_PER_EXTN,                                       *00733000
                   EXTENSION,                                          *00734000
                   CASH_OPT,                                           *00735000
                   CASH_PCT,                                           *00736000
                   CASH_BAL_OPT,                                       *00737000
                   CASH_FREQ,                                          *00738000
                   CASH_TERM,                                          *00739000
                   CASH_DAY,                                           *00740000
                   NBR_ACCOUNTS,                                       *00741000
                   ACCT_QLFY,                                          *00742000
                   NBR_PROD,                                           *00743000
                   PROD_QLFY,                                          *00744000
                   MIN_ACCT,                                           *00745000
                   MIN_ACCT_Q,                                         *00746000
                   MAX_ACCT,                                           *00747000
                   MAX_ACCT_Q,                                         *00748000
                   MIN_PROD,                                           *00749000
                   MIN_PROD_Q,                                         *00750000
                   MAX_PROD,                                           *00751000
                   MAX_PROD_Q,                                         *00752000
                   DEMOGR_CODE,                                        *00753000
                   DEMOGR_QLFY,                                        *00754000
                   CUST_REL_CODE,                                      *00755000
                   CUST_QLFY,                                          *00756000
                   PRIM_BAL_OPT,                                       *00757000
                   PRIM_BAL_OPT_Q,                                     *00758000
                   PRIM_BAL_OPT_I,                                     *00759000
                   ACCT_REL_CD_Q,                                      *00760000
                   ACCT_REL_CD_I,                                      *00761000
                   ACCT_REL_Q,                                         *00762000
                   ACCT_REL_INCEN,                                     *00763000
                   CMB_CATG_CD_1,                                      *00764000
                   CMB_CATG_1_Q,                                       *00765000
                   CMB_CATG_1_INC,                                     *00766000
                   CMB_CATG_CD_2,                                      *00767000
                   CMB_CATG_2_Q,                                       *00768000
                   CMB_CATG_2_INC,                                     *00769000
                   PRIME_CMB_OPT_1,                                    *00770000
                   PRIME_C_1_QLFY,                                     *00771000
                   PRIME_C_1_INCEN,                                    *00772000
                   PRIME_CMB_OPT_2,                                    *00773000
                   PRIME_C_2_QLFY,                                     *00774000
                   PRIME_C_2_INCEN,                                    *00775000
                   PROD_GRP_MIN_1,                                     *00776000
                   PROD_GRP_MAX_1,                                     *00777000
                   PROD_GRP_1_Q,                                       *00778000
                   PROD_GRP_MIN_2,                                     *00779000
                   PROD_GRP_MAX_2,                                     *00780000
                   PROD_GRP_2_Q,                                       *00781000
                   PROD_GRP_MIN_3,                                     *00782000
                   PROD_GRP_MAX_3,                                     *00783000
                   PROD_GRP_3_Q,                                       *00784000
                   PROD_GRP_MIN_4,                                     *00785000
                   PROD_GRP_MAX_4,                                     *00786000
                   PROD_GRP_4_Q,                                       *00787000
                   PROD_GRP_MIN_5,                                     *00788000
                   PROD_GRP_MAX_5,                                     *00789000
                   PROD_GRP_5_Q,                                       *00790000
                   ACCT_INCEN_OVRD,                                    *00791000
                   DEFAULT_MODEL,                                      *00792000
                   INCEN_BAL_TRN,                                      *00793000
                   TRAN_PROFILE,                                       *00794000
                   TRAN_INCENTIVE,                                     *00795000
                   CASH_MAX_AMT_PD,                                    *00796000
                   CASH_MAX_BAL,                                       *00797000
                   CMB_CATG_CD_3,                                      *00798000
                   CMB_CATG_3_Q,                                       *00799000
                   CMB_CATG_3_INC,                                     *00800000
                   CMB_CATG_CD_4,                                      *00801000
                   CMB_CATG_4_Q,                                       *00802000
                   CMB_CATG_4_INC,                                     *00803000
                   PRIME_CMB_OPT_3,                                    *00804000
                   PRIME_C_3_QLFY,                                     *00805000
                   PRIME_C_3_INCEN,                                    *00806000
                   PRIME_CMB_OPT_4,                                    *00807000
                   PRIME_C_4_QLFY,                                     *00808000
                   PRIME_C_4_INCEN,                                    *00809000
                   RSV_FLAG_1,                                         *00810000
                   CLOSED_ACCT_OPT,                                    *00811000
                   SEC_SVC_OPT,                                        *00812000
                   RATE_OPTION,                                        *00813000
                   ENROLL_LEFT,                                        *00814000
                   MODEL_TYPE,                                         *00815000
                   LONG_DESC,                                          *00816000
                   PRIM_REL_CODE,                                      *00817000
                   PRIM_QLFY,                                          *00818000
                   EXTL_ENROLL                                         *00819000
                 FROM S01                                              *00820000
                 WHERE                                                 *00821000
                   INST_NBR = :INST AND                                *00822000
                   RECORD_NBR = :RECNBR AND                            *00823000
                   MODEL >=                                            *00824000
                     :MODEL                                            *00825000
                 ORDER BY MODEL ASC,                                   *00826000
                   EFFECTIVE_DATE ASC                                  *00827000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00828000
         EXEC  SQL OPEN S01GE002                                        00829000
         MVC   SQWCSRCA,=A(CLSGE002)   SET CURSOR CLOSE ROUTINE ADDRESS 00830000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00831000
         BR    14                      RETURN TO CALLER                 00832000
         LTORG                                                          00833000
*                                                                       00834000
SELGE003 DS    0H                                                       00835000
         USING SELGE003,12,8           ESTABLISH BASE REGISTER          00836000
         B     *+6                     BRANCH AROUND ADCON              00837000
BASGE003 DC    AL2(4096)                                                00838000
         LR    8,12                    LOAD SECOND BASE                 00839000
         AH    8,BASGE003              ADD 4K                           00840000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00841000
         AH    3,BASGE003              ADD 4K                           00842000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00843000
         AH    4,BASGE003              ADD 4K                           00844000
         EXEC  SQL DECLARE S01GE003 CURSOR                             *00845000
               WITH ROWSET POSITIONING                                 *00846000
               FOR SELECT                                              *00847000
                   INST_NBR,                                           *00848000
                   RECORD_NBR,                                         *00849000
                   MODEL,                                              *00850000
                   EFFECTIVE_DATE,                                     *00851000
                   AUDIT_DATE,                                         *00852000
                   AUDIT_TIME,                                         *00853000
                   AUDIT_USER,                                         *00854000
                   AUDIT_ORG,                                          *00855000
                   ACTIVE_CODE,                                        *00856000
                   SHORT_DESC,                                         *00857000
                   SCORE,                                              *00858000
                   AUTO_ENROLL,                                        *00859000
                   INCEN_BAL_OPT,                                      *00860000
                   PROCESS_OPTION,                                     *00861000
                   CMB_BAL_OPT,                                        *00862000
                   EXPIRE_FREQ,                                        *00863000
                   EXPIRE_TERM,                                        *00864000
                   END_MONTH,                                          *00865000
                   CLOSE_DATE,                                         *00866000
                   EXPIRE_DATE,                                        *00867000
                   EXPIRE_RET_DAYS,                                    *00868000
                   FIRST_RETENTION,                                    *00869000
                   DAY_PER_EXTN,                                       *00870000
                   EXTENSION,                                          *00871000
                   CASH_OPT,                                           *00872000
                   CASH_PCT,                                           *00873000
                   CASH_BAL_OPT,                                       *00874000
                   CASH_FREQ,                                          *00875000
                   CASH_TERM,                                          *00876000
                   CASH_DAY,                                           *00877000
                   NBR_ACCOUNTS,                                       *00878000
                   ACCT_QLFY,                                          *00879000
                   NBR_PROD,                                           *00880000
                   PROD_QLFY,                                          *00881000
                   MIN_ACCT,                                           *00882000
                   MIN_ACCT_Q,                                         *00883000
                   MAX_ACCT,                                           *00884000
                   MAX_ACCT_Q,                                         *00885000
                   MIN_PROD,                                           *00886000
                   MIN_PROD_Q,                                         *00887000
                   MAX_PROD,                                           *00888000
                   MAX_PROD_Q,                                         *00889000
                   DEMOGR_CODE,                                        *00890000
                   DEMOGR_QLFY,                                        *00891000
                   CUST_REL_CODE,                                      *00892000
                   CUST_QLFY,                                          *00893000
                   PRIM_BAL_OPT,                                       *00894000
                   PRIM_BAL_OPT_Q,                                     *00895000
                   PRIM_BAL_OPT_I,                                     *00896000
                   ACCT_REL_CD_Q,                                      *00897000
                   ACCT_REL_CD_I,                                      *00898000
                   ACCT_REL_Q,                                         *00899000
                   ACCT_REL_INCEN,                                     *00900000
                   CMB_CATG_CD_1,                                      *00901000
                   CMB_CATG_1_Q,                                       *00902000
                   CMB_CATG_1_INC,                                     *00903000
                   CMB_CATG_CD_2,                                      *00904000
                   CMB_CATG_2_Q,                                       *00905000
                   CMB_CATG_2_INC,                                     *00906000
                   PRIME_CMB_OPT_1,                                    *00907000
                   PRIME_C_1_QLFY,                                     *00908000
                   PRIME_C_1_INCEN,                                    *00909000
                   PRIME_CMB_OPT_2,                                    *00910000
                   PRIME_C_2_QLFY,                                     *00911000
                   PRIME_C_2_INCEN,                                    *00912000
                   PROD_GRP_MIN_1,                                     *00913000
                   PROD_GRP_MAX_1,                                     *00914000
                   PROD_GRP_1_Q,                                       *00915000
                   PROD_GRP_MIN_2,                                     *00916000
                   PROD_GRP_MAX_2,                                     *00917000
                   PROD_GRP_2_Q,                                       *00918000
                   PROD_GRP_MIN_3,                                     *00919000
                   PROD_GRP_MAX_3,                                     *00920000
                   PROD_GRP_3_Q,                                       *00921000
                   PROD_GRP_MIN_4,                                     *00922000
                   PROD_GRP_MAX_4,                                     *00923000
                   PROD_GRP_4_Q,                                       *00924000
                   PROD_GRP_MIN_5,                                     *00925000
                   PROD_GRP_MAX_5,                                     *00926000
                   PROD_GRP_5_Q,                                       *00927000
                   ACCT_INCEN_OVRD,                                    *00928000
                   DEFAULT_MODEL,                                      *00929000
                   INCEN_BAL_TRN,                                      *00930000
                   TRAN_PROFILE,                                       *00931000
                   TRAN_INCENTIVE,                                     *00932000
                   CASH_MAX_AMT_PD,                                    *00933000
                   CASH_MAX_BAL,                                       *00934000
                   CMB_CATG_CD_3,                                      *00935000
                   CMB_CATG_3_Q,                                       *00936000
                   CMB_CATG_3_INC,                                     *00937000
                   CMB_CATG_CD_4,                                      *00938000
                   CMB_CATG_4_Q,                                       *00939000
                   CMB_CATG_4_INC,                                     *00940000
                   PRIME_CMB_OPT_3,                                    *00941000
                   PRIME_C_3_QLFY,                                     *00942000
                   PRIME_C_3_INCEN,                                    *00943000
                   PRIME_CMB_OPT_4,                                    *00944000
                   PRIME_C_4_QLFY,                                     *00945000
                   PRIME_C_4_INCEN,                                    *00946000
                   RSV_FLAG_1,                                         *00947000
                   CLOSED_ACCT_OPT,                                    *00948000
                   SEC_SVC_OPT,                                        *00949000
                   RATE_OPTION,                                        *00950000
                   ENROLL_LEFT,                                        *00951000
                   MODEL_TYPE,                                         *00952000
                   LONG_DESC,                                          *00953000
                   PRIM_REL_CODE,                                      *00954000
                   PRIM_QLFY,                                          *00955000
                   EXTL_ENROLL                                         *00956000
                 FROM S01                                              *00957000
                 WHERE                                                 *00958000
                   INST_NBR = :INST AND                                *00959000
                   RECORD_NBR >=                                       *00960000
                     :RECNBR                                           *00961000
                 ORDER BY RECORD_NBR ASC,                              *00962000
                   MODEL ASC,                                          *00963000
                   EFFECTIVE_DATE ASC                                  *00964000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00965000
         EXEC  SQL OPEN S01GE003                                        00966000
         MVC   SQWCSRCA,=A(CLSGE003)   SET CURSOR CLOSE ROUTINE ADDRESS 00967000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00968000
         BR    14                      RETURN TO CALLER                 00969000
         LTORG                                                          00970000
*                                                                       00971000
SELGE004 DS    0H                                                       00972000
         USING SELGE004,12,8           ESTABLISH BASE REGISTER          00973000
         B     *+6                     BRANCH AROUND ADCON              00974000
BASGE004 DC    AL2(4096)                                                00975000
         LR    8,12                    LOAD SECOND BASE                 00976000
         AH    8,BASGE004              ADD 4K                           00977000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00978000
         AH    3,BASGE004              ADD 4K                           00979000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00980000
         AH    4,BASGE004              ADD 4K                           00981000
         EXEC  SQL DECLARE S01GE004 CURSOR                             *00982000
               WITH ROWSET POSITIONING                                 *00983000
               FOR SELECT                                              *00984000
                   INST_NBR,                                           *00985000
                   RECORD_NBR,                                         *00986000
                   MODEL,                                              *00987000
                   EFFECTIVE_DATE,                                     *00988000
                   AUDIT_DATE,                                         *00989000
                   AUDIT_TIME,                                         *00990000
                   AUDIT_USER,                                         *00991000
                   AUDIT_ORG,                                          *00992000
                   ACTIVE_CODE,                                        *00993000
                   SHORT_DESC,                                         *00994000
                   SCORE,                                              *00995000
                   AUTO_ENROLL,                                        *00996000
                   INCEN_BAL_OPT,                                      *00997000
                   PROCESS_OPTION,                                     *00998000
                   CMB_BAL_OPT,                                        *00999000
                   EXPIRE_FREQ,                                        *01000000
                   EXPIRE_TERM,                                        *01001000
                   END_MONTH,                                          *01002000
                   CLOSE_DATE,                                         *01003000
                   EXPIRE_DATE,                                        *01004000
                   EXPIRE_RET_DAYS,                                    *01005000
                   FIRST_RETENTION,                                    *01006000
                   DAY_PER_EXTN,                                       *01007000
                   EXTENSION,                                          *01008000
                   CASH_OPT,                                           *01009000
                   CASH_PCT,                                           *01010000
                   CASH_BAL_OPT,                                       *01011000
                   CASH_FREQ,                                          *01012000
                   CASH_TERM,                                          *01013000
                   CASH_DAY,                                           *01014000
                   NBR_ACCOUNTS,                                       *01015000
                   ACCT_QLFY,                                          *01016000
                   NBR_PROD,                                           *01017000
                   PROD_QLFY,                                          *01018000
                   MIN_ACCT,                                           *01019000
                   MIN_ACCT_Q,                                         *01020000
                   MAX_ACCT,                                           *01021000
                   MAX_ACCT_Q,                                         *01022000
                   MIN_PROD,                                           *01023000
                   MIN_PROD_Q,                                         *01024000
                   MAX_PROD,                                           *01025000
                   MAX_PROD_Q,                                         *01026000
                   DEMOGR_CODE,                                        *01027000
                   DEMOGR_QLFY,                                        *01028000
                   CUST_REL_CODE,                                      *01029000
                   CUST_QLFY,                                          *01030000
                   PRIM_BAL_OPT,                                       *01031000
                   PRIM_BAL_OPT_Q,                                     *01032000
                   PRIM_BAL_OPT_I,                                     *01033000
                   ACCT_REL_CD_Q,                                      *01034000
                   ACCT_REL_CD_I,                                      *01035000
                   ACCT_REL_Q,                                         *01036000
                   ACCT_REL_INCEN,                                     *01037000
                   CMB_CATG_CD_1,                                      *01038000
                   CMB_CATG_1_Q,                                       *01039000
                   CMB_CATG_1_INC,                                     *01040000
                   CMB_CATG_CD_2,                                      *01041000
                   CMB_CATG_2_Q,                                       *01042000
                   CMB_CATG_2_INC,                                     *01043000
                   PRIME_CMB_OPT_1,                                    *01044000
                   PRIME_C_1_QLFY,                                     *01045000
                   PRIME_C_1_INCEN,                                    *01046000
                   PRIME_CMB_OPT_2,                                    *01047000
                   PRIME_C_2_QLFY,                                     *01048000
                   PRIME_C_2_INCEN,                                    *01049000
                   PROD_GRP_MIN_1,                                     *01050000
                   PROD_GRP_MAX_1,                                     *01051000
                   PROD_GRP_1_Q,                                       *01052000
                   PROD_GRP_MIN_2,                                     *01053000
                   PROD_GRP_MAX_2,                                     *01054000
                   PROD_GRP_2_Q,                                       *01055000
                   PROD_GRP_MIN_3,                                     *01056000
                   PROD_GRP_MAX_3,                                     *01057000
                   PROD_GRP_3_Q,                                       *01058000
                   PROD_GRP_MIN_4,                                     *01059000
                   PROD_GRP_MAX_4,                                     *01060000
                   PROD_GRP_4_Q,                                       *01061000
                   PROD_GRP_MIN_5,                                     *01062000
                   PROD_GRP_MAX_5,                                     *01063000
                   PROD_GRP_5_Q,                                       *01064000
                   ACCT_INCEN_OVRD,                                    *01065000
                   DEFAULT_MODEL,                                      *01066000
                   INCEN_BAL_TRN,                                      *01067000
                   TRAN_PROFILE,                                       *01068000
                   TRAN_INCENTIVE,                                     *01069000
                   CASH_MAX_AMT_PD,                                    *01070000
                   CASH_MAX_BAL,                                       *01071000
                   CMB_CATG_CD_3,                                      *01072000
                   CMB_CATG_3_Q,                                       *01073000
                   CMB_CATG_3_INC,                                     *01074000
                   CMB_CATG_CD_4,                                      *01075000
                   CMB_CATG_4_Q,                                       *01076000
                   CMB_CATG_4_INC,                                     *01077000
                   PRIME_CMB_OPT_3,                                    *01078000
                   PRIME_C_3_QLFY,                                     *01079000
                   PRIME_C_3_INCEN,                                    *01080000
                   PRIME_CMB_OPT_4,                                    *01081000
                   PRIME_C_4_QLFY,                                     *01082000
                   PRIME_C_4_INCEN,                                    *01083000
                   RSV_FLAG_1,                                         *01084000
                   CLOSED_ACCT_OPT,                                    *01085000
                   SEC_SVC_OPT,                                        *01086000
                   RATE_OPTION,                                        *01087000
                   ENROLL_LEFT,                                        *01088000
                   MODEL_TYPE,                                         *01089000
                   LONG_DESC,                                          *01090000
                   PRIM_REL_CODE,                                      *01091000
                   PRIM_QLFY,                                          *01092000
                   EXTL_ENROLL                                         *01093000
                 FROM S01                                              *01094000
                 WHERE                                                 *01095000
                   INST_NBR >=                                         *01096000
                     :INST                                             *01097000
                 ORDER BY INST_NBR ASC,                                *01098000
                   RECORD_NBR ASC,                                     *01099000
                   MODEL ASC,                                          *01100000
                   EFFECTIVE_DATE ASC                                  *01101000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01102000
         EXEC  SQL OPEN S01GE004                                        01103000
         MVC   SQWCSRCA,=A(CLSGE004)   SET CURSOR CLOSE ROUTINE ADDRESS 01104000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01105000
         BR    14                      RETURN TO CALLER                 01106000
         LTORG                                                          01107000
*                                                                       01108000
**********************************************************************  01109000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY KEY:          01110000
*   THIS ROUTINE HANDLES PRIMARY KEY SEQUENTIAL CURSORS.                01111000
**********************************************************************  01112000
*                                                                       01113000
NXTXC0   DS    0H                                                       01114000
         USING NXTXC0,12,8             ESTABLISH BASE REGISTER          01115000
         MVI   SQWKMRP,X'87'           CLOSE CURSOR & SET HOST KEY 0    01116000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01117000
         BALR  14,15                   MOVE REQUESTED DATA              01118000
         LH    1,SQWCSRSP              LOAD CURRENT CURSOR POINTER      01119000
         LA    1,4(1)                  INCREMENT TO NEXT CURSOR         01120000
         STH   1,SQWCSRSP              SAVE POINTER FOR NEXT CALL       01121000
         LA    12,NXTXC0P(1)           LOAD POINTER TO NEXT CURSOR      01122000
         L     12,0(12)                LOAD CURSOR ROUTINE ADDRESS      01123000
         BR    12                      GO TO CURRENT CURSOR ROUTINE     01124000
NXTXC0P  DC    A(0)                                                     01125000
         DC    A(NXTGT002)                                              01126000
         DC    A(NXTGT003)                                              01127000
         DC    A(NXTGT004)                                              01128000
         DC    A(NXTGT099)                                              01129000
         DC    A(NXTGT003)                                              01130000
         DC    A(NXTGT004)                                              01131000
         DC    A(NXTGT099)                                              01132000
         DC    A(NXTGT004)                                              01133000
         DC    A(NXTGT099)                                              01134000
         DC    A(NXTGT099)                                              01135000
NXTGT099 LA    0,4                     LOAD VALUE 4 IN REGISTER 0       01136000
         SR    1,0                     ADJUST BACK TO CURRENT POINTER   01137000
         STH   1,SQWCSRSP              SAVE POINTER FOR CURSOR CLOSE    01138000
         LA    0,100                   LOAD VALUE 100 IN REGISTER 0     01139000
         ST    0,SQLCODE               SET SQLCODE TO NO MORE ROWS      01140000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01141000
         BR    14                      RETURN TO SQI                    01142000
         LTORG                                                          01143000
*                                                                       01144000
NXTGT002 DS    0H                                                       01145000
         USING NXTGT002,12,8           ESTABLISH BASE REGISTER          01146000
         B     *+6                     BRANCH AROUND ADCON              01147000
BASGT002 DC    AL2(4096)                                                01148000
         LR    8,12                    LOAD SECOND BASE                 01149000
         AH    8,BASGT002              ADD 4K                           01150000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01151000
         AH    3,BASGT002              ADD 4K                           01152000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01153000
         AH    4,BASGT002              ADD 4K                           01154000
         EXEC  SQL DECLARE S01GT002 CURSOR                             *01155000
               WITH ROWSET POSITIONING                                 *01156000
               FOR SELECT                                              *01157000
                   INST_NBR,                                           *01158000
                   RECORD_NBR,                                         *01159000
                   MODEL,                                              *01160000
                   EFFECTIVE_DATE,                                     *01161000
                   AUDIT_DATE,                                         *01162000
                   AUDIT_TIME,                                         *01163000
                   AUDIT_USER,                                         *01164000
                   AUDIT_ORG,                                          *01165000
                   ACTIVE_CODE,                                        *01166000
                   SHORT_DESC,                                         *01167000
                   SCORE,                                              *01168000
                   AUTO_ENROLL,                                        *01169000
                   INCEN_BAL_OPT,                                      *01170000
                   PROCESS_OPTION,                                     *01171000
                   CMB_BAL_OPT,                                        *01172000
                   EXPIRE_FREQ,                                        *01173000
                   EXPIRE_TERM,                                        *01174000
                   END_MONTH,                                          *01175000
                   CLOSE_DATE,                                         *01176000
                   EXPIRE_DATE,                                        *01177000
                   EXPIRE_RET_DAYS,                                    *01178000
                   FIRST_RETENTION,                                    *01179000
                   DAY_PER_EXTN,                                       *01180000
                   EXTENSION,                                          *01181000
                   CASH_OPT,                                           *01182000
                   CASH_PCT,                                           *01183000
                   CASH_BAL_OPT,                                       *01184000
                   CASH_FREQ,                                          *01185000
                   CASH_TERM,                                          *01186000
                   CASH_DAY,                                           *01187000
                   NBR_ACCOUNTS,                                       *01188000
                   ACCT_QLFY,                                          *01189000
                   NBR_PROD,                                           *01190000
                   PROD_QLFY,                                          *01191000
                   MIN_ACCT,                                           *01192000
                   MIN_ACCT_Q,                                         *01193000
                   MAX_ACCT,                                           *01194000
                   MAX_ACCT_Q,                                         *01195000
                   MIN_PROD,                                           *01196000
                   MIN_PROD_Q,                                         *01197000
                   MAX_PROD,                                           *01198000
                   MAX_PROD_Q,                                         *01199000
                   DEMOGR_CODE,                                        *01200000
                   DEMOGR_QLFY,                                        *01201000
                   CUST_REL_CODE,                                      *01202000
                   CUST_QLFY,                                          *01203000
                   PRIM_BAL_OPT,                                       *01204000
                   PRIM_BAL_OPT_Q,                                     *01205000
                   PRIM_BAL_OPT_I,                                     *01206000
                   ACCT_REL_CD_Q,                                      *01207000
                   ACCT_REL_CD_I,                                      *01208000
                   ACCT_REL_Q,                                         *01209000
                   ACCT_REL_INCEN,                                     *01210000
                   CMB_CATG_CD_1,                                      *01211000
                   CMB_CATG_1_Q,                                       *01212000
                   CMB_CATG_1_INC,                                     *01213000
                   CMB_CATG_CD_2,                                      *01214000
                   CMB_CATG_2_Q,                                       *01215000
                   CMB_CATG_2_INC,                                     *01216000
                   PRIME_CMB_OPT_1,                                    *01217000
                   PRIME_C_1_QLFY,                                     *01218000
                   PRIME_C_1_INCEN,                                    *01219000
                   PRIME_CMB_OPT_2,                                    *01220000
                   PRIME_C_2_QLFY,                                     *01221000
                   PRIME_C_2_INCEN,                                    *01222000
                   PROD_GRP_MIN_1,                                     *01223000
                   PROD_GRP_MAX_1,                                     *01224000
                   PROD_GRP_1_Q,                                       *01225000
                   PROD_GRP_MIN_2,                                     *01226000
                   PROD_GRP_MAX_2,                                     *01227000
                   PROD_GRP_2_Q,                                       *01228000
                   PROD_GRP_MIN_3,                                     *01229000
                   PROD_GRP_MAX_3,                                     *01230000
                   PROD_GRP_3_Q,                                       *01231000
                   PROD_GRP_MIN_4,                                     *01232000
                   PROD_GRP_MAX_4,                                     *01233000
                   PROD_GRP_4_Q,                                       *01234000
                   PROD_GRP_MIN_5,                                     *01235000
                   PROD_GRP_MAX_5,                                     *01236000
                   PROD_GRP_5_Q,                                       *01237000
                   ACCT_INCEN_OVRD,                                    *01238000
                   DEFAULT_MODEL,                                      *01239000
                   INCEN_BAL_TRN,                                      *01240000
                   TRAN_PROFILE,                                       *01241000
                   TRAN_INCENTIVE,                                     *01242000
                   CASH_MAX_AMT_PD,                                    *01243000
                   CASH_MAX_BAL,                                       *01244000
                   CMB_CATG_CD_3,                                      *01245000
                   CMB_CATG_3_Q,                                       *01246000
                   CMB_CATG_3_INC,                                     *01247000
                   CMB_CATG_CD_4,                                      *01248000
                   CMB_CATG_4_Q,                                       *01249000
                   CMB_CATG_4_INC,                                     *01250000
                   PRIME_CMB_OPT_3,                                    *01251000
                   PRIME_C_3_QLFY,                                     *01252000
                   PRIME_C_3_INCEN,                                    *01253000
                   PRIME_CMB_OPT_4,                                    *01254000
                   PRIME_C_4_QLFY,                                     *01255000
                   PRIME_C_4_INCEN,                                    *01256000
                   RSV_FLAG_1,                                         *01257000
                   CLOSED_ACCT_OPT,                                    *01258000
                   SEC_SVC_OPT,                                        *01259000
                   RATE_OPTION,                                        *01260000
                   ENROLL_LEFT,                                        *01261000
                   MODEL_TYPE,                                         *01262000
                   LONG_DESC,                                          *01263000
                   PRIM_REL_CODE,                                      *01264000
                   PRIM_QLFY,                                          *01265000
                   EXTL_ENROLL                                         *01266000
                 FROM S01                                              *01267000
                 WHERE                                                 *01268000
                   INST_NBR = :INST AND                                *01269000
                   RECORD_NBR = :RECNBR AND                            *01270000
                   MODEL >                                             *01271000
                     :MODEL                                            *01272000
                 ORDER BY MODEL ASC,                                   *01273000
                   EFFECTIVE_DATE ASC                                  *01274000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01275000
         EXEC  SQL OPEN S01GT002                                        01276000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            01277000
         BNZ   *+14                    NO - RETURN ERROR                01278000
         MVC   SQWCSRCA,=A(CLSGT002)   SET CURSOR CLOSE ROUTINE ADDRESS 01279000
         L     12,SQWCSRRA             LOAD RETURN ADDRESS              01280000
         BR    12                      RETURN TO FETCH ROUTINE          01281000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01282000
         BR    14                      RETURN TO CALLER                 01283000
         LTORG                                                          01284000
*                                                                       01285000
NXTGT003 DS    0H                                                       01286000
         USING NXTGT003,12,8           ESTABLISH BASE REGISTER          01287000
         B     *+6                     BRANCH AROUND ADCON              01288000
BASGT003 DC    AL2(4096)                                                01289000
         LR    8,12                    LOAD SECOND BASE                 01290000
         AH    8,BASGT003              ADD 4K                           01291000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01292000
         AH    3,BASGT003              ADD 4K                           01293000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01294000
         AH    4,BASGT003              ADD 4K                           01295000
         EXEC  SQL DECLARE S01GT003 CURSOR                             *01296000
               WITH ROWSET POSITIONING                                 *01297000
               FOR SELECT                                              *01298000
                   INST_NBR,                                           *01299000
                   RECORD_NBR,                                         *01300000
                   MODEL,                                              *01301000
                   EFFECTIVE_DATE,                                     *01302000
                   AUDIT_DATE,                                         *01303000
                   AUDIT_TIME,                                         *01304000
                   AUDIT_USER,                                         *01305000
                   AUDIT_ORG,                                          *01306000
                   ACTIVE_CODE,                                        *01307000
                   SHORT_DESC,                                         *01308000
                   SCORE,                                              *01309000
                   AUTO_ENROLL,                                        *01310000
                   INCEN_BAL_OPT,                                      *01311000
                   PROCESS_OPTION,                                     *01312000
                   CMB_BAL_OPT,                                        *01313000
                   EXPIRE_FREQ,                                        *01314000
                   EXPIRE_TERM,                                        *01315000
                   END_MONTH,                                          *01316000
                   CLOSE_DATE,                                         *01317000
                   EXPIRE_DATE,                                        *01318000
                   EXPIRE_RET_DAYS,                                    *01319000
                   FIRST_RETENTION,                                    *01320000
                   DAY_PER_EXTN,                                       *01321000
                   EXTENSION,                                          *01322000
                   CASH_OPT,                                           *01323000
                   CASH_PCT,                                           *01324000
                   CASH_BAL_OPT,                                       *01325000
                   CASH_FREQ,                                          *01326000
                   CASH_TERM,                                          *01327000
                   CASH_DAY,                                           *01328000
                   NBR_ACCOUNTS,                                       *01329000
                   ACCT_QLFY,                                          *01330000
                   NBR_PROD,                                           *01331000
                   PROD_QLFY,                                          *01332000
                   MIN_ACCT,                                           *01333000
                   MIN_ACCT_Q,                                         *01334000
                   MAX_ACCT,                                           *01335000
                   MAX_ACCT_Q,                                         *01336000
                   MIN_PROD,                                           *01337000
                   MIN_PROD_Q,                                         *01338000
                   MAX_PROD,                                           *01339000
                   MAX_PROD_Q,                                         *01340000
                   DEMOGR_CODE,                                        *01341000
                   DEMOGR_QLFY,                                        *01342000
                   CUST_REL_CODE,                                      *01343000
                   CUST_QLFY,                                          *01344000
                   PRIM_BAL_OPT,                                       *01345000
                   PRIM_BAL_OPT_Q,                                     *01346000
                   PRIM_BAL_OPT_I,                                     *01347000
                   ACCT_REL_CD_Q,                                      *01348000
                   ACCT_REL_CD_I,                                      *01349000
                   ACCT_REL_Q,                                         *01350000
                   ACCT_REL_INCEN,                                     *01351000
                   CMB_CATG_CD_1,                                      *01352000
                   CMB_CATG_1_Q,                                       *01353000
                   CMB_CATG_1_INC,                                     *01354000
                   CMB_CATG_CD_2,                                      *01355000
                   CMB_CATG_2_Q,                                       *01356000
                   CMB_CATG_2_INC,                                     *01357000
                   PRIME_CMB_OPT_1,                                    *01358000
                   PRIME_C_1_QLFY,                                     *01359000
                   PRIME_C_1_INCEN,                                    *01360000
                   PRIME_CMB_OPT_2,                                    *01361000
                   PRIME_C_2_QLFY,                                     *01362000
                   PRIME_C_2_INCEN,                                    *01363000
                   PROD_GRP_MIN_1,                                     *01364000
                   PROD_GRP_MAX_1,                                     *01365000
                   PROD_GRP_1_Q,                                       *01366000
                   PROD_GRP_MIN_2,                                     *01367000
                   PROD_GRP_MAX_2,                                     *01368000
                   PROD_GRP_2_Q,                                       *01369000
                   PROD_GRP_MIN_3,                                     *01370000
                   PROD_GRP_MAX_3,                                     *01371000
                   PROD_GRP_3_Q,                                       *01372000
                   PROD_GRP_MIN_4,                                     *01373000
                   PROD_GRP_MAX_4,                                     *01374000
                   PROD_GRP_4_Q,                                       *01375000
                   PROD_GRP_MIN_5,                                     *01376000
                   PROD_GRP_MAX_5,                                     *01377000
                   PROD_GRP_5_Q,                                       *01378000
                   ACCT_INCEN_OVRD,                                    *01379000
                   DEFAULT_MODEL,                                      *01380000
                   INCEN_BAL_TRN,                                      *01381000
                   TRAN_PROFILE,                                       *01382000
                   TRAN_INCENTIVE,                                     *01383000
                   CASH_MAX_AMT_PD,                                    *01384000
                   CASH_MAX_BAL,                                       *01385000
                   CMB_CATG_CD_3,                                      *01386000
                   CMB_CATG_3_Q,                                       *01387000
                   CMB_CATG_3_INC,                                     *01388000
                   CMB_CATG_CD_4,                                      *01389000
                   CMB_CATG_4_Q,                                       *01390000
                   CMB_CATG_4_INC,                                     *01391000
                   PRIME_CMB_OPT_3,                                    *01392000
                   PRIME_C_3_QLFY,                                     *01393000
                   PRIME_C_3_INCEN,                                    *01394000
                   PRIME_CMB_OPT_4,                                    *01395000
                   PRIME_C_4_QLFY,                                     *01396000
                   PRIME_C_4_INCEN,                                    *01397000
                   RSV_FLAG_1,                                         *01398000
                   CLOSED_ACCT_OPT,                                    *01399000
                   SEC_SVC_OPT,                                        *01400000
                   RATE_OPTION,                                        *01401000
                   ENROLL_LEFT,                                        *01402000
                   MODEL_TYPE,                                         *01403000
                   LONG_DESC,                                          *01404000
                   PRIM_REL_CODE,                                      *01405000
                   PRIM_QLFY,                                          *01406000
                   EXTL_ENROLL                                         *01407000
                 FROM S01                                              *01408000
                 WHERE                                                 *01409000
                   INST_NBR = :INST AND                                *01410000
                   RECORD_NBR >                                        *01411000
                     :RECNBR                                           *01412000
                 ORDER BY RECORD_NBR ASC,                              *01413000
                   MODEL ASC,                                          *01414000
                   EFFECTIVE_DATE ASC                                  *01415000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01416000
         EXEC  SQL OPEN S01GT003                                        01417000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            01418000
         BNZ   *+14                    NO - RETURN ERROR                01419000
         MVC   SQWCSRCA,=A(CLSGT003)   SET CURSOR CLOSE ROUTINE ADDRESS 01420000
         L     12,SQWCSRRA             LOAD RETURN ADDRESS              01421000
         BR    12                      RETURN TO FETCH ROUTINE          01422000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01423000
         BR    14                      RETURN TO CALLER                 01424000
         LTORG                                                          01425000
*                                                                       01426000
NXTGT004 DS    0H                                                       01427000
         USING NXTGT004,12,8           ESTABLISH BASE REGISTER          01428000
         B     *+6                     BRANCH AROUND ADCON              01429000
BASGT004 DC    AL2(4096)                                                01430000
         LR    8,12                    LOAD SECOND BASE                 01431000
         AH    8,BASGT004              ADD 4K                           01432000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01433000
         AH    3,BASGT004              ADD 4K                           01434000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    01435000
         AH    4,BASGT004              ADD 4K                           01436000
         EXEC  SQL DECLARE S01GT004 CURSOR                             *01437000
               WITH ROWSET POSITIONING                                 *01438000
               FOR SELECT                                              *01439000
                   INST_NBR,                                           *01440000
                   RECORD_NBR,                                         *01441000
                   MODEL,                                              *01442000
                   EFFECTIVE_DATE,                                     *01443000
                   AUDIT_DATE,                                         *01444000
                   AUDIT_TIME,                                         *01445000
                   AUDIT_USER,                                         *01446000
                   AUDIT_ORG,                                          *01447000
                   ACTIVE_CODE,                                        *01448000
                   SHORT_DESC,                                         *01449000
                   SCORE,                                              *01450000
                   AUTO_ENROLL,                                        *01451000
                   INCEN_BAL_OPT,                                      *01452000
                   PROCESS_OPTION,                                     *01453000
                   CMB_BAL_OPT,                                        *01454000
                   EXPIRE_FREQ,                                        *01455000
                   EXPIRE_TERM,                                        *01456000
                   END_MONTH,                                          *01457000
                   CLOSE_DATE,                                         *01458000
                   EXPIRE_DATE,                                        *01459000
                   EXPIRE_RET_DAYS,                                    *01460000
                   FIRST_RETENTION,                                    *01461000
                   DAY_PER_EXTN,                                       *01462000
                   EXTENSION,                                          *01463000
                   CASH_OPT,                                           *01464000
                   CASH_PCT,                                           *01465000
                   CASH_BAL_OPT,                                       *01466000
                   CASH_FREQ,                                          *01467000
                   CASH_TERM,                                          *01468000
                   CASH_DAY,                                           *01469000
                   NBR_ACCOUNTS,                                       *01470000
                   ACCT_QLFY,                                          *01471000
                   NBR_PROD,                                           *01472000
                   PROD_QLFY,                                          *01473000
                   MIN_ACCT,                                           *01474000
                   MIN_ACCT_Q,                                         *01475000
                   MAX_ACCT,                                           *01476000
                   MAX_ACCT_Q,                                         *01477000
                   MIN_PROD,                                           *01478000
                   MIN_PROD_Q,                                         *01479000
                   MAX_PROD,                                           *01480000
                   MAX_PROD_Q,                                         *01481000
                   DEMOGR_CODE,                                        *01482000
                   DEMOGR_QLFY,                                        *01483000
                   CUST_REL_CODE,                                      *01484000
                   CUST_QLFY,                                          *01485000
                   PRIM_BAL_OPT,                                       *01486000
                   PRIM_BAL_OPT_Q,                                     *01487000
                   PRIM_BAL_OPT_I,                                     *01488000
                   ACCT_REL_CD_Q,                                      *01489000
                   ACCT_REL_CD_I,                                      *01490000
                   ACCT_REL_Q,                                         *01491000
                   ACCT_REL_INCEN,                                     *01492000
                   CMB_CATG_CD_1,                                      *01493000
                   CMB_CATG_1_Q,                                       *01494000
                   CMB_CATG_1_INC,                                     *01495000
                   CMB_CATG_CD_2,                                      *01496000
                   CMB_CATG_2_Q,                                       *01497000
                   CMB_CATG_2_INC,                                     *01498000
                   PRIME_CMB_OPT_1,                                    *01499000
                   PRIME_C_1_QLFY,                                     *01500000
                   PRIME_C_1_INCEN,                                    *01501000
                   PRIME_CMB_OPT_2,                                    *01502000
                   PRIME_C_2_QLFY,                                     *01503000
                   PRIME_C_2_INCEN,                                    *01504000
                   PROD_GRP_MIN_1,                                     *01505000
                   PROD_GRP_MAX_1,                                     *01506000
                   PROD_GRP_1_Q,                                       *01507000
                   PROD_GRP_MIN_2,                                     *01508000
                   PROD_GRP_MAX_2,                                     *01509000
                   PROD_GRP_2_Q,                                       *01510000
                   PROD_GRP_MIN_3,                                     *01511000
                   PROD_GRP_MAX_3,                                     *01512000
                   PROD_GRP_3_Q,                                       *01513000
                   PROD_GRP_MIN_4,                                     *01514000
                   PROD_GRP_MAX_4,                                     *01515000
                   PROD_GRP_4_Q,                                       *01516000
                   PROD_GRP_MIN_5,                                     *01517000
                   PROD_GRP_MAX_5,                                     *01518000
                   PROD_GRP_5_Q,                                       *01519000
                   ACCT_INCEN_OVRD,                                    *01520000
                   DEFAULT_MODEL,                                      *01521000
                   INCEN_BAL_TRN,                                      *01522000
                   TRAN_PROFILE,                                       *01523000
                   TRAN_INCENTIVE,                                     *01524000
                   CASH_MAX_AMT_PD,                                    *01525000
                   CASH_MAX_BAL,                                       *01526000
                   CMB_CATG_CD_3,                                      *01527000
                   CMB_CATG_3_Q,                                       *01528000
                   CMB_CATG_3_INC,                                     *01529000
                   CMB_CATG_CD_4,                                      *01530000
                   CMB_CATG_4_Q,                                       *01531000
                   CMB_CATG_4_INC,                                     *01532000
                   PRIME_CMB_OPT_3,                                    *01533000
                   PRIME_C_3_QLFY,                                     *01534000
                   PRIME_C_3_INCEN,                                    *01535000
                   PRIME_CMB_OPT_4,                                    *01536000
                   PRIME_C_4_QLFY,                                     *01537000
                   PRIME_C_4_INCEN,                                    *01538000
                   RSV_FLAG_1,                                         *01539000
                   CLOSED_ACCT_OPT,                                    *01540000
                   SEC_SVC_OPT,                                        *01541000
                   RATE_OPTION,                                        *01542000
                   ENROLL_LEFT,                                        *01543000
                   MODEL_TYPE,                                         *01544000
                   LONG_DESC,                                          *01545000
                   PRIM_REL_CODE,                                      *01546000
                   PRIM_QLFY,                                          *01547000
                   EXTL_ENROLL                                         *01548000
                 FROM S01                                              *01549000
                 WHERE                                                 *01550000
                   INST_NBR >                                          *01551000
                     :INST                                             *01552000
                 ORDER BY INST_NBR ASC,                                *01553000
                   RECORD_NBR ASC,                                     *01554000
                   MODEL ASC,                                          *01555000
                   EFFECTIVE_DATE ASC                                  *01556000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01557000
         EXEC  SQL OPEN S01GT004                                        01558000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            01559000
         BNZ   *+14                    NO - RETURN ERROR                01560000
         MVC   SQWCSRCA,=A(CLSGT004)   SET CURSOR CLOSE ROUTINE ADDRESS 01561000
         L     12,SQWCSRRA             LOAD RETURN ADDRESS              01562000
         BR    12                      RETURN TO FETCH ROUTINE          01563000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01564000
         BR    14                      RETURN TO CALLER                 01565000
         LTORG                                                          01566000
*                                                                       01567000
**********************************************************************  01568000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:               01569000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01570000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01571000
*     RETRIEVE THE ACTUAL ROW.                                          01572000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01573000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01574000
**********************************************************************  01575000
*                                                                       01576000
FETDC0   DS    0H                                                       01577000
         USING FETDC0,12,8             ESTABLISH BASE REGISTER          01578000
         MVC   SQWCSRRA,=A(FETDC0)     SET RETURN ROUTINE ADDRESS       01579000
         MVC   SQWCSRBR,=A(NXTXC0)     SET CURSOR ROUTINE ADDRESS       01580000
         MVC   SQWRSFVC,=AL2(TBLCOLMS) FETCH ALL COLUMNS IN TABLE       01581000
         LH    1,SQWCSRSP              LOAD CURRENT CURSOR POINTER      01582000
         LA    12,FETDC0P(1)           LOAD POINTER TO FETCH ROUTINE    01583000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01584000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01585000
FETDC0P  DC    A(FETGE001)                                              01586000
         DC    A(FETGT002)                                              01587000
         DC    A(FETGT003)                                              01588000
         DC    A(FETGT004)                                              01589000
         DC    A(FETGE002)                                              01590000
         DC    A(FETGT003)                                              01591000
         DC    A(FETGT004)                                              01592000
         DC    A(FETGE003)                                              01593000
         DC    A(FETGT004)                                              01594000
         DC    A(FETGE004)                                              01595000
         DC    A(0)                                                     01596000
         LTORG                                                          01597000
*                                                                       01598000
**********************************************************************  01599000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01600000
* THE PRIMARY KEY:                                                      01601000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01602000
*     VERBS.                                                            01603000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01604000
*     RETRIEVE THE ACTUAL ROW.                                          01605000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01606000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01607000
*     WILL BE THRU THE UPDATE CURSOR.                                   01608000
**********************************************************************  01609000
*                                                                       01610000
FETKC0   DS    0H                                                       01611000
         USING FETKC0,12,8             ESTABLISH BASE REGISTER          01612000
         MVC   SQWCSRRA,=A(FETKC0)     SET RETURN ROUTINE ADDRESS       01613000
         MVC   SQWCSRBR,=A(NXTXC0)     SET CURSOR ROUTINE ADDRESS       01614000
         MVC   SQWRSFVC,=AL2(KY0COLMS) ONLY FETCH KEY COLUMNS           01615000
         LH    1,SQWCSRSP              LOAD CURRENT CURSOR POINTER      01616000
         LA    12,FETKC0P(1)           LOAD POINTER TO FETCH ROUTINE    01617000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01618000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01619000
FETKC0P  DC    A(FETGE001)                                              01620000
         DC    A(FETGT002)                                              01621000
         DC    A(FETGT003)                                              01622000
         DC    A(FETGT004)                                              01623000
         DC    A(FETGE002)                                              01624000
         DC    A(FETGT003)                                              01625000
         DC    A(FETGT004)                                              01626000
         DC    A(FETGE003)                                              01627000
         DC    A(FETGT004)                                              01628000
         DC    A(FETGE004)                                              01629000
         DC    A(0)                                                     01630000
         LTORG                                                          01631000
*                                                                       01632000
FETGE001 DS    0H                                                       01633000
         USING FETGE001,12,8           ESTABLISH BASE REGISTER          01634000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01635000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01636000
         BALR  14,15                   MOVE REQUESTED DATA              01637000
         EXEC  SQL FETCH NEXT ROWSET FROM S01GE001                     *01638000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01639000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01640000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01641000
         BALR  14,15                   MOVE REQUESTED DATA              01642000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01643000
         BR    14                      RETURN TO CALLER                 01644000
         LTORG                                                          01645000
*                                                                       01646000
FETGE002 DS    0H                                                       01647000
         USING FETGE002,12,8           ESTABLISH BASE REGISTER          01648000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01649000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01650000
         BALR  14,15                   MOVE REQUESTED DATA              01651000
         EXEC  SQL FETCH NEXT ROWSET FROM S01GE002                     *01652000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01653000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01654000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01655000
         BALR  14,15                   MOVE REQUESTED DATA              01656000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01657000
         BR    14                      RETURN TO CALLER                 01658000
         LTORG                                                          01659000
*                                                                       01660000
FETGE003 DS    0H                                                       01661000
         USING FETGE003,12,8           ESTABLISH BASE REGISTER          01662000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01663000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01664000
         BALR  14,15                   MOVE REQUESTED DATA              01665000
         EXEC  SQL FETCH NEXT ROWSET FROM S01GE003                     *01666000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01667000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01668000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01669000
         BALR  14,15                   MOVE REQUESTED DATA              01670000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01671000
         BR    14                      RETURN TO CALLER                 01672000
         LTORG                                                          01673000
*                                                                       01674000
FETGE004 DS    0H                                                       01675000
         USING FETGE004,12,8           ESTABLISH BASE REGISTER          01676000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01677000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01678000
         BALR  14,15                   MOVE REQUESTED DATA              01679000
         EXEC  SQL FETCH NEXT ROWSET FROM S01GE004                     *01680000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01681000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01682000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01683000
         BALR  14,15                   MOVE REQUESTED DATA              01684000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01685000
         BR    14                      RETURN TO CALLER                 01686000
         LTORG                                                          01687000
*                                                                       01688000
FETGT002 DS    0H                                                       01689000
         USING FETGT002,12,8           ESTABLISH BASE REGISTER          01690000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01691000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01692000
         BALR  14,15                   MOVE REQUESTED DATA              01693000
         EXEC  SQL FETCH NEXT ROWSET FROM S01GT002                     *01694000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01695000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01696000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01697000
         BALR  14,15                   MOVE REQUESTED DATA              01698000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01699000
         BR    14                      RETURN TO CALLER                 01700000
         LTORG                                                          01701000
*                                                                       01702000
FETGT003 DS    0H                                                       01703000
         USING FETGT003,12,8           ESTABLISH BASE REGISTER          01704000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01705000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01706000
         BALR  14,15                   MOVE REQUESTED DATA              01707000
         EXEC  SQL FETCH NEXT ROWSET FROM S01GT003                     *01708000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01709000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01710000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01711000
         BALR  14,15                   MOVE REQUESTED DATA              01712000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01713000
         BR    14                      RETURN TO CALLER                 01714000
         LTORG                                                          01715000
*                                                                       01716000
FETGT004 DS    0H                                                       01717000
         USING FETGT004,12,8           ESTABLISH BASE REGISTER          01718000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01719000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01720000
         BALR  14,15                   MOVE REQUESTED DATA              01721000
         EXEC  SQL FETCH NEXT ROWSET FROM S01GT004                     *01722000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01723000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01724000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01725000
         BALR  14,15                   MOVE REQUESTED DATA              01726000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01727000
         BR    14                      RETURN TO CALLER                 01728000
         LTORG                                                          01729000
*                                                                       01730000
**********************************************************************  01731000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:                    01732000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01733000
*     AND GET-NEXT-LOCK VERBS.                                          01734000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01735000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01736000
**********************************************************************  01737000
*                                                                       01738000
CLSXC0   DS    0H                                                       01739000
         USING CLSXC0,12,8             ESTABLISH BASE REGISTER          01740000
         L     12,SQWCSRCA             SET CURSOR CLOSE ROUTINE ADDRESS 01741000
         XC    SQWCSRCA,SQWCSRCA       CLEAR CURSOR CLOSE ROUTINE ADDR  01742000
         BR    12                      GO TO CURSOR CLOSE ROUTINE       01743000
         LTORG                                                          01744000
*                                                                       01745000
CLSGE001 DS    0H                                                       01746000
         USING CLSGE001,12,8           ESTABLISH BASE REGISTER          01747000
         EXEC  SQL CLOSE S01GE001                                       01748000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01749000
         BR    14                      RETURN TO CALLER                 01750000
         LTORG                                                          01751000
*                                                                       01752000
CLSGE002 DS    0H                                                       01753000
         USING CLSGE002,12,8           ESTABLISH BASE REGISTER          01754000
         EXEC  SQL CLOSE S01GE002                                       01755000
         L     14,SQW@RET                                               01756000
         BR    14                                                       01757000
         LTORG                                                          01758000
*                                                                       01759000
CLSGE003 DS    0H                                                       01760000
         USING CLSGE003,12,8           ESTABLISH BASE REGISTER          01761000
         EXEC  SQL CLOSE S01GE003                                       01762000
         L     14,SQW@RET                                               01763000
         BR    14                                                       01764000
         LTORG                                                          01765000
*                                                                       01766000
CLSGE004 DS    0H                                                       01767000
         USING CLSGE004,12,8           ESTABLISH BASE REGISTER          01768000
         EXEC  SQL CLOSE S01GE004                                       01769000
         L     14,SQW@RET                                               01770000
         BR    14                                                       01771000
         LTORG                                                          01772000
*                                                                       01773000
CLSGT002 DS    0H                                                       01774000
         USING CLSGT002,12,8           ESTABLISH BASE REGISTER          01775000
         EXEC  SQL CLOSE S01GT002                                       01776000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01777000
         BR    14                      RETURN TO CALLER                 01778000
         LTORG                                                          01779000
*                                                                       01780000
CLSGT003 DS    0H                                                       01781000
         USING CLSGT003,12,8           ESTABLISH BASE REGISTER          01782000
         EXEC  SQL CLOSE S01GT003                                       01783000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01784000
         BR    14                      RETURN TO CALLER                 01785000
         LTORG                                                          01786000
*                                                                       01787000
CLSGT004 DS    0H                                                       01788000
         USING CLSGT004,12,8           ESTABLISH BASE REGISTER          01789000
         EXEC  SQL CLOSE S01GT004                                       01790000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01791000
         BR    14                      RETURN TO CALLER                 01792000
         LTORG                                                          01793000
*                                                                       01794000
**********************************************************************  01795000
* ALTERNATE KEY 1 NOT DEFINED                                           01796000
**********************************************************************  01797000
*                                                                       01798000
SELIN1   DS    0H                                                       01799000
SELKY1   DS    0H                                                       01800000
SELXC1   DS    0H                                                       01801000
FETDC1   DS    0H                                                       01802000
FETKC1   DS    0H                                                       01803000
CLSXC1   DS    0H                                                       01804000
         DC    X'00DEAD01'             FORCE S0C1 ABEND                 01805000
*                                                                       01806000
**********************************************************************  01807000
* ALTERNATE KEY 2 NOT DEFINED                                           01808000
**********************************************************************  01809000
*                                                                       01810000
SELIN2   DS    0H                                                       01811000
SELKY2   DS    0H                                                       01812000
SELXC2   DS    0H                                                       01813000
FETDC2   DS    0H                                                       01814000
FETKC2   DS    0H                                                       01815000
CLSXC2   DS    0H                                                       01816000
         DC    X'00DEAD02'             FORCE S0C1 ABEND                 01817000
*                                                                       01818000
**********************************************************************  01819000
* ALTERNATE KEY 3 NOT DEFINED                                           01820000
**********************************************************************  01821000
*                                                                       01822000
SELIN3   DS    0H                                                       01823000
SELKY3   DS    0H                                                       01824000
SELXC3   DS    0H                                                       01825000
FETDC3   DS    0H                                                       01826000
FETKC3   DS    0H                                                       01827000
CLSXC3   DS    0H                                                       01828000
         DC    X'00DEAD03'             FORCE S0C1 ABEND                 01829000
*                                                                       01830000
         DS    0H                      END OF SQL STATEMENTS            01831000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   01832000
*                                                                       01833000
**********************************************************************  01834000
* DUMMY ENTRY POINT DSNHLI                                              01835000
**********************************************************************  01836000
*                                                                       01837000
         ENTRY DSNHLI                                                   01838000
DSNHLI   DS    0H                                                       01839000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       01840000
         BR    15                      BRANCH TO ATTACH FACILITY        01841000
*                                                                       01842000
**********************************************************************  01843000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  01844000
**********************************************************************  01845000
*                                                                       01846000
* CONVTAB1 TABLE ENTRY FORMAT IS:                                       01847000
*        DC    H'RRRR',H'VVVV',H'LLLL',X'KK',X'DD'                      01848000
* OR:                                                                   01849000
*        DC    H'RRRR',H'VVVV',X'ZZPP',X'KK',X'DD'                      01850000
* WHERE:                                                                01851000
*   RRRR = RECORD AREA OFFSET                                           01852000
*   VVVV = HOST VARIABLE AREA OFFSET                                    01853000
*   LLLL = HALFWORD LENGTH TO MOVE                                      01854000
*   ZZPP = CONVERT ZONED/PACKED LENGTHS (MINUS 1)                       01855000
*   KK   = KEY FIELD MASK:                                              01856000
*            80 = KEY 0 FIELD                                           01857000
*            40 = KEY 1 FIELD                                           01858000
*            20 = KEY 2 FIELD                                           01859000
*            10 = KEY 3 FIELD                                           01860000
*   DD   = DATA FIELD MASK:                                             01861000
*            80 = RECORD FIELD IS PACKED                                01862000
*            40 = HOST VARIABLE IS PACKED                               01863000
*            20 = NULLABLE FIELD                                        01864000
*            01 = DATE FIELD                                            01865000
*            02 = TIME FIELD                                            01866000
*                                                                       01867000
CONVTAB1 DS    0H                      RECORD/HOST VARIABLE CONVERSIONS 01868000
         DC    H'0000',H'0000',H'0026',X'80',X'00'                      01869000
         DC    H'0026',H'0026',H'0301',X'00',X'00'                      01870000
         DC    8X'FF'                                                   01871000
*                                                                       01872000
* CONVTAB2 TABLE COLUMNS ENTRY FORMAT IS:                               01873000
*        DC    H'RRRR',H'LLLL',H'TTTT'                                  01874000
* OR:                                                                   01875000
*        DC    H'RRRR',X'PPSS',H'TTTT'                                  01876000
* WHERE:                                                                01877000
*   RRRR = RECORD AREA OFFSET                                           01878000
*   LLLL = HALFWORD LENGTH OF HOST VARIABLE (NON-DECIMAL DATA TYPE)     01879000
*   PPSS = PRECISION AND SCALE (DECIMAL DATA TYPE)                      01880000
*   TTTT = SQLDA DATA TYPE:                                             01881000
*            452 = CHAR       453 = CHAR NULLABLE                       01882000
*            484 = DECIMAL    485 = DECIMAL NULLABLE                    01883000
*            496 = INTEGER    497 = INTEGER NULLABLE                    01884000
*            500 = SMALLINT   501 = SMALLINT NULLABLE                   01885000
*                                                                       01886000
CONVTAB2 DS    0F                      RECORD DATA ATTRIBUTE TABLE      01887000
         DC    AL4(TBLCOLMS*44+16)     TOTAL SQLDA SIZE                 01888000
         DC    AL2(TBLCOLMS)           NUMBER OF COLUMNS IN TABLE       01889000
         DC    AL2(KY0COLMS)           NUMBER OF PRIMARY KEY COLUMNS    01890000
         DC    AL2(KY1COLMS)           NUMBER OF ALT1 KEY COLUMNS       01891000
         DC    AL2(KY2COLMS)           NUMBER OF ALT2 KEY COLUMNS       01892000
         DC    AL2(KY3COLMS)           NUMBER OF ALT3 KEY COLUMNS       01893000
         DC    AL2(00327)              SUMMATION OF ALL COLUMN LENGTHS  01894000
         DC    AL2(01000)              PRIMARY MULTIROW FETCH ARRAY     01895000
         DC    AL2(00000)              ALT 1 MULTIROW FETCH ARRAY       01896000
         DC    AL2(00000)              ALT 2 MULTIROW FETCH ARRAY       01897000
         DC    AL2(00000)              ALT 3 MULTIROW FETCH ARRAY       01898000
         DC    AL2(00000)              PRIMARY KEY LOW VALUE LEVEL      01899000
         DC    AL2(00000)              ALT 1 KEY LOW VALUE LEVEL        01900000
         DC    AL2(00000)              ALT 2 KEY LOW VALUE LEVEL        01901000
         DC    AL2(00000)              ALT 3 KEY LOW VALUE LEVEL        01902000
CONVDATA DS    0H                                                       01903000
         DC    H'0000',H'0004',H'452'  INST_NBR                         01904000
         DC    H'0004',H'0004',H'452'  RECORD_NBR                       01905000
         DC    H'0008',H'0010',H'452'  MODEL                            01906000
         DC    H'0018',H'0008',H'452'  EFFECTIVE_DATE                   01907000
KEYCOLMS EQU   (*-CONVDATA)/6          NUMBER OF KEY COLUMNS IN TABLE   01908000
         DC    H'0026',X'0900',H'484'  AUDIT_DATE                       01909000
         DC    H'0031',X'0900',H'484'  AUDIT_TIME                       01910000
         DC    H'0036',H'0008',H'452'  AUDIT_USER                       01911000
         DC    H'0044',H'0006',H'452'  AUDIT_ORG                        01912000
         DC    H'0050',H'0001',H'452'  ACTIVE_CODE                      01913000
         DC    H'0051',H'0020',H'452'  SHORT_DESC                       01914000
         DC    H'0071',H'0003',H'452'  SCORE                            01915000
         DC    H'0074',H'0001',H'452'  AUTO_ENROLL                      01916000
         DC    H'0075',H'0001',H'452'  INCEN_BAL_OPT                    01917000
         DC    H'0076',H'0001',H'452'  PROCESS_OPTION                   01918000
         DC    H'0077',H'0001',H'452'  CMB_BAL_OPT                      01919000
         DC    H'0078',H'0001',H'452'  EXPIRE_FREQ                      01920000
         DC    H'0079',X'0300',H'484'  EXPIRE_TERM                      01921000
         DC    H'0081',H'0001',H'452'  END_MONTH                        01922000
         DC    H'0082',X'0900',H'484'  CLOSE_DATE                       01923000
         DC    H'0087',X'0900',H'484'  EXPIRE_DATE                      01924000
         DC    H'0092',X'0300',H'484'  EXPIRE_RET_DAYS                  01925000
         DC    H'0094',X'0300',H'484'  FIRST_RETENTION                  01926000
         DC    H'0096',X'0300',H'484'  DAY_PER_EXTN                     01927000
         DC    H'0098',H'0002',H'452'  EXTENSION                        01928000
         DC    H'0100',H'0001',H'452'  CASH_OPT                         01929000
         DC    H'0101',X'0909',H'484'  CASH_PCT                         01930000
         DC    H'0106',H'0001',H'452'  CASH_BAL_OPT                     01931000
         DC    H'0107',H'0001',H'452'  CASH_FREQ                        01932000
         DC    H'0108',X'0300',H'484'  CASH_TERM                        01933000
         DC    H'0110',H'0002',H'452'  CASH_DAY                         01934000
         DC    H'0112',H'0002',H'452'  NBR_ACCOUNTS                     01935000
         DC    H'0114',H'0001',H'452'  ACCT_QLFY                        01936000
         DC    H'0115',H'0002',H'452'  NBR_PROD                         01937000
         DC    H'0117',H'0001',H'452'  PROD_QLFY                        01938000
         DC    H'0118',H'0002',H'452'  MIN_ACCT                         01939000
         DC    H'0120',H'0001',H'452'  MIN_ACCT_Q                       01940000
         DC    H'0121',H'0002',H'452'  MAX_ACCT                         01941000
         DC    H'0123',H'0001',H'452'  MAX_ACCT_Q                       01942000
         DC    H'0124',H'0002',H'452'  MIN_PROD                         01943000
         DC    H'0126',H'0001',H'452'  MIN_PROD_Q                       01944000
         DC    H'0127',H'0002',H'452'  MAX_PROD                         01945000
         DC    H'0129',H'0001',H'452'  MAX_PROD_Q                       01946000
         DC    H'0130',H'0006',H'452'  DEMOGR_CODE                      01947000
         DC    H'0136',H'0001',H'452'  DEMOGR_QLFY                      01948000
         DC    H'0137',H'0006',H'452'  CUST_REL_CODE                    01949000
         DC    H'0143',H'0001',H'452'  CUST_QLFY                        01950000
         DC    H'0144',H'0001',H'452'  PRIM_BAL_OPT                     01951000
         DC    H'0145',H'0001',H'452'  PRIM_BAL_OPT_Q                   01952000
         DC    H'0146',H'0001',H'452'  PRIM_BAL_OPT_I                   01953000
         DC    H'0147',H'0001',H'452'  ACCT_REL_CD_Q                    01954000
         DC    H'0148',H'0001',H'452'  ACCT_REL_CD_I                    01955000
         DC    H'0149',H'0001',H'452'  ACCT_REL_Q                       01956000
         DC    H'0150',H'0001',H'452'  ACCT_REL_INCEN                   01957000
         DC    H'0151',H'0006',H'452'  CMB_CATG_CD_1                    01958000
         DC    H'0157',H'0001',H'452'  CMB_CATG_1_Q                     01959000
         DC    H'0158',H'0001',H'452'  CMB_CATG_1_INC                   01960000
         DC    H'0159',H'0006',H'452'  CMB_CATG_CD_2                    01961000
         DC    H'0165',H'0001',H'452'  CMB_CATG_2_Q                     01962000
         DC    H'0166',H'0001',H'452'  CMB_CATG_2_INC                   01963000
         DC    H'0167',H'0001',H'452'  PRIME_CMB_OPT_1                  01964000
         DC    H'0168',H'0001',H'452'  PRIME_C_1_QLFY                   01965000
         DC    H'0169',H'0001',H'452'  PRIME_C_1_INCEN                  01966000
         DC    H'0170',H'0001',H'452'  PRIME_CMB_OPT_2                  01967000
         DC    H'0171',H'0001',H'452'  PRIME_C_2_QLFY                   01968000
         DC    H'0172',H'0001',H'452'  PRIME_C_2_INCEN                  01969000
         DC    H'0173',H'0002',H'452'  PROD_GRP_MIN_1                   01970000
         DC    H'0175',H'0002',H'452'  PROD_GRP_MAX_1                   01971000
         DC    H'0177',H'0001',H'452'  PROD_GRP_1_Q                     01972000
         DC    H'0178',H'0002',H'452'  PROD_GRP_MIN_2                   01973000
         DC    H'0180',H'0002',H'452'  PROD_GRP_MAX_2                   01974000
         DC    H'0182',H'0001',H'452'  PROD_GRP_2_Q                     01975000
         DC    H'0183',H'0002',H'452'  PROD_GRP_MIN_3                   01976000
         DC    H'0185',H'0002',H'452'  PROD_GRP_MAX_3                   01977000
         DC    H'0187',H'0001',H'452'  PROD_GRP_3_Q                     01978000
         DC    H'0188',H'0002',H'452'  PROD_GRP_MIN_4                   01979000
         DC    H'0190',H'0002',H'452'  PROD_GRP_MAX_4                   01980000
         DC    H'0192',H'0001',H'452'  PROD_GRP_4_Q                     01981000
         DC    H'0193',H'0002',H'452'  PROD_GRP_MIN_5                   01982000
         DC    H'0195',H'0002',H'452'  PROD_GRP_MAX_5                   01983000
         DC    H'0197',H'0001',H'452'  PROD_GRP_5_Q                     01984000
         DC    H'0198',H'0001',H'452'  ACCT_INCEN_OVRD                  01985000
         DC    H'0199',H'0010',H'452'  DEFAULT_MODEL                    01986000
         DC    H'0209',H'0001',H'452'  INCEN_BAL_TRN                    01987000
         DC    H'0210',H'0006',H'452'  TRAN_PROFILE                     01988000
         DC    H'0216',H'0001',H'452'  TRAN_INCENTIVE                   01989000
         DC    H'0217',X'0B02',H'484'  CASH_MAX_AMT_PD                  01990000
         DC    H'0223',X'0F02',H'484'  CASH_MAX_BAL                     01991000
         DC    H'0231',H'0006',H'452'  CMB_CATG_CD_3                    01992000
         DC    H'0237',H'0001',H'452'  CMB_CATG_3_Q                     01993000
         DC    H'0238',H'0001',H'452'  CMB_CATG_3_INC                   01994000
         DC    H'0239',H'0006',H'452'  CMB_CATG_CD_4                    01995000
         DC    H'0245',H'0001',H'452'  CMB_CATG_4_Q                     01996000
         DC    H'0246',H'0001',H'452'  CMB_CATG_4_INC                   01997000
         DC    H'0247',H'0001',H'452'  PRIME_CMB_OPT_3                  01998000
         DC    H'0248',H'0001',H'452'  PRIME_C_3_QLFY                   01999000
         DC    H'0249',H'0001',H'452'  PRIME_C_3_INCEN                  02000000
         DC    H'0250',H'0001',H'452'  PRIME_CMB_OPT_4                  02001000
         DC    H'0251',H'0001',H'452'  PRIME_C_4_QLFY                   02002000
         DC    H'0252',H'0001',H'452'  PRIME_C_4_INCEN                  02003000
         DC    H'0253',H'0001',H'452'  RSV_FLAG_1                       02004000
         DC    H'0254',H'0001',H'452'  CLOSED_ACCT_OPT                  02005000
         DC    H'0255',H'0001',H'452'  SEC_SVC_OPT                      02006000
         DC    H'0256',H'0001',H'452'  RATE_OPTION                      02007000
         DC    H'0257',H'0002',H'452'  ENROLL_LEFT                      02008000
         DC    H'0259',H'0010',H'452'  MODEL_TYPE                       02009000
         DC    H'0269',H'0050',H'452'  LONG_DESC                        02010000
         DC    H'0319',H'0006',H'452'  PRIM_REL_CODE                    02011000
         DC    H'0325',H'0001',H'452'  PRIM_QLFY                        02012000
         DC    H'0326',H'0001',H'452'  EXTL_ENROLL                      02013000
TBLCOLMS EQU   (*-CONVDATA)/6          NUMBER OF COLUMNS IN TABLE       02014000
KY0COLMS EQU   00004                   NUMBER OF PRIMARY KEY COLUMNS    02015000
KY1COLMS EQU   00000                   NUMBER OF ALT 1 KEY COLUMNS      02016000
KY2COLMS EQU   00000                   NUMBER OF ALT 2 KEY COLUMNS      02017000
KY3COLMS EQU   00000                   NUMBER OF ALT 3 KEY COLUMNS      02018000
*                                                                       02019000
         LTORG                                                          02020000
         END                                                            02021000
