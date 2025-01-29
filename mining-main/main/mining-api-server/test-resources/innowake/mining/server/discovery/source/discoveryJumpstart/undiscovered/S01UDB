**********************************************************************  00001000
*                                                                       00002000
*  S01UDB .... STATIC SQL UPDATE MODULE                                 00003000
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
* PROGRAM TABLE HEADER SECTION:                                         00173000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00174000
**********************************************************************  00175000
*                                                                       00176000
S01UDB   CSECT                         PROGRAM TABLE SECTION            00177000
S01UDB   AMODE ANY                                                      00178000
S01UDB   RMODE ANY                                                      00179000
         DC    CL8'S01UDB  '           PROGRAM ID                       00180000
         DC    CL1' '                                                   00181000
         DC    CL8'&SYSDATE'           ASSEMBLY DATE                    00182000
         DC    CL1' '                                                   00183000
         DC    CL5'&SYSTIME'           ASSEMBLY TIME                    00184000
         DC    CL1' '                                                   00185000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00186000
         DC    5A(0)                   RESERVED                         00187000
         DC    AL2(0)                  RESERVED                         00188000
         DC    AL2(INDVARL)            NULL INDICATOR AREA LENGTH       00189000
         DC    A(CONVTAB1)             RECORD/HOST CONVERSION TABLE     00190000
         DC    A(0)                    SQLDA DATA TYPE/LENGTH TABLE     00191000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00192000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00193000
         DC    CL29'WWW.INFOR.COM                '                      00193001
*                                                                       00194000
**********************************************************************  00195000
* STATEMENT TABLE SECTION:                                              00196000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00197000
**********************************************************************  00198000
*                                                                       00199000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00200000
STM#TAB  AMODE ANY                                                      00201000
STM#TAB  RMODE ANY                                                      00202000
         DC    4A(0)                   RDB MODULE VECTORS               00203000
         DC    A(SELUC0)               SELECT UPDATE CURSOR (KEY 0)     00204000
         DC    A(SELUC1)               SELECT UPDATE CURSOR (KEY 1)     00205000
         DC    A(SELUC2)               SELECT UPDATE CURSOR (KEY 2)     00206000
         DC    A(SELUC3)               SELECT UPDATE CURSOR (KEY 3)     00207000
         DC    A(FETUC0)               FETCH UPDATE CURSOR (KEY 0)      00208000
         DC    A(FETUC1)               FETCH UPDATE CURSOR (KEY 1)      00209000
         DC    A(FETUC2)               FETCH UPDATE CURSOR (KEY 2)      00210000
         DC    A(FETUC3)               FETCH UPDATE CURSOR (KEY 3)      00211000
         DC    A(CLSUC0)               CLOSE UPDATE CURSOR (KEY 0)      00212000
         DC    A(CLSUC1)               CLOSE UPDATE CURSOR (KEY 1)      00213000
         DC    A(CLSUC2)               CLOSE UPDATE CURSOR (KEY 2)      00214000
         DC    A(CLSUC3)               CLOSE UPDATE CURSOR (KEY 3)      00215000
         DC    20A(0)                  RDB MODULE VECTORS               00216000
         DC    A(INSROW)               INSERT STATEMENT                 00217000
         DC    A(UPDUC0)               UPDATE STATEMENT (KEY 0)         00218000
         DC    A(UPDUC1)               UPDATE STATEMENT (KEY 1)         00219000
         DC    A(UPDUC2)               UPDATE STATEMENT (KEY 2)         00220000
         DC    A(UPDUC3)               UPDATE STATEMENT (KEY 3)         00221000
         DC    A(DELUC0)               DELETE STATEMENT (KEY 0)         00222000
         DC    A(DELUC1)               DELETE STATEMENT (KEY 1)         00223000
         DC    A(DELUC2)               DELETE STATEMENT (KEY 2)         00224000
         DC    A(DELUC3)               DELETE STATEMENT (KEY 3)         00225000
         DC    A(DELTBL)               DELETE ALL STATEMENT             00226000
         DC    4X'FF'                                                   00227000
*                                                                       00228000
**********************************************************************  00229000
* SQL STATEMENT SECTION:                                                00230000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00231000
*     TO SUPPORT THIS TABLE.                                            00232000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00233000
*     ARE EQUIVALENT.                                                   00234000
**********************************************************************  00235000
*                                                                       00236000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00237000
SQL#STMT AMODE ANY                                                      00238000
SQL#STMT RMODE ANY                                                      00239000
         USING SQLDSECT,10,3,4         ADDRESS SQLDSECT                 00240000
         USING COM#AREA,11             ADDRESS COMMAREA                 00241000
*                                                                       00242000
**********************************************************************  00243000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY PRIMARY KEY:       00244000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00245000
*   THEY ARE ALSO USED AFTER A SUCCESSFUL SELECT SEQUENTIAL STATEMENT   00246000
*     FOR THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS.                      00247000
**********************************************************************  00248000
*                                                                       00249000
SELUC0   DS    0H                                                       00250000
         USING SELUC0,12,8             ESTABLISH BASE REGISTER          00251000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00252000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00253000
         BALR  14,15                   MOVE REQUESTED DATA              00254000
         B     *+6                     BRANCH AROUND ADCON              00255000
BASSEL0  DC    AL2(4096)                                                00256000
         LR    8,12                    LOAD SECOND BASE                 00257000
         AH    8,BASSEL0               ADD 4K                           00258000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00259000
         AH    3,BASSEL0               ADD 4K                           00260000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00261000
         AH    4,BASSEL0               ADD 4K                           00262000
         EXEC  SQL DECLARE S01UPD0 CURSOR                              *00263000
               FOR SELECT                                              *00264000
                   AUDIT_DATE,                                         *00265000
                   AUDIT_TIME,                                         *00266000
                   AUDIT_USER,                                         *00267000
                   AUDIT_ORG,                                          *00268000
                   ACTIVE_CODE,                                        *00269000
                   SHORT_DESC,                                         *00270000
                   SCORE,                                              *00271000
                   AUTO_ENROLL,                                        *00272000
                   INCEN_BAL_OPT,                                      *00273000
                   PROCESS_OPTION,                                     *00274000
                   CMB_BAL_OPT,                                        *00275000
                   EXPIRE_FREQ,                                        *00276000
                   EXPIRE_TERM,                                        *00277000
                   END_MONTH,                                          *00278000
                   CLOSE_DATE,                                         *00279000
                   EXPIRE_DATE,                                        *00280000
                   EXPIRE_RET_DAYS,                                    *00281000
                   FIRST_RETENTION,                                    *00282000
                   DAY_PER_EXTN,                                       *00283000
                   EXTENSION,                                          *00284000
                   CASH_OPT,                                           *00285000
                   CASH_PCT,                                           *00286000
                   CASH_BAL_OPT,                                       *00287000
                   CASH_FREQ,                                          *00288000
                   CASH_TERM,                                          *00289000
                   CASH_DAY,                                           *00290000
                   NBR_ACCOUNTS,                                       *00291000
                   ACCT_QLFY,                                          *00292000
                   NBR_PROD,                                           *00293000
                   PROD_QLFY,                                          *00294000
                   MIN_ACCT,                                           *00295000
                   MIN_ACCT_Q,                                         *00296000
                   MAX_ACCT,                                           *00297000
                   MAX_ACCT_Q,                                         *00298000
                   MIN_PROD,                                           *00299000
                   MIN_PROD_Q,                                         *00300000
                   MAX_PROD,                                           *00301000
                   MAX_PROD_Q,                                         *00302000
                   DEMOGR_CODE,                                        *00303000
                   DEMOGR_QLFY,                                        *00304000
                   CUST_REL_CODE,                                      *00305000
                   CUST_QLFY,                                          *00306000
                   PRIM_BAL_OPT,                                       *00307000
                   PRIM_BAL_OPT_Q,                                     *00308000
                   PRIM_BAL_OPT_I,                                     *00309000
                   ACCT_REL_CD_Q,                                      *00310000
                   ACCT_REL_CD_I,                                      *00311000
                   ACCT_REL_Q,                                         *00312000
                   ACCT_REL_INCEN,                                     *00313000
                   CMB_CATG_CD_1,                                      *00314000
                   CMB_CATG_1_Q,                                       *00315000
                   CMB_CATG_1_INC,                                     *00316000
                   CMB_CATG_CD_2,                                      *00317000
                   CMB_CATG_2_Q,                                       *00318000
                   CMB_CATG_2_INC,                                     *00319000
                   PRIME_CMB_OPT_1,                                    *00320000
                   PRIME_C_1_QLFY,                                     *00321000
                   PRIME_C_1_INCEN,                                    *00322000
                   PRIME_CMB_OPT_2,                                    *00323000
                   PRIME_C_2_QLFY,                                     *00324000
                   PRIME_C_2_INCEN,                                    *00325000
                   PROD_GRP_MIN_1,                                     *00326000
                   PROD_GRP_MAX_1,                                     *00327000
                   PROD_GRP_1_Q,                                       *00328000
                   PROD_GRP_MIN_2,                                     *00329000
                   PROD_GRP_MAX_2,                                     *00330000
                   PROD_GRP_2_Q,                                       *00331000
                   PROD_GRP_MIN_3,                                     *00332000
                   PROD_GRP_MAX_3,                                     *00333000
                   PROD_GRP_3_Q,                                       *00334000
                   PROD_GRP_MIN_4,                                     *00335000
                   PROD_GRP_MAX_4,                                     *00336000
                   PROD_GRP_4_Q,                                       *00337000
                   PROD_GRP_MIN_5,                                     *00338000
                   PROD_GRP_MAX_5,                                     *00339000
                   PROD_GRP_5_Q,                                       *00340000
                   ACCT_INCEN_OVRD,                                    *00341000
                   DEFAULT_MODEL,                                      *00342000
                   INCEN_BAL_TRN,                                      *00343000
                   TRAN_PROFILE,                                       *00344000
                   TRAN_INCENTIVE,                                     *00345000
                   CASH_MAX_AMT_PD,                                    *00346000
                   CASH_MAX_BAL,                                       *00347000
                   CMB_CATG_CD_3,                                      *00348000
                   CMB_CATG_3_Q,                                       *00349000
                   CMB_CATG_3_INC,                                     *00350000
                   CMB_CATG_CD_4,                                      *00351000
                   CMB_CATG_4_Q,                                       *00352000
                   CMB_CATG_4_INC,                                     *00353000
                   PRIME_CMB_OPT_3,                                    *00354000
                   PRIME_C_3_QLFY,                                     *00355000
                   PRIME_C_3_INCEN,                                    *00356000
                   PRIME_CMB_OPT_4,                                    *00357000
                   PRIME_C_4_QLFY,                                     *00358000
                   PRIME_C_4_INCEN,                                    *00359000
                   RSV_FLAG_1,                                         *00360000
                   CLOSED_ACCT_OPT,                                    *00361000
                   SEC_SVC_OPT,                                        *00362000
                   RATE_OPTION,                                        *00363000
                   ENROLL_LEFT,                                        *00364000
                   MODEL_TYPE,                                         *00365000
                   LONG_DESC,                                          *00366000
                   PRIM_REL_CODE,                                      *00367000
                   PRIM_QLFY,                                          *00368000
                   EXTL_ENROLL                                         *00369000
                 FROM S01                                              *00370000
                 WHERE                                                 *00371000
                   INST_NBR = :INST AND                                *00372000
                   RECORD_NBR = :RECNBR AND                            *00373000
                   MODEL = :MODEL AND                                  *00374000
                   EFFECTIVE_DATE = :EFFDATE                           *00375000
                 FOR UPDATE OF                                         *00376000
                   AUDIT_DATE,                                         *00377000
                   AUDIT_TIME,                                         *00378000
                   AUDIT_USER,                                         *00379000
                   AUDIT_ORG,                                          *00380000
                   ACTIVE_CODE,                                        *00381000
                   SHORT_DESC,                                         *00382000
                   SCORE,                                              *00383000
                   AUTO_ENROLL,                                        *00384000
                   INCEN_BAL_OPT,                                      *00385000
                   PROCESS_OPTION,                                     *00386000
                   CMB_BAL_OPT,                                        *00387000
                   EXPIRE_FREQ,                                        *00388000
                   EXPIRE_TERM,                                        *00389000
                   END_MONTH,                                          *00390000
                   CLOSE_DATE,                                         *00391000
                   EXPIRE_DATE,                                        *00392000
                   EXPIRE_RET_DAYS,                                    *00393000
                   FIRST_RETENTION,                                    *00394000
                   DAY_PER_EXTN,                                       *00395000
                   EXTENSION,                                          *00396000
                   CASH_OPT,                                           *00397000
                   CASH_PCT,                                           *00398000
                   CASH_BAL_OPT,                                       *00399000
                   CASH_FREQ,                                          *00400000
                   CASH_TERM,                                          *00401000
                   CASH_DAY,                                           *00402000
                   NBR_ACCOUNTS,                                       *00403000
                   ACCT_QLFY,                                          *00404000
                   NBR_PROD,                                           *00405000
                   PROD_QLFY,                                          *00406000
                   MIN_ACCT,                                           *00407000
                   MIN_ACCT_Q,                                         *00408000
                   MAX_ACCT,                                           *00409000
                   MAX_ACCT_Q,                                         *00410000
                   MIN_PROD,                                           *00411000
                   MIN_PROD_Q,                                         *00412000
                   MAX_PROD,                                           *00413000
                   MAX_PROD_Q,                                         *00414000
                   DEMOGR_CODE,                                        *00415000
                   DEMOGR_QLFY,                                        *00416000
                   CUST_REL_CODE,                                      *00417000
                   CUST_QLFY,                                          *00418000
                   PRIM_BAL_OPT,                                       *00419000
                   PRIM_BAL_OPT_Q,                                     *00420000
                   PRIM_BAL_OPT_I,                                     *00421000
                   ACCT_REL_CD_Q,                                      *00422000
                   ACCT_REL_CD_I,                                      *00423000
                   ACCT_REL_Q,                                         *00424000
                   ACCT_REL_INCEN,                                     *00425000
                   CMB_CATG_CD_1,                                      *00426000
                   CMB_CATG_1_Q,                                       *00427000
                   CMB_CATG_1_INC,                                     *00428000
                   CMB_CATG_CD_2,                                      *00429000
                   CMB_CATG_2_Q,                                       *00430000
                   CMB_CATG_2_INC,                                     *00431000
                   PRIME_CMB_OPT_1,                                    *00432000
                   PRIME_C_1_QLFY,                                     *00433000
                   PRIME_C_1_INCEN,                                    *00434000
                   PRIME_CMB_OPT_2,                                    *00435000
                   PRIME_C_2_QLFY,                                     *00436000
                   PRIME_C_2_INCEN,                                    *00437000
                   PROD_GRP_MIN_1,                                     *00438000
                   PROD_GRP_MAX_1,                                     *00439000
                   PROD_GRP_1_Q,                                       *00440000
                   PROD_GRP_MIN_2,                                     *00441000
                   PROD_GRP_MAX_2,                                     *00442000
                   PROD_GRP_2_Q,                                       *00443000
                   PROD_GRP_MIN_3,                                     *00444000
                   PROD_GRP_MAX_3,                                     *00445000
                   PROD_GRP_3_Q,                                       *00446000
                   PROD_GRP_MIN_4,                                     *00447000
                   PROD_GRP_MAX_4,                                     *00448000
                   PROD_GRP_4_Q,                                       *00449000
                   PROD_GRP_MIN_5,                                     *00450000
                   PROD_GRP_MAX_5,                                     *00451000
                   PROD_GRP_5_Q,                                       *00452000
                   ACCT_INCEN_OVRD,                                    *00453000
                   DEFAULT_MODEL,                                      *00454000
                   INCEN_BAL_TRN,                                      *00455000
                   TRAN_PROFILE,                                       *00456000
                   TRAN_INCENTIVE,                                     *00457000
                   CASH_MAX_AMT_PD,                                    *00458000
                   CASH_MAX_BAL,                                       *00459000
                   CMB_CATG_CD_3,                                      *00460000
                   CMB_CATG_3_Q,                                       *00461000
                   CMB_CATG_3_INC,                                     *00462000
                   CMB_CATG_CD_4,                                      *00463000
                   CMB_CATG_4_Q,                                       *00464000
                   CMB_CATG_4_INC,                                     *00465000
                   PRIME_CMB_OPT_3,                                    *00466000
                   PRIME_C_3_QLFY,                                     *00467000
                   PRIME_C_3_INCEN,                                    *00468000
                   PRIME_CMB_OPT_4,                                    *00469000
                   PRIME_C_4_QLFY,                                     *00470000
                   PRIME_C_4_INCEN,                                    *00471000
                   RSV_FLAG_1,                                         *00472000
                   CLOSED_ACCT_OPT,                                    *00473000
                   SEC_SVC_OPT,                                        *00474000
                   RATE_OPTION,                                        *00475000
                   ENROLL_LEFT,                                        *00476000
                   MODEL_TYPE,                                         *00477000
                   LONG_DESC,                                          *00478000
                   PRIM_REL_CODE,                                      *00479000
                   PRIM_QLFY,                                          *00480000
                   EXTL_ENROLL                                         *00481000
                 FETCH FIRST 1 ROW ONLY                                 00482000
         EXEC  SQL OPEN S01UPD0                                         00483000
         MVC   SQWCSUCA,=A(CLSUC0)     SET CURSOR CLOSE ROUTINE ADDRESS 00484000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00485000
         BR    14                      RETURN TO CALLER                 00486000
         LTORG                                                          00487000
*                                                                       00488000
**********************************************************************  00489000
* FETCH FROM UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                   00490000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00491000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00492000
*     THE ACTUAL ROW.                                                   00493000
**********************************************************************  00494000
*                                                                       00495000
FETUC0   DS    0H                                                       00496000
         USING FETUC0,12,8             ESTABLISH BASE REGISTER          00497000
         B     *+6                     BRANCH AROUND ADCON              00498000
BASFET0  DC    AL2(4096)                                                00499000
         LR    8,12                    LOAD SECOND BASE                 00500000
         AH    8,BASFET0               ADD 4K                           00501000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00502000
         AH    3,BASFET0               ADD 4K                           00503000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00504000
         AH    4,BASFET0               ADD 4K                           00505000
         EXEC  SQL FETCH S01UPD0                                       *00506000
                 INTO                                                  *00507000
                   :AUDDATE,                                           *00508000
                   :AUDTIME,                                           *00509000
                   :AUDUSER,                                           *00510000
                   :AUDORG,                                            *00511000
                   :ACTVCDE,                                           *00512000
                   :SHRTDESC,                                          *00513000
                   :SCORE,                                             *00514000
                   :AUENROLL,                                          *00515000
                   :INCTVBAL,                                          *00516000
                   :PROCOPT,                                           *00517000
                   :CBALOPT,                                           *00518000
                   :EXPFREQ,                                           *00519000
                   :EXPTERM,                                           *00520000
                   :ENDMNTH,                                           *00521000
                   :CLSD,                                              *00522000
                   :EXPRDATE,                                          *00523000
                   :EXRETDAY,                                          *00524000
                   :FIRSTRET,                                          *00525000
                   :DAYSEXT,                                           *00526000
                   :EXTN,                                              *00527000
                   :CASHOPT,                                           *00528000
                   :CASHPCT,                                           *00529000
                   :CASHBAL,                                           *00530000
                   :CASHFREQ,                                          *00531000
                   :CASHTERM,                                          *00532000
                   :CASHDAY,                                           *00533000
                   :NBRACCT,                                           *00534000
                   :ACCTQLFY,                                          *00535000
                   :NBRPROD,                                           *00536000
                   :PRODQLFY,                                          *00537000
                   :MINACCT,                                           *00538000
                   :MINACCTQ,                                          *00539000
                   :MAXACCT,                                           *00540000
                   :MAXACCTQ,                                          *00541000
                   :MINPROD,                                           *00542000
                   :MINPRODQ,                                          *00543000
                   :MAXPROD,                                           *00544000
                   :MAXPRODQ,                                          *00545000
                   :DEMOCODE,                                          *00546000
                   :DEMOQLFY,                                          *00547000
                   :CUSTCODE,                                          *00548000
                   :CUSTQLFY,                                          *00549000
                   :PBALOPT,                                           *00550000
                   :PBALOPTQ,                                          *00551000
                   :PBALOPTI,                                          *00552000
                   :ACCTCDQ,                                           *00553000
                   :ACCTCDI,                                           *00554000
                   :ACTRELQ,                                           *00555000
                   :ACINCEN,                                           *00556000
                   :COMBCD1,                                           *00557000
                   :CCQLFY1,                                           *00558000
                   :CC1INCTV,                                          *00559000
                   :COMBCD2,                                           *00560000
                   :CCQLFY2,                                           *00561000
                   :CC2INCTV,                                          *00562000
                   :PRCOME1,                                           *00563000
                   :PC1QLFY,                                           *00564000
                   :PC1INCTV,                                          *00565000
                   :PRCOME2,                                           *00566000
                   :PC2QLFY,                                           *00567000
                   :PC2INCTV,                                          *00568000
                   :PGMIN1,                                            *00569000
                   :PGMAX1,                                            *00570000
                   :PRDGRP1Q,                                          *00571000
                   :PGMIN2,                                            *00572000
                   :PGMAX2,                                            *00573000
                   :PRDGRP2Q,                                          *00574000
                   :PGMIN3,                                            *00575000
                   :PGMAX3,                                            *00576000
                   :PRDGRP3Q,                                          *00577000
                   :PGMIN4,                                            *00578000
                   :PGMAX4,                                            *00579000
                   :PRDGRP4Q,                                          *00580000
                   :PGMIN5,                                            *00581000
                   :PGMAX5,                                            *00582000
                   :PRDGRP5Q,                                          *00583000
                   :INCENTOV,                                          *00584000
                   :DFLTMODL,                                          *00585000
                   :INCBALTR,                                          *00586000
                   :TRANPROF,                                          *00587000
                   :TRPINCEN,                                          *00588000
                   :CRWAMT,                                            *00589000
                   :CRWMAXCB,                                          *00590000
                   :COMBCD3,                                           *00591000
                   :CCQLFY3,                                           *00592000
                   :CC3INCTV,                                          *00593000
                   :COMBCD4,                                           *00594000
                   :CCQLFY4,                                           *00595000
                   :CC4INCTV,                                          *00596000
                   :PRCOME3,                                           *00597000
                   :PC3QLFY,                                           *00598000
                   :PC3INCTV,                                          *00599000
                   :PRCOME4,                                           *00600000
                   :PC4QLFY,                                           *00601000
                   :PC4INCTV,                                          *00602000
                   :RSVFLAG1,                                          *00603000
                   :CLACCOPT,                                          *00604000
                   :SECSVOPT,                                          *00605000
                   :RATEOPT,                                           *00606000
                   :ENRATTP,                                           *00607000
                   :MODELTYP,                                          *00608000
                   :LONGDESC,                                          *00609000
                   :PRIMCODE,                                          *00610000
                   :PRIMQLFY,                                          *00611000
                   :EXTENROL                                            00612000
         MVI   SQWKMRP,X'03'           MOVE HOST VARIABLES TO RECORD    00613000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00614000
         BALR  14,15                   MOVE REQUESTED DATA              00615000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00616000
         BR    14                      RETURN TO CALLER                 00617000
         LTORG                                                          00618000
*                                                                       00619000
**********************************************************************  00620000
* INSERT STATEMENT:                                                     00621000
*   THIS STATEMENT SUPPORTS THE PUT VERB.                               00622000
**********************************************************************  00623000
*                                                                       00624000
INSROW   DS    0H                                                       00625000
         USING INSROW,12,8             ESTABLISH BASE REGISTER          00626000
         MVI   SQWKMRP,X'01'           MOVE RECORD TO HOST VARIABLES    00627000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00628000
         BALR  14,15                   MOVE REQUESTED DATA              00629000
         B     *+6                     BRANCH AROUND ADCON              00630000
BASINS0  DC    AL2(4096)                                                00631000
         LR    8,12                    LOAD SECOND BASE                 00632000
         AH    8,BASINS0               ADD 4K                           00633000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00634000
         AH    3,BASINS0               ADD 4K                           00635000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00636000
         AH    4,BASINS0               ADD 4K                           00637000
         EXEC  SQL INSERT INTO S01                                     *00638000
                   (                                                   *00639000
                   INST_NBR,                                           *00640000
                   RECORD_NBR,                                         *00641000
                   MODEL,                                              *00642000
                   EFFECTIVE_DATE,                                     *00643000
                   AUDIT_DATE,                                         *00644000
                   AUDIT_TIME,                                         *00645000
                   AUDIT_USER,                                         *00646000
                   AUDIT_ORG,                                          *00647000
                   ACTIVE_CODE,                                        *00648000
                   SHORT_DESC,                                         *00649000
                   SCORE,                                              *00650000
                   AUTO_ENROLL,                                        *00651000
                   INCEN_BAL_OPT,                                      *00652000
                   PROCESS_OPTION,                                     *00653000
                   CMB_BAL_OPT,                                        *00654000
                   EXPIRE_FREQ,                                        *00655000
                   EXPIRE_TERM,                                        *00656000
                   END_MONTH,                                          *00657000
                   CLOSE_DATE,                                         *00658000
                   EXPIRE_DATE,                                        *00659000
                   EXPIRE_RET_DAYS,                                    *00660000
                   FIRST_RETENTION,                                    *00661000
                   DAY_PER_EXTN,                                       *00662000
                   EXTENSION,                                          *00663000
                   CASH_OPT,                                           *00664000
                   CASH_PCT,                                           *00665000
                   CASH_BAL_OPT,                                       *00666000
                   CASH_FREQ,                                          *00667000
                   CASH_TERM,                                          *00668000
                   CASH_DAY,                                           *00669000
                   NBR_ACCOUNTS,                                       *00670000
                   ACCT_QLFY,                                          *00671000
                   NBR_PROD,                                           *00672000
                   PROD_QLFY,                                          *00673000
                   MIN_ACCT,                                           *00674000
                   MIN_ACCT_Q,                                         *00675000
                   MAX_ACCT,                                           *00676000
                   MAX_ACCT_Q,                                         *00677000
                   MIN_PROD,                                           *00678000
                   MIN_PROD_Q,                                         *00679000
                   MAX_PROD,                                           *00680000
                   MAX_PROD_Q,                                         *00681000
                   DEMOGR_CODE,                                        *00682000
                   DEMOGR_QLFY,                                        *00683000
                   CUST_REL_CODE,                                      *00684000
                   CUST_QLFY,                                          *00685000
                   PRIM_BAL_OPT,                                       *00686000
                   PRIM_BAL_OPT_Q,                                     *00687000
                   PRIM_BAL_OPT_I,                                     *00688000
                   ACCT_REL_CD_Q,                                      *00689000
                   ACCT_REL_CD_I,                                      *00690000
                   ACCT_REL_Q,                                         *00691000
                   ACCT_REL_INCEN,                                     *00692000
                   CMB_CATG_CD_1,                                      *00693000
                   CMB_CATG_1_Q,                                       *00694000
                   CMB_CATG_1_INC,                                     *00695000
                   CMB_CATG_CD_2,                                      *00696000
                   CMB_CATG_2_Q,                                       *00697000
                   CMB_CATG_2_INC,                                     *00698000
                   PRIME_CMB_OPT_1,                                    *00699000
                   PRIME_C_1_QLFY,                                     *00700000
                   PRIME_C_1_INCEN,                                    *00701000
                   PRIME_CMB_OPT_2,                                    *00702000
                   PRIME_C_2_QLFY,                                     *00703000
                   PRIME_C_2_INCEN,                                    *00704000
                   PROD_GRP_MIN_1,                                     *00705000
                   PROD_GRP_MAX_1,                                     *00706000
                   PROD_GRP_1_Q,                                       *00707000
                   PROD_GRP_MIN_2,                                     *00708000
                   PROD_GRP_MAX_2,                                     *00709000
                   PROD_GRP_2_Q,                                       *00710000
                   PROD_GRP_MIN_3,                                     *00711000
                   PROD_GRP_MAX_3,                                     *00712000
                   PROD_GRP_3_Q,                                       *00713000
                   PROD_GRP_MIN_4,                                     *00714000
                   PROD_GRP_MAX_4,                                     *00715000
                   PROD_GRP_4_Q,                                       *00716000
                   PROD_GRP_MIN_5,                                     *00717000
                   PROD_GRP_MAX_5,                                     *00718000
                   PROD_GRP_5_Q,                                       *00719000
                   ACCT_INCEN_OVRD,                                    *00720000
                   DEFAULT_MODEL,                                      *00721000
                   INCEN_BAL_TRN,                                      *00722000
                   TRAN_PROFILE,                                       *00723000
                   TRAN_INCENTIVE,                                     *00724000
                   CASH_MAX_AMT_PD,                                    *00725000
                   CASH_MAX_BAL,                                       *00726000
                   CMB_CATG_CD_3,                                      *00727000
                   CMB_CATG_3_Q,                                       *00728000
                   CMB_CATG_3_INC,                                     *00729000
                   CMB_CATG_CD_4,                                      *00730000
                   CMB_CATG_4_Q,                                       *00731000
                   CMB_CATG_4_INC,                                     *00732000
                   PRIME_CMB_OPT_3,                                    *00733000
                   PRIME_C_3_QLFY,                                     *00734000
                   PRIME_C_3_INCEN,                                    *00735000
                   PRIME_CMB_OPT_4,                                    *00736000
                   PRIME_C_4_QLFY,                                     *00737000
                   PRIME_C_4_INCEN,                                    *00738000
                   RSV_FLAG_1,                                         *00739000
                   CLOSED_ACCT_OPT,                                    *00740000
                   SEC_SVC_OPT,                                        *00741000
                   RATE_OPTION,                                        *00742000
                   ENROLL_LEFT,                                        *00743000
                   MODEL_TYPE,                                         *00744000
                   LONG_DESC,                                          *00745000
                   PRIM_REL_CODE,                                      *00746000
                   PRIM_QLFY,                                          *00747000
                   EXTL_ENROLL                                         *00748000
                   )                                                   *00749000
                  VALUES                                               *00750000
                   (                                                   *00751000
                   :INST,                                              *00752000
                   :RECNBR,                                            *00753000
                   :MODEL,                                             *00754000
                   :EFFDATE,                                           *00755000
                   :AUDDATE,                                           *00756000
                   :AUDTIME,                                           *00757000
                   :AUDUSER,                                           *00758000
                   :AUDORG,                                            *00759000
                   :ACTVCDE,                                           *00760000
                   :SHRTDESC,                                          *00761000
                   :SCORE,                                             *00762000
                   :AUENROLL,                                          *00763000
                   :INCTVBAL,                                          *00764000
                   :PROCOPT,                                           *00765000
                   :CBALOPT,                                           *00766000
                   :EXPFREQ,                                           *00767000
                   :EXPTERM,                                           *00768000
                   :ENDMNTH,                                           *00769000
                   :CLSD,                                              *00770000
                   :EXPRDATE,                                          *00771000
                   :EXRETDAY,                                          *00772000
                   :FIRSTRET,                                          *00773000
                   :DAYSEXT,                                           *00774000
                   :EXTN,                                              *00775000
                   :CASHOPT,                                           *00776000
                   :CASHPCT,                                           *00777000
                   :CASHBAL,                                           *00778000
                   :CASHFREQ,                                          *00779000
                   :CASHTERM,                                          *00780000
                   :CASHDAY,                                           *00781000
                   :NBRACCT,                                           *00782000
                   :ACCTQLFY,                                          *00783000
                   :NBRPROD,                                           *00784000
                   :PRODQLFY,                                          *00785000
                   :MINACCT,                                           *00786000
                   :MINACCTQ,                                          *00787000
                   :MAXACCT,                                           *00788000
                   :MAXACCTQ,                                          *00789000
                   :MINPROD,                                           *00790000
                   :MINPRODQ,                                          *00791000
                   :MAXPROD,                                           *00792000
                   :MAXPRODQ,                                          *00793000
                   :DEMOCODE,                                          *00794000
                   :DEMOQLFY,                                          *00795000
                   :CUSTCODE,                                          *00796000
                   :CUSTQLFY,                                          *00797000
                   :PBALOPT,                                           *00798000
                   :PBALOPTQ,                                          *00799000
                   :PBALOPTI,                                          *00800000
                   :ACCTCDQ,                                           *00801000
                   :ACCTCDI,                                           *00802000
                   :ACTRELQ,                                           *00803000
                   :ACINCEN,                                           *00804000
                   :COMBCD1,                                           *00805000
                   :CCQLFY1,                                           *00806000
                   :CC1INCTV,                                          *00807000
                   :COMBCD2,                                           *00808000
                   :CCQLFY2,                                           *00809000
                   :CC2INCTV,                                          *00810000
                   :PRCOME1,                                           *00811000
                   :PC1QLFY,                                           *00812000
                   :PC1INCTV,                                          *00813000
                   :PRCOME2,                                           *00814000
                   :PC2QLFY,                                           *00815000
                   :PC2INCTV,                                          *00816000
                   :PGMIN1,                                            *00817000
                   :PGMAX1,                                            *00818000
                   :PRDGRP1Q,                                          *00819000
                   :PGMIN2,                                            *00820000
                   :PGMAX2,                                            *00821000
                   :PRDGRP2Q,                                          *00822000
                   :PGMIN3,                                            *00823000
                   :PGMAX3,                                            *00824000
                   :PRDGRP3Q,                                          *00825000
                   :PGMIN4,                                            *00826000
                   :PGMAX4,                                            *00827000
                   :PRDGRP4Q,                                          *00828000
                   :PGMIN5,                                            *00829000
                   :PGMAX5,                                            *00830000
                   :PRDGRP5Q,                                          *00831000
                   :INCENTOV,                                          *00832000
                   :DFLTMODL,                                          *00833000
                   :INCBALTR,                                          *00834000
                   :TRANPROF,                                          *00835000
                   :TRPINCEN,                                          *00836000
                   :CRWAMT,                                            *00837000
                   :CRWMAXCB,                                          *00838000
                   :COMBCD3,                                           *00839000
                   :CCQLFY3,                                           *00840000
                   :CC3INCTV,                                          *00841000
                   :COMBCD4,                                           *00842000
                   :CCQLFY4,                                           *00843000
                   :CC4INCTV,                                          *00844000
                   :PRCOME3,                                           *00845000
                   :PC3QLFY,                                           *00846000
                   :PC3INCTV,                                          *00847000
                   :PRCOME4,                                           *00848000
                   :PC4QLFY,                                           *00849000
                   :PC4INCTV,                                          *00850000
                   :RSVFLAG1,                                          *00851000
                   :CLACCOPT,                                          *00852000
                   :SECSVOPT,                                          *00853000
                   :RATEOPT,                                           *00854000
                   :ENRATTP,                                           *00855000
                   :MODELTYP,                                          *00856000
                   :LONGDESC,                                          *00857000
                   :PRIMCODE,                                          *00858000
                   :PRIMQLFY,                                          *00859000
                   :EXTENROL                                           *00860000
                   )                                                    00861000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00862000
         BR    14                      RETURN TO CALLER                 00863000
         LTORG                                                          00864000
*                                                                       00865000
**********************************************************************  00866000
* UPDATE STATEMENT BY PRIMARY KEY:                                      00867000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             00868000
**********************************************************************  00869000
*                                                                       00870000
UPDUC0   DS    0H                                                       00871000
         USING UPDUC0,12,8             ESTABLISH BASE REGISTER          00872000
         MVI   SQWKMRP,X'01'           MOVE RECORD TO HOST VARIABLES    00873000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00874000
         BALR  14,15                   MOVE REQUESTED DATA              00875000
         B     *+6                     BRANCH AROUND ADCON              00876000
BASUPD0  DC    AL2(4096)                                                00877000
         LR    8,12                    LOAD SECOND BASE                 00878000
         AH    8,BASUPD0               ADD 4K                           00879000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00880000
         AH    3,BASUPD0               ADD 4K                           00881000
         LR    4,3                     LOAD THIRD  BASE FOR SQLDSECT    00882000
         AH    4,BASUPD0               ADD 4K                           00883000
         EXEC  SQL UPDATE S01                                          *00884000
                   SET                                                 *00885000
                     AUDIT_DATE = :AUDDATE,                            *00886000
                     AUDIT_TIME = :AUDTIME,                            *00887000
                     AUDIT_USER = :AUDUSER,                            *00888000
                     AUDIT_ORG = :AUDORG,                              *00889000
                     ACTIVE_CODE = :ACTVCDE,                           *00890000
                     SHORT_DESC = :SHRTDESC,                           *00891000
                     SCORE = :SCORE,                                   *00892000
                     AUTO_ENROLL = :AUENROLL,                          *00893000
                     INCEN_BAL_OPT = :INCTVBAL,                        *00894000
                     PROCESS_OPTION = :PROCOPT,                        *00895000
                     CMB_BAL_OPT = :CBALOPT,                           *00896000
                     EXPIRE_FREQ = :EXPFREQ,                           *00897000
                     EXPIRE_TERM = :EXPTERM,                           *00898000
                     END_MONTH = :ENDMNTH,                             *00899000
                     CLOSE_DATE = :CLSD,                               *00900000
                     EXPIRE_DATE = :EXPRDATE,                          *00901000
                     EXPIRE_RET_DAYS = :EXRETDAY,                      *00902000
                     FIRST_RETENTION = :FIRSTRET,                      *00903000
                     DAY_PER_EXTN = :DAYSEXT,                          *00904000
                     EXTENSION = :EXTN,                                *00905000
                     CASH_OPT = :CASHOPT,                              *00906000
                     CASH_PCT = :CASHPCT,                              *00907000
                     CASH_BAL_OPT = :CASHBAL,                          *00908000
                     CASH_FREQ = :CASHFREQ,                            *00909000
                     CASH_TERM = :CASHTERM,                            *00910000
                     CASH_DAY = :CASHDAY,                              *00911000
                     NBR_ACCOUNTS = :NBRACCT,                          *00912000
                     ACCT_QLFY = :ACCTQLFY,                            *00913000
                     NBR_PROD = :NBRPROD,                              *00914000
                     PROD_QLFY = :PRODQLFY,                            *00915000
                     MIN_ACCT = :MINACCT,                              *00916000
                     MIN_ACCT_Q = :MINACCTQ,                           *00917000
                     MAX_ACCT = :MAXACCT,                              *00918000
                     MAX_ACCT_Q = :MAXACCTQ,                           *00919000
                     MIN_PROD = :MINPROD,                              *00920000
                     MIN_PROD_Q = :MINPRODQ,                           *00921000
                     MAX_PROD = :MAXPROD,                              *00922000
                     MAX_PROD_Q = :MAXPRODQ,                           *00923000
                     DEMOGR_CODE = :DEMOCODE,                          *00924000
                     DEMOGR_QLFY = :DEMOQLFY,                          *00925000
                     CUST_REL_CODE = :CUSTCODE,                        *00926000
                     CUST_QLFY = :CUSTQLFY,                            *00927000
                     PRIM_BAL_OPT = :PBALOPT,                          *00928000
                     PRIM_BAL_OPT_Q = :PBALOPTQ,                       *00929000
                     PRIM_BAL_OPT_I = :PBALOPTI,                       *00930000
                     ACCT_REL_CD_Q = :ACCTCDQ,                         *00931000
                     ACCT_REL_CD_I = :ACCTCDI,                         *00932000
                     ACCT_REL_Q = :ACTRELQ,                            *00933000
                     ACCT_REL_INCEN = :ACINCEN,                        *00934000
                     CMB_CATG_CD_1 = :COMBCD1,                         *00935000
                     CMB_CATG_1_Q = :CCQLFY1,                          *00936000
                     CMB_CATG_1_INC = :CC1INCTV,                       *00937000
                     CMB_CATG_CD_2 = :COMBCD2,                         *00938000
                     CMB_CATG_2_Q = :CCQLFY2,                          *00939000
                     CMB_CATG_2_INC = :CC2INCTV,                       *00940000
                     PRIME_CMB_OPT_1 = :PRCOME1,                       *00941000
                     PRIME_C_1_QLFY = :PC1QLFY,                        *00942000
                     PRIME_C_1_INCEN = :PC1INCTV,                      *00943000
                     PRIME_CMB_OPT_2 = :PRCOME2,                       *00944000
                     PRIME_C_2_QLFY = :PC2QLFY,                        *00945000
                     PRIME_C_2_INCEN = :PC2INCTV,                      *00946000
                     PROD_GRP_MIN_1 = :PGMIN1,                         *00947000
                     PROD_GRP_MAX_1 = :PGMAX1,                         *00948000
                     PROD_GRP_1_Q = :PRDGRP1Q,                         *00949000
                     PROD_GRP_MIN_2 = :PGMIN2,                         *00950000
                     PROD_GRP_MAX_2 = :PGMAX2,                         *00951000
                     PROD_GRP_2_Q = :PRDGRP2Q,                         *00952000
                     PROD_GRP_MIN_3 = :PGMIN3,                         *00953000
                     PROD_GRP_MAX_3 = :PGMAX3,                         *00954000
                     PROD_GRP_3_Q = :PRDGRP3Q,                         *00955000
                     PROD_GRP_MIN_4 = :PGMIN4,                         *00956000
                     PROD_GRP_MAX_4 = :PGMAX4,                         *00957000
                     PROD_GRP_4_Q = :PRDGRP4Q,                         *00958000
                     PROD_GRP_MIN_5 = :PGMIN5,                         *00959000
                     PROD_GRP_MAX_5 = :PGMAX5,                         *00960000
                     PROD_GRP_5_Q = :PRDGRP5Q,                         *00961000
                     ACCT_INCEN_OVRD = :INCENTOV,                      *00962000
                     DEFAULT_MODEL = :DFLTMODL,                        *00963000
                     INCEN_BAL_TRN = :INCBALTR,                        *00964000
                     TRAN_PROFILE = :TRANPROF,                         *00965000
                     TRAN_INCENTIVE = :TRPINCEN,                       *00966000
                     CASH_MAX_AMT_PD = :CRWAMT,                        *00967000
                     CASH_MAX_BAL = :CRWMAXCB,                         *00968000
                     CMB_CATG_CD_3 = :COMBCD3,                         *00969000
                     CMB_CATG_3_Q = :CCQLFY3,                          *00970000
                     CMB_CATG_3_INC = :CC3INCTV,                       *00971000
                     CMB_CATG_CD_4 = :COMBCD4,                         *00972000
                     CMB_CATG_4_Q = :CCQLFY4,                          *00973000
                     CMB_CATG_4_INC = :CC4INCTV,                       *00974000
                     PRIME_CMB_OPT_3 = :PRCOME3,                       *00975000
                     PRIME_C_3_QLFY = :PC3QLFY,                        *00976000
                     PRIME_C_3_INCEN = :PC3INCTV,                      *00977000
                     PRIME_CMB_OPT_4 = :PRCOME4,                       *00978000
                     PRIME_C_4_QLFY = :PC4QLFY,                        *00979000
                     PRIME_C_4_INCEN = :PC4INCTV,                      *00980000
                     RSV_FLAG_1 = :RSVFLAG1,                           *00981000
                     CLOSED_ACCT_OPT = :CLACCOPT,                      *00982000
                     SEC_SVC_OPT = :SECSVOPT,                          *00983000
                     RATE_OPTION = :RATEOPT,                           *00984000
                     ENROLL_LEFT = :ENRATTP,                           *00985000
                     MODEL_TYPE = :MODELTYP,                           *00986000
                     LONG_DESC = :LONGDESC,                            *00987000
                     PRIM_REL_CODE = :PRIMCODE,                        *00988000
                     PRIM_QLFY = :PRIMQLFY,                            *00989000
                     EXTL_ENROLL = :EXTENROL                           *00990000
                 WHERE CURRENT OF S01UPD0                               00991000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00992000
         BR    14                      RETURN TO CALLER                 00993000
         LTORG                                                          00994000
*                                                                       00995000
**********************************************************************  00996000
* DELETE STATEMENT BY PRIMARY KEY:                                      00997000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            00998000
**********************************************************************  00999000
*                                                                       01000000
DELUC0   DS    0H                                                       01001000
         USING DELUC0,12,8             ESTABLISH BASE REGISTER          01002000
         EXEC  SQL DELETE FROM S01                                     *01003000
                 WHERE CURRENT OF S01UPD0                               01004000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01005000
         BR    14                      RETURN TO CALLER                 01006000
         LTORG                                                          01007000
*                                                                       01008000
**********************************************************************  01009000
* DELETE ALL STATEMENT:                                                 01010000
*   THIS STATEMENT SUPPORTS THE DELETE-FILE VERB.                       01011000
**********************************************************************  01012000
*                                                                       01013000
DELTBL   DS    0H                                                       01014000
         USING DELTBL,12,8             ESTABLISH BASE REGISTER          01015000
         EXEC  SQL DELETE FROM S01                                      01016000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01017000
         BR    14                      RETURN TO CALLER                 01018000
         LTORG                                                          01019000
*                                                                       01020000
**********************************************************************  01021000
* CLOSE UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                        01022000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          01023000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          01024000
*     TO CLOSE THE UPDATE CURSOR.                                       01025000
**********************************************************************  01026000
*                                                                       01027000
CLSUC0   DS    0H                                                       01028000
         USING CLSUC0,12,8             ESTABLISH BASE REGISTER          01029000
         EXEC  SQL CLOSE S01UPD0                                        01030000
         XC    SQWCSUCA,SQWCSUCA       CLEAR CURSOR CLOSE ROUTINE ADDR  01031000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01032000
         BR    14                      RETURN TO CALLER                 01033000
         LTORG                                                          01034000
*                                                                       01035000
**********************************************************************  01036000
* ALTERNATE KEY 1 NOT DEFINED                                           01037000
**********************************************************************  01038000
*                                                                       01039000
SELUC1   DS    0H                                                       01040000
FETUC1   DS    0H                                                       01041000
UPDUC1   DS    0H                                                       01042000
DELUC1   DS    0H                                                       01043000
CLSUC1   DS    0H                                                       01044000
         DC    X'00DEAD01'             FORCE S0C1 ABEND                 01045000
*                                                                       01046000
**********************************************************************  01047000
* ALTERNATE KEY 2 NOT DEFINED                                           01048000
**********************************************************************  01049000
*                                                                       01050000
SELUC2   DS    0H                                                       01051000
FETUC2   DS    0H                                                       01052000
UPDUC2   DS    0H                                                       01053000
DELUC2   DS    0H                                                       01054000
CLSUC2   DS    0H                                                       01055000
         DC    X'00DEAD02'             FORCE S0C1 ABEND                 01056000
*                                                                       01057000
**********************************************************************  01058000
* ALTERNATE KEY 3 NOT DEFINED                                           01059000
**********************************************************************  01060000
*                                                                       01061000
SELUC3   DS    0H                                                       01062000
FETUC3   DS    0H                                                       01063000
UPDUC3   DS    0H                                                       01064000
DELUC3   DS    0H                                                       01065000
CLSUC3   DS    0H                                                       01066000
         DC    X'00DEAD03'             FORCE S0C1 ABEND                 01067000
*                                                                       01068000
         DS    0H                      END OF SQL STATEMENTS            01069000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   01070000
*                                                                       01071000
**********************************************************************  01072000
* DUMMY ENTRY POINT DSNHLI                                              01073000
**********************************************************************  01074000
*                                                                       01075000
         ENTRY DSNHLI                                                   01076000
DSNHLI   DS    0H                                                       01077000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       01078000
         BR    15                      BRANCH TO ATTACH FACILITY        01079000
*                                                                       01080000
**********************************************************************  01081000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  01082000
**********************************************************************  01083000
*                                                                       01084000
* CONVTAB1 TABLE ENTRY FORMAT IS:                                       01085000
*        DC    H'RRRR',H'VVVV',H'LLLL',X'KK',X'DD'                      01086000
* OR:                                                                   01087000
*        DC    H'RRRR',H'VVVV',X'ZZPP',X'KK',X'DD'                      01088000
* WHERE:                                                                01089000
*   RRRR = RECORD AREA OFFSET                                           01090000
*   VVVV = HOST VARIABLE AREA OFFSET                                    01091000
*   LLLL = HALFWORD LENGTH TO MOVE                                      01092000
*   ZZPP = CONVERT ZONED/PACKED LENGTHS (MINUS 1)                       01093000
*   KK   = KEY FIELD MASK:                                              01094000
*            80 = KEY 0 FIELD                                           01095000
*            40 = KEY 1 FIELD                                           01096000
*            20 = KEY 2 FIELD                                           01097000
*            10 = KEY 3 FIELD                                           01098000
*   DD   = DATA FIELD MASK:                                             01099000
*            80 = RECORD FIELD IS PACKED                                01100000
*            40 = HOST VARIABLE IS PACKED                               01101000
*            20 = NULLABLE FIELD                                        01102000
*            01 = DATE FIELD                                            01103000
*            02 = TIME FIELD                                            01104000
*                                                                       01105000
CONVTAB1 DS    0H                      RECORD/HOST VARIABLE CONVERSIONS 01106000
         DC    H'0000',H'0000',H'0026',X'80',X'00'                      01107000
         DC    H'0026',H'0026',H'0301',X'00',X'00'                      01108000
         DC    8X'FF'                                                   01109000
*                                                                       01110000
         LTORG                                                          01111000
         END                                                            01112000
