* REL=S02IODB.10                                                        00001000
**********************************************************************  00002000
*                                                                       00003000
*  S02IODB .... STATIC SQL I/O MODULE                                   00004000
*                                                                       00005000
*  CREATION DATE: 04/25/16                                              00006000
*                                                                       00007000
*  FUNCTIONAL DESCRIPTION: THIS PROGRAM CONTAINS THE STATIC SQL         00008000
*  VECTORS REQUIRED TO SUPPORT I/O TO THE S02 FILE.  IT IS LOADED       00009000
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
COBREC   DS    CL208                   COBOL RECORD (UNALIGNED)         00036000
ASMREC   DS    0F                      ASSEMBLER RECORD (ALIGNED)       00037000
INST     DS    CL4                                                      00038000
RECNBR   DS    CL4                                                      00039000
MODEL    DS    CL10                                                     00040000
PRODCODE DS    CL6                                                      00041000
ACCTTYPE DS    CL3                                                      00042000
AUDDATE  DS    PL5'0.'                                                  00043000
AUDTIME  DS    PL5'0.'                                                  00044000
AUDUSER  DS    CL8                                                      00045000
AUDORG   DS    CL6                                                      00046000
ISPRIM   DS    CL1                                                      00047000
PRODGRP  DS    CL1                                                      00048000
MINACCT  DS    CL2                                                      00049000
MNENQLFY DS    CL1                                                      00050000
MAXSCND  DS    CL2                                                      00051000
MSENQLFY DS    CL1                                                      00052000
MINTERM  DS    CL5                                                      00053000
MINQLFY  DS    CL1                                                      00054000
MAXTERM  DS    CL5                                                      00055000
MAXQLFY  DS    CL1                                                      00056000
BRREGCD  DS    CL6                                                      00057000
BRRGQLFY DS    CL1                                                      00058000
PRIMCATG DS    CL6                                                      00059000
PRIMQLFY DS    CL1                                                      00060000
PRIMINC  DS    CL1                                                      00061000
CBALCAT1 DS    CL2                                                      00062000
CBCAT1Q  DS    CL1                                                      00063000
COMBBAL1 DS    CL1                                                      00064000
CBALCAT2 DS    CL2                                                      00065000
CBCAT2Q  DS    CL1                                                      00066000
COMBBAL2 DS    CL1                                                      00067000
BALCAT1  DS    CL6                                                      00068000
BALQLFY1 DS    CL1                                                      00069000
BALINCV1 DS    CL1                                                      00070000
BALCAT2  DS    CL6                                                      00071000
BALQLFY2 DS    CL1                                                      00072000
BALINCV2 DS    CL1                                                      00073000
USERRTN  DS    CL1                                                      00074000
USERQLFY DS    CL1                                                      00075000
USINCTV  DS    CL1                                                      00076000
PRIMESVC DS    CL1                                                      00077000
SCNDSVC  DS    CL1                                                      00078000
DISCAMT  DS    PL3'0.00'                                                00079000
DISCPCT  DS    PL3'0.00000'                                             00080000
PRIMERTE DS    CL1                                                      00081000
SCNDRATE DS    CL1                                                      00082000
RTINCEN  DS    CL6                                                      00083000
TIMEOPT  DS    CL1                                                      00084000
RTBALCD  DS    CL2                                                      00085000
CASHMIN  DS    PL5'0.'                                                  00086000
CASHMAX  DS    PL5'0.'                                                  00087000
CASHBAL  DS    CL2                                                      00088000
MISCOPT1 DS    CL1                                                      00089000
MISCOPT2 DS    CL1                                                      00090000
MISCOPT3 DS    CL1                                                      00091000
SCINCEN  DS    CL6                                                      00092000
SCBALCD  DS    CL2                                                      00093000
RATEPCT  DS    PL5'0.000000000'                                         00094000
TRANPLAN DS    CL3                                                      00095000
UOPTREL1 DS    CL2                                                      00096000
UOPT1    DS    CL1                                                      00097000
UOPTQ1   DS    CL1                                                      00098000
UOPTREL2 DS    CL2                                                      00099000
UOPT2    DS    CL1                                                      00100000
UOPTQ2   DS    CL1                                                      00101000
UOPTREL3 DS    CL2                                                      00102000
UOPT3    DS    CL1                                                      00103000
UOPTQ3   DS    CL1                                                      00104000
UOPTREL4 DS    CL2                                                      00105000
UOPT4    DS    CL1                                                      00106000
UOPTQ4   DS    CL1                                                      00107000
CBALCAT3 DS    CL2                                                      00108000
CBCAT3Q  DS    CL1                                                      00109000
COMBBAL3 DS    CL1                                                      00110000
CBALCAT4 DS    CL2                                                      00111000
CBCAT4Q  DS    CL1                                                      00112000
COMBBAL4 DS    CL1                                                      00113000
SECSVOPT DS    CL1                                                      00114000
RATEOPT  DS    CL1                                                      00115000
SSPLAN   DS    CL6                                                      00116000
SSTABLE  DS    CL6                                                      00117000
SSBALCD  DS    CL2                                                      00118000
RTETYP   DS    CL1                                                      00119000
USERTYP1 DS    CL1                                                      00120000
USERTYP2 DS    CL1                                                      00121000
USERTYP3 DS    CL1                                                      00122000
USERTYP4 DS    CL1                                                      00123000
*                                                                       00124000
*                                                                       00125000
**********************************************************************  00126000
* PROGRAM TABLE HEADER SECTION:                                         00127000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00128000
*   THE CONTENTS OF THIS SECTION SHOULD NOT BE MODIFIED.                00129000
**********************************************************************  00130000
*                                                                       00131000
S02IODB  CSECT                         PROGRAM TABLE SECTION            00132000
S02IODB  AMODE ANY                                                      00133000
S02IODB  RMODE ANY                                                      00134000
         DC    CL9'S02IODB'            PROGRAM ID                       00135000
         DC    CL8'&SYSDATE',CL1' ',CL5'&SYSTIME',CL1' '                00136000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00137000
         DC    A(0)                    RESERVED                         00138000
         DC    A(0)                    RESERVED                         00139000
         DC    24CL1' '                RESERVED                         00140000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00141000
*                                                                       00142000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00143000
         DC    CL29'WWW.INFOR.COM                '                      00143001
*                                                                       00144000
*                                                                       00145000
**********************************************************************  00146000
* STATEMENT TABLE SECTION:                                              00147000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00148000
*   THE CONTENTS AND ORDER OF THE STATEMENTS IN THIS SECTION            00149000
*   SHOULD NOT BE MODIFIED.                                             00150000
**********************************************************************  00151000
*                                                                       00152000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00153000
STM#TAB  AMODE ANY                                                      00154000
STM#TAB  RMODE ANY                                                      00155000
         DC    A(SQL#0000)             SELECT INTO (KEY 0)              00156000
         DC    A(SQL#0001)             SELECT INTO (KEY 1)              00157000
         DC    A(SQL#0002)             SELECT INTO (KEY 2)              00158000
         DC    A(SQL#0003)             SELECT INTO (KEY 3)              00159000
         DC    A(SQL#0004)             SELECT UPDATE CURSOR (KEY 0)     00160000
         DC    A(SQL#0005)             SELECT UPDATE CURSOR (KEY 1)     00161000
         DC    A(SQL#0006)             SELECT UPDATE CURSOR (KEY 2)     00162000
         DC    A(SQL#0007)             SELECT UPDATE CURSOR (KEY 3)     00163000
         DC    A(SQL#0008)             FETCH UPDATE CURSOR (KEY 0)      00164000
         DC    A(SQL#0009)             FETCH UPDATE CURSOR (KEY 1)      00165000
         DC    A(SQL#0010)             FETCH UPDATE CURSOR (KEY 2)      00166000
         DC    A(SQL#0011)             FETCH UPDATE CURSOR (KEY 3)      00167000
         DC    A(SQL#0012)             CLOSE UPDATE CURSOR (KEY 0)      00168000
         DC    A(SQL#0013)             CLOSE UPDATE CURSOR (KEY 1)      00169000
         DC    A(SQL#0014)             CLOSE UPDATE CURSOR (KEY 2)      00170000
         DC    A(SQL#0015)             CLOSE UPDATE CURSOR (KEY 3)      00171000
         DC    A(SQL#0016)             SELECT SEQ CURSOR (KEY 0)        00172000
         DC    A(SQL#0017)             SELECT SEQ CURSOR (KEY 1)        00173000
         DC    A(SQL#0018)             SELECT SEQ CURSOR (KEY 2)        00174000
         DC    A(SQL#0019)             SELECT SEQ CURSOR (KEY 3)        00175000
         DC    A(SQL#0020)             FETCH SEQ CURSOR (KEY 0)         00176000
         DC    A(SQL#0021)             FETCH SEQ CURSOR (KEY 1)         00177000
         DC    A(SQL#0022)             FETCH SEQ CURSOR (KEY 2)         00178000
         DC    A(SQL#0023)             FETCH SEQ CURSOR (KEY 3)         00179000
         DC    A(SQL#0024)             FETCH SEQ CURSOR UPDATE (KEY 0)  00180000
         DC    A(SQL#0025)             FETCH SEQ CURSOR UPDATE (KEY 1)  00181000
         DC    A(SQL#0026)             FETCH SEQ CURSOR UPDATE (KEY 2)  00182000
         DC    A(SQL#0027)             FETCH SEQ CURSOR UPDATE (KEY 3)  00183000
         DC    A(SQL#0028)             CLOSE SEQ CURSOR (KEY 0)         00184000
         DC    A(SQL#0029)             CLOSE SEQ CURSOR (KEY 1)         00185000
         DC    A(SQL#0030)             CLOSE SEQ CURSOR (KEY 2)         00186000
         DC    A(SQL#0031)             CLOSE SEQ CURSOR (KEY 3)         00187000
         DC    A(SQL#0032)             SELECT KEY (KEY 0)               00188000
         DC    A(SQL#0033)             SELECT KEY (KEY 1)               00189000
         DC    A(SQL#0034)             SELECT KEY (KEY 2)               00190000
         DC    A(SQL#0035)             SELECT KEY (KEY 3)               00191000
         DC    A(SQL#0036)             INSERT STATEMENT                 00192000
         DC    A(SQL#0037)             UPDATE STATEMENT (KEY 0)         00193000
         DC    A(SQL#0038)             UPDATE STATEMENT (KEY 1)         00194000
         DC    A(SQL#0039)             UPDATE STATEMENT (KEY 2)         00195000
         DC    A(SQL#0040)             UPDATE STATEMENT (KEY 3)         00196000
         DC    A(SQL#0041)             DELETE STATEMENT (KEY 0)         00197000
         DC    A(SQL#0042)             DELETE STATEMENT (KEY 1)         00198000
         DC    A(SQL#0043)             DELETE STATEMENT (KEY 2)         00199000
         DC    A(SQL#0044)             DELETE STATEMENT (KEY 3)         00200000
         DC    A(SQL#0045)             DELETE ALL STATEMENT             00201000
         DC    4X'FF'                                                   00202000
*                                                                       00203000
*                                                                       00204000
**********************************************************************  00205000
* SQL STATEMENT SECTION:                                                00206000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00207000
*     TO SUPPORT THIS TABLE.                                            00208000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00209000
*     ARE EQUIVALENT.                                                   00210000
**********************************************************************  00211000
*                                                                       00212000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00213000
SQL#STMT AMODE ANY                                                      00214000
SQL#STMT RMODE ANY                                                      00215000
         USING HOST#VAR,2              ADDRESS HOST VARIABLES           00216000
         USING SQLDSECT,10,3           ADDRESS SQLDSECT                 00217000
         USING COM#AREA,11             ADDRESS COMMAREA                 00218000
*                                                                       00219000
*                                                                       00220000
**********************************************************************  00221000
* SELECT INTO STATEMENT BY PRIMARY KEY:                                 00222000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00223000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00224000
**********************************************************************  00225000
*                                                                       00226000
SQL#0000 DS    0H                                                       00227000
         USING SQL#0000,12             ESTABLISH BASE REGISTER          00228000
         B     *+6                     BRANCH AROUND ADCON              00229000
BASE0000 DC    AL2(4096)                                                00230000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00231000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00232000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00233000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00234000
         AH    3,BASE0000              ADD 4K                           00235000
         EXEC  SQL                                                     *00236000
               SELECT AUDIT_DATE,                                      *00237000
                   AUDIT_TIME,                                         *00238000
                   AUDIT_USER,                                         *00239000
                   AUDIT_ORG,                                          *00240000
                   IS_PRIMARY,                                         *00241000
                   PROD_GROUP,                                         *00242000
                   MIN_ACCT,                                           *00243000
                   MIN_N_QLFY,                                         *00244000
                   MAX_SCND,                                           *00245000
                   MAX_S_QLFY,                                         *00246000
                   MIN_TERM,                                           *00247000
                   MIN_T_QLFY,                                         *00248000
                   MAX_TERM,                                           *00249000
                   MAX_T_QLFY,                                         *00250000
                   BRCH_REG_CODE,                                      *00251000
                   BRCHREG_QLFY,                                       *00252000
                   PRIME_CATG,                                         *00253000
                   PRIME_C_QLFY,                                       *00254000
                   PRIME_C_INCEN,                                      *00255000
                   CMB_BAL_CD_1,                                       *00256000
                   CMB_BAL_CD_1_Q,                                     *00257000
                   CMB_BAL_CD_1_I,                                     *00258000
                   CMB_BAL_CD_2,                                       *00259000
                   CMB_BAL_CD_2_Q,                                     *00260000
                   CMB_BAL_CD_2_I,                                     *00261000
                   BAL_C_CODE_1,                                       *00262000
                   BAL_CATG_Q_1,                                       *00263000
                   BAL_CATG_I_1,                                       *00264000
                   BAL_C_CODE_2,                                       *00265000
                   BAL_CATG_Q_2,                                       *00266000
                   BAL_CATG_I_2,                                       *00267000
                   USER_RTN,                                           *00268000
                   USER_RTN_QLFY,                                      *00269000
                   USER_RTN_INCEN,                                     *00270000
                   PRIMARY_SVC,                                        *00271000
                   SEC_SC_INC_OPT,                                     *00272000
                   DISC_AMT,                                           *00273000
                   DISC_PCT,                                           *00274000
                   PRIMARY_RATE,                                       *00275000
                   SEC_RATE_OPT,                                       *00276000
                   RATE_INCEN,                                         *00277000
                   TIME_OPT,                                           *00278000
                   RATE_BAL_CODE,                                      *00279000
                   CASH_MIN_AMOUNT,                                    *00280000
                   CASH_MAX_AMOUNT,                                    *00281000
                   CASH_BAL_CODE,                                      *00282000
                   MISC_OPTION_1,                                      *00283000
                   MISC_OPTION_2,                                      *00284000
                   MISC_OPTION_3,                                      *00285000
                   SVC_INCEN,                                          *00286000
                   SVC_CHG_BAL_CD,                                     *00287000
                   RATE_PCT,                                           *00288000
                   TRAN_PLAN,                                          *00289000
                   USER_OPT_REL1,                                      *00290000
                   USER_OPT_01,                                        *00291000
                   USER_OPT_Q_1,                                       *00292000
                   USER_OPT_REL2,                                      *00293000
                   USER_OPT_02,                                        *00294000
                   USER_OPT_Q_2,                                       *00295000
                   USER_OPT_REL3,                                      *00296000
                   USER_OPT_03,                                        *00297000
                   USER_OPT_Q_3,                                       *00298000
                   USER_OPT_REL4,                                      *00299000
                   USER_OPT_04,                                        *00300000
                   USER_OPT_Q_4,                                       *00301000
                   CMB_BAL_CD_3,                                       *00302000
                   CMB_BAL_CD_3_Q,                                     *00303000
                   CMB_BAL_CD_3_I,                                     *00304000
                   CMB_BAL_CD_4,                                       *00305000
                   CMB_BAL_CD_4_Q,                                     *00306000
                   CMB_BAL_CD_4_I,                                     *00307000
                   SEC_SVC_OPT,                                        *00308000
                   RATE_OPTION,                                        *00309000
                   SEC_SVC_PLAN,                                       *00310000
                   SEC_SVC_TABLE,                                      *00311000
                   SEC_SVC_BAL_CD,                                     *00312000
                   RATE_TYPE,                                          *00313000
                   USER_TYPE_01,                                       *00314000
                   USER_TYPE_02,                                       *00315000
                   USER_TYPE_03,                                       *00316000
                   USER_TYPE_04                                        *00317000
                 INTO :AUDDATE,                                        *00318000
                   :AUDTIME,                                           *00319000
                   :AUDUSER,                                           *00320000
                   :AUDORG,                                            *00321000
                   :ISPRIM,                                            *00322000
                   :PRODGRP,                                           *00323000
                   :MINACCT,                                           *00324000
                   :MNENQLFY,                                          *00325000
                   :MAXSCND,                                           *00326000
                   :MSENQLFY,                                          *00327000
                   :MINTERM,                                           *00328000
                   :MINQLFY,                                           *00329000
                   :MAXTERM,                                           *00330000
                   :MAXQLFY,                                           *00331000
                   :BRREGCD,                                           *00332000
                   :BRRGQLFY,                                          *00333000
                   :PRIMCATG,                                          *00334000
                   :PRIMQLFY,                                          *00335000
                   :PRIMINC,                                           *00336000
                   :CBALCAT1,                                          *00337000
                   :CBCAT1Q,                                           *00338000
                   :COMBBAL1,                                          *00339000
                   :CBALCAT2,                                          *00340000
                   :CBCAT2Q,                                           *00341000
                   :COMBBAL2,                                          *00342000
                   :BALCAT1,                                           *00343000
                   :BALQLFY1,                                          *00344000
                   :BALINCV1,                                          *00345000
                   :BALCAT2,                                           *00346000
                   :BALQLFY2,                                          *00347000
                   :BALINCV2,                                          *00348000
                   :USERRTN,                                           *00349000
                   :USERQLFY,                                          *00350000
                   :USINCTV,                                           *00351000
                   :PRIMESVC,                                          *00352000
                   :SCNDSVC,                                           *00353000
                   :DISCAMT,                                           *00354000
                   :DISCPCT,                                           *00355000
                   :PRIMERTE,                                          *00356000
                   :SCNDRATE,                                          *00357000
                   :RTINCEN,                                           *00358000
                   :TIMEOPT,                                           *00359000
                   :RTBALCD,                                           *00360000
                   :CASHMIN,                                           *00361000
                   :CASHMAX,                                           *00362000
                   :CASHBAL,                                           *00363000
                   :MISCOPT1,                                          *00364000
                   :MISCOPT2,                                          *00365000
                   :MISCOPT3,                                          *00366000
                   :SCINCEN,                                           *00367000
                   :SCBALCD,                                           *00368000
                   :RATEPCT,                                           *00369000
                   :TRANPLAN,                                          *00370000
                   :UOPTREL1,                                          *00371000
                   :UOPT1,                                             *00372000
                   :UOPTQ1,                                            *00373000
                   :UOPTREL2,                                          *00374000
                   :UOPT2,                                             *00375000
                   :UOPTQ2,                                            *00376000
                   :UOPTREL3,                                          *00377000
                   :UOPT3,                                             *00378000
                   :UOPTQ3,                                            *00379000
                   :UOPTREL4,                                          *00380000
                   :UOPT4,                                             *00381000
                   :UOPTQ4,                                            *00382000
                   :CBALCAT3,                                          *00383000
                   :CBCAT3Q,                                           *00384000
                   :COMBBAL3,                                          *00385000
                   :CBALCAT4,                                          *00386000
                   :CBCAT4Q,                                           *00387000
                   :COMBBAL4,                                          *00388000
                   :SECSVOPT,                                          *00389000
                   :RATEOPT,                                           *00390000
                   :SSPLAN,                                            *00391000
                   :SSTABLE,                                           *00392000
                   :SSBALCD,                                           *00393000
                   :RTETYP,                                            *00394000
                   :USERTYP1,                                          *00395000
                   :USERTYP2,                                          *00396000
                   :USERTYP3,                                          *00397000
                   :USERTYP4                                           *00398000
                 FROM S02                                              *00399000
                 WHERE INST_NBR = :INST AND                            *00400000
                   RECORD_NBR = :RECNBR AND                            *00401000
                   MODEL = :MODEL AND                                  *00402000
                   PROD_CODE = :PRODCODE AND                           *00403000
                   ACCT_TYPE = :ACCTTYPE                               *00404000
                 FETCH FIRST 1 ROW ONLY                                 00405000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   00406000
         BALR  4,5                     MOVE INTO RECORD AREA            00407000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00408000
         BR    14                      RETURN TO CALLER                 00409000
         LTORG                                                          00410000
*                                                                       00411000
*                                                                       00412000
**********************************************************************  00413000
* SELECT INTO STATEMENT BY ALTERNATE KEY 1:                             00414000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00415000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00416000
**********************************************************************  00417000
*                                                                       00418000
SQL#0001 DS    0H                                                       00419000
         USING SQL#0001,12             ESTABLISH BASE REGISTER          00420000
         LA    15,255                  SET RETURN CODE                  00421000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00422000
         BR    14                      RETURN TO CALLER                 00423000
         LTORG                                                          00424000
*                                                                       00425000
*                                                                       00426000
**********************************************************************  00427000
* SELECT INTO STATEMENT BY ALTERNATE KEY 2:                             00428000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00429000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00430000
**********************************************************************  00431000
*                                                                       00432000
SQL#0002 DS    0H                                                       00433000
         USING SQL#0002,12             ESTABLISH BASE REGISTER          00434000
         LA    15,255                  SET RETURN CODE                  00435000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00436000
         BR    14                      RETURN TO CALLER                 00437000
         LTORG                                                          00438000
*                                                                       00439000
*                                                                       00440000
**********************************************************************  00441000
* SELECT INTO STATEMENT BY ALTERNATE KEY 3:                             00442000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00443000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00444000
**********************************************************************  00445000
*                                                                       00446000
SQL#0003 DS    0H                                                       00447000
         USING SQL#0003,12             ESTABLISH BASE REGISTER          00448000
         LA    15,255                  SET RETURN CODE                  00449000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00450000
         BR    14                      RETURN TO CALLER                 00451000
         LTORG                                                          00452000
*                                                                       00453000
*                                                                       00454000
**********************************************************************  00455000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY PRIMARY KEY:       00456000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00457000
*   THEY ARE ALSO USED AFTER A SUCCESSFUL SELECT SEQUENTIAL STATEMENT   00458000
*     FOR THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS.                      00459000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00460000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00461000
*     UPDATE STATEMENT TO MATCH.                                        00462000
**********************************************************************  00463000
*                                                                       00464000
SQL#0004 DS    0H                                                       00465000
         USING SQL#0004,12             ESTABLISH BASE REGISTER          00466000
         B     *+6                     BRANCH AROUND ADCON              00467000
BASE0004 DC    AL2(4096)                                                00468000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00469000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00470000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00471000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00472000
         AH    3,BASE0004              ADD 4K                           00473000
         EXEC  SQL                                                     *00474000
               DECLARE S02UPD0 CURSOR FOR                              *00475000
               SELECT AUDIT_DATE,                                      *00476000
                   AUDIT_TIME,                                         *00477000
                   AUDIT_USER,                                         *00478000
                   AUDIT_ORG,                                          *00479000
                   IS_PRIMARY,                                         *00480000
                   PROD_GROUP,                                         *00481000
                   MIN_ACCT,                                           *00482000
                   MIN_N_QLFY,                                         *00483000
                   MAX_SCND,                                           *00484000
                   MAX_S_QLFY,                                         *00485000
                   MIN_TERM,                                           *00486000
                   MIN_T_QLFY,                                         *00487000
                   MAX_TERM,                                           *00488000
                   MAX_T_QLFY,                                         *00489000
                   BRCH_REG_CODE,                                      *00490000
                   BRCHREG_QLFY,                                       *00491000
                   PRIME_CATG,                                         *00492000
                   PRIME_C_QLFY,                                       *00493000
                   PRIME_C_INCEN,                                      *00494000
                   CMB_BAL_CD_1,                                       *00495000
                   CMB_BAL_CD_1_Q,                                     *00496000
                   CMB_BAL_CD_1_I,                                     *00497000
                   CMB_BAL_CD_2,                                       *00498000
                   CMB_BAL_CD_2_Q,                                     *00499000
                   CMB_BAL_CD_2_I,                                     *00500000
                   BAL_C_CODE_1,                                       *00501000
                   BAL_CATG_Q_1,                                       *00502000
                   BAL_CATG_I_1,                                       *00503000
                   BAL_C_CODE_2,                                       *00504000
                   BAL_CATG_Q_2,                                       *00505000
                   BAL_CATG_I_2,                                       *00506000
                   USER_RTN,                                           *00507000
                   USER_RTN_QLFY,                                      *00508000
                   USER_RTN_INCEN,                                     *00509000
                   PRIMARY_SVC,                                        *00510000
                   SEC_SC_INC_OPT,                                     *00511000
                   DISC_AMT,                                           *00512000
                   DISC_PCT,                                           *00513000
                   PRIMARY_RATE,                                       *00514000
                   SEC_RATE_OPT,                                       *00515000
                   RATE_INCEN,                                         *00516000
                   TIME_OPT,                                           *00517000
                   RATE_BAL_CODE,                                      *00518000
                   CASH_MIN_AMOUNT,                                    *00519000
                   CASH_MAX_AMOUNT,                                    *00520000
                   CASH_BAL_CODE,                                      *00521000
                   MISC_OPTION_1,                                      *00522000
                   MISC_OPTION_2,                                      *00523000
                   MISC_OPTION_3,                                      *00524000
                   SVC_INCEN,                                          *00525000
                   SVC_CHG_BAL_CD,                                     *00526000
                   RATE_PCT,                                           *00527000
                   TRAN_PLAN,                                          *00528000
                   USER_OPT_REL1,                                      *00529000
                   USER_OPT_01,                                        *00530000
                   USER_OPT_Q_1,                                       *00531000
                   USER_OPT_REL2,                                      *00532000
                   USER_OPT_02,                                        *00533000
                   USER_OPT_Q_2,                                       *00534000
                   USER_OPT_REL3,                                      *00535000
                   USER_OPT_03,                                        *00536000
                   USER_OPT_Q_3,                                       *00537000
                   USER_OPT_REL4,                                      *00538000
                   USER_OPT_04,                                        *00539000
                   USER_OPT_Q_4,                                       *00540000
                   CMB_BAL_CD_3,                                       *00541000
                   CMB_BAL_CD_3_Q,                                     *00542000
                   CMB_BAL_CD_3_I,                                     *00543000
                   CMB_BAL_CD_4,                                       *00544000
                   CMB_BAL_CD_4_Q,                                     *00545000
                   CMB_BAL_CD_4_I,                                     *00546000
                   SEC_SVC_OPT,                                        *00547000
                   RATE_OPTION,                                        *00548000
                   SEC_SVC_PLAN,                                       *00549000
                   SEC_SVC_TABLE,                                      *00550000
                   SEC_SVC_BAL_CD,                                     *00551000
                   RATE_TYPE,                                          *00552000
                   USER_TYPE_01,                                       *00553000
                   USER_TYPE_02,                                       *00554000
                   USER_TYPE_03,                                       *00555000
                   USER_TYPE_04                                        *00556000
                 FROM S02                                              *00557000
                 WHERE INST_NBR = :INST AND                            *00558000
                   RECORD_NBR = :RECNBR AND                            *00559000
                   MODEL = :MODEL AND                                  *00560000
                   PROD_CODE = :PRODCODE AND                           *00561000
                   ACCT_TYPE = :ACCTTYPE                               *00562000
                 FOR UPDATE OF AUDIT_DATE,                             *00563000
                   AUDIT_TIME,                                         *00564000
                   AUDIT_USER,                                         *00565000
                   AUDIT_ORG,                                          *00566000
                   IS_PRIMARY,                                         *00567000
                   PROD_GROUP,                                         *00568000
                   MIN_ACCT,                                           *00569000
                   MIN_N_QLFY,                                         *00570000
                   MAX_SCND,                                           *00571000
                   MAX_S_QLFY,                                         *00572000
                   MIN_TERM,                                           *00573000
                   MIN_T_QLFY,                                         *00574000
                   MAX_TERM,                                           *00575000
                   MAX_T_QLFY,                                         *00576000
                   BRCH_REG_CODE,                                      *00577000
                   BRCHREG_QLFY,                                       *00578000
                   PRIME_CATG,                                         *00579000
                   PRIME_C_QLFY,                                       *00580000
                   PRIME_C_INCEN,                                      *00581000
                   CMB_BAL_CD_1,                                       *00582000
                   CMB_BAL_CD_1_Q,                                     *00583000
                   CMB_BAL_CD_1_I,                                     *00584000
                   CMB_BAL_CD_2,                                       *00585000
                   CMB_BAL_CD_2_Q,                                     *00586000
                   CMB_BAL_CD_2_I,                                     *00587000
                   BAL_C_CODE_1,                                       *00588000
                   BAL_CATG_Q_1,                                       *00589000
                   BAL_CATG_I_1,                                       *00590000
                   BAL_C_CODE_2,                                       *00591000
                   BAL_CATG_Q_2,                                       *00592000
                   BAL_CATG_I_2,                                       *00593000
                   USER_RTN,                                           *00594000
                   USER_RTN_QLFY,                                      *00595000
                   USER_RTN_INCEN,                                     *00596000
                   PRIMARY_SVC,                                        *00597000
                   SEC_SC_INC_OPT,                                     *00598000
                   DISC_AMT,                                           *00599000
                   DISC_PCT,                                           *00600000
                   PRIMARY_RATE,                                       *00601000
                   SEC_RATE_OPT,                                       *00602000
                   RATE_INCEN,                                         *00603000
                   TIME_OPT,                                           *00604000
                   RATE_BAL_CODE,                                      *00605000
                   CASH_MIN_AMOUNT,                                    *00606000
                   CASH_MAX_AMOUNT,                                    *00607000
                   CASH_BAL_CODE,                                      *00608000
                   MISC_OPTION_1,                                      *00609000
                   MISC_OPTION_2,                                      *00610000
                   MISC_OPTION_3,                                      *00611000
                   SVC_INCEN,                                          *00612000
                   SVC_CHG_BAL_CD,                                     *00613000
                   RATE_PCT,                                           *00614000
                   TRAN_PLAN,                                          *00615000
                   USER_OPT_REL1,                                      *00616000
                   USER_OPT_01,                                        *00617000
                   USER_OPT_Q_1,                                       *00618000
                   USER_OPT_REL2,                                      *00619000
                   USER_OPT_02,                                        *00620000
                   USER_OPT_Q_2,                                       *00621000
                   USER_OPT_REL3,                                      *00622000
                   USER_OPT_03,                                        *00623000
                   USER_OPT_Q_3,                                       *00624000
                   USER_OPT_REL4,                                      *00625000
                   USER_OPT_04,                                        *00626000
                   USER_OPT_Q_4,                                       *00627000
                   CMB_BAL_CD_3,                                       *00628000
                   CMB_BAL_CD_3_Q,                                     *00629000
                   CMB_BAL_CD_3_I,                                     *00630000
                   CMB_BAL_CD_4,                                       *00631000
                   CMB_BAL_CD_4_Q,                                     *00632000
                   CMB_BAL_CD_4_I,                                     *00633000
                   SEC_SVC_OPT,                                        *00634000
                   RATE_OPTION,                                        *00635000
                   SEC_SVC_PLAN,                                       *00636000
                   SEC_SVC_TABLE,                                      *00637000
                   SEC_SVC_BAL_CD,                                     *00638000
                   RATE_TYPE,                                          *00639000
                   USER_TYPE_01,                                       *00640000
                   USER_TYPE_02,                                       *00641000
                   USER_TYPE_03,                                       *00642000
                   USER_TYPE_04                                        *00643000
                 FETCH FIRST 1 ROW ONLY                                 00644000
         EXEC  SQL                                                     *00645000
               OPEN S02UPD0                                             00646000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00647000
         BR    14                      RETURN TO CALLER                 00648000
         LTORG                                                          00649000
*                                                                       00650000
*                                                                       00651000
**********************************************************************  00652000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 1:   00653000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00654000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00655000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00656000
*     UPDATE STATEMENT TO MATCH.                                        00657000
**********************************************************************  00658000
*                                                                       00659000
SQL#0005 DS    0H                                                       00660000
         USING SQL#0005,12             ESTABLISH BASE REGISTER          00661000
         LA    15,255                  SET RETURN CODE                  00662000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00663000
         BR    14                      RETURN TO CALLER                 00664000
         LTORG                                                          00665000
*                                                                       00666000
*                                                                       00667000
**********************************************************************  00668000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 2:   00669000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00670000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00671000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00672000
*     UPDATE STATEMENT TO MATCH.                                        00673000
**********************************************************************  00674000
*                                                                       00675000
SQL#0006 DS    0H                                                       00676000
         USING SQL#0006,12             ESTABLISH BASE REGISTER          00677000
         LA    15,255                  SET RETURN CODE                  00678000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00679000
         BR    14                      RETURN TO CALLER                 00680000
         LTORG                                                          00681000
*                                                                       00682000
*                                                                       00683000
**********************************************************************  00684000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 3:   00685000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00686000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00687000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00688000
*     UPDATE STATEMENT TO MATCH.                                        00689000
**********************************************************************  00690000
*                                                                       00691000
SQL#0007 DS    0H                                                       00692000
         USING SQL#0007,12             ESTABLISH BASE REGISTER          00693000
         LA    15,255                  SET RETURN CODE                  00694000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00695000
         BR    14                      RETURN TO CALLER                 00696000
         LTORG                                                          00697000
*                                                                       00698000
*                                                                       00699000
**********************************************************************  00700000
* FETCH FROM UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                   00701000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00702000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00703000
*     THE ACTUAL ROW.                                                   00704000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00705000
**********************************************************************  00706000
*                                                                       00707000
SQL#0008 DS    0H                                                       00708000
         USING SQL#0008,12             ESTABLISH BASE REGISTER          00709000
         B     *+6                     BRANCH AROUND ADCON              00710000
BASE0008 DC    AL2(4096)                                                00711000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00712000
         AH    3,BASE0008              ADD 4K                           00713000
         EXEC  SQL                                                     *00714000
               FETCH S02UPD0                                           *00715000
                 INTO :AUDDATE,                                        *00716000
                   :AUDTIME,                                           *00717000
                   :AUDUSER,                                           *00718000
                   :AUDORG,                                            *00719000
                   :ISPRIM,                                            *00720000
                   :PRODGRP,                                           *00721000
                   :MINACCT,                                           *00722000
                   :MNENQLFY,                                          *00723000
                   :MAXSCND,                                           *00724000
                   :MSENQLFY,                                          *00725000
                   :MINTERM,                                           *00726000
                   :MINQLFY,                                           *00727000
                   :MAXTERM,                                           *00728000
                   :MAXQLFY,                                           *00729000
                   :BRREGCD,                                           *00730000
                   :BRRGQLFY,                                          *00731000
                   :PRIMCATG,                                          *00732000
                   :PRIMQLFY,                                          *00733000
                   :PRIMINC,                                           *00734000
                   :CBALCAT1,                                          *00735000
                   :CBCAT1Q,                                           *00736000
                   :COMBBAL1,                                          *00737000
                   :CBALCAT2,                                          *00738000
                   :CBCAT2Q,                                           *00739000
                   :COMBBAL2,                                          *00740000
                   :BALCAT1,                                           *00741000
                   :BALQLFY1,                                          *00742000
                   :BALINCV1,                                          *00743000
                   :BALCAT2,                                           *00744000
                   :BALQLFY2,                                          *00745000
                   :BALINCV2,                                          *00746000
                   :USERRTN,                                           *00747000
                   :USERQLFY,                                          *00748000
                   :USINCTV,                                           *00749000
                   :PRIMESVC,                                          *00750000
                   :SCNDSVC,                                           *00751000
                   :DISCAMT,                                           *00752000
                   :DISCPCT,                                           *00753000
                   :PRIMERTE,                                          *00754000
                   :SCNDRATE,                                          *00755000
                   :RTINCEN,                                           *00756000
                   :TIMEOPT,                                           *00757000
                   :RTBALCD,                                           *00758000
                   :CASHMIN,                                           *00759000
                   :CASHMAX,                                           *00760000
                   :CASHBAL,                                           *00761000
                   :MISCOPT1,                                          *00762000
                   :MISCOPT2,                                          *00763000
                   :MISCOPT3,                                          *00764000
                   :SCINCEN,                                           *00765000
                   :SCBALCD,                                           *00766000
                   :RATEPCT,                                           *00767000
                   :TRANPLAN,                                          *00768000
                   :UOPTREL1,                                          *00769000
                   :UOPT1,                                             *00770000
                   :UOPTQ1,                                            *00771000
                   :UOPTREL2,                                          *00772000
                   :UOPT2,                                             *00773000
                   :UOPTQ2,                                            *00774000
                   :UOPTREL3,                                          *00775000
                   :UOPT3,                                             *00776000
                   :UOPTQ3,                                            *00777000
                   :UOPTREL4,                                          *00778000
                   :UOPT4,                                             *00779000
                   :UOPTQ4,                                            *00780000
                   :CBALCAT3,                                          *00781000
                   :CBCAT3Q,                                           *00782000
                   :COMBBAL3,                                          *00783000
                   :CBALCAT4,                                          *00784000
                   :CBCAT4Q,                                           *00785000
                   :COMBBAL4,                                          *00786000
                   :SECSVOPT,                                          *00787000
                   :RATEOPT,                                           *00788000
                   :SSPLAN,                                            *00789000
                   :SSTABLE,                                           *00790000
                   :SSBALCD,                                           *00791000
                   :RTETYP,                                            *00792000
                   :USERTYP1,                                          *00793000
                   :USERTYP2,                                          *00794000
                   :USERTYP3,                                          *00795000
                   :USERTYP4                                            00796000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   00797000
         BALR  4,5                     MOVE INTO RECORD AREA            00798000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00799000
         BR    14                      RETURN TO CALLER                 00800000
         LTORG                                                          00801000
*                                                                       00802000
*                                                                       00803000
**********************************************************************  00804000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 1:               00805000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00806000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00807000
*     THE ACTUAL ROW.                                                   00808000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00809000
**********************************************************************  00810000
*                                                                       00811000
SQL#0009 DS    0H                                                       00812000
         USING SQL#0009,12             ESTABLISH BASE REGISTER          00813000
         LA    15,255                  SET RETURN CODE                  00814000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00815000
         BR    14                      RETURN TO CALLER                 00816000
         LTORG                                                          00817000
*                                                                       00818000
*                                                                       00819000
**********************************************************************  00820000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 2:               00821000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00822000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00823000
*     THE ACTUAL ROW.                                                   00824000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00825000
**********************************************************************  00826000
*                                                                       00827000
SQL#0010 DS    0H                                                       00828000
         USING SQL#0010,12             ESTABLISH BASE REGISTER          00829000
         LA    15,255                  SET RETURN CODE                  00830000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00831000
         BR    14                      RETURN TO CALLER                 00832000
         LTORG                                                          00833000
*                                                                       00834000
*                                                                       00835000
**********************************************************************  00836000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 3:               00837000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00838000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00839000
*     THE ACTUAL ROW.                                                   00840000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00841000
**********************************************************************  00842000
*                                                                       00843000
SQL#0011 DS    0H                                                       00844000
         USING SQL#0011,12             ESTABLISH BASE REGISTER          00845000
         LA    15,255                  SET RETURN CODE                  00846000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00847000
         BR    14                      RETURN TO CALLER                 00848000
         LTORG                                                          00849000
*                                                                       00850000
*                                                                       00851000
**********************************************************************  00852000
* CLOSE UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                        00853000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00854000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00855000
*     TO CLOSE THE UPDATE CURSOR.                                       00856000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00857000
**********************************************************************  00858000
*                                                                       00859000
SQL#0012 DS    0H                                                       00860000
         USING SQL#0012,12             ESTABLISH BASE REGISTER          00861000
         B     *+6                     BRANCH AROUND ADCON              00862000
BASE0012 DC    AL2(4096)                                                00863000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00864000
         AH    3,BASE0012              ADD 4K                           00865000
         EXEC  SQL                                                     *00866000
               CLOSE S02UPD0                                            00867000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00868000
         BR    14                      RETURN TO CALLER                 00869000
         LTORG                                                          00870000
*                                                                       00871000
*                                                                       00872000
**********************************************************************  00873000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 1:                    00874000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00875000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00876000
*     TO CLOSE THE UPDATE CURSOR.                                       00877000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00878000
**********************************************************************  00879000
*                                                                       00880000
SQL#0013 DS    0H                                                       00881000
         USING SQL#0013,12             ESTABLISH BASE REGISTER          00882000
         LA    15,255                  SET RETURN CODE                  00883000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00884000
         BR    14                      RETURN TO CALLER                 00885000
         LTORG                                                          00886000
*                                                                       00887000
*                                                                       00888000
**********************************************************************  00889000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 2:                    00890000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00891000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00892000
*     TO CLOSE THE UPDATE CURSOR.                                       00893000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00894000
**********************************************************************  00895000
*                                                                       00896000
SQL#0014 DS    0H                                                       00897000
         USING SQL#0014,12             ESTABLISH BASE REGISTER          00898000
         LA    15,255                  SET RETURN CODE                  00899000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00900000
         BR    14                      RETURN TO CALLER                 00901000
         LTORG                                                          00902000
*                                                                       00903000
*                                                                       00904000
**********************************************************************  00905000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 3:                    00906000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00907000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00908000
*     TO CLOSE THE UPDATE CURSOR.                                       00909000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00910000
**********************************************************************  00911000
*                                                                       00912000
SQL#0015 DS    0H                                                       00913000
         USING SQL#0015,12             ESTABLISH BASE REGISTER          00914000
         LA    15,255                  SET RETURN CODE                  00915000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00916000
         BR    14                      RETURN TO CALLER                 00917000
         LTORG                                                          00918000
*                                                                       00919000
*                                                                       00920000
**********************************************************************  00921000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY    00922000
* KEY:                                                                  00923000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00924000
*     AND GET-NEXT-LOCK VERBS.                                          00925000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00926000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00927000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00928000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     00929000
*     ORDER BY CLAUSE.                                                  00930000
**********************************************************************  00931000
*                                                                       00932000
SQL#0016 DS    0H                                                       00933000
         USING SQL#0016,12             ESTABLISH BASE REGISTER          00934000
         B     *+6                     BRANCH AROUND ADCON              00935000
BASE0016 DC    AL2(4096)                                                00936000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00937000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00938000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00939000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00940000
         AH    3,BASE0016              ADD 4K                           00941000
         EXEC  SQL                                                     *00942000
               DECLARE S02SQ001 CURSOR FOR                             *00943000
               SELECT INST_NBR,                                        *00944000
                   RECORD_NBR,                                         *00945000
                   MODEL,                                              *00946000
                   PROD_CODE,                                          *00947000
                   ACCT_TYPE,                                          *00948000
                   AUDIT_DATE,                                         *00949000
                   AUDIT_TIME,                                         *00950000
                   AUDIT_USER,                                         *00951000
                   AUDIT_ORG,                                          *00952000
                   IS_PRIMARY,                                         *00953000
                   PROD_GROUP,                                         *00954000
                   MIN_ACCT,                                           *00955000
                   MIN_N_QLFY,                                         *00956000
                   MAX_SCND,                                           *00957000
                   MAX_S_QLFY,                                         *00958000
                   MIN_TERM,                                           *00959000
                   MIN_T_QLFY,                                         *00960000
                   MAX_TERM,                                           *00961000
                   MAX_T_QLFY,                                         *00962000
                   BRCH_REG_CODE,                                      *00963000
                   BRCHREG_QLFY,                                       *00964000
                   PRIME_CATG,                                         *00965000
                   PRIME_C_QLFY,                                       *00966000
                   PRIME_C_INCEN,                                      *00967000
                   CMB_BAL_CD_1,                                       *00968000
                   CMB_BAL_CD_1_Q,                                     *00969000
                   CMB_BAL_CD_1_I,                                     *00970000
                   CMB_BAL_CD_2,                                       *00971000
                   CMB_BAL_CD_2_Q,                                     *00972000
                   CMB_BAL_CD_2_I,                                     *00973000
                   BAL_C_CODE_1,                                       *00974000
                   BAL_CATG_Q_1,                                       *00975000
                   BAL_CATG_I_1,                                       *00976000
                   BAL_C_CODE_2,                                       *00977000
                   BAL_CATG_Q_2,                                       *00978000
                   BAL_CATG_I_2,                                       *00979000
                   USER_RTN,                                           *00980000
                   USER_RTN_QLFY,                                      *00981000
                   USER_RTN_INCEN,                                     *00982000
                   PRIMARY_SVC,                                        *00983000
                   SEC_SC_INC_OPT,                                     *00984000
                   DISC_AMT,                                           *00985000
                   DISC_PCT,                                           *00986000
                   PRIMARY_RATE,                                       *00987000
                   SEC_RATE_OPT,                                       *00988000
                   RATE_INCEN,                                         *00989000
                   TIME_OPT,                                           *00990000
                   RATE_BAL_CODE,                                      *00991000
                   CASH_MIN_AMOUNT,                                    *00992000
                   CASH_MAX_AMOUNT,                                    *00993000
                   CASH_BAL_CODE,                                      *00994000
                   MISC_OPTION_1,                                      *00995000
                   MISC_OPTION_2,                                      *00996000
                   MISC_OPTION_3,                                      *00997000
                   SVC_INCEN,                                          *00998000
                   SVC_CHG_BAL_CD,                                     *00999000
                   RATE_PCT,                                           *01000000
                   TRAN_PLAN,                                          *01001000
                   USER_OPT_REL1,                                      *01002000
                   USER_OPT_01,                                        *01003000
                   USER_OPT_Q_1,                                       *01004000
                   USER_OPT_REL2,                                      *01005000
                   USER_OPT_02,                                        *01006000
                   USER_OPT_Q_2,                                       *01007000
                   USER_OPT_REL3,                                      *01008000
                   USER_OPT_03,                                        *01009000
                   USER_OPT_Q_3,                                       *01010000
                   USER_OPT_REL4,                                      *01011000
                   USER_OPT_04,                                        *01012000
                   USER_OPT_Q_4,                                       *01013000
                   CMB_BAL_CD_3,                                       *01014000
                   CMB_BAL_CD_3_Q,                                     *01015000
                   CMB_BAL_CD_3_I,                                     *01016000
                   CMB_BAL_CD_4,                                       *01017000
                   CMB_BAL_CD_4_Q,                                     *01018000
                   CMB_BAL_CD_4_I,                                     *01019000
                   SEC_SVC_OPT,                                        *01020000
                   RATE_OPTION,                                        *01021000
                   SEC_SVC_PLAN,                                       *01022000
                   SEC_SVC_TABLE,                                      *01023000
                   SEC_SVC_BAL_CD,                                     *01024000
                   RATE_TYPE,                                          *01025000
                   USER_TYPE_01,                                       *01026000
                   USER_TYPE_02,                                       *01027000
                   USER_TYPE_03,                                       *01028000
                   USER_TYPE_04                                        *01029000
                 FROM S02                                              *01030000
                 WHERE                                                 *01031000
                    INST_NBR = :INST AND                               *01032000
                    RECORD_NBR = :RECNBR AND                           *01033000
                    MODEL = :MODEL AND                                 *01034000
                    PROD_CODE = :PRODCODE AND                          *01035000
                    ACCT_TYPE >= :ACCTTYPE                             *01036000
                 ORDER BY ACCT_TYPE                                    *01037000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01038000
         EXEC  SQL                                                     *01039000
               OPEN S02SQ001                                            01040000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01041000
         BR    14                      RETURN TO CALLER                 01042000
         LTORG                                                          01043000
*                                                                       01044000
*                                                                       01045000
**********************************************************************  01046000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  01047000
* KEY 1:                                                                01048000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         01049000
*     AND GET-NEXT-LOCK VERBS.                                          01050000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              01051000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                01052000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      01053000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     01054000
*     ORDER BY CLAUSE.                                                  01055000
**********************************************************************  01056000
*                                                                       01057000
SQL#0017 DS    0H                                                       01058000
         USING SQL#0017,12             ESTABLISH BASE REGISTER          01059000
         LA    15,255                  SET RETURN CODE                  01060000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01061000
         BR    14                      RETURN TO CALLER                 01062000
         LTORG                                                          01063000
*                                                                       01064000
*                                                                       01065000
**********************************************************************  01066000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  01067000
* KEY 2:                                                                01068000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         01069000
*     AND GET-NEXT-LOCK VERBS.                                          01070000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              01071000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                01072000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      01073000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     01074000
*     ORDER BY CLAUSE.                                                  01075000
**********************************************************************  01076000
*                                                                       01077000
SQL#0018 DS    0H                                                       01078000
         USING SQL#0018,12             ESTABLISH BASE REGISTER          01079000
         LA    15,255                  SET RETURN CODE                  01080000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01081000
         BR    14                      RETURN TO CALLER                 01082000
         LTORG                                                          01083000
*                                                                       01084000
*                                                                       01085000
**********************************************************************  01086000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  01087000
* KEY 3:                                                                01088000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         01089000
*     AND GET-NEXT-LOCK VERBS.                                          01090000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              01091000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                01092000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      01093000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     01094000
*     ORDER BY CLAUSE.                                                  01095000
**********************************************************************  01096000
*                                                                       01097000
SQL#0019 DS    0H                                                       01098000
         USING SQL#0019,12             ESTABLISH BASE REGISTER          01099000
         LA    15,255                  SET RETURN CODE                  01100000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01101000
         BR    14                      RETURN TO CALLER                 01102000
         LTORG                                                          01103000
*                                                                       01104000
*                                                                       01105000
**********************************************************************  01106000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:               01107000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01108000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01109000
*     RETRIEVE THE ACTUAL ROW.                                          01110000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01111000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01112000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01113000
**********************************************************************  01114000
*                                                                       01115000
SQL#0020 DS    0H                                                       01116000
         USING SQL#0020,12             ESTABLISH BASE REGISTER          01117000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01118000
         LA    12,VECT0020(1)          LOAD POINTER TO FETCH ROUTINE    01119000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01120000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01121000
VECT0020 DC    A(SQL#0120)                                              01122000
         DC    A(SQL#0220)                                              01123000
         DC    A(SQL#0320)                                              01124000
         DC    A(SQL#0420)                                              01125000
         DC    A(SQL#0520)                                              01126000
         LTORG                                                          01127000
*                                                                       01128000
SQL#0120 DS    0H                                                       01129000
         USING SQL#0120,12             ESTABLISH BASE REGISTER          01130000
         B     *+6                     BRANCH AROUND ADCON              01131000
BASE0120 DC    AL2(4096)                                                01132000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01133000
         AH    3,BASE0120              ADD 4K                           01134000
         EXEC  SQL                                                     *01135000
               FETCH S02SQ001                                          *01136000
                 INTO :INST,                                           *01137000
                      :RECNBR,                                         *01138000
                      :MODEL,                                          *01139000
                      :PRODCODE,                                       *01140000
                      :ACCTTYPE,                                       *01141000
                      :AUDDATE,                                        *01142000
                      :AUDTIME,                                        *01143000
                      :AUDUSER,                                        *01144000
                      :AUDORG,                                         *01145000
                      :ISPRIM,                                         *01146000
                      :PRODGRP,                                        *01147000
                      :MINACCT,                                        *01148000
                      :MNENQLFY,                                       *01149000
                      :MAXSCND,                                        *01150000
                      :MSENQLFY,                                       *01151000
                      :MINTERM,                                        *01152000
                      :MINQLFY,                                        *01153000
                      :MAXTERM,                                        *01154000
                      :MAXQLFY,                                        *01155000
                      :BRREGCD,                                        *01156000
                      :BRRGQLFY,                                       *01157000
                      :PRIMCATG,                                       *01158000
                      :PRIMQLFY,                                       *01159000
                      :PRIMINC,                                        *01160000
                      :CBALCAT1,                                       *01161000
                      :CBCAT1Q,                                        *01162000
                      :COMBBAL1,                                       *01163000
                      :CBALCAT2,                                       *01164000
                      :CBCAT2Q,                                        *01165000
                      :COMBBAL2,                                       *01166000
                      :BALCAT1,                                        *01167000
                      :BALQLFY1,                                       *01168000
                      :BALINCV1,                                       *01169000
                      :BALCAT2,                                        *01170000
                      :BALQLFY2,                                       *01171000
                      :BALINCV2,                                       *01172000
                      :USERRTN,                                        *01173000
                      :USERQLFY,                                       *01174000
                      :USINCTV,                                        *01175000
                      :PRIMESVC,                                       *01176000
                      :SCNDSVC,                                        *01177000
                      :DISCAMT,                                        *01178000
                      :DISCPCT,                                        *01179000
                      :PRIMERTE,                                       *01180000
                      :SCNDRATE,                                       *01181000
                      :RTINCEN,                                        *01182000
                      :TIMEOPT,                                        *01183000
                      :RTBALCD,                                        *01184000
                      :CASHMIN,                                        *01185000
                      :CASHMAX,                                        *01186000
                      :CASHBAL,                                        *01187000
                      :MISCOPT1,                                       *01188000
                      :MISCOPT2,                                       *01189000
                      :MISCOPT3,                                       *01190000
                      :SCINCEN,                                        *01191000
                      :SCBALCD,                                        *01192000
                      :RATEPCT,                                        *01193000
                      :TRANPLAN,                                       *01194000
                      :UOPTREL1,                                       *01195000
                      :UOPT1,                                          *01196000
                      :UOPTQ1,                                         *01197000
                      :UOPTREL2,                                       *01198000
                      :UOPT2,                                          *01199000
                      :UOPTQ2,                                         *01200000
                      :UOPTREL3,                                       *01201000
                      :UOPT3,                                          *01202000
                      :UOPTQ3,                                         *01203000
                      :UOPTREL4,                                       *01204000
                      :UOPT4,                                          *01205000
                      :UOPTQ4,                                         *01206000
                      :CBALCAT3,                                       *01207000
                      :CBCAT3Q,                                        *01208000
                      :COMBBAL3,                                       *01209000
                      :CBALCAT4,                                       *01210000
                      :CBCAT4Q,                                        *01211000
                      :COMBBAL4,                                       *01212000
                      :SECSVOPT,                                       *01213000
                      :RATEOPT,                                        *01214000
                      :SSPLAN,                                         *01215000
                      :SSTABLE,                                        *01216000
                      :SSBALCD,                                        *01217000
                      :RTETYP,                                         *01218000
                      :USERTYP1,                                       *01219000
                      :USERTYP2,                                       *01220000
                      :USERTYP3,                                       *01221000
                      :USERTYP4                                         01222000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01223000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01224000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01225000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01226000
         BR    12                      OPEN NEXT CURSOR                 01227000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01228000
         BALR  4,5                     MOVE INTO RECORD AREA            01229000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01230000
         BR    14                      RETURN TO CALLER                 01231000
         LTORG                                                          01232000
*                                                                       01233000
SQL#0220 DS    0H                                                       01234000
         USING SQL#0220,12             ESTABLISH BASE REGISTER          01235000
         B     *+6                     BRANCH AROUND ADCON              01236000
BASE0220 DC    AL2(4096)                                                01237000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01238000
         AH    3,BASE0220              ADD 4K                           01239000
         EXEC  SQL                                                     *01240000
               FETCH S02SQ002                                          *01241000
                 INTO :INST,                                           *01242000
                      :RECNBR,                                         *01243000
                      :MODEL,                                          *01244000
                      :PRODCODE,                                       *01245000
                      :ACCTTYPE,                                       *01246000
                      :AUDDATE,                                        *01247000
                      :AUDTIME,                                        *01248000
                      :AUDUSER,                                        *01249000
                      :AUDORG,                                         *01250000
                      :ISPRIM,                                         *01251000
                      :PRODGRP,                                        *01252000
                      :MINACCT,                                        *01253000
                      :MNENQLFY,                                       *01254000
                      :MAXSCND,                                        *01255000
                      :MSENQLFY,                                       *01256000
                      :MINTERM,                                        *01257000
                      :MINQLFY,                                        *01258000
                      :MAXTERM,                                        *01259000
                      :MAXQLFY,                                        *01260000
                      :BRREGCD,                                        *01261000
                      :BRRGQLFY,                                       *01262000
                      :PRIMCATG,                                       *01263000
                      :PRIMQLFY,                                       *01264000
                      :PRIMINC,                                        *01265000
                      :CBALCAT1,                                       *01266000
                      :CBCAT1Q,                                        *01267000
                      :COMBBAL1,                                       *01268000
                      :CBALCAT2,                                       *01269000
                      :CBCAT2Q,                                        *01270000
                      :COMBBAL2,                                       *01271000
                      :BALCAT1,                                        *01272000
                      :BALQLFY1,                                       *01273000
                      :BALINCV1,                                       *01274000
                      :BALCAT2,                                        *01275000
                      :BALQLFY2,                                       *01276000
                      :BALINCV2,                                       *01277000
                      :USERRTN,                                        *01278000
                      :USERQLFY,                                       *01279000
                      :USINCTV,                                        *01280000
                      :PRIMESVC,                                       *01281000
                      :SCNDSVC,                                        *01282000
                      :DISCAMT,                                        *01283000
                      :DISCPCT,                                        *01284000
                      :PRIMERTE,                                       *01285000
                      :SCNDRATE,                                       *01286000
                      :RTINCEN,                                        *01287000
                      :TIMEOPT,                                        *01288000
                      :RTBALCD,                                        *01289000
                      :CASHMIN,                                        *01290000
                      :CASHMAX,                                        *01291000
                      :CASHBAL,                                        *01292000
                      :MISCOPT1,                                       *01293000
                      :MISCOPT2,                                       *01294000
                      :MISCOPT3,                                       *01295000
                      :SCINCEN,                                        *01296000
                      :SCBALCD,                                        *01297000
                      :RATEPCT,                                        *01298000
                      :TRANPLAN,                                       *01299000
                      :UOPTREL1,                                       *01300000
                      :UOPT1,                                          *01301000
                      :UOPTQ1,                                         *01302000
                      :UOPTREL2,                                       *01303000
                      :UOPT2,                                          *01304000
                      :UOPTQ2,                                         *01305000
                      :UOPTREL3,                                       *01306000
                      :UOPT3,                                          *01307000
                      :UOPTQ3,                                         *01308000
                      :UOPTREL4,                                       *01309000
                      :UOPT4,                                          *01310000
                      :UOPTQ4,                                         *01311000
                      :CBALCAT3,                                       *01312000
                      :CBCAT3Q,                                        *01313000
                      :COMBBAL3,                                       *01314000
                      :CBALCAT4,                                       *01315000
                      :CBCAT4Q,                                        *01316000
                      :COMBBAL4,                                       *01317000
                      :SECSVOPT,                                       *01318000
                      :RATEOPT,                                        *01319000
                      :SSPLAN,                                         *01320000
                      :SSTABLE,                                        *01321000
                      :SSBALCD,                                        *01322000
                      :RTETYP,                                         *01323000
                      :USERTYP1,                                       *01324000
                      :USERTYP2,                                       *01325000
                      :USERTYP3,                                       *01326000
                      :USERTYP4                                         01327000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01328000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01329000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01330000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01331000
         BR    12                      OPEN NEXT CURSOR                 01332000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01333000
         BALR  4,5                     MOVE INTO RECORD AREA            01334000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01335000
         BR    14                      RETURN TO CALLER                 01336000
         LTORG                                                          01337000
*                                                                       01338000
SQL#0320 DS    0H                                                       01339000
         USING SQL#0320,12             ESTABLISH BASE REGISTER          01340000
         B     *+6                     BRANCH AROUND ADCON              01341000
BASE0320 DC    AL2(4096)                                                01342000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01343000
         AH    3,BASE0320              ADD 4K                           01344000
         EXEC  SQL                                                     *01345000
               FETCH S02SQ003                                          *01346000
                 INTO :INST,                                           *01347000
                      :RECNBR,                                         *01348000
                      :MODEL,                                          *01349000
                      :PRODCODE,                                       *01350000
                      :ACCTTYPE,                                       *01351000
                      :AUDDATE,                                        *01352000
                      :AUDTIME,                                        *01353000
                      :AUDUSER,                                        *01354000
                      :AUDORG,                                         *01355000
                      :ISPRIM,                                         *01356000
                      :PRODGRP,                                        *01357000
                      :MINACCT,                                        *01358000
                      :MNENQLFY,                                       *01359000
                      :MAXSCND,                                        *01360000
                      :MSENQLFY,                                       *01361000
                      :MINTERM,                                        *01362000
                      :MINQLFY,                                        *01363000
                      :MAXTERM,                                        *01364000
                      :MAXQLFY,                                        *01365000
                      :BRREGCD,                                        *01366000
                      :BRRGQLFY,                                       *01367000
                      :PRIMCATG,                                       *01368000
                      :PRIMQLFY,                                       *01369000
                      :PRIMINC,                                        *01370000
                      :CBALCAT1,                                       *01371000
                      :CBCAT1Q,                                        *01372000
                      :COMBBAL1,                                       *01373000
                      :CBALCAT2,                                       *01374000
                      :CBCAT2Q,                                        *01375000
                      :COMBBAL2,                                       *01376000
                      :BALCAT1,                                        *01377000
                      :BALQLFY1,                                       *01378000
                      :BALINCV1,                                       *01379000
                      :BALCAT2,                                        *01380000
                      :BALQLFY2,                                       *01381000
                      :BALINCV2,                                       *01382000
                      :USERRTN,                                        *01383000
                      :USERQLFY,                                       *01384000
                      :USINCTV,                                        *01385000
                      :PRIMESVC,                                       *01386000
                      :SCNDSVC,                                        *01387000
                      :DISCAMT,                                        *01388000
                      :DISCPCT,                                        *01389000
                      :PRIMERTE,                                       *01390000
                      :SCNDRATE,                                       *01391000
                      :RTINCEN,                                        *01392000
                      :TIMEOPT,                                        *01393000
                      :RTBALCD,                                        *01394000
                      :CASHMIN,                                        *01395000
                      :CASHMAX,                                        *01396000
                      :CASHBAL,                                        *01397000
                      :MISCOPT1,                                       *01398000
                      :MISCOPT2,                                       *01399000
                      :MISCOPT3,                                       *01400000
                      :SCINCEN,                                        *01401000
                      :SCBALCD,                                        *01402000
                      :RATEPCT,                                        *01403000
                      :TRANPLAN,                                       *01404000
                      :UOPTREL1,                                       *01405000
                      :UOPT1,                                          *01406000
                      :UOPTQ1,                                         *01407000
                      :UOPTREL2,                                       *01408000
                      :UOPT2,                                          *01409000
                      :UOPTQ2,                                         *01410000
                      :UOPTREL3,                                       *01411000
                      :UOPT3,                                          *01412000
                      :UOPTQ3,                                         *01413000
                      :UOPTREL4,                                       *01414000
                      :UOPT4,                                          *01415000
                      :UOPTQ4,                                         *01416000
                      :CBALCAT3,                                       *01417000
                      :CBCAT3Q,                                        *01418000
                      :COMBBAL3,                                       *01419000
                      :CBALCAT4,                                       *01420000
                      :CBCAT4Q,                                        *01421000
                      :COMBBAL4,                                       *01422000
                      :SECSVOPT,                                       *01423000
                      :RATEOPT,                                        *01424000
                      :SSPLAN,                                         *01425000
                      :SSTABLE,                                        *01426000
                      :SSBALCD,                                        *01427000
                      :RTETYP,                                         *01428000
                      :USERTYP1,                                       *01429000
                      :USERTYP2,                                       *01430000
                      :USERTYP3,                                       *01431000
                      :USERTYP4                                         01432000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01433000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01434000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01435000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01436000
         BR    12                      OPEN NEXT CURSOR                 01437000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01438000
         BALR  4,5                     MOVE INTO RECORD AREA            01439000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01440000
         BR    14                      RETURN TO CALLER                 01441000
         LTORG                                                          01442000
*                                                                       01443000
SQL#0420 DS    0H                                                       01444000
         USING SQL#0420,12             ESTABLISH BASE REGISTER          01445000
         B     *+6                     BRANCH AROUND ADCON              01446000
BASE0420 DC    AL2(4096)                                                01447000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01448000
         AH    3,BASE0420              ADD 4K                           01449000
         EXEC  SQL                                                     *01450000
               FETCH S02SQ004                                          *01451000
                 INTO :INST,                                           *01452000
                      :RECNBR,                                         *01453000
                      :MODEL,                                          *01454000
                      :PRODCODE,                                       *01455000
                      :ACCTTYPE,                                       *01456000
                      :AUDDATE,                                        *01457000
                      :AUDTIME,                                        *01458000
                      :AUDUSER,                                        *01459000
                      :AUDORG,                                         *01460000
                      :ISPRIM,                                         *01461000
                      :PRODGRP,                                        *01462000
                      :MINACCT,                                        *01463000
                      :MNENQLFY,                                       *01464000
                      :MAXSCND,                                        *01465000
                      :MSENQLFY,                                       *01466000
                      :MINTERM,                                        *01467000
                      :MINQLFY,                                        *01468000
                      :MAXTERM,                                        *01469000
                      :MAXQLFY,                                        *01470000
                      :BRREGCD,                                        *01471000
                      :BRRGQLFY,                                       *01472000
                      :PRIMCATG,                                       *01473000
                      :PRIMQLFY,                                       *01474000
                      :PRIMINC,                                        *01475000
                      :CBALCAT1,                                       *01476000
                      :CBCAT1Q,                                        *01477000
                      :COMBBAL1,                                       *01478000
                      :CBALCAT2,                                       *01479000
                      :CBCAT2Q,                                        *01480000
                      :COMBBAL2,                                       *01481000
                      :BALCAT1,                                        *01482000
                      :BALQLFY1,                                       *01483000
                      :BALINCV1,                                       *01484000
                      :BALCAT2,                                        *01485000
                      :BALQLFY2,                                       *01486000
                      :BALINCV2,                                       *01487000
                      :USERRTN,                                        *01488000
                      :USERQLFY,                                       *01489000
                      :USINCTV,                                        *01490000
                      :PRIMESVC,                                       *01491000
                      :SCNDSVC,                                        *01492000
                      :DISCAMT,                                        *01493000
                      :DISCPCT,                                        *01494000
                      :PRIMERTE,                                       *01495000
                      :SCNDRATE,                                       *01496000
                      :RTINCEN,                                        *01497000
                      :TIMEOPT,                                        *01498000
                      :RTBALCD,                                        *01499000
                      :CASHMIN,                                        *01500000
                      :CASHMAX,                                        *01501000
                      :CASHBAL,                                        *01502000
                      :MISCOPT1,                                       *01503000
                      :MISCOPT2,                                       *01504000
                      :MISCOPT3,                                       *01505000
                      :SCINCEN,                                        *01506000
                      :SCBALCD,                                        *01507000
                      :RATEPCT,                                        *01508000
                      :TRANPLAN,                                       *01509000
                      :UOPTREL1,                                       *01510000
                      :UOPT1,                                          *01511000
                      :UOPTQ1,                                         *01512000
                      :UOPTREL2,                                       *01513000
                      :UOPT2,                                          *01514000
                      :UOPTQ2,                                         *01515000
                      :UOPTREL3,                                       *01516000
                      :UOPT3,                                          *01517000
                      :UOPTQ3,                                         *01518000
                      :UOPTREL4,                                       *01519000
                      :UOPT4,                                          *01520000
                      :UOPTQ4,                                         *01521000
                      :CBALCAT3,                                       *01522000
                      :CBCAT3Q,                                        *01523000
                      :COMBBAL3,                                       *01524000
                      :CBALCAT4,                                       *01525000
                      :CBCAT4Q,                                        *01526000
                      :COMBBAL4,                                       *01527000
                      :SECSVOPT,                                       *01528000
                      :RATEOPT,                                        *01529000
                      :SSPLAN,                                         *01530000
                      :SSTABLE,                                        *01531000
                      :SSBALCD,                                        *01532000
                      :RTETYP,                                         *01533000
                      :USERTYP1,                                       *01534000
                      :USERTYP2,                                       *01535000
                      :USERTYP3,                                       *01536000
                      :USERTYP4                                         01537000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01538000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01539000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01540000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01541000
         BR    12                      OPEN NEXT CURSOR                 01542000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01543000
         BALR  4,5                     MOVE INTO RECORD AREA            01544000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01545000
         BR    14                      RETURN TO CALLER                 01546000
         LTORG                                                          01547000
*                                                                       01548000
SQL#0520 DS    0H                                                       01549000
         USING SQL#0520,12             ESTABLISH BASE REGISTER          01550000
         B     *+6                     BRANCH AROUND ADCON              01551000
BASE0520 DC    AL2(4096)                                                01552000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01553000
         AH    3,BASE0520              ADD 4K                           01554000
         EXEC  SQL                                                     *01555000
               FETCH S02SQ005                                          *01556000
                 INTO :INST,                                           *01557000
                      :RECNBR,                                         *01558000
                      :MODEL,                                          *01559000
                      :PRODCODE,                                       *01560000
                      :ACCTTYPE,                                       *01561000
                      :AUDDATE,                                        *01562000
                      :AUDTIME,                                        *01563000
                      :AUDUSER,                                        *01564000
                      :AUDORG,                                         *01565000
                      :ISPRIM,                                         *01566000
                      :PRODGRP,                                        *01567000
                      :MINACCT,                                        *01568000
                      :MNENQLFY,                                       *01569000
                      :MAXSCND,                                        *01570000
                      :MSENQLFY,                                       *01571000
                      :MINTERM,                                        *01572000
                      :MINQLFY,                                        *01573000
                      :MAXTERM,                                        *01574000
                      :MAXQLFY,                                        *01575000
                      :BRREGCD,                                        *01576000
                      :BRRGQLFY,                                       *01577000
                      :PRIMCATG,                                       *01578000
                      :PRIMQLFY,                                       *01579000
                      :PRIMINC,                                        *01580000
                      :CBALCAT1,                                       *01581000
                      :CBCAT1Q,                                        *01582000
                      :COMBBAL1,                                       *01583000
                      :CBALCAT2,                                       *01584000
                      :CBCAT2Q,                                        *01585000
                      :COMBBAL2,                                       *01586000
                      :BALCAT1,                                        *01587000
                      :BALQLFY1,                                       *01588000
                      :BALINCV1,                                       *01589000
                      :BALCAT2,                                        *01590000
                      :BALQLFY2,                                       *01591000
                      :BALINCV2,                                       *01592000
                      :USERRTN,                                        *01593000
                      :USERQLFY,                                       *01594000
                      :USINCTV,                                        *01595000
                      :PRIMESVC,                                       *01596000
                      :SCNDSVC,                                        *01597000
                      :DISCAMT,                                        *01598000
                      :DISCPCT,                                        *01599000
                      :PRIMERTE,                                       *01600000
                      :SCNDRATE,                                       *01601000
                      :RTINCEN,                                        *01602000
                      :TIMEOPT,                                        *01603000
                      :RTBALCD,                                        *01604000
                      :CASHMIN,                                        *01605000
                      :CASHMAX,                                        *01606000
                      :CASHBAL,                                        *01607000
                      :MISCOPT1,                                       *01608000
                      :MISCOPT2,                                       *01609000
                      :MISCOPT3,                                       *01610000
                      :SCINCEN,                                        *01611000
                      :SCBALCD,                                        *01612000
                      :RATEPCT,                                        *01613000
                      :TRANPLAN,                                       *01614000
                      :UOPTREL1,                                       *01615000
                      :UOPT1,                                          *01616000
                      :UOPTQ1,                                         *01617000
                      :UOPTREL2,                                       *01618000
                      :UOPT2,                                          *01619000
                      :UOPTQ2,                                         *01620000
                      :UOPTREL3,                                       *01621000
                      :UOPT3,                                          *01622000
                      :UOPTQ3,                                         *01623000
                      :UOPTREL4,                                       *01624000
                      :UOPT4,                                          *01625000
                      :UOPTQ4,                                         *01626000
                      :CBALCAT3,                                       *01627000
                      :CBCAT3Q,                                        *01628000
                      :COMBBAL3,                                       *01629000
                      :CBALCAT4,                                       *01630000
                      :CBCAT4Q,                                        *01631000
                      :COMBBAL4,                                       *01632000
                      :SECSVOPT,                                       *01633000
                      :RATEOPT,                                        *01634000
                      :SSPLAN,                                         *01635000
                      :SSTABLE,                                        *01636000
                      :SSBALCD,                                        *01637000
                      :RTETYP,                                         *01638000
                      :USERTYP1,                                       *01639000
                      :USERTYP2,                                       *01640000
                      :USERTYP3,                                       *01641000
                      :USERTYP4                                         01642000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01643000
         BALR  4,5                     MOVE INTO RECORD AREA            01644000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01645000
         BR    14                      RETURN TO CALLER                 01646000
         LTORG                                                          01647000
*                                                                       01648000
*                                                                       01649000
**********************************************************************  01650000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 1:           01651000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01652000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01653000
*     RETRIEVE THE ACTUAL ROW.                                          01654000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01655000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01656000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01657000
**********************************************************************  01658000
*                                                                       01659000
SQL#0021 DS    0H                                                       01660000
         USING SQL#0021,12             ESTABLISH BASE REGISTER          01661000
         LA    15,255                  SET RETURN CODE                  01662000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01663000
         BR    14                      RETURN TO CALLER                 01664000
         LTORG                                                          01665000
*                                                                       01666000
*                                                                       01667000
**********************************************************************  01668000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 2:           01669000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01670000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01671000
*     RETRIEVE THE ACTUAL ROW.                                          01672000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01673000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01674000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01675000
**********************************************************************  01676000
*                                                                       01677000
SQL#0022 DS    0H                                                       01678000
         USING SQL#0022,12             ESTABLISH BASE REGISTER          01679000
         LA    15,255                  SET RETURN CODE                  01680000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01681000
         BR    14                      RETURN TO CALLER                 01682000
         LTORG                                                          01683000
*                                                                       01684000
*                                                                       01685000
**********************************************************************  01686000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 3:           01687000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01688000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01689000
*     RETRIEVE THE ACTUAL ROW.                                          01690000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01691000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01692000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01693000
**********************************************************************  01694000
*                                                                       01695000
SQL#0023 DS    0H                                                       01696000
         USING SQL#0023,12             ESTABLISH BASE REGISTER          01697000
         LA    15,255                  SET RETURN CODE                  01698000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01699000
         BR    14                      RETURN TO CALLER                 01700000
         LTORG                                                          01701000
*                                                                       01702000
*                                                                       01703000
**********************************************************************  01704000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01705000
* THE PRIMARY KEY:                                                      01706000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01707000
*     VERBS.                                                            01708000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01709000
*     RETRIEVE THE ACTUAL ROW.                                          01710000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01711000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01712000
*     WILL BE THRU THE UPDATE CURSOR.                                   01713000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01714000
**********************************************************************  01715000
*                                                                       01716000
SQL#0024 DS    0H                                                       01717000
         USING SQL#0024,12             ESTABLISH BASE REGISTER          01718000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01719000
         LA    12,VECT0024(1)          LOAD POINTER TO FETCH ROUTINE    01720000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01721000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01722000
VECT0024 DC    A(SQL#0124)                                              01723000
         DC    A(SQL#0224)                                              01724000
         DC    A(SQL#0324)                                              01725000
         DC    A(SQL#0424)                                              01726000
         DC    A(SQL#0524)                                              01727000
         LTORG                                                          01728000
*                                                                       01729000
SQL#0124 DS    0H                                                       01730000
         USING SQL#0124,12             ESTABLISH BASE REGISTER          01731000
         B     *+6                     BRANCH AROUND ADCON              01732000
BASE0124 DC    AL2(4096)                                                01733000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01734000
         AH    3,BASE0124              ADD 4K                           01735000
         EXEC  SQL                                                     *01736000
               FETCH S02SQ001                                          *01737000
                 INTO :INST,                                           *01738000
                      :RECNBR,                                         *01739000
                      :MODEL,                                          *01740000
                      :PRODCODE,                                       *01741000
                      :ACCTTYPE                                         01742000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01743000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01744000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01745000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01746000
         BR    12                      OPEN NEXT CURSOR                 01747000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01748000
         BALR  4,5                     MOVE INTO RECORD AREA            01749000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01750000
         BR    14                      RETURN TO CALLER                 01751000
         LTORG                                                          01752000
*                                                                       01753000
SQL#0224 DS    0H                                                       01754000
         USING SQL#0224,12             ESTABLISH BASE REGISTER          01755000
         B     *+6                     BRANCH AROUND ADCON              01756000
BASE0224 DC    AL2(4096)                                                01757000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01758000
         AH    3,BASE0224              ADD 4K                           01759000
         EXEC  SQL                                                     *01760000
               FETCH S02SQ002                                          *01761000
                 INTO :INST,                                           *01762000
                      :RECNBR,                                         *01763000
                      :MODEL,                                          *01764000
                      :PRODCODE,                                       *01765000
                      :ACCTTYPE                                         01766000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01767000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01768000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01769000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01770000
         BR    12                      OPEN NEXT CURSOR                 01771000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01772000
         BALR  4,5                     MOVE INTO RECORD AREA            01773000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01774000
         BR    14                      RETURN TO CALLER                 01775000
         LTORG                                                          01776000
*                                                                       01777000
SQL#0324 DS    0H                                                       01778000
         USING SQL#0324,12             ESTABLISH BASE REGISTER          01779000
         B     *+6                     BRANCH AROUND ADCON              01780000
BASE0324 DC    AL2(4096)                                                01781000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01782000
         AH    3,BASE0324              ADD 4K                           01783000
         EXEC  SQL                                                     *01784000
               FETCH S02SQ003                                          *01785000
                 INTO :INST,                                           *01786000
                      :RECNBR,                                         *01787000
                      :MODEL,                                          *01788000
                      :PRODCODE,                                       *01789000
                      :ACCTTYPE                                         01790000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01791000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01792000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01793000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01794000
         BR    12                      OPEN NEXT CURSOR                 01795000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01796000
         BALR  4,5                     MOVE INTO RECORD AREA            01797000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01798000
         BR    14                      RETURN TO CALLER                 01799000
         LTORG                                                          01800000
*                                                                       01801000
SQL#0424 DS    0H                                                       01802000
         USING SQL#0424,12             ESTABLISH BASE REGISTER          01803000
         B     *+6                     BRANCH AROUND ADCON              01804000
BASE0424 DC    AL2(4096)                                                01805000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01806000
         AH    3,BASE0424              ADD 4K                           01807000
         EXEC  SQL                                                     *01808000
               FETCH S02SQ004                                          *01809000
                 INTO :INST,                                           *01810000
                      :RECNBR,                                         *01811000
                      :MODEL,                                          *01812000
                      :PRODCODE,                                       *01813000
                      :ACCTTYPE                                         01814000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01815000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01816000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01817000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01818000
         BR    12                      OPEN NEXT CURSOR                 01819000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01820000
         BALR  4,5                     MOVE INTO RECORD AREA            01821000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01822000
         BR    14                      RETURN TO CALLER                 01823000
         LTORG                                                          01824000
*                                                                       01825000
SQL#0524 DS    0H                                                       01826000
         USING SQL#0524,12             ESTABLISH BASE REGISTER          01827000
         B     *+6                     BRANCH AROUND ADCON              01828000
BASE0524 DC    AL2(4096)                                                01829000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01830000
         AH    3,BASE0524              ADD 4K                           01831000
         EXEC  SQL                                                     *01832000
               FETCH S02SQ005                                          *01833000
                 INTO :INST,                                           *01834000
                      :RECNBR,                                         *01835000
                      :MODEL,                                          *01836000
                      :PRODCODE,                                       *01837000
                      :ACCTTYPE                                         01838000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01839000
         BALR  4,5                     MOVE INTO RECORD AREA            01840000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01841000
         BR    14                      RETURN TO CALLER                 01842000
         LTORG                                                          01843000
*                                                                       01844000
*                                                                       01845000
**********************************************************************  01846000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01847000
* ALTERNATE KEY 1:                                                      01848000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01849000
*     VERBS.                                                            01850000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01851000
*     RETRIEVE THE ACTUAL ROW.                                          01852000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01853000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01854000
*     WILL BE THRU THE UPDATE CURSOR.                                   01855000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01856000
**********************************************************************  01857000
*                                                                       01858000
SQL#0025 DS    0H                                                       01859000
         USING SQL#0025,12             ESTABLISH BASE REGISTER          01860000
         LA    15,255                  SET RETURN CODE                  01861000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01862000
         BR    14                      RETURN TO CALLER                 01863000
         LTORG                                                          01864000
*                                                                       01865000
*                                                                       01866000
**********************************************************************  01867000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01868000
* ALTERNATE KEY 2:                                                      01869000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01870000
*     VERBS.                                                            01871000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01872000
*     RETRIEVE THE ACTUAL ROW.                                          01873000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01874000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01875000
*     WILL BE THRU THE UPDATE CURSOR.                                   01876000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01877000
**********************************************************************  01878000
*                                                                       01879000
SQL#0026 DS    0H                                                       01880000
         USING SQL#0026,12             ESTABLISH BASE REGISTER          01881000
         LA    15,255                  SET RETURN CODE                  01882000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01883000
         BR    14                      RETURN TO CALLER                 01884000
         LTORG                                                          01885000
*                                                                       01886000
*                                                                       01887000
**********************************************************************  01888000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01889000
* ALTERNATE KEY 3:                                                      01890000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01891000
*     VERBS.                                                            01892000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01893000
*     RETRIEVE THE ACTUAL ROW.                                          01894000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01895000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01896000
*     WILL BE THRU THE UPDATE CURSOR.                                   01897000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01898000
**********************************************************************  01899000
*                                                                       01900000
SQL#0027 DS    0H                                                       01901000
         USING SQL#0027,12             ESTABLISH BASE REGISTER          01902000
         LA    15,255                  SET RETURN CODE                  01903000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01904000
         BR    14                      RETURN TO CALLER                 01905000
         LTORG                                                          01906000
*                                                                       01907000
*                                                                       01908000
**********************************************************************  01909000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:                    01910000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01911000
*     AND GET-NEXT-LOCK VERBS.                                          01912000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01913000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01914000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01915000
**********************************************************************  01916000
*                                                                       01917000
SQL#0028 DS    0H                                                       01918000
         USING SQL#0028,12             ESTABLISH BASE REGISTER          01919000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01920000
         XC    CSRPTR,CSRPTR           CLEAR CURSOR ROUTINE POINTER     01921000
         LA    12,VECT0028(1)          LOAD POINTER TO CLOSE ROUTINE    01922000
         L     12,0(12)                LOAD CLOSE ROUTINE ADDRESS       01923000
         BR    12                      GO TO CURRENT CLOSE ROUTINE      01924000
VECT0028 DC    A(SQL#0128)                                              01925000
         DC    A(SQL#0228)                                              01926000
         DC    A(SQL#0328)                                              01927000
         DC    A(SQL#0428)                                              01928000
         DC    A(SQL#0528)                                              01929000
         LTORG                                                          01930000
*                                                                       01931000
SQL#0128 DS    0H                                                       01932000
         USING SQL#0128,12             ESTABLISH BASE REGISTER          01933000
         B     *+6                     BRANCH AROUND ADCON              01934000
BASE0128 DC    AL2(4096)                                                01935000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01936000
         AH    3,BASE0128              ADD 4K                           01937000
         EXEC  SQL                                                     *01938000
               CLOSE S02SQ001                                           01939000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01940000
         BR    14                      RETURN TO CALLER                 01941000
         LTORG                                                          01942000
*                                                                       01943000
SQL#0228 DS    0H                                                       01944000
         USING SQL#0228,12             ESTABLISH BASE REGISTER          01945000
         B     *+6                     BRANCH AROUND ADCON              01946000
BASE0228 DC    AL2(4096)                                                01947000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01948000
         AH    3,BASE0228              ADD 4K                           01949000
         EXEC  SQL                                                     *01950000
               CLOSE S02SQ002                                           01951000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01952000
         BR    14                      RETURN TO CALLER                 01953000
         LTORG                                                          01954000
*                                                                       01955000
SQL#0328 DS    0H                                                       01956000
         USING SQL#0328,12             ESTABLISH BASE REGISTER          01957000
         B     *+6                     BRANCH AROUND ADCON              01958000
BASE0328 DC    AL2(4096)                                                01959000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01960000
         AH    3,BASE0328              ADD 4K                           01961000
         EXEC  SQL                                                     *01962000
               CLOSE S02SQ003                                           01963000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01964000
         BR    14                      RETURN TO CALLER                 01965000
         LTORG                                                          01966000
*                                                                       01967000
SQL#0428 DS    0H                                                       01968000
         USING SQL#0428,12             ESTABLISH BASE REGISTER          01969000
         B     *+6                     BRANCH AROUND ADCON              01970000
BASE0428 DC    AL2(4096)                                                01971000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01972000
         AH    3,BASE0428              ADD 4K                           01973000
         EXEC  SQL                                                     *01974000
               CLOSE S02SQ004                                           01975000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01976000
         BR    14                      RETURN TO CALLER                 01977000
         LTORG                                                          01978000
*                                                                       01979000
SQL#0528 DS    0H                                                       01980000
         USING SQL#0528,12             ESTABLISH BASE REGISTER          01981000
         B     *+6                     BRANCH AROUND ADCON              01982000
BASE0528 DC    AL2(4096)                                                01983000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01984000
         AH    3,BASE0528              ADD 4K                           01985000
         EXEC  SQL                                                     *01986000
               CLOSE S02SQ005                                           01987000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01988000
         BR    14                      RETURN TO CALLER                 01989000
         LTORG                                                          01990000
*                                                                       01991000
*                                                                       01992000
**********************************************************************  01993000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 1:                01994000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01995000
*     AND GET-NEXT-LOCK VERBS.                                          01996000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01997000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01998000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01999000
**********************************************************************  02000000
*                                                                       02001000
SQL#0029 DS    0H                                                       02002000
         USING SQL#0029,12             ESTABLISH BASE REGISTER          02003000
         LA    15,255                  SET RETURN CODE                  02004000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02005000
         BR    14                      RETURN TO CALLER                 02006000
         LTORG                                                          02007000
*                                                                       02008000
*                                                                       02009000
**********************************************************************  02010000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 2:                02011000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          02012000
*     AND GET-NEXT-LOCK VERBS.                                          02013000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      02014000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   02015000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02016000
**********************************************************************  02017000
*                                                                       02018000
SQL#0030 DS    0H                                                       02019000
         USING SQL#0030,12             ESTABLISH BASE REGISTER          02020000
         LA    15,255                  SET RETURN CODE                  02021000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02022000
         BR    14                      RETURN TO CALLER                 02023000
         LTORG                                                          02024000
*                                                                       02025000
*                                                                       02026000
**********************************************************************  02027000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 3:                02028000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          02029000
*     AND GET-NEXT-LOCK VERBS.                                          02030000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      02031000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   02032000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02033000
**********************************************************************  02034000
*                                                                       02035000
SQL#0031 DS    0H                                                       02036000
         USING SQL#0031,12             ESTABLISH BASE REGISTER          02037000
         LA    15,255                  SET RETURN CODE                  02038000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02039000
         BR    14                      RETURN TO CALLER                 02040000
         LTORG                                                          02041000
*                                                                       02042000
*                                                                       02043000
**********************************************************************  02044000
* SELECT KEY STATEMENT BY PRIMARY KEY:                                  02045000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            02046000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02047000
**********************************************************************  02048000
*                                                                       02049000
SQL#0032 DS    0H                                                       02050000
         USING SQL#0032,12             ESTABLISH BASE REGISTER          02051000
         B     *+6                     BRANCH AROUND ADCON              02052000
BASE0032 DC    AL2(4096)                                                02053000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              02054000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      02055000
         BALR  4,5                     MOVE INTO HOST VARIABLES         02056000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02057000
         AH    3,BASE0032              ADD 4K                           02058000
         EXEC  SQL                                                     *02059000
               SELECT INST_NBR,                                        *02060000
                   RECORD_NBR,                                         *02061000
                   MODEL,                                              *02062000
                   PROD_CODE,                                          *02063000
                   ACCT_TYPE                                           *02064000
                 INTO :INST,                                           *02065000
                   :RECNBR,                                            *02066000
                   :MODEL,                                             *02067000
                   :PRODCODE,                                          *02068000
                   :ACCTTYPE                                           *02069000
                 FROM S02                                              *02070000
                 WHERE INST_NBR = :INST AND                            *02071000
                   RECORD_NBR = :RECNBR AND                            *02072000
                   MODEL = :MODEL AND                                  *02073000
                   PROD_CODE = :PRODCODE AND                           *02074000
                   ACCT_TYPE = :ACCTTYPE                               *02075000
                 FETCH FIRST 1 ROW ONLY                                 02076000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02077000
         BR    14                      RETURN TO CALLER                 02078000
         LTORG                                                          02079000
*                                                                       02080000
*                                                                       02081000
**********************************************************************  02082000
* SELECT KEY STATEMENT BY ALTERNATE KEY 1:                              02083000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            02084000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02085000
**********************************************************************  02086000
*                                                                       02087000
SQL#0033 DS    0H                                                       02088000
         USING SQL#0033,12             ESTABLISH BASE REGISTER          02089000
         LA    15,255                  SET RETURN CODE                  02090000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02091000
         BR    14                      RETURN TO CALLER                 02092000
         LTORG                                                          02093000
*                                                                       02094000
*                                                                       02095000
**********************************************************************  02096000
* SELECT KEY STATEMENT BY ALTERNATE KEY 2:                              02097000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            02098000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02099000
**********************************************************************  02100000
*                                                                       02101000
SQL#0034 DS    0H                                                       02102000
         USING SQL#0034,12             ESTABLISH BASE REGISTER          02103000
         LA    15,255                  SET RETURN CODE                  02104000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02105000
         BR    14                      RETURN TO CALLER                 02106000
         LTORG                                                          02107000
*                                                                       02108000
*                                                                       02109000
**********************************************************************  02110000
* SELECT KEY STATEMENT BY ALTERNATE KEY 3:                              02111000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            02112000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02113000
**********************************************************************  02114000
*                                                                       02115000
SQL#0035 DS    0H                                                       02116000
         USING SQL#0035,12             ESTABLISH BASE REGISTER          02117000
         LA    15,255                  SET RETURN CODE                  02118000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02119000
         BR    14                      RETURN TO CALLER                 02120000
         LTORG                                                          02121000
*                                                                       02122000
*                                                                       02123000
**********************************************************************  02124000
* INSERT STATEMENT:                                                     02125000
*   THIS STATEMENT SUPPORTS THE PUT VERB.                               02126000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02127000
**********************************************************************  02128000
*                                                                       02129000
SQL#0036 DS    0H                                                       02130000
         USING SQL#0036,12             ESTABLISH BASE REGISTER          02131000
         B     *+6                     BRANCH AROUND ADCON              02132000
BASE0036 DC    AL2(4096)                                                02133000
         L     5,=A(IN#HOST)           LOAD INPUT CONVERSION ROUTINE    02134000
         BALR  4,5                     MOVE INTO HOST VARIABLES         02135000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02136000
         AH    3,BASE0036              ADD 4K                           02137000
         EXEC  SQL                                                     *02138000
               INSERT INTO S02                                         *02139000
                   (INST_NBR,                                          *02140000
                    RECORD_NBR,                                        *02141000
                    MODEL,                                             *02142000
                    PROD_CODE,                                         *02143000
                    ACCT_TYPE,                                         *02144000
                    AUDIT_DATE,                                        *02145000
                    AUDIT_TIME,                                        *02146000
                    AUDIT_USER,                                        *02147000
                    AUDIT_ORG,                                         *02148000
                    IS_PRIMARY,                                        *02149000
                    PROD_GROUP,                                        *02150000
                    MIN_ACCT,                                          *02151000
                    MIN_N_QLFY,                                        *02152000
                    MAX_SCND,                                          *02153000
                    MAX_S_QLFY,                                        *02154000
                    MIN_TERM,                                          *02155000
                    MIN_T_QLFY,                                        *02156000
                    MAX_TERM,                                          *02157000
                    MAX_T_QLFY,                                        *02158000
                    BRCH_REG_CODE,                                     *02159000
                    BRCHREG_QLFY,                                      *02160000
                    PRIME_CATG,                                        *02161000
                    PRIME_C_QLFY,                                      *02162000
                    PRIME_C_INCEN,                                     *02163000
                    CMB_BAL_CD_1,                                      *02164000
                    CMB_BAL_CD_1_Q,                                    *02165000
                    CMB_BAL_CD_1_I,                                    *02166000
                    CMB_BAL_CD_2,                                      *02167000
                    CMB_BAL_CD_2_Q,                                    *02168000
                    CMB_BAL_CD_2_I,                                    *02169000
                    BAL_C_CODE_1,                                      *02170000
                    BAL_CATG_Q_1,                                      *02171000
                    BAL_CATG_I_1,                                      *02172000
                    BAL_C_CODE_2,                                      *02173000
                    BAL_CATG_Q_2,                                      *02174000
                    BAL_CATG_I_2,                                      *02175000
                    USER_RTN,                                          *02176000
                    USER_RTN_QLFY,                                     *02177000
                    USER_RTN_INCEN,                                    *02178000
                    PRIMARY_SVC,                                       *02179000
                    SEC_SC_INC_OPT,                                    *02180000
                    DISC_AMT,                                          *02181000
                    DISC_PCT,                                          *02182000
                    PRIMARY_RATE,                                      *02183000
                    SEC_RATE_OPT,                                      *02184000
                    RATE_INCEN,                                        *02185000
                    TIME_OPT,                                          *02186000
                    RATE_BAL_CODE,                                     *02187000
                    CASH_MIN_AMOUNT,                                   *02188000
                    CASH_MAX_AMOUNT,                                   *02189000
                    CASH_BAL_CODE,                                     *02190000
                    MISC_OPTION_1,                                     *02191000
                    MISC_OPTION_2,                                     *02192000
                    MISC_OPTION_3,                                     *02193000
                    SVC_INCEN,                                         *02194000
                    SVC_CHG_BAL_CD,                                    *02195000
                    RATE_PCT,                                          *02196000
                    TRAN_PLAN,                                         *02197000
                    USER_OPT_REL1,                                     *02198000
                    USER_OPT_01,                                       *02199000
                    USER_OPT_Q_1,                                      *02200000
                    USER_OPT_REL2,                                     *02201000
                    USER_OPT_02,                                       *02202000
                    USER_OPT_Q_2,                                      *02203000
                    USER_OPT_REL3,                                     *02204000
                    USER_OPT_03,                                       *02205000
                    USER_OPT_Q_3,                                      *02206000
                    USER_OPT_REL4,                                     *02207000
                    USER_OPT_04,                                       *02208000
                    USER_OPT_Q_4,                                      *02209000
                    CMB_BAL_CD_3,                                      *02210000
                    CMB_BAL_CD_3_Q,                                    *02211000
                    CMB_BAL_CD_3_I,                                    *02212000
                    CMB_BAL_CD_4,                                      *02213000
                    CMB_BAL_CD_4_Q,                                    *02214000
                    CMB_BAL_CD_4_I,                                    *02215000
                    SEC_SVC_OPT,                                       *02216000
                    RATE_OPTION,                                       *02217000
                    SEC_SVC_PLAN,                                      *02218000
                    SEC_SVC_TABLE,                                     *02219000
                    SEC_SVC_BAL_CD,                                    *02220000
                    RATE_TYPE,                                         *02221000
                    USER_TYPE_01,                                      *02222000
                    USER_TYPE_02,                                      *02223000
                    USER_TYPE_03,                                      *02224000
                    USER_TYPE_04)                                      *02225000
                  VALUES (:INST,                                       *02226000
                   :RECNBR,                                            *02227000
                   :MODEL,                                             *02228000
                   :PRODCODE,                                          *02229000
                   :ACCTTYPE,                                          *02230000
                   :AUDDATE,                                           *02231000
                   :AUDTIME,                                           *02232000
                   :AUDUSER,                                           *02233000
                   :AUDORG,                                            *02234000
                   :ISPRIM,                                            *02235000
                   :PRODGRP,                                           *02236000
                   :MINACCT,                                           *02237000
                   :MNENQLFY,                                          *02238000
                   :MAXSCND,                                           *02239000
                   :MSENQLFY,                                          *02240000
                   :MINTERM,                                           *02241000
                   :MINQLFY,                                           *02242000
                   :MAXTERM,                                           *02243000
                   :MAXQLFY,                                           *02244000
                   :BRREGCD,                                           *02245000
                   :BRRGQLFY,                                          *02246000
                   :PRIMCATG,                                          *02247000
                   :PRIMQLFY,                                          *02248000
                   :PRIMINC,                                           *02249000
                   :CBALCAT1,                                          *02250000
                   :CBCAT1Q,                                           *02251000
                   :COMBBAL1,                                          *02252000
                   :CBALCAT2,                                          *02253000
                   :CBCAT2Q,                                           *02254000
                   :COMBBAL2,                                          *02255000
                   :BALCAT1,                                           *02256000
                   :BALQLFY1,                                          *02257000
                   :BALINCV1,                                          *02258000
                   :BALCAT2,                                           *02259000
                   :BALQLFY2,                                          *02260000
                   :BALINCV2,                                          *02261000
                   :USERRTN,                                           *02262000
                   :USERQLFY,                                          *02263000
                   :USINCTV,                                           *02264000
                   :PRIMESVC,                                          *02265000
                   :SCNDSVC,                                           *02266000
                   :DISCAMT,                                           *02267000
                   :DISCPCT,                                           *02268000
                   :PRIMERTE,                                          *02269000
                   :SCNDRATE,                                          *02270000
                   :RTINCEN,                                           *02271000
                   :TIMEOPT,                                           *02272000
                   :RTBALCD,                                           *02273000
                   :CASHMIN,                                           *02274000
                   :CASHMAX,                                           *02275000
                   :CASHBAL,                                           *02276000
                   :MISCOPT1,                                          *02277000
                   :MISCOPT2,                                          *02278000
                   :MISCOPT3,                                          *02279000
                   :SCINCEN,                                           *02280000
                   :SCBALCD,                                           *02281000
                   :RATEPCT,                                           *02282000
                   :TRANPLAN,                                          *02283000
                   :UOPTREL1,                                          *02284000
                   :UOPT1,                                             *02285000
                   :UOPTQ1,                                            *02286000
                   :UOPTREL2,                                          *02287000
                   :UOPT2,                                             *02288000
                   :UOPTQ2,                                            *02289000
                   :UOPTREL3,                                          *02290000
                   :UOPT3,                                             *02291000
                   :UOPTQ3,                                            *02292000
                   :UOPTREL4,                                          *02293000
                   :UOPT4,                                             *02294000
                   :UOPTQ4,                                            *02295000
                   :CBALCAT3,                                          *02296000
                   :CBCAT3Q,                                           *02297000
                   :COMBBAL3,                                          *02298000
                   :CBALCAT4,                                          *02299000
                   :CBCAT4Q,                                           *02300000
                   :COMBBAL4,                                          *02301000
                   :SECSVOPT,                                          *02302000
                   :RATEOPT,                                           *02303000
                   :SSPLAN,                                            *02304000
                   :SSTABLE,                                           *02305000
                   :SSBALCD,                                           *02306000
                   :RTETYP,                                            *02307000
                   :USERTYP1,                                          *02308000
                   :USERTYP2,                                          *02309000
                   :USERTYP3,                                          *02310000
                   :USERTYP4)                                           02311000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02312000
         BR    14                      RETURN TO CALLER                 02313000
         LTORG                                                          02314000
*                                                                       02315000
*                                                                       02316000
**********************************************************************  02317000
* UPDATE STATEMENT BY PRIMARY KEY:                                      02318000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             02319000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        02320000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         02321000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             02322000
*     STATEMENT.                                                        02323000
**********************************************************************  02324000
*                                                                       02325000
SQL#0037 DS    0H                                                       02326000
         USING SQL#0037,12             ESTABLISH BASE REGISTER          02327000
         B     *+6                     BRANCH AROUND ADCON              02328000
BASE0037 DC    AL2(4096)                                                02329000
         L     5,=A(IN#HOST)           LOAD INPUT CONVERSION ROUTINE    02330000
         BALR  4,5                     MOVE INTO HOST VARIABLES         02331000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02332000
         AH    3,BASE0037              ADD 4K                           02333000
         EXEC  SQL                                                     *02334000
               UPDATE S02                                              *02335000
                   SET AUDIT_DATE = :AUDDATE,                          *02336000
                     AUDIT_TIME = :AUDTIME,                            *02337000
                     AUDIT_USER = :AUDUSER,                            *02338000
                     AUDIT_ORG = :AUDORG,                              *02339000
                     IS_PRIMARY = :ISPRIM,                             *02340000
                     PROD_GROUP = :PRODGRP,                            *02341000
                     MIN_ACCT = :MINACCT,                              *02342000
                     MIN_N_QLFY = :MNENQLFY,                           *02343000
                     MAX_SCND = :MAXSCND,                              *02344000
                     MAX_S_QLFY = :MSENQLFY,                           *02345000
                     MIN_TERM = :MINTERM,                              *02346000
                     MIN_T_QLFY = :MINQLFY,                            *02347000
                     MAX_TERM = :MAXTERM,                              *02348000
                     MAX_T_QLFY = :MAXQLFY,                            *02349000
                     BRCH_REG_CODE = :BRREGCD,                         *02350000
                     BRCHREG_QLFY = :BRRGQLFY,                         *02351000
                     PRIME_CATG = :PRIMCATG,                           *02352000
                     PRIME_C_QLFY = :PRIMQLFY,                         *02353000
                     PRIME_C_INCEN = :PRIMINC,                         *02354000
                     CMB_BAL_CD_1 = :CBALCAT1,                         *02355000
                     CMB_BAL_CD_1_Q = :CBCAT1Q,                        *02356000
                     CMB_BAL_CD_1_I = :COMBBAL1,                       *02357000
                     CMB_BAL_CD_2 = :CBALCAT2,                         *02358000
                     CMB_BAL_CD_2_Q = :CBCAT2Q,                        *02359000
                     CMB_BAL_CD_2_I = :COMBBAL2,                       *02360000
                     BAL_C_CODE_1 = :BALCAT1,                          *02361000
                     BAL_CATG_Q_1 = :BALQLFY1,                         *02362000
                     BAL_CATG_I_1 = :BALINCV1,                         *02363000
                     BAL_C_CODE_2 = :BALCAT2,                          *02364000
                     BAL_CATG_Q_2 = :BALQLFY2,                         *02365000
                     BAL_CATG_I_2 = :BALINCV2,                         *02366000
                     USER_RTN = :USERRTN,                              *02367000
                     USER_RTN_QLFY = :USERQLFY,                        *02368000
                     USER_RTN_INCEN = :USINCTV,                        *02369000
                     PRIMARY_SVC = :PRIMESVC,                          *02370000
                     SEC_SC_INC_OPT = :SCNDSVC,                        *02371000
                     DISC_AMT = :DISCAMT,                              *02372000
                     DISC_PCT = :DISCPCT,                              *02373000
                     PRIMARY_RATE = :PRIMERTE,                         *02374000
                     SEC_RATE_OPT = :SCNDRATE,                         *02375000
                     RATE_INCEN = :RTINCEN,                            *02376000
                     TIME_OPT = :TIMEOPT,                              *02377000
                     RATE_BAL_CODE = :RTBALCD,                         *02378000
                     CASH_MIN_AMOUNT = :CASHMIN,                       *02379000
                     CASH_MAX_AMOUNT = :CASHMAX,                       *02380000
                     CASH_BAL_CODE = :CASHBAL,                         *02381000
                     MISC_OPTION_1 = :MISCOPT1,                        *02382000
                     MISC_OPTION_2 = :MISCOPT2,                        *02383000
                     MISC_OPTION_3 = :MISCOPT3,                        *02384000
                     SVC_INCEN = :SCINCEN,                             *02385000
                     SVC_CHG_BAL_CD = :SCBALCD,                        *02386000
                     RATE_PCT = :RATEPCT,                              *02387000
                     TRAN_PLAN = :TRANPLAN,                            *02388000
                     USER_OPT_REL1 = :UOPTREL1,                        *02389000
                     USER_OPT_01 = :UOPT1,                             *02390000
                     USER_OPT_Q_1 = :UOPTQ1,                           *02391000
                     USER_OPT_REL2 = :UOPTREL2,                        *02392000
                     USER_OPT_02 = :UOPT2,                             *02393000
                     USER_OPT_Q_2 = :UOPTQ2,                           *02394000
                     USER_OPT_REL3 = :UOPTREL3,                        *02395000
                     USER_OPT_03 = :UOPT3,                             *02396000
                     USER_OPT_Q_3 = :UOPTQ3,                           *02397000
                     USER_OPT_REL4 = :UOPTREL4,                        *02398000
                     USER_OPT_04 = :UOPT4,                             *02399000
                     USER_OPT_Q_4 = :UOPTQ4,                           *02400000
                     CMB_BAL_CD_3 = :CBALCAT3,                         *02401000
                     CMB_BAL_CD_3_Q = :CBCAT3Q,                        *02402000
                     CMB_BAL_CD_3_I = :COMBBAL3,                       *02403000
                     CMB_BAL_CD_4 = :CBALCAT4,                         *02404000
                     CMB_BAL_CD_4_Q = :CBCAT4Q,                        *02405000
                     CMB_BAL_CD_4_I = :COMBBAL4,                       *02406000
                     SEC_SVC_OPT = :SECSVOPT,                          *02407000
                     RATE_OPTION = :RATEOPT,                           *02408000
                     SEC_SVC_PLAN = :SSPLAN,                           *02409000
                     SEC_SVC_TABLE = :SSTABLE,                         *02410000
                     SEC_SVC_BAL_CD = :SSBALCD,                        *02411000
                     RATE_TYPE = :RTETYP,                              *02412000
                     USER_TYPE_01 = :USERTYP1,                         *02413000
                     USER_TYPE_02 = :USERTYP2,                         *02414000
                     USER_TYPE_03 = :USERTYP3,                         *02415000
                     USER_TYPE_04 = :USERTYP4                          *02416000
                 WHERE CURRENT OF S02UPD0                               02417000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02418000
         BR    14                      RETURN TO CALLER                 02419000
         LTORG                                                          02420000
*                                                                       02421000
*                                                                       02422000
**********************************************************************  02423000
* UPDATE STATEMENT BY ALTERNATE KEY 1:                                  02424000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             02425000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        02426000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         02427000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             02428000
*     STATEMENT.                                                        02429000
**********************************************************************  02430000
*                                                                       02431000
SQL#0038 DS    0H                                                       02432000
         USING SQL#0038,12             ESTABLISH BASE REGISTER          02433000
         LA    15,255                  SET RETURN CODE                  02434000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02435000
         BR    14                      RETURN TO CALLER                 02436000
         LTORG                                                          02437000
*                                                                       02438000
*                                                                       02439000
**********************************************************************  02440000
* UPDATE STATEMENT BY ALTERNATE KEY 2:                                  02441000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             02442000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        02443000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         02444000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             02445000
*     STATEMENT.                                                        02446000
**********************************************************************  02447000
*                                                                       02448000
SQL#0039 DS    0H                                                       02449000
         USING SQL#0039,12             ESTABLISH BASE REGISTER          02450000
         LA    15,255                  SET RETURN CODE                  02451000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02452000
         BR    14                      RETURN TO CALLER                 02453000
         LTORG                                                          02454000
*                                                                       02455000
*                                                                       02456000
**********************************************************************  02457000
* UPDATE STATEMENT BY ALTERNATE KEY 3:                                  02458000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             02459000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        02460000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         02461000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             02462000
*     STATEMENT.                                                        02463000
**********************************************************************  02464000
*                                                                       02465000
SQL#0040 DS    0H                                                       02466000
         USING SQL#0040,12             ESTABLISH BASE REGISTER          02467000
         LA    15,255                  SET RETURN CODE                  02468000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02469000
         BR    14                      RETURN TO CALLER                 02470000
         LTORG                                                          02471000
*                                                                       02472000
*                                                                       02473000
**********************************************************************  02474000
* DELETE STATEMENT BY PRIMARY KEY:                                      02475000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            02476000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02477000
**********************************************************************  02478000
*                                                                       02479000
SQL#0041 DS    0H                                                       02480000
         USING SQL#0041,12             ESTABLISH BASE REGISTER          02481000
         B     *+6                     BRANCH AROUND ADCON              02482000
BASE0041 DC    AL2(4096)                                                02483000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02484000
         AH    3,BASE0041              ADD 4K                           02485000
         EXEC  SQL                                                     *02486000
               DELETE FROM S02                                         *02487000
                 WHERE CURRENT OF S02UPD0                               02488000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02489000
         BR    14                      RETURN TO CALLER                 02490000
         LTORG                                                          02491000
*                                                                       02492000
*                                                                       02493000
**********************************************************************  02494000
* DELETE STATEMENT BY ALTERNATE KEY 1:                                  02495000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            02496000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02497000
**********************************************************************  02498000
*                                                                       02499000
SQL#0042 DS    0H                                                       02500000
         USING SQL#0042,12             ESTABLISH BASE REGISTER          02501000
         LA    15,255                  SET RETURN CODE                  02502000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02503000
         BR    14                      RETURN TO CALLER                 02504000
         LTORG                                                          02505000
*                                                                       02506000
*                                                                       02507000
**********************************************************************  02508000
* DELETE STATEMENT BY ALTERNATE KEY 2:                                  02509000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            02510000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02511000
**********************************************************************  02512000
*                                                                       02513000
SQL#0043 DS    0H                                                       02514000
         USING SQL#0043,12             ESTABLISH BASE REGISTER          02515000
         LA    15,255                  SET RETURN CODE                  02516000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02517000
         BR    14                      RETURN TO CALLER                 02518000
         LTORG                                                          02519000
*                                                                       02520000
*                                                                       02521000
**********************************************************************  02522000
* DELETE STATEMENT BY ALTERNATE KEY 3:                                  02523000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            02524000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02525000
**********************************************************************  02526000
*                                                                       02527000
SQL#0044 DS    0H                                                       02528000
         USING SQL#0044,12             ESTABLISH BASE REGISTER          02529000
         LA    15,255                  SET RETURN CODE                  02530000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02531000
         BR    14                      RETURN TO CALLER                 02532000
         LTORG                                                          02533000
*                                                                       02534000
*                                                                       02535000
**********************************************************************  02536000
* DELETE ALL STATEMENT:                                                 02537000
*   THIS STATEMENT SUPPORTS THE DELETE-FILE VERB.                       02538000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  02539000
**********************************************************************  02540000
*                                                                       02541000
SQL#0045 DS    0H                                                       02542000
         USING SQL#0045,12             ESTABLISH BASE REGISTER          02543000
         B     *+6                     BRANCH AROUND ADCON              02544000
BASE0045 DC    AL2(4096)                                                02545000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02546000
         AH    3,BASE0045              ADD 4K                           02547000
         EXEC  SQL                                                     *02548000
               DELETE FROM S02                                          02549000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02550000
         BR    14                      RETURN TO CALLER                 02551000
         LTORG                                                          02552000
*                                                                       02553000
*                                                                       02554000
**********************************************************************  02555000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY KEY:          02556000
*   THIS ROUTINE HANDLES PRIMARY KEY SEQUENTIAL CURSORS.                02557000
**********************************************************************  02558000
*                                                                       02559000
SQL#0046 DS    0H                                                       02560000
         USING SQL#0046,12             ESTABLISH BASE REGISTER          02561000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              02562000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      02563000
         BALR  4,5                     MOVE INTO HOST VARIABLES         02564000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      02565000
         LA    1,4(1)                  INCREMENT TO NEXT CURSOR         02566000
         STH   1,CSRPTR                SAVE FOR NEXT CALL               02567000
         LA    12,VECT0046(1)          LOAD POINTER TO NEXT CURSOR      02568000
         L     12,0(12)                LOAD CURSOR ROUTINE ADDRESS      02569000
         BR    12                      GO TO CURRENT CURSOR ROUTINE     02570000
VECT0046 DC    A(0)                                                     02571000
         DC    A(SQL#0246)                                              02572000
         DC    A(SQL#0346)                                              02573000
         DC    A(SQL#0446)                                              02574000
         DC    A(SQL#0546)                                              02575000
         LTORG                                                          02576000
*                                                                       02577000
SQL#0246 DS    0H                                                       02578000
         USING SQL#0246,12             ESTABLISH BASE REGISTER          02579000
         B     *+6                     BRANCH AROUND ADCON              02580000
BASE0246 DC    AL2(4096)                                                02581000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02582000
         AH    3,BASE0246              ADD 4K                           02583000
         EXEC  SQL                                                     *02584000
               CLOSE S02SQ001                                           02585000
         EXEC  SQL                                                     *02586000
               DECLARE S02SQ002 CURSOR FOR                             *02587000
               SELECT INST_NBR,                                        *02588000
                   RECORD_NBR,                                         *02589000
                   MODEL,                                              *02590000
                   PROD_CODE,                                          *02591000
                   ACCT_TYPE,                                          *02592000
                   AUDIT_DATE,                                         *02593000
                   AUDIT_TIME,                                         *02594000
                   AUDIT_USER,                                         *02595000
                   AUDIT_ORG,                                          *02596000
                   IS_PRIMARY,                                         *02597000
                   PROD_GROUP,                                         *02598000
                   MIN_ACCT,                                           *02599000
                   MIN_N_QLFY,                                         *02600000
                   MAX_SCND,                                           *02601000
                   MAX_S_QLFY,                                         *02602000
                   MIN_TERM,                                           *02603000
                   MIN_T_QLFY,                                         *02604000
                   MAX_TERM,                                           *02605000
                   MAX_T_QLFY,                                         *02606000
                   BRCH_REG_CODE,                                      *02607000
                   BRCHREG_QLFY,                                       *02608000
                   PRIME_CATG,                                         *02609000
                   PRIME_C_QLFY,                                       *02610000
                   PRIME_C_INCEN,                                      *02611000
                   CMB_BAL_CD_1,                                       *02612000
                   CMB_BAL_CD_1_Q,                                     *02613000
                   CMB_BAL_CD_1_I,                                     *02614000
                   CMB_BAL_CD_2,                                       *02615000
                   CMB_BAL_CD_2_Q,                                     *02616000
                   CMB_BAL_CD_2_I,                                     *02617000
                   BAL_C_CODE_1,                                       *02618000
                   BAL_CATG_Q_1,                                       *02619000
                   BAL_CATG_I_1,                                       *02620000
                   BAL_C_CODE_2,                                       *02621000
                   BAL_CATG_Q_2,                                       *02622000
                   BAL_CATG_I_2,                                       *02623000
                   USER_RTN,                                           *02624000
                   USER_RTN_QLFY,                                      *02625000
                   USER_RTN_INCEN,                                     *02626000
                   PRIMARY_SVC,                                        *02627000
                   SEC_SC_INC_OPT,                                     *02628000
                   DISC_AMT,                                           *02629000
                   DISC_PCT,                                           *02630000
                   PRIMARY_RATE,                                       *02631000
                   SEC_RATE_OPT,                                       *02632000
                   RATE_INCEN,                                         *02633000
                   TIME_OPT,                                           *02634000
                   RATE_BAL_CODE,                                      *02635000
                   CASH_MIN_AMOUNT,                                    *02636000
                   CASH_MAX_AMOUNT,                                    *02637000
                   CASH_BAL_CODE,                                      *02638000
                   MISC_OPTION_1,                                      *02639000
                   MISC_OPTION_2,                                      *02640000
                   MISC_OPTION_3,                                      *02641000
                   SVC_INCEN,                                          *02642000
                   SVC_CHG_BAL_CD,                                     *02643000
                   RATE_PCT,                                           *02644000
                   TRAN_PLAN,                                          *02645000
                   USER_OPT_REL1,                                      *02646000
                   USER_OPT_01,                                        *02647000
                   USER_OPT_Q_1,                                       *02648000
                   USER_OPT_REL2,                                      *02649000
                   USER_OPT_02,                                        *02650000
                   USER_OPT_Q_2,                                       *02651000
                   USER_OPT_REL3,                                      *02652000
                   USER_OPT_03,                                        *02653000
                   USER_OPT_Q_3,                                       *02654000
                   USER_OPT_REL4,                                      *02655000
                   USER_OPT_04,                                        *02656000
                   USER_OPT_Q_4,                                       *02657000
                   CMB_BAL_CD_3,                                       *02658000
                   CMB_BAL_CD_3_Q,                                     *02659000
                   CMB_BAL_CD_3_I,                                     *02660000
                   CMB_BAL_CD_4,                                       *02661000
                   CMB_BAL_CD_4_Q,                                     *02662000
                   CMB_BAL_CD_4_I,                                     *02663000
                   SEC_SVC_OPT,                                        *02664000
                   RATE_OPTION,                                        *02665000
                   SEC_SVC_PLAN,                                       *02666000
                   SEC_SVC_TABLE,                                      *02667000
                   SEC_SVC_BAL_CD,                                     *02668000
                   RATE_TYPE,                                          *02669000
                   USER_TYPE_01,                                       *02670000
                   USER_TYPE_02,                                       *02671000
                   USER_TYPE_03,                                       *02672000
                   USER_TYPE_04                                        *02673000
                 FROM S02                                              *02674000
                 WHERE                                                 *02675000
                    INST_NBR = :INST AND                               *02676000
                    RECORD_NBR = :RECNBR AND                           *02677000
                    MODEL = :MODEL AND                                 *02678000
                    PROD_CODE > :PRODCODE                              *02679000
                 ORDER BY PROD_CODE,                                   *02680000
                   ACCT_TYPE                                           *02681000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       02682000
         EXEC  SQL                                                     *02683000
               OPEN S02SQ002                                            02684000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            02685000
         BNZ   *+8                     NO - RETURN ERROR                02686000
         LR    12,9                    LOAD RETURN ADDRESS              02687000
         BR    12                      RETURN TO FETCH ROUTINE          02688000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02689000
         BR    14                      RETURN TO CALLER                 02690000
         LTORG                                                          02691000
*                                                                       02692000
SQL#0346 DS    0H                                                       02693000
         USING SQL#0346,12             ESTABLISH BASE REGISTER          02694000
         B     *+6                     BRANCH AROUND ADCON              02695000
BASE0346 DC    AL2(4096)                                                02696000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02697000
         AH    3,BASE0346              ADD 4K                           02698000
         EXEC  SQL                                                     *02699000
               CLOSE S02SQ002                                           02700000
         EXEC  SQL                                                     *02701000
               DECLARE S02SQ003 CURSOR FOR                             *02702000
               SELECT INST_NBR,                                        *02703000
                   RECORD_NBR,                                         *02704000
                   MODEL,                                              *02705000
                   PROD_CODE,                                          *02706000
                   ACCT_TYPE,                                          *02707000
                   AUDIT_DATE,                                         *02708000
                   AUDIT_TIME,                                         *02709000
                   AUDIT_USER,                                         *02710000
                   AUDIT_ORG,                                          *02711000
                   IS_PRIMARY,                                         *02712000
                   PROD_GROUP,                                         *02713000
                   MIN_ACCT,                                           *02714000
                   MIN_N_QLFY,                                         *02715000
                   MAX_SCND,                                           *02716000
                   MAX_S_QLFY,                                         *02717000
                   MIN_TERM,                                           *02718000
                   MIN_T_QLFY,                                         *02719000
                   MAX_TERM,                                           *02720000
                   MAX_T_QLFY,                                         *02721000
                   BRCH_REG_CODE,                                      *02722000
                   BRCHREG_QLFY,                                       *02723000
                   PRIME_CATG,                                         *02724000
                   PRIME_C_QLFY,                                       *02725000
                   PRIME_C_INCEN,                                      *02726000
                   CMB_BAL_CD_1,                                       *02727000
                   CMB_BAL_CD_1_Q,                                     *02728000
                   CMB_BAL_CD_1_I,                                     *02729000
                   CMB_BAL_CD_2,                                       *02730000
                   CMB_BAL_CD_2_Q,                                     *02731000
                   CMB_BAL_CD_2_I,                                     *02732000
                   BAL_C_CODE_1,                                       *02733000
                   BAL_CATG_Q_1,                                       *02734000
                   BAL_CATG_I_1,                                       *02735000
                   BAL_C_CODE_2,                                       *02736000
                   BAL_CATG_Q_2,                                       *02737000
                   BAL_CATG_I_2,                                       *02738000
                   USER_RTN,                                           *02739000
                   USER_RTN_QLFY,                                      *02740000
                   USER_RTN_INCEN,                                     *02741000
                   PRIMARY_SVC,                                        *02742000
                   SEC_SC_INC_OPT,                                     *02743000
                   DISC_AMT,                                           *02744000
                   DISC_PCT,                                           *02745000
                   PRIMARY_RATE,                                       *02746000
                   SEC_RATE_OPT,                                       *02747000
                   RATE_INCEN,                                         *02748000
                   TIME_OPT,                                           *02749000
                   RATE_BAL_CODE,                                      *02750000
                   CASH_MIN_AMOUNT,                                    *02751000
                   CASH_MAX_AMOUNT,                                    *02752000
                   CASH_BAL_CODE,                                      *02753000
                   MISC_OPTION_1,                                      *02754000
                   MISC_OPTION_2,                                      *02755000
                   MISC_OPTION_3,                                      *02756000
                   SVC_INCEN,                                          *02757000
                   SVC_CHG_BAL_CD,                                     *02758000
                   RATE_PCT,                                           *02759000
                   TRAN_PLAN,                                          *02760000
                   USER_OPT_REL1,                                      *02761000
                   USER_OPT_01,                                        *02762000
                   USER_OPT_Q_1,                                       *02763000
                   USER_OPT_REL2,                                      *02764000
                   USER_OPT_02,                                        *02765000
                   USER_OPT_Q_2,                                       *02766000
                   USER_OPT_REL3,                                      *02767000
                   USER_OPT_03,                                        *02768000
                   USER_OPT_Q_3,                                       *02769000
                   USER_OPT_REL4,                                      *02770000
                   USER_OPT_04,                                        *02771000
                   USER_OPT_Q_4,                                       *02772000
                   CMB_BAL_CD_3,                                       *02773000
                   CMB_BAL_CD_3_Q,                                     *02774000
                   CMB_BAL_CD_3_I,                                     *02775000
                   CMB_BAL_CD_4,                                       *02776000
                   CMB_BAL_CD_4_Q,                                     *02777000
                   CMB_BAL_CD_4_I,                                     *02778000
                   SEC_SVC_OPT,                                        *02779000
                   RATE_OPTION,                                        *02780000
                   SEC_SVC_PLAN,                                       *02781000
                   SEC_SVC_TABLE,                                      *02782000
                   SEC_SVC_BAL_CD,                                     *02783000
                   RATE_TYPE,                                          *02784000
                   USER_TYPE_01,                                       *02785000
                   USER_TYPE_02,                                       *02786000
                   USER_TYPE_03,                                       *02787000
                   USER_TYPE_04                                        *02788000
                 FROM S02                                              *02789000
                 WHERE                                                 *02790000
                    INST_NBR = :INST AND                               *02791000
                    RECORD_NBR = :RECNBR AND                           *02792000
                    MODEL > :MODEL                                     *02793000
                 ORDER BY MODEL,                                       *02794000
                   PROD_CODE,                                          *02795000
                   ACCT_TYPE                                           *02796000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       02797000
         EXEC  SQL                                                     *02798000
               OPEN S02SQ003                                            02799000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            02800000
         BNZ   *+8                     NO - RETURN ERROR                02801000
         LR    12,9                    LOAD RETURN ADDRESS              02802000
         BR    12                      RETURN TO FETCH ROUTINE          02803000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02804000
         BR    14                      RETURN TO CALLER                 02805000
         LTORG                                                          02806000
*                                                                       02807000
SQL#0446 DS    0H                                                       02808000
         USING SQL#0446,12             ESTABLISH BASE REGISTER          02809000
         B     *+6                     BRANCH AROUND ADCON              02810000
BASE0446 DC    AL2(4096)                                                02811000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02812000
         AH    3,BASE0446              ADD 4K                           02813000
         EXEC  SQL                                                     *02814000
               CLOSE S02SQ003                                           02815000
         EXEC  SQL                                                     *02816000
               DECLARE S02SQ004 CURSOR FOR                             *02817000
               SELECT INST_NBR,                                        *02818000
                   RECORD_NBR,                                         *02819000
                   MODEL,                                              *02820000
                   PROD_CODE,                                          *02821000
                   ACCT_TYPE,                                          *02822000
                   AUDIT_DATE,                                         *02823000
                   AUDIT_TIME,                                         *02824000
                   AUDIT_USER,                                         *02825000
                   AUDIT_ORG,                                          *02826000
                   IS_PRIMARY,                                         *02827000
                   PROD_GROUP,                                         *02828000
                   MIN_ACCT,                                           *02829000
                   MIN_N_QLFY,                                         *02830000
                   MAX_SCND,                                           *02831000
                   MAX_S_QLFY,                                         *02832000
                   MIN_TERM,                                           *02833000
                   MIN_T_QLFY,                                         *02834000
                   MAX_TERM,                                           *02835000
                   MAX_T_QLFY,                                         *02836000
                   BRCH_REG_CODE,                                      *02837000
                   BRCHREG_QLFY,                                       *02838000
                   PRIME_CATG,                                         *02839000
                   PRIME_C_QLFY,                                       *02840000
                   PRIME_C_INCEN,                                      *02841000
                   CMB_BAL_CD_1,                                       *02842000
                   CMB_BAL_CD_1_Q,                                     *02843000
                   CMB_BAL_CD_1_I,                                     *02844000
                   CMB_BAL_CD_2,                                       *02845000
                   CMB_BAL_CD_2_Q,                                     *02846000
                   CMB_BAL_CD_2_I,                                     *02847000
                   BAL_C_CODE_1,                                       *02848000
                   BAL_CATG_Q_1,                                       *02849000
                   BAL_CATG_I_1,                                       *02850000
                   BAL_C_CODE_2,                                       *02851000
                   BAL_CATG_Q_2,                                       *02852000
                   BAL_CATG_I_2,                                       *02853000
                   USER_RTN,                                           *02854000
                   USER_RTN_QLFY,                                      *02855000
                   USER_RTN_INCEN,                                     *02856000
                   PRIMARY_SVC,                                        *02857000
                   SEC_SC_INC_OPT,                                     *02858000
                   DISC_AMT,                                           *02859000
                   DISC_PCT,                                           *02860000
                   PRIMARY_RATE,                                       *02861000
                   SEC_RATE_OPT,                                       *02862000
                   RATE_INCEN,                                         *02863000
                   TIME_OPT,                                           *02864000
                   RATE_BAL_CODE,                                      *02865000
                   CASH_MIN_AMOUNT,                                    *02866000
                   CASH_MAX_AMOUNT,                                    *02867000
                   CASH_BAL_CODE,                                      *02868000
                   MISC_OPTION_1,                                      *02869000
                   MISC_OPTION_2,                                      *02870000
                   MISC_OPTION_3,                                      *02871000
                   SVC_INCEN,                                          *02872000
                   SVC_CHG_BAL_CD,                                     *02873000
                   RATE_PCT,                                           *02874000
                   TRAN_PLAN,                                          *02875000
                   USER_OPT_REL1,                                      *02876000
                   USER_OPT_01,                                        *02877000
                   USER_OPT_Q_1,                                       *02878000
                   USER_OPT_REL2,                                      *02879000
                   USER_OPT_02,                                        *02880000
                   USER_OPT_Q_2,                                       *02881000
                   USER_OPT_REL3,                                      *02882000
                   USER_OPT_03,                                        *02883000
                   USER_OPT_Q_3,                                       *02884000
                   USER_OPT_REL4,                                      *02885000
                   USER_OPT_04,                                        *02886000
                   USER_OPT_Q_4,                                       *02887000
                   CMB_BAL_CD_3,                                       *02888000
                   CMB_BAL_CD_3_Q,                                     *02889000
                   CMB_BAL_CD_3_I,                                     *02890000
                   CMB_BAL_CD_4,                                       *02891000
                   CMB_BAL_CD_4_Q,                                     *02892000
                   CMB_BAL_CD_4_I,                                     *02893000
                   SEC_SVC_OPT,                                        *02894000
                   RATE_OPTION,                                        *02895000
                   SEC_SVC_PLAN,                                       *02896000
                   SEC_SVC_TABLE,                                      *02897000
                   SEC_SVC_BAL_CD,                                     *02898000
                   RATE_TYPE,                                          *02899000
                   USER_TYPE_01,                                       *02900000
                   USER_TYPE_02,                                       *02901000
                   USER_TYPE_03,                                       *02902000
                   USER_TYPE_04                                        *02903000
                 FROM S02                                              *02904000
                 WHERE                                                 *02905000
                    INST_NBR = :INST AND                               *02906000
                    RECORD_NBR > :RECNBR                               *02907000
                 ORDER BY RECORD_NBR,                                  *02908000
                   MODEL,                                              *02909000
                   PROD_CODE,                                          *02910000
                   ACCT_TYPE                                           *02911000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       02912000
         EXEC  SQL                                                     *02913000
               OPEN S02SQ004                                            02914000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            02915000
         BNZ   *+8                     NO - RETURN ERROR                02916000
         LR    12,9                    LOAD RETURN ADDRESS              02917000
         BR    12                      RETURN TO FETCH ROUTINE          02918000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02919000
         BR    14                      RETURN TO CALLER                 02920000
         LTORG                                                          02921000
*                                                                       02922000
SQL#0546 DS    0H                                                       02923000
         USING SQL#0546,12             ESTABLISH BASE REGISTER          02924000
         B     *+6                     BRANCH AROUND ADCON              02925000
BASE0546 DC    AL2(4096)                                                02926000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02927000
         AH    3,BASE0546              ADD 4K                           02928000
         EXEC  SQL                                                     *02929000
               CLOSE S02SQ004                                           02930000
         EXEC  SQL                                                     *02931000
               DECLARE S02SQ005 CURSOR FOR                             *02932000
               SELECT INST_NBR,                                        *02933000
                   RECORD_NBR,                                         *02934000
                   MODEL,                                              *02935000
                   PROD_CODE,                                          *02936000
                   ACCT_TYPE,                                          *02937000
                   AUDIT_DATE,                                         *02938000
                   AUDIT_TIME,                                         *02939000
                   AUDIT_USER,                                         *02940000
                   AUDIT_ORG,                                          *02941000
                   IS_PRIMARY,                                         *02942000
                   PROD_GROUP,                                         *02943000
                   MIN_ACCT,                                           *02944000
                   MIN_N_QLFY,                                         *02945000
                   MAX_SCND,                                           *02946000
                   MAX_S_QLFY,                                         *02947000
                   MIN_TERM,                                           *02948000
                   MIN_T_QLFY,                                         *02949000
                   MAX_TERM,                                           *02950000
                   MAX_T_QLFY,                                         *02951000
                   BRCH_REG_CODE,                                      *02952000
                   BRCHREG_QLFY,                                       *02953000
                   PRIME_CATG,                                         *02954000
                   PRIME_C_QLFY,                                       *02955000
                   PRIME_C_INCEN,                                      *02956000
                   CMB_BAL_CD_1,                                       *02957000
                   CMB_BAL_CD_1_Q,                                     *02958000
                   CMB_BAL_CD_1_I,                                     *02959000
                   CMB_BAL_CD_2,                                       *02960000
                   CMB_BAL_CD_2_Q,                                     *02961000
                   CMB_BAL_CD_2_I,                                     *02962000
                   BAL_C_CODE_1,                                       *02963000
                   BAL_CATG_Q_1,                                       *02964000
                   BAL_CATG_I_1,                                       *02965000
                   BAL_C_CODE_2,                                       *02966000
                   BAL_CATG_Q_2,                                       *02967000
                   BAL_CATG_I_2,                                       *02968000
                   USER_RTN,                                           *02969000
                   USER_RTN_QLFY,                                      *02970000
                   USER_RTN_INCEN,                                     *02971000
                   PRIMARY_SVC,                                        *02972000
                   SEC_SC_INC_OPT,                                     *02973000
                   DISC_AMT,                                           *02974000
                   DISC_PCT,                                           *02975000
                   PRIMARY_RATE,                                       *02976000
                   SEC_RATE_OPT,                                       *02977000
                   RATE_INCEN,                                         *02978000
                   TIME_OPT,                                           *02979000
                   RATE_BAL_CODE,                                      *02980000
                   CASH_MIN_AMOUNT,                                    *02981000
                   CASH_MAX_AMOUNT,                                    *02982000
                   CASH_BAL_CODE,                                      *02983000
                   MISC_OPTION_1,                                      *02984000
                   MISC_OPTION_2,                                      *02985000
                   MISC_OPTION_3,                                      *02986000
                   SVC_INCEN,                                          *02987000
                   SVC_CHG_BAL_CD,                                     *02988000
                   RATE_PCT,                                           *02989000
                   TRAN_PLAN,                                          *02990000
                   USER_OPT_REL1,                                      *02991000
                   USER_OPT_01,                                        *02992000
                   USER_OPT_Q_1,                                       *02993000
                   USER_OPT_REL2,                                      *02994000
                   USER_OPT_02,                                        *02995000
                   USER_OPT_Q_2,                                       *02996000
                   USER_OPT_REL3,                                      *02997000
                   USER_OPT_03,                                        *02998000
                   USER_OPT_Q_3,                                       *02999000
                   USER_OPT_REL4,                                      *03000000
                   USER_OPT_04,                                        *03001000
                   USER_OPT_Q_4,                                       *03002000
                   CMB_BAL_CD_3,                                       *03003000
                   CMB_BAL_CD_3_Q,                                     *03004000
                   CMB_BAL_CD_3_I,                                     *03005000
                   CMB_BAL_CD_4,                                       *03006000
                   CMB_BAL_CD_4_Q,                                     *03007000
                   CMB_BAL_CD_4_I,                                     *03008000
                   SEC_SVC_OPT,                                        *03009000
                   RATE_OPTION,                                        *03010000
                   SEC_SVC_PLAN,                                       *03011000
                   SEC_SVC_TABLE,                                      *03012000
                   SEC_SVC_BAL_CD,                                     *03013000
                   RATE_TYPE,                                          *03014000
                   USER_TYPE_01,                                       *03015000
                   USER_TYPE_02,                                       *03016000
                   USER_TYPE_03,                                       *03017000
                   USER_TYPE_04                                        *03018000
                 FROM S02                                              *03019000
                 WHERE                                                 *03020000
                    INST_NBR > :INST                                   *03021000
                 ORDER BY INST_NBR,                                    *03022000
                   RECORD_NBR,                                         *03023000
                   MODEL,                                              *03024000
                   PROD_CODE,                                          *03025000
                   ACCT_TYPE                                           *03026000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       03027000
         EXEC  SQL                                                     *03028000
               OPEN S02SQ005                                            03029000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            03030000
         BNZ   *+8                     NO - RETURN ERROR                03031000
         LR    12,9                    LOAD RETURN ADDRESS              03032000
         BR    12                      RETURN TO FETCH ROUTINE          03033000
         L     14,SQW@RET              LOAD RETURN ADDRESS              03034000
         BR    14                      RETURN TO CALLER                 03035000
         LTORG                                                          03036000
*                                                                       03037000
*                                                                       03038000
**********************************************************************  03039000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 1:      03040000
*   THIS ROUTINE HANDLES ALTERNATE KEY 1 SEQUENTIAL CURSORS.            03041000
**********************************************************************  03042000
*                                                                       03043000
SQL#0047 DS    0H                                                       03044000
         USING SQL#0047,12             ESTABLISH BASE REGISTER          03045000
         LA    15,255                  SET RETURN CODE                  03046000
         L     14,SQW@RET              LOAD RETURN ADDRESS              03047000
         BR    14                      RETURN TO CALLER                 03048000
         LTORG                                                          03049000
*                                                                       03050000
*                                                                       03051000
**********************************************************************  03052000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 2:      03053000
*   THIS ROUTINE HANDLES ALTERNATE KEY 2 SEQUENTIAL CURSORS.            03054000
**********************************************************************  03055000
*                                                                       03056000
SQL#0048 DS    0H                                                       03057000
         USING SQL#0048,12             ESTABLISH BASE REGISTER          03058000
         LA    15,255                  SET RETURN CODE                  03059000
         L     14,SQW@RET              LOAD RETURN ADDRESS              03060000
         BR    14                      RETURN TO CALLER                 03061000
         LTORG                                                          03062000
*                                                                       03063000
*                                                                       03064000
**********************************************************************  03065000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 3:      03066000
*   THIS ROUTINE HANDLES ALTERNATE KEY 3 SEQUENTIAL CURSORS.            03067000
**********************************************************************  03068000
*                                                                       03069000
SQL#0049 DS    0H                                                       03070000
         USING SQL#0049,12             ESTABLISH BASE REGISTER          03071000
         LA    15,255                  SET RETURN CODE                  03072000
         L     14,SQW@RET              LOAD RETURN ADDRESS              03073000
         BR    14                      RETURN TO CALLER                 03074000
         LTORG                                                          03075000
*                                                                       03076000
         DS    0H                      END OF SQL STATEMENTS            03077000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   03078000
*                                                                       03079000
*                                                                       03080000
**********************************************************************  03081000
* DUMMY ENTRY POINT DSNHLI                                              03082000
*   MUST NOT BE MODIFIED.                                               03083000
**********************************************************************  03084000
*                                                                       03085000
         ENTRY DSNHLI                                                   03086000
DSNHLI   DS    0H                                                       03087000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       03088000
         BR    15                      BRANCH TO ATTACH FACILITY        03089000
*                                                                       03090000
*                                                                       03091000
**********************************************************************  03092000
* CONVERSION ROUTINES INTO AND OUT OF ASSEMBLER HOST VARIABLES          03093000
*   REQUIRED IF THE COBOL RECORD STRUCTURE IS NOT ALIGNED.              03094000
*   MUST NOT BE MODIFIED.                                               03095000
**********************************************************************  03096000
*                                                                       03097000
CONV#RTN CSECT                         CONVERSION ROUTINES SECTION      03098000
CONV#RTN AMODE ANY                                                      03099000
CONV#RTN RMODE ANY                                                      03100000
IN#HOST  DS    0H                      MOVE INTO HOST VARIABLES         03101000
         USING IN#HOST,5               ESTABLISH BASE REGISTER          03102000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    03103000
IN#HOST1 CLI   0(6),X'FF'              END OF TABLE?                    03104000
         BER   4                         YES - RETURN TO SQL            03105000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         03106000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    03107000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  03108000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 03109000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              03110000
         LR    15,1                    SET BOTH LENGTHS EQUAL           03111000
         MVCL  14,0                    MOVE TO HOST VARIABLES           03112000
         LA    6,8(6)                  INCREMENT TO NEXT ENTRY          03113000
         B     IN#HOST1                LOOP UNTIL END OF TABLE          03114000
*                                                                       03115000
IN#KEY   DS    0H                      MOVE KEYS INTO HOST VARIABLES    03116000
         USING IN#KEY,5                ESTABLISH BASE REGISTER          03117000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    03118000
IN#KEY1  CLI   0(6),X'FF'              END OF TABLE?                    03119000
         BER   4                         YES - RETURN TO SQL            03120000
         SLR   0,0                     CLEAR FOR INSERT                 03121000
         IC    0,6(6)                  INSERT KEY FIELD ENTRY           03122000
         NR    0,3                     FOR THE CURRENT KEY?             03123000
         BZ    IN#KEY2                   NO - GO FOR NEXT ENTRY         03124000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         03125000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    03126000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  03127000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 03128000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              03129000
         LR    15,1                    SET BOTH LENGTHS EQUAL           03130000
         MVCL  14,0                    MOVE TO HOST VARIABLES           03131000
         TM    7(6),X'80'              SIGNED DECIMAL FIELD?            03132000
         BNO   IN#KEY2                   NO - GO FOR NEXT ENTRY         03133000
         BCTR  14,0                    DECREMENT TO SIGN BYTE           03134000
         TM    0(14),X'0A'             VALID SIGN BYTE?                 03135000
         BO    IN#KEY2                   YES - GO FOR NEXT ENTRY        03136000
         TM    0(14),X'0C'             VALID SIGN BYTE?                 03137000
         BO    IN#KEY2                   YES - GO FOR NEXT ENTRY        03138000
         OI    0(14),X'0F'               NO  - MAKE IT VALID            03139000
IN#KEY2  LA    6,8(6)                  INCREMENT TO NEXT ENTRY          03140000
         B     IN#KEY1                 LOOP UNTIL END OF TABLE          03141000
*                                                                       03142000
OUT#REC  DS    0H                      MOVE HOST VARIABLES INTO RECORD  03143000
         USING OUT#REC,5               ESTABLISH BASE REGISTER          03144000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    03145000
OUT#REC1 CLI   0(6),X'FF'              END OF TABLE?                    03146000
         BER   4                         YES - RETURN TO SQL            03147000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         03148000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    03149000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  03150000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 03151000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              03152000
         LR    15,1                    SET BOTH LENGTHS EQUAL           03153000
         MVCL  0,14                    MOVE TO RECORD AREA              03154000
         LA    6,8(6)                  INCREMENT TO NEXT ENTRY          03155000
         B     OUT#REC1                LOOP UNTIL END OF TABLE          03156000
*                                                                       03157000
OUT#KEY  DS    0H                      MOVE KEYS INTO RECORD            03158000
         USING OUT#KEY,5               ESTABLISH BASE REGISTER          03159000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    03160000
OUT#KEY1 CLI   0(6),X'FF'              END OF TABLE?                    03161000
         BER   4                         YES - RETURN TO SQL            03162000
         TM    6(6),X'80'              FOR THE PRIMARY KEY?             03163000
         BZ    OUT#KEY2                  NO - GO FOR NEXT ENTRY         03164000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         03165000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    03166000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  03167000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 03168000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              03169000
         LR    15,1                    SET BOTH LENGTHS EQUAL           03170000
         MVCL  0,14                    MOVE TO RECORD AREA              03171000
OUT#KEY2 LA    6,8(6)                  INCREMENT TO NEXT ENTRY          03172000
         B     OUT#KEY1                LOOP UNTIL END OF TABLE          03173000
*                                                                       03174000
*                                                                       03175000
**********************************************************************  03176000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  03177000
*   MUST NOT BE MODIFIED.                                               03178000
**********************************************************************  03179000
*                                                                       03180000
CONV#TAB DC    H'0000'                 COBOL RECORD AREA OFFSET         03181000
         DC    H'0000'                 HOST VARIABLE AREA OFFSET        03182000
         DC    H'0027'                 LENGTH TO MOVE                   03183000
         DC    X'80'                   80 = KEY 0 FIELD                 03184000
*                                      40 = KEY 1 FIELD                 03185000
*                                      20 = KEY 2 FIELD                 03186000
*                                      10 = KEY 3 FIELD                 03187000
         DC    X'00'                   80 = SIGNED DECIMAL FIELD        03188000
*                                                                       03189000
*                                      REST OF FIELD ENTRIES            03190000
         DC    H'0027',H'0027',H'0181',X'00',X'00'                      03191000
         DC    8X'FF'                  END OF FIELD ENTRIES             03192000
         LTORG                                                          03193000
         END                                                            03194000
