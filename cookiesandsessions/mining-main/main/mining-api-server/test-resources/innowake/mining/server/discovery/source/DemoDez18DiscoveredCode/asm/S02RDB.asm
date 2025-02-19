**********************************************************************  00001000
*                                                                       00002000
*  S02RDB .... STATIC SQL READ MODULE                                   00003000
*                                                                       00004000
*  CREATION DATE: 04/25/16                                              00005000
*                                                                       00006000
*  FUNCTIONAL DESCRIPTION: THIS PROGRAM CONTAINS THE STATIC SQL         00007000
*  VECTORS REQUIRED TO SUPPORT I/O TO THE S02 TABLE.  IT IS LOADED      00008000
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
PRODCODE DS    CL6                                                      00053000
ACCTTYPE DS    CL3                                                      00054000
AUDDATE  DS    PL5'0.'                                                  00055000
AUDTIME  DS    PL5'0.'                                                  00056000
AUDUSER  DS    CL8                                                      00057000
AUDORG   DS    CL6                                                      00058000
ISPRIM   DS    CL1                                                      00059000
PRODGRP  DS    CL1                                                      00060000
MINACCT  DS    CL2                                                      00061000
MNENQLFY DS    CL1                                                      00062000
MAXSCND  DS    CL2                                                      00063000
MSENQLFY DS    CL1                                                      00064000
MINTERM  DS    CL5                                                      00065000
MINQLFY  DS    CL1                                                      00066000
MAXTERM  DS    CL5                                                      00067000
MAXQLFY  DS    CL1                                                      00068000
BRREGCD  DS    CL6                                                      00069000
BRRGQLFY DS    CL1                                                      00070000
PRIMCATG DS    CL6                                                      00071000
PRIMQLFY DS    CL1                                                      00072000
PRIMINC  DS    CL1                                                      00073000
CBALCAT1 DS    CL2                                                      00074000
CBCAT1Q  DS    CL1                                                      00075000
COMBBAL1 DS    CL1                                                      00076000
CBALCAT2 DS    CL2                                                      00077000
CBCAT2Q  DS    CL1                                                      00078000
COMBBAL2 DS    CL1                                                      00079000
BALCAT1  DS    CL6                                                      00080000
BALQLFY1 DS    CL1                                                      00081000
BALINCV1 DS    CL1                                                      00082000
BALCAT2  DS    CL6                                                      00083000
BALQLFY2 DS    CL1                                                      00084000
BALINCV2 DS    CL1                                                      00085000
USERRTN  DS    CL1                                                      00086000
USERQLFY DS    CL1                                                      00087000
USINCTV  DS    CL1                                                      00088000
PRIMESVC DS    CL1                                                      00089000
SCNDSVC  DS    CL1                                                      00090000
DISCAMT  DS    PL3'0.00'                                                00091000
DISCPCT  DS    PL3'0.00000'                                             00092000
PRIMERTE DS    CL1                                                      00093000
SCNDRATE DS    CL1                                                      00094000
RTINCEN  DS    CL6                                                      00095000
TIMEOPT  DS    CL1                                                      00096000
RTBALCD  DS    CL2                                                      00097000
CASHMIN  DS    PL5'0.'                                                  00098000
CASHMAX  DS    PL5'0.'                                                  00099000
CASHBAL  DS    CL2                                                      00100000
MISCOPT1 DS    CL1                                                      00101000
MISCOPT2 DS    CL1                                                      00102000
MISCOPT3 DS    CL1                                                      00103000
SCINCEN  DS    CL6                                                      00104000
SCBALCD  DS    CL2                                                      00105000
RATEPCT  DS    PL5'0.000000000'                                         00106000
TRANPLAN DS    CL3                                                      00107000
UOPTREL1 DS    CL2                                                      00108000
UOPT1    DS    CL1                                                      00109000
UOPTQ1   DS    CL1                                                      00110000
UOPTREL2 DS    CL2                                                      00111000
UOPT2    DS    CL1                                                      00112000
UOPTQ2   DS    CL1                                                      00113000
UOPTREL3 DS    CL2                                                      00114000
UOPT3    DS    CL1                                                      00115000
UOPTQ3   DS    CL1                                                      00116000
UOPTREL4 DS    CL2                                                      00117000
UOPT4    DS    CL1                                                      00118000
UOPTQ4   DS    CL1                                                      00119000
CBALCAT3 DS    CL2                                                      00120000
CBCAT3Q  DS    CL1                                                      00121000
COMBBAL3 DS    CL1                                                      00122000
CBALCAT4 DS    CL2                                                      00123000
CBCAT4Q  DS    CL1                                                      00124000
COMBBAL4 DS    CL1                                                      00125000
SECSVOPT DS    CL1                                                      00126000
RATEOPT  DS    CL1                                                      00127000
SSPLAN   DS    CL6                                                      00128000
SSTABLE  DS    CL6                                                      00129000
SSBALCD  DS    CL2                                                      00130000
RTETYP   DS    CL1                                                      00131000
USERTYP1 DS    CL1                                                      00132000
USERTYP2 DS    CL1                                                      00133000
USERTYP3 DS    CL1                                                      00134000
USERTYP4 DS    CL1                                                      00135000
*                                                                       00136000
         ORG   ASMREC+(2000-L'SQWADATA) POINT TO ADDITIONAL DATA        00137000
SQWADATA DS    0CL400                  ADDITIONAL DATA PASSED TO MODULE 00138000
SQWSEGF  DS    CL102                   SEGMENTED FROM KEY VALUE         00139000
SQWSEGT  DS    CL102                   SEGMENTED TO KEY VALUE           00140000
SQWAUDIT DS    CL99                    CALLERS AUDIT DATA               00141000
         DS    CL97                    RESERVED                         00142000
*                                                                       00143000
INDVARS  DS    0H                      NULL INDICATOR VARIABLES         00144000
INDVARX  DS    0H                                                       00145000
INDVARL  EQU   INDVARX-INDVARS         NULL INDICATOR AREA LENGTH       00146000
*                                                                       00147000
*                                                                       00148000
**********************************************************************  00149000
* ROWSET SQLDA AREA ADDRESSED BY REGISTER 2                             00150000
**********************************************************************  00151000
*                                                                       00152000
SQDSQLDA DSECT                         ROWSET SQLDA AREA                00153000
*                                                                       00154000
**********************************************************************  00155000
* PROGRAM TABLE HEADER SECTION:                                         00156000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00157000
**********************************************************************  00158000
*                                                                       00159000
S02RDB   CSECT                         PROGRAM TABLE SECTION            00160000
S02RDB   AMODE ANY                                                      00161000
S02RDB   RMODE ANY                                                      00162000
         DC    CL8'S02RDB  '           PROGRAM ID                       00163000
         DC    CL1' '                                                   00164000
         DC    CL8'&SYSDATE'           ASSEMBLY DATE                    00165000
         DC    CL1' '                                                   00166000
         DC    CL5'&SYSTIME'           ASSEMBLY TIME                    00167000
         DC    CL1' '                                                   00168000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00169000
         DC    5A(0)                   RESERVED                         00170000
         DC    AL2(0)                  RESERVED                         00171000
         DC    AL2(INDVARL)            NULL INDICATOR AREA LENGTH       00172000
         DC    A(CONVTAB1)             RECORD/HOST CONVERSION TABLE     00173000
         DC    A(CONVTAB2)             SQLDA DATA TYPE/LENGTH TABLE     00174000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00175000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00176000
         DC    CL29'WWW.INFOR.COM                '                      00176001
*                                                                       00177000
**********************************************************************  00178000
* STATEMENT TABLE SECTION:                                              00179000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00180000
**********************************************************************  00181000
*                                                                       00182000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00183000
STM#TAB  AMODE ANY                                                      00184000
STM#TAB  RMODE ANY                                                      00185000
         DC    A(SELIN0)               SELECT INTO (KEY 0)              00186000
         DC    A(SELIN1)               SELECT INTO (KEY 1)              00187000
         DC    A(SELIN2)               SELECT INTO (KEY 2)              00188000
         DC    A(SELIN3)               SELECT INTO (KEY 3)              00189000
         DC    12A(0)                  UDB MODULE VECTORS               00190000
         DC    A(SELXC0)               SELECT SEQ CURSOR (KEY 0)        00191000
         DC    A(SELXC1)               SELECT SEQ CURSOR (KEY 1)        00192000
         DC    A(SELXC2)               SELECT SEQ CURSOR (KEY 2)        00193000
         DC    A(SELXC3)               SELECT SEQ CURSOR (KEY 3)        00194000
         DC    A(FETDC0)               FETCH SEQ DATA CURSOR (KEY 0)    00195000
         DC    A(FETDC1)               FETCH SEQ DATA CURSOR (KEY 1)    00196000
         DC    A(FETDC2)               FETCH SEQ DATA CURSOR (KEY 2)    00197000
         DC    A(FETDC3)               FETCH SEQ DATA CURSOR (KEY 3)    00198000
         DC    A(FETKC0)               FETCH SEQ KEY CURSOR (KEY 0)     00199000
         DC    A(FETKC1)               FETCH SEQ KEY CURSOR (KEY 1)     00200000
         DC    A(FETKC2)               FETCH SEQ KEY CURSOR (KEY 2)     00201000
         DC    A(FETKC3)               FETCH SEQ KEY CURSOR (KEY 3)     00202000
         DC    A(CLSXC0)               CLOSE SEQ CURSOR (KEY 0)         00203000
         DC    A(CLSXC1)               CLOSE SEQ CURSOR (KEY 1)         00204000
         DC    A(CLSXC2)               CLOSE SEQ CURSOR (KEY 2)         00205000
         DC    A(CLSXC3)               CLOSE SEQ CURSOR (KEY 3)         00206000
         DC    A(SELKY0)               SELECT KEY (KEY 0)               00207000
         DC    A(SELKY1)               SELECT KEY (KEY 1)               00208000
         DC    A(SELKY2)               SELECT KEY (KEY 2)               00209000
         DC    A(SELKY3)               SELECT KEY (KEY 3)               00210000
         DC    10A(0)                  UDB MODULE VECTORS               00211000
         DC    4X'FF'                                                   00212000
*                                                                       00213000
**********************************************************************  00214000
* SQL STATEMENT SECTION:                                                00215000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00216000
*     TO SUPPORT THIS TABLE.                                            00217000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00218000
*     ARE EQUIVALENT.                                                   00219000
**********************************************************************  00220000
*                                                                       00221000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00222000
SQL#STMT AMODE ANY                                                      00223000
SQL#STMT RMODE ANY                                                      00224000
         USING SQDSQLDA,2              ADDRESS ROWSET SQLDA AREA        00225000
         USING SQLDSECT,10,3           ADDRESS SQLDSECT                 00226000
         USING COM#AREA,11             ADDRESS COMMAREA                 00227000
*                                                                       00228000
**********************************************************************  00229000
* SELECT INTO STATEMENT BY PRIMARY KEY:                                 00230000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00231000
**********************************************************************  00232000
*                                                                       00233000
SELIN0   DS    0H                                                       00234000
         USING SELIN0,12               ESTABLISH BASE REGISTER          00235000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00236000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00237000
         BALR  14,15                   MOVE REQUESTED DATA              00238000
         B     *+6                     BRANCH AROUND ADCON              00239000
BASIN0   DC    AL2(4096)                                                00240000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00241000
         AH    3,BASIN0                ADD 4K                           00242000
         EXEC  SQL SELECT                                              *00243000
                   AUDIT_DATE,                                         *00244000
                   AUDIT_TIME,                                         *00245000
                   AUDIT_USER,                                         *00246000
                   AUDIT_ORG,                                          *00247000
                   IS_PRIMARY,                                         *00248000
                   PROD_GROUP,                                         *00249000
                   MIN_ACCT,                                           *00250000
                   MIN_N_QLFY,                                         *00251000
                   MAX_SCND,                                           *00252000
                   MAX_S_QLFY,                                         *00253000
                   MIN_TERM,                                           *00254000
                   MIN_T_QLFY,                                         *00255000
                   MAX_TERM,                                           *00256000
                   MAX_T_QLFY,                                         *00257000
                   BRCH_REG_CODE,                                      *00258000
                   BRCHREG_QLFY,                                       *00259000
                   PRIME_CATG,                                         *00260000
                   PRIME_C_QLFY,                                       *00261000
                   PRIME_C_INCEN,                                      *00262000
                   CMB_BAL_CD_1,                                       *00263000
                   CMB_BAL_CD_1_Q,                                     *00264000
                   CMB_BAL_CD_1_I,                                     *00265000
                   CMB_BAL_CD_2,                                       *00266000
                   CMB_BAL_CD_2_Q,                                     *00267000
                   CMB_BAL_CD_2_I,                                     *00268000
                   BAL_C_CODE_1,                                       *00269000
                   BAL_CATG_Q_1,                                       *00270000
                   BAL_CATG_I_1,                                       *00271000
                   BAL_C_CODE_2,                                       *00272000
                   BAL_CATG_Q_2,                                       *00273000
                   BAL_CATG_I_2,                                       *00274000
                   USER_RTN,                                           *00275000
                   USER_RTN_QLFY,                                      *00276000
                   USER_RTN_INCEN,                                     *00277000
                   PRIMARY_SVC,                                        *00278000
                   SEC_SC_INC_OPT,                                     *00279000
                   DISC_AMT,                                           *00280000
                   DISC_PCT,                                           *00281000
                   PRIMARY_RATE,                                       *00282000
                   SEC_RATE_OPT,                                       *00283000
                   RATE_INCEN,                                         *00284000
                   TIME_OPT,                                           *00285000
                   RATE_BAL_CODE,                                      *00286000
                   CASH_MIN_AMOUNT,                                    *00287000
                   CASH_MAX_AMOUNT,                                    *00288000
                   CASH_BAL_CODE,                                      *00289000
                   MISC_OPTION_1,                                      *00290000
                   MISC_OPTION_2,                                      *00291000
                   MISC_OPTION_3,                                      *00292000
                   SVC_INCEN,                                          *00293000
                   SVC_CHG_BAL_CD,                                     *00294000
                   RATE_PCT,                                           *00295000
                   TRAN_PLAN,                                          *00296000
                   USER_OPT_REL1,                                      *00297000
                   USER_OPT_01,                                        *00298000
                   USER_OPT_Q_1,                                       *00299000
                   USER_OPT_REL2,                                      *00300000
                   USER_OPT_02,                                        *00301000
                   USER_OPT_Q_2,                                       *00302000
                   USER_OPT_REL3,                                      *00303000
                   USER_OPT_03,                                        *00304000
                   USER_OPT_Q_3,                                       *00305000
                   USER_OPT_REL4,                                      *00306000
                   USER_OPT_04,                                        *00307000
                   USER_OPT_Q_4,                                       *00308000
                   CMB_BAL_CD_3,                                       *00309000
                   CMB_BAL_CD_3_Q,                                     *00310000
                   CMB_BAL_CD_3_I,                                     *00311000
                   CMB_BAL_CD_4,                                       *00312000
                   CMB_BAL_CD_4_Q,                                     *00313000
                   CMB_BAL_CD_4_I,                                     *00314000
                   SEC_SVC_OPT,                                        *00315000
                   RATE_OPTION,                                        *00316000
                   SEC_SVC_PLAN,                                       *00317000
                   SEC_SVC_TABLE,                                      *00318000
                   SEC_SVC_BAL_CD,                                     *00319000
                   RATE_TYPE,                                          *00320000
                   USER_TYPE_01,                                       *00321000
                   USER_TYPE_02,                                       *00322000
                   USER_TYPE_03,                                       *00323000
                   USER_TYPE_04                                        *00324000
                 INTO                                                  *00325000
                   :AUDDATE,                                           *00326000
                   :AUDTIME,                                           *00327000
                   :AUDUSER,                                           *00328000
                   :AUDORG,                                            *00329000
                   :ISPRIM,                                            *00330000
                   :PRODGRP,                                           *00331000
                   :MINACCT,                                           *00332000
                   :MNENQLFY,                                          *00333000
                   :MAXSCND,                                           *00334000
                   :MSENQLFY,                                          *00335000
                   :MINTERM,                                           *00336000
                   :MINQLFY,                                           *00337000
                   :MAXTERM,                                           *00338000
                   :MAXQLFY,                                           *00339000
                   :BRREGCD,                                           *00340000
                   :BRRGQLFY,                                          *00341000
                   :PRIMCATG,                                          *00342000
                   :PRIMQLFY,                                          *00343000
                   :PRIMINC,                                           *00344000
                   :CBALCAT1,                                          *00345000
                   :CBCAT1Q,                                           *00346000
                   :COMBBAL1,                                          *00347000
                   :CBALCAT2,                                          *00348000
                   :CBCAT2Q,                                           *00349000
                   :COMBBAL2,                                          *00350000
                   :BALCAT1,                                           *00351000
                   :BALQLFY1,                                          *00352000
                   :BALINCV1,                                          *00353000
                   :BALCAT2,                                           *00354000
                   :BALQLFY2,                                          *00355000
                   :BALINCV2,                                          *00356000
                   :USERRTN,                                           *00357000
                   :USERQLFY,                                          *00358000
                   :USINCTV,                                           *00359000
                   :PRIMESVC,                                          *00360000
                   :SCNDSVC,                                           *00361000
                   :DISCAMT,                                           *00362000
                   :DISCPCT,                                           *00363000
                   :PRIMERTE,                                          *00364000
                   :SCNDRATE,                                          *00365000
                   :RTINCEN,                                           *00366000
                   :TIMEOPT,                                           *00367000
                   :RTBALCD,                                           *00368000
                   :CASHMIN,                                           *00369000
                   :CASHMAX,                                           *00370000
                   :CASHBAL,                                           *00371000
                   :MISCOPT1,                                          *00372000
                   :MISCOPT2,                                          *00373000
                   :MISCOPT3,                                          *00374000
                   :SCINCEN,                                           *00375000
                   :SCBALCD,                                           *00376000
                   :RATEPCT,                                           *00377000
                   :TRANPLAN,                                          *00378000
                   :UOPTREL1,                                          *00379000
                   :UOPT1,                                             *00380000
                   :UOPTQ1,                                            *00381000
                   :UOPTREL2,                                          *00382000
                   :UOPT2,                                             *00383000
                   :UOPTQ2,                                            *00384000
                   :UOPTREL3,                                          *00385000
                   :UOPT3,                                             *00386000
                   :UOPTQ3,                                            *00387000
                   :UOPTREL4,                                          *00388000
                   :UOPT4,                                             *00389000
                   :UOPTQ4,                                            *00390000
                   :CBALCAT3,                                          *00391000
                   :CBCAT3Q,                                           *00392000
                   :COMBBAL3,                                          *00393000
                   :CBALCAT4,                                          *00394000
                   :CBCAT4Q,                                           *00395000
                   :COMBBAL4,                                          *00396000
                   :SECSVOPT,                                          *00397000
                   :RATEOPT,                                           *00398000
                   :SSPLAN,                                            *00399000
                   :SSTABLE,                                           *00400000
                   :SSBALCD,                                           *00401000
                   :RTETYP,                                            *00402000
                   :USERTYP1,                                          *00403000
                   :USERTYP2,                                          *00404000
                   :USERTYP3,                                          *00405000
                   :USERTYP4                                           *00406000
                 FROM S02                                              *00407000
                 WHERE                                                 *00408000
                   INST_NBR = :INST AND                                *00409000
                   RECORD_NBR = :RECNBR AND                            *00410000
                   MODEL = :MODEL AND                                  *00411000
                   PROD_CODE = :PRODCODE AND                           *00412000
                   ACCT_TYPE = :ACCTTYPE                               *00413000
                 FETCH FIRST 1 ROW ONLY                                 00414000
         MVI   SQWKMRP,X'03'           MOVE HOST VARIABLES TO RECORD    00415000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00416000
         BALR  14,15                   MOVE REQUESTED DATA              00417000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00418000
         BR    14                      RETURN TO CALLER                 00419000
         LTORG                                                          00420000
*                                                                       00421000
**********************************************************************  00422000
* SELECT COUNT STATEMENT BY PRIMARY KEY:                                00423000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            00424000
**********************************************************************  00425000
*                                                                       00426000
SELKY0   DS    0H                                                       00427000
         USING SELKY0,12               ESTABLISH BASE REGISTER          00428000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00429000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00430000
         BALR  14,15                   MOVE REQUESTED DATA              00431000
         B     *+6                     BRANCH AROUND ADCON              00432000
BASKY0   DC    AL2(4096)                                                00433000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00434000
         AH    3,BASKY0                ADD 4K                           00435000
         EXEC  SQL SELECT                                              *00436000
                   COUNT(*)                                            *00437000
                 INTO                                                  *00438000
                   :SQWINTHV                                           *00439000
                 FROM S02                                              *00440000
                 WHERE                                                 *00441000
                   INST_NBR = :INST AND                                *00442000
                   RECORD_NBR = :RECNBR AND                            *00443000
                   MODEL = :MODEL AND                                  *00444000
                   PROD_CODE = :PRODCODE AND                           *00445000
                   ACCT_TYPE = :ACCTTYPE                               *00446000
                 FETCH FIRST 1 ROW ONLY                                 00447000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00448000
         CLC   SQLCODE,=F'0'           GOOD RETURN FROM SQL?            00449000
         BNER  14                      NO - RETURN TO CALLER            00450000
         CLC   SQWINTHV,=F'0'          ANY ROWS MATCH WHERE CLAUSE?     00451000
         BNER  14                      YES - RETURN ZERO SQLCODE        00452000
         MVC   SQLCODE,=F'+100'        SET SQLCODE TO ROW NOT FOUND     00453000
         BR    14                      RETURN TO CALLER                 00454000
         LTORG                                                          00455000
*                                                                       00456000
**********************************************************************  00457000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY    00458000
* KEY:                                                                  00459000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00460000
*     AND GET-NEXT-LOCK VERBS.                                          00461000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00462000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00463000
**********************************************************************  00464000
*                                                                       00465000
SELXC0   DS    0H                                                       00466000
         USING SELXC0,12               ESTABLISH BASE REGISTER          00467000
         MVI   SQWKMRP,X'86'           SET HOST KEY 0 & CURSOR POINTER  00468000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00469000
         BALR  14,15                   MOVE DATA & SET CURSOR POINTER   00470000
         LA    12,SELXC0P              LOAD VECTOR TABLE ADDRESS        00471000
         AH    12,SQWCSRSP             COMPUTE POINTER TO OPEN ROUTINE  00472000
         L     12,0(12)                LOAD OPEN ROUTINE ADDRESS        00473000
         BR    12                      GO TO CURSOR OPEN ROUTINE        00474000
SELXC0P  DC    A(SELGE001)                                              00475000
         DC    (KY0COLMS-01)A(0)                                        00476000
         DC    A(SELGE002)                                              00477000
         DC    (KY0COLMS-02)A(0)                                        00478000
         DC    A(SELGE003)                                              00479000
         DC    (KY0COLMS-03)A(0)                                        00480000
         DC    A(SELGE004)                                              00481000
         DC    (KY0COLMS-04)A(0)                                        00482000
         DC    A(SELGE005)                                              00483000
         LTORG                                                          00484000
*                                                                       00485000
SELGE001 DS    0H                                                       00486000
         USING SELGE001,12             ESTABLISH BASE REGISTER          00487000
         B     *+6                     BRANCH AROUND ADCON              00488000
BASGE001 DC    AL2(4096)                                                00489000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00490000
         AH    3,BASGE001              ADD 4K                           00491000
         EXEC  SQL DECLARE S02GE001 CURSOR                             *00492000
               WITH ROWSET POSITIONING                                 *00493000
               FOR SELECT                                              *00494000
                   INST_NBR,                                           *00495000
                   RECORD_NBR,                                         *00496000
                   MODEL,                                              *00497000
                   PROD_CODE,                                          *00498000
                   ACCT_TYPE,                                          *00499000
                   AUDIT_DATE,                                         *00500000
                   AUDIT_TIME,                                         *00501000
                   AUDIT_USER,                                         *00502000
                   AUDIT_ORG,                                          *00503000
                   IS_PRIMARY,                                         *00504000
                   PROD_GROUP,                                         *00505000
                   MIN_ACCT,                                           *00506000
                   MIN_N_QLFY,                                         *00507000
                   MAX_SCND,                                           *00508000
                   MAX_S_QLFY,                                         *00509000
                   MIN_TERM,                                           *00510000
                   MIN_T_QLFY,                                         *00511000
                   MAX_TERM,                                           *00512000
                   MAX_T_QLFY,                                         *00513000
                   BRCH_REG_CODE,                                      *00514000
                   BRCHREG_QLFY,                                       *00515000
                   PRIME_CATG,                                         *00516000
                   PRIME_C_QLFY,                                       *00517000
                   PRIME_C_INCEN,                                      *00518000
                   CMB_BAL_CD_1,                                       *00519000
                   CMB_BAL_CD_1_Q,                                     *00520000
                   CMB_BAL_CD_1_I,                                     *00521000
                   CMB_BAL_CD_2,                                       *00522000
                   CMB_BAL_CD_2_Q,                                     *00523000
                   CMB_BAL_CD_2_I,                                     *00524000
                   BAL_C_CODE_1,                                       *00525000
                   BAL_CATG_Q_1,                                       *00526000
                   BAL_CATG_I_1,                                       *00527000
                   BAL_C_CODE_2,                                       *00528000
                   BAL_CATG_Q_2,                                       *00529000
                   BAL_CATG_I_2,                                       *00530000
                   USER_RTN,                                           *00531000
                   USER_RTN_QLFY,                                      *00532000
                   USER_RTN_INCEN,                                     *00533000
                   PRIMARY_SVC,                                        *00534000
                   SEC_SC_INC_OPT,                                     *00535000
                   DISC_AMT,                                           *00536000
                   DISC_PCT,                                           *00537000
                   PRIMARY_RATE,                                       *00538000
                   SEC_RATE_OPT,                                       *00539000
                   RATE_INCEN,                                         *00540000
                   TIME_OPT,                                           *00541000
                   RATE_BAL_CODE,                                      *00542000
                   CASH_MIN_AMOUNT,                                    *00543000
                   CASH_MAX_AMOUNT,                                    *00544000
                   CASH_BAL_CODE,                                      *00545000
                   MISC_OPTION_1,                                      *00546000
                   MISC_OPTION_2,                                      *00547000
                   MISC_OPTION_3,                                      *00548000
                   SVC_INCEN,                                          *00549000
                   SVC_CHG_BAL_CD,                                     *00550000
                   RATE_PCT,                                           *00551000
                   TRAN_PLAN,                                          *00552000
                   USER_OPT_REL1,                                      *00553000
                   USER_OPT_01,                                        *00554000
                   USER_OPT_Q_1,                                       *00555000
                   USER_OPT_REL2,                                      *00556000
                   USER_OPT_02,                                        *00557000
                   USER_OPT_Q_2,                                       *00558000
                   USER_OPT_REL3,                                      *00559000
                   USER_OPT_03,                                        *00560000
                   USER_OPT_Q_3,                                       *00561000
                   USER_OPT_REL4,                                      *00562000
                   USER_OPT_04,                                        *00563000
                   USER_OPT_Q_4,                                       *00564000
                   CMB_BAL_CD_3,                                       *00565000
                   CMB_BAL_CD_3_Q,                                     *00566000
                   CMB_BAL_CD_3_I,                                     *00567000
                   CMB_BAL_CD_4,                                       *00568000
                   CMB_BAL_CD_4_Q,                                     *00569000
                   CMB_BAL_CD_4_I,                                     *00570000
                   SEC_SVC_OPT,                                        *00571000
                   RATE_OPTION,                                        *00572000
                   SEC_SVC_PLAN,                                       *00573000
                   SEC_SVC_TABLE,                                      *00574000
                   SEC_SVC_BAL_CD,                                     *00575000
                   RATE_TYPE,                                          *00576000
                   USER_TYPE_01,                                       *00577000
                   USER_TYPE_02,                                       *00578000
                   USER_TYPE_03,                                       *00579000
                   USER_TYPE_04                                        *00580000
                 FROM S02                                              *00581000
                 WHERE                                                 *00582000
                   INST_NBR = :INST AND                                *00583000
                   RECORD_NBR = :RECNBR AND                            *00584000
                   MODEL = :MODEL AND                                  *00585000
                   PROD_CODE = :PRODCODE AND                           *00586000
                   ACCT_TYPE >=                                        *00587000
                     :ACCTTYPE                                         *00588000
                 ORDER BY ACCT_TYPE ASC                                *00589000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00590000
         EXEC  SQL OPEN S02GE001                                        00591000
         MVC   SQWCSRCA,=A(CLSGE001)   SET CURSOR CLOSE ROUTINE ADDRESS 00592000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00593000
         BR    14                      RETURN TO CALLER                 00594000
         LTORG                                                          00595000
*                                                                       00596000
SELGE002 DS    0H                                                       00597000
         USING SELGE002,12             ESTABLISH BASE REGISTER          00598000
         B     *+6                     BRANCH AROUND ADCON              00599000
BASGE002 DC    AL2(4096)                                                00600000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00601000
         AH    3,BASGE002              ADD 4K                           00602000
         EXEC  SQL DECLARE S02GE002 CURSOR                             *00603000
               WITH ROWSET POSITIONING                                 *00604000
               FOR SELECT                                              *00605000
                   INST_NBR,                                           *00606000
                   RECORD_NBR,                                         *00607000
                   MODEL,                                              *00608000
                   PROD_CODE,                                          *00609000
                   ACCT_TYPE,                                          *00610000
                   AUDIT_DATE,                                         *00611000
                   AUDIT_TIME,                                         *00612000
                   AUDIT_USER,                                         *00613000
                   AUDIT_ORG,                                          *00614000
                   IS_PRIMARY,                                         *00615000
                   PROD_GROUP,                                         *00616000
                   MIN_ACCT,                                           *00617000
                   MIN_N_QLFY,                                         *00618000
                   MAX_SCND,                                           *00619000
                   MAX_S_QLFY,                                         *00620000
                   MIN_TERM,                                           *00621000
                   MIN_T_QLFY,                                         *00622000
                   MAX_TERM,                                           *00623000
                   MAX_T_QLFY,                                         *00624000
                   BRCH_REG_CODE,                                      *00625000
                   BRCHREG_QLFY,                                       *00626000
                   PRIME_CATG,                                         *00627000
                   PRIME_C_QLFY,                                       *00628000
                   PRIME_C_INCEN,                                      *00629000
                   CMB_BAL_CD_1,                                       *00630000
                   CMB_BAL_CD_1_Q,                                     *00631000
                   CMB_BAL_CD_1_I,                                     *00632000
                   CMB_BAL_CD_2,                                       *00633000
                   CMB_BAL_CD_2_Q,                                     *00634000
                   CMB_BAL_CD_2_I,                                     *00635000
                   BAL_C_CODE_1,                                       *00636000
                   BAL_CATG_Q_1,                                       *00637000
                   BAL_CATG_I_1,                                       *00638000
                   BAL_C_CODE_2,                                       *00639000
                   BAL_CATG_Q_2,                                       *00640000
                   BAL_CATG_I_2,                                       *00641000
                   USER_RTN,                                           *00642000
                   USER_RTN_QLFY,                                      *00643000
                   USER_RTN_INCEN,                                     *00644000
                   PRIMARY_SVC,                                        *00645000
                   SEC_SC_INC_OPT,                                     *00646000
                   DISC_AMT,                                           *00647000
                   DISC_PCT,                                           *00648000
                   PRIMARY_RATE,                                       *00649000
                   SEC_RATE_OPT,                                       *00650000
                   RATE_INCEN,                                         *00651000
                   TIME_OPT,                                           *00652000
                   RATE_BAL_CODE,                                      *00653000
                   CASH_MIN_AMOUNT,                                    *00654000
                   CASH_MAX_AMOUNT,                                    *00655000
                   CASH_BAL_CODE,                                      *00656000
                   MISC_OPTION_1,                                      *00657000
                   MISC_OPTION_2,                                      *00658000
                   MISC_OPTION_3,                                      *00659000
                   SVC_INCEN,                                          *00660000
                   SVC_CHG_BAL_CD,                                     *00661000
                   RATE_PCT,                                           *00662000
                   TRAN_PLAN,                                          *00663000
                   USER_OPT_REL1,                                      *00664000
                   USER_OPT_01,                                        *00665000
                   USER_OPT_Q_1,                                       *00666000
                   USER_OPT_REL2,                                      *00667000
                   USER_OPT_02,                                        *00668000
                   USER_OPT_Q_2,                                       *00669000
                   USER_OPT_REL3,                                      *00670000
                   USER_OPT_03,                                        *00671000
                   USER_OPT_Q_3,                                       *00672000
                   USER_OPT_REL4,                                      *00673000
                   USER_OPT_04,                                        *00674000
                   USER_OPT_Q_4,                                       *00675000
                   CMB_BAL_CD_3,                                       *00676000
                   CMB_BAL_CD_3_Q,                                     *00677000
                   CMB_BAL_CD_3_I,                                     *00678000
                   CMB_BAL_CD_4,                                       *00679000
                   CMB_BAL_CD_4_Q,                                     *00680000
                   CMB_BAL_CD_4_I,                                     *00681000
                   SEC_SVC_OPT,                                        *00682000
                   RATE_OPTION,                                        *00683000
                   SEC_SVC_PLAN,                                       *00684000
                   SEC_SVC_TABLE,                                      *00685000
                   SEC_SVC_BAL_CD,                                     *00686000
                   RATE_TYPE,                                          *00687000
                   USER_TYPE_01,                                       *00688000
                   USER_TYPE_02,                                       *00689000
                   USER_TYPE_03,                                       *00690000
                   USER_TYPE_04                                        *00691000
                 FROM S02                                              *00692000
                 WHERE                                                 *00693000
                   INST_NBR = :INST AND                                *00694000
                   RECORD_NBR = :RECNBR AND                            *00695000
                   MODEL = :MODEL AND                                  *00696000
                   PROD_CODE >=                                        *00697000
                     :PRODCODE                                         *00698000
                 ORDER BY PROD_CODE ASC,                               *00699000
                   ACCT_TYPE ASC                                       *00700000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00701000
         EXEC  SQL OPEN S02GE002                                        00702000
         MVC   SQWCSRCA,=A(CLSGE002)   SET CURSOR CLOSE ROUTINE ADDRESS 00703000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00704000
         BR    14                      RETURN TO CALLER                 00705000
         LTORG                                                          00706000
*                                                                       00707000
SELGE003 DS    0H                                                       00708000
         USING SELGE003,12             ESTABLISH BASE REGISTER          00709000
         B     *+6                     BRANCH AROUND ADCON              00710000
BASGE003 DC    AL2(4096)                                                00711000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00712000
         AH    3,BASGE003              ADD 4K                           00713000
         EXEC  SQL DECLARE S02GE003 CURSOR                             *00714000
               WITH ROWSET POSITIONING                                 *00715000
               FOR SELECT                                              *00716000
                   INST_NBR,                                           *00717000
                   RECORD_NBR,                                         *00718000
                   MODEL,                                              *00719000
                   PROD_CODE,                                          *00720000
                   ACCT_TYPE,                                          *00721000
                   AUDIT_DATE,                                         *00722000
                   AUDIT_TIME,                                         *00723000
                   AUDIT_USER,                                         *00724000
                   AUDIT_ORG,                                          *00725000
                   IS_PRIMARY,                                         *00726000
                   PROD_GROUP,                                         *00727000
                   MIN_ACCT,                                           *00728000
                   MIN_N_QLFY,                                         *00729000
                   MAX_SCND,                                           *00730000
                   MAX_S_QLFY,                                         *00731000
                   MIN_TERM,                                           *00732000
                   MIN_T_QLFY,                                         *00733000
                   MAX_TERM,                                           *00734000
                   MAX_T_QLFY,                                         *00735000
                   BRCH_REG_CODE,                                      *00736000
                   BRCHREG_QLFY,                                       *00737000
                   PRIME_CATG,                                         *00738000
                   PRIME_C_QLFY,                                       *00739000
                   PRIME_C_INCEN,                                      *00740000
                   CMB_BAL_CD_1,                                       *00741000
                   CMB_BAL_CD_1_Q,                                     *00742000
                   CMB_BAL_CD_1_I,                                     *00743000
                   CMB_BAL_CD_2,                                       *00744000
                   CMB_BAL_CD_2_Q,                                     *00745000
                   CMB_BAL_CD_2_I,                                     *00746000
                   BAL_C_CODE_1,                                       *00747000
                   BAL_CATG_Q_1,                                       *00748000
                   BAL_CATG_I_1,                                       *00749000
                   BAL_C_CODE_2,                                       *00750000
                   BAL_CATG_Q_2,                                       *00751000
                   BAL_CATG_I_2,                                       *00752000
                   USER_RTN,                                           *00753000
                   USER_RTN_QLFY,                                      *00754000
                   USER_RTN_INCEN,                                     *00755000
                   PRIMARY_SVC,                                        *00756000
                   SEC_SC_INC_OPT,                                     *00757000
                   DISC_AMT,                                           *00758000
                   DISC_PCT,                                           *00759000
                   PRIMARY_RATE,                                       *00760000
                   SEC_RATE_OPT,                                       *00761000
                   RATE_INCEN,                                         *00762000
                   TIME_OPT,                                           *00763000
                   RATE_BAL_CODE,                                      *00764000
                   CASH_MIN_AMOUNT,                                    *00765000
                   CASH_MAX_AMOUNT,                                    *00766000
                   CASH_BAL_CODE,                                      *00767000
                   MISC_OPTION_1,                                      *00768000
                   MISC_OPTION_2,                                      *00769000
                   MISC_OPTION_3,                                      *00770000
                   SVC_INCEN,                                          *00771000
                   SVC_CHG_BAL_CD,                                     *00772000
                   RATE_PCT,                                           *00773000
                   TRAN_PLAN,                                          *00774000
                   USER_OPT_REL1,                                      *00775000
                   USER_OPT_01,                                        *00776000
                   USER_OPT_Q_1,                                       *00777000
                   USER_OPT_REL2,                                      *00778000
                   USER_OPT_02,                                        *00779000
                   USER_OPT_Q_2,                                       *00780000
                   USER_OPT_REL3,                                      *00781000
                   USER_OPT_03,                                        *00782000
                   USER_OPT_Q_3,                                       *00783000
                   USER_OPT_REL4,                                      *00784000
                   USER_OPT_04,                                        *00785000
                   USER_OPT_Q_4,                                       *00786000
                   CMB_BAL_CD_3,                                       *00787000
                   CMB_BAL_CD_3_Q,                                     *00788000
                   CMB_BAL_CD_3_I,                                     *00789000
                   CMB_BAL_CD_4,                                       *00790000
                   CMB_BAL_CD_4_Q,                                     *00791000
                   CMB_BAL_CD_4_I,                                     *00792000
                   SEC_SVC_OPT,                                        *00793000
                   RATE_OPTION,                                        *00794000
                   SEC_SVC_PLAN,                                       *00795000
                   SEC_SVC_TABLE,                                      *00796000
                   SEC_SVC_BAL_CD,                                     *00797000
                   RATE_TYPE,                                          *00798000
                   USER_TYPE_01,                                       *00799000
                   USER_TYPE_02,                                       *00800000
                   USER_TYPE_03,                                       *00801000
                   USER_TYPE_04                                        *00802000
                 FROM S02                                              *00803000
                 WHERE                                                 *00804000
                   INST_NBR = :INST AND                                *00805000
                   RECORD_NBR = :RECNBR AND                            *00806000
                   MODEL >=                                            *00807000
                     :MODEL                                            *00808000
                 ORDER BY MODEL ASC,                                   *00809000
                   PROD_CODE ASC,                                      *00810000
                   ACCT_TYPE ASC                                       *00811000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00812000
         EXEC  SQL OPEN S02GE003                                        00813000
         MVC   SQWCSRCA,=A(CLSGE003)   SET CURSOR CLOSE ROUTINE ADDRESS 00814000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00815000
         BR    14                      RETURN TO CALLER                 00816000
         LTORG                                                          00817000
*                                                                       00818000
SELGE004 DS    0H                                                       00819000
         USING SELGE004,12             ESTABLISH BASE REGISTER          00820000
         B     *+6                     BRANCH AROUND ADCON              00821000
BASGE004 DC    AL2(4096)                                                00822000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00823000
         AH    3,BASGE004              ADD 4K                           00824000
         EXEC  SQL DECLARE S02GE004 CURSOR                             *00825000
               WITH ROWSET POSITIONING                                 *00826000
               FOR SELECT                                              *00827000
                   INST_NBR,                                           *00828000
                   RECORD_NBR,                                         *00829000
                   MODEL,                                              *00830000
                   PROD_CODE,                                          *00831000
                   ACCT_TYPE,                                          *00832000
                   AUDIT_DATE,                                         *00833000
                   AUDIT_TIME,                                         *00834000
                   AUDIT_USER,                                         *00835000
                   AUDIT_ORG,                                          *00836000
                   IS_PRIMARY,                                         *00837000
                   PROD_GROUP,                                         *00838000
                   MIN_ACCT,                                           *00839000
                   MIN_N_QLFY,                                         *00840000
                   MAX_SCND,                                           *00841000
                   MAX_S_QLFY,                                         *00842000
                   MIN_TERM,                                           *00843000
                   MIN_T_QLFY,                                         *00844000
                   MAX_TERM,                                           *00845000
                   MAX_T_QLFY,                                         *00846000
                   BRCH_REG_CODE,                                      *00847000
                   BRCHREG_QLFY,                                       *00848000
                   PRIME_CATG,                                         *00849000
                   PRIME_C_QLFY,                                       *00850000
                   PRIME_C_INCEN,                                      *00851000
                   CMB_BAL_CD_1,                                       *00852000
                   CMB_BAL_CD_1_Q,                                     *00853000
                   CMB_BAL_CD_1_I,                                     *00854000
                   CMB_BAL_CD_2,                                       *00855000
                   CMB_BAL_CD_2_Q,                                     *00856000
                   CMB_BAL_CD_2_I,                                     *00857000
                   BAL_C_CODE_1,                                       *00858000
                   BAL_CATG_Q_1,                                       *00859000
                   BAL_CATG_I_1,                                       *00860000
                   BAL_C_CODE_2,                                       *00861000
                   BAL_CATG_Q_2,                                       *00862000
                   BAL_CATG_I_2,                                       *00863000
                   USER_RTN,                                           *00864000
                   USER_RTN_QLFY,                                      *00865000
                   USER_RTN_INCEN,                                     *00866000
                   PRIMARY_SVC,                                        *00867000
                   SEC_SC_INC_OPT,                                     *00868000
                   DISC_AMT,                                           *00869000
                   DISC_PCT,                                           *00870000
                   PRIMARY_RATE,                                       *00871000
                   SEC_RATE_OPT,                                       *00872000
                   RATE_INCEN,                                         *00873000
                   TIME_OPT,                                           *00874000
                   RATE_BAL_CODE,                                      *00875000
                   CASH_MIN_AMOUNT,                                    *00876000
                   CASH_MAX_AMOUNT,                                    *00877000
                   CASH_BAL_CODE,                                      *00878000
                   MISC_OPTION_1,                                      *00879000
                   MISC_OPTION_2,                                      *00880000
                   MISC_OPTION_3,                                      *00881000
                   SVC_INCEN,                                          *00882000
                   SVC_CHG_BAL_CD,                                     *00883000
                   RATE_PCT,                                           *00884000
                   TRAN_PLAN,                                          *00885000
                   USER_OPT_REL1,                                      *00886000
                   USER_OPT_01,                                        *00887000
                   USER_OPT_Q_1,                                       *00888000
                   USER_OPT_REL2,                                      *00889000
                   USER_OPT_02,                                        *00890000
                   USER_OPT_Q_2,                                       *00891000
                   USER_OPT_REL3,                                      *00892000
                   USER_OPT_03,                                        *00893000
                   USER_OPT_Q_3,                                       *00894000
                   USER_OPT_REL4,                                      *00895000
                   USER_OPT_04,                                        *00896000
                   USER_OPT_Q_4,                                       *00897000
                   CMB_BAL_CD_3,                                       *00898000
                   CMB_BAL_CD_3_Q,                                     *00899000
                   CMB_BAL_CD_3_I,                                     *00900000
                   CMB_BAL_CD_4,                                       *00901000
                   CMB_BAL_CD_4_Q,                                     *00902000
                   CMB_BAL_CD_4_I,                                     *00903000
                   SEC_SVC_OPT,                                        *00904000
                   RATE_OPTION,                                        *00905000
                   SEC_SVC_PLAN,                                       *00906000
                   SEC_SVC_TABLE,                                      *00907000
                   SEC_SVC_BAL_CD,                                     *00908000
                   RATE_TYPE,                                          *00909000
                   USER_TYPE_01,                                       *00910000
                   USER_TYPE_02,                                       *00911000
                   USER_TYPE_03,                                       *00912000
                   USER_TYPE_04                                        *00913000
                 FROM S02                                              *00914000
                 WHERE                                                 *00915000
                   INST_NBR = :INST AND                                *00916000
                   RECORD_NBR >=                                       *00917000
                     :RECNBR                                           *00918000
                 ORDER BY RECORD_NBR ASC,                              *00919000
                   MODEL ASC,                                          *00920000
                   PROD_CODE ASC,                                      *00921000
                   ACCT_TYPE ASC                                       *00922000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00923000
         EXEC  SQL OPEN S02GE004                                        00924000
         MVC   SQWCSRCA,=A(CLSGE004)   SET CURSOR CLOSE ROUTINE ADDRESS 00925000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00926000
         BR    14                      RETURN TO CALLER                 00927000
         LTORG                                                          00928000
*                                                                       00929000
SELGE005 DS    0H                                                       00930000
         USING SELGE005,12             ESTABLISH BASE REGISTER          00931000
         B     *+6                     BRANCH AROUND ADCON              00932000
BASGE005 DC    AL2(4096)                                                00933000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00934000
         AH    3,BASGE005              ADD 4K                           00935000
         EXEC  SQL DECLARE S02GE005 CURSOR                             *00936000
               WITH ROWSET POSITIONING                                 *00937000
               FOR SELECT                                              *00938000
                   INST_NBR,                                           *00939000
                   RECORD_NBR,                                         *00940000
                   MODEL,                                              *00941000
                   PROD_CODE,                                          *00942000
                   ACCT_TYPE,                                          *00943000
                   AUDIT_DATE,                                         *00944000
                   AUDIT_TIME,                                         *00945000
                   AUDIT_USER,                                         *00946000
                   AUDIT_ORG,                                          *00947000
                   IS_PRIMARY,                                         *00948000
                   PROD_GROUP,                                         *00949000
                   MIN_ACCT,                                           *00950000
                   MIN_N_QLFY,                                         *00951000
                   MAX_SCND,                                           *00952000
                   MAX_S_QLFY,                                         *00953000
                   MIN_TERM,                                           *00954000
                   MIN_T_QLFY,                                         *00955000
                   MAX_TERM,                                           *00956000
                   MAX_T_QLFY,                                         *00957000
                   BRCH_REG_CODE,                                      *00958000
                   BRCHREG_QLFY,                                       *00959000
                   PRIME_CATG,                                         *00960000
                   PRIME_C_QLFY,                                       *00961000
                   PRIME_C_INCEN,                                      *00962000
                   CMB_BAL_CD_1,                                       *00963000
                   CMB_BAL_CD_1_Q,                                     *00964000
                   CMB_BAL_CD_1_I,                                     *00965000
                   CMB_BAL_CD_2,                                       *00966000
                   CMB_BAL_CD_2_Q,                                     *00967000
                   CMB_BAL_CD_2_I,                                     *00968000
                   BAL_C_CODE_1,                                       *00969000
                   BAL_CATG_Q_1,                                       *00970000
                   BAL_CATG_I_1,                                       *00971000
                   BAL_C_CODE_2,                                       *00972000
                   BAL_CATG_Q_2,                                       *00973000
                   BAL_CATG_I_2,                                       *00974000
                   USER_RTN,                                           *00975000
                   USER_RTN_QLFY,                                      *00976000
                   USER_RTN_INCEN,                                     *00977000
                   PRIMARY_SVC,                                        *00978000
                   SEC_SC_INC_OPT,                                     *00979000
                   DISC_AMT,                                           *00980000
                   DISC_PCT,                                           *00981000
                   PRIMARY_RATE,                                       *00982000
                   SEC_RATE_OPT,                                       *00983000
                   RATE_INCEN,                                         *00984000
                   TIME_OPT,                                           *00985000
                   RATE_BAL_CODE,                                      *00986000
                   CASH_MIN_AMOUNT,                                    *00987000
                   CASH_MAX_AMOUNT,                                    *00988000
                   CASH_BAL_CODE,                                      *00989000
                   MISC_OPTION_1,                                      *00990000
                   MISC_OPTION_2,                                      *00991000
                   MISC_OPTION_3,                                      *00992000
                   SVC_INCEN,                                          *00993000
                   SVC_CHG_BAL_CD,                                     *00994000
                   RATE_PCT,                                           *00995000
                   TRAN_PLAN,                                          *00996000
                   USER_OPT_REL1,                                      *00997000
                   USER_OPT_01,                                        *00998000
                   USER_OPT_Q_1,                                       *00999000
                   USER_OPT_REL2,                                      *01000000
                   USER_OPT_02,                                        *01001000
                   USER_OPT_Q_2,                                       *01002000
                   USER_OPT_REL3,                                      *01003000
                   USER_OPT_03,                                        *01004000
                   USER_OPT_Q_3,                                       *01005000
                   USER_OPT_REL4,                                      *01006000
                   USER_OPT_04,                                        *01007000
                   USER_OPT_Q_4,                                       *01008000
                   CMB_BAL_CD_3,                                       *01009000
                   CMB_BAL_CD_3_Q,                                     *01010000
                   CMB_BAL_CD_3_I,                                     *01011000
                   CMB_BAL_CD_4,                                       *01012000
                   CMB_BAL_CD_4_Q,                                     *01013000
                   CMB_BAL_CD_4_I,                                     *01014000
                   SEC_SVC_OPT,                                        *01015000
                   RATE_OPTION,                                        *01016000
                   SEC_SVC_PLAN,                                       *01017000
                   SEC_SVC_TABLE,                                      *01018000
                   SEC_SVC_BAL_CD,                                     *01019000
                   RATE_TYPE,                                          *01020000
                   USER_TYPE_01,                                       *01021000
                   USER_TYPE_02,                                       *01022000
                   USER_TYPE_03,                                       *01023000
                   USER_TYPE_04                                        *01024000
                 FROM S02                                              *01025000
                 WHERE                                                 *01026000
                   INST_NBR >=                                         *01027000
                     :INST                                             *01028000
                 ORDER BY INST_NBR ASC,                                *01029000
                   RECORD_NBR ASC,                                     *01030000
                   MODEL ASC,                                          *01031000
                   PROD_CODE ASC,                                      *01032000
                   ACCT_TYPE ASC                                       *01033000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01034000
         EXEC  SQL OPEN S02GE005                                        01035000
         MVC   SQWCSRCA,=A(CLSGE005)   SET CURSOR CLOSE ROUTINE ADDRESS 01036000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01037000
         BR    14                      RETURN TO CALLER                 01038000
         LTORG                                                          01039000
*                                                                       01040000
**********************************************************************  01041000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY KEY:          01042000
*   THIS ROUTINE HANDLES PRIMARY KEY SEQUENTIAL CURSORS.                01043000
**********************************************************************  01044000
*                                                                       01045000
NXTXC0   DS    0H                                                       01046000
         USING NXTXC0,12               ESTABLISH BASE REGISTER          01047000
         MVI   SQWKMRP,X'87'           CLOSE CURSOR & SET HOST KEY 0    01048000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01049000
         BALR  14,15                   MOVE REQUESTED DATA              01050000
         LH    1,SQWCSRSP              LOAD CURRENT CURSOR POINTER      01051000
         LA    1,4(1)                  INCREMENT TO NEXT CURSOR         01052000
         STH   1,SQWCSRSP              SAVE POINTER FOR NEXT CALL       01053000
         LA    12,NXTXC0P(1)           LOAD POINTER TO NEXT CURSOR      01054000
         L     12,0(12)                LOAD CURSOR ROUTINE ADDRESS      01055000
         BR    12                      GO TO CURRENT CURSOR ROUTINE     01056000
NXTXC0P  DC    A(0)                                                     01057000
         DC    A(NXTGT002)                                              01058000
         DC    A(NXTGT003)                                              01059000
         DC    A(NXTGT004)                                              01060000
         DC    A(NXTGT005)                                              01061000
         DC    A(NXTGT099)                                              01062000
         DC    A(NXTGT003)                                              01063000
         DC    A(NXTGT004)                                              01064000
         DC    A(NXTGT005)                                              01065000
         DC    A(NXTGT099)                                              01066000
         DC    A(NXTGT004)                                              01067000
         DC    A(NXTGT005)                                              01068000
         DC    A(NXTGT099)                                              01069000
         DC    A(NXTGT005)                                              01070000
         DC    A(NXTGT099)                                              01071000
         DC    A(NXTGT099)                                              01072000
NXTGT099 LA    0,4                     LOAD VALUE 4 IN REGISTER 0       01073000
         SR    1,0                     ADJUST BACK TO CURRENT POINTER   01074000
         STH   1,SQWCSRSP              SAVE POINTER FOR CURSOR CLOSE    01075000
         LA    0,100                   LOAD VALUE 100 IN REGISTER 0     01076000
         ST    0,SQLCODE               SET SQLCODE TO NO MORE ROWS      01077000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01078000
         BR    14                      RETURN TO SQI                    01079000
         LTORG                                                          01080000
*                                                                       01081000
NXTGT002 DS    0H                                                       01082000
         USING NXTGT002,12             ESTABLISH BASE REGISTER          01083000
         B     *+6                     BRANCH AROUND ADCON              01084000
BASGT002 DC    AL2(4096)                                                01085000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01086000
         AH    3,BASGT002              ADD 4K                           01087000
         EXEC  SQL DECLARE S02GT002 CURSOR                             *01088000
               WITH ROWSET POSITIONING                                 *01089000
               FOR SELECT                                              *01090000
                   INST_NBR,                                           *01091000
                   RECORD_NBR,                                         *01092000
                   MODEL,                                              *01093000
                   PROD_CODE,                                          *01094000
                   ACCT_TYPE,                                          *01095000
                   AUDIT_DATE,                                         *01096000
                   AUDIT_TIME,                                         *01097000
                   AUDIT_USER,                                         *01098000
                   AUDIT_ORG,                                          *01099000
                   IS_PRIMARY,                                         *01100000
                   PROD_GROUP,                                         *01101000
                   MIN_ACCT,                                           *01102000
                   MIN_N_QLFY,                                         *01103000
                   MAX_SCND,                                           *01104000
                   MAX_S_QLFY,                                         *01105000
                   MIN_TERM,                                           *01106000
                   MIN_T_QLFY,                                         *01107000
                   MAX_TERM,                                           *01108000
                   MAX_T_QLFY,                                         *01109000
                   BRCH_REG_CODE,                                      *01110000
                   BRCHREG_QLFY,                                       *01111000
                   PRIME_CATG,                                         *01112000
                   PRIME_C_QLFY,                                       *01113000
                   PRIME_C_INCEN,                                      *01114000
                   CMB_BAL_CD_1,                                       *01115000
                   CMB_BAL_CD_1_Q,                                     *01116000
                   CMB_BAL_CD_1_I,                                     *01117000
                   CMB_BAL_CD_2,                                       *01118000
                   CMB_BAL_CD_2_Q,                                     *01119000
                   CMB_BAL_CD_2_I,                                     *01120000
                   BAL_C_CODE_1,                                       *01121000
                   BAL_CATG_Q_1,                                       *01122000
                   BAL_CATG_I_1,                                       *01123000
                   BAL_C_CODE_2,                                       *01124000
                   BAL_CATG_Q_2,                                       *01125000
                   BAL_CATG_I_2,                                       *01126000
                   USER_RTN,                                           *01127000
                   USER_RTN_QLFY,                                      *01128000
                   USER_RTN_INCEN,                                     *01129000
                   PRIMARY_SVC,                                        *01130000
                   SEC_SC_INC_OPT,                                     *01131000
                   DISC_AMT,                                           *01132000
                   DISC_PCT,                                           *01133000
                   PRIMARY_RATE,                                       *01134000
                   SEC_RATE_OPT,                                       *01135000
                   RATE_INCEN,                                         *01136000
                   TIME_OPT,                                           *01137000
                   RATE_BAL_CODE,                                      *01138000
                   CASH_MIN_AMOUNT,                                    *01139000
                   CASH_MAX_AMOUNT,                                    *01140000
                   CASH_BAL_CODE,                                      *01141000
                   MISC_OPTION_1,                                      *01142000
                   MISC_OPTION_2,                                      *01143000
                   MISC_OPTION_3,                                      *01144000
                   SVC_INCEN,                                          *01145000
                   SVC_CHG_BAL_CD,                                     *01146000
                   RATE_PCT,                                           *01147000
                   TRAN_PLAN,                                          *01148000
                   USER_OPT_REL1,                                      *01149000
                   USER_OPT_01,                                        *01150000
                   USER_OPT_Q_1,                                       *01151000
                   USER_OPT_REL2,                                      *01152000
                   USER_OPT_02,                                        *01153000
                   USER_OPT_Q_2,                                       *01154000
                   USER_OPT_REL3,                                      *01155000
                   USER_OPT_03,                                        *01156000
                   USER_OPT_Q_3,                                       *01157000
                   USER_OPT_REL4,                                      *01158000
                   USER_OPT_04,                                        *01159000
                   USER_OPT_Q_4,                                       *01160000
                   CMB_BAL_CD_3,                                       *01161000
                   CMB_BAL_CD_3_Q,                                     *01162000
                   CMB_BAL_CD_3_I,                                     *01163000
                   CMB_BAL_CD_4,                                       *01164000
                   CMB_BAL_CD_4_Q,                                     *01165000
                   CMB_BAL_CD_4_I,                                     *01166000
                   SEC_SVC_OPT,                                        *01167000
                   RATE_OPTION,                                        *01168000
                   SEC_SVC_PLAN,                                       *01169000
                   SEC_SVC_TABLE,                                      *01170000
                   SEC_SVC_BAL_CD,                                     *01171000
                   RATE_TYPE,                                          *01172000
                   USER_TYPE_01,                                       *01173000
                   USER_TYPE_02,                                       *01174000
                   USER_TYPE_03,                                       *01175000
                   USER_TYPE_04                                        *01176000
                 FROM S02                                              *01177000
                 WHERE                                                 *01178000
                   INST_NBR = :INST AND                                *01179000
                   RECORD_NBR = :RECNBR AND                            *01180000
                   MODEL = :MODEL AND                                  *01181000
                   PROD_CODE >                                         *01182000
                     :PRODCODE                                         *01183000
                 ORDER BY PROD_CODE ASC,                               *01184000
                   ACCT_TYPE ASC                                       *01185000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01186000
         EXEC  SQL OPEN S02GT002                                        01187000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            01188000
         BNZ   *+14                    NO - RETURN ERROR                01189000
         MVC   SQWCSRCA,=A(CLSGT002)   SET CURSOR CLOSE ROUTINE ADDRESS 01190000
         L     12,SQWCSRRA             LOAD RETURN ADDRESS              01191000
         BR    12                      RETURN TO FETCH ROUTINE          01192000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01193000
         BR    14                      RETURN TO CALLER                 01194000
         LTORG                                                          01195000
*                                                                       01196000
NXTGT003 DS    0H                                                       01197000
         USING NXTGT003,12             ESTABLISH BASE REGISTER          01198000
         B     *+6                     BRANCH AROUND ADCON              01199000
BASGT003 DC    AL2(4096)                                                01200000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01201000
         AH    3,BASGT003              ADD 4K                           01202000
         EXEC  SQL DECLARE S02GT003 CURSOR                             *01203000
               WITH ROWSET POSITIONING                                 *01204000
               FOR SELECT                                              *01205000
                   INST_NBR,                                           *01206000
                   RECORD_NBR,                                         *01207000
                   MODEL,                                              *01208000
                   PROD_CODE,                                          *01209000
                   ACCT_TYPE,                                          *01210000
                   AUDIT_DATE,                                         *01211000
                   AUDIT_TIME,                                         *01212000
                   AUDIT_USER,                                         *01213000
                   AUDIT_ORG,                                          *01214000
                   IS_PRIMARY,                                         *01215000
                   PROD_GROUP,                                         *01216000
                   MIN_ACCT,                                           *01217000
                   MIN_N_QLFY,                                         *01218000
                   MAX_SCND,                                           *01219000
                   MAX_S_QLFY,                                         *01220000
                   MIN_TERM,                                           *01221000
                   MIN_T_QLFY,                                         *01222000
                   MAX_TERM,                                           *01223000
                   MAX_T_QLFY,                                         *01224000
                   BRCH_REG_CODE,                                      *01225000
                   BRCHREG_QLFY,                                       *01226000
                   PRIME_CATG,                                         *01227000
                   PRIME_C_QLFY,                                       *01228000
                   PRIME_C_INCEN,                                      *01229000
                   CMB_BAL_CD_1,                                       *01230000
                   CMB_BAL_CD_1_Q,                                     *01231000
                   CMB_BAL_CD_1_I,                                     *01232000
                   CMB_BAL_CD_2,                                       *01233000
                   CMB_BAL_CD_2_Q,                                     *01234000
                   CMB_BAL_CD_2_I,                                     *01235000
                   BAL_C_CODE_1,                                       *01236000
                   BAL_CATG_Q_1,                                       *01237000
                   BAL_CATG_I_1,                                       *01238000
                   BAL_C_CODE_2,                                       *01239000
                   BAL_CATG_Q_2,                                       *01240000
                   BAL_CATG_I_2,                                       *01241000
                   USER_RTN,                                           *01242000
                   USER_RTN_QLFY,                                      *01243000
                   USER_RTN_INCEN,                                     *01244000
                   PRIMARY_SVC,                                        *01245000
                   SEC_SC_INC_OPT,                                     *01246000
                   DISC_AMT,                                           *01247000
                   DISC_PCT,                                           *01248000
                   PRIMARY_RATE,                                       *01249000
                   SEC_RATE_OPT,                                       *01250000
                   RATE_INCEN,                                         *01251000
                   TIME_OPT,                                           *01252000
                   RATE_BAL_CODE,                                      *01253000
                   CASH_MIN_AMOUNT,                                    *01254000
                   CASH_MAX_AMOUNT,                                    *01255000
                   CASH_BAL_CODE,                                      *01256000
                   MISC_OPTION_1,                                      *01257000
                   MISC_OPTION_2,                                      *01258000
                   MISC_OPTION_3,                                      *01259000
                   SVC_INCEN,                                          *01260000
                   SVC_CHG_BAL_CD,                                     *01261000
                   RATE_PCT,                                           *01262000
                   TRAN_PLAN,                                          *01263000
                   USER_OPT_REL1,                                      *01264000
                   USER_OPT_01,                                        *01265000
                   USER_OPT_Q_1,                                       *01266000
                   USER_OPT_REL2,                                      *01267000
                   USER_OPT_02,                                        *01268000
                   USER_OPT_Q_2,                                       *01269000
                   USER_OPT_REL3,                                      *01270000
                   USER_OPT_03,                                        *01271000
                   USER_OPT_Q_3,                                       *01272000
                   USER_OPT_REL4,                                      *01273000
                   USER_OPT_04,                                        *01274000
                   USER_OPT_Q_4,                                       *01275000
                   CMB_BAL_CD_3,                                       *01276000
                   CMB_BAL_CD_3_Q,                                     *01277000
                   CMB_BAL_CD_3_I,                                     *01278000
                   CMB_BAL_CD_4,                                       *01279000
                   CMB_BAL_CD_4_Q,                                     *01280000
                   CMB_BAL_CD_4_I,                                     *01281000
                   SEC_SVC_OPT,                                        *01282000
                   RATE_OPTION,                                        *01283000
                   SEC_SVC_PLAN,                                       *01284000
                   SEC_SVC_TABLE,                                      *01285000
                   SEC_SVC_BAL_CD,                                     *01286000
                   RATE_TYPE,                                          *01287000
                   USER_TYPE_01,                                       *01288000
                   USER_TYPE_02,                                       *01289000
                   USER_TYPE_03,                                       *01290000
                   USER_TYPE_04                                        *01291000
                 FROM S02                                              *01292000
                 WHERE                                                 *01293000
                   INST_NBR = :INST AND                                *01294000
                   RECORD_NBR = :RECNBR AND                            *01295000
                   MODEL >                                             *01296000
                     :MODEL                                            *01297000
                 ORDER BY MODEL ASC,                                   *01298000
                   PROD_CODE ASC,                                      *01299000
                   ACCT_TYPE ASC                                       *01300000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01301000
         EXEC  SQL OPEN S02GT003                                        01302000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            01303000
         BNZ   *+14                    NO - RETURN ERROR                01304000
         MVC   SQWCSRCA,=A(CLSGT003)   SET CURSOR CLOSE ROUTINE ADDRESS 01305000
         L     12,SQWCSRRA             LOAD RETURN ADDRESS              01306000
         BR    12                      RETURN TO FETCH ROUTINE          01307000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01308000
         BR    14                      RETURN TO CALLER                 01309000
         LTORG                                                          01310000
*                                                                       01311000
NXTGT004 DS    0H                                                       01312000
         USING NXTGT004,12             ESTABLISH BASE REGISTER          01313000
         B     *+6                     BRANCH AROUND ADCON              01314000
BASGT004 DC    AL2(4096)                                                01315000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01316000
         AH    3,BASGT004              ADD 4K                           01317000
         EXEC  SQL DECLARE S02GT004 CURSOR                             *01318000
               WITH ROWSET POSITIONING                                 *01319000
               FOR SELECT                                              *01320000
                   INST_NBR,                                           *01321000
                   RECORD_NBR,                                         *01322000
                   MODEL,                                              *01323000
                   PROD_CODE,                                          *01324000
                   ACCT_TYPE,                                          *01325000
                   AUDIT_DATE,                                         *01326000
                   AUDIT_TIME,                                         *01327000
                   AUDIT_USER,                                         *01328000
                   AUDIT_ORG,                                          *01329000
                   IS_PRIMARY,                                         *01330000
                   PROD_GROUP,                                         *01331000
                   MIN_ACCT,                                           *01332000
                   MIN_N_QLFY,                                         *01333000
                   MAX_SCND,                                           *01334000
                   MAX_S_QLFY,                                         *01335000
                   MIN_TERM,                                           *01336000
                   MIN_T_QLFY,                                         *01337000
                   MAX_TERM,                                           *01338000
                   MAX_T_QLFY,                                         *01339000
                   BRCH_REG_CODE,                                      *01340000
                   BRCHREG_QLFY,                                       *01341000
                   PRIME_CATG,                                         *01342000
                   PRIME_C_QLFY,                                       *01343000
                   PRIME_C_INCEN,                                      *01344000
                   CMB_BAL_CD_1,                                       *01345000
                   CMB_BAL_CD_1_Q,                                     *01346000
                   CMB_BAL_CD_1_I,                                     *01347000
                   CMB_BAL_CD_2,                                       *01348000
                   CMB_BAL_CD_2_Q,                                     *01349000
                   CMB_BAL_CD_2_I,                                     *01350000
                   BAL_C_CODE_1,                                       *01351000
                   BAL_CATG_Q_1,                                       *01352000
                   BAL_CATG_I_1,                                       *01353000
                   BAL_C_CODE_2,                                       *01354000
                   BAL_CATG_Q_2,                                       *01355000
                   BAL_CATG_I_2,                                       *01356000
                   USER_RTN,                                           *01357000
                   USER_RTN_QLFY,                                      *01358000
                   USER_RTN_INCEN,                                     *01359000
                   PRIMARY_SVC,                                        *01360000
                   SEC_SC_INC_OPT,                                     *01361000
                   DISC_AMT,                                           *01362000
                   DISC_PCT,                                           *01363000
                   PRIMARY_RATE,                                       *01364000
                   SEC_RATE_OPT,                                       *01365000
                   RATE_INCEN,                                         *01366000
                   TIME_OPT,                                           *01367000
                   RATE_BAL_CODE,                                      *01368000
                   CASH_MIN_AMOUNT,                                    *01369000
                   CASH_MAX_AMOUNT,                                    *01370000
                   CASH_BAL_CODE,                                      *01371000
                   MISC_OPTION_1,                                      *01372000
                   MISC_OPTION_2,                                      *01373000
                   MISC_OPTION_3,                                      *01374000
                   SVC_INCEN,                                          *01375000
                   SVC_CHG_BAL_CD,                                     *01376000
                   RATE_PCT,                                           *01377000
                   TRAN_PLAN,                                          *01378000
                   USER_OPT_REL1,                                      *01379000
                   USER_OPT_01,                                        *01380000
                   USER_OPT_Q_1,                                       *01381000
                   USER_OPT_REL2,                                      *01382000
                   USER_OPT_02,                                        *01383000
                   USER_OPT_Q_2,                                       *01384000
                   USER_OPT_REL3,                                      *01385000
                   USER_OPT_03,                                        *01386000
                   USER_OPT_Q_3,                                       *01387000
                   USER_OPT_REL4,                                      *01388000
                   USER_OPT_04,                                        *01389000
                   USER_OPT_Q_4,                                       *01390000
                   CMB_BAL_CD_3,                                       *01391000
                   CMB_BAL_CD_3_Q,                                     *01392000
                   CMB_BAL_CD_3_I,                                     *01393000
                   CMB_BAL_CD_4,                                       *01394000
                   CMB_BAL_CD_4_Q,                                     *01395000
                   CMB_BAL_CD_4_I,                                     *01396000
                   SEC_SVC_OPT,                                        *01397000
                   RATE_OPTION,                                        *01398000
                   SEC_SVC_PLAN,                                       *01399000
                   SEC_SVC_TABLE,                                      *01400000
                   SEC_SVC_BAL_CD,                                     *01401000
                   RATE_TYPE,                                          *01402000
                   USER_TYPE_01,                                       *01403000
                   USER_TYPE_02,                                       *01404000
                   USER_TYPE_03,                                       *01405000
                   USER_TYPE_04                                        *01406000
                 FROM S02                                              *01407000
                 WHERE                                                 *01408000
                   INST_NBR = :INST AND                                *01409000
                   RECORD_NBR >                                        *01410000
                     :RECNBR                                           *01411000
                 ORDER BY RECORD_NBR ASC,                              *01412000
                   MODEL ASC,                                          *01413000
                   PROD_CODE ASC,                                      *01414000
                   ACCT_TYPE ASC                                       *01415000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01416000
         EXEC  SQL OPEN S02GT004                                        01417000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            01418000
         BNZ   *+14                    NO - RETURN ERROR                01419000
         MVC   SQWCSRCA,=A(CLSGT004)   SET CURSOR CLOSE ROUTINE ADDRESS 01420000
         L     12,SQWCSRRA             LOAD RETURN ADDRESS              01421000
         BR    12                      RETURN TO FETCH ROUTINE          01422000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01423000
         BR    14                      RETURN TO CALLER                 01424000
         LTORG                                                          01425000
*                                                                       01426000
NXTGT005 DS    0H                                                       01427000
         USING NXTGT005,12             ESTABLISH BASE REGISTER          01428000
         B     *+6                     BRANCH AROUND ADCON              01429000
BASGT005 DC    AL2(4096)                                                01430000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01431000
         AH    3,BASGT005              ADD 4K                           01432000
         EXEC  SQL DECLARE S02GT005 CURSOR                             *01433000
               WITH ROWSET POSITIONING                                 *01434000
               FOR SELECT                                              *01435000
                   INST_NBR,                                           *01436000
                   RECORD_NBR,                                         *01437000
                   MODEL,                                              *01438000
                   PROD_CODE,                                          *01439000
                   ACCT_TYPE,                                          *01440000
                   AUDIT_DATE,                                         *01441000
                   AUDIT_TIME,                                         *01442000
                   AUDIT_USER,                                         *01443000
                   AUDIT_ORG,                                          *01444000
                   IS_PRIMARY,                                         *01445000
                   PROD_GROUP,                                         *01446000
                   MIN_ACCT,                                           *01447000
                   MIN_N_QLFY,                                         *01448000
                   MAX_SCND,                                           *01449000
                   MAX_S_QLFY,                                         *01450000
                   MIN_TERM,                                           *01451000
                   MIN_T_QLFY,                                         *01452000
                   MAX_TERM,                                           *01453000
                   MAX_T_QLFY,                                         *01454000
                   BRCH_REG_CODE,                                      *01455000
                   BRCHREG_QLFY,                                       *01456000
                   PRIME_CATG,                                         *01457000
                   PRIME_C_QLFY,                                       *01458000
                   PRIME_C_INCEN,                                      *01459000
                   CMB_BAL_CD_1,                                       *01460000
                   CMB_BAL_CD_1_Q,                                     *01461000
                   CMB_BAL_CD_1_I,                                     *01462000
                   CMB_BAL_CD_2,                                       *01463000
                   CMB_BAL_CD_2_Q,                                     *01464000
                   CMB_BAL_CD_2_I,                                     *01465000
                   BAL_C_CODE_1,                                       *01466000
                   BAL_CATG_Q_1,                                       *01467000
                   BAL_CATG_I_1,                                       *01468000
                   BAL_C_CODE_2,                                       *01469000
                   BAL_CATG_Q_2,                                       *01470000
                   BAL_CATG_I_2,                                       *01471000
                   USER_RTN,                                           *01472000
                   USER_RTN_QLFY,                                      *01473000
                   USER_RTN_INCEN,                                     *01474000
                   PRIMARY_SVC,                                        *01475000
                   SEC_SC_INC_OPT,                                     *01476000
                   DISC_AMT,                                           *01477000
                   DISC_PCT,                                           *01478000
                   PRIMARY_RATE,                                       *01479000
                   SEC_RATE_OPT,                                       *01480000
                   RATE_INCEN,                                         *01481000
                   TIME_OPT,                                           *01482000
                   RATE_BAL_CODE,                                      *01483000
                   CASH_MIN_AMOUNT,                                    *01484000
                   CASH_MAX_AMOUNT,                                    *01485000
                   CASH_BAL_CODE,                                      *01486000
                   MISC_OPTION_1,                                      *01487000
                   MISC_OPTION_2,                                      *01488000
                   MISC_OPTION_3,                                      *01489000
                   SVC_INCEN,                                          *01490000
                   SVC_CHG_BAL_CD,                                     *01491000
                   RATE_PCT,                                           *01492000
                   TRAN_PLAN,                                          *01493000
                   USER_OPT_REL1,                                      *01494000
                   USER_OPT_01,                                        *01495000
                   USER_OPT_Q_1,                                       *01496000
                   USER_OPT_REL2,                                      *01497000
                   USER_OPT_02,                                        *01498000
                   USER_OPT_Q_2,                                       *01499000
                   USER_OPT_REL3,                                      *01500000
                   USER_OPT_03,                                        *01501000
                   USER_OPT_Q_3,                                       *01502000
                   USER_OPT_REL4,                                      *01503000
                   USER_OPT_04,                                        *01504000
                   USER_OPT_Q_4,                                       *01505000
                   CMB_BAL_CD_3,                                       *01506000
                   CMB_BAL_CD_3_Q,                                     *01507000
                   CMB_BAL_CD_3_I,                                     *01508000
                   CMB_BAL_CD_4,                                       *01509000
                   CMB_BAL_CD_4_Q,                                     *01510000
                   CMB_BAL_CD_4_I,                                     *01511000
                   SEC_SVC_OPT,                                        *01512000
                   RATE_OPTION,                                        *01513000
                   SEC_SVC_PLAN,                                       *01514000
                   SEC_SVC_TABLE,                                      *01515000
                   SEC_SVC_BAL_CD,                                     *01516000
                   RATE_TYPE,                                          *01517000
                   USER_TYPE_01,                                       *01518000
                   USER_TYPE_02,                                       *01519000
                   USER_TYPE_03,                                       *01520000
                   USER_TYPE_04                                        *01521000
                 FROM S02                                              *01522000
                 WHERE                                                 *01523000
                   INST_NBR >                                          *01524000
                     :INST                                             *01525000
                 ORDER BY INST_NBR ASC,                                *01526000
                   RECORD_NBR ASC,                                     *01527000
                   MODEL ASC,                                          *01528000
                   PROD_CODE ASC,                                      *01529000
                   ACCT_TYPE ASC                                       *01530000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01531000
         EXEC  SQL OPEN S02GT005                                        01532000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            01533000
         BNZ   *+14                    NO - RETURN ERROR                01534000
         MVC   SQWCSRCA,=A(CLSGT005)   SET CURSOR CLOSE ROUTINE ADDRESS 01535000
         L     12,SQWCSRRA             LOAD RETURN ADDRESS              01536000
         BR    12                      RETURN TO FETCH ROUTINE          01537000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01538000
         BR    14                      RETURN TO CALLER                 01539000
         LTORG                                                          01540000
*                                                                       01541000
**********************************************************************  01542000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:               01543000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01544000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01545000
*     RETRIEVE THE ACTUAL ROW.                                          01546000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01547000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01548000
**********************************************************************  01549000
*                                                                       01550000
FETDC0   DS    0H                                                       01551000
         USING FETDC0,12               ESTABLISH BASE REGISTER          01552000
         MVC   SQWCSRRA,=A(FETDC0)     SET RETURN ROUTINE ADDRESS       01553000
         MVC   SQWCSRBR,=A(NXTXC0)     SET CURSOR ROUTINE ADDRESS       01554000
         MVC   SQWRSFVC,=AL2(TBLCOLMS) FETCH ALL COLUMNS IN TABLE       01555000
         LH    1,SQWCSRSP              LOAD CURRENT CURSOR POINTER      01556000
         LA    12,FETDC0P(1)           LOAD POINTER TO FETCH ROUTINE    01557000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01558000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01559000
FETDC0P  DC    A(FETGE001)                                              01560000
         DC    A(FETGT002)                                              01561000
         DC    A(FETGT003)                                              01562000
         DC    A(FETGT004)                                              01563000
         DC    A(FETGT005)                                              01564000
         DC    A(FETGE002)                                              01565000
         DC    A(FETGT003)                                              01566000
         DC    A(FETGT004)                                              01567000
         DC    A(FETGT005)                                              01568000
         DC    A(FETGE003)                                              01569000
         DC    A(FETGT004)                                              01570000
         DC    A(FETGT005)                                              01571000
         DC    A(FETGE004)                                              01572000
         DC    A(FETGT005)                                              01573000
         DC    A(FETGE005)                                              01574000
         DC    A(0)                                                     01575000
         LTORG                                                          01576000
*                                                                       01577000
**********************************************************************  01578000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01579000
* THE PRIMARY KEY:                                                      01580000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01581000
*     VERBS.                                                            01582000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01583000
*     RETRIEVE THE ACTUAL ROW.                                          01584000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01585000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01586000
*     WILL BE THRU THE UPDATE CURSOR.                                   01587000
**********************************************************************  01588000
*                                                                       01589000
FETKC0   DS    0H                                                       01590000
         USING FETKC0,12               ESTABLISH BASE REGISTER          01591000
         MVC   SQWCSRRA,=A(FETKC0)     SET RETURN ROUTINE ADDRESS       01592000
         MVC   SQWCSRBR,=A(NXTXC0)     SET CURSOR ROUTINE ADDRESS       01593000
         MVC   SQWRSFVC,=AL2(KY0COLMS) ONLY FETCH KEY COLUMNS           01594000
         LH    1,SQWCSRSP              LOAD CURRENT CURSOR POINTER      01595000
         LA    12,FETKC0P(1)           LOAD POINTER TO FETCH ROUTINE    01596000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01597000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01598000
FETKC0P  DC    A(FETGE001)                                              01599000
         DC    A(FETGT002)                                              01600000
         DC    A(FETGT003)                                              01601000
         DC    A(FETGT004)                                              01602000
         DC    A(FETGT005)                                              01603000
         DC    A(FETGE002)                                              01604000
         DC    A(FETGT003)                                              01605000
         DC    A(FETGT004)                                              01606000
         DC    A(FETGT005)                                              01607000
         DC    A(FETGE003)                                              01608000
         DC    A(FETGT004)                                              01609000
         DC    A(FETGT005)                                              01610000
         DC    A(FETGE004)                                              01611000
         DC    A(FETGT005)                                              01612000
         DC    A(FETGE005)                                              01613000
         DC    A(0)                                                     01614000
         LTORG                                                          01615000
*                                                                       01616000
FETGE001 DS    0H                                                       01617000
         USING FETGE001,12             ESTABLISH BASE REGISTER          01618000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01619000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01620000
         BALR  14,15                   MOVE REQUESTED DATA              01621000
         EXEC  SQL FETCH NEXT ROWSET FROM S02GE001                     *01622000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01623000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01624000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01625000
         BALR  14,15                   MOVE REQUESTED DATA              01626000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01627000
         BR    14                      RETURN TO CALLER                 01628000
         LTORG                                                          01629000
*                                                                       01630000
FETGE002 DS    0H                                                       01631000
         USING FETGE002,12             ESTABLISH BASE REGISTER          01632000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01633000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01634000
         BALR  14,15                   MOVE REQUESTED DATA              01635000
         EXEC  SQL FETCH NEXT ROWSET FROM S02GE002                     *01636000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01637000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01638000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01639000
         BALR  14,15                   MOVE REQUESTED DATA              01640000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01641000
         BR    14                      RETURN TO CALLER                 01642000
         LTORG                                                          01643000
*                                                                       01644000
FETGE003 DS    0H                                                       01645000
         USING FETGE003,12             ESTABLISH BASE REGISTER          01646000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01647000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01648000
         BALR  14,15                   MOVE REQUESTED DATA              01649000
         EXEC  SQL FETCH NEXT ROWSET FROM S02GE003                     *01650000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01651000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01652000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01653000
         BALR  14,15                   MOVE REQUESTED DATA              01654000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01655000
         BR    14                      RETURN TO CALLER                 01656000
         LTORG                                                          01657000
*                                                                       01658000
FETGE004 DS    0H                                                       01659000
         USING FETGE004,12             ESTABLISH BASE REGISTER          01660000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01661000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01662000
         BALR  14,15                   MOVE REQUESTED DATA              01663000
         EXEC  SQL FETCH NEXT ROWSET FROM S02GE004                     *01664000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01665000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01666000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01667000
         BALR  14,15                   MOVE REQUESTED DATA              01668000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01669000
         BR    14                      RETURN TO CALLER                 01670000
         LTORG                                                          01671000
*                                                                       01672000
FETGE005 DS    0H                                                       01673000
         USING FETGE005,12             ESTABLISH BASE REGISTER          01674000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01675000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01676000
         BALR  14,15                   MOVE REQUESTED DATA              01677000
         EXEC  SQL FETCH NEXT ROWSET FROM S02GE005                     *01678000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01679000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01680000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01681000
         BALR  14,15                   MOVE REQUESTED DATA              01682000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01683000
         BR    14                      RETURN TO CALLER                 01684000
         LTORG                                                          01685000
*                                                                       01686000
FETGT002 DS    0H                                                       01687000
         USING FETGT002,12             ESTABLISH BASE REGISTER          01688000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01689000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01690000
         BALR  14,15                   MOVE REQUESTED DATA              01691000
         EXEC  SQL FETCH NEXT ROWSET FROM S02GT002                     *01692000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01693000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01694000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01695000
         BALR  14,15                   MOVE REQUESTED DATA              01696000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01697000
         BR    14                      RETURN TO CALLER                 01698000
         LTORG                                                          01699000
*                                                                       01700000
FETGT003 DS    0H                                                       01701000
         USING FETGT003,12             ESTABLISH BASE REGISTER          01702000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01703000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01704000
         BALR  14,15                   MOVE REQUESTED DATA              01705000
         EXEC  SQL FETCH NEXT ROWSET FROM S02GT003                     *01706000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01707000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01708000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01709000
         BALR  14,15                   MOVE REQUESTED DATA              01710000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01711000
         BR    14                      RETURN TO CALLER                 01712000
         LTORG                                                          01713000
*                                                                       01714000
FETGT004 DS    0H                                                       01715000
         USING FETGT004,12             ESTABLISH BASE REGISTER          01716000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01717000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01718000
         BALR  14,15                   MOVE REQUESTED DATA              01719000
         EXEC  SQL FETCH NEXT ROWSET FROM S02GT004                     *01720000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01721000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01722000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01723000
         BALR  14,15                   MOVE REQUESTED DATA              01724000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01725000
         BR    14                      RETURN TO CALLER                 01726000
         LTORG                                                          01727000
*                                                                       01728000
FETGT005 DS    0H                                                       01729000
         USING FETGT005,12             ESTABLISH BASE REGISTER          01730000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    01731000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01732000
         BALR  14,15                   MOVE REQUESTED DATA              01733000
         EXEC  SQL FETCH NEXT ROWSET FROM S02GT005                     *01734000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             01735000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   01736000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   01737000
         BALR  14,15                   MOVE REQUESTED DATA              01738000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01739000
         BR    14                      RETURN TO CALLER                 01740000
         LTORG                                                          01741000
*                                                                       01742000
**********************************************************************  01743000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:                    01744000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01745000
*     AND GET-NEXT-LOCK VERBS.                                          01746000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01747000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01748000
**********************************************************************  01749000
*                                                                       01750000
CLSXC0   DS    0H                                                       01751000
         USING CLSXC0,12               ESTABLISH BASE REGISTER          01752000
         L     12,SQWCSRCA             SET CURSOR CLOSE ROUTINE ADDRESS 01753000
         XC    SQWCSRCA,SQWCSRCA       CLEAR CURSOR CLOSE ROUTINE ADDR  01754000
         BR    12                      GO TO CURSOR CLOSE ROUTINE       01755000
         LTORG                                                          01756000
*                                                                       01757000
CLSGE001 DS    0H                                                       01758000
         USING CLSGE001,12             ESTABLISH BASE REGISTER          01759000
         EXEC  SQL CLOSE S02GE001                                       01760000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01761000
         BR    14                      RETURN TO CALLER                 01762000
         LTORG                                                          01763000
*                                                                       01764000
CLSGE002 DS    0H                                                       01765000
         USING CLSGE002,12             ESTABLISH BASE REGISTER          01766000
         EXEC  SQL CLOSE S02GE002                                       01767000
         L     14,SQW@RET                                               01768000
         BR    14                                                       01769000
         LTORG                                                          01770000
*                                                                       01771000
CLSGE003 DS    0H                                                       01772000
         USING CLSGE003,12             ESTABLISH BASE REGISTER          01773000
         EXEC  SQL CLOSE S02GE003                                       01774000
         L     14,SQW@RET                                               01775000
         BR    14                                                       01776000
         LTORG                                                          01777000
*                                                                       01778000
CLSGE004 DS    0H                                                       01779000
         USING CLSGE004,12             ESTABLISH BASE REGISTER          01780000
         EXEC  SQL CLOSE S02GE004                                       01781000
         L     14,SQW@RET                                               01782000
         BR    14                                                       01783000
         LTORG                                                          01784000
*                                                                       01785000
CLSGE005 DS    0H                                                       01786000
         USING CLSGE005,12             ESTABLISH BASE REGISTER          01787000
         EXEC  SQL CLOSE S02GE005                                       01788000
         L     14,SQW@RET                                               01789000
         BR    14                                                       01790000
         LTORG                                                          01791000
*                                                                       01792000
CLSGT002 DS    0H                                                       01793000
         USING CLSGT002,12             ESTABLISH BASE REGISTER          01794000
         EXEC  SQL CLOSE S02GT002                                       01795000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01796000
         BR    14                      RETURN TO CALLER                 01797000
         LTORG                                                          01798000
*                                                                       01799000
CLSGT003 DS    0H                                                       01800000
         USING CLSGT003,12             ESTABLISH BASE REGISTER          01801000
         EXEC  SQL CLOSE S02GT003                                       01802000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01803000
         BR    14                      RETURN TO CALLER                 01804000
         LTORG                                                          01805000
*                                                                       01806000
CLSGT004 DS    0H                                                       01807000
         USING CLSGT004,12             ESTABLISH BASE REGISTER          01808000
         EXEC  SQL CLOSE S02GT004                                       01809000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01810000
         BR    14                      RETURN TO CALLER                 01811000
         LTORG                                                          01812000
*                                                                       01813000
CLSGT005 DS    0H                                                       01814000
         USING CLSGT005,12             ESTABLISH BASE REGISTER          01815000
         EXEC  SQL CLOSE S02GT005                                       01816000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01817000
         BR    14                      RETURN TO CALLER                 01818000
         LTORG                                                          01819000
*                                                                       01820000
**********************************************************************  01821000
* ALTERNATE KEY 1 NOT DEFINED                                           01822000
**********************************************************************  01823000
*                                                                       01824000
SELIN1   DS    0H                                                       01825000
SELKY1   DS    0H                                                       01826000
SELXC1   DS    0H                                                       01827000
FETDC1   DS    0H                                                       01828000
FETKC1   DS    0H                                                       01829000
CLSXC1   DS    0H                                                       01830000
         DC    X'00DEAD01'             FORCE S0C1 ABEND                 01831000
*                                                                       01832000
**********************************************************************  01833000
* ALTERNATE KEY 2 NOT DEFINED                                           01834000
**********************************************************************  01835000
*                                                                       01836000
SELIN2   DS    0H                                                       01837000
SELKY2   DS    0H                                                       01838000
SELXC2   DS    0H                                                       01839000
FETDC2   DS    0H                                                       01840000
FETKC2   DS    0H                                                       01841000
CLSXC2   DS    0H                                                       01842000
         DC    X'00DEAD02'             FORCE S0C1 ABEND                 01843000
*                                                                       01844000
**********************************************************************  01845000
* ALTERNATE KEY 3 NOT DEFINED                                           01846000
**********************************************************************  01847000
*                                                                       01848000
SELIN3   DS    0H                                                       01849000
SELKY3   DS    0H                                                       01850000
SELXC3   DS    0H                                                       01851000
FETDC3   DS    0H                                                       01852000
FETKC3   DS    0H                                                       01853000
CLSXC3   DS    0H                                                       01854000
         DC    X'00DEAD03'             FORCE S0C1 ABEND                 01855000
*                                                                       01856000
         DS    0H                      END OF SQL STATEMENTS            01857000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   01858000
*                                                                       01859000
**********************************************************************  01860000
* DUMMY ENTRY POINT DSNHLI                                              01861000
**********************************************************************  01862000
*                                                                       01863000
         ENTRY DSNHLI                                                   01864000
DSNHLI   DS    0H                                                       01865000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       01866000
         BR    15                      BRANCH TO ATTACH FACILITY        01867000
*                                                                       01868000
**********************************************************************  01869000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  01870000
**********************************************************************  01871000
*                                                                       01872000
* CONVTAB1 TABLE ENTRY FORMAT IS:                                       01873000
*        DC    H'RRRR',H'VVVV',H'LLLL',X'KK',X'DD'                      01874000
* OR:                                                                   01875000
*        DC    H'RRRR',H'VVVV',X'ZZPP',X'KK',X'DD'                      01876000
* WHERE:                                                                01877000
*   RRRR = RECORD AREA OFFSET                                           01878000
*   VVVV = HOST VARIABLE AREA OFFSET                                    01879000
*   LLLL = HALFWORD LENGTH TO MOVE                                      01880000
*   ZZPP = CONVERT ZONED/PACKED LENGTHS (MINUS 1)                       01881000
*   KK   = KEY FIELD MASK:                                              01882000
*            80 = KEY 0 FIELD                                           01883000
*            40 = KEY 1 FIELD                                           01884000
*            20 = KEY 2 FIELD                                           01885000
*            10 = KEY 3 FIELD                                           01886000
*   DD   = DATA FIELD MASK:                                             01887000
*            80 = RECORD FIELD IS PACKED                                01888000
*            40 = HOST VARIABLE IS PACKED                               01889000
*            20 = NULLABLE FIELD                                        01890000
*            01 = DATE FIELD                                            01891000
*            02 = TIME FIELD                                            01892000
*                                                                       01893000
CONVTAB1 DS    0H                      RECORD/HOST VARIABLE CONVERSIONS 01894000
         DC    H'0000',H'0000',H'0027',X'80',X'00'                      01895000
         DC    H'0027',H'0027',H'0181',X'00',X'00'                      01896000
         DC    8X'FF'                                                   01897000
*                                                                       01898000
* CONVTAB2 TABLE COLUMNS ENTRY FORMAT IS:                               01899000
*        DC    H'RRRR',H'LLLL',H'TTTT'                                  01900000
* OR:                                                                   01901000
*        DC    H'RRRR',X'PPSS',H'TTTT'                                  01902000
* WHERE:                                                                01903000
*   RRRR = RECORD AREA OFFSET                                           01904000
*   LLLL = HALFWORD LENGTH OF HOST VARIABLE (NON-DECIMAL DATA TYPE)     01905000
*   PPSS = PRECISION AND SCALE (DECIMAL DATA TYPE)                      01906000
*   TTTT = SQLDA DATA TYPE:                                             01907000
*            452 = CHAR       453 = CHAR NULLABLE                       01908000
*            484 = DECIMAL    485 = DECIMAL NULLABLE                    01909000
*            496 = INTEGER    497 = INTEGER NULLABLE                    01910000
*            500 = SMALLINT   501 = SMALLINT NULLABLE                   01911000
*                                                                       01912000
CONVTAB2 DS    0F                      RECORD DATA ATTRIBUTE TABLE      01913000
         DC    AL4(TBLCOLMS*44+16)     TOTAL SQLDA SIZE                 01914000
         DC    AL2(TBLCOLMS)           NUMBER OF COLUMNS IN TABLE       01915000
         DC    AL2(KY0COLMS)           NUMBER OF PRIMARY KEY COLUMNS    01916000
         DC    AL2(KY1COLMS)           NUMBER OF ALT1 KEY COLUMNS       01917000
         DC    AL2(KY2COLMS)           NUMBER OF ALT2 KEY COLUMNS       01918000
         DC    AL2(KY3COLMS)           NUMBER OF ALT3 KEY COLUMNS       01919000
         DC    AL2(00208)              SUMMATION OF ALL COLUMN LENGTHS  01920000
         DC    AL2(01000)              PRIMARY MULTIROW FETCH ARRAY     01921000
         DC    AL2(00000)              ALT 1 MULTIROW FETCH ARRAY       01922000
         DC    AL2(00000)              ALT 2 MULTIROW FETCH ARRAY       01923000
         DC    AL2(00000)              ALT 3 MULTIROW FETCH ARRAY       01924000
         DC    AL2(00000)              PRIMARY KEY LOW VALUE LEVEL      01925000
         DC    AL2(00000)              ALT 1 KEY LOW VALUE LEVEL        01926000
         DC    AL2(00000)              ALT 2 KEY LOW VALUE LEVEL        01927000
         DC    AL2(00000)              ALT 3 KEY LOW VALUE LEVEL        01928000
CONVDATA DS    0H                                                       01929000
         DC    H'0000',H'0004',H'452'  INST_NBR                         01930000
         DC    H'0004',H'0004',H'452'  RECORD_NBR                       01931000
         DC    H'0008',H'0010',H'452'  MODEL                            01932000
         DC    H'0018',H'0006',H'452'  PROD_CODE                        01933000
         DC    H'0024',H'0003',H'452'  ACCT_TYPE                        01934000
KEYCOLMS EQU   (*-CONVDATA)/6          NUMBER OF KEY COLUMNS IN TABLE   01935000
         DC    H'0027',X'0900',H'484'  AUDIT_DATE                       01936000
         DC    H'0032',X'0900',H'484'  AUDIT_TIME                       01937000
         DC    H'0037',H'0008',H'452'  AUDIT_USER                       01938000
         DC    H'0045',H'0006',H'452'  AUDIT_ORG                        01939000
         DC    H'0051',H'0001',H'452'  IS_PRIMARY                       01940000
         DC    H'0052',H'0001',H'452'  PROD_GROUP                       01941000
         DC    H'0053',H'0002',H'452'  MIN_ACCT                         01942000
         DC    H'0055',H'0001',H'452'  MIN_N_QLFY                       01943000
         DC    H'0056',H'0002',H'452'  MAX_SCND                         01944000
         DC    H'0058',H'0001',H'452'  MAX_S_QLFY                       01945000
         DC    H'0059',H'0005',H'452'  MIN_TERM                         01946000
         DC    H'0064',H'0001',H'452'  MIN_T_QLFY                       01947000
         DC    H'0065',H'0005',H'452'  MAX_TERM                         01948000
         DC    H'0070',H'0001',H'452'  MAX_T_QLFY                       01949000
         DC    H'0071',H'0006',H'452'  BRCH_REG_CODE                    01950000
         DC    H'0077',H'0001',H'452'  BRCHREG_QLFY                     01951000
         DC    H'0078',H'0006',H'452'  PRIME_CATG                       01952000
         DC    H'0084',H'0001',H'452'  PRIME_C_QLFY                     01953000
         DC    H'0085',H'0001',H'452'  PRIME_C_INCEN                    01954000
         DC    H'0086',H'0002',H'452'  CMB_BAL_CD_1                     01955000
         DC    H'0088',H'0001',H'452'  CMB_BAL_CD_1_Q                   01956000
         DC    H'0089',H'0001',H'452'  CMB_BAL_CD_1_I                   01957000
         DC    H'0090',H'0002',H'452'  CMB_BAL_CD_2                     01958000
         DC    H'0092',H'0001',H'452'  CMB_BAL_CD_2_Q                   01959000
         DC    H'0093',H'0001',H'452'  CMB_BAL_CD_2_I                   01960000
         DC    H'0094',H'0006',H'452'  BAL_C_CODE_1                     01961000
         DC    H'0100',H'0001',H'452'  BAL_CATG_Q_1                     01962000
         DC    H'0101',H'0001',H'452'  BAL_CATG_I_1                     01963000
         DC    H'0102',H'0006',H'452'  BAL_C_CODE_2                     01964000
         DC    H'0108',H'0001',H'452'  BAL_CATG_Q_2                     01965000
         DC    H'0109',H'0001',H'452'  BAL_CATG_I_2                     01966000
         DC    H'0110',H'0001',H'452'  USER_RTN                         01967000
         DC    H'0111',H'0001',H'452'  USER_RTN_QLFY                    01968000
         DC    H'0112',H'0001',H'452'  USER_RTN_INCEN                   01969000
         DC    H'0113',H'0001',H'452'  PRIMARY_SVC                      01970000
         DC    H'0114',H'0001',H'452'  SEC_SC_INC_OPT                   01971000
         DC    H'0115',X'0502',H'484'  DISC_AMT                         01972000
         DC    H'0118',X'0505',H'484'  DISC_PCT                         01973000
         DC    H'0121',H'0001',H'452'  PRIMARY_RATE                     01974000
         DC    H'0122',H'0001',H'452'  SEC_RATE_OPT                     01975000
         DC    H'0123',H'0006',H'452'  RATE_INCEN                       01976000
         DC    H'0129',H'0001',H'452'  TIME_OPT                         01977000
         DC    H'0130',H'0002',H'452'  RATE_BAL_CODE                    01978000
         DC    H'0132',X'0900',H'484'  CASH_MIN_AMOUNT                  01979000
         DC    H'0137',X'0900',H'484'  CASH_MAX_AMOUNT                  01980000
         DC    H'0142',H'0002',H'452'  CASH_BAL_CODE                    01981000
         DC    H'0144',H'0001',H'452'  MISC_OPTION_1                    01982000
         DC    H'0145',H'0001',H'452'  MISC_OPTION_2                    01983000
         DC    H'0146',H'0001',H'452'  MISC_OPTION_3                    01984000
         DC    H'0147',H'0006',H'452'  SVC_INCEN                        01985000
         DC    H'0153',H'0002',H'452'  SVC_CHG_BAL_CD                   01986000
         DC    H'0155',X'0909',H'484'  RATE_PCT                         01987000
         DC    H'0160',H'0003',H'452'  TRAN_PLAN                        01988000
         DC    H'0163',H'0002',H'452'  USER_OPT_REL1                    01989000
         DC    H'0165',H'0001',H'452'  USER_OPT_01                      01990000
         DC    H'0166',H'0001',H'452'  USER_OPT_Q_1                     01991000
         DC    H'0167',H'0002',H'452'  USER_OPT_REL2                    01992000
         DC    H'0169',H'0001',H'452'  USER_OPT_02                      01993000
         DC    H'0170',H'0001',H'452'  USER_OPT_Q_2                     01994000
         DC    H'0171',H'0002',H'452'  USER_OPT_REL3                    01995000
         DC    H'0173',H'0001',H'452'  USER_OPT_03                      01996000
         DC    H'0174',H'0001',H'452'  USER_OPT_Q_3                     01997000
         DC    H'0175',H'0002',H'452'  USER_OPT_REL4                    01998000
         DC    H'0177',H'0001',H'452'  USER_OPT_04                      01999000
         DC    H'0178',H'0001',H'452'  USER_OPT_Q_4                     02000000
         DC    H'0179',H'0002',H'452'  CMB_BAL_CD_3                     02001000
         DC    H'0181',H'0001',H'452'  CMB_BAL_CD_3_Q                   02002000
         DC    H'0182',H'0001',H'452'  CMB_BAL_CD_3_I                   02003000
         DC    H'0183',H'0002',H'452'  CMB_BAL_CD_4                     02004000
         DC    H'0185',H'0001',H'452'  CMB_BAL_CD_4_Q                   02005000
         DC    H'0186',H'0001',H'452'  CMB_BAL_CD_4_I                   02006000
         DC    H'0187',H'0001',H'452'  SEC_SVC_OPT                      02007000
         DC    H'0188',H'0001',H'452'  RATE_OPTION                      02008000
         DC    H'0189',H'0006',H'452'  SEC_SVC_PLAN                     02009000
         DC    H'0195',H'0006',H'452'  SEC_SVC_TABLE                    02010000
         DC    H'0201',H'0002',H'452'  SEC_SVC_BAL_CD                   02011000
         DC    H'0203',H'0001',H'452'  RATE_TYPE                        02012000
         DC    H'0204',H'0001',H'452'  USER_TYPE_01                     02013000
         DC    H'0205',H'0001',H'452'  USER_TYPE_02                     02014000
         DC    H'0206',H'0001',H'452'  USER_TYPE_03                     02015000
         DC    H'0207',H'0001',H'452'  USER_TYPE_04                     02016000
TBLCOLMS EQU   (*-CONVDATA)/6          NUMBER OF COLUMNS IN TABLE       02017000
KY0COLMS EQU   00005                   NUMBER OF PRIMARY KEY COLUMNS    02018000
KY1COLMS EQU   00000                   NUMBER OF ALT 1 KEY COLUMNS      02019000
KY2COLMS EQU   00000                   NUMBER OF ALT 2 KEY COLUMNS      02020000
KY3COLMS EQU   00000                   NUMBER OF ALT 3 KEY COLUMNS      02021000
*                                                                       02022000
         LTORG                                                          02023000
         END                                                            02024000
