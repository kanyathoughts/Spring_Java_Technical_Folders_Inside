**********************************************************************  00001000
*                                                                       00002000
*  S02UDB .... STATIC SQL UPDATE MODULE                                 00003000
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
* PROGRAM TABLE HEADER SECTION:                                         00150000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00151000
**********************************************************************  00152000
*                                                                       00153000
S02UDB   CSECT                         PROGRAM TABLE SECTION            00154000
S02UDB   AMODE ANY                                                      00155000
S02UDB   RMODE ANY                                                      00156000
         DC    CL8'S02UDB  '           PROGRAM ID                       00157000
         DC    CL1' '                                                   00158000
         DC    CL8'&SYSDATE'           ASSEMBLY DATE                    00159000
         DC    CL1' '                                                   00160000
         DC    CL5'&SYSTIME'           ASSEMBLY TIME                    00161000
         DC    CL1' '                                                   00162000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00163000
         DC    5A(0)                   RESERVED                         00164000
         DC    AL2(0)                  RESERVED                         00165000
         DC    AL2(INDVARL)            NULL INDICATOR AREA LENGTH       00166000
         DC    A(CONVTAB1)             RECORD/HOST CONVERSION TABLE     00167000
         DC    A(0)                    SQLDA DATA TYPE/LENGTH TABLE     00168000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00169000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00170000
         DC    CL29'WWW.INFOR.COM                '                      00170001
*                                                                       00171000
**********************************************************************  00172000
* STATEMENT TABLE SECTION:                                              00173000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00174000
**********************************************************************  00175000
*                                                                       00176000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00177000
STM#TAB  AMODE ANY                                                      00178000
STM#TAB  RMODE ANY                                                      00179000
         DC    4A(0)                   RDB MODULE VECTORS               00180000
         DC    A(SELUC0)               SELECT UPDATE CURSOR (KEY 0)     00181000
         DC    A(SELUC1)               SELECT UPDATE CURSOR (KEY 1)     00182000
         DC    A(SELUC2)               SELECT UPDATE CURSOR (KEY 2)     00183000
         DC    A(SELUC3)               SELECT UPDATE CURSOR (KEY 3)     00184000
         DC    A(FETUC0)               FETCH UPDATE CURSOR (KEY 0)      00185000
         DC    A(FETUC1)               FETCH UPDATE CURSOR (KEY 1)      00186000
         DC    A(FETUC2)               FETCH UPDATE CURSOR (KEY 2)      00187000
         DC    A(FETUC3)               FETCH UPDATE CURSOR (KEY 3)      00188000
         DC    A(CLSUC0)               CLOSE UPDATE CURSOR (KEY 0)      00189000
         DC    A(CLSUC1)               CLOSE UPDATE CURSOR (KEY 1)      00190000
         DC    A(CLSUC2)               CLOSE UPDATE CURSOR (KEY 2)      00191000
         DC    A(CLSUC3)               CLOSE UPDATE CURSOR (KEY 3)      00192000
         DC    20A(0)                  RDB MODULE VECTORS               00193000
         DC    A(INSROW)               INSERT STATEMENT                 00194000
         DC    A(UPDUC0)               UPDATE STATEMENT (KEY 0)         00195000
         DC    A(UPDUC1)               UPDATE STATEMENT (KEY 1)         00196000
         DC    A(UPDUC2)               UPDATE STATEMENT (KEY 2)         00197000
         DC    A(UPDUC3)               UPDATE STATEMENT (KEY 3)         00198000
         DC    A(DELUC0)               DELETE STATEMENT (KEY 0)         00199000
         DC    A(DELUC1)               DELETE STATEMENT (KEY 1)         00200000
         DC    A(DELUC2)               DELETE STATEMENT (KEY 2)         00201000
         DC    A(DELUC3)               DELETE STATEMENT (KEY 3)         00202000
         DC    A(DELTBL)               DELETE ALL STATEMENT             00203000
         DC    4X'FF'                                                   00204000
*                                                                       00205000
**********************************************************************  00206000
* SQL STATEMENT SECTION:                                                00207000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00208000
*     TO SUPPORT THIS TABLE.                                            00209000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00210000
*     ARE EQUIVALENT.                                                   00211000
**********************************************************************  00212000
*                                                                       00213000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00214000
SQL#STMT AMODE ANY                                                      00215000
SQL#STMT RMODE ANY                                                      00216000
         USING SQLDSECT,10,3           ADDRESS SQLDSECT                 00217000
         USING COM#AREA,11             ADDRESS COMMAREA                 00218000
*                                                                       00219000
**********************************************************************  00220000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY PRIMARY KEY:       00221000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00222000
*   THEY ARE ALSO USED AFTER A SUCCESSFUL SELECT SEQUENTIAL STATEMENT   00223000
*     FOR THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS.                      00224000
**********************************************************************  00225000
*                                                                       00226000
SELUC0   DS    0H                                                       00227000
         USING SELUC0,12               ESTABLISH BASE REGISTER          00228000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00229000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00230000
         BALR  14,15                   MOVE REQUESTED DATA              00231000
         B     *+6                     BRANCH AROUND ADCON              00232000
BASSEL0  DC    AL2(4096)                                                00233000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00234000
         AH    3,BASSEL0               ADD 4K                           00235000
         EXEC  SQL DECLARE S02UPD0 CURSOR                              *00236000
               FOR SELECT                                              *00237000
                   AUDIT_DATE,                                         *00238000
                   AUDIT_TIME,                                         *00239000
                   AUDIT_USER,                                         *00240000
                   AUDIT_ORG,                                          *00241000
                   IS_PRIMARY,                                         *00242000
                   PROD_GROUP,                                         *00243000
                   MIN_ACCT,                                           *00244000
                   MIN_N_QLFY,                                         *00245000
                   MAX_SCND,                                           *00246000
                   MAX_S_QLFY,                                         *00247000
                   MIN_TERM,                                           *00248000
                   MIN_T_QLFY,                                         *00249000
                   MAX_TERM,                                           *00250000
                   MAX_T_QLFY,                                         *00251000
                   BRCH_REG_CODE,                                      *00252000
                   BRCHREG_QLFY,                                       *00253000
                   PRIME_CATG,                                         *00254000
                   PRIME_C_QLFY,                                       *00255000
                   PRIME_C_INCEN,                                      *00256000
                   CMB_BAL_CD_1,                                       *00257000
                   CMB_BAL_CD_1_Q,                                     *00258000
                   CMB_BAL_CD_1_I,                                     *00259000
                   CMB_BAL_CD_2,                                       *00260000
                   CMB_BAL_CD_2_Q,                                     *00261000
                   CMB_BAL_CD_2_I,                                     *00262000
                   BAL_C_CODE_1,                                       *00263000
                   BAL_CATG_Q_1,                                       *00264000
                   BAL_CATG_I_1,                                       *00265000
                   BAL_C_CODE_2,                                       *00266000
                   BAL_CATG_Q_2,                                       *00267000
                   BAL_CATG_I_2,                                       *00268000
                   USER_RTN,                                           *00269000
                   USER_RTN_QLFY,                                      *00270000
                   USER_RTN_INCEN,                                     *00271000
                   PRIMARY_SVC,                                        *00272000
                   SEC_SC_INC_OPT,                                     *00273000
                   DISC_AMT,                                           *00274000
                   DISC_PCT,                                           *00275000
                   PRIMARY_RATE,                                       *00276000
                   SEC_RATE_OPT,                                       *00277000
                   RATE_INCEN,                                         *00278000
                   TIME_OPT,                                           *00279000
                   RATE_BAL_CODE,                                      *00280000
                   CASH_MIN_AMOUNT,                                    *00281000
                   CASH_MAX_AMOUNT,                                    *00282000
                   CASH_BAL_CODE,                                      *00283000
                   MISC_OPTION_1,                                      *00284000
                   MISC_OPTION_2,                                      *00285000
                   MISC_OPTION_3,                                      *00286000
                   SVC_INCEN,                                          *00287000
                   SVC_CHG_BAL_CD,                                     *00288000
                   RATE_PCT,                                           *00289000
                   TRAN_PLAN,                                          *00290000
                   USER_OPT_REL1,                                      *00291000
                   USER_OPT_01,                                        *00292000
                   USER_OPT_Q_1,                                       *00293000
                   USER_OPT_REL2,                                      *00294000
                   USER_OPT_02,                                        *00295000
                   USER_OPT_Q_2,                                       *00296000
                   USER_OPT_REL3,                                      *00297000
                   USER_OPT_03,                                        *00298000
                   USER_OPT_Q_3,                                       *00299000
                   USER_OPT_REL4,                                      *00300000
                   USER_OPT_04,                                        *00301000
                   USER_OPT_Q_4,                                       *00302000
                   CMB_BAL_CD_3,                                       *00303000
                   CMB_BAL_CD_3_Q,                                     *00304000
                   CMB_BAL_CD_3_I,                                     *00305000
                   CMB_BAL_CD_4,                                       *00306000
                   CMB_BAL_CD_4_Q,                                     *00307000
                   CMB_BAL_CD_4_I,                                     *00308000
                   SEC_SVC_OPT,                                        *00309000
                   RATE_OPTION,                                        *00310000
                   SEC_SVC_PLAN,                                       *00311000
                   SEC_SVC_TABLE,                                      *00312000
                   SEC_SVC_BAL_CD,                                     *00313000
                   RATE_TYPE,                                          *00314000
                   USER_TYPE_01,                                       *00315000
                   USER_TYPE_02,                                       *00316000
                   USER_TYPE_03,                                       *00317000
                   USER_TYPE_04                                        *00318000
                 FROM S02                                              *00319000
                 WHERE                                                 *00320000
                   INST_NBR = :INST AND                                *00321000
                   RECORD_NBR = :RECNBR AND                            *00322000
                   MODEL = :MODEL AND                                  *00323000
                   PROD_CODE = :PRODCODE AND                           *00324000
                   ACCT_TYPE = :ACCTTYPE                               *00325000
                 FOR UPDATE OF                                         *00326000
                   AUDIT_DATE,                                         *00327000
                   AUDIT_TIME,                                         *00328000
                   AUDIT_USER,                                         *00329000
                   AUDIT_ORG,                                          *00330000
                   IS_PRIMARY,                                         *00331000
                   PROD_GROUP,                                         *00332000
                   MIN_ACCT,                                           *00333000
                   MIN_N_QLFY,                                         *00334000
                   MAX_SCND,                                           *00335000
                   MAX_S_QLFY,                                         *00336000
                   MIN_TERM,                                           *00337000
                   MIN_T_QLFY,                                         *00338000
                   MAX_TERM,                                           *00339000
                   MAX_T_QLFY,                                         *00340000
                   BRCH_REG_CODE,                                      *00341000
                   BRCHREG_QLFY,                                       *00342000
                   PRIME_CATG,                                         *00343000
                   PRIME_C_QLFY,                                       *00344000
                   PRIME_C_INCEN,                                      *00345000
                   CMB_BAL_CD_1,                                       *00346000
                   CMB_BAL_CD_1_Q,                                     *00347000
                   CMB_BAL_CD_1_I,                                     *00348000
                   CMB_BAL_CD_2,                                       *00349000
                   CMB_BAL_CD_2_Q,                                     *00350000
                   CMB_BAL_CD_2_I,                                     *00351000
                   BAL_C_CODE_1,                                       *00352000
                   BAL_CATG_Q_1,                                       *00353000
                   BAL_CATG_I_1,                                       *00354000
                   BAL_C_CODE_2,                                       *00355000
                   BAL_CATG_Q_2,                                       *00356000
                   BAL_CATG_I_2,                                       *00357000
                   USER_RTN,                                           *00358000
                   USER_RTN_QLFY,                                      *00359000
                   USER_RTN_INCEN,                                     *00360000
                   PRIMARY_SVC,                                        *00361000
                   SEC_SC_INC_OPT,                                     *00362000
                   DISC_AMT,                                           *00363000
                   DISC_PCT,                                           *00364000
                   PRIMARY_RATE,                                       *00365000
                   SEC_RATE_OPT,                                       *00366000
                   RATE_INCEN,                                         *00367000
                   TIME_OPT,                                           *00368000
                   RATE_BAL_CODE,                                      *00369000
                   CASH_MIN_AMOUNT,                                    *00370000
                   CASH_MAX_AMOUNT,                                    *00371000
                   CASH_BAL_CODE,                                      *00372000
                   MISC_OPTION_1,                                      *00373000
                   MISC_OPTION_2,                                      *00374000
                   MISC_OPTION_3,                                      *00375000
                   SVC_INCEN,                                          *00376000
                   SVC_CHG_BAL_CD,                                     *00377000
                   RATE_PCT,                                           *00378000
                   TRAN_PLAN,                                          *00379000
                   USER_OPT_REL1,                                      *00380000
                   USER_OPT_01,                                        *00381000
                   USER_OPT_Q_1,                                       *00382000
                   USER_OPT_REL2,                                      *00383000
                   USER_OPT_02,                                        *00384000
                   USER_OPT_Q_2,                                       *00385000
                   USER_OPT_REL3,                                      *00386000
                   USER_OPT_03,                                        *00387000
                   USER_OPT_Q_3,                                       *00388000
                   USER_OPT_REL4,                                      *00389000
                   USER_OPT_04,                                        *00390000
                   USER_OPT_Q_4,                                       *00391000
                   CMB_BAL_CD_3,                                       *00392000
                   CMB_BAL_CD_3_Q,                                     *00393000
                   CMB_BAL_CD_3_I,                                     *00394000
                   CMB_BAL_CD_4,                                       *00395000
                   CMB_BAL_CD_4_Q,                                     *00396000
                   CMB_BAL_CD_4_I,                                     *00397000
                   SEC_SVC_OPT,                                        *00398000
                   RATE_OPTION,                                        *00399000
                   SEC_SVC_PLAN,                                       *00400000
                   SEC_SVC_TABLE,                                      *00401000
                   SEC_SVC_BAL_CD,                                     *00402000
                   RATE_TYPE,                                          *00403000
                   USER_TYPE_01,                                       *00404000
                   USER_TYPE_02,                                       *00405000
                   USER_TYPE_03,                                       *00406000
                   USER_TYPE_04                                        *00407000
                 FETCH FIRST 1 ROW ONLY                                 00408000
         EXEC  SQL OPEN S02UPD0                                         00409000
         MVC   SQWCSUCA,=A(CLSUC0)     SET CURSOR CLOSE ROUTINE ADDRESS 00410000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00411000
         BR    14                      RETURN TO CALLER                 00412000
         LTORG                                                          00413000
*                                                                       00414000
**********************************************************************  00415000
* FETCH FROM UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                   00416000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00417000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00418000
*     THE ACTUAL ROW.                                                   00419000
**********************************************************************  00420000
*                                                                       00421000
FETUC0   DS    0H                                                       00422000
         USING FETUC0,12               ESTABLISH BASE REGISTER          00423000
         B     *+6                     BRANCH AROUND ADCON              00424000
BASFET0  DC    AL2(4096)                                                00425000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00426000
         AH    3,BASFET0               ADD 4K                           00427000
         EXEC  SQL FETCH S02UPD0                                       *00428000
                 INTO                                                  *00429000
                   :AUDDATE,                                           *00430000
                   :AUDTIME,                                           *00431000
                   :AUDUSER,                                           *00432000
                   :AUDORG,                                            *00433000
                   :ISPRIM,                                            *00434000
                   :PRODGRP,                                           *00435000
                   :MINACCT,                                           *00436000
                   :MNENQLFY,                                          *00437000
                   :MAXSCND,                                           *00438000
                   :MSENQLFY,                                          *00439000
                   :MINTERM,                                           *00440000
                   :MINQLFY,                                           *00441000
                   :MAXTERM,                                           *00442000
                   :MAXQLFY,                                           *00443000
                   :BRREGCD,                                           *00444000
                   :BRRGQLFY,                                          *00445000
                   :PRIMCATG,                                          *00446000
                   :PRIMQLFY,                                          *00447000
                   :PRIMINC,                                           *00448000
                   :CBALCAT1,                                          *00449000
                   :CBCAT1Q,                                           *00450000
                   :COMBBAL1,                                          *00451000
                   :CBALCAT2,                                          *00452000
                   :CBCAT2Q,                                           *00453000
                   :COMBBAL2,                                          *00454000
                   :BALCAT1,                                           *00455000
                   :BALQLFY1,                                          *00456000
                   :BALINCV1,                                          *00457000
                   :BALCAT2,                                           *00458000
                   :BALQLFY2,                                          *00459000
                   :BALINCV2,                                          *00460000
                   :USERRTN,                                           *00461000
                   :USERQLFY,                                          *00462000
                   :USINCTV,                                           *00463000
                   :PRIMESVC,                                          *00464000
                   :SCNDSVC,                                           *00465000
                   :DISCAMT,                                           *00466000
                   :DISCPCT,                                           *00467000
                   :PRIMERTE,                                          *00468000
                   :SCNDRATE,                                          *00469000
                   :RTINCEN,                                           *00470000
                   :TIMEOPT,                                           *00471000
                   :RTBALCD,                                           *00472000
                   :CASHMIN,                                           *00473000
                   :CASHMAX,                                           *00474000
                   :CASHBAL,                                           *00475000
                   :MISCOPT1,                                          *00476000
                   :MISCOPT2,                                          *00477000
                   :MISCOPT3,                                          *00478000
                   :SCINCEN,                                           *00479000
                   :SCBALCD,                                           *00480000
                   :RATEPCT,                                           *00481000
                   :TRANPLAN,                                          *00482000
                   :UOPTREL1,                                          *00483000
                   :UOPT1,                                             *00484000
                   :UOPTQ1,                                            *00485000
                   :UOPTREL2,                                          *00486000
                   :UOPT2,                                             *00487000
                   :UOPTQ2,                                            *00488000
                   :UOPTREL3,                                          *00489000
                   :UOPT3,                                             *00490000
                   :UOPTQ3,                                            *00491000
                   :UOPTREL4,                                          *00492000
                   :UOPT4,                                             *00493000
                   :UOPTQ4,                                            *00494000
                   :CBALCAT3,                                          *00495000
                   :CBCAT3Q,                                           *00496000
                   :COMBBAL3,                                          *00497000
                   :CBALCAT4,                                          *00498000
                   :CBCAT4Q,                                           *00499000
                   :COMBBAL4,                                          *00500000
                   :SECSVOPT,                                          *00501000
                   :RATEOPT,                                           *00502000
                   :SSPLAN,                                            *00503000
                   :SSTABLE,                                           *00504000
                   :SSBALCD,                                           *00505000
                   :RTETYP,                                            *00506000
                   :USERTYP1,                                          *00507000
                   :USERTYP2,                                          *00508000
                   :USERTYP3,                                          *00509000
                   :USERTYP4                                            00510000
         MVI   SQWKMRP,X'03'           MOVE HOST VARIABLES TO RECORD    00511000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00512000
         BALR  14,15                   MOVE REQUESTED DATA              00513000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00514000
         BR    14                      RETURN TO CALLER                 00515000
         LTORG                                                          00516000
*                                                                       00517000
**********************************************************************  00518000
* INSERT STATEMENT:                                                     00519000
*   THIS STATEMENT SUPPORTS THE PUT VERB.                               00520000
**********************************************************************  00521000
*                                                                       00522000
INSROW   DS    0H                                                       00523000
         USING INSROW,12               ESTABLISH BASE REGISTER          00524000
         MVI   SQWKMRP,X'01'           MOVE RECORD TO HOST VARIABLES    00525000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00526000
         BALR  14,15                   MOVE REQUESTED DATA              00527000
         B     *+6                     BRANCH AROUND ADCON              00528000
BASINS0  DC    AL2(4096)                                                00529000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00530000
         AH    3,BASINS0               ADD 4K                           00531000
         EXEC  SQL INSERT INTO S02                                     *00532000
                   (                                                   *00533000
                   INST_NBR,                                           *00534000
                   RECORD_NBR,                                         *00535000
                   MODEL,                                              *00536000
                   PROD_CODE,                                          *00537000
                   ACCT_TYPE,                                          *00538000
                   AUDIT_DATE,                                         *00539000
                   AUDIT_TIME,                                         *00540000
                   AUDIT_USER,                                         *00541000
                   AUDIT_ORG,                                          *00542000
                   IS_PRIMARY,                                         *00543000
                   PROD_GROUP,                                         *00544000
                   MIN_ACCT,                                           *00545000
                   MIN_N_QLFY,                                         *00546000
                   MAX_SCND,                                           *00547000
                   MAX_S_QLFY,                                         *00548000
                   MIN_TERM,                                           *00549000
                   MIN_T_QLFY,                                         *00550000
                   MAX_TERM,                                           *00551000
                   MAX_T_QLFY,                                         *00552000
                   BRCH_REG_CODE,                                      *00553000
                   BRCHREG_QLFY,                                       *00554000
                   PRIME_CATG,                                         *00555000
                   PRIME_C_QLFY,                                       *00556000
                   PRIME_C_INCEN,                                      *00557000
                   CMB_BAL_CD_1,                                       *00558000
                   CMB_BAL_CD_1_Q,                                     *00559000
                   CMB_BAL_CD_1_I,                                     *00560000
                   CMB_BAL_CD_2,                                       *00561000
                   CMB_BAL_CD_2_Q,                                     *00562000
                   CMB_BAL_CD_2_I,                                     *00563000
                   BAL_C_CODE_1,                                       *00564000
                   BAL_CATG_Q_1,                                       *00565000
                   BAL_CATG_I_1,                                       *00566000
                   BAL_C_CODE_2,                                       *00567000
                   BAL_CATG_Q_2,                                       *00568000
                   BAL_CATG_I_2,                                       *00569000
                   USER_RTN,                                           *00570000
                   USER_RTN_QLFY,                                      *00571000
                   USER_RTN_INCEN,                                     *00572000
                   PRIMARY_SVC,                                        *00573000
                   SEC_SC_INC_OPT,                                     *00574000
                   DISC_AMT,                                           *00575000
                   DISC_PCT,                                           *00576000
                   PRIMARY_RATE,                                       *00577000
                   SEC_RATE_OPT,                                       *00578000
                   RATE_INCEN,                                         *00579000
                   TIME_OPT,                                           *00580000
                   RATE_BAL_CODE,                                      *00581000
                   CASH_MIN_AMOUNT,                                    *00582000
                   CASH_MAX_AMOUNT,                                    *00583000
                   CASH_BAL_CODE,                                      *00584000
                   MISC_OPTION_1,                                      *00585000
                   MISC_OPTION_2,                                      *00586000
                   MISC_OPTION_3,                                      *00587000
                   SVC_INCEN,                                          *00588000
                   SVC_CHG_BAL_CD,                                     *00589000
                   RATE_PCT,                                           *00590000
                   TRAN_PLAN,                                          *00591000
                   USER_OPT_REL1,                                      *00592000
                   USER_OPT_01,                                        *00593000
                   USER_OPT_Q_1,                                       *00594000
                   USER_OPT_REL2,                                      *00595000
                   USER_OPT_02,                                        *00596000
                   USER_OPT_Q_2,                                       *00597000
                   USER_OPT_REL3,                                      *00598000
                   USER_OPT_03,                                        *00599000
                   USER_OPT_Q_3,                                       *00600000
                   USER_OPT_REL4,                                      *00601000
                   USER_OPT_04,                                        *00602000
                   USER_OPT_Q_4,                                       *00603000
                   CMB_BAL_CD_3,                                       *00604000
                   CMB_BAL_CD_3_Q,                                     *00605000
                   CMB_BAL_CD_3_I,                                     *00606000
                   CMB_BAL_CD_4,                                       *00607000
                   CMB_BAL_CD_4_Q,                                     *00608000
                   CMB_BAL_CD_4_I,                                     *00609000
                   SEC_SVC_OPT,                                        *00610000
                   RATE_OPTION,                                        *00611000
                   SEC_SVC_PLAN,                                       *00612000
                   SEC_SVC_TABLE,                                      *00613000
                   SEC_SVC_BAL_CD,                                     *00614000
                   RATE_TYPE,                                          *00615000
                   USER_TYPE_01,                                       *00616000
                   USER_TYPE_02,                                       *00617000
                   USER_TYPE_03,                                       *00618000
                   USER_TYPE_04                                        *00619000
                   )                                                   *00620000
                  VALUES                                               *00621000
                   (                                                   *00622000
                   :INST,                                              *00623000
                   :RECNBR,                                            *00624000
                   :MODEL,                                             *00625000
                   :PRODCODE,                                          *00626000
                   :ACCTTYPE,                                          *00627000
                   :AUDDATE,                                           *00628000
                   :AUDTIME,                                           *00629000
                   :AUDUSER,                                           *00630000
                   :AUDORG,                                            *00631000
                   :ISPRIM,                                            *00632000
                   :PRODGRP,                                           *00633000
                   :MINACCT,                                           *00634000
                   :MNENQLFY,                                          *00635000
                   :MAXSCND,                                           *00636000
                   :MSENQLFY,                                          *00637000
                   :MINTERM,                                           *00638000
                   :MINQLFY,                                           *00639000
                   :MAXTERM,                                           *00640000
                   :MAXQLFY,                                           *00641000
                   :BRREGCD,                                           *00642000
                   :BRRGQLFY,                                          *00643000
                   :PRIMCATG,                                          *00644000
                   :PRIMQLFY,                                          *00645000
                   :PRIMINC,                                           *00646000
                   :CBALCAT1,                                          *00647000
                   :CBCAT1Q,                                           *00648000
                   :COMBBAL1,                                          *00649000
                   :CBALCAT2,                                          *00650000
                   :CBCAT2Q,                                           *00651000
                   :COMBBAL2,                                          *00652000
                   :BALCAT1,                                           *00653000
                   :BALQLFY1,                                          *00654000
                   :BALINCV1,                                          *00655000
                   :BALCAT2,                                           *00656000
                   :BALQLFY2,                                          *00657000
                   :BALINCV2,                                          *00658000
                   :USERRTN,                                           *00659000
                   :USERQLFY,                                          *00660000
                   :USINCTV,                                           *00661000
                   :PRIMESVC,                                          *00662000
                   :SCNDSVC,                                           *00663000
                   :DISCAMT,                                           *00664000
                   :DISCPCT,                                           *00665000
                   :PRIMERTE,                                          *00666000
                   :SCNDRATE,                                          *00667000
                   :RTINCEN,                                           *00668000
                   :TIMEOPT,                                           *00669000
                   :RTBALCD,                                           *00670000
                   :CASHMIN,                                           *00671000
                   :CASHMAX,                                           *00672000
                   :CASHBAL,                                           *00673000
                   :MISCOPT1,                                          *00674000
                   :MISCOPT2,                                          *00675000
                   :MISCOPT3,                                          *00676000
                   :SCINCEN,                                           *00677000
                   :SCBALCD,                                           *00678000
                   :RATEPCT,                                           *00679000
                   :TRANPLAN,                                          *00680000
                   :UOPTREL1,                                          *00681000
                   :UOPT1,                                             *00682000
                   :UOPTQ1,                                            *00683000
                   :UOPTREL2,                                          *00684000
                   :UOPT2,                                             *00685000
                   :UOPTQ2,                                            *00686000
                   :UOPTREL3,                                          *00687000
                   :UOPT3,                                             *00688000
                   :UOPTQ3,                                            *00689000
                   :UOPTREL4,                                          *00690000
                   :UOPT4,                                             *00691000
                   :UOPTQ4,                                            *00692000
                   :CBALCAT3,                                          *00693000
                   :CBCAT3Q,                                           *00694000
                   :COMBBAL3,                                          *00695000
                   :CBALCAT4,                                          *00696000
                   :CBCAT4Q,                                           *00697000
                   :COMBBAL4,                                          *00698000
                   :SECSVOPT,                                          *00699000
                   :RATEOPT,                                           *00700000
                   :SSPLAN,                                            *00701000
                   :SSTABLE,                                           *00702000
                   :SSBALCD,                                           *00703000
                   :RTETYP,                                            *00704000
                   :USERTYP1,                                          *00705000
                   :USERTYP2,                                          *00706000
                   :USERTYP3,                                          *00707000
                   :USERTYP4                                           *00708000
                   )                                                    00709000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00710000
         BR    14                      RETURN TO CALLER                 00711000
         LTORG                                                          00712000
*                                                                       00713000
**********************************************************************  00714000
* UPDATE STATEMENT BY PRIMARY KEY:                                      00715000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             00716000
**********************************************************************  00717000
*                                                                       00718000
UPDUC0   DS    0H                                                       00719000
         USING UPDUC0,12               ESTABLISH BASE REGISTER          00720000
         MVI   SQWKMRP,X'01'           MOVE RECORD TO HOST VARIABLES    00721000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00722000
         BALR  14,15                   MOVE REQUESTED DATA              00723000
         B     *+6                     BRANCH AROUND ADCON              00724000
BASUPD0  DC    AL2(4096)                                                00725000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00726000
         AH    3,BASUPD0               ADD 4K                           00727000
         EXEC  SQL UPDATE S02                                          *00728000
                   SET                                                 *00729000
                     AUDIT_DATE = :AUDDATE,                            *00730000
                     AUDIT_TIME = :AUDTIME,                            *00731000
                     AUDIT_USER = :AUDUSER,                            *00732000
                     AUDIT_ORG = :AUDORG,                              *00733000
                     IS_PRIMARY = :ISPRIM,                             *00734000
                     PROD_GROUP = :PRODGRP,                            *00735000
                     MIN_ACCT = :MINACCT,                              *00736000
                     MIN_N_QLFY = :MNENQLFY,                           *00737000
                     MAX_SCND = :MAXSCND,                              *00738000
                     MAX_S_QLFY = :MSENQLFY,                           *00739000
                     MIN_TERM = :MINTERM,                              *00740000
                     MIN_T_QLFY = :MINQLFY,                            *00741000
                     MAX_TERM = :MAXTERM,                              *00742000
                     MAX_T_QLFY = :MAXQLFY,                            *00743000
                     BRCH_REG_CODE = :BRREGCD,                         *00744000
                     BRCHREG_QLFY = :BRRGQLFY,                         *00745000
                     PRIME_CATG = :PRIMCATG,                           *00746000
                     PRIME_C_QLFY = :PRIMQLFY,                         *00747000
                     PRIME_C_INCEN = :PRIMINC,                         *00748000
                     CMB_BAL_CD_1 = :CBALCAT1,                         *00749000
                     CMB_BAL_CD_1_Q = :CBCAT1Q,                        *00750000
                     CMB_BAL_CD_1_I = :COMBBAL1,                       *00751000
                     CMB_BAL_CD_2 = :CBALCAT2,                         *00752000
                     CMB_BAL_CD_2_Q = :CBCAT2Q,                        *00753000
                     CMB_BAL_CD_2_I = :COMBBAL2,                       *00754000
                     BAL_C_CODE_1 = :BALCAT1,                          *00755000
                     BAL_CATG_Q_1 = :BALQLFY1,                         *00756000
                     BAL_CATG_I_1 = :BALINCV1,                         *00757000
                     BAL_C_CODE_2 = :BALCAT2,                          *00758000
                     BAL_CATG_Q_2 = :BALQLFY2,                         *00759000
                     BAL_CATG_I_2 = :BALINCV2,                         *00760000
                     USER_RTN = :USERRTN,                              *00761000
                     USER_RTN_QLFY = :USERQLFY,                        *00762000
                     USER_RTN_INCEN = :USINCTV,                        *00763000
                     PRIMARY_SVC = :PRIMESVC,                          *00764000
                     SEC_SC_INC_OPT = :SCNDSVC,                        *00765000
                     DISC_AMT = :DISCAMT,                              *00766000
                     DISC_PCT = :DISCPCT,                              *00767000
                     PRIMARY_RATE = :PRIMERTE,                         *00768000
                     SEC_RATE_OPT = :SCNDRATE,                         *00769000
                     RATE_INCEN = :RTINCEN,                            *00770000
                     TIME_OPT = :TIMEOPT,                              *00771000
                     RATE_BAL_CODE = :RTBALCD,                         *00772000
                     CASH_MIN_AMOUNT = :CASHMIN,                       *00773000
                     CASH_MAX_AMOUNT = :CASHMAX,                       *00774000
                     CASH_BAL_CODE = :CASHBAL,                         *00775000
                     MISC_OPTION_1 = :MISCOPT1,                        *00776000
                     MISC_OPTION_2 = :MISCOPT2,                        *00777000
                     MISC_OPTION_3 = :MISCOPT3,                        *00778000
                     SVC_INCEN = :SCINCEN,                             *00779000
                     SVC_CHG_BAL_CD = :SCBALCD,                        *00780000
                     RATE_PCT = :RATEPCT,                              *00781000
                     TRAN_PLAN = :TRANPLAN,                            *00782000
                     USER_OPT_REL1 = :UOPTREL1,                        *00783000
                     USER_OPT_01 = :UOPT1,                             *00784000
                     USER_OPT_Q_1 = :UOPTQ1,                           *00785000
                     USER_OPT_REL2 = :UOPTREL2,                        *00786000
                     USER_OPT_02 = :UOPT2,                             *00787000
                     USER_OPT_Q_2 = :UOPTQ2,                           *00788000
                     USER_OPT_REL3 = :UOPTREL3,                        *00789000
                     USER_OPT_03 = :UOPT3,                             *00790000
                     USER_OPT_Q_3 = :UOPTQ3,                           *00791000
                     USER_OPT_REL4 = :UOPTREL4,                        *00792000
                     USER_OPT_04 = :UOPT4,                             *00793000
                     USER_OPT_Q_4 = :UOPTQ4,                           *00794000
                     CMB_BAL_CD_3 = :CBALCAT3,                         *00795000
                     CMB_BAL_CD_3_Q = :CBCAT3Q,                        *00796000
                     CMB_BAL_CD_3_I = :COMBBAL3,                       *00797000
                     CMB_BAL_CD_4 = :CBALCAT4,                         *00798000
                     CMB_BAL_CD_4_Q = :CBCAT4Q,                        *00799000
                     CMB_BAL_CD_4_I = :COMBBAL4,                       *00800000
                     SEC_SVC_OPT = :SECSVOPT,                          *00801000
                     RATE_OPTION = :RATEOPT,                           *00802000
                     SEC_SVC_PLAN = :SSPLAN,                           *00803000
                     SEC_SVC_TABLE = :SSTABLE,                         *00804000
                     SEC_SVC_BAL_CD = :SSBALCD,                        *00805000
                     RATE_TYPE = :RTETYP,                              *00806000
                     USER_TYPE_01 = :USERTYP1,                         *00807000
                     USER_TYPE_02 = :USERTYP2,                         *00808000
                     USER_TYPE_03 = :USERTYP3,                         *00809000
                     USER_TYPE_04 = :USERTYP4                          *00810000
                 WHERE CURRENT OF S02UPD0                               00811000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00812000
         BR    14                      RETURN TO CALLER                 00813000
         LTORG                                                          00814000
*                                                                       00815000
**********************************************************************  00816000
* DELETE STATEMENT BY PRIMARY KEY:                                      00817000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            00818000
**********************************************************************  00819000
*                                                                       00820000
DELUC0   DS    0H                                                       00821000
         USING DELUC0,12               ESTABLISH BASE REGISTER          00822000
         EXEC  SQL DELETE FROM S02                                     *00823000
                 WHERE CURRENT OF S02UPD0                               00824000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00825000
         BR    14                      RETURN TO CALLER                 00826000
         LTORG                                                          00827000
*                                                                       00828000
**********************************************************************  00829000
* DELETE ALL STATEMENT:                                                 00830000
*   THIS STATEMENT SUPPORTS THE DELETE-FILE VERB.                       00831000
**********************************************************************  00832000
*                                                                       00833000
DELTBL   DS    0H                                                       00834000
         USING DELTBL,12               ESTABLISH BASE REGISTER          00835000
         EXEC  SQL DELETE FROM S02                                      00836000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00837000
         BR    14                      RETURN TO CALLER                 00838000
         LTORG                                                          00839000
*                                                                       00840000
**********************************************************************  00841000
* CLOSE UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                        00842000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00843000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00844000
*     TO CLOSE THE UPDATE CURSOR.                                       00845000
**********************************************************************  00846000
*                                                                       00847000
CLSUC0   DS    0H                                                       00848000
         USING CLSUC0,12               ESTABLISH BASE REGISTER          00849000
         EXEC  SQL CLOSE S02UPD0                                        00850000
         XC    SQWCSUCA,SQWCSUCA       CLEAR CURSOR CLOSE ROUTINE ADDR  00851000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00852000
         BR    14                      RETURN TO CALLER                 00853000
         LTORG                                                          00854000
*                                                                       00855000
**********************************************************************  00856000
* ALTERNATE KEY 1 NOT DEFINED                                           00857000
**********************************************************************  00858000
*                                                                       00859000
SELUC1   DS    0H                                                       00860000
FETUC1   DS    0H                                                       00861000
UPDUC1   DS    0H                                                       00862000
DELUC1   DS    0H                                                       00863000
CLSUC1   DS    0H                                                       00864000
         DC    X'00DEAD01'             FORCE S0C1 ABEND                 00865000
*                                                                       00866000
**********************************************************************  00867000
* ALTERNATE KEY 2 NOT DEFINED                                           00868000
**********************************************************************  00869000
*                                                                       00870000
SELUC2   DS    0H                                                       00871000
FETUC2   DS    0H                                                       00872000
UPDUC2   DS    0H                                                       00873000
DELUC2   DS    0H                                                       00874000
CLSUC2   DS    0H                                                       00875000
         DC    X'00DEAD02'             FORCE S0C1 ABEND                 00876000
*                                                                       00877000
**********************************************************************  00878000
* ALTERNATE KEY 3 NOT DEFINED                                           00879000
**********************************************************************  00880000
*                                                                       00881000
SELUC3   DS    0H                                                       00882000
FETUC3   DS    0H                                                       00883000
UPDUC3   DS    0H                                                       00884000
DELUC3   DS    0H                                                       00885000
CLSUC3   DS    0H                                                       00886000
         DC    X'00DEAD03'             FORCE S0C1 ABEND                 00887000
*                                                                       00888000
         DS    0H                      END OF SQL STATEMENTS            00889000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   00890000
*                                                                       00891000
**********************************************************************  00892000
* DUMMY ENTRY POINT DSNHLI                                              00893000
**********************************************************************  00894000
*                                                                       00895000
         ENTRY DSNHLI                                                   00896000
DSNHLI   DS    0H                                                       00897000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       00898000
         BR    15                      BRANCH TO ATTACH FACILITY        00899000
*                                                                       00900000
**********************************************************************  00901000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  00902000
**********************************************************************  00903000
*                                                                       00904000
* CONVTAB1 TABLE ENTRY FORMAT IS:                                       00905000
*        DC    H'RRRR',H'VVVV',H'LLLL',X'KK',X'DD'                      00906000
* OR:                                                                   00907000
*        DC    H'RRRR',H'VVVV',X'ZZPP',X'KK',X'DD'                      00908000
* WHERE:                                                                00909000
*   RRRR = RECORD AREA OFFSET                                           00910000
*   VVVV = HOST VARIABLE AREA OFFSET                                    00911000
*   LLLL = HALFWORD LENGTH TO MOVE                                      00912000
*   ZZPP = CONVERT ZONED/PACKED LENGTHS (MINUS 1)                       00913000
*   KK   = KEY FIELD MASK:                                              00914000
*            80 = KEY 0 FIELD                                           00915000
*            40 = KEY 1 FIELD                                           00916000
*            20 = KEY 2 FIELD                                           00917000
*            10 = KEY 3 FIELD                                           00918000
*   DD   = DATA FIELD MASK:                                             00919000
*            80 = RECORD FIELD IS PACKED                                00920000
*            40 = HOST VARIABLE IS PACKED                               00921000
*            20 = NULLABLE FIELD                                        00922000
*            01 = DATE FIELD                                            00923000
*            02 = TIME FIELD                                            00924000
*                                                                       00925000
CONVTAB1 DS    0H                      RECORD/HOST VARIABLE CONVERSIONS 00926000
         DC    H'0000',H'0000',H'0027',X'80',X'00'                      00927000
         DC    H'0027',H'0027',H'0181',X'00',X'00'                      00928000
         DC    8X'FF'                                                   00929000
*                                                                       00930000
         LTORG                                                          00931000
         END                                                            00932000
