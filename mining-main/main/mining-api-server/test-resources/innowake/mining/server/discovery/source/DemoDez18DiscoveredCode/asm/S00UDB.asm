**********************************************************************  00001000
*                                                                       00002000
*  S00UDB .... STATIC SQL UPDATE MODULE                                 00003000
*                                                                       00004000
*  CREATION DATE: 04/25/16                                              00005000
*                                                                       00006000
*  FUNCTIONAL DESCRIPTION: THIS PROGRAM CONTAINS THE STATIC SQL         00007000
*  VECTORS REQUIRED TO SUPPORT I/O TO THE S00 TABLE.  IT IS LOADED      00008000
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
AUDDATE  DS    PL5'0.'                                                  00052000
AUDTIME  DS    PL5'0.'                                                  00053000
AUDUSER  DS    CL8                                                      00054000
AUDORG   DS    CL6                                                      00055000
ACCTLIMT DS    CL1                                                      00056000
CUSTLIMT DS    CL1                                                      00057000
EXPRDAY  DS    CL3                                                      00058000
AINCOPT  DS    CL1                                                      00059000
SECSVOPT DS    CL1                                                      00060000
VALIDOPT DS    CL1                                                      00061000
MAXRETDY DS    CL3                                                      00062000
MAXEXTN  DS    CL2                                                      00063000
RATEOPT  DS    CL1                                                      00064000
ENTPROPT DS    CL1                                                      00065000
ENTPINST DS    CL4                                                      00066000
DUPINCOP DS    CL1                                                      00067000
DUPINRUL DS    CL1                                                      00068000
MAXMODEL DS    CL2                                                      00069000
MAXACCTS DS    CL3                                                      00070000
EVHSTOPT DS    CL1                                                      00071000
EVHSTDYS DS    CL3                                                      00072000
*                                                                       00073000
         ORG   ASMREC+(2000-L'SQWADATA) POINT TO ADDITIONAL DATA        00074000
SQWADATA DS    0CL400                  ADDITIONAL DATA PASSED TO MODULE 00075000
SQWSEGF  DS    CL102                   SEGMENTED FROM KEY VALUE         00076000
SQWSEGT  DS    CL102                   SEGMENTED TO KEY VALUE           00077000
SQWAUDIT DS    CL99                    CALLERS AUDIT DATA               00078000
         DS    CL97                    RESERVED                         00079000
*                                                                       00080000
INDVARS  DS    0H                      NULL INDICATOR VARIABLES         00081000
INDVARX  DS    0H                                                       00082000
INDVARL  EQU   INDVARX-INDVARS         NULL INDICATOR AREA LENGTH       00083000
*                                                                       00084000
*                                                                       00085000
**********************************************************************  00086000
* PROGRAM TABLE HEADER SECTION:                                         00087000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00088000
**********************************************************************  00089000
*                                                                       00090000
S00UDB   CSECT                         PROGRAM TABLE SECTION            00091000
S00UDB   AMODE ANY                                                      00092000
S00UDB   RMODE ANY                                                      00093000
         DC    CL8'S00UDB  '           PROGRAM ID                       00094000
         DC    CL1' '                                                   00095000
         DC    CL8'&SYSDATE'           ASSEMBLY DATE                    00096000
         DC    CL1' '                                                   00097000
         DC    CL5'&SYSTIME'           ASSEMBLY TIME                    00098000
         DC    CL1' '                                                   00099000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00100000
         DC    5A(0)                   RESERVED                         00101000
         DC    AL2(0)                  RESERVED                         00102000
         DC    AL2(INDVARL)            NULL INDICATOR AREA LENGTH       00103000
         DC    A(CONVTAB1)             RECORD/HOST CONVERSION TABLE     00104000
         DC    A(0)                    SQLDA DATA TYPE/LENGTH TABLE     00105000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00106000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00107000
         DC    CL29'WWW.INFOR.COM                '                      00107001
*                                                                       00108000
**********************************************************************  00109000
* STATEMENT TABLE SECTION:                                              00110000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00111000
**********************************************************************  00112000
*                                                                       00113000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00114000
STM#TAB  AMODE ANY                                                      00115000
STM#TAB  RMODE ANY                                                      00116000
         DC    4A(0)                   RDB MODULE VECTORS               00117000
         DC    A(SELUC0)               SELECT UPDATE CURSOR (KEY 0)     00118000
         DC    A(SELUC1)               SELECT UPDATE CURSOR (KEY 1)     00119000
         DC    A(SELUC2)               SELECT UPDATE CURSOR (KEY 2)     00120000
         DC    A(SELUC3)               SELECT UPDATE CURSOR (KEY 3)     00121000
         DC    A(FETUC0)               FETCH UPDATE CURSOR (KEY 0)      00122000
         DC    A(FETUC1)               FETCH UPDATE CURSOR (KEY 1)      00123000
         DC    A(FETUC2)               FETCH UPDATE CURSOR (KEY 2)      00124000
         DC    A(FETUC3)               FETCH UPDATE CURSOR (KEY 3)      00125000
         DC    A(CLSUC0)               CLOSE UPDATE CURSOR (KEY 0)      00126000
         DC    A(CLSUC1)               CLOSE UPDATE CURSOR (KEY 1)      00127000
         DC    A(CLSUC2)               CLOSE UPDATE CURSOR (KEY 2)      00128000
         DC    A(CLSUC3)               CLOSE UPDATE CURSOR (KEY 3)      00129000
         DC    20A(0)                  RDB MODULE VECTORS               00130000
         DC    A(INSROW)               INSERT STATEMENT                 00131000
         DC    A(UPDUC0)               UPDATE STATEMENT (KEY 0)         00132000
         DC    A(UPDUC1)               UPDATE STATEMENT (KEY 1)         00133000
         DC    A(UPDUC2)               UPDATE STATEMENT (KEY 2)         00134000
         DC    A(UPDUC3)               UPDATE STATEMENT (KEY 3)         00135000
         DC    A(DELUC0)               DELETE STATEMENT (KEY 0)         00136000
         DC    A(DELUC1)               DELETE STATEMENT (KEY 1)         00137000
         DC    A(DELUC2)               DELETE STATEMENT (KEY 2)         00138000
         DC    A(DELUC3)               DELETE STATEMENT (KEY 3)         00139000
         DC    A(DELTBL)               DELETE ALL STATEMENT             00140000
         DC    4X'FF'                                                   00141000
*                                                                       00142000
**********************************************************************  00143000
* SQL STATEMENT SECTION:                                                00144000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00145000
*     TO SUPPORT THIS TABLE.                                            00146000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00147000
*     ARE EQUIVALENT.                                                   00148000
**********************************************************************  00149000
*                                                                       00150000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00151000
SQL#STMT AMODE ANY                                                      00152000
SQL#STMT RMODE ANY                                                      00153000
         USING SQLDSECT,10             ADDRESS SQLDSECT                 00154000
         USING COM#AREA,11             ADDRESS COMMAREA                 00155000
*                                                                       00156000
**********************************************************************  00157000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY PRIMARY KEY:       00158000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00159000
*   THEY ARE ALSO USED AFTER A SUCCESSFUL SELECT SEQUENTIAL STATEMENT   00160000
*     FOR THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS.                      00161000
**********************************************************************  00162000
*                                                                       00163000
SELUC0   DS    0H                                                       00164000
         USING SELUC0,12               ESTABLISH BASE REGISTER          00165000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00166000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00167000
         BALR  14,15                   MOVE REQUESTED DATA              00168000
         EXEC  SQL DECLARE S00UPD0 CURSOR                              *00169000
               FOR SELECT                                              *00170000
                   AUDIT_DATE,                                         *00171000
                   AUDIT_TIME,                                         *00172000
                   AUDIT_USER,                                         *00173000
                   AUDIT_ORG,                                          *00174000
                   ACCT_LIMIT_OPT,                                     *00175000
                   CUST_LIMIT_OPT,                                     *00176000
                   MAX_EXPIRE_DAY,                                     *00177000
                   ADD_INC_OPT,                                        *00178000
                   SEC_SVC_OPT,                                        *00179000
                   VALID_OPT,                                          *00180000
                   MAX_RETN_DAYS,                                      *00181000
                   MAX_EXTN,                                           *00182000
                   RATE_OPTION,                                        *00183000
                   ENTERPRISE_OPT,                                     *00184000
                   ENTERPRISE_INST,                                    *00185000
                   DUP_INC_OPT,                                        *00186000
                   DUP_INC_RULE,                                       *00187000
                   MODEL_LIMIT,                                        *00188000
                   ACCOUNT_LIMIT,                                      *00189000
                   EVENT_HST_OPT,                                      *00190000
                   EVENT_HST_DAYS                                      *00191000
                 FROM S00                                              *00192000
                 WHERE                                                 *00193000
                   INST_NBR = :INST AND                                *00194000
                   RECORD_NBR = :RECNBR                                *00195000
                 FOR UPDATE OF                                         *00196000
                   AUDIT_DATE,                                         *00197000
                   AUDIT_TIME,                                         *00198000
                   AUDIT_USER,                                         *00199000
                   AUDIT_ORG,                                          *00200000
                   ACCT_LIMIT_OPT,                                     *00201000
                   CUST_LIMIT_OPT,                                     *00202000
                   MAX_EXPIRE_DAY,                                     *00203000
                   ADD_INC_OPT,                                        *00204000
                   SEC_SVC_OPT,                                        *00205000
                   VALID_OPT,                                          *00206000
                   MAX_RETN_DAYS,                                      *00207000
                   MAX_EXTN,                                           *00208000
                   RATE_OPTION,                                        *00209000
                   ENTERPRISE_OPT,                                     *00210000
                   ENTERPRISE_INST,                                    *00211000
                   DUP_INC_OPT,                                        *00212000
                   DUP_INC_RULE,                                       *00213000
                   MODEL_LIMIT,                                        *00214000
                   ACCOUNT_LIMIT,                                      *00215000
                   EVENT_HST_OPT,                                      *00216000
                   EVENT_HST_DAYS                                      *00217000
                 FETCH FIRST 1 ROW ONLY                                 00218000
         EXEC  SQL OPEN S00UPD0                                         00219000
         MVC   SQWCSUCA,=A(CLSUC0)     SET CURSOR CLOSE ROUTINE ADDRESS 00220000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00221000
         BR    14                      RETURN TO CALLER                 00222000
         LTORG                                                          00223000
*                                                                       00224000
**********************************************************************  00225000
* FETCH FROM UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                   00226000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00227000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00228000
*     THE ACTUAL ROW.                                                   00229000
**********************************************************************  00230000
*                                                                       00231000
FETUC0   DS    0H                                                       00232000
         USING FETUC0,12               ESTABLISH BASE REGISTER          00233000
         EXEC  SQL FETCH S00UPD0                                       *00234000
                 INTO                                                  *00235000
                   :AUDDATE,                                           *00236000
                   :AUDTIME,                                           *00237000
                   :AUDUSER,                                           *00238000
                   :AUDORG,                                            *00239000
                   :ACCTLIMT,                                          *00240000
                   :CUSTLIMT,                                          *00241000
                   :EXPRDAY,                                           *00242000
                   :AINCOPT,                                           *00243000
                   :SECSVOPT,                                          *00244000
                   :VALIDOPT,                                          *00245000
                   :MAXRETDY,                                          *00246000
                   :MAXEXTN,                                           *00247000
                   :RATEOPT,                                           *00248000
                   :ENTPROPT,                                          *00249000
                   :ENTPINST,                                          *00250000
                   :DUPINCOP,                                          *00251000
                   :DUPINRUL,                                          *00252000
                   :MAXMODEL,                                          *00253000
                   :MAXACCTS,                                          *00254000
                   :EVHSTOPT,                                          *00255000
                   :EVHSTDYS                                            00256000
         MVI   SQWKMRP,X'03'           MOVE HOST VARIABLES TO RECORD    00257000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00258000
         BALR  14,15                   MOVE REQUESTED DATA              00259000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00260000
         BR    14                      RETURN TO CALLER                 00261000
         LTORG                                                          00262000
*                                                                       00263000
**********************************************************************  00264000
* INSERT STATEMENT:                                                     00265000
*   THIS STATEMENT SUPPORTS THE PUT VERB.                               00266000
**********************************************************************  00267000
*                                                                       00268000
INSROW   DS    0H                                                       00269000
         USING INSROW,12               ESTABLISH BASE REGISTER          00270000
         MVI   SQWKMRP,X'01'           MOVE RECORD TO HOST VARIABLES    00271000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00272000
         BALR  14,15                   MOVE REQUESTED DATA              00273000
         EXEC  SQL INSERT INTO S00                                     *00274000
                   (                                                   *00275000
                   INST_NBR,                                           *00276000
                   RECORD_NBR,                                         *00277000
                   AUDIT_DATE,                                         *00278000
                   AUDIT_TIME,                                         *00279000
                   AUDIT_USER,                                         *00280000
                   AUDIT_ORG,                                          *00281000
                   ACCT_LIMIT_OPT,                                     *00282000
                   CUST_LIMIT_OPT,                                     *00283000
                   MAX_EXPIRE_DAY,                                     *00284000
                   ADD_INC_OPT,                                        *00285000
                   SEC_SVC_OPT,                                        *00286000
                   VALID_OPT,                                          *00287000
                   MAX_RETN_DAYS,                                      *00288000
                   MAX_EXTN,                                           *00289000
                   RATE_OPTION,                                        *00290000
                   ENTERPRISE_OPT,                                     *00291000
                   ENTERPRISE_INST,                                    *00292000
                   DUP_INC_OPT,                                        *00293000
                   DUP_INC_RULE,                                       *00294000
                   MODEL_LIMIT,                                        *00295000
                   ACCOUNT_LIMIT,                                      *00296000
                   EVENT_HST_OPT,                                      *00297000
                   EVENT_HST_DAYS                                      *00298000
                   )                                                   *00299000
                  VALUES                                               *00300000
                   (                                                   *00301000
                   :INST,                                              *00302000
                   :RECNBR,                                            *00303000
                   :AUDDATE,                                           *00304000
                   :AUDTIME,                                           *00305000
                   :AUDUSER,                                           *00306000
                   :AUDORG,                                            *00307000
                   :ACCTLIMT,                                          *00308000
                   :CUSTLIMT,                                          *00309000
                   :EXPRDAY,                                           *00310000
                   :AINCOPT,                                           *00311000
                   :SECSVOPT,                                          *00312000
                   :VALIDOPT,                                          *00313000
                   :MAXRETDY,                                          *00314000
                   :MAXEXTN,                                           *00315000
                   :RATEOPT,                                           *00316000
                   :ENTPROPT,                                          *00317000
                   :ENTPINST,                                          *00318000
                   :DUPINCOP,                                          *00319000
                   :DUPINRUL,                                          *00320000
                   :MAXMODEL,                                          *00321000
                   :MAXACCTS,                                          *00322000
                   :EVHSTOPT,                                          *00323000
                   :EVHSTDYS                                           *00324000
                   )                                                    00325000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00326000
         BR    14                      RETURN TO CALLER                 00327000
         LTORG                                                          00328000
*                                                                       00329000
**********************************************************************  00330000
* UPDATE STATEMENT BY PRIMARY KEY:                                      00331000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             00332000
**********************************************************************  00333000
*                                                                       00334000
UPDUC0   DS    0H                                                       00335000
         USING UPDUC0,12               ESTABLISH BASE REGISTER          00336000
         MVI   SQWKMRP,X'01'           MOVE RECORD TO HOST VARIABLES    00337000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00338000
         BALR  14,15                   MOVE REQUESTED DATA              00339000
         EXEC  SQL UPDATE S00                                          *00340000
                   SET                                                 *00341000
                     AUDIT_DATE = :AUDDATE,                            *00342000
                     AUDIT_TIME = :AUDTIME,                            *00343000
                     AUDIT_USER = :AUDUSER,                            *00344000
                     AUDIT_ORG = :AUDORG,                              *00345000
                     ACCT_LIMIT_OPT = :ACCTLIMT,                       *00346000
                     CUST_LIMIT_OPT = :CUSTLIMT,                       *00347000
                     MAX_EXPIRE_DAY = :EXPRDAY,                        *00348000
                     ADD_INC_OPT = :AINCOPT,                           *00349000
                     SEC_SVC_OPT = :SECSVOPT,                          *00350000
                     VALID_OPT = :VALIDOPT,                            *00351000
                     MAX_RETN_DAYS = :MAXRETDY,                        *00352000
                     MAX_EXTN = :MAXEXTN,                              *00353000
                     RATE_OPTION = :RATEOPT,                           *00354000
                     ENTERPRISE_OPT = :ENTPROPT,                       *00355000
                     ENTERPRISE_INST = :ENTPINST,                      *00356000
                     DUP_INC_OPT = :DUPINCOP,                          *00357000
                     DUP_INC_RULE = :DUPINRUL,                         *00358000
                     MODEL_LIMIT = :MAXMODEL,                          *00359000
                     ACCOUNT_LIMIT = :MAXACCTS,                        *00360000
                     EVENT_HST_OPT = :EVHSTOPT,                        *00361000
                     EVENT_HST_DAYS = :EVHSTDYS                        *00362000
                 WHERE CURRENT OF S00UPD0                               00363000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00364000
         BR    14                      RETURN TO CALLER                 00365000
         LTORG                                                          00366000
*                                                                       00367000
**********************************************************************  00368000
* DELETE STATEMENT BY PRIMARY KEY:                                      00369000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            00370000
**********************************************************************  00371000
*                                                                       00372000
DELUC0   DS    0H                                                       00373000
         USING DELUC0,12               ESTABLISH BASE REGISTER          00374000
         EXEC  SQL DELETE FROM S00                                     *00375000
                 WHERE CURRENT OF S00UPD0                               00376000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00377000
         BR    14                      RETURN TO CALLER                 00378000
         LTORG                                                          00379000
*                                                                       00380000
**********************************************************************  00381000
* DELETE ALL STATEMENT:                                                 00382000
*   THIS STATEMENT SUPPORTS THE DELETE-FILE VERB.                       00383000
**********************************************************************  00384000
*                                                                       00385000
DELTBL   DS    0H                                                       00386000
         USING DELTBL,12               ESTABLISH BASE REGISTER          00387000
         EXEC  SQL DELETE FROM S00                                      00388000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00389000
         BR    14                      RETURN TO CALLER                 00390000
         LTORG                                                          00391000
*                                                                       00392000
**********************************************************************  00393000
* CLOSE UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                        00394000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00395000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00396000
*     TO CLOSE THE UPDATE CURSOR.                                       00397000
**********************************************************************  00398000
*                                                                       00399000
CLSUC0   DS    0H                                                       00400000
         USING CLSUC0,12               ESTABLISH BASE REGISTER          00401000
         EXEC  SQL CLOSE S00UPD0                                        00402000
         XC    SQWCSUCA,SQWCSUCA       CLEAR CURSOR CLOSE ROUTINE ADDR  00403000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00404000
         BR    14                      RETURN TO CALLER                 00405000
         LTORG                                                          00406000
*                                                                       00407000
**********************************************************************  00408000
* ALTERNATE KEY 1 NOT DEFINED                                           00409000
**********************************************************************  00410000
*                                                                       00411000
SELUC1   DS    0H                                                       00412000
FETUC1   DS    0H                                                       00413000
UPDUC1   DS    0H                                                       00414000
DELUC1   DS    0H                                                       00415000
CLSUC1   DS    0H                                                       00416000
         DC    X'00DEAD01'             FORCE S0C1 ABEND                 00417000
*                                                                       00418000
**********************************************************************  00419000
* ALTERNATE KEY 2 NOT DEFINED                                           00420000
**********************************************************************  00421000
*                                                                       00422000
SELUC2   DS    0H                                                       00423000
FETUC2   DS    0H                                                       00424000
UPDUC2   DS    0H                                                       00425000
DELUC2   DS    0H                                                       00426000
CLSUC2   DS    0H                                                       00427000
         DC    X'00DEAD02'             FORCE S0C1 ABEND                 00428000
*                                                                       00429000
**********************************************************************  00430000
* ALTERNATE KEY 3 NOT DEFINED                                           00431000
**********************************************************************  00432000
*                                                                       00433000
SELUC3   DS    0H                                                       00434000
FETUC3   DS    0H                                                       00435000
UPDUC3   DS    0H                                                       00436000
DELUC3   DS    0H                                                       00437000
CLSUC3   DS    0H                                                       00438000
         DC    X'00DEAD03'             FORCE S0C1 ABEND                 00439000
*                                                                       00440000
         DS    0H                      END OF SQL STATEMENTS            00441000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   00442000
*                                                                       00443000
**********************************************************************  00444000
* DUMMY ENTRY POINT DSNHLI                                              00445000
**********************************************************************  00446000
*                                                                       00447000
         ENTRY DSNHLI                                                   00448000
DSNHLI   DS    0H                                                       00449000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       00450000
         BR    15                      BRANCH TO ATTACH FACILITY        00451000
*                                                                       00452000
**********************************************************************  00453000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  00454000
**********************************************************************  00455000
*                                                                       00456000
* CONVTAB1 TABLE ENTRY FORMAT IS:                                       00457000
*        DC    H'RRRR',H'VVVV',H'LLLL',X'KK',X'DD'                      00458000
* OR:                                                                   00459000
*        DC    H'RRRR',H'VVVV',X'ZZPP',X'KK',X'DD'                      00460000
* WHERE:                                                                00461000
*   RRRR = RECORD AREA OFFSET                                           00462000
*   VVVV = HOST VARIABLE AREA OFFSET                                    00463000
*   LLLL = HALFWORD LENGTH TO MOVE                                      00464000
*   ZZPP = CONVERT ZONED/PACKED LENGTHS (MINUS 1)                       00465000
*   KK   = KEY FIELD MASK:                                              00466000
*            80 = KEY 0 FIELD                                           00467000
*            40 = KEY 1 FIELD                                           00468000
*            20 = KEY 2 FIELD                                           00469000
*            10 = KEY 3 FIELD                                           00470000
*   DD   = DATA FIELD MASK:                                             00471000
*            80 = RECORD FIELD IS PACKED                                00472000
*            40 = HOST VARIABLE IS PACKED                               00473000
*            20 = NULLABLE FIELD                                        00474000
*            01 = DATE FIELD                                            00475000
*            02 = TIME FIELD                                            00476000
*                                                                       00477000
CONVTAB1 DS    0H                      RECORD/HOST VARIABLE CONVERSIONS 00478000
         DC    H'0000',H'0000',H'0008',X'80',X'00'                      00479000
         DC    H'0008',H'0008',H'0054',X'00',X'00'                      00480000
         DC    8X'FF'                                                   00481000
*                                                                       00482000
         LTORG                                                          00483000
         END                                                            00484000
