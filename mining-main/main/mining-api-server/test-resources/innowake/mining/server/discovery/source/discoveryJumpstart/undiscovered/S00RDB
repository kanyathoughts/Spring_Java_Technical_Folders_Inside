**********************************************************************  00001000
*                                                                       00002000
*  S00RDB .... STATIC SQL READ MODULE                                   00003000
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
* ROWSET SQLDA AREA ADDRESSED BY REGISTER 2                             00087000
**********************************************************************  00088000
*                                                                       00089000
SQDSQLDA DSECT                         ROWSET SQLDA AREA                00090000
*                                                                       00091000
**********************************************************************  00092000
* PROGRAM TABLE HEADER SECTION:                                         00093000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00094000
**********************************************************************  00095000
*                                                                       00096000
S00RDB   CSECT                         PROGRAM TABLE SECTION            00097000
S00RDB   AMODE ANY                                                      00098000
S00RDB   RMODE ANY                                                      00099000
         DC    CL8'S00RDB  '           PROGRAM ID                       00100000
         DC    CL1' '                                                   00101000
         DC    CL8'&SYSDATE'           ASSEMBLY DATE                    00102000
         DC    CL1' '                                                   00103000
         DC    CL5'&SYSTIME'           ASSEMBLY TIME                    00104000
         DC    CL1' '                                                   00105000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00106000
         DC    5A(0)                   RESERVED                         00107000
         DC    AL2(0)                  RESERVED                         00108000
         DC    AL2(INDVARL)            NULL INDICATOR AREA LENGTH       00109000
         DC    A(CONVTAB1)             RECORD/HOST CONVERSION TABLE     00110000
         DC    A(CONVTAB2)             SQLDA DATA TYPE/LENGTH TABLE     00111000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00112000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00113000
         DC    CL29'WWW.INFOR.COM                '                      00113001
*                                                                       00114000
**********************************************************************  00115000
* STATEMENT TABLE SECTION:                                              00116000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00117000
**********************************************************************  00118000
*                                                                       00119000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00120000
STM#TAB  AMODE ANY                                                      00121000
STM#TAB  RMODE ANY                                                      00122000
         DC    A(SELIN0)               SELECT INTO (KEY 0)              00123000
         DC    A(SELIN1)               SELECT INTO (KEY 1)              00124000
         DC    A(SELIN2)               SELECT INTO (KEY 2)              00125000
         DC    A(SELIN3)               SELECT INTO (KEY 3)              00126000
         DC    12A(0)                  UDB MODULE VECTORS               00127000
         DC    A(SELXC0)               SELECT SEQ CURSOR (KEY 0)        00128000
         DC    A(SELXC1)               SELECT SEQ CURSOR (KEY 1)        00129000
         DC    A(SELXC2)               SELECT SEQ CURSOR (KEY 2)        00130000
         DC    A(SELXC3)               SELECT SEQ CURSOR (KEY 3)        00131000
         DC    A(FETDC0)               FETCH SEQ DATA CURSOR (KEY 0)    00132000
         DC    A(FETDC1)               FETCH SEQ DATA CURSOR (KEY 1)    00133000
         DC    A(FETDC2)               FETCH SEQ DATA CURSOR (KEY 2)    00134000
         DC    A(FETDC3)               FETCH SEQ DATA CURSOR (KEY 3)    00135000
         DC    A(FETKC0)               FETCH SEQ KEY CURSOR (KEY 0)     00136000
         DC    A(FETKC1)               FETCH SEQ KEY CURSOR (KEY 1)     00137000
         DC    A(FETKC2)               FETCH SEQ KEY CURSOR (KEY 2)     00138000
         DC    A(FETKC3)               FETCH SEQ KEY CURSOR (KEY 3)     00139000
         DC    A(CLSXC0)               CLOSE SEQ CURSOR (KEY 0)         00140000
         DC    A(CLSXC1)               CLOSE SEQ CURSOR (KEY 1)         00141000
         DC    A(CLSXC2)               CLOSE SEQ CURSOR (KEY 2)         00142000
         DC    A(CLSXC3)               CLOSE SEQ CURSOR (KEY 3)         00143000
         DC    A(SELKY0)               SELECT KEY (KEY 0)               00144000
         DC    A(SELKY1)               SELECT KEY (KEY 1)               00145000
         DC    A(SELKY2)               SELECT KEY (KEY 2)               00146000
         DC    A(SELKY3)               SELECT KEY (KEY 3)               00147000
         DC    10A(0)                  UDB MODULE VECTORS               00148000
         DC    4X'FF'                                                   00149000
*                                                                       00150000
**********************************************************************  00151000
* SQL STATEMENT SECTION:                                                00152000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00153000
*     TO SUPPORT THIS TABLE.                                            00154000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00155000
*     ARE EQUIVALENT.                                                   00156000
**********************************************************************  00157000
*                                                                       00158000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00159000
SQL#STMT AMODE ANY                                                      00160000
SQL#STMT RMODE ANY                                                      00161000
         USING SQDSQLDA,2              ADDRESS ROWSET SQLDA AREA        00162000
         USING SQLDSECT,10             ADDRESS SQLDSECT                 00163000
         USING COM#AREA,11             ADDRESS COMMAREA                 00164000
*                                                                       00165000
**********************************************************************  00166000
* SELECT INTO STATEMENT BY PRIMARY KEY:                                 00167000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00168000
**********************************************************************  00169000
*                                                                       00170000
SELIN0   DS    0H                                                       00171000
         USING SELIN0,12               ESTABLISH BASE REGISTER          00172000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00173000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00174000
         BALR  14,15                   MOVE REQUESTED DATA              00175000
         EXEC  SQL SELECT                                              *00176000
                   AUDIT_DATE,                                         *00177000
                   AUDIT_TIME,                                         *00178000
                   AUDIT_USER,                                         *00179000
                   AUDIT_ORG,                                          *00180000
                   ACCT_LIMIT_OPT,                                     *00181000
                   CUST_LIMIT_OPT,                                     *00182000
                   MAX_EXPIRE_DAY,                                     *00183000
                   ADD_INC_OPT,                                        *00184000
                   SEC_SVC_OPT,                                        *00185000
                   VALID_OPT,                                          *00186000
                   MAX_RETN_DAYS,                                      *00187000
                   MAX_EXTN,                                           *00188000
                   RATE_OPTION,                                        *00189000
                   ENTERPRISE_OPT,                                     *00190000
                   ENTERPRISE_INST,                                    *00191000
                   DUP_INC_OPT,                                        *00192000
                   DUP_INC_RULE,                                       *00193000
                   MODEL_LIMIT,                                        *00194000
                   ACCOUNT_LIMIT,                                      *00195000
                   EVENT_HST_OPT,                                      *00196000
                   EVENT_HST_DAYS                                      *00197000
                 INTO                                                  *00198000
                   :AUDDATE,                                           *00199000
                   :AUDTIME,                                           *00200000
                   :AUDUSER,                                           *00201000
                   :AUDORG,                                            *00202000
                   :ACCTLIMT,                                          *00203000
                   :CUSTLIMT,                                          *00204000
                   :EXPRDAY,                                           *00205000
                   :AINCOPT,                                           *00206000
                   :SECSVOPT,                                          *00207000
                   :VALIDOPT,                                          *00208000
                   :MAXRETDY,                                          *00209000
                   :MAXEXTN,                                           *00210000
                   :RATEOPT,                                           *00211000
                   :ENTPROPT,                                          *00212000
                   :ENTPINST,                                          *00213000
                   :DUPINCOP,                                          *00214000
                   :DUPINRUL,                                          *00215000
                   :MAXMODEL,                                          *00216000
                   :MAXACCTS,                                          *00217000
                   :EVHSTOPT,                                          *00218000
                   :EVHSTDYS                                           *00219000
                 FROM S00                                              *00220000
                 WHERE                                                 *00221000
                   INST_NBR = :INST AND                                *00222000
                   RECORD_NBR = :RECNBR                                *00223000
                 FETCH FIRST 1 ROW ONLY                                 00224000
         MVI   SQWKMRP,X'03'           MOVE HOST VARIABLES TO RECORD    00225000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00226000
         BALR  14,15                   MOVE REQUESTED DATA              00227000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00228000
         BR    14                      RETURN TO CALLER                 00229000
         LTORG                                                          00230000
*                                                                       00231000
**********************************************************************  00232000
* SELECT COUNT STATEMENT BY PRIMARY KEY:                                00233000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            00234000
**********************************************************************  00235000
*                                                                       00236000
SELKY0   DS    0H                                                       00237000
         USING SELKY0,12               ESTABLISH BASE REGISTER          00238000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00239000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00240000
         BALR  14,15                   MOVE REQUESTED DATA              00241000
         EXEC  SQL SELECT                                              *00242000
                   COUNT(*)                                            *00243000
                 INTO                                                  *00244000
                   :SQWINTHV                                           *00245000
                 FROM S00                                              *00246000
                 WHERE                                                 *00247000
                   INST_NBR = :INST AND                                *00248000
                   RECORD_NBR = :RECNBR                                *00249000
                 FETCH FIRST 1 ROW ONLY                                 00250000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00251000
         CLC   SQLCODE,=F'0'           GOOD RETURN FROM SQL?            00252000
         BNER  14                      NO - RETURN TO CALLER            00253000
         CLC   SQWINTHV,=F'0'          ANY ROWS MATCH WHERE CLAUSE?     00254000
         BNER  14                      YES - RETURN ZERO SQLCODE        00255000
         MVC   SQLCODE,=F'+100'        SET SQLCODE TO ROW NOT FOUND     00256000
         BR    14                      RETURN TO CALLER                 00257000
         LTORG                                                          00258000
*                                                                       00259000
**********************************************************************  00260000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY    00261000
* KEY:                                                                  00262000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00263000
*     AND GET-NEXT-LOCK VERBS.                                          00264000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00265000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00266000
**********************************************************************  00267000
*                                                                       00268000
SELXC0   DS    0H                                                       00269000
         USING SELXC0,12               ESTABLISH BASE REGISTER          00270000
         MVI   SQWKMRP,X'86'           SET HOST KEY 0 & CURSOR POINTER  00271000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00272000
         BALR  14,15                   MOVE DATA & SET CURSOR POINTER   00273000
         LA    12,SELXC0P              LOAD VECTOR TABLE ADDRESS        00274000
         AH    12,SQWCSRSP             COMPUTE POINTER TO OPEN ROUTINE  00275000
         L     12,0(12)                LOAD OPEN ROUTINE ADDRESS        00276000
         BR    12                      GO TO CURSOR OPEN ROUTINE        00277000
SELXC0P  DC    A(SELGE001)                                              00278000
         DC    (KY0COLMS-01)A(0)                                        00279000
         DC    A(SELGE002)                                              00280000
         LTORG                                                          00281000
*                                                                       00282000
SELGE001 DS    0H                                                       00283000
         USING SELGE001,12             ESTABLISH BASE REGISTER          00284000
         EXEC  SQL DECLARE S00GE001 CURSOR                             *00285000
               WITH ROWSET POSITIONING                                 *00286000
               FOR SELECT                                              *00287000
                   INST_NBR,                                           *00288000
                   RECORD_NBR,                                         *00289000
                   AUDIT_DATE,                                         *00290000
                   AUDIT_TIME,                                         *00291000
                   AUDIT_USER,                                         *00292000
                   AUDIT_ORG,                                          *00293000
                   ACCT_LIMIT_OPT,                                     *00294000
                   CUST_LIMIT_OPT,                                     *00295000
                   MAX_EXPIRE_DAY,                                     *00296000
                   ADD_INC_OPT,                                        *00297000
                   SEC_SVC_OPT,                                        *00298000
                   VALID_OPT,                                          *00299000
                   MAX_RETN_DAYS,                                      *00300000
                   MAX_EXTN,                                           *00301000
                   RATE_OPTION,                                        *00302000
                   ENTERPRISE_OPT,                                     *00303000
                   ENTERPRISE_INST,                                    *00304000
                   DUP_INC_OPT,                                        *00305000
                   DUP_INC_RULE,                                       *00306000
                   MODEL_LIMIT,                                        *00307000
                   ACCOUNT_LIMIT,                                      *00308000
                   EVENT_HST_OPT,                                      *00309000
                   EVENT_HST_DAYS                                      *00310000
                 FROM S00                                              *00311000
                 WHERE                                                 *00312000
                   INST_NBR = :INST AND                                *00313000
                   RECORD_NBR >=                                       *00314000
                     :RECNBR                                           *00315000
                 ORDER BY RECORD_NBR ASC                               *00316000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00317000
         EXEC  SQL OPEN S00GE001                                        00318000
         MVC   SQWCSRCA,=A(CLSGE001)   SET CURSOR CLOSE ROUTINE ADDRESS 00319000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00320000
         BR    14                      RETURN TO CALLER                 00321000
         LTORG                                                          00322000
*                                                                       00323000
SELGE002 DS    0H                                                       00324000
         USING SELGE002,12             ESTABLISH BASE REGISTER          00325000
         EXEC  SQL DECLARE S00GE002 CURSOR                             *00326000
               WITH ROWSET POSITIONING                                 *00327000
               FOR SELECT                                              *00328000
                   INST_NBR,                                           *00329000
                   RECORD_NBR,                                         *00330000
                   AUDIT_DATE,                                         *00331000
                   AUDIT_TIME,                                         *00332000
                   AUDIT_USER,                                         *00333000
                   AUDIT_ORG,                                          *00334000
                   ACCT_LIMIT_OPT,                                     *00335000
                   CUST_LIMIT_OPT,                                     *00336000
                   MAX_EXPIRE_DAY,                                     *00337000
                   ADD_INC_OPT,                                        *00338000
                   SEC_SVC_OPT,                                        *00339000
                   VALID_OPT,                                          *00340000
                   MAX_RETN_DAYS,                                      *00341000
                   MAX_EXTN,                                           *00342000
                   RATE_OPTION,                                        *00343000
                   ENTERPRISE_OPT,                                     *00344000
                   ENTERPRISE_INST,                                    *00345000
                   DUP_INC_OPT,                                        *00346000
                   DUP_INC_RULE,                                       *00347000
                   MODEL_LIMIT,                                        *00348000
                   ACCOUNT_LIMIT,                                      *00349000
                   EVENT_HST_OPT,                                      *00350000
                   EVENT_HST_DAYS                                      *00351000
                 FROM S00                                              *00352000
                 WHERE                                                 *00353000
                   INST_NBR >=                                         *00354000
                     :INST                                             *00355000
                 ORDER BY INST_NBR ASC,                                *00356000
                   RECORD_NBR ASC                                      *00357000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00358000
         EXEC  SQL OPEN S00GE002                                        00359000
         MVC   SQWCSRCA,=A(CLSGE002)   SET CURSOR CLOSE ROUTINE ADDRESS 00360000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00361000
         BR    14                      RETURN TO CALLER                 00362000
         LTORG                                                          00363000
*                                                                       00364000
**********************************************************************  00365000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY KEY:          00366000
*   THIS ROUTINE HANDLES PRIMARY KEY SEQUENTIAL CURSORS.                00367000
**********************************************************************  00368000
*                                                                       00369000
NXTXC0   DS    0H                                                       00370000
         USING NXTXC0,12               ESTABLISH BASE REGISTER          00371000
         MVI   SQWKMRP,X'87'           CLOSE CURSOR & SET HOST KEY 0    00372000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00373000
         BALR  14,15                   MOVE REQUESTED DATA              00374000
         LH    1,SQWCSRSP              LOAD CURRENT CURSOR POINTER      00375000
         LA    1,4(1)                  INCREMENT TO NEXT CURSOR         00376000
         STH   1,SQWCSRSP              SAVE POINTER FOR NEXT CALL       00377000
         LA    12,NXTXC0P(1)           LOAD POINTER TO NEXT CURSOR      00378000
         L     12,0(12)                LOAD CURSOR ROUTINE ADDRESS      00379000
         BR    12                      GO TO CURRENT CURSOR ROUTINE     00380000
NXTXC0P  DC    A(0)                                                     00381000
         DC    A(NXTGT002)                                              00382000
         DC    A(NXTGT099)                                              00383000
         DC    A(NXTGT099)                                              00384000
NXTGT099 LA    0,4                     LOAD VALUE 4 IN REGISTER 0       00385000
         SR    1,0                     ADJUST BACK TO CURRENT POINTER   00386000
         STH   1,SQWCSRSP              SAVE POINTER FOR CURSOR CLOSE    00387000
         LA    0,100                   LOAD VALUE 100 IN REGISTER 0     00388000
         ST    0,SQLCODE               SET SQLCODE TO NO MORE ROWS      00389000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00390000
         BR    14                      RETURN TO SQI                    00391000
         LTORG                                                          00392000
*                                                                       00393000
NXTGT002 DS    0H                                                       00394000
         USING NXTGT002,12             ESTABLISH BASE REGISTER          00395000
         EXEC  SQL DECLARE S00GT002 CURSOR                             *00396000
               WITH ROWSET POSITIONING                                 *00397000
               FOR SELECT                                              *00398000
                   INST_NBR,                                           *00399000
                   RECORD_NBR,                                         *00400000
                   AUDIT_DATE,                                         *00401000
                   AUDIT_TIME,                                         *00402000
                   AUDIT_USER,                                         *00403000
                   AUDIT_ORG,                                          *00404000
                   ACCT_LIMIT_OPT,                                     *00405000
                   CUST_LIMIT_OPT,                                     *00406000
                   MAX_EXPIRE_DAY,                                     *00407000
                   ADD_INC_OPT,                                        *00408000
                   SEC_SVC_OPT,                                        *00409000
                   VALID_OPT,                                          *00410000
                   MAX_RETN_DAYS,                                      *00411000
                   MAX_EXTN,                                           *00412000
                   RATE_OPTION,                                        *00413000
                   ENTERPRISE_OPT,                                     *00414000
                   ENTERPRISE_INST,                                    *00415000
                   DUP_INC_OPT,                                        *00416000
                   DUP_INC_RULE,                                       *00417000
                   MODEL_LIMIT,                                        *00418000
                   ACCOUNT_LIMIT,                                      *00419000
                   EVENT_HST_OPT,                                      *00420000
                   EVENT_HST_DAYS                                      *00421000
                 FROM S00                                              *00422000
                 WHERE                                                 *00423000
                   INST_NBR >                                          *00424000
                     :INST                                             *00425000
                 ORDER BY INST_NBR ASC,                                *00426000
                   RECORD_NBR ASC                                      *00427000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00428000
         EXEC  SQL OPEN S00GT002                                        00429000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            00430000
         BNZ   *+14                    NO - RETURN ERROR                00431000
         MVC   SQWCSRCA,=A(CLSGT002)   SET CURSOR CLOSE ROUTINE ADDRESS 00432000
         L     12,SQWCSRRA             LOAD RETURN ADDRESS              00433000
         BR    12                      RETURN TO FETCH ROUTINE          00434000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00435000
         BR    14                      RETURN TO CALLER                 00436000
         LTORG                                                          00437000
*                                                                       00438000
**********************************************************************  00439000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:               00440000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              00441000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          00442000
*     RETRIEVE THE ACTUAL ROW.                                          00443000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             00444000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                00445000
**********************************************************************  00446000
*                                                                       00447000
FETDC0   DS    0H                                                       00448000
         USING FETDC0,12               ESTABLISH BASE REGISTER          00449000
         MVC   SQWCSRRA,=A(FETDC0)     SET RETURN ROUTINE ADDRESS       00450000
         MVC   SQWCSRBR,=A(NXTXC0)     SET CURSOR ROUTINE ADDRESS       00451000
         MVC   SQWRSFVC,=AL2(TBLCOLMS) FETCH ALL COLUMNS IN TABLE       00452000
         LH    1,SQWCSRSP              LOAD CURRENT CURSOR POINTER      00453000
         LA    12,FETDC0P(1)           LOAD POINTER TO FETCH ROUTINE    00454000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       00455000
         BR    12                      GO TO CURRENT FETCH ROUTINE      00456000
FETDC0P  DC    A(FETGE001)                                              00457000
         DC    A(FETGT002)                                              00458000
         DC    A(FETGE002)                                              00459000
         DC    A(0)                                                     00460000
         LTORG                                                          00461000
*                                                                       00462000
**********************************************************************  00463000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        00464000
* THE PRIMARY KEY:                                                      00465000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           00466000
*     VERBS.                                                            00467000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          00468000
*     RETRIEVE THE ACTUAL ROW.                                          00469000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    00470000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             00471000
*     WILL BE THRU THE UPDATE CURSOR.                                   00472000
**********************************************************************  00473000
*                                                                       00474000
FETKC0   DS    0H                                                       00475000
         USING FETKC0,12               ESTABLISH BASE REGISTER          00476000
         MVC   SQWCSRRA,=A(FETKC0)     SET RETURN ROUTINE ADDRESS       00477000
         MVC   SQWCSRBR,=A(NXTXC0)     SET CURSOR ROUTINE ADDRESS       00478000
         MVC   SQWRSFVC,=AL2(KY0COLMS) ONLY FETCH KEY COLUMNS           00479000
         LH    1,SQWCSRSP              LOAD CURRENT CURSOR POINTER      00480000
         LA    12,FETKC0P(1)           LOAD POINTER TO FETCH ROUTINE    00481000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       00482000
         BR    12                      GO TO CURRENT FETCH ROUTINE      00483000
FETKC0P  DC    A(FETGE001)                                              00484000
         DC    A(FETGT002)                                              00485000
         DC    A(FETGE002)                                              00486000
         DC    A(0)                                                     00487000
         LTORG                                                          00488000
*                                                                       00489000
FETGE001 DS    0H                                                       00490000
         USING FETGE001,12             ESTABLISH BASE REGISTER          00491000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    00492000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00493000
         BALR  14,15                   MOVE REQUESTED DATA              00494000
         EXEC  SQL FETCH NEXT ROWSET FROM S00GE001                     *00495000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             00496000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   00497000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00498000
         BALR  14,15                   MOVE REQUESTED DATA              00499000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00500000
         BR    14                      RETURN TO CALLER                 00501000
         LTORG                                                          00502000
*                                                                       00503000
FETGE002 DS    0H                                                       00504000
         USING FETGE002,12             ESTABLISH BASE REGISTER          00505000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    00506000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00507000
         BALR  14,15                   MOVE REQUESTED DATA              00508000
         EXEC  SQL FETCH NEXT ROWSET FROM S00GE002                     *00509000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             00510000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   00511000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00512000
         BALR  14,15                   MOVE REQUESTED DATA              00513000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00514000
         BR    14                      RETURN TO CALLER                 00515000
         LTORG                                                          00516000
*                                                                       00517000
FETGT002 DS    0H                                                       00518000
         USING FETGT002,12             ESTABLISH BASE REGISTER          00519000
         MVI   SQWKMRP,X'04'           CHECK FOR MORE ROWS IN ROWSET    00520000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00521000
         BALR  14,15                   MOVE REQUESTED DATA              00522000
         EXEC  SQL FETCH NEXT ROWSET FROM S00GT002                     *00523000
               FOR :SQWRSRMX ROWS INTO DESCRIPTOR :SQDSQLDA             00524000
         MVI   SQWKMRP,X'05'           MOVE HOST ARRAY DATA TO RECORD   00525000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00526000
         BALR  14,15                   MOVE REQUESTED DATA              00527000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00528000
         BR    14                      RETURN TO CALLER                 00529000
         LTORG                                                          00530000
*                                                                       00531000
**********************************************************************  00532000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:                    00533000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          00534000
*     AND GET-NEXT-LOCK VERBS.                                          00535000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      00536000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   00537000
**********************************************************************  00538000
*                                                                       00539000
CLSXC0   DS    0H                                                       00540000
         USING CLSXC0,12               ESTABLISH BASE REGISTER          00541000
         L     12,SQWCSRCA             SET CURSOR CLOSE ROUTINE ADDRESS 00542000
         XC    SQWCSRCA,SQWCSRCA       CLEAR CURSOR CLOSE ROUTINE ADDR  00543000
         BR    12                      GO TO CURSOR CLOSE ROUTINE       00544000
         LTORG                                                          00545000
*                                                                       00546000
CLSGE001 DS    0H                                                       00547000
         USING CLSGE001,12             ESTABLISH BASE REGISTER          00548000
         EXEC  SQL CLOSE S00GE001                                       00549000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00550000
         BR    14                      RETURN TO CALLER                 00551000
         LTORG                                                          00552000
*                                                                       00553000
CLSGE002 DS    0H                                                       00554000
         USING CLSGE002,12             ESTABLISH BASE REGISTER          00555000
         EXEC  SQL CLOSE S00GE002                                       00556000
         L     14,SQW@RET                                               00557000
         BR    14                                                       00558000
         LTORG                                                          00559000
*                                                                       00560000
CLSGT002 DS    0H                                                       00561000
         USING CLSGT002,12             ESTABLISH BASE REGISTER          00562000
         EXEC  SQL CLOSE S00GT002                                       00563000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00564000
         BR    14                      RETURN TO CALLER                 00565000
         LTORG                                                          00566000
*                                                                       00567000
**********************************************************************  00568000
* ALTERNATE KEY 1 NOT DEFINED                                           00569000
**********************************************************************  00570000
*                                                                       00571000
SELIN1   DS    0H                                                       00572000
SELKY1   DS    0H                                                       00573000
SELXC1   DS    0H                                                       00574000
FETDC1   DS    0H                                                       00575000
FETKC1   DS    0H                                                       00576000
CLSXC1   DS    0H                                                       00577000
         DC    X'00DEAD01'             FORCE S0C1 ABEND                 00578000
*                                                                       00579000
**********************************************************************  00580000
* ALTERNATE KEY 2 NOT DEFINED                                           00581000
**********************************************************************  00582000
*                                                                       00583000
SELIN2   DS    0H                                                       00584000
SELKY2   DS    0H                                                       00585000
SELXC2   DS    0H                                                       00586000
FETDC2   DS    0H                                                       00587000
FETKC2   DS    0H                                                       00588000
CLSXC2   DS    0H                                                       00589000
         DC    X'00DEAD02'             FORCE S0C1 ABEND                 00590000
*                                                                       00591000
**********************************************************************  00592000
* ALTERNATE KEY 3 NOT DEFINED                                           00593000
**********************************************************************  00594000
*                                                                       00595000
SELIN3   DS    0H                                                       00596000
SELKY3   DS    0H                                                       00597000
SELXC3   DS    0H                                                       00598000
FETDC3   DS    0H                                                       00599000
FETKC3   DS    0H                                                       00600000
CLSXC3   DS    0H                                                       00601000
         DC    X'00DEAD03'             FORCE S0C1 ABEND                 00602000
*                                                                       00603000
         DS    0H                      END OF SQL STATEMENTS            00604000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   00605000
*                                                                       00606000
**********************************************************************  00607000
* DUMMY ENTRY POINT DSNHLI                                              00608000
**********************************************************************  00609000
*                                                                       00610000
         ENTRY DSNHLI                                                   00611000
DSNHLI   DS    0H                                                       00612000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       00613000
         BR    15                      BRANCH TO ATTACH FACILITY        00614000
*                                                                       00615000
**********************************************************************  00616000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  00617000
**********************************************************************  00618000
*                                                                       00619000
* CONVTAB1 TABLE ENTRY FORMAT IS:                                       00620000
*        DC    H'RRRR',H'VVVV',H'LLLL',X'KK',X'DD'                      00621000
* OR:                                                                   00622000
*        DC    H'RRRR',H'VVVV',X'ZZPP',X'KK',X'DD'                      00623000
* WHERE:                                                                00624000
*   RRRR = RECORD AREA OFFSET                                           00625000
*   VVVV = HOST VARIABLE AREA OFFSET                                    00626000
*   LLLL = HALFWORD LENGTH TO MOVE                                      00627000
*   ZZPP = CONVERT ZONED/PACKED LENGTHS (MINUS 1)                       00628000
*   KK   = KEY FIELD MASK:                                              00629000
*            80 = KEY 0 FIELD                                           00630000
*            40 = KEY 1 FIELD                                           00631000
*            20 = KEY 2 FIELD                                           00632000
*            10 = KEY 3 FIELD                                           00633000
*   DD   = DATA FIELD MASK:                                             00634000
*            80 = RECORD FIELD IS PACKED                                00635000
*            40 = HOST VARIABLE IS PACKED                               00636000
*            20 = NULLABLE FIELD                                        00637000
*            01 = DATE FIELD                                            00638000
*            02 = TIME FIELD                                            00639000
*                                                                       00640000
CONVTAB1 DS    0H                      RECORD/HOST VARIABLE CONVERSIONS 00641000
         DC    H'0000',H'0000',H'0008',X'80',X'00'                      00642000
         DC    H'0008',H'0008',H'0054',X'00',X'00'                      00643000
         DC    8X'FF'                                                   00644000
*                                                                       00645000
* CONVTAB2 TABLE COLUMNS ENTRY FORMAT IS:                               00646000
*        DC    H'RRRR',H'LLLL',H'TTTT'                                  00647000
* OR:                                                                   00648000
*        DC    H'RRRR',X'PPSS',H'TTTT'                                  00649000
* WHERE:                                                                00650000
*   RRRR = RECORD AREA OFFSET                                           00651000
*   LLLL = HALFWORD LENGTH OF HOST VARIABLE (NON-DECIMAL DATA TYPE)     00652000
*   PPSS = PRECISION AND SCALE (DECIMAL DATA TYPE)                      00653000
*   TTTT = SQLDA DATA TYPE:                                             00654000
*            452 = CHAR       453 = CHAR NULLABLE                       00655000
*            484 = DECIMAL    485 = DECIMAL NULLABLE                    00656000
*            496 = INTEGER    497 = INTEGER NULLABLE                    00657000
*            500 = SMALLINT   501 = SMALLINT NULLABLE                   00658000
*                                                                       00659000
CONVTAB2 DS    0F                      RECORD DATA ATTRIBUTE TABLE      00660000
         DC    AL4(TBLCOLMS*44+16)     TOTAL SQLDA SIZE                 00661000
         DC    AL2(TBLCOLMS)           NUMBER OF COLUMNS IN TABLE       00662000
         DC    AL2(KY0COLMS)           NUMBER OF PRIMARY KEY COLUMNS    00663000
         DC    AL2(KY1COLMS)           NUMBER OF ALT1 KEY COLUMNS       00664000
         DC    AL2(KY2COLMS)           NUMBER OF ALT2 KEY COLUMNS       00665000
         DC    AL2(KY3COLMS)           NUMBER OF ALT3 KEY COLUMNS       00666000
         DC    AL2(00062)              SUMMATION OF ALL COLUMN LENGTHS  00667000
         DC    AL2(01000)              PRIMARY MULTIROW FETCH ARRAY     00668000
         DC    AL2(00000)              ALT 1 MULTIROW FETCH ARRAY       00669000
         DC    AL2(00000)              ALT 2 MULTIROW FETCH ARRAY       00670000
         DC    AL2(00000)              ALT 3 MULTIROW FETCH ARRAY       00671000
         DC    AL2(00000)              PRIMARY KEY LOW VALUE LEVEL      00672000
         DC    AL2(00000)              ALT 1 KEY LOW VALUE LEVEL        00673000
         DC    AL2(00000)              ALT 2 KEY LOW VALUE LEVEL        00674000
         DC    AL2(00000)              ALT 3 KEY LOW VALUE LEVEL        00675000
CONVDATA DS    0H                                                       00676000
         DC    H'0000',H'0004',H'452'  INST_NBR                         00677000
         DC    H'0004',H'0004',H'452'  RECORD_NBR                       00678000
KEYCOLMS EQU   (*-CONVDATA)/6          NUMBER OF KEY COLUMNS IN TABLE   00679000
         DC    H'0008',X'0900',H'484'  AUDIT_DATE                       00680000
         DC    H'0013',X'0900',H'484'  AUDIT_TIME                       00681000
         DC    H'0018',H'0008',H'452'  AUDIT_USER                       00682000
         DC    H'0026',H'0006',H'452'  AUDIT_ORG                        00683000
         DC    H'0032',H'0001',H'452'  ACCT_LIMIT_OPT                   00684000
         DC    H'0033',H'0001',H'452'  CUST_LIMIT_OPT                   00685000
         DC    H'0034',H'0003',H'452'  MAX_EXPIRE_DAY                   00686000
         DC    H'0037',H'0001',H'452'  ADD_INC_OPT                      00687000
         DC    H'0038',H'0001',H'452'  SEC_SVC_OPT                      00688000
         DC    H'0039',H'0001',H'452'  VALID_OPT                        00689000
         DC    H'0040',H'0003',H'452'  MAX_RETN_DAYS                    00690000
         DC    H'0043',H'0002',H'452'  MAX_EXTN                         00691000
         DC    H'0045',H'0001',H'452'  RATE_OPTION                      00692000
         DC    H'0046',H'0001',H'452'  ENTERPRISE_OPT                   00693000
         DC    H'0047',H'0004',H'452'  ENTERPRISE_INST                  00694000
         DC    H'0051',H'0001',H'452'  DUP_INC_OPT                      00695000
         DC    H'0052',H'0001',H'452'  DUP_INC_RULE                     00696000
         DC    H'0053',H'0002',H'452'  MODEL_LIMIT                      00697000
         DC    H'0055',H'0003',H'452'  ACCOUNT_LIMIT                    00698000
         DC    H'0058',H'0001',H'452'  EVENT_HST_OPT                    00699000
         DC    H'0059',H'0003',H'452'  EVENT_HST_DAYS                   00700000
TBLCOLMS EQU   (*-CONVDATA)/6          NUMBER OF COLUMNS IN TABLE       00701000
KY0COLMS EQU   00002                   NUMBER OF PRIMARY KEY COLUMNS    00702000
KY1COLMS EQU   00000                   NUMBER OF ALT 1 KEY COLUMNS      00703000
KY2COLMS EQU   00000                   NUMBER OF ALT 2 KEY COLUMNS      00704000
KY3COLMS EQU   00000                   NUMBER OF ALT 3 KEY COLUMNS      00705000
*                                                                       00706000
         LTORG                                                          00707000
         END                                                            00708000
