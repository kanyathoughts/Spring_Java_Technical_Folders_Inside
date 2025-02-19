**********************************************************************  00001000
*                                                                       00002000
*  S03UDB .... STATIC SQL UPDATE MODULE                                 00003000
*                                                                       00004000
*  CREATION DATE: 04/25/16                                              00005000
*                                                                       00006000
*  FUNCTIONAL DESCRIPTION: THIS PROGRAM CONTAINS THE STATIC SQL         00007000
*  VECTORS REQUIRED TO SUPPORT I/O TO THE S03 TABLE.  IT IS LOADED      00008000
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
RATETABL DS    CL6                                                      00052000
EFFDATE  DS    CL8                                                      00053000
AUDDATE  DS    PL5'0.'                                                  00054000
AUDTIME  DS    PL5'0.'                                                  00055000
AUDUSER  DS    CL8                                                      00056000
AUDORG   DS    CL6                                                      00057000
BALOPT   DS    CL1                                                      00058000
RATEOPT  DS    CL1                                                      00059000
BALORDR  DS    CL2                                                      00060000
TERMBAL  DS    CL6                                                      00061000
RATEAMT1 DS    PL5'0.'                                                  00062000
TERM1    DS    PL3'0.'                                                  00063000
RATPCT1  DS    PL5'0.000000000'                                         00064000
RATEAMT2 DS    PL5'0.'                                                  00065000
TERM2    DS    PL3'0.'                                                  00066000
RATPCT2  DS    PL5'0.000000000'                                         00067000
RATEAMT3 DS    PL5'0.'                                                  00068000
TERM3    DS    PL3'0.'                                                  00069000
RATPCT3  DS    PL5'0.000000000'                                         00070000
RATEAMT4 DS    PL5'0.'                                                  00071000
TERM4    DS    PL3'0.'                                                  00072000
RATPCT4  DS    PL5'0.000000000'                                         00073000
RATEAMT5 DS    PL5'0.'                                                  00074000
TERM5    DS    PL3'0.'                                                  00075000
RATPCT5  DS    PL5'0.000000000'                                         00076000
RATEAMT6 DS    PL5'0.'                                                  00077000
TERM6    DS    PL3'0.'                                                  00078000
RATPCT6  DS    PL5'0.000000000'                                         00079000
RATEAMT7 DS    PL5'0.'                                                  00080000
TERM7    DS    PL3'0.'                                                  00081000
RATPCT7  DS    PL5'0.000000000'                                         00082000
RATEAMT8 DS    PL5'0.'                                                  00083000
TERM8    DS    PL3'0.'                                                  00084000
RATPCT8  DS    PL5'0.000000000'                                         00085000
RATEAMT9 DS    PL5'0.'                                                  00086000
TERM9    DS    PL3'0.'                                                  00087000
RATPCT9  DS    PL5'0.000000000'                                         00088000
RATEAT10 DS    PL5'0.'                                                  00089000
TERM10   DS    PL3'0.'                                                  00090000
RATPCT10 DS    PL5'0.000000000'                                         00091000
RATAMT11 DS    PL5'0.'                                                  00092000
TERM11   DS    PL3'0.'                                                  00093000
RATPCT11 DS    PL5'0.000000000'                                         00094000
RATAMT12 DS    PL5'0.'                                                  00095000
TERM12   DS    PL3'0.'                                                  00096000
RATPCT12 DS    PL5'0.000000000'                                         00097000
RATAMT13 DS    PL5'0.'                                                  00098000
TERM13   DS    PL3'0.'                                                  00099000
RATPCT13 DS    PL5'0.000000000'                                         00100000
RATAMT14 DS    PL5'0.'                                                  00101000
TERM14   DS    PL3'0.'                                                  00102000
RATPCT14 DS    PL5'0.000000000'                                         00103000
RATAMT15 DS    PL5'0.'                                                  00104000
TERM15   DS    PL3'0.'                                                  00105000
RATPCT15 DS    PL5'0.000000000'                                         00106000
*                                                                       00107000
         ORG   ASMREC+(2000-L'SQWADATA) POINT TO ADDITIONAL DATA        00108000
SQWADATA DS    0CL400                  ADDITIONAL DATA PASSED TO MODULE 00109000
SQWSEGF  DS    CL102                   SEGMENTED FROM KEY VALUE         00110000
SQWSEGT  DS    CL102                   SEGMENTED TO KEY VALUE           00111000
SQWAUDIT DS    CL99                    CALLERS AUDIT DATA               00112000
         DS    CL97                    RESERVED                         00113000
*                                                                       00114000
INDVARS  DS    0H                      NULL INDICATOR VARIABLES         00115000
INDVARX  DS    0H                                                       00116000
INDVARL  EQU   INDVARX-INDVARS         NULL INDICATOR AREA LENGTH       00117000
*                                                                       00118000
*                                                                       00119000
**********************************************************************  00120000
* PROGRAM TABLE HEADER SECTION:                                         00121000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00122000
**********************************************************************  00123000
*                                                                       00124000
S03UDB   CSECT                         PROGRAM TABLE SECTION            00125000
S03UDB   AMODE ANY                                                      00126000
S03UDB   RMODE ANY                                                      00127000
         DC    CL8'S03UDB  '           PROGRAM ID                       00128000
         DC    CL1' '                                                   00129000
         DC    CL8'&SYSDATE'           ASSEMBLY DATE                    00130000
         DC    CL1' '                                                   00131000
         DC    CL5'&SYSTIME'           ASSEMBLY TIME                    00132000
         DC    CL1' '                                                   00133000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00134000
         DC    5A(0)                   RESERVED                         00135000
         DC    AL2(0)                  RESERVED                         00136000
         DC    AL2(INDVARL)            NULL INDICATOR AREA LENGTH       00137000
         DC    A(CONVTAB1)             RECORD/HOST CONVERSION TABLE     00138000
         DC    A(0)                    SQLDA DATA TYPE/LENGTH TABLE     00139000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00140000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00141000
         DC    CL29'WWW.INFOR.COM                '                      00141001
*                                                                       00142000
**********************************************************************  00143000
* STATEMENT TABLE SECTION:                                              00144000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00145000
**********************************************************************  00146000
*                                                                       00147000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00148000
STM#TAB  AMODE ANY                                                      00149000
STM#TAB  RMODE ANY                                                      00150000
         DC    4A(0)                   RDB MODULE VECTORS               00151000
         DC    A(SELUC0)               SELECT UPDATE CURSOR (KEY 0)     00152000
         DC    A(SELUC1)               SELECT UPDATE CURSOR (KEY 1)     00153000
         DC    A(SELUC2)               SELECT UPDATE CURSOR (KEY 2)     00154000
         DC    A(SELUC3)               SELECT UPDATE CURSOR (KEY 3)     00155000
         DC    A(FETUC0)               FETCH UPDATE CURSOR (KEY 0)      00156000
         DC    A(FETUC1)               FETCH UPDATE CURSOR (KEY 1)      00157000
         DC    A(FETUC2)               FETCH UPDATE CURSOR (KEY 2)      00158000
         DC    A(FETUC3)               FETCH UPDATE CURSOR (KEY 3)      00159000
         DC    A(CLSUC0)               CLOSE UPDATE CURSOR (KEY 0)      00160000
         DC    A(CLSUC1)               CLOSE UPDATE CURSOR (KEY 1)      00161000
         DC    A(CLSUC2)               CLOSE UPDATE CURSOR (KEY 2)      00162000
         DC    A(CLSUC3)               CLOSE UPDATE CURSOR (KEY 3)      00163000
         DC    20A(0)                  RDB MODULE VECTORS               00164000
         DC    A(INSROW)               INSERT STATEMENT                 00165000
         DC    A(UPDUC0)               UPDATE STATEMENT (KEY 0)         00166000
         DC    A(UPDUC1)               UPDATE STATEMENT (KEY 1)         00167000
         DC    A(UPDUC2)               UPDATE STATEMENT (KEY 2)         00168000
         DC    A(UPDUC3)               UPDATE STATEMENT (KEY 3)         00169000
         DC    A(DELUC0)               DELETE STATEMENT (KEY 0)         00170000
         DC    A(DELUC1)               DELETE STATEMENT (KEY 1)         00171000
         DC    A(DELUC2)               DELETE STATEMENT (KEY 2)         00172000
         DC    A(DELUC3)               DELETE STATEMENT (KEY 3)         00173000
         DC    A(DELTBL)               DELETE ALL STATEMENT             00174000
         DC    4X'FF'                                                   00175000
*                                                                       00176000
**********************************************************************  00177000
* SQL STATEMENT SECTION:                                                00178000
*   THIS SECTION CONTAINS ALL THE STATIC SQL STATEMENTS REQUIRED        00179000
*     TO SUPPORT THIS TABLE.                                            00180000
*   THE INDICATED STATEMENTS MAY BE MODIFIED, AS LONG AS THE RESULTS    00181000
*     ARE EQUIVALENT.                                                   00182000
**********************************************************************  00183000
*                                                                       00184000
SQL#STMT CSECT                         SQL STATEMENT SECTION            00185000
SQL#STMT AMODE ANY                                                      00186000
SQL#STMT RMODE ANY                                                      00187000
         USING SQLDSECT,10,3           ADDRESS SQLDSECT                 00188000
         USING COM#AREA,11             ADDRESS COMMAREA                 00189000
*                                                                       00190000
**********************************************************************  00191000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY PRIMARY KEY:       00192000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00193000
*   THEY ARE ALSO USED AFTER A SUCCESSFUL SELECT SEQUENTIAL STATEMENT   00194000
*     FOR THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS.                      00195000
**********************************************************************  00196000
*                                                                       00197000
SELUC0   DS    0H                                                       00198000
         USING SELUC0,12               ESTABLISH BASE REGISTER          00199000
         MVI   SQWKMRP,X'80'           MOVE RECORD TO HOST FOR KEY 0    00200000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00201000
         BALR  14,15                   MOVE REQUESTED DATA              00202000
         B     *+6                     BRANCH AROUND ADCON              00203000
BASSEL0  DC    AL2(4096)                                                00204000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00205000
         AH    3,BASSEL0               ADD 4K                           00206000
         EXEC  SQL DECLARE S03UPD0 CURSOR                              *00207000
               FOR SELECT                                              *00208000
                   AUDIT_DATE,                                         *00209000
                   AUDIT_TIME,                                         *00210000
                   AUDIT_USER,                                         *00211000
                   AUDIT_ORG,                                          *00212000
                   BAL_OPT,                                            *00213000
                   RATE_OPTION,                                        *00214000
                   BAL_ORDER,                                          *00215000
                   TERM_BAL,                                           *00216000
                   RATE_AMT_1,                                         *00217000
                   TERM_1,                                             *00218000
                   RATE_PCT_1,                                         *00219000
                   RATE_AMT_2,                                         *00220000
                   TERM_2,                                             *00221000
                   RATE_PCT_2,                                         *00222000
                   RATE_AMT_3,                                         *00223000
                   TERM_3,                                             *00224000
                   RATE_PCT_3,                                         *00225000
                   RATE_AMT_4,                                         *00226000
                   TERM_4,                                             *00227000
                   RATE_PCT_4,                                         *00228000
                   RATE_AMT_5,                                         *00229000
                   TERM_5,                                             *00230000
                   RATE_PCT_5,                                         *00231000
                   RATE_AMT_6,                                         *00232000
                   TERM_6,                                             *00233000
                   RATE_PCT_6,                                         *00234000
                   RATE_AMT_7,                                         *00235000
                   TERM_7,                                             *00236000
                   RATE_PCT_7,                                         *00237000
                   RATE_AMT_8,                                         *00238000
                   TERM_8,                                             *00239000
                   RATE_PCT_8,                                         *00240000
                   RATE_AMT_9,                                         *00241000
                   TERM_9,                                             *00242000
                   RATE_PCT_9,                                         *00243000
                   RATE_AMT_10,                                        *00244000
                   TERM_10,                                            *00245000
                   RATE_PCT_10,                                        *00246000
                   RATE_AMT_11,                                        *00247000
                   TERM_11,                                            *00248000
                   RATE_PCT_11,                                        *00249000
                   RATE_AMT_12,                                        *00250000
                   TERM_12,                                            *00251000
                   RATE_PCT_12,                                        *00252000
                   RATE_AMT_13,                                        *00253000
                   TERM_13,                                            *00254000
                   RATE_PCT_13,                                        *00255000
                   RATE_AMT_14,                                        *00256000
                   TERM_14,                                            *00257000
                   RATE_PCT_14,                                        *00258000
                   RATE_AMT_15,                                        *00259000
                   TERM_15,                                            *00260000
                   RATE_PCT_15                                         *00261000
                 FROM S03                                              *00262000
                 WHERE                                                 *00263000
                   INST_NBR = :INST AND                                *00264000
                   RECORD_NBR = :RECNBR AND                            *00265000
                   RATE_TABLE = :RATETABL AND                          *00266000
                   EFFECTIVE_DATE = :EFFDATE                           *00267000
                 FOR UPDATE OF                                         *00268000
                   AUDIT_DATE,                                         *00269000
                   AUDIT_TIME,                                         *00270000
                   AUDIT_USER,                                         *00271000
                   AUDIT_ORG,                                          *00272000
                   BAL_OPT,                                            *00273000
                   RATE_OPTION,                                        *00274000
                   BAL_ORDER,                                          *00275000
                   TERM_BAL,                                           *00276000
                   RATE_AMT_1,                                         *00277000
                   TERM_1,                                             *00278000
                   RATE_PCT_1,                                         *00279000
                   RATE_AMT_2,                                         *00280000
                   TERM_2,                                             *00281000
                   RATE_PCT_2,                                         *00282000
                   RATE_AMT_3,                                         *00283000
                   TERM_3,                                             *00284000
                   RATE_PCT_3,                                         *00285000
                   RATE_AMT_4,                                         *00286000
                   TERM_4,                                             *00287000
                   RATE_PCT_4,                                         *00288000
                   RATE_AMT_5,                                         *00289000
                   TERM_5,                                             *00290000
                   RATE_PCT_5,                                         *00291000
                   RATE_AMT_6,                                         *00292000
                   TERM_6,                                             *00293000
                   RATE_PCT_6,                                         *00294000
                   RATE_AMT_7,                                         *00295000
                   TERM_7,                                             *00296000
                   RATE_PCT_7,                                         *00297000
                   RATE_AMT_8,                                         *00298000
                   TERM_8,                                             *00299000
                   RATE_PCT_8,                                         *00300000
                   RATE_AMT_9,                                         *00301000
                   TERM_9,                                             *00302000
                   RATE_PCT_9,                                         *00303000
                   RATE_AMT_10,                                        *00304000
                   TERM_10,                                            *00305000
                   RATE_PCT_10,                                        *00306000
                   RATE_AMT_11,                                        *00307000
                   TERM_11,                                            *00308000
                   RATE_PCT_11,                                        *00309000
                   RATE_AMT_12,                                        *00310000
                   TERM_12,                                            *00311000
                   RATE_PCT_12,                                        *00312000
                   RATE_AMT_13,                                        *00313000
                   TERM_13,                                            *00314000
                   RATE_PCT_13,                                        *00315000
                   RATE_AMT_14,                                        *00316000
                   TERM_14,                                            *00317000
                   RATE_PCT_14,                                        *00318000
                   RATE_AMT_15,                                        *00319000
                   TERM_15,                                            *00320000
                   RATE_PCT_15                                         *00321000
                 FETCH FIRST 1 ROW ONLY                                 00322000
         EXEC  SQL OPEN S03UPD0                                         00323000
         MVC   SQWCSUCA,=A(CLSUC0)     SET CURSOR CLOSE ROUTINE ADDRESS 00324000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00325000
         BR    14                      RETURN TO CALLER                 00326000
         LTORG                                                          00327000
*                                                                       00328000
**********************************************************************  00329000
* FETCH FROM UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                   00330000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00331000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00332000
*     THE ACTUAL ROW.                                                   00333000
**********************************************************************  00334000
*                                                                       00335000
FETUC0   DS    0H                                                       00336000
         USING FETUC0,12               ESTABLISH BASE REGISTER          00337000
         B     *+6                     BRANCH AROUND ADCON              00338000
BASFET0  DC    AL2(4096)                                                00339000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00340000
         AH    3,BASFET0               ADD 4K                           00341000
         EXEC  SQL FETCH S03UPD0                                       *00342000
                 INTO                                                  *00343000
                   :AUDDATE,                                           *00344000
                   :AUDTIME,                                           *00345000
                   :AUDUSER,                                           *00346000
                   :AUDORG,                                            *00347000
                   :BALOPT,                                            *00348000
                   :RATEOPT,                                           *00349000
                   :BALORDR,                                           *00350000
                   :TERMBAL,                                           *00351000
                   :RATEAMT1,                                          *00352000
                   :TERM1,                                             *00353000
                   :RATPCT1,                                           *00354000
                   :RATEAMT2,                                          *00355000
                   :TERM2,                                             *00356000
                   :RATPCT2,                                           *00357000
                   :RATEAMT3,                                          *00358000
                   :TERM3,                                             *00359000
                   :RATPCT3,                                           *00360000
                   :RATEAMT4,                                          *00361000
                   :TERM4,                                             *00362000
                   :RATPCT4,                                           *00363000
                   :RATEAMT5,                                          *00364000
                   :TERM5,                                             *00365000
                   :RATPCT5,                                           *00366000
                   :RATEAMT6,                                          *00367000
                   :TERM6,                                             *00368000
                   :RATPCT6,                                           *00369000
                   :RATEAMT7,                                          *00370000
                   :TERM7,                                             *00371000
                   :RATPCT7,                                           *00372000
                   :RATEAMT8,                                          *00373000
                   :TERM8,                                             *00374000
                   :RATPCT8,                                           *00375000
                   :RATEAMT9,                                          *00376000
                   :TERM9,                                             *00377000
                   :RATPCT9,                                           *00378000
                   :RATEAT10,                                          *00379000
                   :TERM10,                                            *00380000
                   :RATPCT10,                                          *00381000
                   :RATAMT11,                                          *00382000
                   :TERM11,                                            *00383000
                   :RATPCT11,                                          *00384000
                   :RATAMT12,                                          *00385000
                   :TERM12,                                            *00386000
                   :RATPCT12,                                          *00387000
                   :RATAMT13,                                          *00388000
                   :TERM13,                                            *00389000
                   :RATPCT13,                                          *00390000
                   :RATAMT14,                                          *00391000
                   :TERM14,                                            *00392000
                   :RATPCT14,                                          *00393000
                   :RATAMT15,                                          *00394000
                   :TERM15,                                            *00395000
                   :RATPCT15                                            00396000
         MVI   SQWKMRP,X'03'           MOVE HOST VARIABLES TO RECORD    00397000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00398000
         BALR  14,15                   MOVE REQUESTED DATA              00399000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00400000
         BR    14                      RETURN TO CALLER                 00401000
         LTORG                                                          00402000
*                                                                       00403000
**********************************************************************  00404000
* INSERT STATEMENT:                                                     00405000
*   THIS STATEMENT SUPPORTS THE PUT VERB.                               00406000
**********************************************************************  00407000
*                                                                       00408000
INSROW   DS    0H                                                       00409000
         USING INSROW,12               ESTABLISH BASE REGISTER          00410000
         MVI   SQWKMRP,X'01'           MOVE RECORD TO HOST VARIABLES    00411000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00412000
         BALR  14,15                   MOVE REQUESTED DATA              00413000
         B     *+6                     BRANCH AROUND ADCON              00414000
BASINS0  DC    AL2(4096)                                                00415000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00416000
         AH    3,BASINS0               ADD 4K                           00417000
         EXEC  SQL INSERT INTO S03                                     *00418000
                   (                                                   *00419000
                   INST_NBR,                                           *00420000
                   RECORD_NBR,                                         *00421000
                   RATE_TABLE,                                         *00422000
                   EFFECTIVE_DATE,                                     *00423000
                   AUDIT_DATE,                                         *00424000
                   AUDIT_TIME,                                         *00425000
                   AUDIT_USER,                                         *00426000
                   AUDIT_ORG,                                          *00427000
                   BAL_OPT,                                            *00428000
                   RATE_OPTION,                                        *00429000
                   BAL_ORDER,                                          *00430000
                   TERM_BAL,                                           *00431000
                   RATE_AMT_1,                                         *00432000
                   TERM_1,                                             *00433000
                   RATE_PCT_1,                                         *00434000
                   RATE_AMT_2,                                         *00435000
                   TERM_2,                                             *00436000
                   RATE_PCT_2,                                         *00437000
                   RATE_AMT_3,                                         *00438000
                   TERM_3,                                             *00439000
                   RATE_PCT_3,                                         *00440000
                   RATE_AMT_4,                                         *00441000
                   TERM_4,                                             *00442000
                   RATE_PCT_4,                                         *00443000
                   RATE_AMT_5,                                         *00444000
                   TERM_5,                                             *00445000
                   RATE_PCT_5,                                         *00446000
                   RATE_AMT_6,                                         *00447000
                   TERM_6,                                             *00448000
                   RATE_PCT_6,                                         *00449000
                   RATE_AMT_7,                                         *00450000
                   TERM_7,                                             *00451000
                   RATE_PCT_7,                                         *00452000
                   RATE_AMT_8,                                         *00453000
                   TERM_8,                                             *00454000
                   RATE_PCT_8,                                         *00455000
                   RATE_AMT_9,                                         *00456000
                   TERM_9,                                             *00457000
                   RATE_PCT_9,                                         *00458000
                   RATE_AMT_10,                                        *00459000
                   TERM_10,                                            *00460000
                   RATE_PCT_10,                                        *00461000
                   RATE_AMT_11,                                        *00462000
                   TERM_11,                                            *00463000
                   RATE_PCT_11,                                        *00464000
                   RATE_AMT_12,                                        *00465000
                   TERM_12,                                            *00466000
                   RATE_PCT_12,                                        *00467000
                   RATE_AMT_13,                                        *00468000
                   TERM_13,                                            *00469000
                   RATE_PCT_13,                                        *00470000
                   RATE_AMT_14,                                        *00471000
                   TERM_14,                                            *00472000
                   RATE_PCT_14,                                        *00473000
                   RATE_AMT_15,                                        *00474000
                   TERM_15,                                            *00475000
                   RATE_PCT_15                                         *00476000
                   )                                                   *00477000
                  VALUES                                               *00478000
                   (                                                   *00479000
                   :INST,                                              *00480000
                   :RECNBR,                                            *00481000
                   :RATETABL,                                          *00482000
                   :EFFDATE,                                           *00483000
                   :AUDDATE,                                           *00484000
                   :AUDTIME,                                           *00485000
                   :AUDUSER,                                           *00486000
                   :AUDORG,                                            *00487000
                   :BALOPT,                                            *00488000
                   :RATEOPT,                                           *00489000
                   :BALORDR,                                           *00490000
                   :TERMBAL,                                           *00491000
                   :RATEAMT1,                                          *00492000
                   :TERM1,                                             *00493000
                   :RATPCT1,                                           *00494000
                   :RATEAMT2,                                          *00495000
                   :TERM2,                                             *00496000
                   :RATPCT2,                                           *00497000
                   :RATEAMT3,                                          *00498000
                   :TERM3,                                             *00499000
                   :RATPCT3,                                           *00500000
                   :RATEAMT4,                                          *00501000
                   :TERM4,                                             *00502000
                   :RATPCT4,                                           *00503000
                   :RATEAMT5,                                          *00504000
                   :TERM5,                                             *00505000
                   :RATPCT5,                                           *00506000
                   :RATEAMT6,                                          *00507000
                   :TERM6,                                             *00508000
                   :RATPCT6,                                           *00509000
                   :RATEAMT7,                                          *00510000
                   :TERM7,                                             *00511000
                   :RATPCT7,                                           *00512000
                   :RATEAMT8,                                          *00513000
                   :TERM8,                                             *00514000
                   :RATPCT8,                                           *00515000
                   :RATEAMT9,                                          *00516000
                   :TERM9,                                             *00517000
                   :RATPCT9,                                           *00518000
                   :RATEAT10,                                          *00519000
                   :TERM10,                                            *00520000
                   :RATPCT10,                                          *00521000
                   :RATAMT11,                                          *00522000
                   :TERM11,                                            *00523000
                   :RATPCT11,                                          *00524000
                   :RATAMT12,                                          *00525000
                   :TERM12,                                            *00526000
                   :RATPCT12,                                          *00527000
                   :RATAMT13,                                          *00528000
                   :TERM13,                                            *00529000
                   :RATPCT13,                                          *00530000
                   :RATAMT14,                                          *00531000
                   :TERM14,                                            *00532000
                   :RATPCT14,                                          *00533000
                   :RATAMT15,                                          *00534000
                   :TERM15,                                            *00535000
                   :RATPCT15                                           *00536000
                   )                                                    00537000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00538000
         BR    14                      RETURN TO CALLER                 00539000
         LTORG                                                          00540000
*                                                                       00541000
**********************************************************************  00542000
* UPDATE STATEMENT BY PRIMARY KEY:                                      00543000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             00544000
**********************************************************************  00545000
*                                                                       00546000
UPDUC0   DS    0H                                                       00547000
         USING UPDUC0,12               ESTABLISH BASE REGISTER          00548000
         MVI   SQWKMRP,X'01'           MOVE RECORD TO HOST VARIABLES    00549000
         L     15,SQW@SQRX             LOAD INTERFACE ROUTINE ADDRESS   00550000
         BALR  14,15                   MOVE REQUESTED DATA              00551000
         B     *+6                     BRANCH AROUND ADCON              00552000
BASUPD0  DC    AL2(4096)                                                00553000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00554000
         AH    3,BASUPD0               ADD 4K                           00555000
         EXEC  SQL UPDATE S03                                          *00556000
                   SET                                                 *00557000
                     AUDIT_DATE = :AUDDATE,                            *00558000
                     AUDIT_TIME = :AUDTIME,                            *00559000
                     AUDIT_USER = :AUDUSER,                            *00560000
                     AUDIT_ORG = :AUDORG,                              *00561000
                     BAL_OPT = :BALOPT,                                *00562000
                     RATE_OPTION = :RATEOPT,                           *00563000
                     BAL_ORDER = :BALORDR,                             *00564000
                     TERM_BAL = :TERMBAL,                              *00565000
                     RATE_AMT_1 = :RATEAMT1,                           *00566000
                     TERM_1 = :TERM1,                                  *00567000
                     RATE_PCT_1 = :RATPCT1,                            *00568000
                     RATE_AMT_2 = :RATEAMT2,                           *00569000
                     TERM_2 = :TERM2,                                  *00570000
                     RATE_PCT_2 = :RATPCT2,                            *00571000
                     RATE_AMT_3 = :RATEAMT3,                           *00572000
                     TERM_3 = :TERM3,                                  *00573000
                     RATE_PCT_3 = :RATPCT3,                            *00574000
                     RATE_AMT_4 = :RATEAMT4,                           *00575000
                     TERM_4 = :TERM4,                                  *00576000
                     RATE_PCT_4 = :RATPCT4,                            *00577000
                     RATE_AMT_5 = :RATEAMT5,                           *00578000
                     TERM_5 = :TERM5,                                  *00579000
                     RATE_PCT_5 = :RATPCT5,                            *00580000
                     RATE_AMT_6 = :RATEAMT6,                           *00581000
                     TERM_6 = :TERM6,                                  *00582000
                     RATE_PCT_6 = :RATPCT6,                            *00583000
                     RATE_AMT_7 = :RATEAMT7,                           *00584000
                     TERM_7 = :TERM7,                                  *00585000
                     RATE_PCT_7 = :RATPCT7,                            *00586000
                     RATE_AMT_8 = :RATEAMT8,                           *00587000
                     TERM_8 = :TERM8,                                  *00588000
                     RATE_PCT_8 = :RATPCT8,                            *00589000
                     RATE_AMT_9 = :RATEAMT9,                           *00590000
                     TERM_9 = :TERM9,                                  *00591000
                     RATE_PCT_9 = :RATPCT9,                            *00592000
                     RATE_AMT_10 = :RATEAT10,                          *00593000
                     TERM_10 = :TERM10,                                *00594000
                     RATE_PCT_10 = :RATPCT10,                          *00595000
                     RATE_AMT_11 = :RATAMT11,                          *00596000
                     TERM_11 = :TERM11,                                *00597000
                     RATE_PCT_11 = :RATPCT11,                          *00598000
                     RATE_AMT_12 = :RATAMT12,                          *00599000
                     TERM_12 = :TERM12,                                *00600000
                     RATE_PCT_12 = :RATPCT12,                          *00601000
                     RATE_AMT_13 = :RATAMT13,                          *00602000
                     TERM_13 = :TERM13,                                *00603000
                     RATE_PCT_13 = :RATPCT13,                          *00604000
                     RATE_AMT_14 = :RATAMT14,                          *00605000
                     TERM_14 = :TERM14,                                *00606000
                     RATE_PCT_14 = :RATPCT14,                          *00607000
                     RATE_AMT_15 = :RATAMT15,                          *00608000
                     TERM_15 = :TERM15,                                *00609000
                     RATE_PCT_15 = :RATPCT15                           *00610000
                 WHERE CURRENT OF S03UPD0                               00611000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00612000
         BR    14                      RETURN TO CALLER                 00613000
         LTORG                                                          00614000
*                                                                       00615000
**********************************************************************  00616000
* DELETE STATEMENT BY PRIMARY KEY:                                      00617000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            00618000
**********************************************************************  00619000
*                                                                       00620000
DELUC0   DS    0H                                                       00621000
         USING DELUC0,12               ESTABLISH BASE REGISTER          00622000
         EXEC  SQL DELETE FROM S03                                     *00623000
                 WHERE CURRENT OF S03UPD0                               00624000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00625000
         BR    14                      RETURN TO CALLER                 00626000
         LTORG                                                          00627000
*                                                                       00628000
**********************************************************************  00629000
* DELETE ALL STATEMENT:                                                 00630000
*   THIS STATEMENT SUPPORTS THE DELETE-FILE VERB.                       00631000
**********************************************************************  00632000
*                                                                       00633000
DELTBL   DS    0H                                                       00634000
         USING DELTBL,12               ESTABLISH BASE REGISTER          00635000
         EXEC  SQL DELETE FROM S03                                      00636000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00637000
         BR    14                      RETURN TO CALLER                 00638000
         LTORG                                                          00639000
*                                                                       00640000
**********************************************************************  00641000
* CLOSE UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                        00642000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00643000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00644000
*     TO CLOSE THE UPDATE CURSOR.                                       00645000
**********************************************************************  00646000
*                                                                       00647000
CLSUC0   DS    0H                                                       00648000
         USING CLSUC0,12               ESTABLISH BASE REGISTER          00649000
         EXEC  SQL CLOSE S03UPD0                                        00650000
         XC    SQWCSUCA,SQWCSUCA       CLEAR CURSOR CLOSE ROUTINE ADDR  00651000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00652000
         BR    14                      RETURN TO CALLER                 00653000
         LTORG                                                          00654000
*                                                                       00655000
**********************************************************************  00656000
* ALTERNATE KEY 1 NOT DEFINED                                           00657000
**********************************************************************  00658000
*                                                                       00659000
SELUC1   DS    0H                                                       00660000
FETUC1   DS    0H                                                       00661000
UPDUC1   DS    0H                                                       00662000
DELUC1   DS    0H                                                       00663000
CLSUC1   DS    0H                                                       00664000
         DC    X'00DEAD01'             FORCE S0C1 ABEND                 00665000
*                                                                       00666000
**********************************************************************  00667000
* ALTERNATE KEY 2 NOT DEFINED                                           00668000
**********************************************************************  00669000
*                                                                       00670000
SELUC2   DS    0H                                                       00671000
FETUC2   DS    0H                                                       00672000
UPDUC2   DS    0H                                                       00673000
DELUC2   DS    0H                                                       00674000
CLSUC2   DS    0H                                                       00675000
         DC    X'00DEAD02'             FORCE S0C1 ABEND                 00676000
*                                                                       00677000
**********************************************************************  00678000
* ALTERNATE KEY 3 NOT DEFINED                                           00679000
**********************************************************************  00680000
*                                                                       00681000
SELUC3   DS    0H                                                       00682000
FETUC3   DS    0H                                                       00683000
UPDUC3   DS    0H                                                       00684000
DELUC3   DS    0H                                                       00685000
CLSUC3   DS    0H                                                       00686000
         DC    X'00DEAD03'             FORCE S0C1 ABEND                 00687000
*                                                                       00688000
         DS    0H                      END OF SQL STATEMENTS            00689000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   00690000
*                                                                       00691000
**********************************************************************  00692000
* DUMMY ENTRY POINT DSNHLI                                              00693000
**********************************************************************  00694000
*                                                                       00695000
         ENTRY DSNHLI                                                   00696000
DSNHLI   DS    0H                                                       00697000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       00698000
         BR    15                      BRANCH TO ATTACH FACILITY        00699000
*                                                                       00700000
**********************************************************************  00701000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  00702000
**********************************************************************  00703000
*                                                                       00704000
* CONVTAB1 TABLE ENTRY FORMAT IS:                                       00705000
*        DC    H'RRRR',H'VVVV',H'LLLL',X'KK',X'DD'                      00706000
* OR:                                                                   00707000
*        DC    H'RRRR',H'VVVV',X'ZZPP',X'KK',X'DD'                      00708000
* WHERE:                                                                00709000
*   RRRR = RECORD AREA OFFSET                                           00710000
*   VVVV = HOST VARIABLE AREA OFFSET                                    00711000
*   LLLL = HALFWORD LENGTH TO MOVE                                      00712000
*   ZZPP = CONVERT ZONED/PACKED LENGTHS (MINUS 1)                       00713000
*   KK   = KEY FIELD MASK:                                              00714000
*            80 = KEY 0 FIELD                                           00715000
*            40 = KEY 1 FIELD                                           00716000
*            20 = KEY 2 FIELD                                           00717000
*            10 = KEY 3 FIELD                                           00718000
*   DD   = DATA FIELD MASK:                                             00719000
*            80 = RECORD FIELD IS PACKED                                00720000
*            40 = HOST VARIABLE IS PACKED                               00721000
*            20 = NULLABLE FIELD                                        00722000
*            01 = DATE FIELD                                            00723000
*            02 = TIME FIELD                                            00724000
*                                                                       00725000
CONVTAB1 DS    0H                      RECORD/HOST VARIABLE CONVERSIONS 00726000
         DC    H'0000',H'0000',H'0022',X'80',X'00'                      00727000
         DC    H'0022',H'0022',H'0229',X'00',X'00'                      00728000
         DC    8X'FF'                                                   00729000
*                                                                       00730000
         LTORG                                                          00731000
         END                                                            00732000
