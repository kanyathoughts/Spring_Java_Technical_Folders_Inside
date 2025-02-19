* REL=S04IODB.10                                                        00001000
**********************************************************************  00002000
*                                                                       00003000
*  S04IODB .... STATIC SQL I/O MODULE                                   00004000
*                                                                       00005000
*  CREATION DATE: 04/25/16                                              00006000
*                                                                       00007000
*  FUNCTIONAL DESCRIPTION: THIS PROGRAM CONTAINS THE STATIC SQL         00008000
*  VECTORS REQUIRED TO SUPPORT I/O TO THE S04 FILE.  IT IS LOADED       00009000
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
COBREC   DS    CL139                   COBOL RECORD (UNALIGNED)         00036000
ASMREC   DS    0F                      ASSEMBLER RECORD (ALIGNED)       00037000
INST     DS    CL4                                                      00038000
RECNBR   DS    CL4                                                      00039000
RELCODE  DS    CL6                                                      00040000
AUDDATE  DS    PL5'0.'                                                  00041000
AUDTIME  DS    PL5'0.'                                                  00042000
AUDUSER  DS    CL8                                                      00043000
AUDORG   DS    CL6                                                      00044000
INCLEXCL DS    CL1                                                      00045000
RELAT1   DS    CL2                                                      00046000
RELAT2   DS    CL2                                                      00047000
RELAT3   DS    CL2                                                      00048000
RELAT4   DS    CL2                                                      00049000
RELAT5   DS    CL2                                                      00050000
RELAT6   DS    CL2                                                      00051000
RELAT7   DS    CL2                                                      00052000
RELAT8   DS    CL2                                                      00053000
RELAT9   DS    CL2                                                      00054000
RELAT10  DS    CL2                                                      00055000
RELAT11  DS    CL2                                                      00056000
RELAT12  DS    CL2                                                      00057000
RELAT13  DS    CL2                                                      00058000
RELAT14  DS    CL2                                                      00059000
RELAT15  DS    CL2                                                      00060000
RELAT16  DS    CL2                                                      00061000
RELAT17  DS    CL2                                                      00062000
RELAT18  DS    CL2                                                      00063000
RELAT19  DS    CL2                                                      00064000
RELAT20  DS    CL2                                                      00065000
RELAT21  DS    CL2                                                      00066000
RELAT22  DS    CL2                                                      00067000
RELAT23  DS    CL2                                                      00068000
RELAT24  DS    CL2                                                      00069000
RELAT25  DS    CL2                                                      00070000
RELAT26  DS    CL2                                                      00071000
RELAT27  DS    CL2                                                      00072000
RELAT28  DS    CL2                                                      00073000
RELAT29  DS    CL2                                                      00074000
RELAT30  DS    CL2                                                      00075000
RELAT31  DS    CL2                                                      00076000
RELAT32  DS    CL2                                                      00077000
RELAT33  DS    CL2                                                      00078000
RELAT34  DS    CL2                                                      00079000
RELAT35  DS    CL2                                                      00080000
RELAT36  DS    CL2                                                      00081000
RELAT37  DS    CL2                                                      00082000
RELAT38  DS    CL2                                                      00083000
RELAT39  DS    CL2                                                      00084000
RELAT40  DS    CL2                                                      00085000
RELAT41  DS    CL2                                                      00086000
RELAT42  DS    CL2                                                      00087000
RELAT43  DS    CL2                                                      00088000
RELAT44  DS    CL2                                                      00089000
RELAT45  DS    CL2                                                      00090000
RELAT46  DS    CL2                                                      00091000
RELAT47  DS    CL2                                                      00092000
RELAT48  DS    CL2                                                      00093000
RELAT49  DS    CL2                                                      00094000
RELAT50  DS    CL2                                                      00095000
*                                                                       00096000
*                                                                       00097000
**********************************************************************  00098000
* PROGRAM TABLE HEADER SECTION:                                         00099000
*   THIS SECTION CONTAINS STATIC DESCRIPTIVE FIELDS.                    00100000
*   THE CONTENTS OF THIS SECTION SHOULD NOT BE MODIFIED.                00101000
**********************************************************************  00102000
*                                                                       00103000
S04IODB  CSECT                         PROGRAM TABLE SECTION            00104000
S04IODB  AMODE ANY                                                      00105000
S04IODB  RMODE ANY                                                      00106000
         DC    CL9'S04IODB'            PROGRAM ID                       00107000
         DC    CL8'&SYSDATE',CL1' ',CL5'&SYSTIME',CL1' '                00108000
         DC    A(SQLDLEN)              SQLDSECT SIZE                    00109000
         DC    A(0)                    RESERVED                         00110000
         DC    A(0)                    RESERVED                         00111000
         DC    24CL1' '                RESERVED                         00112000
         DC    A(STM#TAB)              STATEMENT TABLE ADDRESS          00113000
*                                                                       00114000
         DC    CL43'COPYRIGHT 2017 INFOR. ALL RIGHTS RESERVED. '        00115000
         DC    CL29'WWW.INFOR.COM                '                      00115001
*                                                                       00116000
*                                                                       00117000
**********************************************************************  00118000
* STATEMENT TABLE SECTION:                                              00119000
*   THIS SECTION DEFINES AN ENTRY VECTOR FOR EACH SQL STATEMENT.        00120000
*   THE CONTENTS AND ORDER OF THE STATEMENTS IN THIS SECTION            00121000
*   SHOULD NOT BE MODIFIED.                                             00122000
**********************************************************************  00123000
*                                                                       00124000
STM#TAB  CSECT                         STATEMENT TABLE SECTION          00125000
STM#TAB  AMODE ANY                                                      00126000
STM#TAB  RMODE ANY                                                      00127000
         DC    A(SQL#0000)             SELECT INTO (KEY 0)              00128000
         DC    A(SQL#0001)             SELECT INTO (KEY 1)              00129000
         DC    A(SQL#0002)             SELECT INTO (KEY 2)              00130000
         DC    A(SQL#0003)             SELECT INTO (KEY 3)              00131000
         DC    A(SQL#0004)             SELECT UPDATE CURSOR (KEY 0)     00132000
         DC    A(SQL#0005)             SELECT UPDATE CURSOR (KEY 1)     00133000
         DC    A(SQL#0006)             SELECT UPDATE CURSOR (KEY 2)     00134000
         DC    A(SQL#0007)             SELECT UPDATE CURSOR (KEY 3)     00135000
         DC    A(SQL#0008)             FETCH UPDATE CURSOR (KEY 0)      00136000
         DC    A(SQL#0009)             FETCH UPDATE CURSOR (KEY 1)      00137000
         DC    A(SQL#0010)             FETCH UPDATE CURSOR (KEY 2)      00138000
         DC    A(SQL#0011)             FETCH UPDATE CURSOR (KEY 3)      00139000
         DC    A(SQL#0012)             CLOSE UPDATE CURSOR (KEY 0)      00140000
         DC    A(SQL#0013)             CLOSE UPDATE CURSOR (KEY 1)      00141000
         DC    A(SQL#0014)             CLOSE UPDATE CURSOR (KEY 2)      00142000
         DC    A(SQL#0015)             CLOSE UPDATE CURSOR (KEY 3)      00143000
         DC    A(SQL#0016)             SELECT SEQ CURSOR (KEY 0)        00144000
         DC    A(SQL#0017)             SELECT SEQ CURSOR (KEY 1)        00145000
         DC    A(SQL#0018)             SELECT SEQ CURSOR (KEY 2)        00146000
         DC    A(SQL#0019)             SELECT SEQ CURSOR (KEY 3)        00147000
         DC    A(SQL#0020)             FETCH SEQ CURSOR (KEY 0)         00148000
         DC    A(SQL#0021)             FETCH SEQ CURSOR (KEY 1)         00149000
         DC    A(SQL#0022)             FETCH SEQ CURSOR (KEY 2)         00150000
         DC    A(SQL#0023)             FETCH SEQ CURSOR (KEY 3)         00151000
         DC    A(SQL#0024)             FETCH SEQ CURSOR UPDATE (KEY 0)  00152000
         DC    A(SQL#0025)             FETCH SEQ CURSOR UPDATE (KEY 1)  00153000
         DC    A(SQL#0026)             FETCH SEQ CURSOR UPDATE (KEY 2)  00154000
         DC    A(SQL#0027)             FETCH SEQ CURSOR UPDATE (KEY 3)  00155000
         DC    A(SQL#0028)             CLOSE SEQ CURSOR (KEY 0)         00156000
         DC    A(SQL#0029)             CLOSE SEQ CURSOR (KEY 1)         00157000
         DC    A(SQL#0030)             CLOSE SEQ CURSOR (KEY 2)         00158000
         DC    A(SQL#0031)             CLOSE SEQ CURSOR (KEY 3)         00159000
         DC    A(SQL#0032)             SELECT KEY (KEY 0)               00160000
         DC    A(SQL#0033)             SELECT KEY (KEY 1)               00161000
         DC    A(SQL#0034)             SELECT KEY (KEY 2)               00162000
         DC    A(SQL#0035)             SELECT KEY (KEY 3)               00163000
         DC    A(SQL#0036)             INSERT STATEMENT                 00164000
         DC    A(SQL#0037)             UPDATE STATEMENT (KEY 0)         00165000
         DC    A(SQL#0038)             UPDATE STATEMENT (KEY 1)         00166000
         DC    A(SQL#0039)             UPDATE STATEMENT (KEY 2)         00167000
         DC    A(SQL#0040)             UPDATE STATEMENT (KEY 3)         00168000
         DC    A(SQL#0041)             DELETE STATEMENT (KEY 0)         00169000
         DC    A(SQL#0042)             DELETE STATEMENT (KEY 1)         00170000
         DC    A(SQL#0043)             DELETE STATEMENT (KEY 2)         00171000
         DC    A(SQL#0044)             DELETE STATEMENT (KEY 3)         00172000
         DC    A(SQL#0045)             DELETE ALL STATEMENT             00173000
         DC    4X'FF'                                                   00174000
*                                                                       00175000
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
         USING HOST#VAR,2              ADDRESS HOST VARIABLES           00188000
         USING SQLDSECT,10,3           ADDRESS SQLDSECT                 00189000
         USING COM#AREA,11             ADDRESS COMMAREA                 00190000
*                                                                       00191000
*                                                                       00192000
**********************************************************************  00193000
* SELECT INTO STATEMENT BY PRIMARY KEY:                                 00194000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00195000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00196000
**********************************************************************  00197000
*                                                                       00198000
SQL#0000 DS    0H                                                       00199000
         USING SQL#0000,12             ESTABLISH BASE REGISTER          00200000
         B     *+6                     BRANCH AROUND ADCON              00201000
BASE0000 DC    AL2(4096)                                                00202000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00203000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00204000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00205000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00206000
         AH    3,BASE0000              ADD 4K                           00207000
         EXEC  SQL                                                     *00208000
               SELECT AUDIT_DATE,                                      *00209000
                   AUDIT_TIME,                                         *00210000
                   AUDIT_USER,                                         *00211000
                   AUDIT_ORG,                                          *00212000
                   INCL_EXCL,                                          *00213000
                   REL_CODE_01,                                        *00214000
                   REL_CODE_02,                                        *00215000
                   REL_CODE_03,                                        *00216000
                   REL_CODE_04,                                        *00217000
                   REL_CODE_05,                                        *00218000
                   REL_CODE_06,                                        *00219000
                   REL_CODE_07,                                        *00220000
                   REL_CODE_08,                                        *00221000
                   REL_CODE_09,                                        *00222000
                   REL_CODE_10,                                        *00223000
                   REL_CODE_11,                                        *00224000
                   REL_CODE_12,                                        *00225000
                   REL_CODE_13,                                        *00226000
                   REL_CODE_14,                                        *00227000
                   REL_CODE_15,                                        *00228000
                   REL_CODE_16,                                        *00229000
                   REL_CODE_17,                                        *00230000
                   REL_CODE_18,                                        *00231000
                   REL_CODE_19,                                        *00232000
                   REL_CODE_20,                                        *00233000
                   REL_CODE_21,                                        *00234000
                   REL_CODE_22,                                        *00235000
                   REL_CODE_23,                                        *00236000
                   REL_CODE_24,                                        *00237000
                   REL_CODE_25,                                        *00238000
                   REL_CODE_26,                                        *00239000
                   REL_CODE_27,                                        *00240000
                   REL_CODE_28,                                        *00241000
                   REL_CODE_29,                                        *00242000
                   REL_CODE_30,                                        *00243000
                   REL_CODE_31,                                        *00244000
                   REL_CODE_32,                                        *00245000
                   REL_CODE_33,                                        *00246000
                   REL_CODE_34,                                        *00247000
                   REL_CODE_35,                                        *00248000
                   REL_CODE_36,                                        *00249000
                   REL_CODE_37,                                        *00250000
                   REL_CODE_38,                                        *00251000
                   REL_CODE_39,                                        *00252000
                   REL_CODE_40,                                        *00253000
                   REL_CODE_41,                                        *00254000
                   REL_CODE_42,                                        *00255000
                   REL_CODE_43,                                        *00256000
                   REL_CODE_44,                                        *00257000
                   REL_CODE_45,                                        *00258000
                   REL_CODE_46,                                        *00259000
                   REL_CODE_47,                                        *00260000
                   REL_CODE_48,                                        *00261000
                   REL_CODE_49,                                        *00262000
                   REL_CODE_50                                         *00263000
                 INTO :AUDDATE,                                        *00264000
                   :AUDTIME,                                           *00265000
                   :AUDUSER,                                           *00266000
                   :AUDORG,                                            *00267000
                   :INCLEXCL,                                          *00268000
                   :RELAT1,                                            *00269000
                   :RELAT2,                                            *00270000
                   :RELAT3,                                            *00271000
                   :RELAT4,                                            *00272000
                   :RELAT5,                                            *00273000
                   :RELAT6,                                            *00274000
                   :RELAT7,                                            *00275000
                   :RELAT8,                                            *00276000
                   :RELAT9,                                            *00277000
                   :RELAT10,                                           *00278000
                   :RELAT11,                                           *00279000
                   :RELAT12,                                           *00280000
                   :RELAT13,                                           *00281000
                   :RELAT14,                                           *00282000
                   :RELAT15,                                           *00283000
                   :RELAT16,                                           *00284000
                   :RELAT17,                                           *00285000
                   :RELAT18,                                           *00286000
                   :RELAT19,                                           *00287000
                   :RELAT20,                                           *00288000
                   :RELAT21,                                           *00289000
                   :RELAT22,                                           *00290000
                   :RELAT23,                                           *00291000
                   :RELAT24,                                           *00292000
                   :RELAT25,                                           *00293000
                   :RELAT26,                                           *00294000
                   :RELAT27,                                           *00295000
                   :RELAT28,                                           *00296000
                   :RELAT29,                                           *00297000
                   :RELAT30,                                           *00298000
                   :RELAT31,                                           *00299000
                   :RELAT32,                                           *00300000
                   :RELAT33,                                           *00301000
                   :RELAT34,                                           *00302000
                   :RELAT35,                                           *00303000
                   :RELAT36,                                           *00304000
                   :RELAT37,                                           *00305000
                   :RELAT38,                                           *00306000
                   :RELAT39,                                           *00307000
                   :RELAT40,                                           *00308000
                   :RELAT41,                                           *00309000
                   :RELAT42,                                           *00310000
                   :RELAT43,                                           *00311000
                   :RELAT44,                                           *00312000
                   :RELAT45,                                           *00313000
                   :RELAT46,                                           *00314000
                   :RELAT47,                                           *00315000
                   :RELAT48,                                           *00316000
                   :RELAT49,                                           *00317000
                   :RELAT50                                            *00318000
                 FROM S04                                              *00319000
                 WHERE INST_NBR = :INST AND                            *00320000
                   RECORD_NBR = :RECNBR AND                            *00321000
                   RELATE_CODE = :RELCODE                              *00322000
                 FETCH FIRST 1 ROW ONLY                                 00323000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   00324000
         BALR  4,5                     MOVE INTO RECORD AREA            00325000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00326000
         BR    14                      RETURN TO CALLER                 00327000
         LTORG                                                          00328000
*                                                                       00329000
*                                                                       00330000
**********************************************************************  00331000
* SELECT INTO STATEMENT BY ALTERNATE KEY 1:                             00332000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00333000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00334000
**********************************************************************  00335000
*                                                                       00336000
SQL#0001 DS    0H                                                       00337000
         USING SQL#0001,12             ESTABLISH BASE REGISTER          00338000
         LA    15,255                  SET RETURN CODE                  00339000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00340000
         BR    14                      RETURN TO CALLER                 00341000
         LTORG                                                          00342000
*                                                                       00343000
*                                                                       00344000
**********************************************************************  00345000
* SELECT INTO STATEMENT BY ALTERNATE KEY 2:                             00346000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00347000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00348000
**********************************************************************  00349000
*                                                                       00350000
SQL#0002 DS    0H                                                       00351000
         USING SQL#0002,12             ESTABLISH BASE REGISTER          00352000
         LA    15,255                  SET RETURN CODE                  00353000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00354000
         BR    14                      RETURN TO CALLER                 00355000
         LTORG                                                          00356000
*                                                                       00357000
*                                                                       00358000
**********************************************************************  00359000
* SELECT INTO STATEMENT BY ALTERNATE KEY 3:                             00360000
*   THIS STATEMENT SUPPORTS THE GET (WITHOUT LOCK) VERB.                00361000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00362000
**********************************************************************  00363000
*                                                                       00364000
SQL#0003 DS    0H                                                       00365000
         USING SQL#0003,12             ESTABLISH BASE REGISTER          00366000
         LA    15,255                  SET RETURN CODE                  00367000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00368000
         BR    14                      RETURN TO CALLER                 00369000
         LTORG                                                          00370000
*                                                                       00371000
*                                                                       00372000
**********************************************************************  00373000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY PRIMARY KEY:       00374000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00375000
*   THEY ARE ALSO USED AFTER A SUCCESSFUL SELECT SEQUENTIAL STATEMENT   00376000
*     FOR THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS.                      00377000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00378000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00379000
*     UPDATE STATEMENT TO MATCH.                                        00380000
**********************************************************************  00381000
*                                                                       00382000
SQL#0004 DS    0H                                                       00383000
         USING SQL#0004,12             ESTABLISH BASE REGISTER          00384000
         B     *+6                     BRANCH AROUND ADCON              00385000
BASE0004 DC    AL2(4096)                                                00386000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00387000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00388000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00389000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00390000
         AH    3,BASE0004              ADD 4K                           00391000
         EXEC  SQL                                                     *00392000
               DECLARE S04UPD0 CURSOR FOR                              *00393000
               SELECT AUDIT_DATE,                                      *00394000
                   AUDIT_TIME,                                         *00395000
                   AUDIT_USER,                                         *00396000
                   AUDIT_ORG,                                          *00397000
                   INCL_EXCL,                                          *00398000
                   REL_CODE_01,                                        *00399000
                   REL_CODE_02,                                        *00400000
                   REL_CODE_03,                                        *00401000
                   REL_CODE_04,                                        *00402000
                   REL_CODE_05,                                        *00403000
                   REL_CODE_06,                                        *00404000
                   REL_CODE_07,                                        *00405000
                   REL_CODE_08,                                        *00406000
                   REL_CODE_09,                                        *00407000
                   REL_CODE_10,                                        *00408000
                   REL_CODE_11,                                        *00409000
                   REL_CODE_12,                                        *00410000
                   REL_CODE_13,                                        *00411000
                   REL_CODE_14,                                        *00412000
                   REL_CODE_15,                                        *00413000
                   REL_CODE_16,                                        *00414000
                   REL_CODE_17,                                        *00415000
                   REL_CODE_18,                                        *00416000
                   REL_CODE_19,                                        *00417000
                   REL_CODE_20,                                        *00418000
                   REL_CODE_21,                                        *00419000
                   REL_CODE_22,                                        *00420000
                   REL_CODE_23,                                        *00421000
                   REL_CODE_24,                                        *00422000
                   REL_CODE_25,                                        *00423000
                   REL_CODE_26,                                        *00424000
                   REL_CODE_27,                                        *00425000
                   REL_CODE_28,                                        *00426000
                   REL_CODE_29,                                        *00427000
                   REL_CODE_30,                                        *00428000
                   REL_CODE_31,                                        *00429000
                   REL_CODE_32,                                        *00430000
                   REL_CODE_33,                                        *00431000
                   REL_CODE_34,                                        *00432000
                   REL_CODE_35,                                        *00433000
                   REL_CODE_36,                                        *00434000
                   REL_CODE_37,                                        *00435000
                   REL_CODE_38,                                        *00436000
                   REL_CODE_39,                                        *00437000
                   REL_CODE_40,                                        *00438000
                   REL_CODE_41,                                        *00439000
                   REL_CODE_42,                                        *00440000
                   REL_CODE_43,                                        *00441000
                   REL_CODE_44,                                        *00442000
                   REL_CODE_45,                                        *00443000
                   REL_CODE_46,                                        *00444000
                   REL_CODE_47,                                        *00445000
                   REL_CODE_48,                                        *00446000
                   REL_CODE_49,                                        *00447000
                   REL_CODE_50                                         *00448000
                 FROM S04                                              *00449000
                 WHERE INST_NBR = :INST AND                            *00450000
                   RECORD_NBR = :RECNBR AND                            *00451000
                   RELATE_CODE = :RELCODE                              *00452000
                 FOR UPDATE OF AUDIT_DATE,                             *00453000
                   AUDIT_TIME,                                         *00454000
                   AUDIT_USER,                                         *00455000
                   AUDIT_ORG,                                          *00456000
                   INCL_EXCL,                                          *00457000
                   REL_CODE_01,                                        *00458000
                   REL_CODE_02,                                        *00459000
                   REL_CODE_03,                                        *00460000
                   REL_CODE_04,                                        *00461000
                   REL_CODE_05,                                        *00462000
                   REL_CODE_06,                                        *00463000
                   REL_CODE_07,                                        *00464000
                   REL_CODE_08,                                        *00465000
                   REL_CODE_09,                                        *00466000
                   REL_CODE_10,                                        *00467000
                   REL_CODE_11,                                        *00468000
                   REL_CODE_12,                                        *00469000
                   REL_CODE_13,                                        *00470000
                   REL_CODE_14,                                        *00471000
                   REL_CODE_15,                                        *00472000
                   REL_CODE_16,                                        *00473000
                   REL_CODE_17,                                        *00474000
                   REL_CODE_18,                                        *00475000
                   REL_CODE_19,                                        *00476000
                   REL_CODE_20,                                        *00477000
                   REL_CODE_21,                                        *00478000
                   REL_CODE_22,                                        *00479000
                   REL_CODE_23,                                        *00480000
                   REL_CODE_24,                                        *00481000
                   REL_CODE_25,                                        *00482000
                   REL_CODE_26,                                        *00483000
                   REL_CODE_27,                                        *00484000
                   REL_CODE_28,                                        *00485000
                   REL_CODE_29,                                        *00486000
                   REL_CODE_30,                                        *00487000
                   REL_CODE_31,                                        *00488000
                   REL_CODE_32,                                        *00489000
                   REL_CODE_33,                                        *00490000
                   REL_CODE_34,                                        *00491000
                   REL_CODE_35,                                        *00492000
                   REL_CODE_36,                                        *00493000
                   REL_CODE_37,                                        *00494000
                   REL_CODE_38,                                        *00495000
                   REL_CODE_39,                                        *00496000
                   REL_CODE_40,                                        *00497000
                   REL_CODE_41,                                        *00498000
                   REL_CODE_42,                                        *00499000
                   REL_CODE_43,                                        *00500000
                   REL_CODE_44,                                        *00501000
                   REL_CODE_45,                                        *00502000
                   REL_CODE_46,                                        *00503000
                   REL_CODE_47,                                        *00504000
                   REL_CODE_48,                                        *00505000
                   REL_CODE_49,                                        *00506000
                   REL_CODE_50                                         *00507000
                 FETCH FIRST 1 ROW ONLY                                 00508000
         EXEC  SQL                                                     *00509000
               OPEN S04UPD0                                             00510000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00511000
         BR    14                      RETURN TO CALLER                 00512000
         LTORG                                                          00513000
*                                                                       00514000
*                                                                       00515000
**********************************************************************  00516000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 1:   00517000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00518000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00519000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00520000
*     UPDATE STATEMENT TO MATCH.                                        00521000
**********************************************************************  00522000
*                                                                       00523000
SQL#0005 DS    0H                                                       00524000
         USING SQL#0005,12             ESTABLISH BASE REGISTER          00525000
         LA    15,255                  SET RETURN CODE                  00526000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00527000
         BR    14                      RETURN TO CALLER                 00528000
         LTORG                                                          00529000
*                                                                       00530000
*                                                                       00531000
**********************************************************************  00532000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 2:   00533000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00534000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00535000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00536000
*     UPDATE STATEMENT TO MATCH.                                        00537000
**********************************************************************  00538000
*                                                                       00539000
SQL#0006 DS    0H                                                       00540000
         USING SQL#0006,12             ESTABLISH BASE REGISTER          00541000
         LA    15,255                  SET RETURN CODE                  00542000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00543000
         BR    14                      RETURN TO CALLER                 00544000
         LTORG                                                          00545000
*                                                                       00546000
*                                                                       00547000
**********************************************************************  00548000
* SELECT UPDATE AND OPEN UPDATE CURSOR STATEMENTS BY ALTERNATE KEY 3:   00549000
*   THESE STATEMENTS SUPPORT THE GET-LOCK VERB.                         00550000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00551000
*     COLUMNS REFERENCED AS UPDATEABLE, AND THEN MODIFYING THE          00552000
*     UPDATE STATEMENT TO MATCH.                                        00553000
**********************************************************************  00554000
*                                                                       00555000
SQL#0007 DS    0H                                                       00556000
         USING SQL#0007,12             ESTABLISH BASE REGISTER          00557000
         LA    15,255                  SET RETURN CODE                  00558000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00559000
         BR    14                      RETURN TO CALLER                 00560000
         LTORG                                                          00561000
*                                                                       00562000
*                                                                       00563000
**********************************************************************  00564000
* FETCH FROM UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                   00565000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00566000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00567000
*     THE ACTUAL ROW.                                                   00568000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00569000
**********************************************************************  00570000
*                                                                       00571000
SQL#0008 DS    0H                                                       00572000
         USING SQL#0008,12             ESTABLISH BASE REGISTER          00573000
         B     *+6                     BRANCH AROUND ADCON              00574000
BASE0008 DC    AL2(4096)                                                00575000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00576000
         AH    3,BASE0008              ADD 4K                           00577000
         EXEC  SQL                                                     *00578000
               FETCH S04UPD0                                           *00579000
                 INTO :AUDDATE,                                        *00580000
                   :AUDTIME,                                           *00581000
                   :AUDUSER,                                           *00582000
                   :AUDORG,                                            *00583000
                   :INCLEXCL,                                          *00584000
                   :RELAT1,                                            *00585000
                   :RELAT2,                                            *00586000
                   :RELAT3,                                            *00587000
                   :RELAT4,                                            *00588000
                   :RELAT5,                                            *00589000
                   :RELAT6,                                            *00590000
                   :RELAT7,                                            *00591000
                   :RELAT8,                                            *00592000
                   :RELAT9,                                            *00593000
                   :RELAT10,                                           *00594000
                   :RELAT11,                                           *00595000
                   :RELAT12,                                           *00596000
                   :RELAT13,                                           *00597000
                   :RELAT14,                                           *00598000
                   :RELAT15,                                           *00599000
                   :RELAT16,                                           *00600000
                   :RELAT17,                                           *00601000
                   :RELAT18,                                           *00602000
                   :RELAT19,                                           *00603000
                   :RELAT20,                                           *00604000
                   :RELAT21,                                           *00605000
                   :RELAT22,                                           *00606000
                   :RELAT23,                                           *00607000
                   :RELAT24,                                           *00608000
                   :RELAT25,                                           *00609000
                   :RELAT26,                                           *00610000
                   :RELAT27,                                           *00611000
                   :RELAT28,                                           *00612000
                   :RELAT29,                                           *00613000
                   :RELAT30,                                           *00614000
                   :RELAT31,                                           *00615000
                   :RELAT32,                                           *00616000
                   :RELAT33,                                           *00617000
                   :RELAT34,                                           *00618000
                   :RELAT35,                                           *00619000
                   :RELAT36,                                           *00620000
                   :RELAT37,                                           *00621000
                   :RELAT38,                                           *00622000
                   :RELAT39,                                           *00623000
                   :RELAT40,                                           *00624000
                   :RELAT41,                                           *00625000
                   :RELAT42,                                           *00626000
                   :RELAT43,                                           *00627000
                   :RELAT44,                                           *00628000
                   :RELAT45,                                           *00629000
                   :RELAT46,                                           *00630000
                   :RELAT47,                                           *00631000
                   :RELAT48,                                           *00632000
                   :RELAT49,                                           *00633000
                   :RELAT50                                             00634000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   00635000
         BALR  4,5                     MOVE INTO RECORD AREA            00636000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00637000
         BR    14                      RETURN TO CALLER                 00638000
         LTORG                                                          00639000
*                                                                       00640000
*                                                                       00641000
**********************************************************************  00642000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 1:               00643000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00644000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00645000
*     THE ACTUAL ROW.                                                   00646000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00647000
**********************************************************************  00648000
*                                                                       00649000
SQL#0009 DS    0H                                                       00650000
         USING SQL#0009,12             ESTABLISH BASE REGISTER          00651000
         LA    15,255                  SET RETURN CODE                  00652000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00653000
         BR    14                      RETURN TO CALLER                 00654000
         LTORG                                                          00655000
*                                                                       00656000
*                                                                       00657000
**********************************************************************  00658000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 2:               00659000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00660000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00661000
*     THE ACTUAL ROW.                                                   00662000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00663000
**********************************************************************  00664000
*                                                                       00665000
SQL#0010 DS    0H                                                       00666000
         USING SQL#0010,12             ESTABLISH BASE REGISTER          00667000
         LA    15,255                  SET RETURN CODE                  00668000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00669000
         BR    14                      RETURN TO CALLER                 00670000
         LTORG                                                          00671000
*                                                                       00672000
*                                                                       00673000
**********************************************************************  00674000
* FETCH FROM UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 3:               00675000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00676000
*   IT WILL FOLLOW A SUCCESSFUL SELECT UPDATE STATEMENT TO RETRIEVE     00677000
*     THE ACTUAL ROW.                                                   00678000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00679000
**********************************************************************  00680000
*                                                                       00681000
SQL#0011 DS    0H                                                       00682000
         USING SQL#0011,12             ESTABLISH BASE REGISTER          00683000
         LA    15,255                  SET RETURN CODE                  00684000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00685000
         BR    14                      RETURN TO CALLER                 00686000
         LTORG                                                          00687000
*                                                                       00688000
*                                                                       00689000
**********************************************************************  00690000
* CLOSE UPDATE CURSOR STATEMENT FOR PRIMARY KEY:                        00691000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00692000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00693000
*     TO CLOSE THE UPDATE CURSOR.                                       00694000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00695000
**********************************************************************  00696000
*                                                                       00697000
SQL#0012 DS    0H                                                       00698000
         USING SQL#0012,12             ESTABLISH BASE REGISTER          00699000
         B     *+6                     BRANCH AROUND ADCON              00700000
BASE0012 DC    AL2(4096)                                                00701000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00702000
         AH    3,BASE0012              ADD 4K                           00703000
         EXEC  SQL                                                     *00704000
               CLOSE S04UPD0                                            00705000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00706000
         BR    14                      RETURN TO CALLER                 00707000
         LTORG                                                          00708000
*                                                                       00709000
*                                                                       00710000
**********************************************************************  00711000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 1:                    00712000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00713000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00714000
*     TO CLOSE THE UPDATE CURSOR.                                       00715000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00716000
**********************************************************************  00717000
*                                                                       00718000
SQL#0013 DS    0H                                                       00719000
         USING SQL#0013,12             ESTABLISH BASE REGISTER          00720000
         LA    15,255                  SET RETURN CODE                  00721000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00722000
         BR    14                      RETURN TO CALLER                 00723000
         LTORG                                                          00724000
*                                                                       00725000
*                                                                       00726000
**********************************************************************  00727000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 2:                    00728000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00729000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00730000
*     TO CLOSE THE UPDATE CURSOR.                                       00731000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00732000
**********************************************************************  00733000
*                                                                       00734000
SQL#0014 DS    0H                                                       00735000
         USING SQL#0014,12             ESTABLISH BASE REGISTER          00736000
         LA    15,255                  SET RETURN CODE                  00737000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00738000
         BR    14                      RETURN TO CALLER                 00739000
         LTORG                                                          00740000
*                                                                       00741000
*                                                                       00742000
**********************************************************************  00743000
* CLOSE UPDATE CURSOR STATEMENT FOR ALTERNATE KEY 3:                    00744000
*   THIS STATEMENT SUPPORTS THE GET-LOCK VERB.                          00745000
*   IT WILL FOLLOW THE LAST FETCH FROM UPDATE CURSOR STATEMENT          00746000
*     TO CLOSE THE UPDATE CURSOR.                                       00747000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00748000
**********************************************************************  00749000
*                                                                       00750000
SQL#0015 DS    0H                                                       00751000
         USING SQL#0015,12             ESTABLISH BASE REGISTER          00752000
         LA    15,255                  SET RETURN CODE                  00753000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00754000
         BR    14                      RETURN TO CALLER                 00755000
         LTORG                                                          00756000
*                                                                       00757000
*                                                                       00758000
**********************************************************************  00759000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY    00760000
* KEY:                                                                  00761000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00762000
*     AND GET-NEXT-LOCK VERBS.                                          00763000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00764000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00765000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00766000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     00767000
*     ORDER BY CLAUSE.                                                  00768000
**********************************************************************  00769000
*                                                                       00770000
SQL#0016 DS    0H                                                       00771000
         USING SQL#0016,12             ESTABLISH BASE REGISTER          00772000
         B     *+6                     BRANCH AROUND ADCON              00773000
BASE0016 DC    AL2(4096)                                                00774000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              00775000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      00776000
         BALR  4,5                     MOVE INTO HOST VARIABLES         00777000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00778000
         AH    3,BASE0016              ADD 4K                           00779000
         EXEC  SQL                                                     *00780000
               DECLARE S04SQ001 CURSOR FOR                             *00781000
               SELECT INST_NBR,                                        *00782000
                   RECORD_NBR,                                         *00783000
                   RELATE_CODE,                                        *00784000
                   AUDIT_DATE,                                         *00785000
                   AUDIT_TIME,                                         *00786000
                   AUDIT_USER,                                         *00787000
                   AUDIT_ORG,                                          *00788000
                   INCL_EXCL,                                          *00789000
                   REL_CODE_01,                                        *00790000
                   REL_CODE_02,                                        *00791000
                   REL_CODE_03,                                        *00792000
                   REL_CODE_04,                                        *00793000
                   REL_CODE_05,                                        *00794000
                   REL_CODE_06,                                        *00795000
                   REL_CODE_07,                                        *00796000
                   REL_CODE_08,                                        *00797000
                   REL_CODE_09,                                        *00798000
                   REL_CODE_10,                                        *00799000
                   REL_CODE_11,                                        *00800000
                   REL_CODE_12,                                        *00801000
                   REL_CODE_13,                                        *00802000
                   REL_CODE_14,                                        *00803000
                   REL_CODE_15,                                        *00804000
                   REL_CODE_16,                                        *00805000
                   REL_CODE_17,                                        *00806000
                   REL_CODE_18,                                        *00807000
                   REL_CODE_19,                                        *00808000
                   REL_CODE_20,                                        *00809000
                   REL_CODE_21,                                        *00810000
                   REL_CODE_22,                                        *00811000
                   REL_CODE_23,                                        *00812000
                   REL_CODE_24,                                        *00813000
                   REL_CODE_25,                                        *00814000
                   REL_CODE_26,                                        *00815000
                   REL_CODE_27,                                        *00816000
                   REL_CODE_28,                                        *00817000
                   REL_CODE_29,                                        *00818000
                   REL_CODE_30,                                        *00819000
                   REL_CODE_31,                                        *00820000
                   REL_CODE_32,                                        *00821000
                   REL_CODE_33,                                        *00822000
                   REL_CODE_34,                                        *00823000
                   REL_CODE_35,                                        *00824000
                   REL_CODE_36,                                        *00825000
                   REL_CODE_37,                                        *00826000
                   REL_CODE_38,                                        *00827000
                   REL_CODE_39,                                        *00828000
                   REL_CODE_40,                                        *00829000
                   REL_CODE_41,                                        *00830000
                   REL_CODE_42,                                        *00831000
                   REL_CODE_43,                                        *00832000
                   REL_CODE_44,                                        *00833000
                   REL_CODE_45,                                        *00834000
                   REL_CODE_46,                                        *00835000
                   REL_CODE_47,                                        *00836000
                   REL_CODE_48,                                        *00837000
                   REL_CODE_49,                                        *00838000
                   REL_CODE_50                                         *00839000
                 FROM S04                                              *00840000
                 WHERE                                                 *00841000
                    INST_NBR = :INST AND                               *00842000
                    RECORD_NBR = :RECNBR AND                           *00843000
                    RELATE_CODE >= :RELCODE                            *00844000
                 ORDER BY RELATE_CODE                                  *00845000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       00846000
         EXEC  SQL                                                     *00847000
               OPEN S04SQ001                                            00848000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00849000
         BR    14                      RETURN TO CALLER                 00850000
         LTORG                                                          00851000
*                                                                       00852000
*                                                                       00853000
**********************************************************************  00854000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  00855000
* KEY 1:                                                                00856000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00857000
*     AND GET-NEXT-LOCK VERBS.                                          00858000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00859000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00860000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00861000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     00862000
*     ORDER BY CLAUSE.                                                  00863000
**********************************************************************  00864000
*                                                                       00865000
SQL#0017 DS    0H                                                       00866000
         USING SQL#0017,12             ESTABLISH BASE REGISTER          00867000
         LA    15,255                  SET RETURN CODE                  00868000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00869000
         BR    14                      RETURN TO CALLER                 00870000
         LTORG                                                          00871000
*                                                                       00872000
*                                                                       00873000
**********************************************************************  00874000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  00875000
* KEY 2:                                                                00876000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00877000
*     AND GET-NEXT-LOCK VERBS.                                          00878000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00879000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00880000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00881000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     00882000
*     ORDER BY CLAUSE.                                                  00883000
**********************************************************************  00884000
*                                                                       00885000
SQL#0018 DS    0H                                                       00886000
         USING SQL#0018,12             ESTABLISH BASE REGISTER          00887000
         LA    15,255                  SET RETURN CODE                  00888000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00889000
         BR    14                      RETURN TO CALLER                 00890000
         LTORG                                                          00891000
*                                                                       00892000
*                                                                       00893000
**********************************************************************  00894000
* SELECT SEQUENTIAL AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE  00895000
* KEY 3:                                                                00896000
*   THESE STATEMENTS SUPPORT THE GET-GE, GET-GE-LOCK, GET-NEXT,         00897000
*     AND GET-NEXT-LOCK VERBS.                                          00898000
*   A SELECT UPDATE STATEMENT WILL FOLLOW IN THE CASE OF A              00899000
*     GET-GE-LOCK OR GET-NEXT-LOCK VERB.                                00900000
*   THEY MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF      00901000
*     GREATER THAN COMPARES IN THE WHERE CLAUSE AND BY DROPPING THE     00902000
*     ORDER BY CLAUSE.                                                  00903000
**********************************************************************  00904000
*                                                                       00905000
SQL#0019 DS    0H                                                       00906000
         USING SQL#0019,12             ESTABLISH BASE REGISTER          00907000
         LA    15,255                  SET RETURN CODE                  00908000
         L     14,SQW@RET              LOAD RETURN ADDRESS              00909000
         BR    14                      RETURN TO CALLER                 00910000
         LTORG                                                          00911000
*                                                                       00912000
*                                                                       00913000
**********************************************************************  00914000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:               00915000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              00916000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          00917000
*     RETRIEVE THE ACTUAL ROW.                                          00918000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             00919000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                00920000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  00921000
**********************************************************************  00922000
*                                                                       00923000
SQL#0020 DS    0H                                                       00924000
         USING SQL#0020,12             ESTABLISH BASE REGISTER          00925000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      00926000
         LA    12,VECT0020(1)          LOAD POINTER TO FETCH ROUTINE    00927000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       00928000
         BR    12                      GO TO CURRENT FETCH ROUTINE      00929000
VECT0020 DC    A(SQL#0120)                                              00930000
         DC    A(SQL#0220)                                              00931000
         DC    A(SQL#0320)                                              00932000
         LTORG                                                          00933000
*                                                                       00934000
SQL#0120 DS    0H                                                       00935000
         USING SQL#0120,12             ESTABLISH BASE REGISTER          00936000
         B     *+6                     BRANCH AROUND ADCON              00937000
BASE0120 DC    AL2(4096)                                                00938000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    00939000
         AH    3,BASE0120              ADD 4K                           00940000
         EXEC  SQL                                                     *00941000
               FETCH S04SQ001                                          *00942000
                 INTO :INST,                                           *00943000
                      :RECNBR,                                         *00944000
                      :RELCODE,                                        *00945000
                      :AUDDATE,                                        *00946000
                      :AUDTIME,                                        *00947000
                      :AUDUSER,                                        *00948000
                      :AUDORG,                                         *00949000
                      :INCLEXCL,                                       *00950000
                      :RELAT1,                                         *00951000
                      :RELAT2,                                         *00952000
                      :RELAT3,                                         *00953000
                      :RELAT4,                                         *00954000
                      :RELAT5,                                         *00955000
                      :RELAT6,                                         *00956000
                      :RELAT7,                                         *00957000
                      :RELAT8,                                         *00958000
                      :RELAT9,                                         *00959000
                      :RELAT10,                                        *00960000
                      :RELAT11,                                        *00961000
                      :RELAT12,                                        *00962000
                      :RELAT13,                                        *00963000
                      :RELAT14,                                        *00964000
                      :RELAT15,                                        *00965000
                      :RELAT16,                                        *00966000
                      :RELAT17,                                        *00967000
                      :RELAT18,                                        *00968000
                      :RELAT19,                                        *00969000
                      :RELAT20,                                        *00970000
                      :RELAT21,                                        *00971000
                      :RELAT22,                                        *00972000
                      :RELAT23,                                        *00973000
                      :RELAT24,                                        *00974000
                      :RELAT25,                                        *00975000
                      :RELAT26,                                        *00976000
                      :RELAT27,                                        *00977000
                      :RELAT28,                                        *00978000
                      :RELAT29,                                        *00979000
                      :RELAT30,                                        *00980000
                      :RELAT31,                                        *00981000
                      :RELAT32,                                        *00982000
                      :RELAT33,                                        *00983000
                      :RELAT34,                                        *00984000
                      :RELAT35,                                        *00985000
                      :RELAT36,                                        *00986000
                      :RELAT37,                                        *00987000
                      :RELAT38,                                        *00988000
                      :RELAT39,                                        *00989000
                      :RELAT40,                                        *00990000
                      :RELAT41,                                        *00991000
                      :RELAT42,                                        *00992000
                      :RELAT43,                                        *00993000
                      :RELAT44,                                        *00994000
                      :RELAT45,                                        *00995000
                      :RELAT46,                                        *00996000
                      :RELAT47,                                        *00997000
                      :RELAT48,                                        *00998000
                      :RELAT49,                                        *00999000
                      :RELAT50                                          01000000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01001000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01002000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01003000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01004000
         BR    12                      OPEN NEXT CURSOR                 01005000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01006000
         BALR  4,5                     MOVE INTO RECORD AREA            01007000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01008000
         BR    14                      RETURN TO CALLER                 01009000
         LTORG                                                          01010000
*                                                                       01011000
SQL#0220 DS    0H                                                       01012000
         USING SQL#0220,12             ESTABLISH BASE REGISTER          01013000
         B     *+6                     BRANCH AROUND ADCON              01014000
BASE0220 DC    AL2(4096)                                                01015000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01016000
         AH    3,BASE0220              ADD 4K                           01017000
         EXEC  SQL                                                     *01018000
               FETCH S04SQ002                                          *01019000
                 INTO :INST,                                           *01020000
                      :RECNBR,                                         *01021000
                      :RELCODE,                                        *01022000
                      :AUDDATE,                                        *01023000
                      :AUDTIME,                                        *01024000
                      :AUDUSER,                                        *01025000
                      :AUDORG,                                         *01026000
                      :INCLEXCL,                                       *01027000
                      :RELAT1,                                         *01028000
                      :RELAT2,                                         *01029000
                      :RELAT3,                                         *01030000
                      :RELAT4,                                         *01031000
                      :RELAT5,                                         *01032000
                      :RELAT6,                                         *01033000
                      :RELAT7,                                         *01034000
                      :RELAT8,                                         *01035000
                      :RELAT9,                                         *01036000
                      :RELAT10,                                        *01037000
                      :RELAT11,                                        *01038000
                      :RELAT12,                                        *01039000
                      :RELAT13,                                        *01040000
                      :RELAT14,                                        *01041000
                      :RELAT15,                                        *01042000
                      :RELAT16,                                        *01043000
                      :RELAT17,                                        *01044000
                      :RELAT18,                                        *01045000
                      :RELAT19,                                        *01046000
                      :RELAT20,                                        *01047000
                      :RELAT21,                                        *01048000
                      :RELAT22,                                        *01049000
                      :RELAT23,                                        *01050000
                      :RELAT24,                                        *01051000
                      :RELAT25,                                        *01052000
                      :RELAT26,                                        *01053000
                      :RELAT27,                                        *01054000
                      :RELAT28,                                        *01055000
                      :RELAT29,                                        *01056000
                      :RELAT30,                                        *01057000
                      :RELAT31,                                        *01058000
                      :RELAT32,                                        *01059000
                      :RELAT33,                                        *01060000
                      :RELAT34,                                        *01061000
                      :RELAT35,                                        *01062000
                      :RELAT36,                                        *01063000
                      :RELAT37,                                        *01064000
                      :RELAT38,                                        *01065000
                      :RELAT39,                                        *01066000
                      :RELAT40,                                        *01067000
                      :RELAT41,                                        *01068000
                      :RELAT42,                                        *01069000
                      :RELAT43,                                        *01070000
                      :RELAT44,                                        *01071000
                      :RELAT45,                                        *01072000
                      :RELAT46,                                        *01073000
                      :RELAT47,                                        *01074000
                      :RELAT48,                                        *01075000
                      :RELAT49,                                        *01076000
                      :RELAT50                                          01077000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01078000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01079000
         L     9,=A(SQL#0020)          LOAD RETURN ROUTINE ADDRESS      01080000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01081000
         BR    12                      OPEN NEXT CURSOR                 01082000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01083000
         BALR  4,5                     MOVE INTO RECORD AREA            01084000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01085000
         BR    14                      RETURN TO CALLER                 01086000
         LTORG                                                          01087000
*                                                                       01088000
SQL#0320 DS    0H                                                       01089000
         USING SQL#0320,12             ESTABLISH BASE REGISTER          01090000
         B     *+6                     BRANCH AROUND ADCON              01091000
BASE0320 DC    AL2(4096)                                                01092000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01093000
         AH    3,BASE0320              ADD 4K                           01094000
         EXEC  SQL                                                     *01095000
               FETCH S04SQ003                                          *01096000
                 INTO :INST,                                           *01097000
                      :RECNBR,                                         *01098000
                      :RELCODE,                                        *01099000
                      :AUDDATE,                                        *01100000
                      :AUDTIME,                                        *01101000
                      :AUDUSER,                                        *01102000
                      :AUDORG,                                         *01103000
                      :INCLEXCL,                                       *01104000
                      :RELAT1,                                         *01105000
                      :RELAT2,                                         *01106000
                      :RELAT3,                                         *01107000
                      :RELAT4,                                         *01108000
                      :RELAT5,                                         *01109000
                      :RELAT6,                                         *01110000
                      :RELAT7,                                         *01111000
                      :RELAT8,                                         *01112000
                      :RELAT9,                                         *01113000
                      :RELAT10,                                        *01114000
                      :RELAT11,                                        *01115000
                      :RELAT12,                                        *01116000
                      :RELAT13,                                        *01117000
                      :RELAT14,                                        *01118000
                      :RELAT15,                                        *01119000
                      :RELAT16,                                        *01120000
                      :RELAT17,                                        *01121000
                      :RELAT18,                                        *01122000
                      :RELAT19,                                        *01123000
                      :RELAT20,                                        *01124000
                      :RELAT21,                                        *01125000
                      :RELAT22,                                        *01126000
                      :RELAT23,                                        *01127000
                      :RELAT24,                                        *01128000
                      :RELAT25,                                        *01129000
                      :RELAT26,                                        *01130000
                      :RELAT27,                                        *01131000
                      :RELAT28,                                        *01132000
                      :RELAT29,                                        *01133000
                      :RELAT30,                                        *01134000
                      :RELAT31,                                        *01135000
                      :RELAT32,                                        *01136000
                      :RELAT33,                                        *01137000
                      :RELAT34,                                        *01138000
                      :RELAT35,                                        *01139000
                      :RELAT36,                                        *01140000
                      :RELAT37,                                        *01141000
                      :RELAT38,                                        *01142000
                      :RELAT39,                                        *01143000
                      :RELAT40,                                        *01144000
                      :RELAT41,                                        *01145000
                      :RELAT42,                                        *01146000
                      :RELAT43,                                        *01147000
                      :RELAT44,                                        *01148000
                      :RELAT45,                                        *01149000
                      :RELAT46,                                        *01150000
                      :RELAT47,                                        *01151000
                      :RELAT48,                                        *01152000
                      :RELAT49,                                        *01153000
                      :RELAT50                                          01154000
         L     5,=A(OUT#REC)           LOAD OUTPUT CONVERSION ROUTINE   01155000
         BALR  4,5                     MOVE INTO RECORD AREA            01156000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01157000
         BR    14                      RETURN TO CALLER                 01158000
         LTORG                                                          01159000
*                                                                       01160000
*                                                                       01161000
**********************************************************************  01162000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 1:           01163000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01164000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01165000
*     RETRIEVE THE ACTUAL ROW.                                          01166000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01167000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01168000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01169000
**********************************************************************  01170000
*                                                                       01171000
SQL#0021 DS    0H                                                       01172000
         USING SQL#0021,12             ESTABLISH BASE REGISTER          01173000
         LA    15,255                  SET RETURN CODE                  01174000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01175000
         BR    14                      RETURN TO CALLER                 01176000
         LTORG                                                          01177000
*                                                                       01178000
*                                                                       01179000
**********************************************************************  01180000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 2:           01181000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01182000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01183000
*     RETRIEVE THE ACTUAL ROW.                                          01184000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01185000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01186000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01187000
**********************************************************************  01188000
*                                                                       01189000
SQL#0022 DS    0H                                                       01190000
         USING SQL#0022,12             ESTABLISH BASE REGISTER          01191000
         LA    15,255                  SET RETURN CODE                  01192000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01193000
         BR    14                      RETURN TO CALLER                 01194000
         LTORG                                                          01195000
*                                                                       01196000
*                                                                       01197000
**********************************************************************  01198000
* FETCH FROM SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 3:           01199000
*   THIS STATEMENT SUPPORTS THE GET-GE AND GET-NEXT VERBS.              01200000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01201000
*     RETRIEVE THE ACTUAL ROW.                                          01202000
*   THE GET-GE-LOCK AND GET-NEXT-LOCK VERBS USE A DIFFERENT             01203000
*     FETCH STATEMENT AS ONLY THE KEY FIELDS ARE NEEDED.                01204000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01205000
**********************************************************************  01206000
*                                                                       01207000
SQL#0023 DS    0H                                                       01208000
         USING SQL#0023,12             ESTABLISH BASE REGISTER          01209000
         LA    15,255                  SET RETURN CODE                  01210000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01211000
         BR    14                      RETURN TO CALLER                 01212000
         LTORG                                                          01213000
*                                                                       01214000
*                                                                       01215000
**********************************************************************  01216000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01217000
* THE PRIMARY KEY:                                                      01218000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01219000
*     VERBS.                                                            01220000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01221000
*     RETRIEVE THE ACTUAL ROW.                                          01222000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01223000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01224000
*     WILL BE THRU THE UPDATE CURSOR.                                   01225000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01226000
**********************************************************************  01227000
*                                                                       01228000
SQL#0024 DS    0H                                                       01229000
         USING SQL#0024,12             ESTABLISH BASE REGISTER          01230000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01231000
         LA    12,VECT0024(1)          LOAD POINTER TO FETCH ROUTINE    01232000
         L     12,0(12)                LOAD FETCH ROUTINE ADDRESS       01233000
         BR    12                      GO TO CURRENT FETCH ROUTINE      01234000
VECT0024 DC    A(SQL#0124)                                              01235000
         DC    A(SQL#0224)                                              01236000
         DC    A(SQL#0324)                                              01237000
         LTORG                                                          01238000
*                                                                       01239000
SQL#0124 DS    0H                                                       01240000
         USING SQL#0124,12             ESTABLISH BASE REGISTER          01241000
         B     *+6                     BRANCH AROUND ADCON              01242000
BASE0124 DC    AL2(4096)                                                01243000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01244000
         AH    3,BASE0124              ADD 4K                           01245000
         EXEC  SQL                                                     *01246000
               FETCH S04SQ001                                          *01247000
                 INTO :INST,                                           *01248000
                      :RECNBR,                                         *01249000
                      :RELCODE                                          01250000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01251000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01252000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01253000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01254000
         BR    12                      OPEN NEXT CURSOR                 01255000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01256000
         BALR  4,5                     MOVE INTO RECORD AREA            01257000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01258000
         BR    14                      RETURN TO CALLER                 01259000
         LTORG                                                          01260000
*                                                                       01261000
SQL#0224 DS    0H                                                       01262000
         USING SQL#0224,12             ESTABLISH BASE REGISTER          01263000
         B     *+6                     BRANCH AROUND ADCON              01264000
BASE0224 DC    AL2(4096)                                                01265000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01266000
         AH    3,BASE0224              ADD 4K                           01267000
         EXEC  SQL                                                     *01268000
               FETCH S04SQ002                                          *01269000
                 INTO :INST,                                           *01270000
                      :RECNBR,                                         *01271000
                      :RELCODE                                          01272000
         CLC   SQLCODE,=F'+100'        CHECK FOR ROW NOT FOUND          01273000
         BNE   *+14                    RETURN IF NOT SQLCODE +100       01274000
         L     9,=A(SQL#0024)          LOAD RETURN ROUTINE ADDRESS      01275000
         L     12,=A(SQL#0046)         LOAD CURSOR ROUTINE BASE REG     01276000
         BR    12                      OPEN NEXT CURSOR                 01277000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01278000
         BALR  4,5                     MOVE INTO RECORD AREA            01279000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01280000
         BR    14                      RETURN TO CALLER                 01281000
         LTORG                                                          01282000
*                                                                       01283000
SQL#0324 DS    0H                                                       01284000
         USING SQL#0324,12             ESTABLISH BASE REGISTER          01285000
         B     *+6                     BRANCH AROUND ADCON              01286000
BASE0324 DC    AL2(4096)                                                01287000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01288000
         AH    3,BASE0324              ADD 4K                           01289000
         EXEC  SQL                                                     *01290000
               FETCH S04SQ003                                          *01291000
                 INTO :INST,                                           *01292000
                      :RECNBR,                                         *01293000
                      :RELCODE                                          01294000
         L     5,=A(OUT#KEY)           LOAD OUTPUT CONVERSION ROUTINE   01295000
         BALR  4,5                     MOVE INTO RECORD AREA            01296000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01297000
         BR    14                      RETURN TO CALLER                 01298000
         LTORG                                                          01299000
*                                                                       01300000
*                                                                       01301000
**********************************************************************  01302000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01303000
* ALTERNATE KEY 1:                                                      01304000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01305000
*     VERBS.                                                            01306000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01307000
*     RETRIEVE THE ACTUAL ROW.                                          01308000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01309000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01310000
*     WILL BE THRU THE UPDATE CURSOR.                                   01311000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01312000
**********************************************************************  01313000
*                                                                       01314000
SQL#0025 DS    0H                                                       01315000
         USING SQL#0025,12             ESTABLISH BASE REGISTER          01316000
         LA    15,255                  SET RETURN CODE                  01317000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01318000
         BR    14                      RETURN TO CALLER                 01319000
         LTORG                                                          01320000
*                                                                       01321000
*                                                                       01322000
**********************************************************************  01323000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01324000
* ALTERNATE KEY 2:                                                      01325000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01326000
*     VERBS.                                                            01327000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01328000
*     RETRIEVE THE ACTUAL ROW.                                          01329000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01330000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01331000
*     WILL BE THRU THE UPDATE CURSOR.                                   01332000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01333000
**********************************************************************  01334000
*                                                                       01335000
SQL#0026 DS    0H                                                       01336000
         USING SQL#0026,12             ESTABLISH BASE REGISTER          01337000
         LA    15,255                  SET RETURN CODE                  01338000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01339000
         BR    14                      RETURN TO CALLER                 01340000
         LTORG                                                          01341000
*                                                                       01342000
*                                                                       01343000
**********************************************************************  01344000
* FETCH FROM SEQUENTIAL CURSOR FOR EVENTUAL UPDATE STATEMENT FOR        01345000
* ALTERNATE KEY 3:                                                      01346000
*   THIS STATEMENT SUPPORTS THE GET-GE-LOCK AND GET-NEXT-LOCK           01347000
*     VERBS.                                                            01348000
*   IT WILL FOLLOW A SUCCESSFUL SELECT SEQUENTIAL STATEMENT TO          01349000
*     RETRIEVE THE ACTUAL ROW.                                          01350000
*   ONLY THE PRIMARY KEY FIELDS ARE RETRIEVED AS A SELECT FOR UPDATE    01351000
*     STATEMENT WILL FOLLOW AND RETREIVAL OF THE ACTUAL ROW             01352000
*     WILL BE THRU THE UPDATE CURSOR.                                   01353000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01354000
**********************************************************************  01355000
*                                                                       01356000
SQL#0027 DS    0H                                                       01357000
         USING SQL#0027,12             ESTABLISH BASE REGISTER          01358000
         LA    15,255                  SET RETURN CODE                  01359000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01360000
         BR    14                      RETURN TO CALLER                 01361000
         LTORG                                                          01362000
*                                                                       01363000
*                                                                       01364000
**********************************************************************  01365000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR PRIMARY KEY:                    01366000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01367000
*     AND GET-NEXT-LOCK VERBS.                                          01368000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01369000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01370000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01371000
**********************************************************************  01372000
*                                                                       01373000
SQL#0028 DS    0H                                                       01374000
         USING SQL#0028,12             ESTABLISH BASE REGISTER          01375000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01376000
         XC    CSRPTR,CSRPTR           CLEAR CURSOR ROUTINE POINTER     01377000
         LA    12,VECT0028(1)          LOAD POINTER TO CLOSE ROUTINE    01378000
         L     12,0(12)                LOAD CLOSE ROUTINE ADDRESS       01379000
         BR    12                      GO TO CURRENT CLOSE ROUTINE      01380000
VECT0028 DC    A(SQL#0128)                                              01381000
         DC    A(SQL#0228)                                              01382000
         DC    A(SQL#0328)                                              01383000
         LTORG                                                          01384000
*                                                                       01385000
SQL#0128 DS    0H                                                       01386000
         USING SQL#0128,12             ESTABLISH BASE REGISTER          01387000
         B     *+6                     BRANCH AROUND ADCON              01388000
BASE0128 DC    AL2(4096)                                                01389000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01390000
         AH    3,BASE0128              ADD 4K                           01391000
         EXEC  SQL                                                     *01392000
               CLOSE S04SQ001                                           01393000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01394000
         BR    14                      RETURN TO CALLER                 01395000
         LTORG                                                          01396000
*                                                                       01397000
SQL#0228 DS    0H                                                       01398000
         USING SQL#0228,12             ESTABLISH BASE REGISTER          01399000
         B     *+6                     BRANCH AROUND ADCON              01400000
BASE0228 DC    AL2(4096)                                                01401000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01402000
         AH    3,BASE0228              ADD 4K                           01403000
         EXEC  SQL                                                     *01404000
               CLOSE S04SQ002                                           01405000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01406000
         BR    14                      RETURN TO CALLER                 01407000
         LTORG                                                          01408000
*                                                                       01409000
SQL#0328 DS    0H                                                       01410000
         USING SQL#0328,12             ESTABLISH BASE REGISTER          01411000
         B     *+6                     BRANCH AROUND ADCON              01412000
BASE0328 DC    AL2(4096)                                                01413000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01414000
         AH    3,BASE0328              ADD 4K                           01415000
         EXEC  SQL                                                     *01416000
               CLOSE S04SQ003                                           01417000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01418000
         BR    14                      RETURN TO CALLER                 01419000
         LTORG                                                          01420000
*                                                                       01421000
*                                                                       01422000
**********************************************************************  01423000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 1:                01424000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01425000
*     AND GET-NEXT-LOCK VERBS.                                          01426000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01427000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01428000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01429000
**********************************************************************  01430000
*                                                                       01431000
SQL#0029 DS    0H                                                       01432000
         USING SQL#0029,12             ESTABLISH BASE REGISTER          01433000
         LA    15,255                  SET RETURN CODE                  01434000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01435000
         BR    14                      RETURN TO CALLER                 01436000
         LTORG                                                          01437000
*                                                                       01438000
*                                                                       01439000
**********************************************************************  01440000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 2:                01441000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01442000
*     AND GET-NEXT-LOCK VERBS.                                          01443000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01444000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01445000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01446000
**********************************************************************  01447000
*                                                                       01448000
SQL#0030 DS    0H                                                       01449000
         USING SQL#0030,12             ESTABLISH BASE REGISTER          01450000
         LA    15,255                  SET RETURN CODE                  01451000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01452000
         BR    14                      RETURN TO CALLER                 01453000
         LTORG                                                          01454000
*                                                                       01455000
*                                                                       01456000
**********************************************************************  01457000
* CLOSE SEQUENTIAL CURSOR STATEMENT FOR ALTERNATE KEY 3:                01458000
*   THIS STATEMENT SUPPORTS THE GET-GE, GET-GE-LOCK, GET-NEXT,          01459000
*     AND GET-NEXT-LOCK VERBS.                                          01460000
*   IT WILL FOLLOW THE LAST FETCH FROM SEQUENTIAL CURSOR STATEMENT      01461000
*     TO CLOSE THE SEQUENTIAL CURSOR.                                   01462000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01463000
**********************************************************************  01464000
*                                                                       01465000
SQL#0031 DS    0H                                                       01466000
         USING SQL#0031,12             ESTABLISH BASE REGISTER          01467000
         LA    15,255                  SET RETURN CODE                  01468000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01469000
         BR    14                      RETURN TO CALLER                 01470000
         LTORG                                                          01471000
*                                                                       01472000
*                                                                       01473000
**********************************************************************  01474000
* SELECT KEY STATEMENT BY PRIMARY KEY:                                  01475000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            01476000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01477000
**********************************************************************  01478000
*                                                                       01479000
SQL#0032 DS    0H                                                       01480000
         USING SQL#0032,12             ESTABLISH BASE REGISTER          01481000
         B     *+6                     BRANCH AROUND ADCON              01482000
BASE0032 DC    AL2(4096)                                                01483000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              01484000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      01485000
         BALR  4,5                     MOVE INTO HOST VARIABLES         01486000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01487000
         AH    3,BASE0032              ADD 4K                           01488000
         EXEC  SQL                                                     *01489000
               SELECT INST_NBR,                                        *01490000
                   RECORD_NBR,                                         *01491000
                   RELATE_CODE                                         *01492000
                 INTO :INST,                                           *01493000
                   :RECNBR,                                            *01494000
                   :RELCODE                                            *01495000
                 FROM S04                                              *01496000
                 WHERE INST_NBR = :INST AND                            *01497000
                   RECORD_NBR = :RECNBR AND                            *01498000
                   RELATE_CODE = :RELCODE                              *01499000
                 FETCH FIRST 1 ROW ONLY                                 01500000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01501000
         BR    14                      RETURN TO CALLER                 01502000
         LTORG                                                          01503000
*                                                                       01504000
*                                                                       01505000
**********************************************************************  01506000
* SELECT KEY STATEMENT BY ALTERNATE KEY 1:                              01507000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            01508000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01509000
**********************************************************************  01510000
*                                                                       01511000
SQL#0033 DS    0H                                                       01512000
         USING SQL#0033,12             ESTABLISH BASE REGISTER          01513000
         LA    15,255                  SET RETURN CODE                  01514000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01515000
         BR    14                      RETURN TO CALLER                 01516000
         LTORG                                                          01517000
*                                                                       01518000
*                                                                       01519000
**********************************************************************  01520000
* SELECT KEY STATEMENT BY ALTERNATE KEY 2:                              01521000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            01522000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01523000
**********************************************************************  01524000
*                                                                       01525000
SQL#0034 DS    0H                                                       01526000
         USING SQL#0034,12             ESTABLISH BASE REGISTER          01527000
         LA    15,255                  SET RETURN CODE                  01528000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01529000
         BR    14                      RETURN TO CALLER                 01530000
         LTORG                                                          01531000
*                                                                       01532000
*                                                                       01533000
**********************************************************************  01534000
* SELECT KEY STATEMENT BY ALTERNATE KEY 3:                              01535000
*   THIS STATEMENT SUPPORTS THE LOCATE VERB.                            01536000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01537000
**********************************************************************  01538000
*                                                                       01539000
SQL#0035 DS    0H                                                       01540000
         USING SQL#0035,12             ESTABLISH BASE REGISTER          01541000
         LA    15,255                  SET RETURN CODE                  01542000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01543000
         BR    14                      RETURN TO CALLER                 01544000
         LTORG                                                          01545000
*                                                                       01546000
*                                                                       01547000
**********************************************************************  01548000
* INSERT STATEMENT:                                                     01549000
*   THIS STATEMENT SUPPORTS THE PUT VERB.                               01550000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01551000
**********************************************************************  01552000
*                                                                       01553000
SQL#0036 DS    0H                                                       01554000
         USING SQL#0036,12             ESTABLISH BASE REGISTER          01555000
         B     *+6                     BRANCH AROUND ADCON              01556000
BASE0036 DC    AL2(4096)                                                01557000
         L     5,=A(IN#HOST)           LOAD INPUT CONVERSION ROUTINE    01558000
         BALR  4,5                     MOVE INTO HOST VARIABLES         01559000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01560000
         AH    3,BASE0036              ADD 4K                           01561000
         EXEC  SQL                                                     *01562000
               INSERT INTO S04                                         *01563000
                   (INST_NBR,                                          *01564000
                    RECORD_NBR,                                        *01565000
                    RELATE_CODE,                                       *01566000
                    AUDIT_DATE,                                        *01567000
                    AUDIT_TIME,                                        *01568000
                    AUDIT_USER,                                        *01569000
                    AUDIT_ORG,                                         *01570000
                    INCL_EXCL,                                         *01571000
                    REL_CODE_01,                                       *01572000
                    REL_CODE_02,                                       *01573000
                    REL_CODE_03,                                       *01574000
                    REL_CODE_04,                                       *01575000
                    REL_CODE_05,                                       *01576000
                    REL_CODE_06,                                       *01577000
                    REL_CODE_07,                                       *01578000
                    REL_CODE_08,                                       *01579000
                    REL_CODE_09,                                       *01580000
                    REL_CODE_10,                                       *01581000
                    REL_CODE_11,                                       *01582000
                    REL_CODE_12,                                       *01583000
                    REL_CODE_13,                                       *01584000
                    REL_CODE_14,                                       *01585000
                    REL_CODE_15,                                       *01586000
                    REL_CODE_16,                                       *01587000
                    REL_CODE_17,                                       *01588000
                    REL_CODE_18,                                       *01589000
                    REL_CODE_19,                                       *01590000
                    REL_CODE_20,                                       *01591000
                    REL_CODE_21,                                       *01592000
                    REL_CODE_22,                                       *01593000
                    REL_CODE_23,                                       *01594000
                    REL_CODE_24,                                       *01595000
                    REL_CODE_25,                                       *01596000
                    REL_CODE_26,                                       *01597000
                    REL_CODE_27,                                       *01598000
                    REL_CODE_28,                                       *01599000
                    REL_CODE_29,                                       *01600000
                    REL_CODE_30,                                       *01601000
                    REL_CODE_31,                                       *01602000
                    REL_CODE_32,                                       *01603000
                    REL_CODE_33,                                       *01604000
                    REL_CODE_34,                                       *01605000
                    REL_CODE_35,                                       *01606000
                    REL_CODE_36,                                       *01607000
                    REL_CODE_37,                                       *01608000
                    REL_CODE_38,                                       *01609000
                    REL_CODE_39,                                       *01610000
                    REL_CODE_40,                                       *01611000
                    REL_CODE_41,                                       *01612000
                    REL_CODE_42,                                       *01613000
                    REL_CODE_43,                                       *01614000
                    REL_CODE_44,                                       *01615000
                    REL_CODE_45,                                       *01616000
                    REL_CODE_46,                                       *01617000
                    REL_CODE_47,                                       *01618000
                    REL_CODE_48,                                       *01619000
                    REL_CODE_49,                                       *01620000
                    REL_CODE_50)                                       *01621000
                  VALUES (:INST,                                       *01622000
                   :RECNBR,                                            *01623000
                   :RELCODE,                                           *01624000
                   :AUDDATE,                                           *01625000
                   :AUDTIME,                                           *01626000
                   :AUDUSER,                                           *01627000
                   :AUDORG,                                            *01628000
                   :INCLEXCL,                                          *01629000
                   :RELAT1,                                            *01630000
                   :RELAT2,                                            *01631000
                   :RELAT3,                                            *01632000
                   :RELAT4,                                            *01633000
                   :RELAT5,                                            *01634000
                   :RELAT6,                                            *01635000
                   :RELAT7,                                            *01636000
                   :RELAT8,                                            *01637000
                   :RELAT9,                                            *01638000
                   :RELAT10,                                           *01639000
                   :RELAT11,                                           *01640000
                   :RELAT12,                                           *01641000
                   :RELAT13,                                           *01642000
                   :RELAT14,                                           *01643000
                   :RELAT15,                                           *01644000
                   :RELAT16,                                           *01645000
                   :RELAT17,                                           *01646000
                   :RELAT18,                                           *01647000
                   :RELAT19,                                           *01648000
                   :RELAT20,                                           *01649000
                   :RELAT21,                                           *01650000
                   :RELAT22,                                           *01651000
                   :RELAT23,                                           *01652000
                   :RELAT24,                                           *01653000
                   :RELAT25,                                           *01654000
                   :RELAT26,                                           *01655000
                   :RELAT27,                                           *01656000
                   :RELAT28,                                           *01657000
                   :RELAT29,                                           *01658000
                   :RELAT30,                                           *01659000
                   :RELAT31,                                           *01660000
                   :RELAT32,                                           *01661000
                   :RELAT33,                                           *01662000
                   :RELAT34,                                           *01663000
                   :RELAT35,                                           *01664000
                   :RELAT36,                                           *01665000
                   :RELAT37,                                           *01666000
                   :RELAT38,                                           *01667000
                   :RELAT39,                                           *01668000
                   :RELAT40,                                           *01669000
                   :RELAT41,                                           *01670000
                   :RELAT42,                                           *01671000
                   :RELAT43,                                           *01672000
                   :RELAT44,                                           *01673000
                   :RELAT45,                                           *01674000
                   :RELAT46,                                           *01675000
                   :RELAT47,                                           *01676000
                   :RELAT48,                                           *01677000
                   :RELAT49,                                           *01678000
                   :RELAT50)                                            01679000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01680000
         BR    14                      RETURN TO CALLER                 01681000
         LTORG                                                          01682000
*                                                                       01683000
*                                                                       01684000
**********************************************************************  01685000
* UPDATE STATEMENT BY PRIMARY KEY:                                      01686000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             01687000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        01688000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         01689000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             01690000
*     STATEMENT.                                                        01691000
**********************************************************************  01692000
*                                                                       01693000
SQL#0037 DS    0H                                                       01694000
         USING SQL#0037,12             ESTABLISH BASE REGISTER          01695000
         B     *+6                     BRANCH AROUND ADCON              01696000
BASE0037 DC    AL2(4096)                                                01697000
         L     5,=A(IN#HOST)           LOAD INPUT CONVERSION ROUTINE    01698000
         BALR  4,5                     MOVE INTO HOST VARIABLES         01699000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01700000
         AH    3,BASE0037              ADD 4K                           01701000
         EXEC  SQL                                                     *01702000
               UPDATE S04                                              *01703000
                   SET AUDIT_DATE = :AUDDATE,                          *01704000
                     AUDIT_TIME = :AUDTIME,                            *01705000
                     AUDIT_USER = :AUDUSER,                            *01706000
                     AUDIT_ORG = :AUDORG,                              *01707000
                     INCL_EXCL = :INCLEXCL,                            *01708000
                     REL_CODE_01 = :RELAT1,                            *01709000
                     REL_CODE_02 = :RELAT2,                            *01710000
                     REL_CODE_03 = :RELAT3,                            *01711000
                     REL_CODE_04 = :RELAT4,                            *01712000
                     REL_CODE_05 = :RELAT5,                            *01713000
                     REL_CODE_06 = :RELAT6,                            *01714000
                     REL_CODE_07 = :RELAT7,                            *01715000
                     REL_CODE_08 = :RELAT8,                            *01716000
                     REL_CODE_09 = :RELAT9,                            *01717000
                     REL_CODE_10 = :RELAT10,                           *01718000
                     REL_CODE_11 = :RELAT11,                           *01719000
                     REL_CODE_12 = :RELAT12,                           *01720000
                     REL_CODE_13 = :RELAT13,                           *01721000
                     REL_CODE_14 = :RELAT14,                           *01722000
                     REL_CODE_15 = :RELAT15,                           *01723000
                     REL_CODE_16 = :RELAT16,                           *01724000
                     REL_CODE_17 = :RELAT17,                           *01725000
                     REL_CODE_18 = :RELAT18,                           *01726000
                     REL_CODE_19 = :RELAT19,                           *01727000
                     REL_CODE_20 = :RELAT20,                           *01728000
                     REL_CODE_21 = :RELAT21,                           *01729000
                     REL_CODE_22 = :RELAT22,                           *01730000
                     REL_CODE_23 = :RELAT23,                           *01731000
                     REL_CODE_24 = :RELAT24,                           *01732000
                     REL_CODE_25 = :RELAT25,                           *01733000
                     REL_CODE_26 = :RELAT26,                           *01734000
                     REL_CODE_27 = :RELAT27,                           *01735000
                     REL_CODE_28 = :RELAT28,                           *01736000
                     REL_CODE_29 = :RELAT29,                           *01737000
                     REL_CODE_30 = :RELAT30,                           *01738000
                     REL_CODE_31 = :RELAT31,                           *01739000
                     REL_CODE_32 = :RELAT32,                           *01740000
                     REL_CODE_33 = :RELAT33,                           *01741000
                     REL_CODE_34 = :RELAT34,                           *01742000
                     REL_CODE_35 = :RELAT35,                           *01743000
                     REL_CODE_36 = :RELAT36,                           *01744000
                     REL_CODE_37 = :RELAT37,                           *01745000
                     REL_CODE_38 = :RELAT38,                           *01746000
                     REL_CODE_39 = :RELAT39,                           *01747000
                     REL_CODE_40 = :RELAT40,                           *01748000
                     REL_CODE_41 = :RELAT41,                           *01749000
                     REL_CODE_42 = :RELAT42,                           *01750000
                     REL_CODE_43 = :RELAT43,                           *01751000
                     REL_CODE_44 = :RELAT44,                           *01752000
                     REL_CODE_45 = :RELAT45,                           *01753000
                     REL_CODE_46 = :RELAT46,                           *01754000
                     REL_CODE_47 = :RELAT47,                           *01755000
                     REL_CODE_48 = :RELAT48,                           *01756000
                     REL_CODE_49 = :RELAT49,                           *01757000
                     REL_CODE_50 = :RELAT50                            *01758000
                 WHERE CURRENT OF S04UPD0                               01759000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01760000
         BR    14                      RETURN TO CALLER                 01761000
         LTORG                                                          01762000
*                                                                       01763000
*                                                                       01764000
**********************************************************************  01765000
* UPDATE STATEMENT BY ALTERNATE KEY 1:                                  01766000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             01767000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        01768000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         01769000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             01770000
*     STATEMENT.                                                        01771000
**********************************************************************  01772000
*                                                                       01773000
SQL#0038 DS    0H                                                       01774000
         USING SQL#0038,12             ESTABLISH BASE REGISTER          01775000
         LA    15,255                  SET RETURN CODE                  01776000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01777000
         BR    14                      RETURN TO CALLER                 01778000
         LTORG                                                          01779000
*                                                                       01780000
*                                                                       01781000
**********************************************************************  01782000
* UPDATE STATEMENT BY ALTERNATE KEY 2:                                  01783000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             01784000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        01785000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         01786000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             01787000
*     STATEMENT.                                                        01788000
**********************************************************************  01789000
*                                                                       01790000
SQL#0039 DS    0H                                                       01791000
         USING SQL#0039,12             ESTABLISH BASE REGISTER          01792000
         LA    15,255                  SET RETURN CODE                  01793000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01794000
         BR    14                      RETURN TO CALLER                 01795000
         LTORG                                                          01796000
*                                                                       01797000
*                                                                       01798000
**********************************************************************  01799000
* UPDATE STATEMENT BY ALTERNATE KEY 3:                                  01800000
*   THIS STATEMENT SUPPORTS THE REPUT VERB.                             01801000
*   IT MAY BE MODIFIED FOR PERFORMANCE BY REDUCING THE NUMBER OF        01802000
*     COLUMNS REFERENCED AS UPDATEABLE IN THE SELECT FOR UPDATE         01803000
*     STATEMENT, THEN MATCHING THAT REDUCTION IN THE UPDATE             01804000
*     STATEMENT.                                                        01805000
**********************************************************************  01806000
*                                                                       01807000
SQL#0040 DS    0H                                                       01808000
         USING SQL#0040,12             ESTABLISH BASE REGISTER          01809000
         LA    15,255                  SET RETURN CODE                  01810000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01811000
         BR    14                      RETURN TO CALLER                 01812000
         LTORG                                                          01813000
*                                                                       01814000
*                                                                       01815000
**********************************************************************  01816000
* DELETE STATEMENT BY PRIMARY KEY:                                      01817000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            01818000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01819000
**********************************************************************  01820000
*                                                                       01821000
SQL#0041 DS    0H                                                       01822000
         USING SQL#0041,12             ESTABLISH BASE REGISTER          01823000
         B     *+6                     BRANCH AROUND ADCON              01824000
BASE0041 DC    AL2(4096)                                                01825000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01826000
         AH    3,BASE0041              ADD 4K                           01827000
         EXEC  SQL                                                     *01828000
               DELETE FROM S04                                         *01829000
                 WHERE CURRENT OF S04UPD0                               01830000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01831000
         BR    14                      RETURN TO CALLER                 01832000
         LTORG                                                          01833000
*                                                                       01834000
*                                                                       01835000
**********************************************************************  01836000
* DELETE STATEMENT BY ALTERNATE KEY 1:                                  01837000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            01838000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01839000
**********************************************************************  01840000
*                                                                       01841000
SQL#0042 DS    0H                                                       01842000
         USING SQL#0042,12             ESTABLISH BASE REGISTER          01843000
         LA    15,255                  SET RETURN CODE                  01844000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01845000
         BR    14                      RETURN TO CALLER                 01846000
         LTORG                                                          01847000
*                                                                       01848000
*                                                                       01849000
**********************************************************************  01850000
* DELETE STATEMENT BY ALTERNATE KEY 2:                                  01851000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            01852000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01853000
**********************************************************************  01854000
*                                                                       01855000
SQL#0043 DS    0H                                                       01856000
         USING SQL#0043,12             ESTABLISH BASE REGISTER          01857000
         LA    15,255                  SET RETURN CODE                  01858000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01859000
         BR    14                      RETURN TO CALLER                 01860000
         LTORG                                                          01861000
*                                                                       01862000
*                                                                       01863000
**********************************************************************  01864000
* DELETE STATEMENT BY ALTERNATE KEY 3:                                  01865000
*   THIS STATEMENT SUPPORTS THE DELETE VERB.                            01866000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01867000
**********************************************************************  01868000
*                                                                       01869000
SQL#0044 DS    0H                                                       01870000
         USING SQL#0044,12             ESTABLISH BASE REGISTER          01871000
         LA    15,255                  SET RETURN CODE                  01872000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01873000
         BR    14                      RETURN TO CALLER                 01874000
         LTORG                                                          01875000
*                                                                       01876000
*                                                                       01877000
**********************************************************************  01878000
* DELETE ALL STATEMENT:                                                 01879000
*   THIS STATEMENT SUPPORTS THE DELETE-FILE VERB.                       01880000
*   IT SHOULD NOT NEED TO BE MODIFIED FOR PERFORMANCE.                  01881000
**********************************************************************  01882000
*                                                                       01883000
SQL#0045 DS    0H                                                       01884000
         USING SQL#0045,12             ESTABLISH BASE REGISTER          01885000
         B     *+6                     BRANCH AROUND ADCON              01886000
BASE0045 DC    AL2(4096)                                                01887000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01888000
         AH    3,BASE0045              ADD 4K                           01889000
         EXEC  SQL                                                     *01890000
               DELETE FROM S04                                          01891000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01892000
         BR    14                      RETURN TO CALLER                 01893000
         LTORG                                                          01894000
*                                                                       01895000
*                                                                       01896000
**********************************************************************  01897000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY PRIMARY KEY:          01898000
*   THIS ROUTINE HANDLES PRIMARY KEY SEQUENTIAL CURSORS.                01899000
**********************************************************************  01900000
*                                                                       01901000
SQL#0046 DS    0H                                                       01902000
         USING SQL#0046,12             ESTABLISH BASE REGISTER          01903000
         LA    3,X'80'                 LOAD MASK FOR KEY 0              01904000
         L     5,=A(IN#KEY)            LOAD KEY CONVERSION ROUTINE      01905000
         BALR  4,5                     MOVE INTO HOST VARIABLES         01906000
         LH    1,CSRPTR                LOAD CURRENT CURSOR POINTER      01907000
         LA    1,4(1)                  INCREMENT TO NEXT CURSOR         01908000
         STH   1,CSRPTR                SAVE FOR NEXT CALL               01909000
         LA    12,VECT0046(1)          LOAD POINTER TO NEXT CURSOR      01910000
         L     12,0(12)                LOAD CURSOR ROUTINE ADDRESS      01911000
         BR    12                      GO TO CURRENT CURSOR ROUTINE     01912000
VECT0046 DC    A(0)                                                     01913000
         DC    A(SQL#0246)                                              01914000
         DC    A(SQL#0346)                                              01915000
         LTORG                                                          01916000
*                                                                       01917000
SQL#0246 DS    0H                                                       01918000
         USING SQL#0246,12             ESTABLISH BASE REGISTER          01919000
         B     *+6                     BRANCH AROUND ADCON              01920000
BASE0246 DC    AL2(4096)                                                01921000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    01922000
         AH    3,BASE0246              ADD 4K                           01923000
         EXEC  SQL                                                     *01924000
               CLOSE S04SQ001                                           01925000
         EXEC  SQL                                                     *01926000
               DECLARE S04SQ002 CURSOR FOR                             *01927000
               SELECT INST_NBR,                                        *01928000
                   RECORD_NBR,                                         *01929000
                   RELATE_CODE,                                        *01930000
                   AUDIT_DATE,                                         *01931000
                   AUDIT_TIME,                                         *01932000
                   AUDIT_USER,                                         *01933000
                   AUDIT_ORG,                                          *01934000
                   INCL_EXCL,                                          *01935000
                   REL_CODE_01,                                        *01936000
                   REL_CODE_02,                                        *01937000
                   REL_CODE_03,                                        *01938000
                   REL_CODE_04,                                        *01939000
                   REL_CODE_05,                                        *01940000
                   REL_CODE_06,                                        *01941000
                   REL_CODE_07,                                        *01942000
                   REL_CODE_08,                                        *01943000
                   REL_CODE_09,                                        *01944000
                   REL_CODE_10,                                        *01945000
                   REL_CODE_11,                                        *01946000
                   REL_CODE_12,                                        *01947000
                   REL_CODE_13,                                        *01948000
                   REL_CODE_14,                                        *01949000
                   REL_CODE_15,                                        *01950000
                   REL_CODE_16,                                        *01951000
                   REL_CODE_17,                                        *01952000
                   REL_CODE_18,                                        *01953000
                   REL_CODE_19,                                        *01954000
                   REL_CODE_20,                                        *01955000
                   REL_CODE_21,                                        *01956000
                   REL_CODE_22,                                        *01957000
                   REL_CODE_23,                                        *01958000
                   REL_CODE_24,                                        *01959000
                   REL_CODE_25,                                        *01960000
                   REL_CODE_26,                                        *01961000
                   REL_CODE_27,                                        *01962000
                   REL_CODE_28,                                        *01963000
                   REL_CODE_29,                                        *01964000
                   REL_CODE_30,                                        *01965000
                   REL_CODE_31,                                        *01966000
                   REL_CODE_32,                                        *01967000
                   REL_CODE_33,                                        *01968000
                   REL_CODE_34,                                        *01969000
                   REL_CODE_35,                                        *01970000
                   REL_CODE_36,                                        *01971000
                   REL_CODE_37,                                        *01972000
                   REL_CODE_38,                                        *01973000
                   REL_CODE_39,                                        *01974000
                   REL_CODE_40,                                        *01975000
                   REL_CODE_41,                                        *01976000
                   REL_CODE_42,                                        *01977000
                   REL_CODE_43,                                        *01978000
                   REL_CODE_44,                                        *01979000
                   REL_CODE_45,                                        *01980000
                   REL_CODE_46,                                        *01981000
                   REL_CODE_47,                                        *01982000
                   REL_CODE_48,                                        *01983000
                   REL_CODE_49,                                        *01984000
                   REL_CODE_50                                         *01985000
                 FROM S04                                              *01986000
                 WHERE                                                 *01987000
                    INST_NBR = :INST AND                               *01988000
                    RECORD_NBR > :RECNBR                               *01989000
                 ORDER BY RECORD_NBR,                                  *01990000
                   RELATE_CODE                                         *01991000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       01992000
         EXEC  SQL                                                     *01993000
               OPEN S04SQ002                                            01994000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            01995000
         BNZ   *+8                     NO - RETURN ERROR                01996000
         LR    12,9                    LOAD RETURN ADDRESS              01997000
         BR    12                      RETURN TO FETCH ROUTINE          01998000
         L     14,SQW@RET              LOAD RETURN ADDRESS              01999000
         BR    14                      RETURN TO CALLER                 02000000
         LTORG                                                          02001000
*                                                                       02002000
SQL#0346 DS    0H                                                       02003000
         USING SQL#0346,12             ESTABLISH BASE REGISTER          02004000
         B     *+6                     BRANCH AROUND ADCON              02005000
BASE0346 DC    AL2(4096)                                                02006000
         LR    3,10                    LOAD SECOND BASE FOR SQLDSECT    02007000
         AH    3,BASE0346              ADD 4K                           02008000
         EXEC  SQL                                                     *02009000
               CLOSE S04SQ002                                           02010000
         EXEC  SQL                                                     *02011000
               DECLARE S04SQ003 CURSOR FOR                             *02012000
               SELECT INST_NBR,                                        *02013000
                   RECORD_NBR,                                         *02014000
                   RELATE_CODE,                                        *02015000
                   AUDIT_DATE,                                         *02016000
                   AUDIT_TIME,                                         *02017000
                   AUDIT_USER,                                         *02018000
                   AUDIT_ORG,                                          *02019000
                   INCL_EXCL,                                          *02020000
                   REL_CODE_01,                                        *02021000
                   REL_CODE_02,                                        *02022000
                   REL_CODE_03,                                        *02023000
                   REL_CODE_04,                                        *02024000
                   REL_CODE_05,                                        *02025000
                   REL_CODE_06,                                        *02026000
                   REL_CODE_07,                                        *02027000
                   REL_CODE_08,                                        *02028000
                   REL_CODE_09,                                        *02029000
                   REL_CODE_10,                                        *02030000
                   REL_CODE_11,                                        *02031000
                   REL_CODE_12,                                        *02032000
                   REL_CODE_13,                                        *02033000
                   REL_CODE_14,                                        *02034000
                   REL_CODE_15,                                        *02035000
                   REL_CODE_16,                                        *02036000
                   REL_CODE_17,                                        *02037000
                   REL_CODE_18,                                        *02038000
                   REL_CODE_19,                                        *02039000
                   REL_CODE_20,                                        *02040000
                   REL_CODE_21,                                        *02041000
                   REL_CODE_22,                                        *02042000
                   REL_CODE_23,                                        *02043000
                   REL_CODE_24,                                        *02044000
                   REL_CODE_25,                                        *02045000
                   REL_CODE_26,                                        *02046000
                   REL_CODE_27,                                        *02047000
                   REL_CODE_28,                                        *02048000
                   REL_CODE_29,                                        *02049000
                   REL_CODE_30,                                        *02050000
                   REL_CODE_31,                                        *02051000
                   REL_CODE_32,                                        *02052000
                   REL_CODE_33,                                        *02053000
                   REL_CODE_34,                                        *02054000
                   REL_CODE_35,                                        *02055000
                   REL_CODE_36,                                        *02056000
                   REL_CODE_37,                                        *02057000
                   REL_CODE_38,                                        *02058000
                   REL_CODE_39,                                        *02059000
                   REL_CODE_40,                                        *02060000
                   REL_CODE_41,                                        *02061000
                   REL_CODE_42,                                        *02062000
                   REL_CODE_43,                                        *02063000
                   REL_CODE_44,                                        *02064000
                   REL_CODE_45,                                        *02065000
                   REL_CODE_46,                                        *02066000
                   REL_CODE_47,                                        *02067000
                   REL_CODE_48,                                        *02068000
                   REL_CODE_49,                                        *02069000
                   REL_CODE_50                                         *02070000
                 FROM S04                                              *02071000
                 WHERE                                                 *02072000
                    INST_NBR > :INST                                   *02073000
                 ORDER BY INST_NBR,                                    *02074000
                   RECORD_NBR,                                         *02075000
                   RELATE_CODE                                         *02076000
                 OPTIMIZE FOR 1 ROW FOR READ ONLY                       02077000
         EXEC  SQL                                                     *02078000
               OPEN S04SQ003                                            02079000
         ICM   15,B'1111',SQLCODE      IS RETURN CODE ZERO ?            02080000
         BNZ   *+8                     NO - RETURN ERROR                02081000
         LR    12,9                    LOAD RETURN ADDRESS              02082000
         BR    12                      RETURN TO FETCH ROUTINE          02083000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02084000
         BR    14                      RETURN TO CALLER                 02085000
         LTORG                                                          02086000
*                                                                       02087000
*                                                                       02088000
**********************************************************************  02089000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 1:      02090000
*   THIS ROUTINE HANDLES ALTERNATE KEY 1 SEQUENTIAL CURSORS.            02091000
**********************************************************************  02092000
*                                                                       02093000
SQL#0047 DS    0H                                                       02094000
         USING SQL#0047,12             ESTABLISH BASE REGISTER          02095000
         LA    15,255                  SET RETURN CODE                  02096000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02097000
         BR    14                      RETURN TO CALLER                 02098000
         LTORG                                                          02099000
*                                                                       02100000
*                                                                       02101000
**********************************************************************  02102000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 2:      02103000
*   THIS ROUTINE HANDLES ALTERNATE KEY 2 SEQUENTIAL CURSORS.            02104000
**********************************************************************  02105000
*                                                                       02106000
SQL#0048 DS    0H                                                       02107000
         USING SQL#0048,12             ESTABLISH BASE REGISTER          02108000
         LA    15,255                  SET RETURN CODE                  02109000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02110000
         BR    14                      RETURN TO CALLER                 02111000
         LTORG                                                          02112000
*                                                                       02113000
*                                                                       02114000
**********************************************************************  02115000
* SELECT AND OPEN SEQUENTIAL CURSOR STATEMENTS BY ALTERNATE KEY 3:      02116000
*   THIS ROUTINE HANDLES ALTERNATE KEY 3 SEQUENTIAL CURSORS.            02117000
**********************************************************************  02118000
*                                                                       02119000
SQL#0049 DS    0H                                                       02120000
         USING SQL#0049,12             ESTABLISH BASE REGISTER          02121000
         LA    15,255                  SET RETURN CODE                  02122000
         L     14,SQW@RET              LOAD RETURN ADDRESS              02123000
         BR    14                      RETURN TO CALLER                 02124000
         LTORG                                                          02125000
*                                                                       02126000
         DS    0H                      END OF SQL STATEMENTS            02127000
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                   02128000
*                                                                       02129000
*                                                                       02130000
**********************************************************************  02131000
* DUMMY ENTRY POINT DSNHLI                                              02132000
*   MUST NOT BE MODIFIED.                                               02133000
**********************************************************************  02134000
*                                                                       02135000
         ENTRY DSNHLI                                                   02136000
DSNHLI   DS    0H                                                       02137000
         L     15,SQW@CAF              LOAD ENTRY POINT TO ATTACH       02138000
         BR    15                      BRANCH TO ATTACH FACILITY        02139000
*                                                                       02140000
*                                                                       02141000
**********************************************************************  02142000
* CONVERSION ROUTINES INTO AND OUT OF ASSEMBLER HOST VARIABLES          02143000
*   REQUIRED IF THE COBOL RECORD STRUCTURE IS NOT ALIGNED.              02144000
*   MUST NOT BE MODIFIED.                                               02145000
**********************************************************************  02146000
*                                                                       02147000
CONV#RTN CSECT                         CONVERSION ROUTINES SECTION      02148000
CONV#RTN AMODE ANY                                                      02149000
CONV#RTN RMODE ANY                                                      02150000
IN#HOST  DS    0H                      MOVE INTO HOST VARIABLES         02151000
         USING IN#HOST,5               ESTABLISH BASE REGISTER          02152000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    02153000
IN#HOST1 CLI   0(6),X'FF'              END OF TABLE?                    02154000
         BER   4                         YES - RETURN TO SQL            02155000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         02156000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    02157000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  02158000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 02159000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              02160000
         LR    15,1                    SET BOTH LENGTHS EQUAL           02161000
         MVCL  14,0                    MOVE TO HOST VARIABLES           02162000
         LA    6,8(6)                  INCREMENT TO NEXT ENTRY          02163000
         B     IN#HOST1                LOOP UNTIL END OF TABLE          02164000
*                                                                       02165000
IN#KEY   DS    0H                      MOVE KEYS INTO HOST VARIABLES    02166000
         USING IN#KEY,5                ESTABLISH BASE REGISTER          02167000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    02168000
IN#KEY1  CLI   0(6),X'FF'              END OF TABLE?                    02169000
         BER   4                         YES - RETURN TO SQL            02170000
         SLR   0,0                     CLEAR FOR INSERT                 02171000
         IC    0,6(6)                  INSERT KEY FIELD ENTRY           02172000
         NR    0,3                     FOR THE CURRENT KEY?             02173000
         BZ    IN#KEY2                   NO - GO FOR NEXT ENTRY         02174000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         02175000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    02176000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  02177000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 02178000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              02179000
         LR    15,1                    SET BOTH LENGTHS EQUAL           02180000
         MVCL  14,0                    MOVE TO HOST VARIABLES           02181000
         TM    7(6),X'80'              SIGNED DECIMAL FIELD?            02182000
         BNO   IN#KEY2                   NO - GO FOR NEXT ENTRY         02183000
         BCTR  14,0                    DECREMENT TO SIGN BYTE           02184000
         TM    0(14),X'0A'             VALID SIGN BYTE?                 02185000
         BO    IN#KEY2                   YES - GO FOR NEXT ENTRY        02186000
         TM    0(14),X'0C'             VALID SIGN BYTE?                 02187000
         BO    IN#KEY2                   YES - GO FOR NEXT ENTRY        02188000
         OI    0(14),X'0F'               NO  - MAKE IT VALID            02189000
IN#KEY2  LA    6,8(6)                  INCREMENT TO NEXT ENTRY          02190000
         B     IN#KEY1                 LOOP UNTIL END OF TABLE          02191000
*                                                                       02192000
OUT#REC  DS    0H                      MOVE HOST VARIABLES INTO RECORD  02193000
         USING OUT#REC,5               ESTABLISH BASE REGISTER          02194000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    02195000
OUT#REC1 CLI   0(6),X'FF'              END OF TABLE?                    02196000
         BER   4                         YES - RETURN TO SQL            02197000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         02198000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    02199000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  02200000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 02201000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              02202000
         LR    15,1                    SET BOTH LENGTHS EQUAL           02203000
         MVCL  0,14                    MOVE TO RECORD AREA              02204000
         LA    6,8(6)                  INCREMENT TO NEXT ENTRY          02205000
         B     OUT#REC1                LOOP UNTIL END OF TABLE          02206000
*                                                                       02207000
OUT#KEY  DS    0H                      MOVE KEYS INTO RECORD            02208000
         USING OUT#KEY,5               ESTABLISH BASE REGISTER          02209000
         LA    6,CONV#TAB              LOAD CONVERSION TABLE ADDRESS    02210000
OUT#KEY1 CLI   0(6),X'FF'              END OF TABLE?                    02211000
         BER   4                         YES - RETURN TO SQL            02212000
         TM    6(6),X'80'              FOR THE PRIMARY KEY?             02213000
         BZ    OUT#KEY2                  NO - GO FOR NEXT ENTRY         02214000
         LA    0,COBREC                LOAD RECORD AREA ADDRESS         02215000
         AH    0,0(6)                  ADD OFFSET WITHIN RECORD AREA    02216000
         LA    14,ASMREC               LOAD HOST VARIABLE AREA ADDRESS  02217000
         AH    14,2(6)                 ADD OFFSET WITHIN HOST VARIABLES 02218000
         LH    1,4(6)                  LOAD LENGTH TO MOVE              02219000
         LR    15,1                    SET BOTH LENGTHS EQUAL           02220000
         MVCL  0,14                    MOVE TO RECORD AREA              02221000
OUT#KEY2 LA    6,8(6)                  INCREMENT TO NEXT ENTRY          02222000
         B     OUT#KEY1                LOOP UNTIL END OF TABLE          02223000
*                                                                       02224000
*                                                                       02225000
**********************************************************************  02226000
* CONVERSION TABLE VALUES FOR ALL FIELDS IN THIS TABLE                  02227000
*   MUST NOT BE MODIFIED.                                               02228000
**********************************************************************  02229000
*                                                                       02230000
CONV#TAB DC    H'0000'                 COBOL RECORD AREA OFFSET         02231000
         DC    H'0000'                 HOST VARIABLE AREA OFFSET        02232000
         DC    H'0014'                 LENGTH TO MOVE                   02233000
         DC    X'80'                   80 = KEY 0 FIELD                 02234000
*                                      40 = KEY 1 FIELD                 02235000
*                                      20 = KEY 2 FIELD                 02236000
*                                      10 = KEY 3 FIELD                 02237000
         DC    X'00'                   80 = SIGNED DECIMAL FIELD        02238000
*                                                                       02239000
*                                      REST OF FIELD ENTRIES            02240000
         DC    H'0014',H'0014',H'0125',X'00',X'00'                      02241000
         DC    8X'FF'                  END OF FIELD ENTRIES             02242000
         LTORG                                                          02243000
         END                                                            02244000
