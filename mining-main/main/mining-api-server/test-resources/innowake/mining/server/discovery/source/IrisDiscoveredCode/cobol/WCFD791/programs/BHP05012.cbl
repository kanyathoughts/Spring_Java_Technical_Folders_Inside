000001 IDENTIFICATION DIVISION.                                         00000100
TCSONS PROGRAM-ID. BHP05012.                                            00000200
000003 AUTHOR. THERESA RABA.                                            00000300
000004 DATE-WRITTEN. AUGUST 1989.                                       00000400
000005 DATE-COMPILED.                                                   00000500
000020 ENVIRONMENT DIVISION.                                            00003301
000021*                                                                 00003401
000022 CONFIGURATION SECTION.                                           00003501
000023*                                                                 00003601
000024 INPUT-OUTPUT SECTION.                                            00003701
000025*                                                                 00003801
000026 FILE-CONTROL.                                                    00003901
000027*                                                                 00004001
000028     SELECT WORK-FILE ASSIGN TO UT-S-DISKOA.                      00004101
000029     SELECT PRINT1-FILE ASSIGN TO UT-S-PRINT1.                    00004201
000030     SELECT STATE-FILE                                            00004301
000031         ASSIGN TO UT-S-STATE                                     00004401
000032     FILE STATUS WS-STATE-STATUS.                                 00004501
000033*                                                                 00004601
TCSD  *IDMS-CONTROL SECTION.                                            00004701
000035*                                                                 00004801
TCSD  *PROTOCOL.    MODE IS  BATCH DEBUG.                               00004901
TCSD  *             IDMS-RECORDS MANUAL.                                00005001
000038*                                                                 00005101
000039 DATA DIVISION.                                                   00005201
000046*                                                                 00005901
000071******************************************************************00008401
000072*                  WORKING STORAGE SECTION                       *00008501
000073******************************************************************00008601
000074 WORKING-STORAGE SECTION.                                         00008701
000445      EXEC SQL                                                    00044500
000446        DECLARE LIC_CSR CURSOR FOR                                00044600
000447        SELECT                                                    00044700
000448                PARTY_SUBJ_ID                                     00044800
000449               ,TIN_NUM                                           00044900
000450               ,PARTY_CD                                          00045000
000451               ,LIC_AUTH_SUBJ_ID                                  00045100
000452               ,LIC_TYP_CD                                        00045200
000453               ,LIC_DEF_START_DT                                  00045300
000454               ,LIC_RES_CD                                        00045400
000455               ,LIC_ISS_CD                                        00045500
000456               ,SPNSR_ORG_SUBJ_ID                                 00045600
000457               ,ENDRS_ORG_SUBJ_ID                                 00045700
000458               ,LIC_NUM                                           00045800
000459               ,LIC_TEMP_IND                                      00045900
000460               ,LIC_ORIG_APVL_DT                                  00046000
000461               ,LIC_RNWL_DT                                       00046100
000462               ,STATUS_CD                                         00046200
000463               ,LIC_STAT_DT                                       00046300
000464               ,LIC_EXPR_DT                                       00046400
000465               ,LIC_START_DT                                      00046500
000466          FROM BHTLICENSE                                         00046600
2000B7*         WHERE (PARTY_SUBJ_ID   = :WS-PARTY-SUBJ-ID              00046701
2000B7          WHERE FIRM_CD          = 'PRU'                          00046801
2000B7           AND  (PARTY_SUBJ_ID   = 1                              00046901
000468*PARENT-FIRM CHANGE BEGINS                                        00047001
000469           OR    PARTY_SUBJ_ID   = 0 )                            00047101
000471*PARENT-FIRM CHANGE ENDS                                          00047301
000472           AND   LIC_END_DT      =  '9999-12-31'                  00047401
000473           AND   STATUS_CD       <> '0008'                        00047501
000474           AND  LIC_AUTH_SUBJ_ID <> 900000000                     00047601
000475           AND  LIC_AUTH_SUBJ_ID <> 0                             00047701
2000B8*          AND  LIC_TYP_CD       <> '0063'                        00047801
2000B8           AND  LIC_TYP_CD NOT IN ('0063','0065')                 00047901
000477           ORDER BY STATUS_CD                                     00048001
000478      END-EXEC.                                                   00048101
000549******************************************************************00055501
000550*                    LINKAGE SECTION                             *00055601
000551******************************************************************00055701
000552 LINKAGE SECTION.                                                 00055801
000553 01  PARM-IN.                                                     00055901
000554 02  FILLER                               PIC XX.                 00056001
TCSBTH*02  PARM-DBNAME                          PIC X(4).               00056101
TCSBTH*02  PARM-DICT                            PIC X(4).               00056201
000557 02  PARM-REPORT-TYPE                     PIC X.                  00056301
000558 02  PARM-PRINT-DETACH                    PIC X.                  00056401
000559 02  PARM-RMO-CODE                        PIC X.                  00056501
000560 02  PARM-BROKER-CODE                     PIC X.                  00056601
000561 02  PARM-OFFICE-FIELDS                   PIC X(32).              00056701
000562 02  F                                    REDEFINES               00056801
000563     PARM-OFFICE-FIELDS.                                          00056901
000564 03  PARM-OFFICE                          PIC X(4) OCCURS 8 TIMES.00057001
000565                                                                  00057101
000566******************************************************************00057201
000567*                  PROCEDURE DIVISION                            *00057301
000568******************************************************************00057401
000569 PROCEDURE DIVISION
000570 0100-P-HOUSEKEEPING.                                             00057601
