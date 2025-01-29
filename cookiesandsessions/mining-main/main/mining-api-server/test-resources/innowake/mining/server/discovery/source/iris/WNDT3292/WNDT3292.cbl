       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL                                                   00880000
               DECLARE ACH CURSOR FOR                                   00890001
                 SELECT SUBSTR(B.ASGNMT_VAL,1,8)AS ACCT_NUM,            00891002
                      A.GROSS_AM,                                       00893002
                      C.LNK_MONEY_MKT_PRNC,                             00895002
                      A.CREATN_TS                                       00897002
                 FROM VPNDG_LDGR_ENTRY A                                00898003
                   INNER JOIN VACCT_ALTN_ID_ASGN B                      00899003
                      ON A.ACCT_ID = B.ACCT_ID                          00899102
                     AND B.ACCT_ALTN_ID_CD = 9                          00899202
                   INNER JOIN VACCT_BRKG_CURR_DY C                      00899303
                      ON A.ACCT_ID = C.ACCT_ID                          00899402
                     AND C.PRTN_ID = :WS-PRTN-ID                        00899502
                     AND C.SECTY_ACCT_TYP_CD = '1'                      00899634
                   INNER JOIN VPNDG_LDGR_STATUS  D                      00899735
                      ON D.LDGR_ID        = A.LDGR_ID                   00899835
                     AND D.LDGR_ACTVTY_TYP_CD   IN ('ENNS', 'ABRV')     00899935
                 WHERE A.LDGR_TYP_CD                      ='ACH'        00900002
                   AND A.DB_CR_CD  = 'D'                                00900102
                   AND A.CREATN_TS BETWEEN :WS-FROM-TS                  00900212
                                       AND :WS-TO-TS                    00900312
                   AND A.RK_PROCS_DT = CURRENT DATE                     00900435
                   AND A.LDGR_ID NOT IN (SELECT X.LDGR_ID               00900510
                                           FROM VORD_TO_LDGR_RLSHP X    00900610
                                          WHERE X.LDGR_ID = A.LDGR_ID   00900710
                                        )                               00900810
                   AND D.STATUS_TS   = (SELECT MAX(STAT.STATUS_TS)      00900935
                                        FROM   VPNDG_LDGR_STATUS STAT   00901135
                                       WHERE STAT.LDGR_ID = A.LDGR_ID   00901235
                                       )                                00901335
                 ORDER BY A.GROSS_AM DESC                               00901402
                 WITH UR   
                 FOR FETCH ONLY                                         00950002
           END-EXEC.
       PROCEDURE DIVISION.
            DISPLAY 'HELLO'
            GOBACK.
