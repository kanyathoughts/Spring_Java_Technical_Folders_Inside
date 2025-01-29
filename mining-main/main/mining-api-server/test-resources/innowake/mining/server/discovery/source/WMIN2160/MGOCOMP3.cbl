       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOCOMP3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELD-1                   PIC 9(9).
       EXEC SQL
         INCLUDE SQLCA
       END-EXEC.

       EXEC SQL                                                         KESVH000
021080         DECLARE EBBOEFF1 CURSOR FOR                              KESVH000
021090         SELECT  C_FAM,                                           KESVH000
021100                 C_CHNG_XREF,                                     KESVH000
021110                 I_MOD_YR,                                        KESVH000
021120                 I_PRODCT_PLAN_CHNG,                              KESVH000
021130                 C_STAT,                                          KESVH000
021140                 D_UPD,                                           KESVH000
021150                 I_MOD_YR_OUT,                                    KESVH000
021160                 C_MKT_LST,                                          CL*17
021170                 I_PCHNG,                                            CL*17
021180                 I_LOGON                                             CL*17
021190           FROM  E.EBBOEFF                                        KESVH000
021200*DMT2     WHERE  (I_MOD_YR <= :WS-MDL-YEAR                        KESVH000
021210**               AND I_MOD_YR_OUT >= :WS-MDL-YEAR)                KESVH000
021220          WHERE (C_FAM = :MD-FAM                                     CL*21
021230             OR C_FAM = '$$')                                        CL*21
021240            AND C_CHNG_XREF = :MD-XREF                            KESVH000
021250*           AND C_CHNG_XREF LIKE :MD-XREF                            CL*17
021260*DMT2       AND C_MKT_LST LIKE :WS-MKT-LST                        KESVH000
021270***         AND I_PRODCT_PLAN_CHNG = :MD-AUTH                        CL**3
021280            AND C_STAT ^= 'W'                                     KESVH000
021290       ORDER BY  C_FAM,                                           KESVH000
021300                 I_PRODCT_PLAN_CHNG,                              KESVH000
021310                 C_CHNG_XREF,                                     KESVH000
021320                 D_UPD,                                           KESVH000
021330                 I_MOD_YR                                         KESVH000
021340     END-EXEC

       PROCEDURE DIVISION.
         FIRST-PARA.
           IF FIELD-1 = 'P'
               DISPLAY 'P'.
           IF FIELD-1 = 'R'
               DISPLAY 'R'.
           DISPLAY 'Q'.
           EXIT.
