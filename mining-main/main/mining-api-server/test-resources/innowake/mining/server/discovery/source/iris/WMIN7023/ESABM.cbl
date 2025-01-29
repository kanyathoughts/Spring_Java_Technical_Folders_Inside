       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ESABM IS INITIAL PROGRAM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           EXEC SQL
             DECLARE BETA_TRN CURSOR FOR
               SELECT BTRAN.ACT_ID AS ACT_ID
                   FROM UTABLE11 CLNT
             INNER JOIN UTABLE22 ACCT
                    ON CLNT.EXT_CID = ACCT.EXTCID
                    AND CLNT.STY    = 2

                    UNION
                       (
                    SELECT DISTINCT CAST(ACCT.EXTCID AS DECIMAL(15))
                                     AS SAGID
                    ,  PNDTXN.ACTID  AS ACTID
                    ,  PNDTXN.APID   AS APID
                    FROM UTABLE13          CLNT
                    INNER JOIN UTABLE14    ACCT
                       ON CLNT.EXTCID = ACCT.EXTCID
                       AND CLNT.STYPE = 2
                  )

                    WITH UR
           END-EXEC
           EXEC SQL
              DECLARE BETA_TRN CURSOR FOR
               SELECT BTRAN.ACT_ID AS ACT_ID
                   FROM XTABLE1 CLNT
             INNER JOIN XTABLE2 ACCT
                    ON CLNT.EXT_CID = ACCT.EXTCID
                    AND CLNT.STY    = 2

                    UNION
                       (
                    SELECT
                       PNDTXN.ACTID AS ACTID
                    ,  PNDTXN.APID AS APID
                    FROM XTABLE3          CLNT
                  ) 
                    WITH UR
           END-EXEC