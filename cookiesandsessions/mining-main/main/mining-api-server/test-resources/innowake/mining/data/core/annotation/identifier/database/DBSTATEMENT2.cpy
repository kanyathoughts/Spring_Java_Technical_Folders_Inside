           EXEC SQL
             DECLARE   C-VSAMK    CURSOR FOR
               SELECT    KSDS_PRIMARY_INDEX
               FROM      MMRS00C_AWA_VSAMK AS VSAMK
               ORDER BY  VSAMK.KSDS_PRIMARY_INDEX ASC
               FOR       FETCH ONLY
           END-EXEC
