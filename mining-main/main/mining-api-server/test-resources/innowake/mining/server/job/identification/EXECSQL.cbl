       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EXECSQL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL DECLARE MMRS00C_AWA_VSAMK TABLE
             (
               KSDS_PRIMARY_INDEX   VARCHAR(10)
             )
           END-EXEC
      *    ************************************************************
           EXEC SQL
             DECLARE   C-VSAMK    CURSOR FOR
               SELECT    KSDS_PRIMARY_INDEX
               FROM      MMRS00C_AWA_VSAMK AS VSAMK
               ORDER BY  VSAMK.KSDS_PRIMARY_INDEX ASC
               FOR       FETCH ONLY
           END-EXEC
       PROCEDURE DIVISION.
           GOBACK.
