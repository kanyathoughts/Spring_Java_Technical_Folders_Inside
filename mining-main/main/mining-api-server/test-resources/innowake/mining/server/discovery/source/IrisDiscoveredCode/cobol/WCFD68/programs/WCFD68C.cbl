      IDENTIFICATION DIVISION.
      PROGRAM-ID. WCFD68C.
      ENVIRONMENT DIVISION.
      INPUT-OUTPUT SECTION.
      FILE-CONTROL.
      DATA DIVISION.
      FILE SECTION.
      WORKING-STORAGE SECTION.
      01  HOST-VARS.
          10 ALPHA-SHORT PIC X(8).
          10 OTHER-FIELD PIC X(8).
 
      EXEC SQL
         INCLUDE SQLCA
      END-EXEC.
      LINKAGE SECTION.
      PROCEDURE DIVISION.
      BEGIN.
      
          EXEC SQL
                   SELECT
                    ALPHA_SHORT,
                    OTHER_TABLE.OTHER_FIELD
                   INTO
                   :ALPHA-SHORT
                   :OTHER-FIELD
                   FROM IW_SQL_TEST
                   JOIN OTHER_TABLE ON
                   IW_SQL_TEST.ALPHA_SHORT = OTHER_TABLE.ALPHA_SHORT
          END-EXEC.
 
          EXEC SQL
                   SELECT
                    ALPHA_SHORT,
                    OTHER_TABLE.OTHER_FIELD
                   INTO
                   :ALPHA-SHORT
                   :OTHER-FIELD
                   FROM IW_SQL_TEST
                   INNER JOIN OTHER_TABLE ON
                   IW_SQL_TEST.ALPHA_SHORT = OTHER_TABLE.ALPHA_SHORT
          END-EXEC.
          
          EXEC SQL
                   SELECT
                    ALPHA_SHORT,
                    OTHER_TABLE.OTHER_FIELD
                   INTO
                   :ALPHA-SHORT
                   :OTHER-FIELD
                   FROM IW_SQL_TEST A
                   INNER JOIN OTHER_TABLE B ON
                   IW_SQL_TEST.ALPHA_SHORT = OTHER_TABLE.ALPHA_SHORT
          END-EXEC.
          
          EXEC SQL
                   SELECT
                    ALPHA_SHORT,
                    OTHER_TABLE.OTHER_FIELD
                   INTO
                   :ALPHA-SHORT
                   :OTHER-FIELD
                   FROM IW_SQL_TEST
                   LEFT OUTER JOIN OTHER_TABLE ON
                   IW_SQL_TEST.ALPHA_SHORT = OTHER_TABLE.ALPHA_SHORT
          END-EXEC.
          
          EXEC SQL
                   SELECT
                    ALPHA_SHORT,
                    OTHER_TABLE.OTHER_FIELD
                   INTO
                   :ALPHA-SHORT
                   :OTHER-FIELD
                   FROM IW_SQL_TEST
                   RIGHT OUTER JOIN OTHER_TABLE ON
                   IW_SQL_TEST.ALPHA_SHORT = OTHER_TABLE.ALPHA_SHORT
          END-EXEC.
          
          EXEC SQL
                   SELECT
                    ALPHA_SHORT,
                    OTHER_TABLE.OTHER_FIELD
                   INTO
                   :ALPHA-SHORT
                   :OTHER-FIELD
                   FROM IW_SQL_TEST
                   FULL OUTER JOIN OTHER_TABLE ON
                   IW_SQL_TEST.ALPHA_SHORT = OTHER_TABLE.ALPHA_SHORT
          END-EXEC.
          GOBACK.
 
      END PROGRAM WCFD68C.