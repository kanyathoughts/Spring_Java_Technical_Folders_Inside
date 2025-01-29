       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCFD459A.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  HOST-VARS.
           10 ALPHA-SHORT PIC X(8).
     
       EXEC SQL
          INCLUDE EMPREC
       END-EXEC.
       
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       BEGIN.
      * original test case 1 (simple select)
           EXEC SQL
                    SELECT
                     ALPHA_SHORT
                    INTO
                    :ALPHA-SHORT
                    FROM IW_SQL_TEST WHERE
                    ALPHA_SHORT = 'TEST1'
           END-EXEC.

       END PROGRAM WCFD68A.

