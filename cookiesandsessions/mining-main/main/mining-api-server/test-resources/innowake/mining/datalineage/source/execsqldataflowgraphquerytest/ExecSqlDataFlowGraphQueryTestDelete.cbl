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
      * some DECLARE TABLE test
            EXEC SQL
              DELETE FROM DEL.TABLE1
                WHERE SOME_VALUE = :ALPHA-SHORT
           END-EXEC
       END PROGRAM WCFD68A.

