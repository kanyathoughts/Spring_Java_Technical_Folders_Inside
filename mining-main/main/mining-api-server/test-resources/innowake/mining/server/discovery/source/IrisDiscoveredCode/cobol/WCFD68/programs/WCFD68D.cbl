       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCFD68D.
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

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       BEGIN.

      * declare sql cursor for select
           EXEC SQL
              DECLARE EMPCURS CURSOR FOR
              SELECT LNAME, FNAME, PAYRATE, HOURS
              FROM CURSTABLE
              WHERE DEPT = 'D11'
           END-EXEC

      * open sql cursor
           EXEC SQL
               OPEN EMPCURS
           END-EXEC

      * execute fetch of row using cursor
           EXEC SQL
             FETCH EMPCURS
               INTO :ENO, :LNAME, :FNAME, :DEPT
           END-EXEC.

           GOBACK.

       END PROGRAM WCFD68D.

