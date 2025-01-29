       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCFD459A.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
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

      * new test case (simple select 2)
           EXEC SQL
              SELECT LNAME, FNAME, PAYRATE, HOURS
              FROM EMPLOYEE
              WHERE DEPT = :ALPHA-SHORT
           END-EXEC.
           
      * some DELETE FROM test on Employees
           EXEC SQL
              DELETE FROM EMPLOYEE
              WHERE DEPT = :ALPHA-SHORT
           END-EXEC.
      
      * some UPDATE test
           EXEC SQL
              UPDATE UPD.DEPT
              SET MGRNO = :ALPHA-SHORT
              WHERE DEPTNO = '2345678'
           END-EXEC.
           
      * declare cursor test
           EXEC SQL
              DECLARE EMPCURS CURSOR FOR
              SELECT LNAME, FNAME, PAYRATE, HOURS
              FROM EMPLOYEE
              WHERE DEPT = :ALPHA-SHORT
           END-EXEC.
           
           GOBACK.

       END PROGRAM WCFD68A.

