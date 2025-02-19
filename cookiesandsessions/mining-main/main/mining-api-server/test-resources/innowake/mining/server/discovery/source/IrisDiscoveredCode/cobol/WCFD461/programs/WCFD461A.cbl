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

      * new test case (simple select 2)
           EXEC SQL
              SELECT LNAME, FNAME, PAYRATE, HOURS
              FROM EMPLOYEES
              WHERE DEPT = 'D11'
           END-EXEC
           
      * some DECLARE TABLE test
           EXEC SQL
              DECLARE CANADIAN_SALES TABLE
          (PRODUCT_ITEM   INTEGER,
             MONTH          INTEGER,
             YEAR           INTEGER,
             TOTAL          DECIMAL(9,2)
           END-EXEC
           
      * some DELETE FROM test
           EXEC SQL
              DELETE FROM DEL.TABLE1
                WHERE SOME_VALUE = 100
           END-EXEC
           
       * some DELETE FROM test on Employees
           EXEC SQL
              DELETE FROM EMPLOYEES
                WHERE DEPT = 'D11'
           END-EXEC
      
      * some INSERT test (needs more work, not recognizing Select table)
           EXEC SQL
              INSERT INTO SESSION.TEMPEMPL
            SELECT *
            FROM INS.EMP
            WHERE WORKDEPT='D11'
           END-EXEC
           
      * some LOCK TABLE test
           EXEC SQL
              LOCK TABLE LOCK.TABLE1
                IN EXCLUSIVE MODE
           END-EXEC
      
      * some UPDATE test
           EXEC SQL
              UPDATE UPD.DEPT
          SET MGRNO = :MGR-NUM
          WHERE DEPTNO = :INT-DEPT
           END-EXEC
           
      * declare cursor test
           EXEC SQL
            DECLARE EMPCURS CURSOR FOR
            SELECT LNAME, FNAME, PAYRATE, HOURS
            FROM EMPLOYEE
            WHERE DEPT = :DEPT-NUM
         END-EXEC

         EXEC SQL
            OPEN EMPCURS
         END-EXEC
           
           GOBACK.

       END PROGRAM WCFD68A.

