****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7112.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
      *
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7112:'.
      *    ************************************************************
      *    USE COPYBOOKS MMRS710A
           COPY MMRS710A.
      *    ************************************************************
       01  MY-COUNTER.
           10 MYSQLIN-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYSQLIN-DISPLAY     PIC Z(3).Z(3).ZZ9.
       01  MY-INFO.
           10 MYSQLCA-SQLCODE     PIC -9(9) VALUE ZERO.
      *    ************************************************************
           EXEC SQL
               SELECT    KSDS_PRIMARY_INDEX~~
               FROM      MMRS00C_AWA_VSAMK AS VSAMK
               ORDER BY  VSAMK.KSDS_PRIMARY_INDEX ASC
               FOR       FETCH ONLY
           END-EXEC
      *    ************************************************************
       LINKAGE SECTION.
      *    ************************************************************
       PROCEDURE DIVISION.
           DISPLAY 'Project: ' MMRS-M01-GLOBAL-NAME
                   MY-PROGRAM-NAME  ' Start '
      *    --------------------------------------------------
      *    MAIN CONTROL
           EXEC SQL
           UPDATE EMPLOYEE`
           SET SALARY = SALARY * :PERCENTAGE
           WHERE COMM >= :COMMISSION
           END-EXEC

           EXEC SQL
           INSERT INTO T1 VALUES ('abc', 10)^
           END-EXEC

           EXEC SQL
           DELETE FROM C-VSAMK''
           WHERE COMM >= :COMMISSION
           END-EXEC
           .
      *    --------------------------------------------------
