****************************************************************************
******** MMRS-M01  MAINFRAME MODERNIZATION REFERENCE SYSTEM
****************************************************************************
******* MMRS7112  READ SQL AND SHOW DATA WITH DISPLAY
****************************************************************************
******* MMRS7112  CAUTION: PERFORM UNTIL CAUSES A MIGRATION ERROR
******* MMRS7112           ACTIVATE "WITH TEST AFTER" FOR MIGRATION
****************************************************************************
******* MMRS7112  CAUTION: AT THE MOMEMT THIS PROGRAM IS NOT WORKING
******* MMRS7112           WITH ORACLE.
******* MMRS7112  OPTION CURSOR OVER COMMIT IS NOT YET IMPLEMENTED
******* MMRS7112  PACKAGE INNOWAKE.MEE.COBOL.RUNTIME.SQL;
******* MMRS7112  SQLSTATEMENT.JAVA
******* MMRS7112      PROTECTED INT GETRESULTSETHOLDABILITY() {
******* MMRS7112  //      RETURN RESULTSET.CLOSE_CURSORS_AT_COMMIT;
******* MMRS7112          RETURN RESULTSET.HOLD_CURSORS_OVER_COMMIT;
******* MMRS7112      }

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
      * declare section
             EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  KSDS-PRIMARY-INDEX  PIC X(10).
             EXEC SQL END DECLARE SECTION END-EXEC.
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
           EXEC SQL DECLARE MMRS00C_AWA_VSAMK TABLE
             (
               KSDS_PRIMARY_INDEX   VARCHAR(10)
             )
           END-EXEC

      *    ************************************************************
           EXEC SQL
             INCLUDE SQLCA
           END-EXEC
      *    ************************************************************
           EXEC SQL
             DECLARE   C-VSAMK    CURSOR FOR
               SELECT    KSDS_PRIMARY_INDEX
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
           EXEC SQL OPEN C-VSAMK END-EXEC
           MOVE  SQLCODE         TO  MYSQLCA-SQLCODE
           DISPLAY 'C-VSAMK OPEN CURSOR=' MYSQLCA-SQLCODE
           IF MYSQLCA-SQLCODE = ZERO
             PERFORM FETCH-C-VSAMK
           END-IF
      *        UNTIL MYSQLCA-SQLCODE NOT = ZERO
           EXEC SQL CLOSE C-VSAMK END-EXEC
           MOVE  SQLCODE        TO  MYSQLCA-SQLCODE
           DISPLAY 'C-VSAMK CLOSE=' MYSQLCA-SQLCODE

           MOVE  MYSQLIN-COUNTER     TO MYSQLIN-DISPLAY
           DISPLAY 'C-VSAMK Records='   MYSQLIN-DISPLAY
           .
      *    --------------------------------------------------
      *    SET RETURN CODE FOR JCL
           MOVE 0 TO RETURN-CODE
      *    --------------------------------------------------
           DISPLAY 'Project: ' MMRS-M01-GLOBAL-NAME
                   MY-PROGRAM-NAME  ' End '
           GOBACK.


      *    --------------------------------------------------
      *    FETCH ONE RECORD OF C-VSAMK
       FETCH-C-VSAMK SECTION.
           EXEC SQL
             FETCH  C-VSAMK
             INTO :KSDS-PRIMARY-INDEX
           END-EXEC
           MOVE  SQLCODE        TO  MYSQLCA-SQLCODE
           IF SQLCODE = ZERO
             ADD 1 TO MYSQLIN-COUNTER
             DISPLAY 'C-VSAMK FETCH=' MYSQLCA-SQLCODE
                   ' rec=' MYSQLIN-COUNTER
                   ' data=' KSDS-PRIMARY-INDEX
           ELSE
             DISPLAY 'C-VSAMK FETCH SQLCODE=' MYSQLCA-SQLCODE
           END-IF
           .
      *    --------------------------------------------------
