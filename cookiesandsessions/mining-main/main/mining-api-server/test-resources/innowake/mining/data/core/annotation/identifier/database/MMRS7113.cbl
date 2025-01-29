       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7112.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * declare section
             EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  KSDS-PRIMARY-INDEX  PIC X(10).
             EXEC SQL END DECLARE SECTION END-EXEC.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7112:'.
           COPY MMRS710A.
       01  MY-COUNTER.
           10 MYSQLIN-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYSQLIN-DISPLAY     PIC Z(3).Z(3).ZZ9.
       01  MY-INFO.
           10 MYSQLCA-SQLCODE     PIC -9(9) VALUE ZERO.
           COPY DBSTATEMENT1.
           EXEC SQL
             INCLUDE SQLCA
           END-EXEC
           EXEC SQL
             INCLUDE DBSTATEMENT2
           END-EXEC
       LINKAGE SECTION.
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
