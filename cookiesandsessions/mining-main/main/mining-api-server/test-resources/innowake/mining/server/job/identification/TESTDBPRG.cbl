       IDENTIFICATION DIVISION.

       PROGRAM-ID. TESTDBPRG.
       AUTHOR-NAME. ME.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AT.
       OBJECT-COMPUTER. IBM-AT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       EXEC SQL
         INCLUDE EMPREC
       END-EXEC.

       01 DISP-RATE PIC $$$,$$$,$$9.99.
       01 DISP-COM PIC Z.99.
       01 DISP-CODE PIC ----9.
       01 FAKE-CHAR PIC X.
       01 ANSS PIC X.
       01 COM-NULL-IND PIC S9(4) COMP.

       EXEC SQL
         INCLUDE SQLCA
       END-EXEC.

       PROCEDURE DIVISION.

       100-MAIN.
      * declare cursor for select
           EXEC SQL
               DECLARE EMPTBL CURSOR FOR
               SELECT *
                 FROM EMPLOYEE
               ORDER BY LNAME
           END-EXEC

      * open cursor
           EXEC SQL
               OPEN EMPTBL
           END-EXEC
           MOVE SQLCODE TO DISP-CODE
           DISPLAY 'open ' DISP-CODE

      * fetch a data item
           EXEC SQL
               FETCH EMPTBL INTO
                 :ENO,:LNAME,:FNAME,:STREET,:CITY,
                 :ST,:ZIP,:DEPT,:PAYRATE,
                 :COM :COM-NULL-IND
           END-EXEC.

       100-TEST.
           MOVE SQLCODE TO DISP-CODE
           DISPLAY 'fetch ' DISP-CODE

      * loop until no more data
           PERFORM UNTIL SQLCODE < 0 OR SQLCODE = 100

      * display the record
           MOVE PAYRATE TO DISP-RATE
           MOVE COM TO DISP-COM
           DISPLAY 'department ' DEPT
           DISPLAY 'last name ' LNAME
           DISPLAY 'first name ' FNAME
           DISPLAY 'street ' STREET
           DISPLAY 'city ' CITY
           DISPLAY 'state ' ST
           DISPLAY 'zip code ' ZIP
           DISPLAY 'payrate ' DISP-RATE
           IF COM-NULL-IND < 0
               DISPLAY 'commission is null'
           ELSE
               DISPLAY 'commission ' DISP-COM
           END-IF
           DISPLAY 'Do you want to see the next record? (y/n)'
           ACCEPT ANSS
           IF ANSS = 'Y' OR 'y'
               EXEC SQL
                 FETCH EMPTBL INTO
                   :ENO,:LNAME,:FNAME,:STREET,:CITY,
                   :ST,:ZIP,:DEPT,:PAYRATE,
                   :COM :COM-NULL-IND
               END-EXEC
           ELSE
               GO TO CLOSE-LOOP
           END-IF
           MOVE SQLCODE TO DISP-CODE
           DISPLAY 'fetch ' DISP-CODE
           END-PERFORM

           DISPLAY 'All records in this table have been selected'.

       CLOSE-LOOP.
      * close the cursor
           EXEC SQL
               CLOSE EMPTBL
           END-EXEC.

       100-EXIT.
           STOP RUN.
