           IDENTIFICATION DIVISION.
           PROGRAM-ID. AccountManagement.

           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-ACCOUNT-DETAILS.
              05 WS-ACCOUNT-ID PIC 9(10).
              05 WS-ACCOUNT-NAME PIC X(30).
              05 WS-ACCOUNT-BALANCE PIC 9(10)V99.

           PROCEDURE DIVISION.
           SELECT-ACCOUNT.
              MOVE "1234567890" TO WS-ACCOUNT-ID.
              EXEC SQL
              SELECT ACCOUNT_NAME, ACCOUNT_BALANCE
              INTO :WS-ACCOUNT-NAME, :WS-ACCOUNT-BALANCE
              FROM ACCOUNTS
              WHERE ACCOUNT_ID = :WS-ACCOUNT-ID
              END-EXEC.

           UPDATE-ACCOUNT.
              MOVE "New Account Name" TO WS-ACCOUNT-NAME.
              MOVE 1000 TO WS-ACCOUNT-BALANCE.
              EXEC SQL
              UPDATE ACCOUNTS
              SET ACCOUNT_NAME = :WS-ACCOUNT-NAME,
                        ACCOUNT_BALANCE = :WS-ACCOUNT-BALANCE
              WHERE ACCOUNT_ID = :WS-ACCOUNT-ID
              END-EXEC.

           DELETE-ACCOUNT.
              EXEC SQL
              DELETE FROM ACCOUNTS
              WHERE ACCOUNT_ID = :WS-ACCOUNT-ID
              END-EXEC.

           STOP RUN.
