       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRE2_TECHNICAL4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE ASSIGN TO EMPL
             ORGANIZATION IS INDEXED
             ACCESS IS RANDOM
             RECORD KEY IS EMPL-ID.
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE.
       01 EMPLOYEE-REC.
          05 EMPL-ID       PIC 9(05).
          05 EMPL-NAME     PIC X(15).
       WORKING-STORAGE SECTION.
       01 WS-EMPL.
          05 WS-EMPL-ID       PIC 9(05).
          05 WS-EMPL-NAME     PIC X(15).
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN I-O EMPLOYEE
           MOVE '20000' TO EMPL-ID
      **  Start of Technical Rule:
           READ EMPLOYEE INTO WS-EMPL
             KEY IS EMPL-ID
             INVALID KEY
                DISPLAY 'RECEORD KEY IS INVALID'
             NOT INVALID KEY
                DISPLAY 'REC : "' WS-EMPL '" WILL BE DELETED'
           END-READ.
      **  End of Technical Rule
           
      **  Start of Technical Rule:
           DELETE EMPLOYEE RECORD
             INVALID KEY
                DISPLAY 'RECEORD KEY IS INVALID'
             NOT INVALID KEY
                DISPLAY 'REC DELETION SUCCESSFUL'
           END-DELETE.
      ** End of Technical Rule
           CLOSE EMPLOYEE
           STOP RUN.
