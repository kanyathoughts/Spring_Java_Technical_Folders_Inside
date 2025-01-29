       IDENTIFICATION DIVISION.
       PROGRAM-ID. COCRDLIC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  WS-VARIABLES.
           05  FIRST-NAME              PIC X(10).
           05  FLAG-ON-SW              PIC X(01).
               88 FLAG-ON              VALUE 'Y'.
           05  FLAG-GROUP-SW           PIC X(01).
               88 FLAG-GROUP           VALUE 'N'.
           05  LINE-OF-CD              PIC X(02).
           05  NEW-TYPE-CD             PIC X(01).
           05  ELECTION-TYPE-CD        PIC X(01).

       PROCEDURE DIVISION.
       0000-MAIN.
           IF FIRST-NAME = SPACES
           AND NOT FLAG-ON
           AND NOT FLAG-GROUP
           PERFORM 0100-TEST-CASE
           END-IF.
       0000-EXIT.
           STOP RUN.

       0100-TEST-CASE.
           IF LINE-OF-CD = '01'
              IF ELECTION-TYPE-CD = 'F'
                 MOVE 'B' TO ELECTION-TYPE-CD
              END-IF
              IF ELECTION-TYPE-CD = SPACES
                 MOVE 'A' TO ELECTION-TYPE-CD
              END-IF
           END-IF.

       0100-EXIT.
