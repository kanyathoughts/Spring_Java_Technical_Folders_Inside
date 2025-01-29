****************************************************************************
******** MMRS-M01  MAINFRAME MODERNIZATION REFERENCE SYSTEM
****************************************************************************
******* MMRS7111  READ WORKFILE AND WRITE SEQ. AND VSAMM FILES
******* MMRS7111  RECORDFORMAT OF FILES ARE
******* MMRS7111  F, FB, V, VB, VSAM-ESDS, VSAM-KSDS, VSAM-RRDS
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7111.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYSYSIN      ASSIGN MYSYSIN
                               ORGANIZATION IS SEQUENTIAL
                               FILE STATUS  IS MYSYSIN-STATUS.

           SELECT MYDATIN      ASSIGN MYDATIN
                               FILE STATUS  IS MYDATIN-STATUS.

           SELECT MYFXOUT      ASSIGN MYFXOUT
                               FILE STATUS  IS MYFXOUT-STATUS.

           SELECT MYFBOUT      ASSIGN MYFBOUT
                               FILE STATUS  IS MYFBOUT-STATUS.

           SELECT MYVXOUT      ASSIGN MYVXOUT
                               FILE STATUS  IS MYVXOUT-STATUS.

           SELECT MYVBOUT      ASSIGN MYVBOUT
                               FILE STATUS  IS MYVBOUT-STATUS.

           SELECT MYVSAMK      ASSIGN MYVSAMK
                               ORGANIZATION IS INDEXED
                               ACCESS       IS RANDOM
                               RECORD KEY   IS MYVSAMK-KEY
                               FILE STATUS  IS MYVSAMK-STATUS.
           SELECT MYVSAMR      ASSIGN MYVSAMR
                               ORGANIZATION IS RELATIVE
                               ACCESS       IS RANDOM
                               RELATIVE KEY IS MYVSAMR-RRN
                               FILE STATUS  IS MYVSAMR-STATUS.
           SELECT MYVSAME      ASSIGN SEQUENTIAL-AS-MYVSAME
                               ORGANIZATION IS SEQUENTIAL
                               ACCESS       IS SEQUENTIAL
                               FILE STATUS  IS MYVSAME-STATUS.

       DATA DIVISION.
       FILE SECTION.
      *    ************************************************************
      *    INFO COMING FROM JCL (SYSIN)
       FD  MYSYSIN                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS.
       01  MYSYSIN-RECORD.
           05 MYSYSIN-COMMAND     PIC X(10).
              88 MYSYSIN-COMMAND-F         VALUE 'F         '.
              88 MYSYSIN-COMMAND-FB        VALUE 'FB        '.
              88 MYSYSIN-COMMAND-V         VALUE 'V         '.
              88 MYSYSIN-COMMAND-VB        VALUE 'VB        '.
              88 MYSYSIN-COMMAND-VSAMK     VALUE 'VSAMK     '.
              88 MYSYSIN-COMMAND-VSAME     VALUE 'VSAME     '.
              88 MYSYSIN-COMMAND-VSAMR     VALUE 'VSAMR     '.
              88 MYSYSIN-COMMAND-ALL       VALUE 'ALL       '.
           05 MYSYSIN-REST        PIC X(70).
      *    ************************************************************
      *    DATA TO BE LOADED COMING FROM DATAFILE
       FD  MYDATIN                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS.
       01  MYDATIN-RECORD.
           05 MYDATIN-LEN         PIC X(04).
           05 MYDATIN-ALL.
             10 MYDATIN-KEY.
               15 FILLER          PIC X(06).
               15 MYDATIN-RRN     PIC 9(04).
             10 MYDATIN-DATA      PIC X(66).
      *    ************************************************************
      *    DATAFILE FORMAT=F      (FIXED UNBLOCKED)
       FD  MYFXOUT                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS.
       01  MYFXOUT-RECORD.
           05 MYFXOUT-ALL         PIC X(80).
      *    ************************************************************
      *    DATAFILE FORMAT=FB     (FIXED BLOCKED)
       FD  MYFBOUT                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       01  MYFBOUT-RECORD.
           05 MYFBOUT-ALL         PIC X(80).
      *    ************************************************************
      *    DATAFILE FORMAT=V      (VARIABLE UNBLOCKED)
       FD  MYVXOUT                RECORDING MODE V
                                  LABEL RECORDS STANDARD
                                  RECORD VARYING FROM 1 TO 80
                                  DEPENDING ON MYVXOUT-LEN.
       01  MYVXOUT-RECORD.
           05 MYVXOUT-ALL         PIC X(80).
      *    ************************************************************
      *    DATAFILE FORMAT=VB     (VARIABLE BLOCKED)
       FD  MYVBOUT                RECORDING MODE V
                                  LABEL RECORDS STANDARD
                                  RECORD VARYING FROM 1 TO 80
                                  DEPENDING ON MYVBOUT-LEN
                                  BLOCK CONTAINS 0 CHARACTERS.
       01  MYVBOUT-RECORD.
           05 MYVBOUT-ALL         PIC X(80).

      *    ************************************************************
      *    DATAFILE FORMAT=VSAM-KSDS     (INDEX SEQUENTIAL)
       FD  MYVSAMK                RECORD IS VARYING FROM 15 TO 80
                                  DEPENDING ON MYVSAMK-LEN
                                  LABEL RECORDS ARE STANDARD.
       01  MYVSAMK-RECORD.
           05 MYVSAMK-ALL.
            10 MYVSAMK-LENX       PIC X(04).
            10 MYVSAMK-KEY        PIC X(10).
            10 MYVSAMK-DATA       PIC X(66).

      *    ************************************************************
      *    DATAFILE FORMAT=VSAM-RRDS     (RELATIVE RECORD)
       FD  MYVSAMR                RECORD IS VARYING FROM 15 TO 80
                                  DEPENDING ON MYVSAMR-LEN
                                  LABEL RECORDS ARE STANDARD.
       01  MYVSAMR-RECORD.
           05 MYVSAMR-ALL.
            10 MYVSAMR-LENX       PIC X(04).
            10 FILLER             PIC X(10).
            10 MYVSAMR-DATA       PIC X(66).

      *    ************************************************************
      *    DATAFILE FORMAT=VSAM-ESDS     (SEQUENTIAL)
       FD  MYVSAME                RECORD IS VARYING FROM 15 TO 80
                                  DEPENDING ON MYVSAME-LEN
                                  LABEL RECORDS ARE STANDARD.
       01  MYVSAME-RECORD.
           05 MYVSAME-ALL.
            10 MYVSAME-LENX       PIC X(04).
            10 FILLER             PIC X(10).
            10 MYVSAME-DATA       PIC X(66).

      *    ************************************************************
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7111:'.
      *    ************************************************************
      *    USE COPYBOOKS MMRS710A
           COPY MMRS710A.
      *    ************************************************************
       01  MY-STATUS.
           10 MYSYSIN-STATUS      PIC X(2).
           10 MYDATIN-STATUS      PIC X(2).
           10 MYFXOUT-STATUS      PIC X(2).
           10 MYFBOUT-STATUS      PIC X(2).
           10 MYVXOUT-STATUS      PIC X(2).
           10 MYVXOUT-LEN         PIC 9(4) COMP.
           10 MYVBOUT-STATUS      PIC X(2).
           10 MYVBOUT-LEN         PIC 9(4) COMP.
           10 MYVSAMK-STATUS      PIC X(2).
           10 MYVSAMK-LEN         PIC 9(4) COMP.
           10 MYVSAMR-STATUS      PIC X(2).
           10 MYVSAMR-LEN         PIC 9(4) COMP.
           10 MYVSAME-STATUS      PIC X(2).
           10 MYVSAME-LEN         PIC 9(4) COMP.
       01  MY-COUNTER.
           10 MYDATIN-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYDATIN-DISPLAY     PIC Z(3).Z(3).ZZ9.
           10 MYFXOUT-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYFXOUT-DISPLAY     PIC Z(3).Z(3).ZZ9.
           10 MYFBOUT-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYFBOUT-DISPLAY     PIC Z(3).Z(3).ZZ9.
           10 MYVXOUT-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYVXOUT-DISPLAY     PIC Z(3).Z(3).ZZ9.
           10 MYVBOUT-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYVBOUT-DISPLAY     PIC Z(3).Z(3).ZZ9.
           10 MYVSAMK-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYVSAMK-DISPLAY     PIC Z(3).Z(3).ZZ9.
           10 MYVSAMR-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYVSAMR-DISPLAY     PIC Z(3).Z(3).ZZ9.
           10 MYVSAMR-RRN         PIC 9(4) VALUE ZERO.
           10 MYVSAME-COUNTER     PIC 9(9) VALUE ZERO.
           10 MYVSAME-DISPLAY     PIC Z(3).Z(3).ZZ9.
      *    ************************************************************
       01  WS-NUM6   PIC S9(4) BINARY VALUE 11.
       01  WS-NUM5   PIC S9(4) BINARY VALUE 222.
       01  WS-NUM4   PIC S9(3)V9(2) VALUE 0.
       01  WS-NUM1   PIC S9(3)V9(2) VALUE -123,45.
       01  WS-NUM2   PIC 999V99     VALUE 123,45.
       01  WS-NUM3   PIC -9(3),9(2) VALUE "+123,45".
       01  WS-CHARA  PIC A(6) VALUE 'ABCDEF'.
       01  WS-CHARX  PIC X(5) VALUE 'A121$'.
      *    ************************************************************
       LINKAGE SECTION.
      *    ************************************************************
       PROCEDURE DIVISION.
           DISPLAY 'Project: ' MMRS-M01-GLOBAL-NAME
                   MY-PROGRAM-NAME  ' Start '
      *    --------------------------------------------------
      *    DISPLAY 'Show working storage values '
      *    DISPLAY 'NUM1 : ' WS-NUM1.
      *    DISPLAY 'NUM2 : ' WS-NUM2.
      *    DISPLAY 'NUM3 : ' WS-NUM3.
      *    DISPLAY 'CHARA: ' WS-CHARA.
      *    DISPLAY 'CHARX: ' WS-CHARX.
      *    COMPUTE WS-NUM4 = FUNCTION MAX(WS-NUM1 WS-NUM2)
      *    DISPLAY 'max  : ' WS-NUM4.
      *    MOVE FUNCTION CHAR(222) TO WS-CHARX.
      *    DISPLAY 'char : ' WS-CHARX.
      *    MOVE FUNCTION CHAR(WS-NUM5) TO WS-CHARX.
      *    DISPLAY 'char : ' WS-CHARX.
      *    COMPUTE WS-NUM4 = FUNCTION RANDOM(WS-NUM6).
      *    DISPLAY 'random num5: ' WS-NUM4.
      *    --------------------------------------------------
      *    MAIN CONTROL
           DISPLAY 'Start working with files '
           OPEN INPUT MYSYSIN
           DISPLAY 'MYSYSIN OPEN INPUT=' MYSYSIN-STATUS
           IF MYSYSIN-STATUS = '00'
             READ MYSYSIN
             DISPLAY 'MYSYSIN READ=' MYSYSIN-STATUS
             IF MYSYSIN-STATUS = '00'
               PERFORM CHECK-MYSYSIN-COMMAND
               IF MYSYSIN-STATUS = '00'
                 PERFORM CONTROL-MYDATIN
               END-IF
             END-IF
           END-IF
           CLOSE MYSYSIN
           DISPLAY 'MYSYSIN CLOSE=' MYSYSIN-STATUS

           MOVE  MYDATIN-COUNTER     TO MYDATIN-DISPLAY
           DISPLAY 'MYDATIN Records='   MYDATIN-DISPLAY
           MOVE  MYFXOUT-COUNTER     TO MYFXOUT-DISPLAY
           DISPLAY 'MYFXOUT Records='   MYFXOUT-DISPLAY
           MOVE  MYFBOUT-COUNTER     TO MYFBOUT-DISPLAY
           DISPLAY 'MYFBOUT Records='   MYFBOUT-DISPLAY
           MOVE  MYVXOUT-COUNTER     TO MYVXOUT-DISPLAY
           DISPLAY 'MYVXOUT Records='   MYVXOUT-DISPLAY
           MOVE  MYVBOUT-COUNTER     TO MYVBOUT-DISPLAY
           DISPLAY 'MYVBOUT Records='   MYVBOUT-DISPLAY
           MOVE  MYVSAMK-COUNTER     TO MYVSAMK-DISPLAY
           DISPLAY 'MYVSAMK Records='   MYVSAMK-DISPLAY
           MOVE  MYVSAMR-COUNTER     TO MYVSAMR-DISPLAY
           DISPLAY 'MYVSAMR Records='   MYVSAMR-DISPLAY
           MOVE  MYVSAME-COUNTER     TO MYVSAME-DISPLAY
           DISPLAY 'MYVSAME Records='   MYVSAME-DISPLAY
           .
      *    --------------------------------------------------
      *    SET RETURN CODE FOR JCL
           MOVE 0 TO RETURN-CODE
      *    --------------------------------------------------
           DISPLAY 'Project: ' MMRS-M01-GLOBAL-NAME
                   MY-PROGRAM-NAME  ' End '
           GOBACK.


      *    --------------------------------------------------
      *    PROCESS INPUTDATA
       CONTROL-MYDATIN SECTION.
             OPEN INPUT MYDATIN
             DISPLAY 'MYDATIN OPEN INPUT=' MYDATIN-STATUS
             IF MYDATIN-STATUS = '00'
               PERFORM DO-ALL-OPEN-OUTPUT
               PERFORM PROCESS-MYDATIN UNTIL MYDATIN-STATUS NOT = '00'
               PERFORM DO-ALL-CLOSE-OUTPUT
             END-IF
             CLOSE MYDATIN
             DISPLAY 'MYDATIN CLOSE=' MYDATIN-STATUS
           .
      *    --------------------------------------------------
      *    PROCESS ONE RECORD OF INPUTDATA
       PROCESS-MYDATIN SECTION.
           READ MYDATIN
           IF MYDATIN-STATUS = '00'
             ADD 1 TO MYDATIN-COUNTER
             DISPLAY 'MYDATIN READ=' MYDATIN-STATUS
                   ' rec=' MYDATIN-COUNTER
                   ' len=' MYDATIN-LEN
                   ' data=' MYDATIN-ALL
             PERFORM DO-ALL-WRITE-OUTPUT
           END-IF
           .
      *    --------------------------------------------------
      *    WRITE OUTPUT FILES
       DO-ALL-WRITE-OUTPUT SECTION.
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
               WRITE MYFXOUT-RECORD FROM MYDATIN-RECORD
               ADD 1 TO MYFXOUT-COUNTER
               IF MYFXOUT-STATUS NOT = '00'
                 DISPLAY 'MYFXOUT WRITE=' MYFXOUT-STATUS
               END-IF
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
               WRITE MYFBOUT-RECORD FROM MYDATIN-RECORD
               ADD 1 TO MYFBOUT-COUNTER
               IF MYFBOUT-STATUS NOT = '00'
                 DISPLAY 'MYFBOUT WRITE=' MYFBOUT-STATUS
               END-IF
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-V
               MOVE MYDATIN-LEN TO MYVXOUT-LEN
               WRITE MYVXOUT-RECORD FROM MYDATIN-RECORD
               ADD 1 TO MYVXOUT-COUNTER
               IF MYVXOUT-STATUS NOT = '00'
                 DISPLAY 'MYVXOUT WRITE=' MYVXOUT-STATUS
                         ' LEN=' MYVXOUT-LEN
               END-IF
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VB
               MOVE MYDATIN-LEN TO MYVBOUT-LEN
               WRITE MYVBOUT-RECORD FROM MYDATIN-RECORD
               ADD 1 TO MYVBOUT-COUNTER
               IF MYVBOUT-STATUS NOT = '00'
                 DISPLAY 'MYVBOUT WRITE=' MYVBOUT-STATUS
                         ' LEN=' MYVBOUT-LEN
               END-IF
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VSAMK
               MOVE MYDATIN-LEN TO MYVSAMK-LEN
               WRITE MYVSAMK-RECORD FROM MYDATIN-RECORD
               ADD 1 TO MYVSAMK-COUNTER
               IF MYVSAMK-STATUS NOT = '00'
                 DISPLAY 'MYVSAMK WRITE=' MYVSAMK-STATUS
                         ' LEN=' MYVSAMK-LEN
               END-IF
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VSAMR
               MOVE MYDATIN-LEN TO MYVSAMR-LEN
               MOVE MYDATIN-RRN TO MYVSAMR-RRN
               WRITE MYVSAMR-RECORD FROM MYDATIN-RECORD
               ADD 1 TO MYVSAMR-COUNTER
               IF MYVSAMR-STATUS NOT = '00'
                 DISPLAY 'MYVSAMR WRITE=' MYVSAMR-STATUS
                         ' LEN=' MYVSAMR-LEN
               END-IF
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VSAME
               MOVE MYDATIN-LEN TO MYVSAME-LEN
               WRITE MYVSAME-RECORD FROM MYDATIN-RECORD
               ADD 1 TO MYVSAME-COUNTER
               IF MYVSAME-STATUS NOT = '00'
                 DISPLAY 'MYVSAME WRITE=' MYVSAME-STATUS
                         ' LEN=' MYVSAME-LEN
               END-IF
           END-IF
           .
      *    --------------------------------------------------
      *    OPEN OUTPUT FILES
       DO-ALL-OPEN-OUTPUT SECTION.
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
             OPEN OUTPUT MYFXOUT
             DISPLAY 'MYFXOUT OPEN OUTPUT=' MYFXOUT-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN OUTPUT MYFBOUT
             DISPLAY 'MYFBOUT OPEN OUTPUT=' MYFBOUT-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-V
             OPEN OUTPUT MYVXOUT
             DISPLAY 'MYVXOUT OPEN OUTPUT=' MYVXOUT-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VB
             OPEN OUTPUT MYVBOUT
             DISPLAY 'MYVBOUT OPEN OUTPUT=' MYVBOUT-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VSAMK
             OPEN OUTPUT MYVSAMK
             DISPLAY 'MYVSAMK OPEN OUTPUT=' MYVSAMK-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VSAMR
             OPEN OUTPUT MYVSAMR
             DISPLAY 'MYVSAMR OPEN OUTPUT=' MYVSAMR-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VSAME
             OPEN OUTPUT MYVSAME
             DISPLAY 'MYVSAME OPEN OUTPUT=' MYVSAME-STATUS
           END-IF
           .
      *    --------------------------------------------------
      *    CLOSE OUTPUT FILES
       DO-ALL-CLOSE-OUTPUT SECTION.
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
             CLOSE MYFXOUT
             DISPLAY 'MYFXOUT CLOSE=' MYFXOUT-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             CLOSE MYFBOUT
             DISPLAY 'MYFBOUT CLOSE=' MYFBOUT-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-V
             CLOSE MYVXOUT
             DISPLAY 'MYVXOUT CLOSE=' MYVXOUT-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VB
             CLOSE MYVBOUT
             DISPLAY 'MYVBOUT CLOSE=' MYVBOUT-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VSAMK
             CLOSE MYVSAMK
             DISPLAY 'MYVSAMK CLOSE=' MYVSAMK-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VSAMR
             CLOSE MYVSAMR
             DISPLAY 'MYVSAMR CLOSE=' MYVSAMK-STATUS
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-VSAME
             CLOSE MYVSAME
             DISPLAY 'MYVSAMK CLOSE=' MYVSAME-STATUS
           END-IF
           .
      *    --------------------------------------------------
      *    CHECK FOR VALID SYSIN COMMAND
       CHECK-MYSYSIN-COMMAND SECTION.
           IF MYSYSIN-COMMAND-ALL
           OR MYSYSIN-COMMAND-F
           OR MYSYSIN-COMMAND-FB
           OR MYSYSIN-COMMAND-V
           OR MYSYSIN-COMMAND-VB
           OR MYSYSIN-COMMAND-VSAMK
           OR MYSYSIN-COMMAND-VSAMR
           OR MYSYSIN-COMMAND-VSAME
             DISPLAY 'MYSYSIN valid command=' MYSYSIN-COMMAND
           ELSE
             DISPLAY 'MYSYSIN unsupported command=' MYSYSIN-COMMAND
             MOVE '01' TO MYSYSIN-STATUS
           END-IF
           .
      *    --------------------------------------------------
