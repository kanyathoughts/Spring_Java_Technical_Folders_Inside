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
           COPY MMRS710B.
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
      *    --------------------------------------------------
      *    SET RETURN CODE FOR JCL
           MOVE 0 TO RETURN-CODE
      *    --------------------------------------------------
           DISPLAY 'Project: ' MMRS-M01-GLOBAL-NAME
                   MY-PROGRAM-NAME  ' End '
           GOBACK.