****************************************************************************
******** MMRS-M01  MAINFRAME MODERNIZATION REFERENCE SYSTEM
****************************************************************************
******** MMRS7101    USE COPYCODES WITHIN COBOL SOURCE
******** MMRS7101    WRITE INFORMATION WITH DISPLAY TO SPOOLFILE IN JES
******** MMRS7101    TRANSLATE CHARACTER INTO HEX VALUES (EBCDIC)
******** MMRS7101    SET RETURNCODE FOR USAGE IN JCL
******** MMRS7101    SHOW DIFFERENT BEHAVIOR OF TRUNC (BIN) TO (STD)
******** MMRS7101    TEST MEE INSERT TO ADD JAVA INTO COBOL SOURCE
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7101.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7101:'.
      *    ************************************************************
      *    test truncation of binary fields. Default is TRUNC(STD)
       01  TRUNC-TEST-DISP    PIC -99 VALUE ZERO.
       01  MY-BIN-FIELDS BINARY.
           05  TRUNC-TEST              PIC S9        VALUE ZERO.
      *    ************************************************************
      *    USE ONE OF THE COPYBOOKS MMRS710A OR MMRS710B OR MMRS710C
           COPY MMRS710A.
           05 MY-COPY-NAME PIC X(10) VALUE 'MMRS710A: '.
      *    COPY MMRS710B.
      *    COPY MMRS710C.
      *    ************************************************************
       01  MY-EBCDIC-CHECK-1     PIC X(8) VALUE '1'.
       01  MY-EBCDIC-CHECK-A     PIC X(8) VALUE 'A'.
       01  MY-HEX-TRANS          PIC X(8) VALUE 'MMRS71Z1'.
       01  MY-HEX-ORIGIN-LEN     PIC 9(5).
       01  MY-HEX-ORIGIN         PIC X(100).
       01  MY-HEX-CONV-RESULT    PIC X(200).
       01  MY-ADD-SOURCE     PIC 9(5)V9(2) VALUE 12345.11.
       01  MY-ADD-TARGET     PIC 9(5)V9(2) VALUE ZERO.
      *    ************************************************************
       LINKAGE SECTION.
       PROCEDURE DIVISION.
      * ++++++ test annotation
      * <$ MEE INSERT
      * System.out.println("MEE INSERT: java in cobol source ");
      * $>
           DISPLAY 'Project: ' MMRS-M01-GLOBAL-NAME
           DISPLAY MY-PROGRAM-NAME  ' Start '
      *    ************************************************************
      *    THIS WORKS IF YOU ACTIVATE COPYCODE MMRS7101B
      *    DISPLAY '-COPYBOOK USED: ' MY-COPY-NAME IN MMRS7101B
           DISPLAY MY-PROGRAM-NAME  ' use copy: ' MY-COPY-NAME
      *    ************************************************************
      *    USE SUBPROG TO SHOW HEX VALUES OF CHARACTERS
           MOVE 10         TO MY-HEX-ORIGIN-LEN
           MOVE 'ABCDEF '  TO MY-HEX-ORIGIN
           MOVE SPACES     TO MY-HEX-CONV-RESULT
           DISPLAY MY-PROGRAM-NAME
           ' char=>' MY-HEX-ORIGIN(1:10) '<'

      *    USE SUBPROG TO SHOW HEX VALUES OF NUMBERS
      * <$ MEE INSERT
      * System.out.println("hexTrans: in=MMRS0101" +
      * "hexTrans: len="+MY_HEX_ORIGIN_LEN);
      * $>
           CALL MY-HEX-TRANS USING MY-HEX-ORIGIN-LEN, MY-HEX-ORIGIN,
                                   MY-HEX-CONV-RESULT
           DISPLAY MY-PROGRAM-NAME
           ' hex =>' MY-HEX-CONV-RESULT(1:20) '<'
           MOVE 10         TO MY-HEX-ORIGIN-LEN
           MOVE '123456 '  TO MY-HEX-ORIGIN
           MOVE SPACES     TO MY-HEX-CONV-RESULT
           DISPLAY MY-PROGRAM-NAME
           ' char=>' MY-HEX-ORIGIN(1:10) '<'
           CALL MY-HEX-TRANS USING MY-HEX-ORIGIN-LEN, MY-HEX-ORIGIN,
                                   MY-HEX-CONV-RESULT
           DISPLAY MY-PROGRAM-NAME
           ' hex =>' MY-HEX-CONV-RESULT(1:20) '<'

      *    COMPARE CHARACTERS AND NUMBERS TO CHECK FOR EBCDIC CODEPAGE
           IF MY-EBCDIC-CHECK-1 GREATER MY-EBCDIC-CHECK-A
             DISPLAY MY-PROGRAM-NAME
             ' 1>A => OK EBCDIC environment active '
           ELSE
             DISPLAY MY-PROGRAM-NAME
             ' 1<A => ERROR we are working not like a host environment'
             ' This is a problem !!!!!!   '
           END-IF

      *    DISPLAY SOME SPECIAL CHARACTERS ÄÖÜ @ € S.O.
           DISPLAY MY-PROGRAM-NAME '!"§$%&/()=?¡“¶¢[]|{}'
           DISPLAY MY-PROGRAM-NAME '€€€€€€€ (EURO)'
           DISPLAY MY-PROGRAM-NAME 'äöüÄÖÜ ;:_*+#-.,'

      *    ADD VALUE TO TARGET FIELD
           MOVE ZERO TO MY-ADD-TARGET
           ADD MY-ADD-SOURCE TO MY-ADD-TARGET
           DISPLAY MY-PROGRAM-NAME
             ' SOURCE=' MY-ADD-SOURCE
             ' TARGET=' MY-ADD-TARGET
             ' - SHOULD BE THE SAME VALUE '
      *    ************************************************************
      *    COBOL FLOW TEST
           DISPLAY MY-PROGRAM-NAME 'my-flow      Test start '
           DISPLAY MY-PROGRAM-NAME 'PERFORM MY-FLOW-01 '
           PERFORM MY-FLOW-01
           DISPLAY MY-PROGRAM-NAME 'PERFORM MY-FLOW-01 THRU MY-FLOW-02'
           PERFORM MY-FLOW-01 THRU MY-FLOW-02
           DISPLAY MY-PROGRAM-NAME 'PERFORM MY-FLOW-01 THRU MY-FLOW-03'
           PERFORM MY-FLOW-01 THRU MY-FLOW-03
           DISPLAY MY-PROGRAM-NAME 'now continue without perform'
           .
       MY-FLOW-01.
           DISPLAY MY-PROGRAM-NAME 'my-flow-01   Label 01    '
           .
       MY-FLOW-02.
           DISPLAY MY-PROGRAM-NAME 'my-flow-02   Label 02    '
           .
           EXIT.
       MY-FLOW-03.
           DISPLAY MY-PROGRAM-NAME 'my-flow-03   Label 03    '
           .
       MY-FLOW-END.
           DISPLAY MY-PROGRAM-NAME 'my-flow-end  Test end       '
           .

      *    ************************************************************
      *    TRUNC BIN/STD TEST
           PERFORM TRUNCTEST

      *    ************************************************************
      *    SET RETURN CODE FOR JCL
           MOVE 2 TO RETURN-CODE
      *    ************************************************************
           DISPLAY MY-PROGRAM-NAME  ' End   '
           GOBACK.
      *    ************************************************************
       TRUNCTEST SECTION.
           DISPLAY MY-PROGRAM-NAME
                   'TRUNC(BIN/STD): move 9 to TRUNC-TEST'
           MOVE 9 TO TRUNC-TEST
           MOVE TRUNC-TEST TO TRUNC-TEST-DISP
           DISPLAY MY-PROGRAM-NAME
                   'TRUNC(BIN/STD): read TRUNC-TEST='
                   TRUNC-TEST-DISP
           DISPLAY MY-PROGRAM-NAME
                   'TRUNC(BIN/STD): move 10 to TRUNC-TEST'
           MOVE 10 TO TRUNC-TEST
           MOVE TRUNC-TEST TO TRUNC-TEST-DISP
           DISPLAY MY-PROGRAM-NAME
                   'TRUNC(BIN/STD): read TRUNC-TEST='
                   TRUNC-TEST-DISP
           IF TRUNC-TEST = 10
             DISPLAY MY-PROGRAM-NAME 'TRUNC=BIN is active'
           ELSE
             DISPLAY MY-PROGRAM-NAME 'TRUNC=STD is active'
           END-IF
           .
      *    ************************************************************
      
