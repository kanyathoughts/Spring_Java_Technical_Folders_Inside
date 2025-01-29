  CBL****************************************************************************
******** MMRS-M01  mainframe modernization reference system
****************************************************************************
******* MMRS7102  call assembler
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7102.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7102:'.
           COPY MMRS710A.
      *    ************************************************************
       01  MY-HEX-TRANS           PIC X(8) VALUE 'MMRS71Z1'.
       01  MY-HEX-ORIGIN-LEN      PIC 9(5).
       01  MY-HEX-CONV-RESULT     PIC X(200).
       01  MMRS7ZA1               PIC X(08) VALUE 'MMRS7ZA1'.
       01  MMRS71Z2               PIC X(08) VALUE 'MMRS71Z2'.
       01  OLD-CPU                PIC 9(18).
       01  DIFF-CPU               PIC 9(12).9(6).
       01  ASSTIMEDATE.
           10 ASSTIME-HH          PIC X(2).
           10 ASSTIME-MM          PIC X(2).
           10 ASSTIME-SS          PIC X(2).
           10 ASSTIME-REST        PIC X(6).
           10 ASSDATE-YYYY        PIC X(4).
           10 ASSDATE-MM          PIC X(2).
           10 ASSDATE-DD          PIC X(2).
           10 ASSCPU              PIC 9(18).
       01  DISPDATETIME.
           10 ASSDATE-DD          PIC X(2).
           10 FILLER              PIC X(1) VALUE '/'.
           10 ASSDATE-MM          PIC X(2).
           10 FILLER              PIC X(1) VALUE '/'.
           10 ASSDATE-YYYY        PIC X(4).
           10 FILLER              PIC X(1) VALUE '/'.
           10 ASSTIME-HH          PIC X(2).
           10 FILLER              PIC X(1) VALUE ':'.
           10 ASSTIME-MM          PIC X(2).
           10 FILLER              PIC X(1) VALUE ':'.
           10 ASSTIME-SS          PIC X(2).
           10 FILLER              PIC X(1) VALUE ':'.
           10 ASSTIME-REST        PIC X(6).
           10 FILLER              PIC X(7) VALUE ' - CPU:'.
           10 CPUTIME-SEC         PIC 9(18).

       01  ASSPAR1.
           10 PAREYET             PIC X(4) VALUE 'TIM:'.
           10 RC1                 PIC S9(9) BINARY VALUE ZERO.
           10 TIMEDATE1           PIC X(20) VALUE SPACES.
           10 PAREYEC             PIC X(4) VALUE 'CPU:'.
           10 RC2                 PIC S9(9) BINARY VALUE ZERO.
           10 CPUTIME             PIC S9(18) BINARY VALUE ZERO.

       01  DISPPAR1-RC            PIC ZZZZZ9.
       01  DISPPAR1.
           10 PAREYET             PIC X(4) VALUE 'TIM:'.
           10 RC1                 PIC -9(9)  VALUE ZERO.
           10 TIMEDATE1           PIC X(20) VALUE SPACES.
           10 PAREYEC             PIC X(4) VALUE 'CPU:'.
           10 RC2                 PIC -9(9)  VALUE ZERO.
           10 CPUTIME             PIC -9(18)  VALUE ZERO.

       01  ASSPAR2.
           10 PAREYET             PIC X(4) VALUE 'TIM:'.
           10 RC1                 PIC S9(9) BINARY VALUE ZERO.
           10 TIMEDATE1           PIC X(20) VALUE SPACES.
           10 PAREYEC             PIC X(4) VALUE 'CPU:'.
           10 RC2                 PIC S9(9) BINARY VALUE ZERO.
           10 CPUTIME             PIC S9(18) BINARY VALUE ZERO.

       01  DISPPAR2-RC            PIC ZZZZZ9.
       01  DISPPAR2.
           10 PAREYET             PIC X(4) VALUE 'TIM:'.
           10 RC1                 PIC -9(9)  VALUE ZERO.
           10 TIMEDATE1           PIC X(20) VALUE SPACES.
           10 PAREYEC             PIC X(4) VALUE 'CPU:'.
           10 RC2                 PIC -9(9)  VALUE ZERO.
           10 CPUTIME             PIC -9(18)  VALUE ZERO.

       01  I1                     PIC 9(5) COMP-3.
       01  MY-CPU-KILLER-TEST     PIC X(1).
           88  MY-CPU-KILLER-TEST-1     VALUE '1'.
           88  MY-CPU-KILLER-TEST-2     VALUE '2'.
           88  MY-CPU-KILLER-TEST-3     VALUE '3'.
           88  MY-CPU-KILLER-TEST-4     VALUE '4'.
           88  MY-CPU-KILLER-TEST-5     VALUE '5'.
       01  MY-CPU-KILLER.
           05 MY-CPU-KILLER-1.
             10  KILL1            PIC X(32000).
           05 MY-CPU-KILLER-2.
             10  KILL2            PIC X(32000).

      *    ************************************************************
       LINKAGE SECTION.
      *    ************************************************************
       PROCEDURE DIVISION.
           DISPLAY 'Project: ' MMRS-M01-GLOBAL-NAME
                   MY-PROGRAM-NAME  ' Start '
      *    --------------------------------------------------
      *    main control

           DISPLAY 'USE=' MMRS71Z2
           CALL MMRS71Z2 USING ASSTIMEDATE
           MOVE CORRESPONDING ASSTIMEDATE TO DISPDATETIME
           MOVE ASSCPU TO CPUTIME-SEC
           SUBTRACT OLD-CPU FROM ASSCPU GIVING DIFF-CPU
           MOVE ASSCPU  TO OLD-CPU
           DISPLAY DISPDATETIME ' cpu-time start=' DIFF-CPU

           MOVE '1' TO MY-CPU-KILLER-TEST
           PERFORM USECPU

           DISPLAY 'USE=' MMRS7ZA1 'data=' MY-CPU-KILLER(1:80)
           PERFORM ASSCALL1
           MOVE TIMEDATE1 OF ASSPAR1 TO ASSTIMEDATE
           MOVE CORRESPONDING ASSTIMEDATE TO DISPDATETIME
           MOVE CPUTIME OF ASSPAR1 TO CPUTIME-SEC
           SUBTRACT OLD-CPU FROM CPUTIME OF ASSPAR1 GIVING DIFF-CPU
           MOVE CPUTIME OF ASSPAR1  TO OLD-CPU
           DISPLAY DISPDATETIME ' cpu-time  used=' DIFF-CPU

           MOVE '2' TO MY-CPU-KILLER-TEST
           PERFORM USECPU

           DISPLAY 'USE=' MMRS7ZA1 'data=' MY-CPU-KILLER(1:80)
           PERFORM ASSCALL2
           MOVE TIMEDATE1 OF ASSPAR2 TO ASSTIMEDATE
           MOVE CORRESPONDING ASSTIMEDATE TO DISPDATETIME
           MOVE CPUTIME OF ASSPAR2 TO CPUTIME-SEC
           SUBTRACT OLD-CPU FROM CPUTIME OF ASSPAR2 GIVING DIFF-CPU
           MOVE CPUTIME OF ASSPAR2  TO OLD-CPU
           DISPLAY DISPDATETIME ' cpu-time  used=' DIFF-CPU

           MOVE '3' TO MY-CPU-KILLER-TEST
           PERFORM USECPU

           DISPLAY 'USE=' MMRS7ZA1 'data=' MY-CPU-KILLER(1:80)
           PERFORM ASSCALL2
           MOVE TIMEDATE1 OF ASSPAR2 TO ASSTIMEDATE
           MOVE CORRESPONDING ASSTIMEDATE TO DISPDATETIME
           MOVE CPUTIME OF ASSPAR2 TO CPUTIME-SEC
           SUBTRACT OLD-CPU FROM CPUTIME OF ASSPAR2 GIVING DIFF-CPU
           MOVE CPUTIME OF ASSPAR2  TO OLD-CPU
           DISPLAY DISPDATETIME ' cpu-time  used=' DIFF-CPU

           MOVE '4' TO MY-CPU-KILLER-TEST
           PERFORM USECPU

           DISPLAY 'USE=' MMRS7ZA1 'data=' MY-CPU-KILLER(1:80)
           PERFORM ASSCALL2
           MOVE TIMEDATE1 OF ASSPAR2 TO ASSTIMEDATE
           MOVE CORRESPONDING ASSTIMEDATE TO DISPDATETIME
           MOVE CPUTIME OF ASSPAR2 TO CPUTIME-SEC
           SUBTRACT OLD-CPU FROM CPUTIME OF ASSPAR2 GIVING DIFF-CPU
           MOVE CPUTIME OF ASSPAR2  TO OLD-CPU
           DISPLAY DISPDATETIME ' cpu-time  used=' DIFF-CPU


           .
      *    --------------------------------------------------
      *    pass return of assembler code to jcl
      *    --------------------------------------------------
           DISPLAY 'Project: ' MMRS-M01-GLOBAL-NAME
                   MY-PROGRAM-NAME  ' End '
           GOBACK.


      *    --------------------------------------------------
      *    use cpu
       USECPU SECTION.

           EVALUATE TRUE
            WHEN   MY-CPU-KILLER-TEST-1
               DISPLAY 'USECPU: ' MMRS-M01-GLOBAL-NAME
                        MY-PROGRAM-NAME  ' Test 1 '
            WHEN   MY-CPU-KILLER-TEST-2
               DISPLAY 'USECPU: ' MMRS-M01-GLOBAL-NAME
                        MY-PROGRAM-NAME  ' Test 2 '
            WHEN   MY-CPU-KILLER-TEST-3
               DISPLAY 'USECPU: ' MMRS-M01-GLOBAL-NAME
                        MY-PROGRAM-NAME  ' Test 3 '
            WHEN   MY-CPU-KILLER-TEST-4
               DISPLAY 'USECPU: ' MMRS-M01-GLOBAL-NAME
                        MY-PROGRAM-NAME  ' Test 4 '
            WHEN   MY-CPU-KILLER-TEST-5
               DISPLAY 'USECPU: ' MMRS-M01-GLOBAL-NAME
                        MY-PROGRAM-NAME  ' Test 5 '
            WHEN OTHER
               DISPLAY 'USECPU: ' MMRS-M01-GLOBAL-NAME
                        MY-PROGRAM-NAME  ' Test unknown '
           END-EVALUATE


           PERFORM VARYING I1
            FROM   1
            BY     1
            UNTIL  I1 > 60000
              MOVE MY-CPU-KILLER-1 TO MY-CPU-KILLER-2
           END-PERFORM

           .


      *    --------------------------------------------------
      *    call assembler
       ASSCALL1 SECTION.
             MOVE CORRESPONDING ASSPAR1 TO DISPPAR1
             PERFORM ASSHEX1
             CALL MMRS7ZA1 USING ASSPAR1
             IF RETURN-CODE NOT = ZERO
               MOVE RETURN-CODE TO DISPPAR1-RC
               DISPLAY 'AFTER =' MMRS7ZA1 ' RC=' DISPPAR1-RC
             END-IF
             MOVE CORRESPONDING ASSPAR1 TO DISPPAR1
             PERFORM ASSHEX1
           .

      *    --------------------------------------------------
      *    display results in hex
       ASSHEX1 SECTION.
           MOVE LENGTH OF ASSPAR1   TO MY-HEX-ORIGIN-LEN
           MOVE SPACES              TO MY-HEX-CONV-RESULT
           CALL MY-HEX-TRANS USING MY-HEX-ORIGIN-LEN, ASSPAR1,
                                   MY-HEX-CONV-RESULT
           .
      *    --------------------------------------------------

      *    --------------------------------------------------
      *    call assembler
       ASSCALL2 SECTION.
             MOVE CORRESPONDING ASSPAR2 TO DISPPAR2
             PERFORM ASSHEX2
             CALL MMRS7ZA1 USING ASSPAR2
             IF RETURN-CODE NOT = ZERO
               MOVE RETURN-CODE TO DISPPAR2-RC
               DISPLAY 'AFTER =' MMRS7ZA1 ' RC=' DISPPAR2-RC
             END-IF
             MOVE CORRESPONDING ASSPAR2 TO DISPPAR2
             PERFORM ASSHEX2
           .

      *    --------------------------------------------------
      *    display results in hex
       ASSHEX2 SECTION.
           MOVE LENGTH OF ASSPAR2   TO MY-HEX-ORIGIN-LEN
           MOVE SPACES              TO MY-HEX-CONV-RESULT
           CALL MY-HEX-TRANS USING MY-HEX-ORIGIN-LEN, ASSPAR2,
                                   MY-HEX-CONV-RESULT
           .
      *    --------------------------------------------------
