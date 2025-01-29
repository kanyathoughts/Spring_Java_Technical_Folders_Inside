****************************************************************************
******* MMRS7102  adapted
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7102.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7102:'.
      *    ************************************************************

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
       PROCEDURE DIVISION.
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
