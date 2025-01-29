       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_MISC4.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           03  GROUP-ONE                      PIC X(01) VALUE 'N'.
               88  VAR1                       VALUE 'N'.
       PROCEDURE DIVISION.
        EVALUATE FIELD-3
              WHEN 8
                 DISPLAY 'TRUE'
                 IF FOO EQ 'BAR'  AND VAR1 THEN
                  DISPLAY 'TRUE'
                  PERFORM 2000-PROCESS
                  UNTIL WS-EOF-LICENSE-ACTIVITY = 'Y' OR
                  WS-FILL-AREA2 = HIGH-VALUES
                 END-IF
              WHEN 2
                 DISPLAY 'FALSE'
                 GO TO PARA-B IN SECTION-A
              WHEN OTHER
                 DISPLAY 'OTHER'
           END-EVALUATE.
           
       SECTION-A SECTION.
         PARA-B.
           DISPLAY 'IN PARA-D'
           GOBACK.