       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_MISC2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           03  GROUP-ONE                      PIC X(01) VALUE 'N'.
               88  VAR1                       VALUE 'N'.
               88  VAR2                       VALUE 'N'.
       PROCEDURE DIVISION.
        IF FOO EQ 'BAR' THEN
           DISPLAY 'TRUE'
        END-IF.
        IF FOO EQ 'BAR' AND BAR EQ 'FOO'THEN
           DISPLAY 'FALSE'
        END-IF.
        PERFORM 2000-PROCESSING-PARA
         UNTIL WS-EOF-LICENSE-ACTIVITY = 'Y' OR
         VAR1 AND VAR2
           DISPLAY 'NO'.
           GO TO SEC1 DEPENDING ON CONDITION.
        3100-SECTION-ONE.
         IF FOO EQ 'BAR' THEN
          DISPLAY 'TRUE'
           EVALUATE FIELD-3
              WHEN 8
                 DISPLAY 'TRUE'
              WHEN 2
                 DISPLAY 'FALSE'
              WHEN OTHER
                 DISPLAY 'OTHER'
           END-EVALUATE.
         END-IF.
        SEC1 SECTION
          IF FOO EQ 'BAR' AND BAR EQ 'FOO'THEN
           DISPLAY 'FALSE'
          END-IF.
        SEC2 SECTION
          IF FOO EQ 'BAR' THEN
           DISPLAY 'TRUE'
          END-IF.