       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_Perform4.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
        EVALUATE FIELD-3
              WHEN 8
                 DISPLAY 'TRUE'
                 IF FOO EQ 'BAR' THEN
                  DISPLAY 'TRUE'
                  PERFORM 2000-PROCESSING-PARA
                  UNTIL WS-EOF-LICENSE-ACTIVITY = 'Y' OR
                  WS-FILL-AREA2 = HIGH-VALUES
                 END-IF
              WHEN 2
                 DISPLAY 'FALSE'
              WHEN OTHER
                 DISPLAY 'OTHER'
         END-EVALUATE.
