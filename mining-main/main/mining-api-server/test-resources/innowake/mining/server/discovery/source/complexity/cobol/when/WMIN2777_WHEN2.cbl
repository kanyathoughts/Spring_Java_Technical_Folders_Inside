       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_WHEN2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           EVALUATE FIELD-3
              WHEN 8
                 DISPLAY 'TRUE'
              WHEN 2
                 DISPLAY 'FALSE'
              WHEN OTHER
                 DISPLAY 'OTHER'
                 IF FOO EQ 'BAR' THEN
                  DISPLAY 'TRUE'
                 END-IF
           END-EVALUATE.