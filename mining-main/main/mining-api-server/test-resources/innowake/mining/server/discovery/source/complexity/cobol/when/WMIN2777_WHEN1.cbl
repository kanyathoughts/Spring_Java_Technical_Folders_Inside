       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_WHEN1.
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
           END-EVALUATE.