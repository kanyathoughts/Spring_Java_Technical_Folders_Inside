       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_SECTION2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
        SEC1 SECTION
          IF FOO EQ 'BAR' THEN
           DISPLAY 'TRUE'
          IF FOO EQ 'BAR' AND BAR EQ 'FOO'THEN
           DISPLAY 'FALSE'
          END-IF.
         END-IF.
        SEC2 SECTION
