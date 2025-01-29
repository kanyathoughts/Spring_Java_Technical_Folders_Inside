       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_SECTION6.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
        IF FOO EQ 'BAR' THEN
           DISPLAY 'TRUE'
        END-IF.
        IF FOO EQ 'BAR' AND BAR EQ 'FOO'THEN
           DISPLAY 'FALSE'
        END-IF.
        IF FOO EQ 'BAR' THEN
          DISPLAY 'TRUE'
         END-IF.
        IF FOO EQ 'BAR' AND BAR EQ 'FOO'THEN
           DISPLAY 'FALSE'
         END-IF.
        IF FOO EQ 'BAR' THEN
           DISPLAY 'TRUE'
          END-IF.
        3100-SECTION-ONE.
         
        SEC1 SECTION
          
        SEC2 SECTION
          