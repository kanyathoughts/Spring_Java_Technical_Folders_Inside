       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_Perform5.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
        IF FOO EQ 'BAR' THEN
         DISPLAY 'TRUE'
         PERFORM 2000-PROCESSING-PARA
        END-IF.