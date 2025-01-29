       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_MISC1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           03  GROUP-ONE                      PIC X(01) VALUE 'N'.
               88  VAR1                       VALUE 'N'.
               88  VAR2                       VALUE 'N'.

       PROCEDURE DIVISION.
         PERFORM PROCEDURE_1 
           UNTIL VAR1
               DISPLAY 'NO'.
               
        PROCEDURE_1.
           EVALUATE FIELD-3
              WHEN 8
                 DISPLAY 'TRUE'
              WHEN 2
                 DISPLAY 'FALSE'
              WHEN OTHER
                 DISPLAY 'OTHER'
           END-EVALUATE.
       GOBACK.