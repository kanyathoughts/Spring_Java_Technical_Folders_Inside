       IDENTIFICATION DIVISION.
       PROGRAM-ID. IFELSE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTFIELD PIC X(80).
       01 PARENT1 PIC X VALUE SPACE.
          88  FLAG-ENABLED   VALUE "E".
          88  FLAG-DISABLED  VALUE "D".
       01 PARENT2 PIC X VALUE SPACE.
          88  FLAG-ENABLED   VALUE "E".
          88  FLAG-DISABLED  VALUE "D".
       01 PARENT3 PIC X VALUE SPACE.
          88  FLAG-ENABLED   VALUE "E".

       PROCEDURE DIVISION.

      * No candidate: only 1 FIELD_REFERENCE which is not from a Copybook
           IF PARENT3.FLAG-ENABLED = TRUE
              DISPLAY 'if 1'
           END-IF.

      * Candidate: at least two FIELD_REFERENCEs
           IF PARENT3.FLAG-ENABLED = PARENT1.FLAG-ENABLED
              DISPLAY 'if 2'
           ELSE
              DISPLAY 'if not 2'
           END-IF.

      * Candidate: at least two CONSTANT_REFERENCEs
           IF 'y' = 'Y'
              DISPLAY 'if 4'
           END-IF.

           DISPLAY '5' '6' PARENT1.FLAG-ENABLED PARENT3.FLAG-ENABLED.

       END PROGRAM TEST.
