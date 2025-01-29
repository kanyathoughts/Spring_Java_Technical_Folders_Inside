       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULEXIFELSE.
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

      * Candidate: Multiple expression If else condition
           IF PARENT3.FLAG-ENABLED = TRUE AND PARENT1.FLAG-ENABLED = FALSE
              DISPLAY 'if 1'
           END-IF.

      * Candidate: Multiple expression If else condition
           IF PARENT3.FLAG-ENABLED = TRUE OR PARENT1.FLAG-ENABLED = FALSE
              DISPLAY 'if 2'
           END-IF.

           DISPLAY '5' '6' PARENT1.FLAG-ENABLED PARENT3.FLAG-ENABLED.

       END PROGRAM TEST.
