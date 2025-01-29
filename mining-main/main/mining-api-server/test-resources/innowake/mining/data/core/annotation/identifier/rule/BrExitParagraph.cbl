       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTFIELD X(80).
       01 PARENT1 PIC X VALUE SPACE.
          88  FLAG-ENABLED   VALUE "E".
          88  FLAG-DISABLED  VALUE "D".
       01 PARENT2 PIC X VALUE SPACE.
          88  FLAG-ENABLED   VALUE "E".
          88  FLAG-DISABLED  VALUE "D".
       01 PARENT3 PIC X VALUE SPACE.
          88  FLAG-ENABLED   VALUE "E".

       PROCEDURE DIVISION.
           MAINLINE.

      * Candidate: at least two FIELD_REFERENCEs
              IF PARENT3.FLAG-ENABLED = PARENT1.FLAG-ENABLED
                 DISPLAY 'if 2'
              END-IF.

           100-EXIT.

      * No candidate: EXIT paragraph
              IF PARENT3.FLAG-ENABLED = PARENT1.FLAG-ENABLED
                 DISPLAY 'if 2'
              END-IF.

       END PROGRAM TEST.
