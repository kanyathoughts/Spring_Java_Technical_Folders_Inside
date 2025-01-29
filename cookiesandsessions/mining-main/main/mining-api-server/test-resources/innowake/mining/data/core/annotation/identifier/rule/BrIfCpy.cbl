       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             REPLACE ==XYZ== BY == XXX ==.
               COPY BrIfCpy.
             REPLACE OFF.
       01 PARENT3 PIC X VALUE SPACE.
          88  FLAG-ENABLED   VALUE "E".

       PROCEDURE DIVISION.

      * Candidate: TESTFIELD is from BrIfCpy Copybook
           IF TESTFIELD = 'Y'
              DISPLAY 'if 1'
           END-IF.

      * Candidate: PARENT1.FLAG-ENABLED is from BrIfCpy Copybook
           IF PARENT1.FLAG-ENABLED = TRUE
              DISPLAY 'if 2'
           END-IF.

      * No candidate: only 1 FIELD_REFERENCE which is not from a Copybook
           IF PARENT3.FLAG-ENABLED = TRUE
              DISPLAY 'if 4'
           END-IF.

      * No candidate: XYZ is replaced by XXX which is not from a Copybook
           IF XYZ = 'Y'
              DISPLAY 'if 5'
           END-IF.

      * Candidate: FIELD_REFERENCE XXX which is from BrIfCpy Copybook
           IF XXX = 'Y'
              DISPLAY 'if 6'
           END-IF.

       END PROGRAM TEST.
