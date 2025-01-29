       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEE4790A.
      * This test is to prove that with the changes made for
      * WMEE-4809, fields in DDMs can now be unsigned.
      *
      * Two fields of the the MEE4809A_DDM have a leading comment
      * with an @UNSIGNED 'annotation', turning them into unsigned fields
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.

      * Dummy select to get the buffers of the view setup.
      * We don't actually care about the result - only need the fields
           EXEC ADABAS
               READ ISN
               SELECT USGND_PCKD, SGND_PCKD, USGND_NRC, SGND_NRC
               FROM MEE4809A S_U_VIEW
               WHERE ISN=777
           END-EXEC

      * move -1 to USGND_PCKD, when checking it should be positive
           MOVE -1 TO S_U_VIEW.USGND_PCKD
           IF S_U_VIEW.USGND_PCKD < 0 THEN
               DISPLAY "IS NEGATIVE".
           IF S_U_VIEW.USGND_PCKD > 0 THEN
               DISPLAY "IS POSITIVE".

      * move -1 to SGND_PCKD when checking it should be negative
           MOVE -1 TO S_U_VIEW.SGND_PCKD
           IF S_U_VIEW.SGND_PCKD < 0 THEN
               DISPLAY "IS NEGATIVE".
           IF S_U_VIEW.SGND_PCKD > 0 THEN
               DISPLAY "IS POSITIVE".

      * move -1 to USGND_NRC when checking it should be positive
           MOVE -1 TO S_U_VIEW.USGND_NRC
           IF S_U_VIEW.USGND_NRC < 0 THEN
               DISPLAY "IS NEGATIVE".
           IF S_U_VIEW.USGND_NRC > 0 THEN
               DISPLAY "IS POSITIVE".

      * move -1 to SGND_NRC when checking it should be negative
           MOVE -1 TO S_U_VIEW.SGND_NRC
           IF S_U_VIEW.SGND_NRC < 0 THEN
               DISPLAY "IS NEGATIVE".
           IF S_U_VIEW.SGND_NRC > 0 THEN
               DISPLAY "IS POSITIVE".

           STOP RUN.
           EXIT PROGRAM.



