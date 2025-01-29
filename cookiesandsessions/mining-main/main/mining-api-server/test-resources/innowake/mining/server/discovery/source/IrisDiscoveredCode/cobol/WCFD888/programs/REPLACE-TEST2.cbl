       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPLACE-TEST2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  SCRATCH.
00243      03  PROGRAM-HOLD-FIELDS.                                     0000000
00244          05  HOLD-CC-1                    PIC X(01).              0000000

       01  REPORT-AREA.
00244          05  FIELD-2                      PIC X(01).              0000000

       PROCEDURE DIVISION.

           COPY MGOCOPY2 REPLACING                                      0000000
00745                    FIELD-1 BY HOLD-CC-1 OF SCRATCH .              0000000

           DISPLAY "TEST REPLACING OF".

         STOP RUN.
