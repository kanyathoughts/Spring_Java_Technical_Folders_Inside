       IDENTIFICATION DIVISION.
       PROGRAM-ID. SampleProgram.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RFM-OTHER-COVERAGE.
          05 RFM-STATE-OTH-COV REDEFINES RFM-OTHER-COVERAGE.
          10 RFM-OTH-COV-STATE PIC X(02).
             88 VALID-OTH-COV-STATE VALUE 'SC'
             							  'OH'.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'RFM-OTH-COV-STATE: ' RFM-OTH-COV-STATE
           IF VALID-OTH-COV-STATE
               DISPLAY 'Valid State Code'
           ELSE
               DISPLAY 'Invalid State Code'
           END-IF
           STOP RUN.
