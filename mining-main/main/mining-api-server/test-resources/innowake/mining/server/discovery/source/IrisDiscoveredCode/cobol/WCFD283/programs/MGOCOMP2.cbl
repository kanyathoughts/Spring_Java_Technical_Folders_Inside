       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOCOMP2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELD-1                   PIC 9(9).

       PROCEDURE DIVISION.
         FIRST-PARA.
           IF FIELD-1 = 'P'
               DISPLAY 'P'
           ELSE
               IF FIELD-1 = 'Q'
                   DISPLAY 'Q'
               ELSE
                   DISPLAY 'NP'.
           CALL 'MGOASM1'
           EXEC CICS LINK 
               PROGRAM('MGOASM2') 
           END-EXEC.
           CALL 'COBORASM'
           EXEC CICS LINK 
               PROGRAM('COBORASM') 
           END-EXEC.
           EXIT.







