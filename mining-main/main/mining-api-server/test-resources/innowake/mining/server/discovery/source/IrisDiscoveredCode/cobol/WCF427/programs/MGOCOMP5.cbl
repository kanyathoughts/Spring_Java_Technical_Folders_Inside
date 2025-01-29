       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOCOMP5.
       REMARKS.  THIS PROGRAM IS DESIGNED TO CONVERT THE 135 CHARACTER  
                 DATA ENTRY RECORD INTO DB2 EMOIHDR OR EMOIMDL FORMAT.

              EJECT
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELD-1                   PIC 9(9).

       PROCEDURE DIVISION.
         FIRST-PARA.
           IF FIELD-1 NOT = 'a'
               DISPLAY 'b'
           ELSE
           IF FIELD-1 NOT = 'e' AND 'd' AND 'q'
               DISPLAY 'z'
           .
           EXIT.
