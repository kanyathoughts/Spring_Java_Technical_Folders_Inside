       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AUTOFB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY AUTOFBCP.

       01  TRAN-CAT-BAL-RECORD.
           05  TRAN-CAT-BAL                            PIC S9(09)V99.

       01  DIS-GROUP-RECORD.
           05  DIS-INT-RATE                            PIC S9(04)V99.

       PROCEDURE DIVISION.

           COMPUTE WS-MONTHLY-INT
            = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200

           ADD WS-MONTHLY-INT  TO WS-TOTAL-INT

           DISPLAY 'END OF EXECUTION'.

           GOBACK.
