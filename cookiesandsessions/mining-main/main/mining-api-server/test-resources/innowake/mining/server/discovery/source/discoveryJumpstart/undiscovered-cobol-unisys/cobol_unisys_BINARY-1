      /
       IDENTIFICATION DIVISION.

       PROGRAM-ID.      SAMPLE2.


       DATE-COMPILED.


      /
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

UCOB08 SOURCE-COMPUTER.   UNISYS-2200.
UCOB08 OBJECT-COMPUTER.   UNISYS-2200.


UCOB08 SPECIAL-NAMES.

UCOB08     PRINTER IS PRINTER.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

      /
       DATA DIVISION.

       FILE SECTION.


      /
       WORKING-STORAGE SECTION.

       01  DIRECT-AK.
CJB897     02  PAGE-NUM                    PIC 1(18) BINARY-1.
CJB897     02  RECORD-NUM                  PIC 1(18) BINARY-1.

VM9810 COMMON-STORAGE SECTION.

       01  OVERAGE-CONTROL-REC-WS.
CJB897     02  BEG-REPORT-DATE-WS                  PIC 9(8).
CJB897     02  END-REPORT-DATE-WS                  PIC 9(8).


      /
       PROCEDURE DIVISION.

      **************************************************************
      **** MODULE 0.0                  REVISION-DATE: 05-11-84
      **************************************************************

       BLD-TABLE-AND-EDIT-CTRL-LIST.

           MOVE ZEROES TO
                   BEG-REPORT-DATE-WS OF INVENTORY-CONTROL-REC-WS.

           STOP RUN.

