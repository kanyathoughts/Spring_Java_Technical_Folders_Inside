       IDENTIFICATION DIVISION.
       PROGRAM-ID. NIIEMOD.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  MY-PROGRAM-NAME        PIC X(8) VALUE 'NIIEMOD'.

       LINKAGE SECTION.
       01  DFHCOMMAREA            PIC X(250).

       PROCEDURE DIVISION.
           .

       END PROGRAM NIIEMOD.
