       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       01  NUMBER-ONE PIC 9(4) VALUE 1000.
001600 
001610 LINKAGE SECTION.
001620 01  MY-FIELD            PIC S9(09) COMP.
001780 
001790 PROCEDURE DIVISION USING MY-FIELD.
005340 CALL "C_PROG_FOR_CBL_BASIC"
005350 EXIT PROGRAM.
