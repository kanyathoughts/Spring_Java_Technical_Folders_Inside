       IDENTIFICATION DIVISION.
       PROGRAM-ID. ME11412A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY cobol_unisys_copybook1 REPLACING ==S9(7)V99== BY ==S9(9)== ,
                             ==S9(8)V99== BY ==S9(10)== ,
                             ==S9(9)V99== BY ==S9(11)== ,
                             ==S9(11)V99== BY ==S9(13)== ,
                             ==S9(12)V99== BY ==S9(14)== .
       PROCEDURE DIVISION.
           MOVE 1 TO TESTFIELD.
           DISPLAY TESTFIELD.
           GOBACK.
