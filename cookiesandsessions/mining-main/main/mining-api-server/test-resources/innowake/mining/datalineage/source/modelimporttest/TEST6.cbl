000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. WMIN143A.
000300
000400 ENVIRONMENT DIVISION.
000500 CONFIGURATION SECTION.
000600    SOURCE-COMPUTER. IBM-ISERIES.
000700    OBJECT-COMPUTER. IBM-ISERIES.
000800
000900 DATA DIVISION.
001000 WORKING-STORAGE SECTION.
       01 GROUP1.
           05 FIRSTGROUP.
             10 FOURBYTES PIC X(4).
             10 FOURBYTES2 PIC X(4).
             10 FOURBYTES3 PIC X(4).
           05 TESTGROUP REDEFINES FIRSTGROUP.
             10 TESTNUM PIC 9(6) USAGE IS BINARY.
             10 TESTCOMP PIC 9(8) COMP.
             10 FILLER PIC X(4).
001700 PROCEDURE DIVISION.
           INITIALIZE FOURBYTES
           MOVE 'XXXX' TO FOURBYTES2
           MOVE 'XXXX' TO FOURBYTES3
           MOVE 111111 TO TESTNUM
           DISPLAY FOURBYTES2
           MOVE 11111111 TO TESTCOMP
           DISPLAY FOURBYTES3

002700     GOBACK.
002800 END PROGRAM WMIN143A.
