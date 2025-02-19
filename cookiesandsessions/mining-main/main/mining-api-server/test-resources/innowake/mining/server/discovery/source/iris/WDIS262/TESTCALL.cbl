000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. ADAS01.
000030 ENVIRONMENT DIVISION.
000040 CONFIGURATION SECTION.
000070 INPUT-OUTPUT SECTION.
000080 FILE-CONTROL.
000090 DATA DIVISION.
000100 FILE SECTION.
000110 WORKING-STORAGE SECTION.
000111     01  WS-FORM-ADCX04.
000112         05  WS-FORM       PIC X(06)    VALUE "ADCX04".
000120 REPORT SECTION.
000130 PROCEDURE DIVISION.
000140 THE-MAIN SECTION.
002780     CALL "FDV$ATERM" USING
002790         BY DESCRIPTOR WS-TCA
002800         BY REFERENCE  WS-TSIZE
002810         BY REFERENCE  WS-TERM-CHAN
002820         OMITTED
002830         OMITTED
002840         BY REFERENCE  WS-NO-FDV-AST-SUPPORT.
003850     CALL "FDV$AWKSP" USING
003860         BY DESCRIPTOR WS-WKSP
003870         BY REFERENCE  WS-WSIZE.
003880     CALL "FDV$LCHAN" USING WS-LIB-CHAN.
003890     CALL "FDV$LOPEN" USING BY DESCRIPTOR WS-FORMLIB.
003970     CALL "FDV$DISP" USING BY DESCRIPTOR "ADAS01".
004090     CALL "FDV$PUTL" USING BY DESCRIPTOR WS-BOTTOM-LONG.
005000     CALL "FDV$CDISP" USING BY DESCRIPTOR "ADASR1".
005010     CALL "FDV$DISP" USING BY DESCRIPTOR "ADDD01"
005005     MOVE 'ABCDEFG' TO WS-FORM
005020     CALL "FDV$CDISP" USING BY DESCRIPTOR WS-FORM.
005030     CALL "FDV$DISPW" USING BY REFERENCE WS-FMS-DUMMY
005040     CALL "FDV$DISPW".
005050     CALL "FDV$DISP" USING BY DESCRIPTOR WS-FORM-NAME.
005060     CALL "FDV$CLRSH" USING BY DESCRIPTOR WS-FORM1.
