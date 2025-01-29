000100 IDENTIFICATION DIVISION.                                         00010038
000200 PROGRAM-ID.         PROG1030.                                    00020038
000300 AUTHOR.             NN.                                          00030038
000400 DATE-WRITTEN.                                                   00040038
000500 DATE-COMPILED.                                                   00050038
004100 EJECT                                                            00410038
004200 ENVIRONMENT DIVISION.                                            00420038
004300 CONFIGURATION SECTION.                                           00430038
004400 SOURCE-COMPUTER.        IBM-370.                                 00440038
004500 OBJECT-COMPUTER.        IBM-370.                                 00450038
004600                                                                  00460038
004700 INPUT-OUTPUT SECTION.                                            00470038
004800 FILE-CONTROL.                                                    00480038
004900                                                                  00490041
005000                                                                  00500038
005100 DATA DIVISION.                                                   00510038
005200 FILE SECTION.                                                    00520038
005300 FD  TINVOIC-EXTRACT-FILE                                         00530041
005400     RECORDING MODE IS F                                          00540038
005500     LABEL RECORDS ARE STANDARD                                   00550038
005600     BLOCK CONTAINS 0 RECORDS                                     00560038
005700     RECORD CONTAINS 189 CHARACTERS                               00570044
005800     DATA RECORD IS TINVOIC-EXTRACT-DATA.                         00580041
005900 01  TINVOIC-EXTRACT-DATA       PIC X(189).                       00590042
006000                                                                  00600038
006100                                                                  00610038
006200 EJECT                                                            00620038
006300 WORKING-STORAGE SECTION.                                         00630038
006400                                                                  00640038
006500 01  FILLER                       PIC X(58)     VALUE             00650038
006600     '************WORKING STORAGE STARTS HERE*******************'.00660038
006700 01  INVOIC-PO-NBR      PIC X(50).                                00670038
006700 01  INVOIC-INVC-ID     PIC X(50).                                00670038
006700                                                                  00670038
016500 PROCEDURE DIVISION.                                              01650041
016600 MAINLINE-ROUTINE.                                                01660041
016700     EXEC SQL                                                     01670041
016800         DELETE FROM TINVOIC WHERE PO_NBR  = :INVOIC-PO-NBR       01680041
016900         AND INVC_ID = :INVOIC-INVC-ID                            01690041
017000     END-EXEC.                                                    01700041
017100     GOBACK.                                                      01710041
017200 EJECT                                                            01720041
