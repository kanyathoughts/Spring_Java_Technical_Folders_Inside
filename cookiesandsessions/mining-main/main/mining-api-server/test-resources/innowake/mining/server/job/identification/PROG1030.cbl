000100 IDENTIFICATION DIVISION.                                         00010038
000200 PROGRAM-ID.         PROG1030.                                    00020038
000300 AUTHOR.             NN.                                          00030038
000400 DATE-WRITTEN.                                                   00040038
000500 DATE-COMPILED.                                                   00050038
000600*************************************************************     00060038
000700*    COBOL 2 WITH SQL                                       *     00070038
000800*                                                           *     00080038
000900*    PROG1030                                               *     00090041
001000*                                                           *     00100038
001100*                                                           *     00110038
001200*                                                           *     00120038
001300*                                                           *     00130041
001400*                                                           *     00140038
001500*                                                           *     00150038
001600*                                                           *     00160038
001700*                                                           *     00170038
001800*                                                           *     00180038
001900*                                                           *     00190041
002000*                                                           *     00200038
002100*                                                           *     00210038
002200*                                                           *     00220038
002300*                                                           *     00230038
002400*                                                           *     00240038
002500*                                                           *     00250038
002600*                                                           *     00260038
002700*                                                           *     00270038
002800*                                                           *     00280041
002900*                                                           *     00290038
003000*                                                           *     00300038
003100*                                                           *     00310038
003200*                                                           *     00320038
003300*                                                           *     00330038
003400*                                                           *     00340038
003500*                                                           *     00350038
003600*                                                           *     00360038
003700*                                                           *     00370038
003800*************************************************************     00380038
003810*-----------------------------------------------------------*     00381042
003820*                                                           *     00382042
003830*                                                           *     00383046
003840*                                                           *     00384042
003850*                                                           *     00385042
003860*                                                           *     00386042
003870*                                                           *     00387042
003880*                                                           *     00388045
003890*-----------------------------------------------------------*     00389042
004000*************************************************************     00400038
004100 EJECT                                                            00410038
004200 ENVIRONMENT DIVISION.                                            00420038
004300 CONFIGURATION SECTION.                                           00430038
004400 SOURCE-COMPUTER.        IBM-370.                                 00440038
004500 OBJECT-COMPUTER.        IBM-370.                                 00450038
004600                                                                  00460038
004700 INPUT-OUTPUT SECTION.                                            00470038
004800 FILE-CONTROL.                                                    00480038
004900     SELECT TINVOIC-EXTRACT-FILE ASSIGN TO INP01.                 00490041
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
006700                                                                  00670038
006800 01  PV-PROGRAM-VARIABLES.                                        00680038
006900     05  PV-PROGRAM-NAME          PIC  X(08)    VALUE 'AHKUP030'. 00690038
007000     05  PV-CKPT-STAT-CODE        PIC X(01)     VALUE SPACE.      00700038
007100     05  PV-CURRENT-PARAGRAPH     PIC X(50)     VALUE SPACES.     00710038
007200     05  PV-CURRENT-TMST          PIC X(26)     VALUE SPACES.     00720038
007300                                                                  00730038
007400     05  PV-TINVOIC-INPUT-COUNT   PIC S9(09)    VALUE ZERO        00740041
007500                                                COMP-3.           00750038
007600     05  PV-TINVOIC-DELETE-COUNT  PIC S9(09)    VALUE ZERO        00760041
007700                                                COMP-3.           00770038
007800     05  PV-SQL-DELETE-COUNT      PIC S9(09)    VALUE ZERO        00780038
007900                                                COMP-3.           00790038
008000     05  PV-SQLERRD3              PIC S9(09)    VALUE ZERO        00800038
008100                                                COMP-3.           00810038
008200     05  PV-DISP-INVC-ID          PIC X(09)    VALUE SPACES.      00820041
008300     05  PV-DISP-PO-NBR           PIC X(09)    VALUE SPACES.      00830038
008400 01  PC-PROGRAM-CONSTANTS.                                        00840038
008500     05  PC-RETRY-MAX             PIC S9(04)    VALUE +3          00850038
008600                                                COMP SYNC.        00860038
008700     05  PC-RETRY-COUNT           PIC S9(04)    VALUE ZEROS.      00870038
008800     05  PC-IN-PROCESS-CODE       PIC X(01)     VALUE '9'.        00880038
008900     05  PC-RUN-COMPLETE-CODE     PIC X(01)     VALUE '0'.        00890038
009000                                                                  00900038
009100 01  PS-PROGRAM-SWITCHES.                                         00910038
009200     05  PS-COMMITS-TAKEN-SW      PIC  X(01)    VALUE 'N'.        00920038
009300         88  COMMITS-TAKEN                      VALUE 'T'.        00930038
009400         88  NO-COMMITS-TAKEN                   VALUE 'N'.        00940038
009500     05  PS-END-EXTRACT-SW        PIC  X(01)    VALUE 'M'.        00950038
009600         88  MORE-EXTRACT                       VALUE 'M'.        00960038
009700         88  END-OF-EXTRACT                     VALUE 'E'.        00970038
009800     05  PS-RUN-TYPE-SW           PIC  X(01)    VALUE ' '.        00980038
009900         88  NORMAL-RUN                         VALUE '0'.        00990038
010000         88  RESTART-RUN                        VALUE '9'.        01000038
010100                                                                  01010038
010200 01  WS-COUNTER.                                                  01020038
010300     05  WS-HOLD-TMST             PIC X(26)     VALUE SPACES.     01030038
010400     05  WS-TMST                  PIC X(26)     VALUE SPACES.     01040038
010500                                                                  01050038
010600 01  CKPT-RESTART-INFO.                                           01060038
010700     05  CR-EXTRACT-RECS-WORKED   PIC  9(9)     VALUE ZERO.       01070038
010800                                                                  01080038
010900*----------------------------------------------------------------*01090038
011000*  ABEND DATA AREAS                                              *01100038
011100*----------------------------------------------------------------*01110038
011200 01  ABEND-CODE                  PIC S9(04)  VALUE ZEROS          01120038
011300                                             COMP SYNC.           01130038
011400     88  AC-BAD-RECEIVE-DATA                 VALUE +4001.         01140038
011500     88  AC-BAD-DATA                         VALUE +4008.         01150038
011600     88  AC-DB2-ERROR                        VALUE +4013.         01160038
011700     88  AC-DATE-ERROR                       VALUE +4019.         01170038
011800                                                                  01180038
011900                                                                  01190038
012000 01  ABEND-AREAS.                                                 01200038
012100     05  AA-ABEND-LIT            PIC  X(40)  VALUE                01210038
012200             '*****       ABEND'.                                 01220038
012300     05  AA-PROGRAM-LIT          PIC  X(40)  VALUE                01230038
012400             '*****   PROGRAM: AHKUP030'.                         01240038
012500     05  AA-PARAGRAPH-LIT.                                        01250038
012600         10  FILLER              PIC  X(17)  VALUE                01260038
012700             '***** PARAGRAPH: '.                                 01270038
012800         10  AA-PARAGRAPH-NAME   PIC  X(35)  VALUE SPACES.        01280038
012900     05  AA-MESSAGE-LINE-1.                                       01290038
013000         10  FILLER              PIC  X(06)  VALUE '*****'.       01300038
013100         10  AA-MESSAGE-1        PIC  X(127) VALUE SPACES.        01310038
013200     05  AA-MESSAGE-LINE-2.                                       01320038
013300         10  FILLER              PIC  X(06)  VALUE '*****'.       01330038
013400         10  AA-MESSAGE-2        PIC  X(127) VALUE SPACES.        01340038
013500     05  AA-DB2-ERROR-LIT        PIC  X(40)  VALUE                01350038
013600             '*****    DB2 ERROR'.                                01360038
013700     05  AA-DB2-OPERATION-LIT.                                    01370038
013800         10  FILLER              PIC  X(17)  VALUE                01380038
013900             '***** OPERATION: '.                                 01390038
014000         10  AA-DB2-OPERATION    PIC  X(50)  VALUE SPACES.        01400038
014100     05  AA-DB2-TABLE            PIC  X(08)  VALUE SPACES.        01410038
014200     EJECT                                                        01420038
014300                                                                  01430038
014400*----------------------------------------------------------------*01440038
014500*      TINVOIC DELETION CONTROL FILE                              01450041
014600*----------------------------------------------------------------*01460038
014700*                                                                 01470038
014800     COPY DPPD004.                                                01480041
014900     EJECT                                                        01490038
015000     COPY DPWS004.                                                01500038
015100     EJECT                                                        01510038
015200                                                                  01520038
015300*----------------------------------------------------------------*01530038
015400*      TINVOIC                                                   01540041
015500*----------------------------------------------------------------*01550038
015600         EXEC SQL                                                 01560038
015700             INCLUDE TINVOIC                                      01570041
015800         END-EXEC.                                                01580038
015900                                                                  01590038
016000*----------------------------------------------------------------*01600041
016100*                                                                 01610041
016200*----------------------------------------------------------------*01620041
016300          COPY SQLCA2.                                            01630041
016400     EJECT                                                        01640041
016500 PROCEDURE DIVISION.                                              01650041
016600 MAINLINE-ROUTINE.                                                01660041
016700     EXEC SQL                                                     01670041
016800         DELETE FROM TINVOIC WHERE PO_NBR  = :INVOIC-PO-NBR       01680041
016900     AND INVC_ID = :INVOIC-INVC-ID                                01690041
017000     END-EXEC.                                                    01700041
017100     GOBACK.                                                      01710041
017200 EJECT                                                            01720041
