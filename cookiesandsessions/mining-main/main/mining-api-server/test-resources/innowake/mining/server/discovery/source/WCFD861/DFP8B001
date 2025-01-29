000001 IDENTIFICATION DIVISION.                                         00010000
000002 PROGRAM-ID.    DFP8B001.                                         00020000
000003 AUTHOR.        SHIREESHA.                                        00030000
000004 DATE-WRITTEN.  03/17/2010.                                       00040000
000005 DATE-COMPILED.                                                   00050000
000006******************************************************************00060000
000007* SYSTEM         : STATEMENTS TEAM                               *00070000
000008* DIVISION       : RETIREMENT SERVICES                           *00080000
000009* DESCRIPTION    : REQUEST ID CREATION FOR PARTICIPANTS WITH     *00090000
000010*                  PRINT BYPASS CODE GREATER THAN ZERO.STMT_RQST,*00100000
      *                  PLAN_INCL_ON_RQST,CUST_INCL_ON_RQST TABLES ARE*00110000
      *                  POPULATED FOR THE REQUEST ID CREATED.         *00110100
      *                  REQUEST ID CREATED.                           *00120000
000011* INPUT  FILE(S) : PART-FILE                                     *00130000
000012*                                                                *00140000
000013* OUTPUT FILE(S) : RPT-FILE                                      *00150000
000014*                  ERROR-FILE                                    *00160000
000014*                  COUNT-FILE                                    *00170000
000015******************************************************************00180000
000015******************************************************************00190000
000016*                                                                *00200000
000017*                  MODIFICATION LOG                              *00210000
000018*                                                                *00220000
000019* DATE       DEVELOPED BY       DESCRIPTION                      *00230000
000020* ----       ------------       -----------                      *00240000
000021* 03/17/10   SHIREESHA          NEW PROGRAM - DMS#77732          *00250000
000021* 08/26/10   NISHANT            Handle -803 in plan_incl_on_rqst *00250100
000000* *                                                              *00260000
      * 04/26/2011  AAPSAR SYED        RECOMPILE FOR DMS#84384         *00260100
      *                                DCLGEN - ZNHDSTRQ.              *00260200
000023******************************************************************00270000
000024                                                                  00280000
000025 ENVIRONMENT DIVISION.                                            00290000
000026 CONFIGURATION SECTION.                                           00300000
000027 SOURCE-COMPUTER.   IBM-3090.                                     00310000
000028 OBJECT-COMPUTER.   IBM-3090.                                     00320000
000029 INPUT-OUTPUT SECTION.                                            00330000
000030                                                                  00340000
000031 FILE-CONTROL.                                                    00350000
000032     SELECT PART-FILE              ASSIGN TO PARTFILE             00360000
000033            FILE STATUS IS PPTFIL-STATUS.                         00370000
000034     SELECT RPT-FILE               ASSIGN TO RPOUTPUT             00380000
000035            FILE STATUS IS RPTFIL-STATUS.                         00390000
000034     SELECT ERROR-FILE             ASSIGN TO ERRFIL               00400000
000035            FILE STATUS IS ERRFIL-STATUS.                         00410000
000034     SELECT COUNT-FILE             ASSIGN TO CNTFIL               00420000
000035            FILE STATUS IS CNTFIL-STATUS.                         00430000
000036                                                                  00440000
000037 DATA DIVISION.                                                   00450000
000038 FILE SECTION.                                                    00460000
000039                                                                  00470000
000040 FD  PART-FILE                                                    00480000
000041     LABEL RECORDS ARE STANDARD                                   00490000
000042     RECORDING MODE IS F                                          00500000
000043     BLOCK CONTAINS 0 RECORDS                                     00510000
      *    RECORD IS VARYING FROM 7 TO 39 CHARACTERS                    00510010
           RECORD CONTAINS 35 CHARACTERS                                00510020
000059     DATA RECORD IS IN-VISION-REC20.                              00510200
000046                                                                  00530000
       01  IN-VISION-REC20.                                             00540000
           05 INP-DATE-KEY.                                             00540100
              10 INP-BOP-DATE              PIC  9(08) COMP-3.           00540200
              10 INP-EOP-DATE              PIC  9(08) COMP-3.           00540300
           05 INP-REC-ID                   PIC  X(02).                  00540400
           05 INP-PLAN-NUM.                                             00540500
             09 INP-PL-PREFIX              PIC  X(01).                  00540600
             09 INP-PL-SUFFIX              PIC  X(05).                  00540700
           05 INP-SSN-NUM                  PIC  9(09).                  00540900
           05 INP-SUBPLAN-NUM              PIC  X(06).                  00541000
           05 INP-PART-EXT1                PIC  X(01).                  00541100
           05 INP-PART-EXT2                PIC  X(01).                  00541200
000053                                                                  00560000
000054 FD  RPT-FILE                                                     00570000
000055     LABEL RECORDS ARE STANDARD                                   00580000
000056     RECORDING MODE IS F                                          00590000
000057     BLOCK CONTAINS 0 RECORDS                                     00600000
000059     DATA RECORD IS REPORT-RECORD.                                00620000
000060                                                                  00630000
       01  REPORT-RECORD                     PIC X(80).                 00640000
                                                                        00650000
000064                                                                  00660000
000054 FD  ERROR-FILE                                                   00670000
000055     LABEL RECORDS ARE STANDARD                                   00680000
000056     RECORDING MODE IS F                                          00690000
000057     BLOCK CONTAINS 0 RECORDS                                     00700000
000059     DATA RECORD IS ERROR-RECORD.                                 00720000
000060                                                                  00730000
       01  ERROR-RECORD                      PIC X(80).                 00740000
                                                                        00750000
000054 FD  COUNT-FILE                                                   00760000
000055     LABEL RECORDS ARE STANDARD                                   00770000
000056     RECORDING MODE IS F                                          00780000
000057     BLOCK CONTAINS 0 RECORDS                                     00790000
000059     DATA RECORD IS COUNT-RECORD.                                 00810000
000060                                                                  00820000
       01  COUNT-RECORD                      PIC X(80).                 00830000
                                                                        00840000
                                                                        00850000
000065 WORKING-STORAGE SECTION.                                         00860000
000070                                                                  00870000
000061 01  REPORT-FILE.                                                 00880000
           05  REPORT-HEADER.                                           00890000
               10 FILLER                     PIC X(10)   VALUE          00900000
                   'REQUEST ID'.                                        00910000
               10 FILLER                     PIC X(02).                 00920000
               10 FILLER                     PIC X(11)   VALUE          00930000
                   'PLAN NUMBER'.                                       00940000
               10 FILLER                     PIC X(03).                 00950000
               10 FILLER                     PIC X(22)   VALUE          00990000
                   'NUMBER OF PARTICIPANTS'.                            01000000
               10 FILLER                     PIC X(32).                 01010000
           05  REPORT-DTL .                                             01020000
               10 WS-RQST-ID                 PIC 9(09).                 01030000
               10 FILLER                     PIC X(03).                 01040000
               10 WS-PLAN-NUMBER             PIC X(06).                 01050000
               10 FILLER                     PIC X(08).                 01060000
               10 WS-PART-INCLD-IN-RQST      PIC 9(09).                 01090000
               10 FILLER                     PIC X(45).                 01100000
           05  REPORT-FOOTER .                                          01120000
               10 WS-FOOTER-MSG              PIC X(30)   VALUE SPACES.  01130000
               10 WS-TOTAL-RECORDS           PIC 9(09)  VALUE ZEROS.    01150000
               10 FILLER                     PIC X(41).                 01160000
       01  ERR-FILE.                                                    01230000
           05 ERROR-HEADER.                                             01240000
              10 FILLER                      PIC X(09) VALUE            01280000
                    'SSN      '.                                        01290000
              10 FILLER                      PIC X(02).                 01300000
              10 FILLER                      PIC X(09) VALUE            01310000
                    'PLAN '.                                            01320000
              10 FILLER                      PIC X(02).                 01330000
              10 FILLER                      PIC X(09) VALUE            01340000
                    'SUBPLAN'.                                          01350000
              10 FILLER                      PIC X(02).                 01360000
              10 FILLER                      PIC X(32) VALUE            01370000
                    'ERROR MESSAGE'.                                    01380000
              10 FILLER                      PIC X(15).                 01380100
           05 ERROR-DTL.                                                01450000
              10 WS-ERR-SSN                  PIC X(09).                 01470100
              10 FILLER                      PIC X(02).                 01470200
              10 WS-ERR-PLAN-NUM             PIC X(06).                 01480000
              10 FILLER                      PIC X(02).                 01490000
              10 WS-ERR-SUBPLAN-NUM          PIC X(06).                 01500000
              10 FILLER                      PIC X(02).                 01510000
              10 WS-ERR-MESSAGE              PIC X(25).                 01520000
              10 FILLER                      PIC X(18).                 01590000
       01  CNT-FILE.                                                    01600000
           05  COUNT-HEADER.                                            01610000
               10 FILLER                     PIC X(30)   VALUE          01620000
                  '**** DFP8B001 STARTED ****'.                         01630000
               10 FILLER                     PIC X(50).                 01640000
           05  COUNT-LINE1 .                                            01650000
               10 FILLER                     PIC X(30)   VALUE          01660000
                  'CURRENT TIMESTAMP  =  '.                             01670000
               10 WS-START-TIME              PIC X(26).                 01680000
               10 FILLER                     PIC X(24).                 01690000
           05  COUNT-LINE2 .                                            01700000
               10 WS-COUNT-DTL               PIC X(30)   VALUE SPACES.  01710000
               10 WS-RECORD-COUNT            PIC 9(09)   VALUE ZEROS.   01730000
               10 FILLER                     PIC X(41).                 01730100
           05  COUNT-FOOTER.                                            01890000
               10 FILLER                     PIC X(30)   VALUE          01900000
                  '**** DFP8B001 ENDED ****'.                           01910000
               10 FILLER                     PIC X(50).                 01920000
                                                                        01930000
       01 DATE-TIME-AREA.                                               01940000
000173    05 WS-DEFAULT-TMSTMP               PIC  X(26)                 01950000
000174       VALUE '0001-01-01-00.00.00.000000'.                        01960000
000175    05 WS-DEFAULT-DT                   PIC  X(10)                 01970000
000176       VALUE '0001-01-01'.                                        01980000
                                                                        01990000
000096    05 WS-PROCESS-BOP.                                            02000000
000097       10 WS-PROCESS-BOP-CC         PIC  X(02).                   02010000
000098       10 WS-PROCESS-BOP-YY         PIC  X(02).                   02020000
000099       10 FILLER                    PIC  X(01) VALUE '-'.         02030000
000100       10 WS-PROCESS-BOP-MM         PIC  X(02).                   02040000
000101       10 FILLER                    PIC  X(01) VALUE '-'.         02050000
000102       10 WS-PROCESS-BOP-DD         PIC  X(02).                   02060000
000103                                                                  02070000
000104    05 WS-PROCESS-EOP.                                            02080000
000105       10 WS-PROCESS-EOP-CC         PIC  X(02).                   02090000
000106       10 WS-PROCESS-EOP-YY         PIC  X(02).                   02100000
000107       10 FILLER                    PIC  X(01) VALUE '-'.         02110000
000108       10 WS-PROCESS-EOP-MM         PIC  X(02).                   02120000
000109       10 FILLER                    PIC  X(01) VALUE '-'.         02130000
000110       10 WS-PROCESS-EOP-DD         PIC  X(02).                   02140000
                                                                        02150000
       01 WS-FILE-STATUS.                                               02160000
          05 PPTFIL-STATUS                   PIC X(2)    VALUE SPACES.  02170000
          05 RPTFIL-STATUS                   PIC X(2)    VALUE SPACES.  02180000
          05 ERRFIL-STATUS                   PIC X(2)    VALUE SPACES.  02190000
          05 CNTFIL-STATUS                   PIC X(2)    VALUE SPACES.  02200000
                                                                        02210000
       01 WS-COUNTERS.                                                  02220000
000136    05 WS-LOAD-CTR                     PIC 9(02)   VALUE 0.       02230000
          05 WS-INPUT-REC-CTR                PIC 9(09)   VALUE 0.       02250100
          05 WS-INPUT-REC-SKIPPED            PIC 9(09)   VALUE 0.       02250200
          05 CUSTOMER-COUNT                  PIC 9(09)   VALUE 0.       02260000
          05 WS-TOT-RQST-CREATED             PIC 9(09)   VALUE 0.       02280000
          05 WS-CUST-PER-REQUEST             PIC 9(09)   VALUE 0.       02290000
          05 WS-CUSTOMER-INSERTED            PIC 9(09)   VALUE 0.       02290100
                                                                        02320000
       01 WS-DIAG-FIELDS.                                               02330000
          05 DB2RC                           PIC S9(09)  COMP.          02340000
          05 ROW-COUNT                       PIC S9(18)  COMP-3.        02350000
          05 ERRCNT                          PIC S9(09)  COMP.          02360000
          05 CTR                             PIC S9(09)  COMP.          02370000
          05 ERR-SQLCODE                     PIC S9(09)  COMP.          02380000
          05 ERR-SQLSTATE                    PIC X(05).                 02390000
          05 ROW-NUM                         PIC S9(18)  COMP-3.        02400000
          05 NUM-ROWS                        PIC S9(06)  COMP.          02410000
                                                                        02420000
       01 WS-LOCAL-FIELDS.                                              02430000
          05 WS-AGENT-ID                     PIC X(04)   VALUE ZERO.    02440000
          05 WS-AC-PLAN-ID                   PIC S9(09)  COMP.          02450000
          05 WS-BLANK-SUBPLN-ACPLN           PIC S9(09)  COMP.          02450100
          05 WS-AC-CLIENT-ID                 PIC S9(09)  COMP.          02460000
          05 WS-PLAN-NUM                     PIC X(06)   VALUE ZERO.    02470000
          05 WS-SUBPLAN-NUM                  PIC X(06)   VALUE ZERO.    02480000
          05 WS-INVALID-PLAN-NUM             PIC X(06)   VALUE ZERO.    02480100
          05 WS-INVALID-SUBPLAN-NUM          PIC X(06)   VALUE ZERO.    02480200
          05 WS-DC-SOURCE-SYSTEM             PIC S9(04)  Comp.          02490000
          05 WS-DELIM-CHAR                   PIC X(01)   VALUE X'05'.   02500000
          05 WS-SSN                          PIC S9(09)  COMP.          02510000
          05 WS-SSN-EXT                      PIC X(02).                 02510100
          05 WS-TBL-CUST-ID                  PIC S9(09)  COMP.          02520000
          05 WS-TBL-NAME                     PIC X(20).                 02530000
          05 WS-IN-PROCESS-BOP               PIC 9(08).                 02540000
          05 WS-IN-PROCESS-BOPX   REDEFINES WS-IN-PROCESS-BOP.          02550000
             10 WS-IN-PROCESS-BOP-X          PIC X(08).                 02560000
          05 WS-IN-PROCESS-EOP               PIC 9(08).                 02570000
          05 WS-IN-PROCESS-EOPX   REDEFINES WS-IN-PROCESS-EOP.          02580000
             10 WS-IN-PROCESS-EOP-X          PIC X(08).                 02590000
          05 WS-CUSTOMER .                                              02640000
             07 WS-CUSTOMER-ID    PIC S9(09)  COMP OCCURS 8000 TIMES.   02650000
                                                                        02650100
       01 WS-HOLD-VARIABLES.                                            02710000
          05 WS-HOLD-PLAN-NUM                PIC X(06)   VALUE ZEROES.  02720000
          05 WS-HOLD-SUBPLAN-NUM             PIC X(06)   VALUE ZEROES.  02730000
                                                                        02740000
       01 WS-SWITCHES.                                                  02750000
          05 EOF-INPUT-IND                   PIC X       VALUE 'N'.     02760000
             88 WS-ALL-RECORDS-READ                      VALUE 'Y'.     02760100
          05 WS-FIRST-RECORD-SW              PIC X       VALUE 'N'.     02770000
             88 WS-FIRST-RECORD                          VALUE 'Y'.     02780000
             88 WS-NOT-FIRST-RECORD                      VALUE 'N'.     02780100
          05 WS-PLAN-CHANGE-SW               PIC X       VALUE 'N'.     02780200
             88 WS-ITS-NEW-PLAN                          VALUE 'Y'.     02780300
          05 WS-SUBPLAN-CHANGE-SW            PIC X       VALUE 'N'.     02780400
             88 WS-ITS-NEW-SUBPLAN                       VALUE 'Y'.     02780500
000171    05 WS-INSERT-SUCCESSFUL-SW         PIC X       VALUE 'N'.     02800000
000172       88 WS-INSERT-SUCCESSFUL                     VALUE 'Y'.     02810000
          05 WS-RECORD-VALID-SW              PIC X       VALUE 'N'.     02820000
             88 WS-VALID-RECORD-FND                      VALUE 'Y'.     02830000
             88 WS-RECORD-NOT-VALID                      VALUE 'N'.     02830100
                                                                        02840000
          05 WS-ACPLAN-SW                    PIC X       VALUE 'N'.     02840010
             88 WS-ACPLAN-FND                            VALUE 'Y'.     02840020
             88 WS-ACPLAN-NOT-FND                        VALUE 'N'.     02840030
                                                                        02840040
          05 WS-INCREASE-SW                  PIC X       VALUE 'N'.     02850000
          05 WS-DECREASE-SW                  PIC X       VALUE 'N'.     02860000
                                                                        02870000
000180 01 WS-TIMESTAMP                       PIC X(26)   VALUE SPACES.  02880000
000181 01 WS-TIMESTAMP-PART.                                            02890000
000182    05 WS-TIMESTAMP-DATE               PIC X(10).                 02900000
000183    05 FILLER                          PIC X(01).                 02910000
000184    05 WS-TIMESTAMP-HOUR               PIC 9(02).                 02920000
000185    05 FILLER                          PIC X(01).                 02930000
000186    05 WS-TIMESTAMP-MIN                PIC 9(02).                 02940000
000187    05 FILLER                          PIC X(01).                 02950000
000188    05 WS-TIMESTAMP-SEC                PIC 9(02).                 02960000
000189    05 FILLER                          PIC X(01).                 02970000
000190    05 WS-TIMESTAMP-MIL                PIC 9(06).                 02980000
s00191                                                                  02990000
000192 01 WS-RANDOM-NUMBER                   PIC 9(09).                 03000000
000193 01 WS-RANDOM-NUMBER-DTL REDEFINES WS-RANDOM-NUMBER.              03010000
000194    05 WS-RANDOM-NUM-ZEROES            PIC 9(01).                 03020000
000195    05 WS-RANDOM-NUM-HIGH-PART         PIC 9(06).                 03030000
000196    05 WS-RANDOM-NUM-LOW-PART          PIC 9(02).                 03040000
000197                                                                  03050000
000198 01 WS-LOW-KEY-REDEF.                                             03060000
000199    05 WS-LOW-KEY-ZEROES               PIC 9(01).                 03070000
000200    05 WS-LOW-KEY-HIGH-PART            PIC 9(06).                 03080000
000201    05 WS-LOW-KEY-LOW-PART             PIC 9(02).                 03090000
000202                                                                  03100000
000203 01 WS-HIGH-KEY-REDEF.                                            03110000
000204    05 WS-HIGH-KEY-ZEROES              PIC 9(01).                 03120000
000205    05 WS-HIGH-KEY-HIGH-PART           PIC 9(06).                 03130000
000206    05 WS-HIGH-KEY-LOW-PART            PIC 9(02).                 03140000
000207                                                                  03150000
000208 01 WS-STMT-RQST-PART-TABLE.                                      03160000
000209    05 FILLER            PIC X(22) VALUE '0100000000010003124999'.03170000
000210    05 FILLER            PIC X(22) VALUE '0200031250000006249999'.03180000
000211    05 FILLER            PIC X(22) VALUE '0300062500000009374999'.03190000
000212    05 FILLER            PIC X(22) VALUE '0400093750000012499999'.03200000
000213    05 FILLER            PIC X(22) VALUE '0500125000000015624999'.03210000
000214    05 FILLER            PIC X(22) VALUE '0600156250000018749999'.03220000
000215    05 FILLER            PIC X(22) VALUE '0700187500000021874999'.03230000
000216    05 FILLER            PIC X(22) VALUE '0800218750000024999999'.03240000
000217    05 FILLER            PIC X(22) VALUE '0900250000000028124999'.03250000
000218    05 FILLER            PIC X(22) VALUE '1000281250000031249999'.03260000
000219    05 FILLER            PIC X(22) VALUE '1100312500000034374999'.03270000
000220    05 FILLER            PIC X(22) VALUE '1200343750000037499999'.03280000
000221    05 FILLER            PIC X(22) VALUE '1300375000000040624999'.03290000
000222    05 FILLER            PIC X(22) VALUE '1400406250000043749999'.03300000
000223    05 FILLER            PIC X(22) VALUE '1500437500000046874999'.03310000
000224    05 FILLER            PIC X(22) VALUE '1600468750000049999999'.03320000
000225    05 FILLER            PIC X(22) VALUE '1700500000000053124999'.03330000
000226    05 FILLER            PIC X(22) VALUE '1800531250000056249999'.03340000
000227    05 FILLER            PIC X(22) VALUE '1900562500000059374999'.03350000
000228    05 FILLER            PIC X(22) VALUE '2000593750000062499999'.03360000
000229    05 FILLER            PIC X(22) VALUE '2100625000000065624999'.03370000
000230    05 FILLER            PIC X(22) VALUE '2200656250000068749999'.03380000
000231    05 FILLER            PIC X(22) VALUE '2300687500000071874999'.03390000
000232    05 FILLER            PIC X(22) VALUE '2400718750000074999999'.03400000
000233    05 FILLER            PIC X(22) VALUE '2500750000000078124999'.03410000
000234    05 FILLER            PIC X(22) VALUE '2600781250000081249999'.03420000
000235    05 FILLER            PIC X(22) VALUE '2700812500000084374999'.03430000
000236    05 FILLER            PIC X(22) VALUE '2800843750000087499999'.03440000
000237    05 FILLER            PIC X(22) VALUE '2900875000000090624999'.03450000
000238    05 FILLER            PIC X(22) VALUE '3000906250000093749999'.03460000
000239    05 FILLER            PIC X(22) VALUE '3100937500000096874999'.03470000
000240    05 FILLER            PIC X(22) VALUE '3200968750000999999999'.03480000
000241                                                                  03490000
000242 01 WS-RANGE-TABLE REDEFINES WS-STMT-RQST-PART-TABLE.             03500000
000243    05 WS-RANGE-DATA OCCURS 32 TIMES                              03510000
000244       INDEXED BY WS-RANGE-IDX.                                   03520000
000245       10 WS-RANGE-RQST-PARTITION      PIC 99.                    03530000
000246       10 WS-RANGE-LOW-KEY.                                       03540000
000247          15 WS-RANGE-LOW-KEY-1        PIC 9(01).                 03550000
000248          15 WS-RANGE-LOW-KEY-9        PIC 9(09).                 03560000
000249       10 WS-RANGE-HIGH-KEY.                                      03570000
000250          15 WS-RANGE-HIGH-KEY-1       PIC 9(01).                 03580000
000251          15 WS-RANGE-HIGH-KEY-9       PIC 9(09).                 03590000
                                                                        03600000
000067 01 WS-ABEND-AREA.                                                03610000
000068    05 WS-ABEND-PGM                   PIC  X(08) VALUE 'ILBOABN0'.03620000
000069    05 WS-ABEND-CODE                   PIC S9(04) COMP VALUE 9999.03630000
                                                                        03640000
       01 WS-ERR-FIELDS.                                                03650000
          05 ERR-FILE-OPERATION              PIC X(08).                 03660000
          05 ERR-FILE-NAME                   PIC X(10).                 03670000
          05 ERR-FILE-STATUS                 PIC X(02).                 03680000
          05 ERR-PARA-NAME                   PIC X(20).                 03690000
       01 WS-SQL-ERR-FIELDS.                                            03700000
          05 WS-SQLCODE                      PIC Z(08)9-.               03710000
000253******************************************************************03740000
000254*  DB2 TABLE DECLARATION(S)                                      *03750000
000255******************************************************************03760000
000256                                                                  03770000
000257*** SQL Communication Area                                        03780000
000258     EXEC SQL                                                     03790000
000259        INCLUDE SQLCA                                             03800000
000260     END-EXEC.                                                    03810000
000261                                                                  03820000
000262*** TABLE FOR AC_PLAN_ID                                          03830000
000263     EXEC SQL                                                     03840000
000264        INCLUDE ZNHDACPL                                          03850000
000265     END-EXEC.                                                    03860000
000266                                                                  03870000
000267*** TABLE FOR STMT_RQST                                           03880000
000268     EXEC SQL                                                     03890000
000269        INCLUDE ZNHDSTRQ                                          03900000
000270     END-EXEC.                                                    03910000
000271                                                                  03920000
000272*** TABLE FOR PLAN_INCL_ON_RQST                                   03930000
000273     EXEC SQL                                                     03940000
000274        INCLUDE ZNHDPLIR                                          03950000
000275     END-EXEC.                                                    03960000
000276                                                                  03970000
000277*** TABLE FOR CUST_INCL_ON_RQST                                   03980000
000278     EXEC SQL                                                     03990000
000279        INCLUDE ZNHDCUIR                                          04000000
000280     END-EXEC.                                                    04010000
000281                                                                  04020000
000282*** TABLE FOR CUSTOMER                                            04030000
000283     EXEC SQL                                                     04040000
000284        INCLUDE ZNHDCUST                                          04050000
000285     END-EXEC.                                                    04060000
                                                                        04070000
000282*** TABLE FOR CUSTOMER_PLAN                                       04080000
000283     EXEC SQL                                                     04090000
000284        INCLUDE ZNHDCSPL                                          04100000
000285     END-EXEC.                                                    04110000
000286                                                                  04120000
000288*** TABLE FOR NZNH.AGENT                                          04130000
000289     EXEC SQL                                                     04140000
000290        INCLUDE ZNHDAGNT                                          04150000
000291     END-EXEC.                                                    04160000
                                                                        04170000
00248 ******************************************************************04180000
00249 *  LINKAGE SECTION                                                04190000
00250 ******************************************************************04200000
00251  LINKAGE SECTION.                                                 04210000
                                                                        04220000
         01 LINK-PARM.                                                  04230000
            05 WS-LINK-LENGTH           PIC S9(4) COMP.                 04230010
            05 WS-PART-COUNT            PIC 9(5).                       04240000
            05 WS-DIGIT                 PIC X(3).                       04250000
                                                                        04260000
      **=============================================================** 04270000
      **=        P R O C E D U R E     D I V I S I O N              =** 04280000
      **=============================================================** 04290000
       PROCEDURE DIVISION USING LINK-PARM.                              04300000
                                                                        04310000
       P0000-MAINLINE.                                                  04320000
           PERFORM P1000-INITIALIZATION                                 04330000
              THRU P1000-EXIT                                           04340000
           SET WS-FIRST-RECORD TO TRUE                                  04340100
           PERFORM P3000-PROCESS-RECORD                                 04390000
              THRU P3000-EXIT UNTIL WS-ALL-RECORDS-READ                 04400000
           PERFORM P5000-WRAP-UP-PROCESS                                04420000
              THRU P5000-EXIT                                           04430000
                                                                        04440000
           STOP RUN.                                                    04450000
                                                                        04460000
       P0000-EXIT. EXIT.                                                04470000
         EJECT                                                          04480000
                                                                        04490000
      ******************************************************************04500000
      ***  P1000   - INITIALIZATION.                                    04510000
      ***  P2000   - READ INPUT FILE.                                   04530000
      ***  P2100   - GET AC PLAN ID FOR DC PLAN AND SUB PLAN COMBINATION04540000
      ***  P3000   - PROCESS THE RECORDS READ.                          04550000
      ***  P3100   - INSERTS INTO CUST_INCL_ON_RQST TABLE.              04560000
      ***  P4000   - GENERATES REQUEST IDFOR EACH PLAN AND SUBPLAN COMB.04570000
      ***  P4100   - SEARCHS STMT_RQST PARTITION TABLE.                 04580000
      ***  P4110   - INCREASES THE RQST ID.                             04590000
      ***  P4120   - DECREASES RQST ID.                                 04600000
      ***  P4300   - INSERTS REQUEST INTO STMT_RQST TABLE.              04610000
      ***  P4400   - INSERTS INTO PLAN_INCL_RQST TABLE.                 04620000
      ***  P5000   - CLOSES ALL THE FILES.                              04630000
      ******************************************************************04640000
                                                                        04650000
       P1000-INITIALIZATION.                                            04660000
            OPEN INPUT  PART-FILE                                       04680000
            IF PPTFIL-STATUS NOT = '00'                                 04690000
               MOVE 'OPEN'             TO ERR-FILE-OPERATION            04700000
               MOVE 'PART FILE     '   TO ERR-FILE-NAME                 04710000
               MOVE PPTFIL-STATUS      TO ERR-FILE-STATUS               04720000
               MOVE 'P1000-INITIALIZATION.' TO ERR-PARA-NAME            04730000
               PERFORM P9999-ABEND-PROGRAM                              04750000
            END-IF                                                      04760000
                                                                        04770000
            OPEN OUTPUT RPT-FILE                                        04780000
            IF RPTFIL-STATUS NOT = '00'                                 04790000
               MOVE 'OPEN'             TO ERR-FILE-OPERATION            04800000
               MOVE 'REPORT FILE   '   TO ERR-FILE-NAME                 04810000
               MOVE RPTFIL-STATUS      TO ERR-FILE-STATUS               04820000
               MOVE 'P1000-INITIALIZATION.' TO ERR-PARA-NAME            04830000
               PERFORM P9999-ABEND-PROGRAM                              04850000
            END-IF                                                      04860000
                                                                        04870000
            OPEN OUTPUT ERROR-FILE                                      04880000
            IF ERRFIL-STATUS NOT = '00'                                 04890000
               MOVE 'OPEN'             TO ERR-FILE-OPERATION            04900000
               MOVE 'ERROR FILE    '   TO ERR-FILE-NAME                 04910000
               MOVE ERRFIL-STATUS      TO ERR-FILE-STATUS               04920000
               MOVE 'P1000-INITIALIZATION.' TO ERR-PARA-NAME            04930000
               PERFORM P9999-ABEND-PROGRAM                              04950000
            END-IF                                                      04960000
                                                                        04970000
            OPEN OUTPUT COUNT-FILE                                      04980000
            IF CNTFIL-STATUS NOT = '00'                                 04990000
               MOVE 'OPEN'             TO ERR-FILE-OPERATION            05000000
               MOVE 'COUNT FILE    '   TO ERR-FILE-NAME                 05010000
               MOVE CNTFIL-STATUS      TO ERR-FILE-STATUS               05020000
               MOVE 'P1000-INITIALIZATION.' TO ERR-PARA-NAME            05030000
               PERFORM P9999-ABEND-PROGRAM                              05050000
            END-IF                                                      05060000
                                                                        05070000
                                                                        05070100
            MOVE COUNT-HEADER           TO  COUNT-RECORD                05070200
            WRITE COUNT-RECORD AFTER ADVANCING 1                        05070300
            EXEC SQL                                                    05070400
                SET :WS-TIMESTAMP = CURRENT TIMESTAMP                   05070500
            END-EXEC                                                    05070600
            MOVE WS-TIMESTAMP           TO  WS-START-TIME               05070710
            MOVE SPACES                 TO  COUNT-RECORD                05070800
            MOVE COUNT-LINE1            TO  COUNT-RECORD                05070900
            WRITE COUNT-RECORD AFTER ADVANCING 1                        05071000
                                                                        05071100
            INITIALIZE                     WS-HOLD-PLAN-NUM             05120000
                                           WS-HOLD-SUBPLAN-NUM          05130000
            MOVE ZEROES                 TO WS-RANDOM-NUM-ZEROES         05140000
                                           WS-LOW-KEY-ZEROES            05150000
                                           WS-HIGH-KEY-ZEROES           05160000
                                           WS-INPUT-REC-SKIPPED         05160100
                                           WS-INPUT-REC-CTR             05160200
                                           WS-CUSTOMER-INSERTED         05200000
                                                                        05200100
            WRITE REPORT-RECORD FROM REPORT-HEADER                      05210000
            MOVE SPACES                TO REPORT-RECORD                 05220000
            WRITE REPORT-RECORD AFTER ADVANCING 1                       05230000
            WRITE ERROR-RECORD  FROM ERROR-HEADER                       05240000
            MOVE SPACES                TO ERROR-RECORD                  05250000
            WRITE ERROR-RECORD AFTER ADVANCING 1                        05260000
            IF WS-DIGIT = 'P02' OR 'P08'                                05270000
                EXEC SQL                                                05280000
                  SELECT AGENT_ID                                       05290000
                    INTO :ZNHDAGNT-AGENT-ID                             05300000
                    FROM NZNH.AGENT                                     05310000
                   WHERE AGENT_ACFID = 'PRTBYPSN'                       05320000
                END-EXEC                                                05330000
            ELSE                                                        05340000
                EXEC SQL                                                05340100
                  SELECT AGENT_ID                                       05340200
                    INTO :ZNHDAGNT-AGENT-ID                             05340300
                    FROM NZNH.AGENT                                     05340400
                   WHERE AGENT_ACFID = 'PRTBYPSQ'                       05340500
                END-EXEC                                                05340600
            END-IF                                                      05340700
            IF SQLCODE = +0                                             05350000
               MOVE  ZNHDAGNT-AGENT-ID TO  WS-AGENT-ID                  05360000
            ELSE                                                        05380000
               MOVE  'SELECT '        TO ERR-FILE-OPERATION             05400000
               MOVE  'INITIALIZATION' TO ERR-PARA-NAME                  05410000
               MOVE  'AGENT ID'       TO ERR-FILE-NAME                  05430000
               MOVE  SQLCODE          TO WS-SQLCODE                     05440000
               PERFORM P9998-ABEND-SQL THRU P9998-EXIT                  05460100
            END-IF.                                                     05470000
       P1000-EXIT. EXIT.                                                05490000
                                                                        05500000
       P2000-READ.                                                      05510000
                                                                        05520000
           MOVE 'N' TO  WS-PLAN-CHANGE-SW                               05520100
                        WS-SUBPLAN-CHANGE-SW                            05520200
           READ PART-FILE                                               05530000
             AT END                                                     05540000
               SET WS-ALL-RECORDS-READ TO TRUE                          05550000
             NOT AT END                                                 05560000
               ADD   +1                TO WS-INPUT-REC-CTR              05570000
               MOVE  INP-PLAN-NUM      TO WS-PLAN-NUM                   05580000
               MOVE  INP-SUBPLAN-NUM                                    05590000
                                       TO WS-SUBPLAN-NUM                05600000
               MOVE  INP-SSN-NUM       TO WS-SSN                        05610000
               MOVE  INP-PART-EXT1     TO WS-SSN-EXT(1:1)               05610100
               MOVE  INP-PART-EXT1     TO WS-SSN-EXT(2:1)               05610200
               MOVE  INP-BOP-DATE      TO WS-IN-PROCESS-BOP             05620000
               MOVE  INP-EOP-DATE      TO WS-IN-PROCESS-EOP             05630000
               MOVE WS-IN-PROCESS-BOP-X(1:2)                            05640000
                                       TO WS-PROCESS-BOP-CC             05650000
               MOVE WS-IN-PROCESS-BOP-X(3:2)                            05660000
                                       TO WS-PROCESS-BOP-YY             05670000
               MOVE WS-IN-PROCESS-BOP-X(5:2)                            05680000
                                       TO WS-PROCESS-BOP-MM             05690000
               MOVE WS-IN-PROCESS-BOP-X(7:2)                            05700000
                                       TO WS-PROCESS-BOP-DD             05710000
               MOVE WS-IN-PROCESS-EOP-X(1:2)                            05720000
                                       TO WS-PROCESS-EOP-CC             05730000
               MOVE WS-IN-PROCESS-EOP-X(3:2)                            05740000
                                       TO WS-PROCESS-EOP-YY             05750000
               MOVE WS-IN-PROCESS-EOP-X(5:2)                            05760000
                                       TO WS-PROCESS-EOP-MM             05770000
               MOVE WS-IN-PROCESS-EOP-X(7:2)                            05780000
                                       TO WS-PROCESS-EOP-DD             05790000
               IF WS-PLAN-NUM NOT EQUAL WS-HOLD-PLAN-NUM                05790100
                  SET WS-ITS-NEW-PLAN TO TRUE                           05790200
               ELSE                                                     05790300
                  IF WS-SUBPLAN-NUM NOT EQUAL WS-HOLD-SUBPLAN-NUM       05790400
                     SET WS-ITS-NEW-SUBPLAN TO TRUE                     05790500
                  END-IF                                                05790600
               END-IF                                                   05790610
           END-READ.                                                    05840000
       P2000-EXIT. EXIT.                                                05870000
                                                                        05880000
       P2100-GET-ACPLAN-ID.                                             05890000
                                                                        05900000
             MOVE 2                    TO WS-DC-SOURCE-SYSTEM           05910000
             EXEC SQL                                                   05920000
               SELECT AC_PLAN_ID                                        05930000
                     ,AC_CLIENT_ID                                      05940000
                  INTO :ZNHDACPL-AC-PLAN-ID                             05950000
                      ,:ZNHDACPL-AC-CLIENT-ID                           05960000
               FROM NZNH.AC_PLAN_ID                                     05970000
               WHERE PLAN_NUM        = :WS-PLAN-NUM                     05980000
               AND   SUBPLAN_NUM     = :WS-SUBPLAN-NUM                  05990000
               AND   SOURCE_SYSTEM   = :WS-DC-SOURCE-SYSTEM             06000000
             END-EXEC                                                   06010000
                                                                        06020000
             EVALUATE SQLCODE                                           06030000
               WHEN +0                                                  06040000
                 MOVE ZNHDACPL-AC-PLAN-ID   TO  WS-AC-PLAN-ID           06050000
                 MOVE ZNHDACPL-AC-CLIENT-ID TO  WS-AC-CLIENT-ID         06070000
                 SET WS-ACPLAN-FND          TO  TRUE                    06070100
               WHEN +100                                                06100000
                 SET WS-ACPLAN-NOT-FND      TO  TRUE                    06100100
                 ADD +1 TO WS-INPUT-REC-SKIPPED                         06100110
                 INITIALIZE        ERROR-DTL                            06100200
                 MOVE   WS-SSN              TO WS-ERR-SSN               06130000
                 MOVE   WS-PLAN-NUM         TO WS-ERR-PLAN-NUM          06140000
                 MOVE   WS-SUBPLAN-NUM      TO WS-ERR-SUBPLAN-NUM       06150000
                 MOVE   'AC-PLAN-ID NOT FOUND'                          06160000
                                           TO WS-ERR-MESSAGE            06160100
                 WRITE  ERROR-RECORD   FROM ERROR-DTL                   06170000
               WHEN OTHER                                               06260000
                 MOVE  'SELECT'          TO ERR-FILE-OPERATION          06270000
                 MOVE  'P2100-GET-    '  TO ERR-PARA-NAME               06280000
                 MOVE  'AC_PLAN_ID  '    TO ERR-FILE-NAME               06290000
                 MOVE  SQLCODE           TO WS-SQLCODE                  06300000
                 PERFORM P9998-ABEND-SQL                                06310000
             END-EVALUATE.                                              06320000
                                                                        06330000
       P2100-EXIT.EXIT.                                                 06340000
                                                                        06350000
       P2200-GET-BLANK-SUBPLAN.                                         06350100
                                                                        06350200
             MOVE 2                    TO WS-DC-SOURCE-SYSTEM           06350300
             EXEC SQL                                                   06350400
               SELECT AC_PLAN_ID                                        06350500
                     ,AC_CLIENT_ID                                      06350600
                  INTO :ZNHDACPL-AC-PLAN-ID                             06350700
               FROM NZNH.AC_PLAN_ID                                     06350900
               WHERE PLAN_NUM        = :WS-PLAN-NUM                     06351000
               AND   SUBPLAN_NUM     = '      '                         06351100
               AND   SOURCE_SYSTEM   = :WS-DC-SOURCE-SYSTEM             06351200
             END-EXEC                                                   06351300
                                                                        06351400
             EVALUATE SQLCODE                                           06351500
               WHEN +0                                                  06351600
                 MOVE  ZNHDACPL-AC-PLAN-ID  TO  ZNHDPLIR-AC-PLAN-ID     06351610
                 PERFORM P4400-INSERT-PLAN-INCL-RQST THRU P4400-EXIT    06351800
               WHEN +100                                                06352000
                 CONTINUE                                               06352100
               WHEN OTHER                                               06352900
                 MOVE  'SELECT'          TO ERR-FILE-OPERATION          06353000
                 MOVE  'P2200-GET-    '  TO ERR-PARA-NAME               06353100
                 MOVE  'AC_PLAN_ID  '    TO ERR-FILE-NAME               06353200
                 MOVE  SQLCODE           TO WS-SQLCODE                  06353300
                 PERFORM P9998-ABEND-SQL                                06353400
             END-EVALUATE.                                              06353500
                                                                        06353600
       P2200-EXIT.EXIT.                                                 06353700
                                                                        06353800
       P3000-PROCESS-RECORD.                                            06360000
             PERFORM P2000-READ  THRU P2000-EXIT                        06360020
                                                                        06360021
             IF WS-ALL-RECORDS-READ                                     06360030
                GO TO P3000-EXIT                                        06360040
             END-IF                                                     06360050
                                                                        06360060
             IF WS-ITS-NEW-PLAN OR WS-ITS-NEW-SUBPLAN                   06360204
                PERFORM P2100-GET-ACPLAN-ID THRU P2100-EXIT             06360205
             END-IF                                                     06360206
                                                                        06360207
             IF  WS-ACPLAN-NOT-FND                                      06360208
                 PERFORM P3400-SKIP-PLAN THRU P3400-EXIT                06360209
                      UNTIL WS-ACPLAN-FND OR WS-ALL-RECORDS-READ        06360210
                 IF WS-ALL-RECORDS-READ                                 06360211
                    GO TO P3000-EXIT                                    06360212
                 END-IF                                                 06360213
             END-IF                                                     06360214
                                                                        06360220
             PERFORM P3200-GET-CUSTOMER-ID THRU P3200-EXIT              06370000
             IF  WS-RECORD-NOT-VALID                                    06370100
                GO TO P3000-EXIT                                        06370200
             END-IF                                                     06370300
             IF WS-ITS-NEW-PLAN                                         06990212
                IF WS-NOT-FIRST-RECORD                                  06990213
                   PERFORM P3100-INSERT-CUST-INCL-RQST                  06990220
                                       THRU P3100-EXIT                  06990230
                   PERFORM  P4500-WRITE-REPORT-DTL                      06990300
                                       THRU P4500-EXIT                  06990301
                   MOVE ZEROES         TO  CUSTOMER-COUNT               06990310
                END-IF                                                  06990400
                PERFORM P3300-CREATE-NEW-RQST                           06990500
                                       THRU P3300-EXIT                  06990600
                GO TO P3000-EXIT                                        07110500
             END-IF.                                                    07160100
                                                                        07160200
             IF WS-ITS-NEW-SUBPLAN                                      07160300
                MOVE WS-AC-PLAN-ID  TO  ZNHDPLIR-AC-PLAN-ID             07160301
                PERFORM P4400-INSERT-PLAN-INCL-RQST THRU P4400-EXIT     07160310
                MOVE WS-SUBPLAN-NUM   TO  WS-HOLD-SUBPLAN-NUM           07160311
             END-IF                                                     07160320
                                                                        07160330
             ADD +1              TO CUSTOMER-COUNT                      07160340
             MOVE WS-TBL-CUST-ID TO WS-CUSTOMER-ID(CUSTOMER-COUNT)      07160350
             IF (CUSTOMER-COUNT >= WS-PART-COUNT)                       07160400
                PERFORM P3100-INSERT-CUST-INCL-RQST THRU P3100-EXIT     07160500
                MOVE ZEROES         TO  CUSTOMER-COUNT                  07160600
             END-IF.                                                    07160800
                                                                        07540000
       P3000-EXIT. EXIT.                                                07640000
                                                                        07650000
       P3100-INSERT-CUST-INCL-RQST.                                     07660000
             IF CUSTOMER-COUNT EQUAL ZEROES                             07670000
                GO TO P3100-EXIT                                        07670100
             END-IF                                                     07670200
             MOVE WS-RANDOM-NUMBER       TO ZNHDCUIR-STMT-RQST-ID       07740000
             MOVE 'N'                    TO ZNHDCUIR-SMPL-CUST-YN       07750000
             MOVE ZEROES                 TO ZNHDCUIR-RSLT-CD            07760000
             MOVE CUSTOMER-COUNT         TO NUM-ROWS                    07760100
                                                                        07780000
             EXEC SQL                                                   07790000
                 INSERT INTO CUST_INCL_ON_RQST                          07800000
                     (STMT_RQST_ID                                      07810000
                    , CUSTOMER_ID                                       07820000
                    , SMPL_CUST_YN                                      07830000
                    , RSLT_CD)                                          07840000
                 VALUES                                                 07850000
                     (:ZNHDCUIR-STMT-RQST-ID                            07860000
                    , :WS-CUSTOMER-ID                                   07880000
                    , :ZNHDCUIR-SMPL-CUST-YN                            07900000
                    , :ZNHDCUIR-RSLT-CD)                                07930000
                      FOR :NUM-ROWS ROWS                                07940000
                      NOT ATOMIC CONTINUE ON SQLEXCEPTION               07950000
             END-EXEC.                                                  07960000
                                                                        07970000
             IF SQLCODE = +0                                            07970100
                    ADD CUSTOMER-COUNT          TO WS-CUST-PER-REQUEST  07970110
                                                   WS-CUSTOMER-INSERTED 07970111
                    MOVE SPACES                 TO  COUNT-RECORD        07970112
                    MOVE 'RECORDS PROCESSED  :' TO  WS-COUNT-DTL        07970113
                    MOVE WS-INPUT-REC-CTR       TO  WS-RECORD-COUNT     07970115
                    MOVE COUNT-LINE2            TO  COUNT-RECORD        07970116
                    WRITE COUNT-RECORD AFTER ADVANCING 1                07970117
             ELSE                                                       07970300
                 IF SQLCODE = -253                                      07970400
                    INITIALIZE DB2RC                                    07970410
                               ROW-COUNT                                07970420
                               ERRCNT                                   07970430
                    EXEC SQL                                            07970431
                      GET DIAGNOSTICS                                   07970432
                          :ROW-COUNT = ROW_COUNT,                       07970434
                          :ERRCNT    = NUMBER                           07970435
                    END-EXEC                                            07970436
                    ADD ROW-COUNT TO WS-CUST-PER-REQUEST                07970449
                                     WS-CUSTOMER-INSERTED               07970450
                    MOVE SPACES                 TO  COUNT-RECORD        07970460
                    MOVE 'RECORDS PROCESSED  :' TO  WS-COUNT-DTL        07970470
                    MOVE WS-INPUT-REC-CTR       TO  WS-RECORD-COUNT     07970480
                    MOVE COUNT-LINE2            TO  COUNT-RECORD        07970490
                    WRITE COUNT-RECORD AFTER ADVANCING 1                07970491
                    GO TO P3100-EXIT                                    07970492
                END-IF                                                  07970500
                EXEC SQL                                                08060000
                 GET DIAGNOSTICS CONDITION  2                           08070000
                     :ERR-SQLCODE  = DB2_RETURNED_SQLCODE,              08080000
                     :ERR-SQLSTATE = RETURNED_SQLSTATE,                 08090000
                     :ROW-NUM      = DB2_ROW_NUMBER                     08100000
                END-EXEC                                                08120000
                DISPLAY 'SQLCODE        :'  ERR-SQLCODE                 08170000
                DISPLAY 'ROW SQLCODE    :'  ERR-SQLCODE                 08170010
                DISPLAY 'SQLSTATE       :'  ERR-SQLSTATE                08170100
                DISPLAY 'CUSTOMER ID    :'  WS-CUSTOMER-ID(ROW-NUM)     08170200
                DISPLAY 'PLAN NUM       :'  WS-SUBPLAN-NUM              08170300
                MOVE  'INSERT'          TO  ERR-FILE-OPERATION          08190000
                MOVE  'P3100-INSERT  '  TO  ERR-PARA-NAME               08190100
                MOVE  'CUST_INCL_ON'    TO  ERR-FILE-NAME               08190200
                MOVE  ERR-SQLCODE       TO WS-SQLCODE                   08190300
                PERFORM P9998-ABEND-SQL                                 08190400
             END-IF.                                                    08450010
                                                                        08640000
       P3100-EXIT.EXIT.                                                 08650000
       P3200-GET-CUSTOMER-ID.                                           08660000
      *CHECKS IF THE CUSTOMER EXISTS IN THE STATEMENT TABLES (CUSTOMER  08660100
      *        AND CUSTOMER_PLAN)                                       08660200
                                                                        08660300
             EXEC SQL                                                   08660400
                 SELECT A.CUSTOMER_ID                                   08660500
                   INTO :WS-TBL-CUST-ID                                 08660600
                 FROM NZNH.CUSTOMER         A,                          08660700
                      NZNH.CUSTOMER_PLAN    B                           08660800
                 WHERE A.SSN         = :WS-SSN                          08660900
                 AND  A.SSN_SUFFIX   = :WS-SSN-EXT                      08660910
                 AND  A.CUSTOMER_ID  = B.CUSTOMER_ID                    08661000
                 AND  B.AC_PLAN_ID   = :WS-AC-PLAN-ID                   08661100
             END-EXEC                                                   08661200
                                                                        08661300
             EVALUATE SQLCODE                                           08661400
                WHEN +0                                                 08661500
                   SET WS-VALID-RECORD-FND TO TRUE                      08661510
d77732*            ADD +1              TO CUSTOMER-COUNT                08661520
d77732*            MOVE WS-TBL-CUST-ID TO WS-CUSTOMER-ID(CUSTOMER-COUNT)08661600
                WHEN +100                                               08661900
                   SET WS-RECORD-NOT-VALID TO TRUE                      08661901
                   ADD +1 TO WS-INPUT-REC-SKIPPED                       08661902
                   INITIALIZE        ERROR-DTL                          08661903
                   MOVE   WS-SSN              TO WS-ERR-SSN             08661904
                   MOVE   WS-PLAN-NUM         TO WS-ERR-PLAN-NUM        08661905
                   MOVE   WS-SUBPLAN-NUM      TO WS-ERR-SUBPLAN-NUM     08661906
                   MOVE   'CUSTOMER-ID NOT FOUND'                       08661907
                                             TO WS-ERR-MESSAGE          08661908
                   WRITE  ERROR-RECORD   FROM ERROR-DTL                 08661909
               WHEN OTHER                                               08661910
                 MOVE  'SELECT'          TO ERR-FILE-OPERATION          08661911
                 MOVE  'P3200-GET-   '   TO ERR-PARA-NAME               08661912
                 MOVE  'CUSTOMER ID '    TO ERR-FILE-NAME               08661913
                 MOVE  SQLCODE           TO WS-SQLCODE                  08661914
                 PERFORM P9998-ABEND-SQL                                08661915
             END-EVALUATE.                                              08664100
       P3200-EXIT. EXIT.                                                08664200
                                                                        08664300
       P3300-CREATE-NEW-RQST.                                           08664310
             PERFORM P4000-GENERATE-REQUEST-ID  THRU P4000-EXIT         08664400
             MOVE 'N'               TO  WS-INSERT-SUCCESSFUL-SW         08664500
                                                                        08664510
             PERFORM  P4300-INSERT-STMT-RQST                            08664600
                        THRU  P4300-EXIT UNTIL WS-INSERT-SUCCESSFUL     08664700
             ADD +1              TO CUSTOMER-COUNT                      08664701
             MOVE WS-TBL-CUST-ID TO WS-CUSTOMER-ID(CUSTOMER-COUNT)      08664702
                                                                        08664710
             MOVE WS-AC-PLAN-ID  TO  ZNHDPLIR-AC-PLAN-ID                08664800
             PERFORM P4400-INSERT-PLAN-INCL-RQST THRU P4400-EXIT        08664900
                                                                        08664910
             PERFORM P2200-GET-BLANK-SUBPLAN     THRU P2200-EXIT        08665100
                                                                        08665210
             SET WS-NOT-FIRST-RECORD TO TRUE                            08665300
             MOVE ZEROS TO WS-CUST-PER-REQUEST.                         08665600
             MOVE WS-PLAN-NUM      TO  WS-HOLD-PLAN-NUM                 08665601
             MOVE WS-SUBPLAN-NUM   TO  WS-HOLD-SUBPLAN-NUM.             08665602
                                                                        08665610
       P3300-EXIT. EXIT.                                                08665700
                                                                        08665800
       P3400-SKIP-PLAN.                                                 08665900
                                                                        08665901
             MOVE WS-PLAN-NUM     TO WS-INVALID-PLAN-NUM                08665910
             MOVE WS-SUBPLAN-NUM  TO WS-INVALID-SUBPLAN-NUM             08665920
             PERFORM P2000-READ   THRU P2000-EXIT                       08665930
                                                                        08665931
             IF (WS-PLAN-NUM NOT EQUAL TO WS-INVALID-PLAN-NUM)          08665933
             OR (WS-SUBPLAN-NUM NOT EQUAL TO WS-INVALID-SUBPLAN-NUM)    08665934
                PERFORM P2100-GET-ACPLAN-ID  THRU P2100-EXIT            08665940
             END-IF.                                                    08665950
                                                                        08665960
       P3400-EXIT.                                                      08666000
                                                                        08666100
       P4000-GENERATE-REQUEST-ID.                                       08670000
                                                                        08680000
            ADD +1 TO WS-LOAD-CTR                                       08680100
            IF WS-LOAD-CTR > 32                                         08680200
               MOVE 1 TO WS-LOAD-CTR                                    08680300
            END-IF                                                      08680400
                                                                        08680500
000923     PERFORM P4100-SEARCH-PARTITION-TABLE                         08690000
000923        THRU P4100-EXIT                                           08700000
000924                                                                  08710000
000925     MOVE WS-TIMESTAMP           TO WS-TIMESTAMP-PART             08720000
000926     MOVE WS-TIMESTAMP-SEC       TO WS-RANDOM-NUM-LOW-PART        08730000
000927     COMPUTE WS-RANDOM-NUM-HIGH-PART =                            08740000
000928             WS-LOW-KEY-HIGH-PART + WS-TIMESTAMP-MIL              08750000
000929                                                                  08760000
000930     IF WS-RANDOM-NUMBER < WS-LOW-KEY-REDEF                       08770000
000931        MOVE 'N'                 TO WS-INCREASE-SW                08780000
000932        PERFORM P4110-INCREASE-RQST-ID                            08790000
000932           THRU P4110-EXIT   UNTIL WS-INCREASE-SW = 'Y'           08800000
000934     END-IF                                                       08810000
000935                                                                  08820000
000936     IF WS-RANDOM-NUMBER > WS-HIGH-KEY-REDEF                      08830000
000937        MOVE 'N'                 TO WS-DECREASE-SW                08840000
000938        PERFORM P4120-DECREASE-RQST-ID                            08850000
000938           THRU P4120-EXIT  UNTIL WS-DECREASE-SW = 'Y'            08860000
000940     END-IF                                                       08870000
000941                                                                  08880000
           .                                                            08900000
000944 P4000-EXIT.                                                      08910000
000945     EXIT.                                                        08920000
000946                                                                  08930000
000947 P4100-SEARCH-PARTITION-TABLE.                                    08940000
000949                                                                  08950000
000950     SET WS-RANGE-IDX            TO 1                             08960000
000951     SEARCH WS-RANGE-DATA VARYING WS-RANGE-IDX                    08970000
000952       AT END                                                     08980000
000953         DISPLAY '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'08990000
000954         DISPLAY 'DATA ERROR: CANT FIND A MATCH ON PARTITION # '  09000000
000955                  WS-LOAD-CTR                                     09010000
000956         DISPLAY 'PROGRAM WILL BE TERMINATED '                    09020000
000957         DISPLAY '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'09030000
000958         MOVE 'SEARCH'           TO ERR-FILE-OPERATION            09040000
000959         MOVE 'PARTITION-TABLE'  TO ERR-FILE-NAME                 09050000
000960         MOVE  ZEROES            TO ERR-FILE-STATUS               09060000
000961         MOVE '4100'             TO ERR-PARA-NAME                 09070000
000962         PERFORM P9999-ABEND-PROGRAM                              09080000
000963       WHEN WS-RANGE-RQST-PARTITION(WS-RANGE-IDX) = WS-LOAD-CTR   09090000
000964         MOVE WS-RANGE-LOW-KEY-9(WS-LOAD-CTR)                     09100000
000965                                 TO WS-LOW-KEY-REDEF              09110000
000966         MOVE WS-RANGE-HIGH-KEY-9(WS-LOAD-CTR)                    09120000
000967                                 TO WS-HIGH-KEY-REDEF             09130000
000968     END-SEARCH.                                                  09140000
000969                                                                  09150000
000970 P4100-EXIT.                                                      09160000
000971     EXIT.                                                        09170000
000972                                                                  09180000
000973 P4110-INCREASE-RQST-ID.                                          09190000
000975                                                                  09200000
000976     COMPUTE WS-RANDOM-NUMBER = WS-RANDOM-NUMBER + 500000         09210000
000977                                                                  09220000
000978     IF WS-RANDOM-NUMBER > WS-LOW-KEY-REDEF                       09230000
000979        MOVE 'Y'                 TO WS-INCREASE-SW                09240000
000980     ELSE                                                         09260000
000981        CONTINUE                                                  09270000
000982     END-IF.                                                      09280000
000983                                                                  09290000
000984 P4110-EXIT.                                                      09300000
000985     EXIT.                                                        09310000
000986                                                                  09320000
000987 P4120-DECREASE-RQST-ID.                                          09330000
000989                                                                  09340000
000990     COMPUTE WS-RANDOM-NUMBER = WS-RANDOM-NUMBER - 500000         09350000
000991                                                                  09360000
000992     IF WS-RANDOM-NUMBER > WS-HIGH-KEY-REDEF                      09370000
000993        CONTINUE                                                  09380000
000994     ELSE                                                         09390000
000995        MOVE 'Y'                 TO WS-DECREASE-SW                09400000
000996     END-IF.                                                      09420000
000997                                                                  09430000
000998 P4120-EXIT.                                                      09440000
000999     EXIT.                                                        09450000
                                                                        09460000
                                                                        09470000
       P4300-INSERT-STMT-RQST.                                          09480000
                                                                        09490000
001011     MOVE 3                      TO ZNHDSTRQ-STMT-TYPE-ID         09500000
001007     MOVE 'N'                    TO ZNHDSTRQ-ALL-SSN-YN           09510000
001015                                                                  09520000
001016     MOVE WS-RANDOM-NUMBER       TO ZNHDSTRQ-STMT-RQST-ID         09530000
001017     MOVE WS-AC-CLIENT-ID        TO ZNHDSTRQ-AC-CLIENT-ID         09540000
001017                                                                  09550000
001018     MOVE '3'                    TO ZNHDSTRQ-STMT-STAT-ID         09560000
001020     MOVE WS-AGENT-ID            TO ZNHDSTRQ-CREATE-BY            09570000
001021                                    ZNHDSTRQ-UPDATE-BY            09580000
001024     MOVE WS-PROCESS-BOP         TO ZNHDSTRQ-AS-OF-FROM-DT        09590000
001025     MOVE WS-PROCESS-EOP         TO ZNHDSTRQ-TO-DT                09600000
001026     MOVE WS-DEFAULT-DT          TO ZNHDSTRQ-SENT-TO-VNDR-DT      09610000
001027     MOVE WS-DEFAULT-DT          TO ZNHDSTRQ-SENT-FROM-VNDR-DT    09620000
001028     MOVE WS-TIMESTAMP           TO ZNHDSTRQ-CREATE-TMSTMP        09630000
001029     MOVE WS-DEFAULT-TMSTMP      TO ZNHDSTRQ-UPDATE-TMSTMP        09640000
001030     MOVE 1                      TO ZNHDSTRQ-DEST-OPT-ID          09650000
001031     MOVE 'Y'                    TO ZNHDSTRQ-DSPL-ONLINE-YN       09660000
001032                                                                  09670000
001033     EXEC SQL                                                     09680000
001034         INSERT INTO STMT_RQST                                    09690000
001035             (STMT_RQST_ID                                        09700000
001036            , AC_CLIENT_ID                                        09710000
001037            , STMT_TYPE_ID                                        09720000
001038            , STMT_STAT_ID                                        09730000
001039            , STMT_STAT_TMSTMP                                    09740000
001040            , ALL_SSN_YN                                          09750000
001041            , AS_OF_FROM_DT                                       09760000
001042            , TO_DT                                               09770000
001043            , SENT_TO_VNDR_DT                                     09780000
001044            , SENT_FROM_VNDR_DT                                   09790000
001045            , CREATE_BY                                           09800000
001046            , CREATE_TMSTMP                                       09810000
001047            , UPDATE_BY                                           09820000
001048            , UPDATE_TMSTMP                                       09830000
001049            , DEST_OPT_ID                                         09840000
001050            , DSPL_ONLINE_YN)                                     09850000
001051         VALUES                                                   09860000
001052             (:ZNHDSTRQ-STMT-RQST-ID                              09870000
001053            , :ZNHDSTRQ-AC-CLIENT-ID                              09880000
001054            , :ZNHDSTRQ-STMT-TYPE-ID                              09890000
001055            , :ZNHDSTRQ-STMT-STAT-ID                              09900000
001056            , CURRENT TIMESTAMP                                   09910000
001057            , :ZNHDSTRQ-ALL-SSN-YN                                09920000
001058            , :ZNHDSTRQ-AS-OF-FROM-DT                             09930000
001059            , :ZNHDSTRQ-TO-DT                                     09940000
001060            , :ZNHDSTRQ-SENT-TO-VNDR-DT                           09950000
001061            , :ZNHDSTRQ-SENT-FROM-VNDR-DT                         09960000
001062            , :ZNHDSTRQ-CREATE-BY                                 09970000
001063            , :ZNHDSTRQ-CREATE-TMSTMP                             09980000
001064            , :ZNHDSTRQ-UPDATE-BY                                 09990000
001065            , :ZNHDSTRQ-UPDATE-TMSTMP                             10000000
001066            , :ZNHDSTRQ-DEST-OPT-ID                               10010000
001067            , :ZNHDSTRQ-DSPL-ONLINE-YN)                           10020000
001068     END-EXEC.                                                    10030000
001069                                                                  10040000
001070     IF SQLCODE = +0                                              10050000
001073        SET WS-INSERT-SUCCESSFUL TO TRUE                          10080000
001074        ADD +1                   TO WS-TOT-RQST-CREATED           10090000
001075     ELSE                                                         10100000
001076        IF SQLCODE = -803                                         10110000
001077           ADD +1                TO WS-RANDOM-NUMBER              10120000
001078                                                                  10130000
001079           IF WS-RANDOM-NUMBER > WS-HIGH-KEY-REDEF                10140000
001080              MOVE 'N'           TO WS-DECREASE-SW                10150000
001081              PERFORM P4120-DECREASE-RQST-ID                      10160000
001081                 THRU P4120-EXIT UNTIL WS-DECREASE-SW = 'Y'       10170000
001083           END-IF                                                 10180000
001084                                                                  10190000
001085           IF WS-RANDOM-NUMBER < WS-LOW-KEY-REDEF                 10200000
001086              MOVE 'N'           TO WS-INCREASE-SW                10210000
001087              PERFORM P4110-INCREASE-RQST-ID                      10220000
001087                 THRU P4110-EXIT UNTIL WS-INCREASE-SW = 'Y'       10230000
001089           END-IF                                                 10240000
001090                                                                  10250000
001091           MOVE WS-RANDOM-NUMBER TO ZNHDSTRQ-STMT-RQST-ID         10260000
001092           DISPLAY 'STMT-RQST-ID ALREADY EXISTS, BUMPING UP'      10270000
000000                   ' RQST ID' WS-RANDOM-NUMBER                    10270100
001093        ELSE                                                      10280000
001110           MOVE 'INSERT'           TO ERR-FILE-OPERATION          10310000
001111           MOVE 'P4300-INSERT- '   TO ERR-PARA-NAME               10320000
001111           MOVE 'STMT_RQST     '   TO ERR-FILE-NAME               10320100
001112           MOVE SQLCODE            TO WS-SQLCODE                  10330000
                 PERFORM P9998-ABEND-SQL                                10380000
001115        END-IF                                                    10390000
001116     END-IF.                                                      10400000
             .                                                          10410000
       P4300-EXIT.EXIT.                                                 10420000
                                                                        10430000
       P4400-INSERT-PLAN-INCL-RQST.                                     10440000
                                                                        10450000
            MOVE WS-RANDOM-NUMBER      TO  ZNHDPLIR-STMT-RQST-ID        10460000
            MOVE SPACES                TO  ZNHDPLIR-PLAN-NAME           10480000
            MOVE 'N'                   TO  ZNHDPLIR-PRIMARY-PLAN-YN     10490000
            MOVE ZEROES                TO  ZNHDPLIR-MAC-GRP-NO          10500000
            MOVE 'W'                   TO  ZNHDPLIR-PLAN-EXTR-STAT      10510000
            MOVE WS-PROCESS-BOP        TO  ZNHDPLIR-AS-OF-FROM-DT       10520000
            MOVE WS-PROCESS-EOP        TO  ZNHDPLIR-TO-DT               10530000
                                                                        10540000
           EXEC SQL                                                     10550000
               INSERT INTO PLAN_INCL_ON_RQST                            10560000
                   (STMT_RQST_ID                                        10570000
                  , AC_PLAN_ID                                          10580000
                  , PLAN_NAME                                           10590000
                  , PRIMARY_PLAN_YN                                     10600000
                  , MAC_GRP_NO                                          10610000
                  , PLAN_EXTR_STAT                                      10620000
                  , AS_OF_FROM_DT                                       10630000
                  , TO_DT)                                              10640000
               VALUES                                                   10650000
                   (:ZNHDPLIR-STMT-RQST-ID                              10660000
                  , :ZNHDPLIR-AC-PLAN-ID                                10670000
                  , :ZNHDPLIR-PLAN-NAME                                 10680000
                  , :ZNHDPLIR-PRIMARY-PLAN-YN                           10690000
                  , :ZNHDPLIR-MAC-GRP-NO                                10700000
                  , :ZNHDPLIR-PLAN-EXTR-STAT                            10710000
                  , :ZNHDPLIR-AS-OF-FROM-DT                             10720000
                  , :ZNHDPLIR-TO-DT)                                    10730000
            END-EXEC.                                                   10740000
                                                                        10750000
001179     IF SQLCODE = +0 OR -803                                      10760000
              CONTINUE                                                  10760100
           ELSE                                                         10760200
001182        MOVE 'INSERT'            TO ERR-FILE-OPERATION            10800000
001183        MOVE 'PLAN_INCL_ON_RQST' TO ERR-FILE-NAME                 10810000
001184        MOVE SQLCODE             TO WS-SQLCODE                    10820000
001185        MOVE '4400-INSERT-'      TO ERR-PARA-NAME                 10830000
              PERFORM P9998-ABEND-SQL                                   10870000
001187     END-IF.                                                      10880000
             .                                                          10890000
       P4400-EXIT.EXIT.                                                 10900000
                                                                        10910000
       P4500-WRITE-REPORT-DTL.                                          10920000
                                                                        10930000
           INITIALIZE                      REPORT-DTL                   10940000
           MOVE WS-RANDOM-NUMBER       TO  WS-RQST-ID                   10950000
           MOVE WS-HOLD-PLAN-NUM       TO  WS-PLAN-NUMBER               10960000
           MOVE WS-CUST-PER-REQUEST    TO  WS-PART-INCLD-IN-RQST        10970000
           MOVE SPACES                 TO  REPORT-RECORD                10990000
           WRITE REPORT-RECORD FROM  REPORT-DTL                         11000000
           MOVE ZEROS TO WS-CUST-PER-REQUEST.                           11010000
       P4500-EXIT.EXIT.                                                 11030000
                                                                        11040000
001401 P5000-WRAP-UP-PROCESS.                                           11610000
001403                                                                  11620000
           PERFORM P3100-INSERT-CUST-INCL-RQST THRU P3100-EXIT          11620100
           PERFORM  P4500-WRITE-REPORT-DTL THRU  P4500-EXIT             11630000
                                                                        11640000
           MOVE  WS-INPUT-REC-CTR      TO  WS-TOTAL-RECORDS             11660000
           MOVE 'TOTAL RECORDS READ     :' TO WS-FOOTER-MSG             11660100
           MOVE  SPACES                TO  REPORT-RECORD                11670000
           MOVE  REPORT-FOOTER         TO  REPORT-RECORD                11680000
           WRITE REPORT-RECORD AFTER ADVANCING 1                        11690000
                                                                        11690001
           MOVE  WS-INPUT-REC-SKIPPED  TO  WS-TOTAL-RECORDS             11690002
           MOVE 'TOTAL RECORDS SKIPPED  :' TO WS-FOOTER-MSG             11690003
           MOVE  SPACES                TO  REPORT-RECORD                11690004
           MOVE  REPORT-FOOTER         TO  REPORT-RECORD                11690005
           WRITE REPORT-RECORD AFTER ADVANCING 1                        11690006
                                                                        11690007
           MOVE  WS-TOT-RQST-CREATED   TO  WS-TOTAL-RECORDS             11690008
           MOVE 'TOTAL REQUEST CREATED  :' TO WS-FOOTER-MSG             11690009
           MOVE  SPACES                TO  REPORT-RECORD                11690010
                                           COUNT-RECORD                 11690011
           MOVE  REPORT-FOOTER         TO  REPORT-RECORD                11690012
                                           COUNT-RECORD                 11690013
           WRITE REPORT-RECORD AFTER ADVANCING 1                        11690014
           WRITE COUNT-RECORD  AFTER ADVANCING 1                        11690020
                                                                        11690030
           MOVE  WS-CUSTOMER-INSERTED  TO  WS-TOTAL-RECORDS             11690100
           MOVE 'TOTAL CUSTOMER INSERTED:' TO WS-FOOTER-MSG             11690200
           MOVE  SPACES                TO  REPORT-RECORD                11700000
                                           COUNT-RECORD                 11700100
           MOVE REPORT-FOOTER          TO  REPORT-RECORD                11710000
                                           COUNT-RECORD                 11710100
           WRITE REPORT-RECORD AFTER ADVANCING 1                        11720000
           WRITE COUNT-RECORD  AFTER ADVANCING 1                        11720010
           MOVE SPACES                 TO  COUNT-RECORD                 11720400
           MOVE COUNT-FOOTER           TO  COUNT-RECORD                 11720700
           WRITE COUNT-RECORD AFTER ADVANCING 1                         11720800
                                                                        11730000
001404     CLOSE PART-FILE                                              11740000
                 RPT-FILE                                               11750000
                 ERROR-FILE                                             11760000
                 COUNT-FILE                                             11770000
001405     .                                                            11780000
001406 P5000-EXIT.                                                      11790000
001407     EXIT.                                                        11800000
001408                                                                  11810000
001409 P9998-ABEND-SQL.                                                 11820000
001411                                                                  11830000
           DISPLAY '**********************************************'     11840000
           DISPLAY '******   DFP8B001 ABENDING      **************'     11850000
           DISPLAY '**********************************************'     11860000
           DISPLAY '* DFP8B001 - SQL PROCESSING ERROR    *********'     11870000
           DISPLAY '* SQL CODE            : ' WS-SQLCODE.               11880000
           DISPLAY '* ERROR SQL OPERATION : ' ERR-FILE-OPERATION.       11890000
           DISPLAY '* TABLE               : ' ERR-FILE-NAME.            11900000
001418     DISPLAY '* ERROR PARA NAME     : ' ERR-PARA-NAME.            11910000
           DISPLAY '**********************************************'     11920000
001421                                                                  11930000
001422     MOVE +9998                  TO WS-ABEND-CODE.                11940000
001423     CALL WS-ABEND-PGM USING WS-ABEND-CODE.                       11950000
001424                                                                  11960000
001425 P9998-EXIT.                                                      11970000
001426     EXIT.                                                        11980000
001427                                                                  11990000
001428 P9999-ABEND-PROGRAM.                                             12000000
001430                                                                  12010000
001431     DISPLAY '**********************************************'.    12020000
001432     DISPLAY '**********************************************'.    12030000
001433     DISPLAY '**PROGRAM IS BEING TERMINATED...'.                  12040000
001434     DISPLAY '**ERROR IN FILE       :  ' ERR-FILE-NAME.           12050000
001435     DISPLAY '**ERROR FILE OPERATION:  ' ERR-FILE-OPERATION.      12060000
001436     DISPLAY '**ERROR FILE STATUS   :  ' ERR-FILE-STATUS.         12070000
001437     DISPLAY '**ERROR PARA NAME     :  ' ERR-PARA-NAME.           12080000
001438     DISPLAY '**********************************************'.    12090000
001439     DISPLAY '**********************************************'.    12100000
001440                                                                  12110000
001441     MOVE +9999                  TO WS-ABEND-CODE.                12120000
001442     CALL WS-ABEND-PGM USING WS-ABEND-CODE.                       12130000
001443                                                                  12140000
001444 P9999-EXIT.                                                      12150000
001445     EXIT.                                                        12160000
