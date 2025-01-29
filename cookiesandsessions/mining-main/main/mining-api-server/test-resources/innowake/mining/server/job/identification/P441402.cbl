00001 *************************                                         09/12/08
00002  IDENTIFICATION DIVISION.                                         P441402
00003 *************************                                            LV002
00004                                                                   P441402
00005  PROGRAM-ID.    P441402.                                          P441402
00006  AUTHOR.        KATHY GARRETT.                                    P441402
00007  DATE-WRITTEN.  AUGUST  2004.                                     P441402
00008  DATE-COMPILED.                                                   P441402
00009  INSTALLATION.  TEXAS DEPT OF TRANSPORTATION, AUSTIN, TX.         P441402
00010                                                                   P441402
00011 ***************************************************************** P441402
00012 *    APPLICATION AREA : KODAK IMAGELINK SYSTEM                  * P441402
00013 *                       RETRIEVE DATA RECORDS FOR A GIVEN       * P441402
00014 *                       PARTIAL KEY FROM KCAA VSAM FILE.        * P441402
00015 *                                                               * P441402
00016 *    1.  DESCRIPTION:                                           * P441402
00017 *        THIS MODULE READS THE FOLLOWING VSAM FILE:             * P441402
00018 *         : KCAA (KODAK) MICROFILM IMAGE FILE                   * P441402
00019 *                                                               * P441402
00020 *        READ LIST OF DOCNO(S) TO BE RETERIEVED FROM THE IMAGE  * P441402
00021 *        FILE - VDM REQUESTS A LIST OF ALL RECORDS MATCHING THE * P441402
00022 *        PARTIAL KEY IN ORDER TO TRY AND FIND THE CORRECT REC.  * P441402
00023 *                                                               * P441402
00024 *    2.  SPECIAL CONSIDERATIONS:                                * P441402
00025 *                                                               * P441402
00026 *        THIS PROGRAM SHOULD BE COMPILED USING MVS COBOL:       * P441402
00027 *        'EXEC PNCBMCL' AND THE FOLLOWING OPTIONS:              * P441402
00028 *         PARM.COBMVS='DYNAM,XREF,LIST,MAP,DATA(24)'            * P441402
00029 *                                                               * P441402
00030 *        LINK EDIT USING THE FOLLOWING CONTROL STATEMENTS:      * P441402
00031 *         PARM.LKED=CALL,LIST,LET,XREF,AMODE(24),RMODE(24)'     *    CL**2
00032 *           ENTRY P441402                                      *  P441402
00033 *           SETSSI UUUUYJJJ   >> USER ID AND JULIAN DATE <<     * P441402
00034 *           NAME P441402(R)                                    *  P441402
00035 *                                                               * P441402
00036 ******************* MAINTENANCE SCHEDULE************************* P441402
00037 *                                                               * P441402
00038 * PAN LVL  PRGRMR     DATE      IRR #     DESCRIPTION OF CHANGE * P441402
00039 * -------  ------  ----------  ---------  --------------------- * P441402
00040 *   001     KMG    08/31/2005  52040016   INITIAL PAN LOAD      * P441402
00041 *   002     KMG    09/12/2008  5PRDSUP09  ALLOW FULL DOCNO MATCH*    CL**2
00042 ***************************************************************** P441402
00043      EJECT                                                        P441402
00044 **********************                                            P441402
00045  ENVIRONMENT DIVISION.                                            P441402
00046 **********************                                            P441402
00047                                                                   P441402
00048  CONFIGURATION SECTION.                                           P441402
00049  INPUT-OUTPUT SECTION.                                            P441402
00050  FILE-CONTROL.                                                    P441402
00051                                                                   P441402
00052      SELECT KODAK-INDEX-DATA-FILE                                 P441402
00053          ASSIGN TO KCAADAT                                        P441402
00054          ORGANIZATION IS INDEXED                                  P441402
00055          ACCESS MODE IS SEQUENTIAL                                P441402
00056          RECORD KEY IS KODAK-INDEX-DATA-KEY                       P441402
00057          FILE STATUS IS KODAK-FILE-STATUS.                        P441402
00058                                                                   P441402
00059      SELECT INPUT-SELECT-FILE          ASSIGN TO KCAACTL.         P441402
00060      SELECT INPUT-DISPLAY-C-FILE       ASSIGN TO KCAACNT.         P441402
00061      EJECT                                                        P441402
00062                                                                   P441402
00063 ***************                                                   P441402
00064  DATA DIVISION.                                                   P441402
00065 ***************                                                   P441402
00066  FILE SECTION.                                                    P441402
00067                                                                   P441402
00068  FD  INPUT-DISPLAY-C-FILE                                         P441402
00069      LABEL RECORDS ARE OMITTED                                    P441402
00070      BLOCK CONTAINS 0 CHARACTERS                                  P441402
00071      RECORD CONTAINS 80 CHARACTERS                                P441402
00072      DATA RECORD IS INPUT-DISPLAY-C-REC.                          P441402
00073                                                                   P441402
00074  01  INPUT-DISPLAY-C-REC.                                         P441402
00075      05  INPUT-MAX-COUNT        PIC 9(3).                         P441402
00076      05  FILLER                 PIC X(77).                        P441402
00077                                                                   P441402
00078 **       MASTER FILE:                                             P441402
00079  FD  KODAK-INDEX-DATA-FILE                                        P441402
00080      RECORD CONTAINS 71 CHARACTERS                                P441402
00081      LABEL RECORDS STANDARD                                       P441402
00082      DATA RECORD IS KODAK-INDEX-DATA-REC0RD.                      P441402
00083                                                                   P441402
00084  01  KODAK-INDEX-DATA-RECORD.                                     P441402
00085      05  KODAK-INDEX-DATA-TYPE   PIC X(3).                        P441402
00086      05  KODAK-INDEX-DATA-KEY.                                    P441402
00087          07  KODAK-INDEX-DOCNO   PIC X(17).                       P441402
00088          07  KODAK-INDEX-INV-YR  PIC 9(4).                        P441402
00089          07  KODAK-INDEX-INV-MO  PIC 99.                          P441402
00090          07  KODAK-INDEX-INV-DA  PIC 99.                          P441402
00091          07  KODAK-INDEX-DOC-TYP PIC X(5).                        P441402
00092          07  FILLER              PIC XX.                          P441402
00093      05  KODAK-INDEX-DATA-ROLL   PIC X(08).                       P441402
00094      05  KODAK-INDEX-DATA-FRAME  PIC X(08).                       P441402
00095      05  KODAK-INDEX-DATA-VIN    PIC X(18).                       P441402
00096      05  FILLER                  PIC X(02).                       P441402
00097                                                                   P441402
00098  FD  INPUT-SELECT-FILE                                            P441402
00099      RECORDING MODE IS F                                          P441402
00100      RECORD CONTAINS 80 CHARACTERS                                P441402
00101      BLOCK CONTAINS 0 CHARACTERS                                  P441402
00102      LABEL RECORDS STANDARD                                       P441402
00103      DATA RECORD IS INPUT-DOCNO-REC.                              P441402
00104                                                                   P441402
00105  01  INPUT-DOCNO-REC.                                             P441402
00106      05  INPUT-DOCNO             PIC X(17).                       P441402
00107      05  FILLER                  PIC X(63).                       P441402
00108                                                                   P441402
00109                                                                   P441402
00110 *************************                                         P441402
00111  WORKING-STORAGE SECTION.                                         P441402
00112 *************************                                         P441402
00113                                                                   P441402
00114  01  FILLER                                PIC X(40) VALUE        P441402
00115       'WORKING STORAGE FOR P441402 BEGINS HERE'.                  P441402
00116                                                                   P441402
00117 ******************************************************************P441402
00118 *                  TEMPORARY WORKING STORAGE                     *P441402
00119 ******************************************************************P441402
00120                                                                   P441402
00121  01  TEMPORARY-STORAGE-SWITCHES.                                  P441402
00122      05  DOCNO-SELECT-SW                    PIC X    VALUE 'Y'.   P441402
00123          88  MORE-DOCNO-RECORDS                      VALUE 'Y'.   P441402
00124          88  NO-MORE-DOCNO-RECORDS                   VALUE 'N'.   P441402
00125                                                                   P441402
00126  01  WS-SUBS-COUNTERS-ACCUMULATORS.                               P441402
00127      05  WS-INPUT-CNT                      PIC 9(05) VALUE ZEROES.P441402
00128      05  WS-CURR-DATA-CNT                  PIC 9(05) VALUE ZEROES.P441402
00129      05  THE-NEW-DATE-DT                   PIC 9(12) VALUE ZEROES.P441402
00130      05  FILLER REDEFINES THE-NEW-DATE-DT.                        P441402
00131          07  THE-CURR-DATE                 PIC 9(8).              P441402
00132          07  FILLER REDEFINES THE-CURR-DATE.                      P441402
00133              10  YYYY                      PIC 9(4).              P441402
00134              10  MM                        PIC 9(2).              P441402
00135              10  DD                        PIC 9(2).              P441402
00136          07  CURR-TIME                     PIC 9(4).              P441402
00137                                                                   P441402
00138      05  WS-CURRENT-DATE-DT.                                      P441402
00139          10  WS-MONTH                      PIC XX    VALUE ZEROS. P441402
00140          10  FILLER                        PIC X     VALUE '/'.   P441402
00141          10  WS-DAY                        PIC XX    VALUE ZEROS. P441402
00142          10  FILLER                        PIC X     VALUE '/'.   P441402
00143          10  WS-YEAR                       PIC X(4)  VALUE ZEROS. P441402
00144      05  KODAK-FILE-STATUS                 PIC 99    VALUE ZEROS. P441402
00145                                                                   P441402
00146  01  WS-HOLD-FIELDS.                                              P441402
00147      05  WS-HOLD-ROLL-FRAME.                                      P441402
00148          10  WS-HOLD-ROLL                  PIC X(8)  VALUE SPACES.P441402
00149          10  FILLER                        PIC X     VALUE '-'.   P441402
00150          10  WS-HOLD-FRAME                 PIC X(8)  VALUE SPACES.P441402
00151      05  WS-SHOW-FILM-DT.                                         P441402
00152          10  WS-SHOW-MO                    PIC 99    VALUE ZEROS. P441402
00153          10  FILLER                        PIC X     VALUE '/'.   P441402
00154          10  WS-SHOW-DA                    PIC 99    VALUE ZEROS. P441402
00155          10  FILLER                        PIC X     VALUE '/'.   P441402
00156          10  WS-SHOW-YR                    PIC 9999  VALUE ZEROS. P441402
00157      05  IDX                               PIC 99    VALUE ZERO.  P441402
00158      05  WS-STRT-DOCNO                     PIC X(17) VALUE SPACES.P441402
00159      05  FILLER REDEFINES WS-STRT-DOCNO.                          P441402
00160          10  WS-STRT-DOC-X                 PIC X  OCCURS 17 TIMES.P441402
00161      05  WS-END-DOCNO                      PIC X(17) VALUE SPACES.P441402
00162      05  FILLER REDEFINES WS-END-DOCNO.                           P441402
00163          10  WS-END-DOC-X                  PIC X  OCCURS 17 TIMES.P441402
00164                                                                   P441402
00165  01  FILLER                        PIC X(40) VALUE                P441402
00166       'WORKING STORAGE FOR P441402 ENDS HERE'.                    P441402
00167      EJECT                                                        P441402
00168                                                                   P441402
00169 ****************************                                      P441402
00170  PROCEDURE DIVISION.                                              P441402
00171 ****************************                                      P441402
00172                                                                   P441402
00173  0-MAINLINE-PROCESSING-P441402.                                   P441402
00174                                                                   P441402
00175      PERFORM 1-INITIALIZATION.                                    P441402
00176                                                                   P441402
00177      PERFORM 2-PROCESS-ACCTS                                      P441402
00178          UNTIL NO-MORE-DOCNO-RECORDS.                             P441402
00179                                                                   P441402
00180      PERFORM Z-4-TERMINATION.                                     P441402
00181                                                                   P441402
00182  1-INITIALIZATION.                                                P441402
00183 ******************************************************************P441402
00184 *             OPEN FILES AND SETUP DATES                         *P441402
00185 ******************************************************************P441402
00186                                                                   P441402
00187      MOVE FUNCTION CURRENT-DATE(1:12) TO THE-NEW-DATE-DT.         P441402
00188      MOVE MM                          TO WS-MONTH.                P441402
00189      MOVE DD                          TO WS-DAY.                  P441402
00190      MOVE YYYY                        TO WS-YEAR.                 P441402
00191                                                                   P441402
00192      DISPLAY '   P441402                          DATE: '         P441402
00193              WS-CURRENT-DATE-DT.                                  P441402
00194      DISPLAY ' *****  KODAK IMAGELINK PARTIAL DOCNO'              P441402
00195              ' SELECTION *****'.                                  P441402
00196      OPEN INPUT  INPUT-SELECT-FILE.                               P441402
00197      MOVE 'Y'                         TO DOCNO-SELECT-SW.         P441402
00198                                                                   P441402
00199      OPEN INPUT  KODAK-INDEX-DATA-FILE.                           P441402
00200      OPEN INPUT  INPUT-DISPLAY-C-FILE.                            P441402
00201      READ INPUT-DISPLAY-C-FILE                                    P441402
00202      AT END                                                       P441402
00203          MOVE ZEROS                  TO INPUT-MAX-COUNT.          P441402
00204      IF INPUT-MAX-COUNT = ZERO                                    P441402
00205          MOVE 250                    TO INPUT-MAX-COUNT.          P441402
00206                                                                   P441402
00207      PERFORM Z-1-READ-DOCNO-SELECTS.                              P441402
00208                                                                   P441402
00209 ******************************************************************P441402
00210  2-PROCESS-ACCTS.                                                 P441402
00211 ******************************************************************P441402
00212                                                                   P441402
00213      MOVE SPACES                     TO KODAK-INDEX-DATA-KEY.     P441402
00214      MOVE ZEROS                      TO KODAK-INDEX-INV-YR        P441402
00215                                         KODAK-INDEX-INV-MO        P441402
00216                                         KODAK-INDEX-INV-DA        P441402
00217      MOVE SPACES                     TO KODAK-INDEX-DOC-TYP.      P441402
00218      MOVE ZEROS                      TO WS-CURR-DATA-CNT.         P441402
00219      MOVE INPUT-DOCNO                TO WS-STRT-DOCNO.            P441402
00220      MOVE INPUT-DOCNO                TO WS-END-DOCNO.             P441402
00221      PERFORM 21-SET-DOCNOS                                        P441402
00222          VARYING IDX FROM 17 BY -1                                P441402
00223          UNTIL IDX = 1                                            P441402
00224      MOVE WS-STRT-DOCNO              TO KODAK-INDEX-DOCNO.        P441402
00225      ADD 1                           TO WS-INPUT-CNT.             P441402
00226                                                                   P441402
00227      START KODAK-INDEX-DATA-FILE                                  P441402
00228          KEY IS GREATER THAN OR EQUAL TO KODAK-INDEX-DATA-KEY.       CL**2
00229      IF KODAK-FILE-STATUS = ZEROES                                P441402
00230          PERFORM Z-2-READ-KODAK-INDEX                             P441402
00231      END-IF.                                                      P441402
00232                                                                   P441402
00233      IF KODAK-FILE-STATUS = ZEROES                                P441402
00234          PERFORM 22-CHECK-MATCH UNTIL                             P441402
00235          KODAK-FILE-STATUS > ZEROES                               P441402
00236      END-IF.                                                      P441402
00237                                                                   P441402
00238      PERFORM Z-1-READ-DOCNO-SELECTS.                              P441402
00239 ******************************************************************P441402
00240  21-SET-DOCNOS.                                                   P441402
00241 ******************************************************************P441402
00242                                                                   P441402
00243      IF WS-STRT-DOC-X(IDX) = SPACES                               P441402
00244          MOVE '0'                    TO WS-STRT-DOC-X(IDX)        P441402
00245      END-IF.                                                      P441402
00246      IF WS-END-DOC-X(IDX) = SPACES                                P441402
00247          MOVE '9'                    TO WS-END-DOC-X(IDX)         P441402
00248      END-IF.                                                      P441402
00249                                                                   P441402
00250 ******************************************************************P441402
00251  22-CHECK-MATCH.                                                  P441402
00252 ******************************************************************P441402
00253                                                                   P441402
00254      IF KODAK-FILE-STATUS    = ZEROES       AND                   P441402
00255         (KODAK-INDEX-DOCNO < WS-END-DOCNO OR                         CL**2
00256          KODAK-INDEX-DOCNO = WS-END-DOCNO)                           CL**2
00257          ADD 1                     TO WS-CURR-DATA-CNT            P441402
00258          SUBTRACT KODAK-INDEX-INV-YR FROM 9999                    P441402
00259              GIVING WS-SHOW-YR                                    P441402
00260          SUBTRACT  KODAK-INDEX-INV-MO FROM 99                     P441402
00261              GIVING WS-SHOW-MO                                    P441402
00262          SUBTRACT KODAK-INDEX-INV-DA  FROM 99                     P441402
00263              GIVING WS-SHOW-DA                                    P441402
00264          MOVE KODAK-INDEX-DATA-ROLL   TO WS-HOLD-ROLL             P441402
00265          MOVE KODAK-INDEX-DATA-FRAME  TO WS-HOLD-FRAME            P441402
00266          DISPLAY ' '  KODAK-INDEX-DOCNO                           P441402
00267                  '  ' KODAK-INDEX-DATA-VIN                        P441402
00268                  ' '  WS-HOLD-ROLL-FRAME                          P441402
00269                  '    ' WS-SHOW-FILM-DT                           P441402
00270                  '   ' KODAK-INDEX-DOC-TYP                        P441402
00271          PERFORM Z-2-READ-KODAK-INDEX                             P441402
00272      ELSE                                                         P441402
00273          MOVE 99                      TO KODAK-FILE-STATUS        P441402
00274      END-IF.                                                      P441402
00275      IF WS-CURR-DATA-CNT > INPUT-MAX-COUNT                        P441402
00276          MOVE 99                      TO KODAK-FILE-STATUS        P441402
00277          DISPLAY ' LIMIT > ' INPUT-MAX-COUNT                      P441402
00278                  ' MATCHES FOUND! PLEASE CHECK KCAACNT INPUT'     P441402
00279                  ' OR CALL ISD IF PROBLEMS.....THX!'              P441402
00280      END-IF.                                                      P441402
00281                                                                   P441402
00282 ******************************************************************P441402
00283 *              ALL Z MODULES FOLLOW BELOW                        *P441402
00284 ******************************************************************P441402
00285                                                                   P441402
00286 ******************************************************************P441402
00287 *  INPUT DOCNO REQUEST NUMBERS                                   *P441402
00288 ******************************************************************P441402
00289  Z-1-READ-DOCNO-SELECTS.                                          P441402
00290                                                                   P441402
00291      READ INPUT-SELECT-FILE                                       P441402
00292          AT END MOVE 'N'           TO DOCNO-SELECT-SW.            P441402
00293                                                                   P441402
00294      IF MORE-DOCNO-RECORDS                                        P441402
00295          DISPLAY '  '                                             P441402
00296          DISPLAY '   **  **  **  '                                P441402
00297          DISPLAY ' MATCHES FOR INPUT REQUEST =  ' INPUT-DOCNO     P441402
00298          DISPLAY '   '                                            P441402
00299          DISPLAY '  DOCUMENT NUMBER      VIN NUMBER          M'   P441402
00300                  'EDIA-ID         FILM DATE   DOC TYP'            P441402
00301      END-IF.                                                      P441402
00302 ******************************************************************P441402
00303 *  KODAK INDEX FILE READ.                                        *P441402
00304 ******************************************************************P441402
00305  Z-2-READ-KODAK-INDEX.                                            P441402
00306                                                                   P441402
00307      READ KODAK-INDEX-DATA-FILE NEXT                              P441402
00308          AT END MOVE 99            TO KODAK-FILE-STATUS.          P441402
00309                                                                   P441402
00310  Z-4-TERMINATION.                                                 P441402
00311 ******************************************************************P441402
00312 *  FINSH OUT REPORTING, CLOSE FILES, STOP RUN.                   *P441402
00313 ******************************************************************P441402
00314                                                                   P441402
00315      DISPLAY '  '                                                 P441402
00316      DISPLAY '  ** END OF PROGRAM  OUTPUT **'.                    P441402
00317      CLOSE       INPUT-SELECT-FILE.                               P441402
00318      CLOSE       KODAK-INDEX-DATA-FILE.                           P441402
00319      CLOSE       INPUT-DISPLAY-C-FILE.                            P441402
00320                                                                   P441402
00321      STOP RUN.                                                    P441402
00322                                                                   P441402
00323 ***********  END OF PROGRAM  **************                       P441402
