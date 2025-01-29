00001 ******************************************************************06/25/13
00002  IDENTIFICATION DIVISION.                                         P442288 
00003 ******************************************************************   LV024
00004                                                                   P442288 
00005  PROGRAM-ID.    P442288.                                          P442288 
00006  AUTHOR.        ROY MILLS AND WALLY LUKASZUK.                     P442288 
00007  DATE-WRITTEN.  AUGUST 2006.                                      P442288 
00008  INSTALLATION.  ( TXDOT D-19 ).                                   P442288 
00009 *********  A NEW EXPLORE VERSION  OF P442276 AFTER LEVEL 046 **** P442288 
00010 *   N O T E :                                                   * P442288 
00011 *   =========                                                   * P442288 
00012 *    AN EXECUTION PARM CAN TRIGGER:                             * P442288 
00013 * 1. BYPASS THURSDAY (ONLY) RUN - NOW THE NORM                  * P442288 
00014 * 2. SPECIFY INQUIRY DBID, FLAG FOR DIAGNOSTIC DISPLAYS,        * P442288 
00015 *    BYPASS GENERATION OF ADABAS UPDATES AND/OR IRP VEHICLE FILE* P442288 
00016 *    UPDATES.  DATE OVERRIDE CAN ALSO SPECIFY THE SPECIFIC      * P442288 
00017 *    RUN DATE DESIRED ON THE REPORTS.                           * P442288 
00018 *                                                               * P442288 
00019 *    THOSE FLAGS ARE SET BY PARAGRAPH  00-SET-TEST5-ENVIRONMENT * P442288 
00020 *    IF THE FIRST PARAMETER BYTE IS NUMERIC.                    * P442288 
00021 *           EX: EXEC PGM=P442288,PARM='2YN'                     * P442288 
00022 *                                                               * P442288 
00023 *                                                               * P442288 
00024 *   END  OF  NOTE.                                              * P442288 
00025 ***************************************************************** P442288 
00026 *    APPLICATION AREA : INTERNATIONAL REGISTRATION PLAN         * P442288 
00027 *        TXIRP - EXPLORE IRP SUPPLIED INTERFACE DATA            * P442288 
00028 *      RTS ADABSE TRANSACTIONS GENERATION PROGRAM.              * P442288 
00029 *                                                               * P442288 
00030 *      PROGRAM NUMBER - P442288                                 * P442288 
00031 *    1.  DESCRIPTION:                                           * P442288 
00032 *        THIS MODULE READS THE IRP BATCH CONTROL FILE AND       * P442288 
00033 *           THE  WRKRTS INTERFACE FILE IN ORDER TO CREATE       * P442288 
00034 *           THE FOLLOWING RTS ADABASE TRANSACTIONS:             * P442288 
00035 *            RTS_TRANS                                          * P442288 
00036 *            RTS_FUND_FUNC_TRNS                                 * P442288 
00037 *            RTS_TR_FDS_DETAIL                                  * P442288 
00038 *            RTS_INV_FUNC_TRANS                                 * P442288 
00039 *            RTS_TR_INV_DETAIL                                  * P442288 
00040 *            RTS_MV_FUNC_TRANS                                  * P442288 
00041 *        IT DELETES THE BATCH CONTROL FILE AND IRPRTS INTERFACE * P442288 
00042 *        FILE RECORDS AS IT PROCESSES THEM.                     * P442288 
00043 *                                                               * P442288 
00044 *        IT VALIDATES TITLE DATA IN RTS ABABAS FILES:           * P442288 
00045 *            RTS-TITLE                                          * P442288 
00046 *            RTS-INACT-TITLE                                    * P442288 
00047 *        IT CHECKS FOR REGIS DATA IN RTS ABABAS FILES:          * P442288 
00048 *            RTS-REGIS                                          * P442288 
00049 *            RTS-INACT-REGIS                                    * P442288 
00050 *                                                               * P442288 
00051 *        THIS MODULE ALSO UPDATES THE WKRTS- INTERFACE FILE WITH* P442288 
00052 *          THE LATEST DOCNO AND OWNER NAME WHENEVER THE ORIGINAL* P442288 
00053 *          DOCNO CANNOT BE LOCATED, BUT A UNIQUE RTS ADABAS REC-* P442288 
00054 *          ORD CAN BE LOCATED BY VIN.                           * P442288 
00055 *                                                               * P442288 
00056 *        NUMEROUS FIELDS FROM THE TITLE AND REGIS FILES ARE     * P442288 
00057 *        PLACED IN THE RTS TRANSACTIONS FILES:                  * P442288 
00058 *            RTS_MV_FUNC_TRANS                                  * P442288 
00059 *            RTS_TRANS                                          * P442288 
00060 *                                                               * P442288 
00061 *        NOTE:  WEDNESDAY RUN ONLY - THIS JOB SHOULD NOW BE RUN * P442288 
00062 *            ONLY ON WEDNESDAY, TO COINCIDE WITH REGINOAL OFFICE* P442288 
00063 *            IRP REPORTING TO THE COUNTIES.  THE 3 DAY TIME     * P442288 
00064 *            DELAY IS NOW DROPPED,  AND ALL 'PAID' ACCOUNTS WILL* P442288 
00065 *            BE PROCESSED BY THIS THURSDAY RUN.                 * P442288 
00066 *            WS-MIN-BATCH-AGE IS SET TO ZERO.                   * P442288 
00067 *            THE JOB IS NOW RUN BY THE JOBTRAC JOB SCHEDULER.   * P442288 
00068 *                                                               * P442288 
00069 *                                                               * P442288 
00070 *        SOME CONDITIONS PREVENT RTS FROM COMPLETELY PROCESSING * P442288 
00071 *        THE TRANSACTION.  THE "FUNDS" ARE, HOWEVER, PROCESSED  * P442288 
00072 *        CORRECTLY.  REGISTRATION ERROR MESSAGES WILL BE PRINTED* P442288 
00073 *        HERE AND ADDITIONAL ERROR MESSAGES WILL BE TRIGGERED   * P442288 
00074 *        IN THE NIGHTLY RTS BATCH UPDATE PROCESS.  MESSAGES WILL* P442288 
00075 *        BE ROUTED TO THE PROPER REGIONAL OFFICE, AS IDENTIFIED * P442288 
00076 *        BY THE ON-LINE PF10 ADDRESS SCREEN.                    * P442288 
00077 *                                                               * P442288 
00078 *        1.  IRP RECORD IS FOR A NON-TITLED (DUMMY) UNIT.  THIS * P442288 
00079 *            SHOULD NOT OCCUR ONCE RTS IS FULLY IMPLEMENTED AT  * P442288 
00080 *            THE COUNTY LEVEL, UNLESS AN OPERATOR MAKES AN ERROR* P442288 
00081 *            ENTERING THE DOCNO FOR A RECORD WHOSE TITLE HAS NOT* P442288 
00082 *            BEEN UPDATED ON THE RTS ADABAS FILES.  IN THESE    * P442288 
00083 *            CASES, THE REGISTRATION IS BEING PROCESSED BEFORE  * P442288 
00084 *            THE TITLE PROCESS HAS BEEN COMPLETED.  THE TITLE IS* P442288 
00085 *            APPLIED FOR BUT IS NOT YET ON THE ADABAS FILES.    * P442288 
00086 *        2.  THE RECORD (DOCNO) IS ALREADY CURRENTLY REGISTERED * P442288 
00087 *            WITH ANOTHER LICENSE NUMBER FOR THE LATEST PERIOD  * P442288 
00088 *            OF REGISTRATION.  THAT IS, THERE WOULD BE TWO REGIS- P442288 
00089 *            TRATIONS ACTIVE AT THE SAME TIME.  THIS HAPPENS    * P442288 
00090 *            WHEN A CURRENTLY REGISTERED UNIT IS REGISTERED UNDER P442288 
00091 *            IRP, AND THEN A REFUND IS GIVEN FOR THE CANCELLED  * P442288 
00092 *            TEXAS REGISTRATION.  THIS COORDINATION PROBLEM IS  * P442288 
00093 *            BEING INVESTIGATED BY RTS.  CURRENTLY THE ATTEMPTED* P442288 
00094 *            NEW UPDATE TRANSACTIONS WOULD BE REJECTED BY RTS.  * P442288 
00095 *                                                               * P442288 
00096 *        ONE CONDITION PREVENTS RTS FUNDS TRANSACTIONS FROM     * P442288 
00097 *        BEING GENERATED.  THIS FUNCTION IS USEFUL WHEN IT IS   * P442288 
00098 *        NECESSARY TO SYNCHRONIZE THE IRP / RTS REGISTRATIONS,  * P442288 
00099 *        WITHOUT ALSO SENDING COUNTY FUNDS INFORMATION.  SPECIAL* P442288 
00100 *        RUN(S) OF P442260 WILL GENERATE IRP/RTS INTERFACE      * P442288 
00101 *        RECORDS FOR ACTIVE IRP REGISTRATIONS WHICH ARE NOT     * P442288 
00102 *        UPDATED ON THE RTS ADABAS FILES.    SPECIAL BATCH      * P442288 
00103 *        CONTROL RECORDS WILL ALSO BE NECESSARY.                * P442288 
00104 *                                                               * P442288 
00105 *        1.  IF THE IRP BATCH NUMBER IS > 99000, THEN THE FUNDS * P442288 
00106 *            TRANSACTIONS ARE NOT GENERATED.  THE BATCH NUMBER  * P442288 
00107 *            IS NORMALLY ZERO, FOR A BATCH RECORD CREATED BY THE* P442288 
00108 *            ON-LINE FUNCTION OF MARKING THE SUPPLEMENT PAID.   * P442288 
00109 *                                                               * P442288 
00110 *    3.  SPECIAL CONSIDERATIONS:                                * P442288 
00111 *        A. THIS PROGRAM MUST BE COMPILED FOR COBOL-MVS USING   * P442288 
00112 *           "EXEC PNQCMCL,DBID=PRD2,DSNADA='D59.ADASYS.PRD2'"   * P442288 
00113 *           WITH THE ADABAS OVERRIDES AND THE FOLLOWING OPTIONS:* P442288 
00114 *           PARM.COBMVS='NODYNAM,XREF,LIST,MAP,DATA(24),          P442288 
00115 *                        TEST(NONE,SYM)'                          P442288 
00116 *        B. LINK EDIT CONTROL INCLUDE STATEMENTS:               * P442288 
00117 *           PARM.LKED='CALL,LIST,LET,XREF,AMODE(24),RMODE(24)'  * P442288 
00118 *           WITH SYSIN INPUT WITH THE FOLLOWING:                * P442288 
00119 *               INCLUDE ADALIB(ADAUSER)                         * P442288 
00120 *               INCLUDE ADALIB(RESPINT)                         * P442288 
00121 *               INCLUDE ADALIB(PRTRACE)                         * P442288 
00122 *               INCLUDE ADALIB(PRPABEND)                        * P442288 
00123 *               ENTRY P442288                                   * P442288 
00124 *               SETSSI UUUUYJJJ   >> USER ID AND JULIAN DATE << * P442288 
00125 *               NAME P442288(R)                                 * P442288 
00126 *        C. THE FOLLOWING STEP CONDITION OVERRIDES ARE ALSO     * P442288 
00127 *           SUGGESTED OPTIONS:                                  * P442288 
00128 *           COND.COBMVS=((8,LE,PAN1),(8,LE,ADASQL))             * P442288 
00129 *           COND.LKED=((0,NE,PAN1),(0,NE,ADASQL),(5,LT,COBMVS)) * P442288 
00130 *                                                               * P442288 
00131 *    4.  SUBROUTINES CALLED:                                    * P442288 
00132 *        THE FOLLOWING LANGUAGE ENVIRONMENT/MVS CALLED ROUTINES:* P442288 
00133 *        CEEDAYS  - GET CURRENT DATE IN LILIAN FORMAT           * P442288 
00134 *        CEEDATE  - GET JULIAN DATE FROM LILIAN FORMATTED DATE  * P442288 
00135 *        CEEDYWK  - GET DAY-OF-WEEK FROM LILIAN FORMATTED DATE  * P442288 
00136 *                                                               * P442288 
00137 *    5.  EXECUTION PARAMETERS:                                  * P442288 
00138 *                                                               * P442288 
00139 *    SPECIAL RESTART WILL USE CURRENT TIME TO RESET RTS-TRANSTIME P442288 
00140 *    FIRST BYTE RUN-TYPE 'N' FOR NORMAL, OR 'S' FOR SYNC RUN.   * P442288 
00141 *    FIRST BYTE RUN-TYPE 'M' FOR NORMAL MONDAY RUN NO INC DT.   *    CL*17
00142 *    SECOND AND THIRD BYTE FILLER 'XX' - THIS IS NOT CHECKED.   * P442288 
00143 *    THIRD BYTE WILL BE 'R' IF THIS RUN IS A "RESTART".         * P442288 
00144 *    ALL OTHER COMBINATIONS ARE IGNORED FOR RESTART CHECK.      * P442288 
00145 *           EX: EXEC PGM=P442288,PARM='NXXR'                    * P442288 
00146 *               EXEC PGM=P442288,PARM='SXXR'                    * P442288 
00147 *    TEST RUNS USE AN EXECUTION PARM TO TRIGGER INQUIRY AGAINST * P442288 
00148 *    RTS TST5 OR PRD2 FILES.  THESE FILES ARE SET BY PERFORMING * P442288 
00149 *    PARAGRAPH  00-SET-TEST5-ENVIRONMENT                        * P442288 
00150 *           EX: EXEC PGM=P442288,PARM='2YN'                     * P442288 
00151 *               THIS DOES INQUIRY AGAINST PRD2 FILES, AND       * P442288 
00152 *               GENERATES DISPLAY DIAGNOSTICS USED FOR DEBUGGING* P442288 
00153 *               PURPOSES.                                       * P442288 
00154 *    TEST RUNS CAN REQUEST BYPASSING (NEGATE) THE GENERATION OF * P442288 
00155 *    RTS TRANSACTIONS COMPLETELY, ALONG WITH UPDATING THE IRP   * P442288 
00156 *    VEHICLE FILE (WHEN NEW DOCNOS ARE BEING COORDINATED BETWEEN* P442288 
00157 *    RTS AND IRP).                                              * P442288 
00158 *           EX: EXEC PGM=P442288,PARM='23YN'                    * P442288 
00159 *    THE IRP FILE UPDATE CAN ALSO BE SELECTIVELY EXCLUDED       * P442288 
00160 *    (INDEPENDENT OF THE ADABAS TRANSACTION UPDATES), BY USING  * P442288 
00161 *    THIS BYPASS FLAG.                                          * P442288 
00162 *           EX: EXEC PGM=P442288,PARM='23YI'                    * P442288 
00163 *    SPECIAL CARE SHOULD ALWAYS BE USED WHEN TESTING, TO ENSURE * P442288 
00164 *    TST5 DIBID IS BEING USED FOR WRITTEN ADABASE TRANSACTIONS. * P442288 
00165 *    A PRELIMINARY TEST RUN SHOULD BE RUN WITH OVERRIDE PARM TO * P442288 
00166 *    BYPASS UPDATES.  AN EXAMPLE IS:                            * P442288 
00167 *           EX: EXEC PGM=P442288,PARM='2YN'                       P442288 
00168 *    VIEW THE DDCARD OUTPUT TO ENSURE THAT DBID=294 (TST5)      * P442288 
00169 *                                                               * P442288 
00170 *    TEST RUNS, OR PRODUCTION RUNS WHEN ABSOLUTELY NECESSARY,   * P442288 
00171 *    CAN CHANGE RUN DATES (THIS EFFECTS THE AMDATE PORTION OF   * P442288 
00172 *    THE TRANSACTION KEYS.  THE RUNDATE IS ONLY USED IF IT IS   * P442288 
00173 *    NUMERIC.  THE CALENDAR AND JULIAN DATES ARE BOTH REQUIRED. * P442288 
00174 *    THE FORMAT IS YYMMDD AND YYDDD                             * P442288 
00175 *           EX: EXEC PGM=P442288,PARM='2Y YYMMDDYYDDD'          * P442288 
00176 *           EX: EXEC PGM=P442288,PARM='2Y 94102994293'          * P442288 
00177 ***************************************************************** P442288 
00178       EJECT                                                       P442288 
00179                                                                   P442288 
00180 ***************************************************************** P442288 
00181 ***************************************************************** P442288 
00182 * ---------------MAINTENANCE HISTORY----------------------------* P442288 
00183 *                                                               * P442288 
00184 *  PAN   PROGRAMMER     MOD     SPECTRUM     MODIFICATION       * P442288 
00185 * LEVEL    NAME         DATE    REQUEST      DESCRIPTION        * P442288 
00186 * -----  ---------    --------  --------     ------------------ * P442288 
00187 *  001   R. MILLS   08/21/2006  52910010    EXPLORE (TXIRP)     * P442288 
00188 *                                           VERSION OF P442276 -* P442288 
00189 *                                           CREATE RTS INTERFACE* P442288 
00190 *  002   R. MILLS   09/13/2006  PRDSUP07    USE EXP ACT-TYPE    *    CL**2
00191 *                                           INSTEAD OF LEADING  *    CL**2
00192 *                                           ACCOUNT DIGIT       *    CL**2
00193 *  003   R. MILLS   10/18/2006  PRDSUP07    CORRECT FORESTRY ERR*    CL**3
00194 *                                           -NO LONGER INDICATED*    CL**3
00195 *                                           BY ACCOUNT NUMBER 9X*    CL**3
00196 *  004   R. MILLS   11/15/2006  PRDSUP07    ADD VOUCHER-NUM TO  *    CL**4
00197 *                                           TRANSMITTAL REPORT  *    CL**4
00198 *  005   R. MILLS   11/29/2006  PRDSUP07    PROCESS X AUDITS    *    CL**5
00199 *  006   R. MILLS   12/13/2006  PRDSUP07    DO NOT SEND FUNDS   *    CL**6
00200 *                                           TRANS $.00 /FORESTRY*    CL**6
00201 *  007   R. MILLS   01/03/2007  PRDSUP07    DO NOT SEND FUNDS   *    CL**7
00202 *                                           TRANS $.00 /SUPP 000*    CL**7
00203 *                                           RENTAL OR TOKEN TRLR*    CL**7
00204 *  008   R. MILLS   03/28/2007  PRDSUP07    CHECK UPDATE AGAINST*    CL**8
00205 *                                           PREV PLATE - PLATE  *    CL**8
00206 *                                           ALREADY IN PREV PLATE    CL**8
00207 *                                         + CHECK FOR NO PLATE  *    CL**8
00208 *                                           ASSIGNMENT IN IF REC*    CL**8
00209 *  009   R. MILLS   04/18/2007  PRDSUP07    CORRECT PREV PLATE  *    CL**9
00210 *  010   R. MILLS   04/19/2007  PRDSUP07    FIX REDUNDANT UPDT       CL*10
00211 *  011   R. MILLS   04/20/2007  PRDSUP07    FIX PREV PLT UPDT        CL*11
00212 *  012   R. MILLS   05/07/2007  PRDSUP07    AUDIT SUMMARY RECORD     CL*12
00213 *                                           WITH TEMPORARY BPYASS    CL*12
00214 *  013   R. MILLS   05/15/2007  PRDSUP07    AUDIT SUMMARY RECORD     CL*13
00215 *                                           WILL NOW PROCESS         CL*13
00216 *  014   R. MILLS   10/20/2008  52080025    ADD CNTYMOBILTYFEE       CL*14
00217 *                                         + CHECK FULL BILLS FOR     CL*14
00218 *                                           INVALID NEG FUNDS        CL*14
00219 *  015   R. MILLS   10/22/2008  52080025    CORRECT IRPFNDX - ERR    CL*15
00220 *  016   R. MILLS   02/03/2010  PRDSUP10    ACCEPT K CORRECTIONS:    CL*16
00221 *                                           SAME FORMAT AS AUDITS    CL*16
00222 *                                           NO VEH - NETTED SUPP     CL*16
00223 *  017   R. MILLS   03/24/2010  52100003    TXIRP FLEET CHANGES      CL*17
00224 *  018   R. MILLS   02/01/2011  52110008    TOKEN TRAILER WITH       CL*18
00225 *                                            ITMCD + REGCL = FLEET   CL*18
00226 *  019   R. MILLS   04/05/2011  PRDSUP11    FIX RE-UPDATE CHECK      CL*19
00227 *                                            ERROR FOR TT            CL*19
00228 *  020   R. MILLS   09/21/2011  PRDSUP12    REPL1 NEW ITMCD FOR      CL*20
00229 *                                            SEPT 2011 REGIS.        CL*20
00230 *  021   R. MILLS   09/23/2011  PRDSUP12    FIX2 ON REPL, REPL1      CL*21
00231 *  022   S. MEYER   11/08/2011  PRDSUP12    REMOVE ACCT IT CD REPL   CL*22
00232 *                                           COMMENT GO BACK IN 0-M   CL*22
00233 *                                           AND Z-Z-TERM. FIX IF A   CL*22
00234 *                                           ADD STOP RUN IN Z-Z-TE   CL*22
00235 *  023   R. MILLS   11/28/2012  PRDSUP13    DO NOT SET GWT=00000     CL*23
00236 *  024   S. MEYER   04/22/2013  52120009    CODE FOR FLEET PLUS      CL*24
00237 *                                           PLT ITMCD 'APPTKDBL'.    CL*24
00238 ***************************************************************** P442288 
00239 ***************************************************************** P442288 
00240       EJECT                                                       P442288 
00241                                                                      CL**2
00242                                                                      CL**2
00243                                                                      CL**2
00244                                                                      CL**2
00245 ***************************************************************** P442288 
00246 * ----- HISTORICALLY SIGNIFICANT MAINTENANCE TO OLD P442276-----* P442288 
00247 *                                                               * P442288 
00248 *  PAN   PROGRAMMER     MOD     SPECTRUM     MODIFICATION       * P442288 
00249 * LEVEL    NAME         DATE    REQUEST      DESCRIPTION        * P442288 
00250 * -----  ---------    --------  --------     ------------------ * P442288 
00251 *  001   W.LUKASZUK   10-02-91  52910010     5-IRP-0048         * P442288 
00252 *                                            INITIAL CODING     * P442288 
00253 *        R. MILLS     10-28-94               INTEGRATION TESTING* P442288 
00254 *                                            CHANGES AND MOVE TO* P442288 
00255 *                                            PRODUCTION.        * P442288 
00256 *  002   R.MILLS      11-04-91  52910010     5-IRP-0048  FIX THE* P442288 
00257 *                                            PRODUCTION PROBLEMS* P442288 
00258 *                                            @ IMPLEMENTATION.  * P442288 
00259 *                                            1.DOCNO WITH INDEXNO P442288 
00260 *                                            2.IRPLIC WITH LEAD-  P442288 
00261 *                                              ING '0' ON ERR RPT P442288 
00262 *                                            3.WGT INCR & FEE =$0 P442288 
00263 *                                            4.DL REGION NOT DA   P442288 
00264 *                                            5.DO NOT SEND DUMMYS P442288 
00265 *                                              TO RTS_MV_FUNC     P442288 
00266 *                                            6.DO NOT SEND TX WGT P442288 
00267 *                                              INCR IF FEES = $ 0 P442288 
00268 *  003   R.MILLS      01-27-95  52910010     5-IRP-0048  DROP   * P442288 
00269 *                                            CONVERTER GEAR UNITS P442288 
00270 *                                            AND MILEAGE CHANGES* P442288 
00271 *                                            FROM TRANSMITTAL   * P442288 
00272 *                                            DATA.              * P442288 
00273 *                                          + GET REGIONAL OFFICE* P442288 
00274 *                                            ID FROM IRP MSTR   * P442288 
00275 *                                            SUPPLEMENT RECORD. * P442288 
00276 *                                          + DROP 3 DAY DELAY AND P442288 
00277 *                                            RUN ONLY ON THURSDAY P442288 
00278 *                                          + DO NOT SEND FUNDS    P442288 
00279 *                                            DATA IF IT IS ZERO   P442288 
00280 *  004   R.MILLS      02-01-95  52910010     5-IRP-0048  CORRECT* P442288 
00281 *                                            REGIONAL OFFICE    * P442288 
00282 *                                            ISSUANCE CD & NO.  * P442288 
00283 *  005   R.MILLS      02-23-95  52910010     5-IRP-0048  CORRECT* P442288 
00284 *                                            PROBLEM WITH DELETE* P442288 
00285 *                                            SUPP.              * P442288 
00286 *  006   R.MILLS      06/30/95  52910011     5-SPC-0257 - RTS   * P442288 
00287 *                                            INTERFACE CHANGE TO* P442288 
00288 *                                            USE THE COMPTROLLER* P442288 
00289 *                                            COUNTY NUMBERS.    * P442288 
00290 *                                         +  5-PRDSUP95  ALLOW  * P442288 
00291 *                                            BYPASS THR ONLY RUN* P442288 
00292 *  007   R.MILLS      10/10/95  52950033    5-IRP50033 RTS MODS:* P442288 
00293 *                                           CAPTURE CHILD SAFETY* P442288 
00294 *                                           PORTION OF CNTY FEE;* P442288 
00295 *                                         + CREATE RTS 'INVREQ' * P442288 
00296 *                                           FOR EXCHANGED VEHLS * P442288 
00297 *                                           AN EXAMPLE WOULD BE:* P442288 
00298 *                                           CURRENTLY REGISTERED* P442288 
00299 *                                           COMBINATION PLATES  * P442288 
00300 *                                           OR A PREVIOUS IRP   * P442288 
00301 *                                           REGISTRATION BY AN  * P442288 
00302 *                                           INDEPENDENT TRUCKER * P442288 
00303 *                                           MOVING FROM ONE     * P442288 
00304 *                                           ACCOUNT TO ANOOTHER.* P442288 
00305 *  008   R.MILLS      10/11/95  --NONE--    5-PRDSUP96 FIX TRANS* P442288 
00306 *                                           ID BUG-CORRECT RGN  * P442288 
00307 *  009   R.MILLS      02/23/96  52960013     5-IRP60013 - CHANGE* P442288 
00308 *                                           TO HANDLE SPECIAL   * P442288 
00309 *                                           REGISTRATION SYNCHRO- P442288 
00310 *                                           NIZATION PROCESS.   * P442288 
00311                                                                   P442288 
00312 *  010   R.MILLS      04/03/96  52910010     5-IRP-0048 - RESET * P442288 
00313 *                                           INDICATORS AND FIELDS P442288 
00314 *                                           IN RTS-TRANS FILE AS* P442288 
00315 *                                           PER RTS REQUEST.    * P442288 
00316 *                                        +  SET CORRECT PLATE   * P442288 
00317 *                                           TYPE FOR 5 YR ACCTS.* P442288 
00318 *                                        +  DO NOT SHOW DUMMY   * P442288 
00319 *                                           RECORDS ON ERR RPT. * P442288 
00320 *                                        +  REPORT ERRORS IF 5YR* P442288 
00321 *                                           RENEWAL IS ATTEMPTED* P442288 
00322 *                                           FOR DOCNO THAT HAS  * P442288 
00323 *                                           NOTE: 5 YR RENEWALS * P442288 
00324 *                                           (YRS TO BILL<5) WILL* P442288 
00325 *                                           ONLY COME THROUGH IF* P442288 
00326 *                                           COUNTY FEES INCREASE* P442288 
00327 *  011   R.MILLS      10/02/96  52910010     5-IRP-0048 - SETUP * P442288 
00328 *                                           VERSION CONTROL AND * P442288 
00329 *                                           OTHER ELEMENTS AS   * P442288 
00330 *                                           PER RTS REQUEST.    * P442288 
00331 *  012   R.MILLS    03/12/1997  52970007     5-IRP70007 - ADD   * P442288 
00332 *                                           FORESTRY LICENSE    * P442288 
00333 *                                           PROCESSING.         * P442288 
00334 *                                           REMOVE ** FROM 617  * P442288 
00335 *                                           REMOVE ** FROM 626  * P442288 
00336 *                                           FIX 618-610 ERROR   * P442288 
00337 *                                           MESSAGE COMBINATION.* P442288 
00338 *  013   R.MILLS    04/01/1997  52960032     5-IRP60032 - COOR- * P442288 
00339 *                                           DINATE NEWLY TITLED * P442288 
00340 *                                           DOCNOS BETWEEN RTS  * P442288 
00341 *                                           AND IRP (UPDATE IRP * P442288 
00342 *                                           VEHICLE FILE WITH   * P442288 
00343 *                                           DOWNO & OWNER NAME).* P442288 
00344 *                                         + FIX CANCELLED REG   * P442288 
00345 *                                           ERROR- USE OLD EXPYR* P442288 
00346 *  014   R.MILLS    04/02/1997  52960032     5-IRP60032 - CORR- * P442288 
00347 *                                           ECT BUGS: UPDATE    * P442288 
00348 *                                           VEHL DOCNO, + CHECK * P442288 
00349 *                                           RTS TITLE DATE.     * P442288 
00350 *  015   R.MILLS    05/22/1997  --NONE--     5-PRDSUP97 - CORR- * P442288 
00351 *                                           ECT BUG THAT ALLOWED* P442288 
00352 *                                           AUDIT DATA THROUGH  * P442288 
00353 *                                           (IF NON-TITLED DATA)* P442288 
00354 *  016   R.MILLS    08/05/1997  52970026     5-IRP70026 - ADD   * P442288 
00355 *                                           RENTAL TRAILER PLATE* P442288 
00356 *                                           PROCESSING, WHICH IS* P442288 
00357 *                                           A NON-IRP ITEM BUT  * P442288 
00358 *                                           HANDLED THROUGH IRP.* P442288 
00359 *                                         + CORRECT REG CLASS   * P442288 
00360 *                                           CODES AND ITEM CODES* P442288 
00361 *                                           FOR NON-IRP DATA,   * P442288 
00362 *                                           ALSO CHANGE THE W-S * P442288 
00363 *                                           NAMES TO DISTINGUISH* P442288 
00364 *                                           BETWEEN INVENTORY   * P442288 
00365 *                                           AND ACCOUNTING ITEM * P442288 
00366 *                                           CODES.              * P442288 
00367 *  017   R.MILLS    08/13/1997  52970026     5-IRP70026 - FIX   * P442288 
00368 *                                           BUG FROM PREVIOUS   * P442288 
00369 *                                           UPDATE - DROPPED THE* P442288 
00370 *                                           TRANSCD FOR LICENSE * P442288 
00371 *                                           REPLACEMENT TRANS.  * P442288 
00372 *  018   K.GARRETT  12/30/1997  5996002     (PAC2 5-Y2KIRP)     * P442288 
00373 *                                           1.BRING CODE UPTO   * P442288 
00374 *                                             COBOL MVS STNDRDS * P442288 
00375 *                                           2.UPDATE HGDS & DTS * P442288 
00376 *                                             FOR Y2K STANDARDS * P442288 
00377 *                                           3.COMPILE FOR NEW   * P442288 
00378 *                                             Y2K BATCH AND RTS * P442288 
00379 *                                             I/F FILE COPYBKS  * P442288 
00380 *  019   K.GARRETT  01/26/1998  5-PRDSUP98  FIX MOVES FROM      * P442288 
00381 *                                           RTS-KEY TO VEHL-KEY * P442288 
00382 *                                           (NEEDS OLD DT FORMT)* P442288 
00383 *  020   R.MILLS    04/10/1998  5-PRDSUP98  TEMP FIX TO DROP THE* P442288 
00384 *                                           MV-FUNC WRITE WHEN  * P442288 
00385 *                                           RENEWAL IS FOR SUPP.* P442288 
00386 *                                           000 (RENEWALS) ON 5 * P442288 
00387 *                                           YR TRAILER ACCOUNTS.* P442288 
00388 *  021   R.MILLS  11/04/1998  52950025     5-IRP50025 - IRP     * P442288 
00389 *                                          STAGGERED REG CHANGES* P442288 
00390 *                           + 59960002     5-Y2KIRP - INCLUDING * P442288 
00391 *                                          CHANGES FOR Y2K      * P442288 
00392 *                                        + USE DYNAMIC CALL FOR * P442288 
00393 *                                          P446089              * P442288 
00394 *                                        + PREDICT DATA DIC.    * P442288 
00395 *                                          MOD FOR PRIVACYOPTCD * P442288 
00396 *  022   R.MILLS  02/02/1999  52950025     5-IRP50025 - PICKUP  * P442288 
00397 *                                          EXPIRATION DATE FROM * P442288 
00398 *                                          ACCOUNT RECORD.      * P442288 
00399 *  023   R.MILLS  02/17/1999  52950025     5-IRP50025 - CORRECT * P442288 
00400 *                                          ITEM CODE ERROR..ALL * P442288 
00401 *                                          WERE BEING SET APPRTR* P442288 
00402 *  024   R.MILLS  02/24/1999  52950025     5-IRP50025 - CORRECT * P442288 
00403 *                                          EXP DT FOR 5 YR ACCTS* P442288 
00404 *                                        + CORRECT TIRE TYPE CD * P442288 
00405 *  025   R.MILLS  02/24/1999  52950025     5-IRP50025 - CORRECT * P442288 
00406 *                                          BAD FIX FOR TIRE TYPE* P442288 
00407 *  026   R.MILLS  03/10/1999  52950025     5-IRP50025 - TESTING * P442288 
00408 *                                          TIRETYPECD PROBLEM   * P442288 
00409 *  027   R.MILLS  06/14/1999  52950025     5-IRP50025 - CORRECT * P442288 
00410 *                                          SPLIT BILLINGS ERROR * P442288 
00411 *                                          THAT IGNORED 2 EXP MO* P442288 
00412 *                                          FOR A SINGLE YEAR.   * P442288 
00413 *                                       +  NO IRPIVD  FOR LICN  * P442288 
00414 *                                          REPLACEMENTS ('L')   * P442288 
00415 *  028   R.MILLS  09/29/2000  52950025     5-IRP50025 - USE NEW * P442288 
00416 *                                          FIELDNAME FILETIERCD * P442288 
00417 *                                        + ALLOW REFUNDS TO CAN-* P442288 
00418 *                                          CEL REGISTRATION IF  * P442288 
00419 *                                          BATCH NUMBER IS 9999 * P442288 
00420 *                                          07/16/1999 ONE TIME ?* P442288 
00421 *                           + 5299042      5-VTR59042 - HB 3014 * P442288 
00422 *                                          REPORT AUTOMATION AND* P442288 
00423 *                                          REFLECTORIZATION FEES* P442288 
00424 *                                          SEPARATELY.08/02/2000* P442288 
00425 *  029   R.MILLS  10/11/2000  --NONE--     5-PRDSUP01 - CORRECT * P442288 
00426 *                                          EVEN EXCHANGE PROBLEM* P442288 
00427 *                                          WITH REFLECT AND AUTO* P442288 
00428 *                                          MATE FEES.           * P442288 
00429 *                                        + REMOVE UPDATE DBID   * P442288 
00430 *                                          OPTION.  USE DDCARD'S* P442288 
00431 *  030   R.MILLS  12/18/2000  52990041     5-VTR59041 - CHANGE    P442288 
00432 *                                          5-APPRTR TO REGCL 51*  P442288 
00433 *  031   R.MILLS  05/15/2001  52980007     5-VTR48007 - CHANGES * P442288 
00434 *                                          FOR RTS RELEASE 4.2  * P442288 
00435 *                                          USE VERSIONCD 05     * P442288 
00436 *                                          MOVE COMPTCNTYNO TO  * P442288 
00437 *                                          RTS_FUND_FUNC_TRNS   * P442288 
00438 *                                          04/18/2001 TESTING   * P442288 
00439 *  032   R.MILLS  08/16/2001  --NONE--     5-PRDSUP-1 - DO NOT  * P442288 
00440 *                                          USE RTS VIN ACQUIRED * P442288 
00441 *                                          RECORD IF OLD TTLDATE* P442288 
00442 *  033   R.MILLS  10/24/2001  52010047     5-VTR51047 - ITIMIZE * P442288 
00443 *                                          SB005 EMISSIONS AMT  * P442288 
00444 *                                          WITH REGISEMI        * P442288 
00445 *  034   R.MILLS  03/13/2002  52020005     5-VTR52005 - RTS UPD * P442288 
00446 *                                          5 YR RENTAL TRAILERS * P442288 
00447 *                                       +  CHNG IRPRPL TO IRPXCH* P442288 
00448 *                                       +  TWEEK UPD FROM VIN   * P442288 
00449 *                                       +  RTS-SYNC-REG-UPDATE-RUNP442288 
00450 *  035   R.MILLS  03/20/2002  52020005     5-VTR52005 - RTS UPD * P442288 
00451 *                                          5 YR RENTAL TRAILERS * P442288 
00452 *                                       +  CORRECT DT CHK ON VIN* P442288 
00453 *  036   R.MILLS  03/27/2002  52020005     5-VTR52005 - CORRECT * P442288 
00454 *                                          SKIP TITLE CAUSING   * P442288 
00455 *                                          ALL REC CALLUP BY VIN* P442288 
00456 *                                       +  NEW EXEC PARM USED TO* P442288 
00457 *                                          CONTROL SYNC FUNC    * P442288 
00458 *  037   R.MILLS  04/09/2002  52020005     5-VTR52005 -TEMPORARY* P442288 
00459 *                                          CHANGE RNT TO 5APPRTR* P442288 
00460 *                                        + CONTROL CHANGE R99...* P442288 
00461 *                                          FOR NO-FUNDS MVUPDATE* P442288 
00462 *  038   R.MILLS  09/23/2002  52020005     5-VTR52005 COORDINATE* P442288 
00463 *                                          DOCNO BETWEEN IRP&RTS* P442288 
00464 *                                        + CORRECT REGION ID IF * P442288 
00465 *                                          PAID FLAG IS NOT SET * P442288 
00466 *                                          BY THE REGION        * P442288 
00467 *  039   R.MILLS  09/26/2002  --NONE--     5-PRDSUP03 CORRECT   * P442288 
00468 *                                          PROG BUG DEALING WITH* P442288 
00469 *                                          INPUT (TEST) PARMS   * P442288 
00470 *  040   RLM      03/17/2003  5PRDSUP03    FIX SOC7/CUSTACTREGF * P442288 
00471 *  041   RLM      07/30/2003  52030019     5-VTR23019 - ADD NEW * P442288 
00472 *                                          INS VERIFICATION FEE * P442288 
00473 *                                        + CORRECT ACCESS TO RTS* P442288 
00474 *                                          TITLES AND REGIS     * P442288 
00475 *                                          ARCHIVE FILES.       * P442288 
00476 *                                        + CORRECT ADABAS NOUPDT* P442288 
00477 *                                          OVERRIDE-WAS WRITING * P442288 
00478 *                                          IRPIVD'S (REG CANCEL)* P442288 
00479 *                                        + REMOVE SAME VIN IVD  * P442288 
00480 *  042   RLM      01/03/2005  59040020   + TERPS CHANGE INCREASE* P442288 
00481 *                                          REGCLASSCD TO 3 DIGIT* P442288 
00482 *                                        + DATA DICTIONARY MOD  * P442288 
00483 *                                          REMOVE BRANCHOFCID   * P442288 
00484 *                                        + NEW MONDAY PROCESS TO* P442288 
00485 *                                          INCREMENT AMDATE     * P442288 
00486 *                                        + USE DEFAULT INQ DBID * P442288 
00487 *                                        + FIX SKIP OF AUDITS   * P442288 
00488 *                                        + DATA DICTIONARY TST5 * P442288 
00489 *                                        + RTPHVY REG CHANGES   * P442288 
00490 *                                        + MISCL CHANGES        * P442288 
00491 *  043   RLM      01/19/2005  5-PRDSUP05 + FIX ERR MSG 627      * P442288 
00492 *  044   RLM      05/04/2005  5-PRDSUP05   DO NOT INCR AMDATE   * P442288 
00493 *                                          FOR MONDAY SYNC RUN, * P442288 
00494 *                                        + CR IRPRENEW FOR OUT  * P442288 
00495 *                                          OF DATE REG BEFORE A * P442288 
00496 *                                          LICENSE EXCHANGE TRAN* P442288 
00497 *                                        + AT 1-INIT - CHECK FOR* P442288 
00498 *                                          RTS TRANSACTION STOP * P442288 
00499 *                                          BUT ALLOW TEST BYPASS* P442288 
00500 *  045   RLM      07/13/2005  52950028   + VERSIONCD 08 MODS    * P442288 
00501 *                                          INCLUDE PLTBIRTHDATE.* P442288 
00502 *  046   RLM      07/15/2005  52950028     CORRECT PLTBIRTHDATE * P442288 
00503 *                                          TO NOT EXCEED CURRENT* P442288 
00504 *                                          DATE                 * P442288 
00505 *  047   RLM      02/15/2005  52010022     ADJUST FOR EXPLORE   * P442288 
00506 *                                          DATA FORMAT AND LOSS * P442288 
00507 *                                          OF ACCESS TO MASTER  * P442288 
00508 *                                          AND VEHICLE VSAM RECS* P442288 
00509 *  048   RLM      08/11/2006  PRDSUP06     EXPLORE TXIRP VERSION* P442288 
00510 *                                        + FIX: NO SYNC FUNDS   * P442288 
00511 *                                          SENT ON RECOVERY IF  * P442288 
00512 *                                        + WINDOW FOR NEW TITLES* P442288 
00513 *                                        + SETUP FOR AUDIT FUNDS* P442288 
00514 *                                          PROCESSING - BYPASS  * P442288 
00515 *                                          UNTIL INTERFACE REC- * P442288 
00516 *                                          ORDS ARE FIXED==>NO  * P442288 
00517 *                                          - VALUE IN COLLECTED * P442288 
00518 *                                          AUDITS (ADJUST FOR   * P442288 
00519 *                                          REFUND SUPPLEMENTS). * P442288 
00520 ***************************************************************** P442288 
00521       EJECT                                                       P442288 
00522  ENVIRONMENT DIVISION.                                            P442288 
       CONFIGURATION SECTION.                                                   
      *SOURCE-COMPUTER. MAINFRAME.                                              
       SOURCE-COMPUTER. MAINFRAME WITH DEBUGGING MODE.                          
00523  INPUT-OUTPUT SECTION.                                            P442288 
00524  FILE-CONTROL.                                                    P442288 
00525                                                                   P442288 
00526      SELECT IRP-BATCH-FILE                                        P442288 
00527             ASSIGN TO IRPBATCH                                    P442288 
00528             ORGANIZATION IS INDEXED                               P442288 
00529             ACCESS MODE IS DYNAMIC                                P442288 
00530             RECORD KEY IS BTCH-KEY                                P442288 
00531             FILE STATUS IS BATCH-FILE-STATUS.                     P442288 
00532                                                                   P442288 
00533      SELECT  WORK-FD-INTERFACE-FILE                               P442288 
00534              ASSIGN  TO  IRPRTSIF                                 P442288 
00535              ORGANIZATION IS INDEXED                              P442288 
00536              ACCESS MODE IS DYNAMIC                               P442288 
00537              RECORD KEY IS  WORK-FD-KEY                           P442288 
00538              FILE STATUS IS WRKRTS-FILE-STATUS.                   P442288 
00539                                                                   P442288 
00540      SELECT INTERFACE-RESTART    ASSIGN TO RESTART                P442288 
00541                                  RELATIVE KEY IS RESTART-REC-NUM  P442288 
00542                                  ORGANIZATION IS RELATIVE         P442288 
00543                                  ACCESS MODE  IS RANDOM           P442288 
00544                                  FILE STATUS  IS RESTART-STATUS.  P442288 
00545                                                                   P442288 
00546      SELECT TRANSMITTAL-RPT-FILE         ASSIGN TO UT-S-RTSRPTDT. P442288 
00547      SELECT UPDATES-ARCHIVE-FILE         ASSIGN TO UT-S-RTSARCHV. P442288 
00548      SELECT PROCESS-CONTROL-REPORT       ASSIGN TO UT-S-IRP8801.  P442288 
00549      SELECT PROCESS-UPDATE-REPORT        ASSIGN TO UT-S-IRP8802.  P442288 
00550      SELECT PROCESS-MVUPDT-REPORT        ASSIGN TO UT-S-IRP8803.  P442288 
00551      SELECT PROCESS-MSGOUT-REPORT        ASSIGN TO UT-S-IRP8804.  P442288 
00552      EJECT                                                        P442288 
00553  DATA DIVISION.                                                   P442288 
00554 ***********************                                           P442288 
00555                                                                   P442288 
00556  FILE SECTION.                                                    P442288 
00557 ***************                                                   P442288 
00558                                                                   P442288 
00001 * D442209F: IRP INCLUDE COPY MEMBER       MAINTAINED BY UNIT 52   07/17/00
00002 ******************************************************************D442209F
00003 *                                                                *   LV004
00004 *  THIS IS THE FILE DESCRIPTION FOR THE BILLING AND TRANSMITTAL  *D442209F
00005 *    BATCH CONTROL FILE.  FILE KEY:  BILLING-RECAP FLAG          *D442209F
00006 *                                    BATCH NUMBER                *D442209F
00007 *                                    REGISTRATION YEAR           *D442209F
00008 *                                    ACCOUNT NUMBER              *D442209F
00009 *                                    FLEET NUMBER                *D442209F
00010 *                                    SUPPLEMENT NUMBER           *D442209F
00011 *                                    SUFFIX NUMBER               *D442209F
00012 *                                                                *D442209F
00013 ******************************************************************D442209F
00014                                                                   D442209F
00015  FD  IRP-BATCH-FILE                                               D442209F
00016      RECORD CONTAINS 58 CHARACTERS                                D442209F
00017      LABEL RECORDS ARE STANDARD                                   D442209F
00018      DATA RECORD IS IRP-BTCH-RECORD.                              D442209F
00019                                                                   D442209F
00020  01  IRP-BTCH-RECORD.                                             D442209F
00021      05  BTCH-KEY.                                                D442209F
00022          10  BTCH-BILLING-NUMBER.                                 D442209F
00023              15  BTCH-BILLING-RECAP-DEP    PIC X.                 D442209F
00024              15  BTCH-NUMBER               PIC X(5).              D442209F
00025          10  BTCH-REGISTRATION-DT          PIC 9(6).              D442209F
00026          10  FILLER REDEFINES BTCH-REGISTRATION-DT.               D442209F
00027              15  BTCH-REGIS-YYYY           PIC 9(4).              D442209F
00028              15  BTCH-REGIS-MM             PIC 9(2).              D442209F
00029          10  BTCH-ACCOUNT-NUMBER           PIC 9(6).              D442209F
00030          10  BTCH-FLEET-NBR                PIC X(2).              D442209F
00031          10  BTCH-SUPPLEMENT-NUMBER        PIC 9(3).              D442209F
00032          10  BTCH-SUFFIX-NUMBER            PIC X.                 D442209F
00033      05  BTCH-FILLER                       PIC X(34).             D442209F
00001 * D442288F: IRP INCLUDE COPY MEMBER       MAINTAINED BY UNIT 52   02/15/06
00002 ***************************************************************** D442288F
00003 *    WRKRTS - R T S   INTERFACE  FILE                                LV001
00004 *    THIS FILE CONTAINS THE TXIRP  INTERFACE DATA TO CREATE       D442288F
00005 *    R.T.S  TRANSACTIONS.                                         D442288F
00006 *                       ----- IRP-RTS FILE KEY -------            D442288F
00007 *                       1. SUPPLEMENT NUMBER                      D442288F
00008 *                       2. REGISTRATION YEAR                      D442288F
00009 *                       3. ACCOUNT NUMBER                         D442288F
00010 *                       4. FLEET NUMBER                           D442288F
00011 *                       5. SUFFIX NUMBER                          D442288F
00012 *                       6. EQUIP-NBR AND EQUIP-SEQ-NBR            D442288F
00013 *LAST UPDATE 02/15/2006                                           D442288F
00014 ***************************************************************** D442288F
00015                                                                   D442288F
00016  FD  WORK-FD-INTERFACE-FILE                                       D442288F
00017      RECORD CONTAINS 500 CHARACTERS                               D442288F
00018      LABEL RECORDS ARE STANDARD                                   D442288F
00019      DATA RECORD IS WORK-FD-RECORD.                               D442288F
00020                                                                   D442288F
00021  01  WORK-FD-RECORD.                                              D442288F
00022      05  WORK-FD-KEY.                                             D442288F
00023          10  WORK-FD-SUPP-NBR                PIC 9(3).            D442288F
00024          10  WORK-FD-REGIS-DT                PIC 9(6).            D442288F
00025          10  FILLER REDEFINES WORK-FD-REGIS-DT.                   D442288F
00026              15  WORK-FD-REG-YYYY            PIC 9(4).            D442288F
00027              15  WORK-FD-REG-MM              PIC 9(2).            D442288F
00028          10  WORK-FD-ACCOUNT-NBR             PIC 9(6).            D442288F
00029          10  WORK-FD-FLEET-NBR               PIC X(2).            D442288F
00030          10  WORK-FD-SUFFIX-NBR              PIC X(1).            D442288F
00031          10  WORK-FD-OWNER-EQUIP-NBR         PIC X(6).            D442288F
00032          10  WORK-FD-OWNER-EQUIP-SEQ-NBR     PIC 9(3).            D442288F
00033 * END OF KEY *                                                    D442288F
00034      05  FILLER                              PIC X(273).          D442288F
00035      05  FILLER                              PIC X(200).          D442288F
00559                                                                   P442288 
00560  FD  TRANSMITTAL-RPT-FILE                                         P442288 
00561      BLOCK CONTAINS 0 CHARACTERS                                  P442288 
00562      RECORD CONTAINS  85 CHARACTERS                                  CL**4
00563      LABEL RECORDS STANDARD                                       P442288 
00564      DATA RECORD IS TRANSMITTAL-RPT-DATA.                         P442288 
00565                                                                   P442288 
00566  01  TRANSMITTAL-RPT-DATA           PIC X(85).                       CL**4
00567                                                                   P442288 
00568  FD  UPDATES-ARCHIVE-FILE                                         P442288 
00569      BLOCK CONTAINS 0 CHARACTERS                                  P442288 
00570      RECORD CONTAINS 500 CHARACTERS                               P442288 
00571      LABEL RECORDS STANDARD                                       P442288 
00572      DATA RECORD IS WRKRTS-ARCHIVE-RECORD.                        P442288 
00573                                                                   P442288 
00574  01  WRKRTS-ARCHIVE-RECORD         PIC X(500).                    P442288 
00575                                                                   P442288 
00576  FD  PROCESS-CONTROL-REPORT                                       P442288 
00577      BLOCK CONTAINS 0 CHARACTERS                                  P442288 
00578      RECORD CONTAINS 133 CHARACTERS                               P442288 
00579      LABEL RECORDS STANDARD                                       P442288 
00580      DATA RECORD IS PROCESS-CONTROL-REP-REC.                      P442288 
00581                                                                   P442288 
00582  01  PROCESS-CONTROL-REP-REC       PIC X(133).                    P442288 
00583                                                                   P442288 
00584  FD  PROCESS-UPDATE-REPORT                                        P442288 
00585      BLOCK CONTAINS 0 CHARACTERS                                  P442288 
00586      RECORD CONTAINS 133 CHARACTERS                               P442288 
00587      LABEL RECORDS STANDARD                                       P442288 
00588      DATA RECORD IS PROCESS-UPDATE-REP-REC.                       P442288 
00589                                                                   P442288 
00590  01  PROCESS-UPDATE-REP-REC        PIC X(133).                    P442288 
00591                                                                   P442288 
00592  FD  PROCESS-MVUPDT-REPORT                                        P442288 
00593      BLOCK CONTAINS 0 CHARACTERS                                  P442288 
00594      RECORD CONTAINS 133 CHARACTERS                               P442288 
00595      LABEL RECORDS STANDARD                                       P442288 
00596      DATA RECORD IS PROCESS-MVUPDT-REP-REC.                       P442288 
00597                                                                   P442288 
00598  01  PROCESS-MVUPDT-REP-REC        PIC X(133).                    P442288 
00599                                                                   P442288 
00600  FD  PROCESS-MSGOUT-REPORT                                        P442288 
00601      BLOCK CONTAINS 0 CHARACTERS                                  P442288 
00602      RECORD CONTAINS 133 CHARACTERS                               P442288 
00603      LABEL RECORDS STANDARD                                       P442288 
00604      DATA RECORD IS PROCESS-MSGOUT-REP-REC.                       P442288 
00605                                                                   P442288 
00606  01  PROCESS-MSGOUT-REP-REC        PIC X(133).                    P442288 
00607                                                                   P442288 
00608  FD  INTERFACE-RESTART                                            P442288 
00609      DATA RECORD IS INTERFACE-RESTART-VSAM-REC.                   P442288 
00610                                                                   P442288 
00611  01  INTERFACE-RESTART-VSAM-REC       PIC X(40).                  P442288 
00612                                                                   P442288 
00613      EJECT                                                        P442288 
00614  WORKING-STORAGE SECTION.                                         P442288 
00615                                                                   P442288 
00616  01  FILLER.                                                      P442288 
00617      05  FILLER                    PIC X(8)  VALUE 'PROGRAM '.    P442288 
00618      05  WS-THIS-PGM               PIC X(8)  VALUE 'P442288'.     P442288 
00619      05  FILLER                    PIC X(28) VALUE                P442288 
00620       ' WORKING STORAGE BEGINS HERE'.                             P442288 
00621      05  FILLER                    PIC X(13) VALUE                P442288 
00622       ' COMPILED ON '.                                            P442288 
00623      05  WS-WHEN-COMPILED.                                        P442288 
00624              10  WS-COMP-DATE      PIC X(8)  VALUE ALL '*'.       P442288 
00625              10  WS-COMP-TIME      PIC X(8)  VALUE ALL '*'.       P442288 
00626      05  FILLER                    PIC X(7)  VALUE ALL '*'.       P442288 
00627                                                                   P442288 
00628  01  P442288-CALLING-LINKAGE-AREA.                                P442288 
00629      05  FILLER                            PIC S9(4) COMP         P442288 
00630                                                      VALUE ZEROS. P442288 
00631  01  C-CONSTANT.                                                  P442288 
00632      05  C-MARCH-EXPDT             PIC 9(8)  VALUE 99990331.      P442288 
00633 *---------------------------------------------------------------*    CL**5
00634 *    NOTE:  C-TRANS-CODE MUST BE IN SYNC WITH EXPLORE TABLE     *    CL**5
00635 *           SINCE IT IS OVERLAYED AT INIT                       *    CL**5
00636 *           PROBABLY SHOULD JUST CHANGE THESE QUICKLY           *    CL**5
00637 *---------------------------------------------------------------*    CL**5
00638      05  C-TRANS-CODE.                                            P442288 
00639          10  C-TRANS-IRPRPL        PIC X(06) VALUE 'IRPRPL'.      P442288 
00640          10  C-TRANS-IRPXCH        PIC X(06) VALUE 'IRPXCH'.      P442288 
00641          10  C-TRANS-IRPRNW        PIC X(06) VALUE 'IRPRNW'.      P442288 
00642          10  C-TRANS-IRPAW         PIC X(06) VALUE 'IRPAW '.      P442288 
00643          10  C-TRANS-IRPAUD        PIC X(06) VALUE 'IRPAUD'.      P442288 
00644          10  C-TRANS-IRPIVD        PIC X(06) VALUE 'IRPIVD'.      P442288 
00645      05  EXPLORE-TXIRP-TRANS-CODES.                               P442288 
00646          10  X-TRANS-IRPRPL        PIC X(06) VALUE 'XIRRPL'.      P442288 
00647          10  X-TRANS-IRPXCH        PIC X(06) VALUE 'XIRXCH'.      P442288 
00648          10  X-TRANS-IRPRNW        PIC X(06) VALUE 'XIRRNW'.      P442288 
00649          10  X-TRANS-IRPAW         PIC X(06) VALUE 'XIRAW '.      P442288 
00650          10  X-TRANS-IRPAUD        PIC X(06) VALUE 'XIRAUD'.      P442288 
00651          10  X-TRANS-IRPIVD        PIC X(06) VALUE 'XIRIVD'.      P442288 
00652      05  EXPLORE-FLEET-TRANS-CODES.                                  CL*17
00653          10  X-TRANS-XFLRPL        PIC X(06) VALUE 'XIRRPL'.         CL*17
00654          10  X-TRANS-XFLXCH        PIC X(06) VALUE 'XFLXCH'.         CL*17
00655          10  X-TRANS-XFLRNW        PIC X(06) VALUE 'XFLRNW'.         CL*17
00656          10  X-TRANS-XFLAW         PIC X(06) VALUE 'XFLAW '.         CL*17
00657          10  X-TRANS-XFLAUD        PIC X(06) VALUE 'XFLAUD'.         CL*17
00658          10  X-TRANS-XFLIVD        PIC X(06) VALUE 'XFLIVD'.         CL*17
00659          10  X-TRANS-XFLREM        PIC X(06) VALUE 'XFLREM'.         CL*17
00660          10  X-TRANS-XFLHVA        PIC X(06) VALUE 'XFLHVA'.         CL*17
00661          10  X-TRANS-XFLHVR        PIC X(06) VALUE 'XFLHVR'.         CL*17
00662      05  WS-TIRE-TYPE-CD-P         PIC X     VALUE  'P'.          P442288 
00663      05  C-OFC-ISSUANCE-NO-HQ      PIC 9(03) VALUE  296.          P442288 
00664      05  C-OFC-ISSUANCE-CODE-HQ    PIC 9(01) VALUE  1.            P442288 
00665      05  C-OFC-ISSUANCE-CODE-RGN   PIC 9(01) VALUE  2.            P442288 
00666 *        TRANSWSID CAN BE FROM 1 TO 999 TO MAKE TRANS KEY UNIQUE  P442288 
00667      05  C-TRANS-WS-ID             PIC 9(03) VALUE  996.          P442288 
00668 *---------------------------------------------------------------* P442288 
00669 *   PAIRINGS FOR INVENTORY ITMCD AND REGCLASSCD:                * P442288 
00670 *   REGCLASSCD IS IN THE RTS_MV_FUNC_TRANS RECORD               * P442288 
00671 *   ITMCD      IS IN THE RTS_TR_INV_DETAIL RECORD               * P442288 
00672 *                                                               * P442288 
00673 *   ITMCDS  - REGCLASSCD TABLE:                                 * P442288 
00674 *   APPRTK  - 06  --> APPORTIONED TRUCK                         * P442288 
00675 *   APPRTR  - 06  --> APPORTIONED TRAILER                       * P442288 
00676 *   5APPRTR - 51  --> FIVE YEAR APPORTIONED TRAILER             * P442288 
00677 *   5TTP    - 02  --> FIVE YEAR TOKEN TRAILER                   * P442288 
00678 *   RTP     - 01  --> RENTAL TRAILER (5 YR PLATE - 1 YEARS FEES)* P442288 
00679 *   FOREST  - 44  --> FORESTRY                                  * P442288 
00680 *   TTPP    - 113 --> MULTI YEAR TOKEN TRAILERS                 *    CL*18
00681 *---------------------------------------------------------------* P442288 
00682 *---------------------------------------------------------------* P442288 
00683 *   THESE -ITM- CODES ARE SET IN PARA. 21136-RTS-INV-TR-DETAIL  * P442288 
00684 *           ITMCD  IN  RTS_TR_INV_DETAIL ADABAS RECORD          * P442288 
00685 *---------------------------------------------------------------* P442288 
00686      05  C-TRANS-ITM-CD.                                          P442288 
00687          10  C-TRANS-ITM-APP-TRUCK  PIC X(08) VALUE  'APPRTK'.    P442288 
00688          10  C-TRANS-ITM-APP-TRLR   PIC X(08) VALUE  'APPRTR'.    P442288 
00689          10  C-TRANS-ITM-5-TKN-TRLR PIC X(08) VALUE  '5TTP  '.    P442288 
00690          10  C-TRANS-ITM-5-FLT-TRLR PIC X(08) VALUE  'TTPP  '.       CL*18
00691          10  C-TRANS-ITM-US         PIC X(08) VALUE  'US    '.       CL*18
00692          10  C-TRANS-ITM-5-RNT-TRLR    PIC X(08) VALUE  'RTP    '.P442288 
00693          10  C-TRANS-ITM-5-RTPHVY-TRLR PIC X(08) VALUE  'RTPHVY '.P442288 
00694          10  C-TRANS-ITM-5-APP-TRLR PIC X(08) VALUE  '5APPRTR'.   P442288 
00695          10  C-TRANS-ITM-FOREST     PIC X(08) VALUE  'FOREST'.    P442288 
00696 *---------------------------------------------------------------*    CL*17
00697 *     NOTE: IF IMP RECORD FROM WRKRTS-EXTFLEET-ACCT:            *    CL*17
00698 *           WRKRTS-ACT-TYPE  = 'E' THEN                         *    CL*17
00699 *           FLEET ITM CODE FROM FLD - WRKRTS-PLATE-ITMCD        *    CL*17
00700 *---------------------------------------------------------------*    CL*17
00701      05  C-REGCLASSCD-CODES.                                      P442288 
00702          10  C-REGCLASSCD-IRP       PIC 9(3)  VALUE 006.          P442288 
00703          10  C-REGCLASSCD-FOREST    PIC 9(3)  VALUE 044.          P442288 
00704          10  C-REGCLASSCD-5TT       PIC 9(3)  VALUE 002.          P442288 
00705          10  C-REGCLASSCD-5RT       PIC 9(3)  VALUE 001.          P442288 
00706          10  C-REGCLASSCD-5AT       PIC 9(3)  VALUE 051.          P442288 
00707          10  C-REGCLASSCD-5RTPHVY   PIC 9(2)  VALUE 66.           P442288 
00708 *---------------------------------------------------------------*    CL*17
00709 *     NOTE: IF IMP RECORD FROM WRKRTS-EXTFLEET-ACCT :           *    CL*17
00710 *           WRKRTS-ACT-TYPE  = 'E' THEN                         *    CL*17
00711 *           FLEET REGCLASSCD FROM FLD - WRKRTS-REG-CLASS-CD     *    CL*17
00712 *---------------------------------------------------------------*    CL*17
00713 *---------------------------------------------------------------* P442288 
00714 *            NO LONGER USE APPORTIONED TRAILERS                 * P442288 
00715 *        10  C-REGCLASSCD-5AT       PIC 9(2)  VALUE 51.         * P442288 
00716 *---------------------------------------------------------------* P442288 
00717 *---------------------------------------------------------------* P442288 
00718 *   THESE -ITEM- CODES ARE SET IN PARA. 21133-RTS-FUND-TR-DETAIL* P442288 
00719 *           ACCTITMCD IN RTS_TR_FDS_DETAIL ADABAS RECORD        * P442288 
00720 *---------------------------------------------------------------* P442288 
00721      05  C-ACCT-ITMCD-CD.                                         P442288 
00722          10  X-ACCT-ITMCD-FLEETFND PIC X(08) VALUE  'FLEETFND'.      CL*17
00723          10  C-ACCT-ITMCD-IRPFND   PIC X(08) VALUE  'IRPFNDS'.    P442288 
00724          10  C-ACCT-ITMCD-REPL1    PIC X(08) VALUE  'REPL1'.         CL*20
00725          10  C-ACCT-ITMCD-AW       PIC X(08) VALUE  'AW'.         P442288 
00726          10  C-ACCT-ITMCD-CRBF     PIC X(08) VALUE  'CRBF'.       P442288 
00727          10  C-ACCT-ITMCD-CSF      PIC X(08) VALUE  'CSF'.        P442288 
00728          10  C-ACCT-ITMCD-AUDIT    PIC X(08) VALUE  'IRPAUD'.     P442288 
00729          10  C-ACCT-ITMCD-REFLECT  PIC X(08) VALUE  'REFLECT'.    P442288 
00730          10  C-ACCT-ITMCD-AUTOMATE PIC X(08) VALUE  'AUTOMATE'.   P442288 
00731          10  C-ACCT-ITMCD-INS      PIC X(08) VALUE  'INS     '.   P442288 
00732          10  C-ACCT-ITMCD-SB005    PIC X(08) VALUE  'REGISEMI'.   P442288 
00733          10  C-ACCT-ITMCD-MOBILITY PIC X(08) VALUE  'CMF'.           CL*14
00734      05  C-TRANS-VERSIONCD         PIC 9(2)  VALUE  08.           P442288 
00735      05  C-TRANS-EMPTY-ZERO        PIC 9(4)  VALUE  ZEROS.        P442288 
00736      05  C-TRANS-QUANTITY-1        PIC 9(4)  VALUE  1.            P442288 
00737      05  C-TRANS-EMPTY-SPACES      PIC X(40) VALUE  SPACES.       P442288 
00738      05  C-SET-REG-INVALID-INDI    PIC X     VALUE  '1'.          P442288 
00739      05  C-RGN-OFC-TABLE.                                         P442288 
00740          10  FILLER                PIC X(06) VALUE  'AB 260'.     P442288 
00741          10  FILLER                PIC X(06) VALUE  'AM 261'.     P442288 
00742          10  FILLER                PIC X(06) VALUE  'AU 262'.     P442288 
00743          10  FILLER                PIC X(06) VALUE  'BT 263'.     P442288 
00744          10  FILLER                PIC X(06) VALUE  'CC 264'.     P442288 
00745          10  FILLER                PIC X(06) VALUE  'DL 265'.     P442288 
00746          10  FILLER                PIC X(06) VALUE  'EP 266'.     P442288 
00747          10  FILLER                PIC X(06) VALUE  'FA 267'.     P442288 
00748          10  FILLER                PIC X(06) VALUE  'HO 268'.     P442288 
00749          10  FILLER                PIC X(06) VALUE  'LV 269'.     P442288 
00750          10  FILLER                PIC X(06) VALUE  'LB 270'.     P442288 
00751          10  FILLER                PIC X(06) VALUE  'MO 271'.     P442288 
00752          10  FILLER                PIC X(06) VALUE  'PH 272'.     P442288 
00753          10  FILLER                PIC X(06) VALUE  'AN 273'.     P442288 
00754          10  FILLER                PIC X(06) VALUE  'SA 274'.     P442288 
00755          10  FILLER                PIC X(06) VALUE  'WA 275'.     P442288 
00756          10  FILLER                PIC X(06) VALUE  'WF 276'.     P442288 
00757          10  FILLER                PIC X(06) VALUE  'HQ 296'.     P442288 
00758          10  FILLER                PIC X(06) VALUE  'XO 297'.     P442288 
00759          10  FILLER                PIC X(06) VALUE  'ZZ 000'.     P442288 
00760      05  FILLER REDEFINES C-RGN-OFC-TABLE OCCURS 20 TIMES.        P442288 
00761          10  C-TRANS-OFC-RGN-ID    PIC X(02).                     P442288 
00762          10  FILLER                PIC X(01).                     P442288 
00763          10  C-TRANS-OFC-RGN-NO    PIC 9(03).                     P442288 
00764      05  C-RGN-OFC-TABLE-NDX-MAX   PIC 9(2) COMP VALUE 18.        P442288 
00765      05  C-CHECK-REPL-FULL-FEE-6   PIC S9(5)V99 VALUE +6.00.         CL*21
00766                                                                   P442288 
00767  01  FILES-STATUS.                                                P442288 
00768      05  BATCH-FILE-STATUS         PIC 99.                        P442288 
00769          88  BATCH-OPEN-OK               VALUE 0, 97.             P442288 
00770          88  BATCH-OK                    VALUE 0.                 P442288 
00771          88  NO-BATCH-REC                VALUE 23.                P442288 
00772          88  IRPBATCH-HELD-ELSEWHERE     VALUE 93.                P442288 
00773      05  WRKRTS-FILE-STATUS        PIC 99.                        P442288 
00774          88  WRKRTS-OPEN-OK              VALUE 0, 97.             P442288 
00775          88  WRKRTS-OK                   VALUE 0.                 P442288 
00776          88  WRKRTS-END-OF-FILE          VALUE 10.                P442288 
00777          88  NO-WRKRTS-REC               VALUE 23.                P442288 
00778          88  IRPRTSIF-HELD-ELSEWHERE     VALUE 93.                P442288 
00779                                                                   P442288 
00780  01  RESTART-DIAG-CONTROL.                                        P442288 
00781      05  RESTART-REC-NUM         PIC 99      VALUE ZERO.          P442288 
00782      05  RESTART-STATUS          PIC 99      VALUE ZERO.          P442288 
00783          88  RESTART-OPEN-OK             VALUE 0, 97.             P442288 
00784          88  RESTART-OK                  VALUE 0.                 P442288 
00785          88  RESTART-END-OF-FILE         VALUE 10.                P442288 
00786          88  NO-RESTART-REC              VALUE 23.                P442288 
00787          88  RESTART-HELD-ELSEWHERE      VALUE 93.                P442288 
00788                                                                   P442288 
00789      05  RESTART-DIAG-READ-ERROR PIC X       VALUE SPACES.        P442288 
00790      05  RESTART-DIAG-OPEN-INDI  PIC X       VALUE 'C'.           P442288 
00791                                                                   P442288 
00792  01  INITIAL-IMAGE.                                               P442288 
00793      05  INITIAL-OFCISSUANCENO      PIC 9(3) VALUE 260.           P442288 
00794      05  INITIAL-TRANSWSID          PIC 9(3) VALUE 996.           P442288 
00795      05  INITIAL-TRANSAMDATE        PIC 9(5) VALUE ZEROS.         P442288 
00796      05  INITIAL-TIME               PIC 9(6) VALUE ZEROS.         P442288 
00797      05  FILLER                     PIC X(23).                    P442288 
00798                                                                   P442288 
00799  01  RESTART-IMAGE.                                               P442288 
00800      05  RESTART-OFCISSUANCENO   PIC 9(3)    VALUE 260.           P442288 
00801      05  RESTART-TRANSWSID       PIC 9(3)    VALUE 996.           P442288 
00802      05  RESTART-TRANSAMDATE     PIC X(5).                        P442288 
00803      05  RESTART-UNIQUE-SEQ      PIC 9(6).                        P442288 
00804      05  FILLER                  PIC X(23).                       P442288 
00805                                                                   P442288 
00806                                                                   P442288 
00001 * D442209W: IRP INCLUDE COPY MEMBER       MAINTAINED BY UNIT 52   12/03/01
00002 ***************************************************************** D442209W
00003 *                                                                    LV009
00004 *                        I.R.P. BATCH FILE                        D442209W
00005 *  THIS FILE CONTAINS THE BILLING AND TRANSMITTAL BATCH RECORDS.  D442209W
00006 *    KEY :   1.  BATCH NUMBER        2.  ACCOUNT NUMBER           D442209W
00007 *            3.  FLEET  NUMBER       4.  SUFFIX NUMBER            D442209W
00008 *            5.  SUPPLEMENT NUMBER   6.  REGISTRATION YEAR        D442209W
00009 *               *                            *                    D442209W
00010 *               *    I.R.P. BATCH RECORD     *                    D442209W
00011 *               *        CHARACTERS = 58     *                    D442209W
00012 *                                                                 D442209W
00013 ***************************************************************** D442209W
00014                                                                   D442209W
00015  01  IRP-BATCH-REC.                                               D442209W
00016      05  BATCH-KEY.                                               D442209W
00017          10  BATCH-BILLING-NUMBER.                                D442209W
00018              15  BATCH-BILLING-RECAP-DEP   PIC X    VALUE SPACE.  D442209W
00019                  88  BATCH-RECAP-DEPOSIT            VALUE 'D'.    D442209W
00020                  88  BATCH-BILLING                  VALUE 'B'.    D442209W
00021                  88  BATCH-RTS-INTERFACE            VALUE 'R'.    D442209W
00022              15  BATCH-NUMBER              PIC 9(5) VALUE ZERO.   D442209W
00023              15  FILLER REDEFINES BATCH-NUMBER.                   D442209W
00024                  20  BATCH-NUMBER-X JUSTIFIED RIGHT  PIC X(5).    D442209W
00025              15  FILLER REDEFINES BATCH-NUMBER.                   D442209W
00026                  20  BATCH-BILL-FWD        PIC X(3).              D442209W
00027                  20  BATCH-BILL-FWD-REGION PIC X(2).              D442209W
00028              15  FILLER REDEFINES BATCH-NUMBER.                   D442209W
00029                  20  BATCH-CFWD-REGION     PIC X(2).              D442209W
00030                  20  FILLER                PIC X(3).              D442209W
00031          10  BATCH-REGIS-DT                PIC 9(6) VALUE ZERO.   D442209W
00032          10  FILLER REDEFINES BATCH-REGIS-DT.                     D442209W
00033              15  BATCH-REGIS-YYYY          PIC 9(4).              D442209W
00034              15  BATCH-REGIS-MM            PIC 9(2).              D442209W
00035          10  BATCH-ACCOUNT-NUMBER          PIC 9(6) VALUE ZEROES. D442209W
00036          10  BATCH-FLEET-NBR               PIC X(2) VALUE SPACE.  D442209W
00037          10  BATCH-SUPPLEMENT-NUMBER       PIC 9(3) VALUE ZERO.   D442209W
00038          10  BATCH-SUFFIX-NUMBER           PIC X    VALUE SPACES. D442209W
00039      05  BATCH-OWNER-EQUIP-NBR             PIC X(6) VALUE SPACES. D442209W
00040      05  BATCH-OWNER-EQUIP-SEQ-NBR         PIC X(3) VALUE SPACES. D442209W
00041      05  BATCH-NUM-SUPP-STATES             PIC 9(2) VALUE ZEROS.  D442209W
00042      05  FILLER                            PIC X    VALUE SPACES. D442209W
00043      05  BATCH-ADDED-DATE                  PIC 9(8) VALUE ZEROES. D442209W
00044      05  BATCH-LAST-CHG-DATE REDEFINES BATCH-ADDED-DATE.          D442209W
00045          07  FILLER                        PIC 9(8).              D442209W
00046      05  BATCH-LAST-CHG-TERM               PIC X(6) VALUE SPACES. D442209W
00047      05  FILLER REDEFINES BATCH-LAST-CHG-TERM.                    D442209W
00048          10  BATCH-CHANGE-TERMINAL         PIC X(4).              D442209W
00049          10  BATCH-CHANGE-REGION           PIC X(2).              D442209W
00050      05  BATCH-LAST-CHG-USER               PIC X(8) VALUE SPACES. D442209W
00001 *----------------------------------------------------------------*07/06/11
00002 *PAN MEMBER D442288W - IRP EXPLORE - MAINTAINED BY UNIT 52       *D442288W
00003 *----------------------------------------------------------------*   LV007
00004 *                                                                *D442288W
00005 *                  I.R.P. -  WRK INTERFACE RECORD                *D442288W
00006 *              CONTAINS THE I.R.P.  INFORMATION USED TO          *D442288W
00007 *    CREATE RTS TRANSACTIONS.                                    *D442288W
00008 *                       ----  IRP-WRK  INTERFACE  KEY -------    *D442288W
00009 *                       1. SUPPLEMENT NUMBER                     *D442288W
00010 *                       2. REGISTRATION YEAR                     *D442288W
00011 *                       3. ACCOUNT NUMBER                        *D442288W
00012 *                       3. FLEET NUMBER                          *D442288W
00013 *                       4. SUFFIX NUMBER                         *D442288W
00014 *                       5. OWNER-EQUIP-NBR & OWNER-EQUIP-SEQ-NBR *D442288W
00015 *       CHANGE 08/07/2008                                        *D442288W
00016 **      CHANGE 03/22/2010 - FLEET REGIS ADDITIONAL FIELDS - VER 3*D442288W
00017 ** LAST CHANGE 07/06/2011 - 8 BYTE EQUIP NUMBERS - NO SEQ NUM    *D442288W
00018 ******************************************************************D442288W
00019                                                                   D442288W
00020  01  WRKRTS-INTERFACE-RECORD.                                     D442288W
00021      05  WRKRTS-KEY.                                              D442288W
00022          10  WRKRTS-SUPP-NBR               PIC 9(3)  VALUE ZEROS. D442288W
00023          10  WRKRTS-REGIS-DT               PIC 9(6)  VALUE ZEROS. D442288W
00024          10  FILLER REDEFINES WRKRTS-REGIS-DT.                    D442288W
00025              15  WRKRTS-REGIS-YYYY         PIC 9(4).              D442288W
00026              15  WRKRTS-REGIS-MM           PIC 9(2).              D442288W
00027          10  WRKRTS-ACCOUNT-NBR            PIC 9(6)  VALUE ZEROS. D442288W
00028          10  WRKRTS-FLEET-NBR              PIC X(2)  VALUE SPACES.D442288W
00029          10  WRKRTS-SUFFIX-NBR             PIC X(1)  VALUE SPACES.D442288W
00030              88  PULLER-SUFFIX                       VALUE 'P'.   D442288W
00031              88  TRUCK-SUFFIX                        VALUE 'K'.   D442288W
00032              88  TRAILER-SUFFIX                      VALUE 'T'.   D442288W
00033              88  UNKNOWN-SUFFIX                      VALUE 'U'.   D442288W
00034          10  WRKRTS-OWNER-EQUIP-NBR        PIC X(8)  VALUE SPACES.D442288W
00035          10  WRKRTS-TYPE-OF-CHANGE-FLAG    PIC X     VALUE SPACES.D442288W
00036              88  WRKRTS-WEIGHT-CHANGE-VEHL           VALUE 'W'.   D442288W
00037              88  WRKRTS-ADD-VEHL-FULL-BILL           VALUE 'F'.   D442288W
00038              88  WRKRTS-ADD-VEHL-BILL-TEXAS          VALUE 'T'.   D442288W
00039              88  WRKRTS-EVEN-EXCHANGE-VEHL           VALUE 'E'.   D442288W
00040              88  WRKRTS-DELETE-VEHICLE-VEHL          VALUE 'D'.   D442288W
00041              88  WRKRTS-CABCARD-CHANGE-REQ           VALUE 'C'.   D442288W
00042              88  WRKRTS-REFUND-VEHL-TEXAS-FEES       VALUE 'R'.   D442288W
00043              88  WRKRTS-OUTSVC-VEHL                  VALUE 'O'.   D442288W
00044              88  WRKRTS-LICENSE-REPLACEMENT          VALUE 'L'.   D442288W
00045              88  WRKRTS-AUDIT-SUPPLEMENT-VEHL        VALUE 'A'.   D442288W
00046              88  WRKRTS-AUDIT-SUMMARY-EXTENSION      VALUE 'X'.   D442288W
00047              88  WRKRTS-MILEAGE-SUPPLEMENT-VEHL      VALUE 'M'.   D442288W
00048              88  WRKRTS-UNKNOWN-CHANGE-REQUEST       VALUE 'U'.   D442288W
00049 * END OF KEY *                                                    D442288W
00050      05  WRKRTS-LICENSE-KEY-INV-EXPYR.                            D442288W
00051          10  WRKRTS-CURRENT-TEXAS-LIC-NBR  PIC X(7)  VALUE SPACES.D442288W
00052          10  WRKRTS-INV-REGIS-EXP-YR       PIC 9(4)  VALUE ZEROS. D442288W
00053      05  WRKRTS-VEHICLE-YEAR               PIC 9(4)  VALUE ZEROS. D442288W
00054      05  WRKRTS-MAKE-OF-VEHICLE            PIC X(10) VALUE SPACES.D442288W
00055      05  WRKRTS-VEHICLE-TYPE               PIC X(02) VALUE SPACES.D442288W
00056          88  TRUCK-TRAILER                          VALUE 'TT'.   D442288W
00057          88  SINGLE-TRUCK                           VALUE 'TK'.   D442288W
00058          88  TRAILER                                VALUE 'TL'.   D442288W
00059          88  SEMI-TRAILER                           VALUE 'ST'.   D442288W
00060          88  RENTAL-TRAILER                         VALUE 'RT'.   D442288W
00061          88  CONVERTER-GEAR                         VALUE 'CG'.   D442288W
00062          88  BUS                                    VALUE 'BS'.   D442288W
00063          88  FULL-TRAILER                           VALUE 'FT'.   D442288W
00064      05  WRKRTS-IDENTIFICATION-NBR         PIC X(22) VALUE SPACES.D442288W
00065      05  WRKRTS-REGION                     PIC X(2)  VALUE SPACES.D442288W
00066      05  WRKRTS-REGION-NUMBER              PIC 9(3)  VALUE ZEROES.D442288W
00067      05  WRKRTS-COUNTY                     PIC 9(3)  VALUE ZEROS. D442288W
00068      05  WRKRTS-NAME-OF-APPLICANT          PIC X(38) VALUE SPACES.D442288W
00069      05  WRKRTS-OWNER-OPERATOR             PIC X(30) VALUE SPACES.D442288W
00070 *---------------------------------------------------------------* D442288W
00071      05  WRKRTS-DOCNO.                                            D442288W
00072          10  WRKRTS-TEXAS-ORIGIN           PIC X(2)  VALUE SPACES.D442288W
00073          10  WRKRTS-TEXAS-TITLE-INDEX      PIC X(7)  VALUE ZEROS. D442288W
00074          10  WRKRTS-TEXAS-TITLE-NUMBER     PIC X(8)  VALUE SPACES.D442288W
00075      05  FILLER REDEFINES WRKRTS-DOCNO.                           D442288W
00076          10  WRKRTS-DOCNO-OFFICE           PIC 9(3).              D442288W
00077          10  WRKRTS-DOCNO-PC-NUM           PIC 9(3).              D442288W
00078          10  WRKRTS-DOCNO-AMDATE           PIC 9(5).              D442288W
00079          10  WRKRTS-DOCNO-AMTIME           PIC 9(6).              D442288W
00080      05  WRKRTS-DOCNO-NUM REDEFINES WRKRTS-DOCNO PIC 9(17).       D442288W
00081      05  WRKRTS-LAST-CHG-DT.                                      D442288W
00082          10  WRKRTS-LAST-CHG-YR            PIC 9(4)  VALUE ZEROS. D442288W
00083          10  WRKRTS-LAST-CHG-MO            PIC 9(2)  VALUE ZEROS. D442288W
00084          10  WRKRTS-LAST-CHG-DA            PIC 9(2)  VALUE ZEROS. D442288W
00085      05  WRKRTS-EXCHANGED-DOCNO            PIC X(17) VALUE SPACES.D442288W
00086      05  WRKRTS-EXCHANGED-LICNO            PIC X(07) VALUE SPACES.D442288W
00087      05  WRKRTS-GROSS-WEIGHT               PIC 9(7)  VALUE ZERO.  D442288W
00088      05  WRKRTS-FUEL                       PIC X     VALUE SPACE. D442288W
00089          88  EXPLIC-DIESEL                           VALUE 'D'.   D442288W
00090          88  EXPLIC-GASOLINE                         VALUE 'G'.   D442288W
00091          88  EXPLIC-PROPANE                          VALUE 'P'.   D442288W
00092      05  WRKRTS-BEGINNING-REG-DT           PIC 9(6)  VALUE ZEROS. D442288W
00093      05  FILLER REDEFINES WRKRTS-BEGINNING-REG-DT.                D442288W
00094          10  WRKRTS-BEGINNING-REG-YYYY     PIC 9(4).              D442288W
00095          10  WRKRTS-BEGINNING-REG-MM       PIC 9(2).              D442288W
00096      05  WRKRTS-REG-EXP-DT                 PIC 9(6) VALUE ZEROS.  D442288W
00097      05  FILLER REDEFINES WRKRTS-REG-EXP-DT.                      D442288W
00098          10  WRKRTS-REG-EXP-DT-YYYY        PIC 9(4).              D442288W
00099          10  WRKRTS-REG-EXP-DT-MM          PIC 9(2).              D442288W
00100 *---------------------------------------------------------------* D442288W
00101 *    05  WRKRTS-APPORTIONED-FEE            PIC S9(7)V99 COMP-3  * D442288W
00102      05  WRKRTS-APPORTIONED-FEE            PIC S9(7)V99           D442288W
00103                                                      VALUE ZEROS. D442288W
00104 *---------------------------------------------------------------* D442288W
00105 *    05  WRKRTS-INS-PORTION                PIC S9(3)V99 COMP-3  * D442288W
00106      05  WRKRTS-INS-PORTION                PIC S9(3)V99           D442288W
00107                                                      VALUE ZEROS. D442288W
00108 *---------------------------------------------------------------* D442288W
00109 *    05  WRKRTS-TERP-PORTION               PIC S9(5)V99 COMP-3  * D442288W
00110      05  WRKRTS-TERP-PORTION               PIC S9(5)V99           D442288W
00111                                                      VALUE ZEROS. D442288W
00112 *---------------------------------------------------------------* D442288W
00113 *    05  WRKRTS-REFLECT-PORTION            PIC S9(3)V99 COMP-3  * D442288W
00114      05  WRKRTS-REFLECT-PORTION            PIC S9(3)V99           D442288W
00115                                                      VALUE ZEROS. D442288W
00116 *---------------------------------------------------------------* D442288W
00117 *    05  WRKRTS-AUTOMATE-PORTION           PIC S9(3)V99 COMP-3  * D442288W
00118      05  WRKRTS-AUTOMATE-PORTION           PIC S9(3)V99           D442288W
00119                                                      VALUE ZEROS. D442288W
00120 *---------------------------------------------------------------* D442288W
00121 *    05  WRKRTS-CNTY-CHILD-SAFE-PORTION    PIC S9(3)V99 COMP-3    D442288W
00122      05  WRKRTS-CNTY-CHILD-SAFE-PORTION    PIC S9(3)V99           D442288W
00123                                                      VALUE ZEROS. D442288W
00124      05  WRKRTS-CNTY-CSF-FEE REDEFINES                            D442288W
00125          WRKRTS-CNTY-CHILD-SAFE-PORTION    PIC S9(3)V99.          D442288W
00126 *---------------------------------------------------------------* D442288W
00127 *    05  WRKRTS-CNTY-ADD-ON-FEE            PIC S9(5)V99 COMP-3  * D442288W
00128      05  WRKRTS-CNTY-ADD-ON-FEE            PIC S9(5)V99           D442288W
00129                                                      VALUE ZEROS. D442288W
00130 *---------------------------------------------------------------* D442288W
00131 *    05  WRKRTS-REPLACEMENT-FEE            PIC S9(5)V99 COMP-3  * D442288W
00132      05  WRKRTS-REPLACEMENT-FEE            PIC S9(5)V99           D442288W
00133                                                      VALUE ZEROS. D442288W
00134 *---------------------------------------------------------------* D442288W
00135      05  WRKRTS-ACT-TYPE                   PIC X     VALUE SPACES.D442288W
00136          88  WRKRTS-APPORTIONED-ACCT                 VALUE 'A'.   D442288W
00137          88  WRKRTS-TOKEN--TRLR-ACCT                 VALUE 'T'.   D442288W
00138          88  WRKRTS-RENTAL-ACCT                      VALUE 'R'.   D442288W
00139          88  WRKRTS-INTRA-RENTAL-ACCT                VALUE 'R'.   D442288W
00140          88  WRKRTS-FOREST-ACCT                      VALUE 'F'.   D442288W
00141          88  WRKRTS-INTRA-FORESTRY-ACCT              VALUE 'F'.   D442288W
00142          88  WRKRTS-EXT-FLEET-ACCT                   VALUE 'E'.   D442288W
00143          88  WRKRTS-INTRA-EX-FLEET-ACCT              VALUE 'E'.   D442288W
00144 *---------------------------------------------------------------* D442288W
00145 *       REMOVED FROM BOTH FILES..DUPL OF -NAME-OF-APPLICANT     *.D442288W
00146 *    05  WRKRTS-ACT-NAME-OF-APPLICANT      PIC X(38) VALUE SPACE*.D442288W
00147 *---------------------------------------------------------------* D442288W
00148      05  WRKRTS-ACT-MAILING-STREET-ADDR    PIC X(28) VALUE SPACES.D442288W
00149      05  WRKRTS-ACT-CITY                   PIC X(15) VALUE SPACES.D442288W
00150      05  WRKRTS-ACT-STATE                  PIC X(2)  VALUE SPACES.D442288W
00151      05  WRKRTS-ACT-ZIPCODE                PIC X(9)  VALUE ZEROES.D442288W
00152      05  WRKRTS-VOUCHER-NUM                PIC X(7)  VALUE ZEROES.D442288W
00153      05  WRKRTS-EXPL-SERIAL-NUM            PIC 9(9)  VALUE ZEROES.D442288W
00154 *---------------------------------------------------------------* D442288W
00155      05  WRKRTS-ITEM-CODE                  PIC X(8) VALUE SPACES. D442288W
00156      05  WRKRTS-RTS-TRANSID.                                      D442288W
00157        10  WRKRTS-RTS-OFCISSUANCENO        PIC 9(3)  VALUE ZEROS. D442288W
00158        10  WRKRTS-RTS-TRANSWSID            PIC 9(3)  VALUE ZEROS. D442288W
00159        10  WRKRTS-RTS-TRAN-DATE            PIC 9(5)  VALUE ZEROS. D442288W
00160        10  WRKRTS-RTS-TRAN-TIME            PIC 9(6)  VALUE ZEROS. D442288W
00161      05  WRKRTS-UPD-INDI                   PIC X     VALUE SPACES.D442288W
00162          88  IRPRTS-P442289-IF-SYNC                  VALUE 'S'.   D442288W
00163          88  IRPRTS-P442289-IF-NORM                  VALUE 'N'.   D442288W
00164          88  IRPRTS-P442289-IF-OUTDT                 VALUE 'O'.   D442288W
00165          88  IRPRTS-P442289-IF-PURGE                 VALUE 'X'.   D442288W
00166      05  WRKRTS-PROGRAM-CHG-FLAG REDEFINES WRKRTS-UPD-INDI PIC X. D442288W
00167          88  WRKRTS-UPDAT-BY-RTS-INTERFACE           VALUE 'I'.   D442288W
00168          88  WRKRTS-UPDAT-DOCNO-SWITCH-BACK          VALUE '*'.   D442288W
00169      05  WRKRTS-REPLACED-DOCNO             PIC X(17) VALUE SPACE. D442288W
00170      05  WRKRTS-AFTERVIEW-CHG-FLAG         PIC X     VALUE SPACE. D442288W
00171          88  WRKRTS-UPDAT-BY-RTS-AFTERVIEW           VALUE 'A'.   D442288W
00172      05  WRKRTS-PROCESSING-ID              PIC X(8)  VALUE SPACE. D442288W
00173      05  WRKRTS-PROCESSING-DT.                                    D442288W
00174          10  WRKRTS-PROCESSING-YR          PIC 9(4)  VALUE ZEROS. D442288W
00175          10  WRKRTS-PROCESSING-MO          PIC 9(2)  VALUE ZEROS. D442288W
00176          10  WRKRTS-PROCESSING-DA          PIC 9(2)  VALUE ZEROS. D442288W
00177      05  WRKRTS-PROCESSING-FLAG            PIC X     VALUE SPACES.D442288W
00178          88  WRKRTS-INITIAL-CREATION                 VALUE 'I'.   D442288W
00179          88  WRKRTS-PENDING                          VALUE 'P'.   D442288W
00180          88  WRKRTS-FINAL-PROCESSING-OK              VALUE 'F'.   D442288W
00181          88  WRKRTS-CANCELLED-MVUPDATE               VALUE 'C'.   D442288W
00182          88  WRKRTS-DUMMY-NO-MVUPDATE                VALUE 'D'.   D442288W
00183          88  WRKRTS-ERROR-NO-MVUPDATE                VALUE 'E'.   D442288W
00184          88  WRKRTS-REFUND-MVUPDATE                  VALUE 'R'.   D442288W
00185          88  WRKRTS-OUTDATED-RTS                     VALUE 'O'.   D442288W
00186          88  WRKRTS-TONLY                            VALUE 'T'.   D442288W
00187          88  WRKRTS-UPDATED-VIN-DOCNO                VALUE 'U'.   D442288W
00188          88  WRKRTS-VIN-NOT-LOCATED                  VALUE 'V'.   D442288W
00189          88  WRKRTS-EXCLUDE--DELETE-IF               VALUE 'X'.   D442288W
00190          88  WRKRTS-REGIS-ZEROS                      VALUE 'Z'.   D442288W
00191      05  WRKRTS-LAST-FIELDX                PIC X(3)  VALUE 'END'. D442288W
00192 *---------------------------------------------------------------* D442288W
00193      05  WRKRTS-VERSION-CONTROL            PIC X(2).              D442288W
00194 *---------------------------------------------------------------* D442288W
00195      05  WRKRTS-CNTY-MOBILITY-PORTION      PIC S9(3)V99           D442288W
00196                                                      VALUE ZEROS. D442288W
00197      05  WRKRTS-CNTY-MOBILITY-FEE REDEFINES                       D442288W
00198          WRKRTS-CNTY-MOBILITY-PORTION      PIC S9(3)V99.          D442288W
00199      05  WRKRTS-FLEET-PLT-ITMCD            PIC X(8)  VALUE SPACES.D442288W
00200      05  WRKRTS-FLEET-REGCL-CD             PIC 9(3)  VALUE ZEROS. D442288W
00201      05  WRKRTS-FLEET-CREDIT-AMOUNT        PIC S9(5)V99           D442288W
00202                                                      VALUE ZEROS. D442288W
00203      05  WRKRTS-FLEET-PREV-REG-EXPDT       PIC 9(8)  VALUE ZEROS. D442288W
00204      05  WRKRTS-FLEET-REG-FEE              PIC S9(3)V99           D442288W
00205                                                      VALUE ZEROS. D442288W
00206      05  WRKRTS-MAN-FEE-GENERIC            PIC S9(3)V99           D442288W
00207                                                      VALUE ZEROS. D442288W
00208      05  WRKRTS-MAN-FEE-LOGO               PIC S9(3)V99           D442288W
00209                                                      VALUE ZEROS. D442288W
00001 *PAN MEMBER D442277W - IRP SYSTEM - MAINTAINED BY UNIT 52        *11/08/06
00002 ******************************************************************D442277W
00003 *             I.R.P. -  RTS INTERFACE REPORING FILE              *   LV004
00004 *                                                                *D442277W
00005 *                  IRPSYSRPTDATA                                 *D442277W
00006 *                                                                *D442277W
00007 *    RECORD CONTAINS    85 BYTES                                  D442277W
00008 *                                                                *D442277W
00009 * LAST UPDATE 11/07/2006 - APPEND TXIRP VOUCHER & TRANS DATE      D442277W
00010 ******************************************************************D442277W
00011                                                                   D442277W
00012  01  IRPSYS-RPT-DATA.                                             D442277W
00013      05  IRPRPT-TRANSID.                                          D442277W
00014        10  IRPRPT-OFCISSUANCENO            PIC 9(03).             D442277W
00015        10  IRPRPT-TRANSWSID                PIC 9(03).             D442277W
00016        10  IRPRPT-TRANSAMDATE              PIC 9(05).             D442277W
00017        10  IRPRPT-TRANSTIME                PIC 9(06).             D442277W
00018      05  IRPRPT-CNTYNO                     PIC 9(03).             D442277W
00019      05  IRPRPT-ITMPRICE                   PIC S9(07)V99.         D442277W
00020      05  IRPRPT-ITMCD                      PIC X(08).             D442277W
00021      05  IRPRPT-INVITMYR                   PIC 9(04).             D442277W
00022      05  IRPRPT-INVITMNO                   PIC X(10).             D442277W
00023      05  IRPRPT-IRPACCTNO                  PIC 9(06).             D442277W
00024      05  IRPRPT-ACCTITMCD                  PIC X(08).             D442277W
00025      05  IRPRPT-FLEETNO                    PIC X(02).             D442277W
00026      05  IRPRPT-SUPPLMNTNO                 PIC 9(03).             D442277W
00027      05  IRPRPT-TRANS-DATE                 PIC 9(08).             D442277W
00028      05  IRPRPT-VOUCHER-NUM                PIC 9(07).             D442277W
00807                                                                   P442288 
00808                                                                   P442288 
00809  01  DAYNUM             PIC S9(9) COMP.                           P442288 
00001  01  LILIAN                  PIC S9(9) COMP.                      05/14/97
      D01  WS-LILIAN               PIC 9(9).                                    
00002  01  CHRDATE                 PIC X(80).                           DDTRNWS 
00003  01  IN-DATE.                                                        LV002
00004      02  VSTRING-LENGTH      PIC S9(4) COMP.                      DDTRNWS 
00005      02  VSTRING-TEXT.                                            DDTRNWS 
00006          03  VSTRING-CHAR    PIC X,                               DDTRNWS 
00007                      OCCURS 0 TO 256 TIMES,                       DDTRNWS 
00008                      DEPENDING ON VSTRING-LENGTH                  DDTRNWS 
00009                          OF IN-DATE.                              DDTRNWS 
00010  01  PICSTR.                                                      DDTRNWS 
00011      02  VSTRING-LENGTH      PIC S9(4) COMP.                      DDTRNWS 
00012      02  VSTRING-TEXT.                                            DDTRNWS 
00013          03  VSTRING-CHAR    PIC X,                               DDTRNWS 
00014                      OCCURS 0 TO 256 TIMES,                       DDTRNWS 
00015                      DEPENDING ON VSTRING-LENGTH                  DDTRNWS 
00016                         OF PICSTR.                                DDTRNWS 
00017  01  FC.                                                          DDTRNWS 
00018      02  CONDITION-TOKEN-VALUE.                                   DDTRNWS 
00001 ****************************************************              05/07/97
00002 *  CEEIGZCT  COPY                                  *              CEEIGZCT
00003 *                                                  *                 LV001
00004 *  IBM LANGUAGE ENVIRONMENT FOR MVS & VM           *              CEEIGZCT
00005 *       VERSION 1 RELEASE 5 MODIFICATION 0         *              CEEIGZCT
00006 *                                                  *              CEEIGZCT
00007 *  LICENSED MATERIALS - PROPERTY OF IBM.           *              CEEIGZCT
00008 *                                                  *              CEEIGZCT
00009 *  5688-198 (C) COPYRIGHT IBM CORP. 1993,  1995    *              CEEIGZCT
00010 *  ALL RIGHTS RESERVED                             *              CEEIGZCT
00011 *  US GOVERNMENT USERS RESTRICTED RIGHTS - USE,    *              CEEIGZCT
00012 *  DUPLICATION OR DISCLOSURE RESTRICTED BY GSA     *              CEEIGZCT
00013 *  ADP SCHEDULE CONTRACT WITH IBM CORP.            *              CEEIGZCT
00014 *                                                  *              CEEIGZCT
00015 ****************************************************              CEEIGZCT
00016 * Condition Token Declarations                                    CEEIGZCT
00017 *                                                                 CEEIGZCT
00018  88  CEE000        VALUE X"0000000000000000".                     CEEIGZCT
00019  88  CEE001        VALUE X"0000000141C3C5C5".                     CEEIGZCT
00020  88  CEE002        VALUE X"0000000241C3C5C5".                     CEEIGZCT
00021  88  CEE003        VALUE X"0000000341C3C5C5".                     CEEIGZCT
00022  88  CEE004        VALUE X"0000000441C3C5C5".                     CEEIGZCT
00023  88  CEE005        VALUE X"0000000541C3C5C5".                     CEEIGZCT
00024  88  CEE006        VALUE X"0000000641C3C5C5".                     CEEIGZCT
00025  88  CEE007        VALUE X"0000000741C3C5C5".                     CEEIGZCT
00026  88  CEE008        VALUE X"0000000841C3C5C5".                     CEEIGZCT
00027  88  CEE009        VALUE X"0000000941C3C5C5".                     CEEIGZCT
00028  88  CEE00A        VALUE X"0000000A41C3C5C5".                     CEEIGZCT
00029  88  CEE00B        VALUE X"0000000B41C3C5C5".                     CEEIGZCT
00030  88  CEE00C        VALUE X"0000000C41C3C5C5".                     CEEIGZCT
00031  88  CEE00D        VALUE X"0000000D41C3C5C5".                     CEEIGZCT
00032  88  CEE00E        VALUE X"0000000E41C3C5C5".                     CEEIGZCT
00033  88  CEE00F        VALUE X"0000000F41C3C5C5".                     CEEIGZCT
00034  88  CEE00G        VALUE X"0000001041C3C5C5".                     CEEIGZCT
00035  88  CEE00H        VALUE X"0000001141C3C5C5".                     CEEIGZCT
00036  88  CEE00I        VALUE X"0000001241C3C5C5".                     CEEIGZCT
00037  88  CEE00J        VALUE X"0000001341C3C5C5".                     CEEIGZCT
00038  88  CEE00L        VALUE X"0000001541C3C5C5".                     CEEIGZCT
00039  88  CEE00M        VALUE X"0000001641C3C5C5".                     CEEIGZCT
00040  88  CEE00N        VALUE X"0000001741C3C5C5".                     CEEIGZCT
00041  88  CEE00O        VALUE X"0000001841C3C5C5".                     CEEIGZCT
00042  88  CEE00P        VALUE X"0000001941C3C5C5".                     CEEIGZCT
00043  88  CEE00Q        VALUE X"0000001A41C3C5C5".                     CEEIGZCT
00044  88  CEE00R        VALUE X"0000001B41C3C5C5".                     CEEIGZCT
00045  88  CEE00S        VALUE X"0000001C41C3C5C5".                     CEEIGZCT
00046  88  CEE00T        VALUE X"0000001D41C3C5C5".                     CEEIGZCT
00047  88  CEE00U        VALUE X"0000001E41C3C5C5".                     CEEIGZCT
00048  88  CEE00V        VALUE X"0000001F41C3C5C5".                     CEEIGZCT
00049  88  CEE010        VALUE X"0000002041C3C5C5".                     CEEIGZCT
00050  88  CEE011        VALUE X"0000002141C3C5C5".                     CEEIGZCT
00051  88  CEE012        VALUE X"0000002241C3C5C5".                     CEEIGZCT
00052  88  CEE013        VALUE X"0000002341C3C5C5".                     CEEIGZCT
00053  88  CEE014        VALUE X"0000002441C3C5C5".                     CEEIGZCT
00054  88  CEE015        VALUE X"0000002541C3C5C5".                     CEEIGZCT
00055  88  CEE016        VALUE X"0000002641C3C5C5".                     CEEIGZCT
00056  88  CEE017        VALUE X"0000002741C3C5C5".                     CEEIGZCT
00057  88  CEE036        VALUE X"0003006659C3C5C5".                     CEEIGZCT
00058  88  CEE03E        VALUE X"0003006E59C3C5C5".                     CEEIGZCT
00059  88  CEE03F        VALUE X"0003006F59C3C5C5".                     CEEIGZCT
00060  88  CEE03G        VALUE X"0003007059C3C5C5".                     CEEIGZCT
00061  88  CEE03H        VALUE X"0003007159C3C5C5".                     CEEIGZCT
00062  88  CEE066        VALUE X"000300C659C3C5C5".                     CEEIGZCT
00063  88  CEE067        VALUE X"000100C749C3C5C5".                     CEEIGZCT
00064  88  CEE069        VALUE X"000000C941C3C5C5".                     CEEIGZCT
00065  88  CEE07Q        VALUE X"000300FA59C3C5C5".                     CEEIGZCT
00066  88  CEE07S        VALUE X"000100FC49C3C5C5".                     CEEIGZCT
00067  88  CEE07T        VALUE X"000100FD49C3C5C5".                     CEEIGZCT
00068  88  CEE07U        VALUE X"000100FE49C3C5C5".                     CEEIGZCT
00069  88  CEE07V        VALUE X"000300FF59C3C5C5".                     CEEIGZCT
00070  88  CEE080        VALUE X"0001010049C3C5C5".                     CEEIGZCT
00071  88  CEE081        VALUE X"0003010159C3C5C5".                     CEEIGZCT
00072  88  CEE083        VALUE X"0003010359C3C5C5".                     CEEIGZCT
00073  88  CEE084        VALUE X"0003010459C3C5C5".                     CEEIGZCT
00074  88  CEE088        VALUE X"0003010859C3C5C5".                     CEEIGZCT
00075  88  CEE08L        VALUE X"0001011549C3C5C5".                     CEEIGZCT
00076  88  CEE0B3        VALUE X"0004016361C3C5C5".                     CEEIGZCT
00077  88  CEE0B4        VALUE X"0004016461C3C5C5".                     CEEIGZCT
00078  88  CEE0BM        VALUE X"0004017661C3C5C5".                     CEEIGZCT
00079  88  CEE0CE        VALUE X"0001018E49C3C5C5".                     CEEIGZCT
00080  88  CEE0CF        VALUE X"0001018F49C3C5C5".                     CEEIGZCT
00081  88  CEE0CG        VALUE X"0002019051C3C5C5".                     CEEIGZCT
00082  88  CEE0CH        VALUE X"0003019159C3C5C5".                     CEEIGZCT
00083  88  CEE0CI        VALUE X"0003019259C3C5C5".                     CEEIGZCT
00084  88  CEE0CJ        VALUE X"0003019359C3C5C5".                     CEEIGZCT
00085  88  CEE0CK        VALUE X"0001019449C3C5C5".                     CEEIGZCT
00086  88  CEE0E2        VALUE X"000301C259C3C5C5".                     CEEIGZCT
00087  88  CEE0E3        VALUE X"000301C359C3C5C5".                     CEEIGZCT
00088  88  CEE0E4        VALUE X"000301C459C3C5C5".                     CEEIGZCT
00089  88  CEE0E6        VALUE X"000301C659C3C5C5".                     CEEIGZCT
00090  88  CEE0E7        VALUE X"000101C749C3C5C5".                     CEEIGZCT
00091  88  CEE0E9        VALUE X"000301C959C3C5C5".                     CEEIGZCT
00092  88  CEE0EA        VALUE X"000301CA59C3C5C5".                     CEEIGZCT
00093  88  CEE0EB        VALUE X"000301CB59C3C5C5".                     CEEIGZCT
00094  88  CEE0EC        VALUE X"000101CC49C3C5C5".                     CEEIGZCT
00095  88  CEE0ED        VALUE X"000301CD59C3C5C5".                     CEEIGZCT
00096  88  CEE0EE        VALUE X"000301CE59C3C5C5".                     CEEIGZCT
00097  88  CEE0EF        VALUE X"000301CF59C3C5C5".                     CEEIGZCT
00098  88  CEE0EG        VALUE X"000301D059C3C5C5".                     CEEIGZCT
00099  88  CEE0FM        VALUE X"000301F659C3C5C5".                     CEEIGZCT
00100  88  CEE0H9        VALUE X"0003022959C3C5C5".                     CEEIGZCT
00101  88  CEE0HA        VALUE X"0001022A49C3C5C5".                     CEEIGZCT
00102  88  CEE0P2        VALUE X"0004032261C3C5C5".                     CEEIGZCT
00103  88  CEE0P3        VALUE X"0003032359C3C5C5".                     CEEIGZCT
00104  88  CEE0P4        VALUE X"0003032459C3C5C5".                     CEEIGZCT
00105  88  CEE0P5        VALUE X"0003032559C3C5C5".                     CEEIGZCT
00106  88  CEE0P6        VALUE X"0003032659C3C5C5".                     CEEIGZCT
00107  88  CEE0P8        VALUE X"0003032859C3C5C5".                     CEEIGZCT
00108  88  CEE0P9        VALUE X"0003032959C3C5C5".                     CEEIGZCT
00109  88  CEE0PA        VALUE X"0003032A59C3C5C5".                     CEEIGZCT
00110  88  CEE0PC        VALUE X"0003032C59C3C5C5".                     CEEIGZCT
00111  88  CEE0PD        VALUE X"0003032D59C3C5C5".                     CEEIGZCT
00112  88  CEE0V8        VALUE X"000303E859C3C5C5".                     CEEIGZCT
00113  88  CEE0V9        VALUE X"000203E951C3C5C5".                     CEEIGZCT
00114  88  CEE1UH        VALUE X"000207D151C3C5C5".                     CEEIGZCT
00115  88  CEE1UI        VALUE X"000207D251C3C5C5".                     CEEIGZCT
00116  88  CEE1UJ        VALUE X"000207D351C3C5C5".                     CEEIGZCT
00117  88  CEE1UK        VALUE X"000207D451C3C5C5".                     CEEIGZCT
00118  88  CEE1UL        VALUE X"000207D551C3C5C5".                     CEEIGZCT
00119  88  CEE1UM        VALUE X"000207D651C3C5C5".                     CEEIGZCT
00120  88  CEE1UN        VALUE X"000207D751C3C5C5".                     CEEIGZCT
00121  88  CEE1UO        VALUE X"000207D851C3C5C5".                     CEEIGZCT
00122  88  CEE1UP        VALUE X"000207D951C3C5C5".                     CEEIGZCT
00123  88  CEE1UQ        VALUE X"000207DA51C3C5C5".                     CEEIGZCT
00124  88  CEE1UR        VALUE X"000207DB51C3C5C5".                     CEEIGZCT
00125  88  CEE1US        VALUE X"000207DC51C3C5C5".                     CEEIGZCT
00126  88  CEE1UT        VALUE X"000207DD51C3C5C5".                     CEEIGZCT
00127  88  CEE1UU        VALUE X"000207DE51C3C5C5".                     CEEIGZCT
00128  88  CEE1UV        VALUE X"000207DF51C3C5C5".                     CEEIGZCT
00129  88  CEE1V0        VALUE X"000207E051C3C5C5".                     CEEIGZCT
00130  88  CEE1V1        VALUE X"000207E151C3C5C5".                     CEEIGZCT
00131  88  CEE1V2        VALUE X"000207E251C3C5C5".                     CEEIGZCT
00132  88  CEE1V3        VALUE X"000207E351C3C5C5".                     CEEIGZCT
00133  88  CEE1V4        VALUE X"000207E451C3C5C5".                     CEEIGZCT
00134  88  CEE1V5        VALUE X"000207E551C3C5C5".                     CEEIGZCT
00135  88  CEE1V6        VALUE X"000207E651C3C5C5".                     CEEIGZCT
00136  88  CEE1V8        VALUE X"000207E851C3C5C5".                     CEEIGZCT
00137  88  CEE1V9        VALUE X"000107E949C3C5C5".                     CEEIGZCT
00138  88  CEE1VC        VALUE X"000207EC51C3C5C5".                     CEEIGZCT
00139  88  CEE1VD        VALUE X"000207ED51C3C5C5".                     CEEIGZCT
00140  88  CEE1VE        VALUE X"000207EE51C3C5C5".                     CEEIGZCT
00141  88  CEE1VF        VALUE X"000207EF51C3C5C5".                     CEEIGZCT
00142  88  CEE1VO        VALUE X"000207F851C3C5C5".                     CEEIGZCT
00143  88  CEE1VP        VALUE X"000207F951C3C5C5".                     CEEIGZCT
00144  88  CEE1VQ        VALUE X"000207FA51C3C5C5".                     CEEIGZCT
00145  88  CEE1VR        VALUE X"000207FB51C3C5C5".                     CEEIGZCT
00146  88  CEE202        VALUE X"0003080259C3C5C5".                     CEEIGZCT
00147  88  CEE203        VALUE X"0003080359C3C5C5".                     CEEIGZCT
00148  88  CEE204        VALUE X"0003080459C3C5C5".                     CEEIGZCT
00149  88  CEE205        VALUE X"0003080559C3C5C5".                     CEEIGZCT
00150  88  CEE2E6        VALUE X"000309C659C3C5C5".                     CEEIGZCT
00151  88  CEE2E7        VALUE X"000309C759C3C5C5".                     CEEIGZCT
00152  88  CEE2E9        VALUE X"000309C959C3C5C5".                     CEEIGZCT
00153  88  CEE2EA        VALUE X"000309CA59C3C5C5".                     CEEIGZCT
00154  88  CEE2EB        VALUE X"000309CB59C3C5C5".                     CEEIGZCT
00155  88  CEE2EC        VALUE X"000309CC59C3C5C5".                     CEEIGZCT
00156  88  CEE2ED        VALUE X"000309CD59C3C5C5".                     CEEIGZCT
00157  88  CEE2EE        VALUE X"000309CE59C3C5C5".                     CEEIGZCT
00158  88  CEE2EF        VALUE X"000309CF59C3C5C5".                     CEEIGZCT
00159  88  CEE2EG        VALUE X"000309D059C3C5C5".                     CEEIGZCT
00160  88  CEE2EH        VALUE X"000309D159C3C5C5".                     CEEIGZCT
00161  88  CEE2EI        VALUE X"000309D259C3C5C5".                     CEEIGZCT
00162  88  CEE2EJ        VALUE X"000309D359C3C5C5".                     CEEIGZCT
00163  88  CEE2EK        VALUE X"000309D459C3C5C5".                     CEEIGZCT
00164  88  CEE2EL        VALUE X"000309D559C3C5C5".                     CEEIGZCT
00165  88  CEE2EM        VALUE X"000309D659C3C5C5".                     CEEIGZCT
00166  88  CEE2EN        VALUE X"000309D759C3C5C5".                     CEEIGZCT
00167  88  CEE2EO        VALUE X"000309D859C3C5C5".                     CEEIGZCT
00168  88  CEE2EP        VALUE X"000309D959C3C5C5".                     CEEIGZCT
00169  88  CEE2EQ        VALUE X"000309DA59C3C5C5".                     CEEIGZCT
00170  88  CEE2ER        VALUE X"000109DB49C3C5C5".                     CEEIGZCT
00171  88  CEE2ES        VALUE X"000309DC59C3C5C5".                     CEEIGZCT
00172  88  CEE2ET        VALUE X"000309DD59C3C5C5".                     CEEIGZCT
00173  88  CEE2EU        VALUE X"000209DE51C3C5C5".                     CEEIGZCT
00174  88  CEE2EV        VALUE X"000209DF51C3C5C5".                     CEEIGZCT
00175  88  CEE2F1        VALUE X"000309E159C3C5C5".                     CEEIGZCT
00176  88  CEE2F2        VALUE X"000309E259C3C5C5".                     CEEIGZCT
00177  88  CEE2F3        VALUE X"000309E359C3C5C5".                     CEEIGZCT
00178  88  CEE2F5        VALUE X"000309E559C3C5C5".                     CEEIGZCT
00179  88  CEE2F6        VALUE X"000109E649C3C5C5".                     CEEIGZCT
00180  88  CEE2H8        VALUE X"00000A2841C3C5C5".                     CEEIGZCT
00181  88  CEE2H9        VALUE X"00000A2941C3C5C5".                     CEEIGZCT
00182  88  CEE2HA        VALUE X"00000A2A41C3C5C5".                     CEEIGZCT
00183  88  CEE2HB        VALUE X"00000A2B41C3C5C5".                     CEEIGZCT
00184  88  CEE2HC        VALUE X"00000A2C41C3C5C5".                     CEEIGZCT
00185  88  CEE2HE        VALUE X"00020A2E51C3C5C5".                     CEEIGZCT
00186  88  CEE2HF        VALUE X"00020A2F51C3C5C5".                     CEEIGZCT
00187  88  CEE2HG        VALUE X"00020A3051C3C5C5".                     CEEIGZCT
00188  88  CEE2KD        VALUE X"00030A8D59C3C5C5".                     CEEIGZCT
00189  88  CEE2KE        VALUE X"00030A8E59C3C5C5".                     CEEIGZCT
00190  88  CEE2TN        VALUE X"00040BB761C3C5C5".                     CEEIGZCT
00191  88  CEE2TO        VALUE X"00000BB841C3C5C5".                     CEEIGZCT
00192  88  CEE2TP        VALUE X"00000BB941C3C5C5".                     CEEIGZCT
00193  88  CEE2TQ        VALUE X"00000BBA41C3C5C5".                     CEEIGZCT
00194  88  CEE2TR        VALUE X"00000BBB41C3C5C5".                     CEEIGZCT
00195  88  CEE2TS        VALUE X"00000BBC41C3C5C5".                     CEEIGZCT
00196  88  CEE2TT        VALUE X"00000BBD41C3C5C5".                     CEEIGZCT
00197  88  CEE2TU        VALUE X"00000BBE41C3C5C5".                     CEEIGZCT
00198  88  CEE2TV        VALUE X"00000BBF41C3C5C5".                     CEEIGZCT
00199  88  CEE2U0        VALUE X"00000BC041C3C5C5".                     CEEIGZCT
00200  88  CEE2U1        VALUE X"00000BC141C3C5C5".                     CEEIGZCT
00201  88  CEE2U2        VALUE X"00000BC241C3C5C5".                     CEEIGZCT
00202  88  CEE2U3        VALUE X"00000BC341C3C5C5".                     CEEIGZCT
00203  88  CEE2U4        VALUE X"00000BC441C3C5C5".                     CEEIGZCT
00204  88  CEE2U5        VALUE X"00000BC541C3C5C5".                     CEEIGZCT
00205  88  CEE2U6        VALUE X"00000BC641C3C5C5".                     CEEIGZCT
00206  88  CEE2U7        VALUE X"00000BC741C3C5C5".                     CEEIGZCT
00207  88  CEE2U8        VALUE X"00000BC841C3C5C5".                     CEEIGZCT
00208  88  CEE2U9        VALUE X"00000BC941C3C5C5".                     CEEIGZCT
00209  88  CEE2UA        VALUE X"00000BCA41C3C5C5".                     CEEIGZCT
00210  88  CEE2UB        VALUE X"00000BCB41C3C5C5".                     CEEIGZCT
00211  88  CEE2UC        VALUE X"00000BCC41C3C5C5".                     CEEIGZCT
00212  88  CEE2UD        VALUE X"00000BCD41C3C5C5".                     CEEIGZCT
00213  88  CEE2UE        VALUE X"00000BCE41C3C5C5".                     CEEIGZCT
00214  88  CEE2UF        VALUE X"00000BCF41C3C5C5".                     CEEIGZCT
00215  88  CEE2UG        VALUE X"00000BD041C3C5C5".                     CEEIGZCT
00216  88  CEE2UH        VALUE X"00000BD141C3C5C5".                     CEEIGZCT
00217  88  CEE2UI        VALUE X"00000BD241C3C5C5".                     CEEIGZCT
00218  88  CEE2UJ        VALUE X"00000BD341C3C5C5".                     CEEIGZCT
00219  88  CEE2UK        VALUE X"00000BD441C3C5C5".                     CEEIGZCT
00220  88  CEE2UL        VALUE X"00000BD541C3C5C5".                     CEEIGZCT
00221  88  CEE2UM        VALUE X"00000BD641C3C5C5".                     CEEIGZCT
00222  88  CEE2UN        VALUE X"00000BD741C3C5C5".                     CEEIGZCT
00223  88  CEE2UO        VALUE X"00000BD841C3C5C5".                     CEEIGZCT
00224  88  CEE2UP        VALUE X"00000BD941C3C5C5".                     CEEIGZCT
00225  88  CEE2UQ        VALUE X"00000BDA41C3C5C5".                     CEEIGZCT
00226  88  CEE2UR        VALUE X"00000BDB41C3C5C5".                     CEEIGZCT
00227  88  CEE2US        VALUE X"00000BDC41C3C5C5".                     CEEIGZCT
00228  88  CEE2UT        VALUE X"00000BDD41C3C5C5".                     CEEIGZCT
00229  88  CEE2UU        VALUE X"00000BDE41C3C5C5".                     CEEIGZCT
00230  88  CEE2UV        VALUE X"00000BDF41C3C5C5".                     CEEIGZCT
00231  88  CEE2V0        VALUE X"00000BE041C3C5C5".                     CEEIGZCT
00232  88  CEE2V1        VALUE X"00000BE141C3C5C5".                     CEEIGZCT
00233  88  CEE2V2        VALUE X"00000BE241C3C5C5".                     CEEIGZCT
00234  88  CEE2V3        VALUE X"00000BE341C3C5C5".                     CEEIGZCT
00235  88  CEE2V4        VALUE X"00000BE441C3C5C5".                     CEEIGZCT
00236  88  CEE2V5        VALUE X"00000BE541C3C5C5".                     CEEIGZCT
00237  88  CEE2V6        VALUE X"00000BE641C3C5C5".                     CEEIGZCT
00238  88  CEE2V7        VALUE X"00000BE741C3C5C5".                     CEEIGZCT
00239  88  CEE2V8        VALUE X"00000BE841C3C5C5".                     CEEIGZCT
00240  88  CEE2V9        VALUE X"00000BE941C3C5C5".                     CEEIGZCT
00241  88  CEE2VA        VALUE X"00000BEA41C3C5C5".                     CEEIGZCT
00242  88  CEE2VB        VALUE X"00000BEB41C3C5C5".                     CEEIGZCT
00243  88  CEE2VC        VALUE X"00000BEC41C3C5C5".                     CEEIGZCT
00244  88  CEE2VD        VALUE X"00000BED41C3C5C5".                     CEEIGZCT
00245  88  CEE2VE        VALUE X"00000BEE41C3C5C5".                     CEEIGZCT
00246  88  CEE2VF        VALUE X"00000BEF41C3C5C5".                     CEEIGZCT
00247  88  CEE2VG        VALUE X"00000BF041C3C5C5".                     CEEIGZCT
00248  88  CEE2VH        VALUE X"00000BF141C3C5C5".                     CEEIGZCT
00249  88  CEE2VI        VALUE X"00000BF241C3C5C5".                     CEEIGZCT
00250  88  CEE2VJ        VALUE X"00000BF341C3C5C5".                     CEEIGZCT
00251  88  CEE2VK        VALUE X"00000BF441C3C5C5".                     CEEIGZCT
00252  88  CEE2VL        VALUE X"00000BF541C3C5C5".                     CEEIGZCT
00253  88  CEE2VM        VALUE X"00000BF641C3C5C5".                     CEEIGZCT
00254  88  CEE2VN        VALUE X"00000BF741C3C5C5".                     CEEIGZCT
00255  88  CEE2VO        VALUE X"00000BF841C3C5C5".                     CEEIGZCT
00256  88  CEE2VP        VALUE X"00000BF941C3C5C5".                     CEEIGZCT
00257  88  CEE2VQ        VALUE X"00000BFA41C3C5C5".                     CEEIGZCT
00258  88  CEE30Q        VALUE X"00030C1A59C3C5C5".                     CEEIGZCT
00259  88  CEE30T        VALUE X"00020C1D51C3C5C5".                     CEEIGZCT
00260  88  CEE30U        VALUE X"00020C1E51C3C5C5".                     CEEIGZCT
00261  88  CEE30V        VALUE X"00030C1F59C3C5C5".                     CEEIGZCT
00262  88  CEE310        VALUE X"00030C2059C3C5C5".                     CEEIGZCT
00263  88  CEE311        VALUE X"00030C2159C3C5C5".                     CEEIGZCT
00264  88  CEE312        VALUE X"00030C2259C3C5C5".                     CEEIGZCT
00265  88  CEE313        VALUE X"00020C2351C3C5C5".                     CEEIGZCT
00266  88  CEE314        VALUE X"00020C2451C3C5C5".                     CEEIGZCT
00267  88  CEE315        VALUE X"00000C2541C3C5C5".                     CEEIGZCT
00268  88  CEE31G        VALUE X"00000C3041C3C5C5".                     CEEIGZCT
00269  88  CEE31H        VALUE X"00000C3141C3C5C5".                     CEEIGZCT
00270  88  CEE31I        VALUE X"00000C3241C3C5C5".                     CEEIGZCT
00271  88  CEE31J        VALUE X"00000C3341C3C5C5".                     CEEIGZCT
00272  88  CEE31K        VALUE X"00000C3441C3C5C5".                     CEEIGZCT
00273  88  CEE31L        VALUE X"00000C3541C3C5C5".                     CEEIGZCT
00274  88  CEE31M        VALUE X"00000C3641C3C5C5".                     CEEIGZCT
00275  88  CEE31N        VALUE X"00000C3741C3C5C5".                     CEEIGZCT
00276  88  CEE31O        VALUE X"00000C3841C3C5C5".                     CEEIGZCT
00277  88  CEE31P        VALUE X"00000C3941C3C5C5".                     CEEIGZCT
00278  88  CEE31Q        VALUE X"00000C3A41C3C5C5".                     CEEIGZCT
00279  88  CEE31R        VALUE X"00000C3B41C3C5C5".                     CEEIGZCT
00280  88  CEE31S        VALUE X"00000C3C41C3C5C5".                     CEEIGZCT
00281  88  CEE31T        VALUE X"00000C3D41C3C5C5".                     CEEIGZCT
00282  88  CEE31U        VALUE X"00000C3E41C3C5C5".                     CEEIGZCT
00283  88  CEE320        VALUE X"00000C4041C3C5C5".                     CEEIGZCT
00284  88  CEE321        VALUE X"00000C4141C3C5C5".                     CEEIGZCT
00285  88  CEE322        VALUE X"00000C4241C3C5C5".                     CEEIGZCT
00286  88  CEE323        VALUE X"00000C4341C3C5C5".                     CEEIGZCT
00287  88  CEE324        VALUE X"00000C4441C3C5C5".                     CEEIGZCT
00288  88  CEE325        VALUE X"00000C4541C3C5C5".                     CEEIGZCT
00289  88  CEE326        VALUE X"00000C4641C3C5C5".                     CEEIGZCT
00290  88  CEE327        VALUE X"00000C4741C3C5C5".                     CEEIGZCT
00291  88  CEE328        VALUE X"00000C4841C3C5C5".                     CEEIGZCT
00292  88  CEE329        VALUE X"00000C4941C3C5C5".                     CEEIGZCT
00293  88  CEE32A        VALUE X"00000C4A41C3C5C5".                     CEEIGZCT
00294  88  CEE32B        VALUE X"00000C4B41C3C5C5".                     CEEIGZCT
00295  88  CEE32C        VALUE X"00000C4C41C3C5C5".                     CEEIGZCT
00296  88  CEE32D        VALUE X"00000C4D41C3C5C5".                     CEEIGZCT
00297  88  CEE32E        VALUE X"00000C4E41C3C5C5".                     CEEIGZCT
00298  88  CEE32F        VALUE X"00000C4F41C3C5C5".                     CEEIGZCT
00299  88  CEE32G        VALUE X"00000C5041C3C5C5".                     CEEIGZCT
00300  88  CEE32H        VALUE X"00000C5141C3C5C5".                     CEEIGZCT
00301  88  CEE32I        VALUE X"00000C5241C3C5C5".                     CEEIGZCT
00302  88  CEE32J        VALUE X"00000C5341C3C5C5".                     CEEIGZCT
00303  88  CEE32K        VALUE X"00000C5441C3C5C5".                     CEEIGZCT
00304  88  CEE32L        VALUE X"00000C5541C3C5C5".                     CEEIGZCT
00305  88  CEE32M        VALUE X"00000C5641C3C5C5".                     CEEIGZCT
00306  88  CEE32N        VALUE X"00000C5741C3C5C5".                     CEEIGZCT
00307  88  CEE32O        VALUE X"00000C5841C3C5C5".                     CEEIGZCT
00308  88  CEE32P        VALUE X"00000C5941C3C5C5".                     CEEIGZCT
00309  88  CEE32Q        VALUE X"00000C5A41C3C5C5".                     CEEIGZCT
00310  88  CEE32R        VALUE X"00000C5B41C3C5C5".                     CEEIGZCT
00311  88  CEE32S        VALUE X"00000C5C41C3C5C5".                     CEEIGZCT
00312  88  CEE32T        VALUE X"00000C5D41C3C5C5".                     CEEIGZCT
00313  88  CEE32U        VALUE X"00000C5E41C3C5C5".                     CEEIGZCT
00314  88  CEE32V        VALUE X"00000C5F41C3C5C5".                     CEEIGZCT
00315  88  CEE330        VALUE X"00000C6041C3C5C5".                     CEEIGZCT
00316  88  CEE331        VALUE X"00000C6141C3C5C5".                     CEEIGZCT
00317  88  CEE332        VALUE X"00000C6241C3C5C5".                     CEEIGZCT
00318  88  CEE333        VALUE X"00000C6341C3C5C5".                     CEEIGZCT
00319  88  CEE334        VALUE X"00000C6441C3C5C5".                     CEEIGZCT
00320  88  CEE335        VALUE X"00000C6541C3C5C5".                     CEEIGZCT
00321  88  CEE336        VALUE X"00000C6641C3C5C5".                     CEEIGZCT
00322  88  CEE337        VALUE X"00000C6741C3C5C5".                     CEEIGZCT
00323  88  CEE338        VALUE X"00000C6841C3C5C5".                     CEEIGZCT
00324  88  CEE339        VALUE X"00000C6941C3C5C5".                     CEEIGZCT
00325  88  CEE33A        VALUE X"00000C6A41C3C5C5".                     CEEIGZCT
00326  88  CEE33B        VALUE X"00000C6B41C3C5C5".                     CEEIGZCT
00327  88  CEE33C        VALUE X"00000C6C41C3C5C5".                     CEEIGZCT
00328  88  CEE33D        VALUE X"00000C6D41C3C5C5".                     CEEIGZCT
00329  88  CEE33E        VALUE X"00000C6E41C3C5C5".                     CEEIGZCT
00330  88  CEE33F        VALUE X"00000C6F41C3C5C5".                     CEEIGZCT
00331  88  CEE33G        VALUE X"00000C7041C3C5C5".                     CEEIGZCT
00332  88  CEE33H        VALUE X"00000C7141C3C5C5".                     CEEIGZCT
00333  88  CEE33I        VALUE X"00020C7251C3C5C5".                     CEEIGZCT
00334  88  CEE33J        VALUE X"00000C7341C3C5C5".                     CEEIGZCT
00335  88  CEE33K        VALUE X"00000C7441C3C5C5".                     CEEIGZCT
00336  88  CEE33L        VALUE X"00000C7541C3C5C5".                     CEEIGZCT
00337  88  CEE33M        VALUE X"00000C7641C3C5C5".                     CEEIGZCT
00338  88  CEE33N        VALUE X"00020C7751C3C5C5".                     CEEIGZCT
00339  88  CEE33O        VALUE X"00040C7861C3C5C5".                     CEEIGZCT
00340  88  CEE33P        VALUE X"00000C7941C3C5C5".                     CEEIGZCT
00341  88  CEE33R        VALUE X"00010C7B49C3C5C5".                     CEEIGZCT
00342  88  CEE33S        VALUE X"00010C7C49C3C5C5".                     CEEIGZCT
00343  88  CEE33T        VALUE X"00010C7D49C3C5C5".                     CEEIGZCT
00344  88  CEE33U        VALUE X"00030C7E59C3C5C5".                     CEEIGZCT
00345  88  CEE33V        VALUE X"00030C7F59C3C5C5".                     CEEIGZCT
00346  88  CEE341        VALUE X"00030C8159C3C5C5".                     CEEIGZCT
00347  88  CEE342        VALUE X"00030C8259C3C5C5".                     CEEIGZCT
00348  88  CEE343        VALUE X"00030C8359C3C5C5".                     CEEIGZCT
00349  88  CEE344        VALUE X"00030C8459C3C5C5".                     CEEIGZCT
00350  88  CEE345        VALUE X"00030C8559C3C5C5".                     CEEIGZCT
00351  88  CEE346        VALUE X"00030C8659C3C5C5".                     CEEIGZCT
00352  88  CEE347        VALUE X"00030C8759C3C5C5".                     CEEIGZCT
00353  88  CEE348        VALUE X"00030C8859C3C5C5".                     CEEIGZCT
00354  88  CEE349        VALUE X"00030C8959C3C5C5".                     CEEIGZCT
00355  88  CEE34A        VALUE X"00030C8A59C3C5C5".                     CEEIGZCT
00356  88  CEE34B        VALUE X"00030C8B59C3C5C5".                     CEEIGZCT
00357  88  CEE34C        VALUE X"00030C8C59C3C5C5".                     CEEIGZCT
00358  88  CEE34D        VALUE X"00030C8D59C3C5C5".                     CEEIGZCT
00359  88  CEE34E        VALUE X"00030C8E59C3C5C5".                     CEEIGZCT
00360  88  CEE34F        VALUE X"00030C8F59C3C5C5".                     CEEIGZCT
00361  88  CEE34U        VALUE X"00020C9E51C3C5C5".                     CEEIGZCT
00362  88  CEE35H        VALUE X"00000CB141C3C5C5".                     CEEIGZCT
00363  88  CEE35I        VALUE X"00040CB261C3C5C5".                     CEEIGZCT
00364  88  CEE35J        VALUE X"00000CB341C3C5C5".                     CEEIGZCT
00365  88  CEE35L        VALUE X"00040CB561C3C5C5".                     CEEIGZCT
00366  88  CEE35M        VALUE X"00040CB661C3C5C5".                     CEEIGZCT
00367  88  CEE35N        VALUE X"00040CB761C3C5C5".                     CEEIGZCT
00368  88  CEE35S        VALUE X"00010CBC49C3C5C5".                     CEEIGZCT
00369  88  CEE35T        VALUE X"00010CBD49C3C5C5".                     CEEIGZCT
00370  88  CEE35U        VALUE X"00010CBE49C3C5C5".                     CEEIGZCT
00371  88  CEE35V        VALUE X"00040CBF61C3C5C5".                     CEEIGZCT
00372  88  CEE360        VALUE X"00030CC059C3C5C5".                     CEEIGZCT
00373  88  CEE36S        VALUE X"00010CDC49C3C5C5".                     CEEIGZCT
00374  88  CEE36T        VALUE X"00040CDD61C3C5C5".                     CEEIGZCT
00375  88  CEE36U        VALUE X"00020CDE51C3C5C5".                     CEEIGZCT
00376  88  CEE36V        VALUE X"00020CDF51C3C5C5".                     CEEIGZCT
00377  88  CEE370        VALUE X"00020CE051C3C5C5".                     CEEIGZCT
00378  88  CEE371        VALUE X"00020CE151C3C5C5".                     CEEIGZCT
00379  88  CEE372        VALUE X"00020CE251C3C5C5".                     CEEIGZCT
00380  88  CEE373        VALUE X"00020CE351C3C5C5".                     CEEIGZCT
00381  88  CEE374        VALUE X"00020CE451C3C5C5".                     CEEIGZCT
00382  88  CEE375        VALUE X"00020CE551C3C5C5".                     CEEIGZCT
00383  88  CEE38M        VALUE X"00030D1659C3C5C5".                     CEEIGZCT
00384  88  CEE38N        VALUE X"00030D1759C3C5C5".                     CEEIGZCT
00385  88  CEE38O        VALUE X"00020D1851C3C5C5".                     CEEIGZCT
00386  88  CEE38P        VALUE X"00030D1959C3C5C5".                     CEEIGZCT
00387  88  CEE38Q        VALUE X"00030D1A59C3C5C5".                     CEEIGZCT
00388  88  CEE38R        VALUE X"00030D1B59C3C5C5".                     CEEIGZCT
00389  88  CEE38S        VALUE X"00030D1C59C3C5C5".                     CEEIGZCT
00390  88  CEE38T        VALUE X"00030D1D59C3C5C5".                     CEEIGZCT
00391  88  CEE38U        VALUE X"00020D1E51C3C5C5".                     CEEIGZCT
00392  88  CEE390        VALUE X"00030D2059C3C5C5".                     CEEIGZCT
00393  88  CEE391        VALUE X"00010D2149C3C5C5".                     CEEIGZCT
00394  88  CEE392        VALUE X"00030D2259C3C5C5".                     CEEIGZCT
00395  88  CEE393        VALUE X"00030D2359C3C5C5".                     CEEIGZCT
00396  88  CEE394        VALUE X"00010D2449C3C5C5".                     CEEIGZCT
00397  88  CEE395        VALUE X"00030D2559C3C5C5".                     CEEIGZCT
00398  88  CEE39A        VALUE X"00010D2A49C3C5C5".                     CEEIGZCT
00399  88  CEE39K        VALUE X"00010D3449C3C5C5".                     CEEIGZCT
00400  88  CEE3A8        VALUE X"00010D4849C3C5C5".                     CEEIGZCT
00401  88  CEE3A9        VALUE X"00010D4949C3C5C5".                     CEEIGZCT
00402  88  CEE3AA        VALUE X"00020D4A51C3C5C5".                     CEEIGZCT
00403  88  CEE3B0        VALUE X"00030D6059C3C5C5".                     CEEIGZCT
00404  88  CEE3B1        VALUE X"00030D6159C3C5C5".                     CEEIGZCT
00405  88  CEE3B2        VALUE X"00030D6259C3C5C5".                     CEEIGZCT
00406  88  CEE3B3        VALUE X"00030D6359C3C5C5".                     CEEIGZCT
00407  88  CEE3B4        VALUE X"00030D6459C3C5C5".                     CEEIGZCT
00408  88  CEE3B5        VALUE X"00030D6559C3C5C5".                     CEEIGZCT
00409  88  CEE3BP        VALUE X"00030D7959C3C5C5".                     CEEIGZCT
00410  88  CEE3BQ        VALUE X"00020D7A51C3C5C5".                     CEEIGZCT
00411  88  CEE3BR        VALUE X"00030D7B59C3C5C5".                     CEEIGZCT
00412  88  CEE3BS        VALUE X"00030D7C59C3C5C5".                     CEEIGZCT
00413  88  CEE3BU        VALUE X"00030D7E59C3C5C5".                     CEEIGZCT
00414  88  CEE3BV        VALUE X"00020D7F51C3C5C5".                     CEEIGZCT
00415  88  CEE3C0        VALUE X"00030D8059C3C5C5".                     CEEIGZCT
00416  88  CEE3C1        VALUE X"00030D8159C3C5C5".                     CEEIGZCT
00417  88  CEE3C3        VALUE X"00030D8359C3C5C5".                     CEEIGZCT
00418  88  CEE3C4        VALUE X"00020D8451C3C5C5".                     CEEIGZCT
00419  88  CEE3C5        VALUE X"00020D8551C3C5C5".                     CEEIGZCT
00420  88  CEE3C6        VALUE X"00020D8651C3C5C5".                     CEEIGZCT
00421  88  CEE3C7        VALUE X"00020D8751C3C5C5".                     CEEIGZCT
00422  88  CEE3C8        VALUE X"00020D8851C3C5C5".                     CEEIGZCT
00423  88  CEE3C9        VALUE X"00020D8951C3C5C5".                     CEEIGZCT
00424  88  CEE3CA        VALUE X"00020D8A51C3C5C5".                     CEEIGZCT
00425  88  CEE3CB        VALUE X"00020D8B51C3C5C5".                     CEEIGZCT
00426  88  CEE3CC        VALUE X"00020D8C51C3C5C5".                     CEEIGZCT
00427  88  CEE3CD        VALUE X"00020D8D51C3C5C5".                     CEEIGZCT
00428  88  CEE3CE        VALUE X"00020D8E51C3C5C5".                     CEEIGZCT
00429  88  CEE3CF        VALUE X"00020D8F51C3C5C5".                     CEEIGZCT
00430  88  CEE3CG        VALUE X"00030D9059C3C5C5".                     CEEIGZCT
00431  88  CEE3CH        VALUE X"00030D9159C3C5C5".                     CEEIGZCT
00432  88  CEE3CJ        VALUE X"00030D9359C3C5C5".                     CEEIGZCT
00433  88  CEE3CK        VALUE X"00030D9459C3C5C5".                     CEEIGZCT
00434  88  CEE3CO        VALUE X"00030D9859C3C5C5".                     CEEIGZCT
00435  88  CEE3CP        VALUE X"00030D9959C3C5C5".                     CEEIGZCT
00436  88  CEE3CQ        VALUE X"00030D9A59C3C5C5".                     CEEIGZCT
00437  88  CEE3CT        VALUE X"00030D9D59C3C5C5".                     CEEIGZCT
00438  88  CEE3CU        VALUE X"00030D9E59C3C5C5".                     CEEIGZCT
00439  88  CEE3CV        VALUE X"00030D9F59C3C5C5".                     CEEIGZCT
00440  88  CEE3D0        VALUE X"00030DA059C3C5C5".                     CEEIGZCT
00441  88  CEE3D1        VALUE X"00030DA159C3C5C5".                     CEEIGZCT
00442  88  CEE3D2        VALUE X"00030DA259C3C5C5".                     CEEIGZCT
00443  88  CEE3D3        VALUE X"00030DA359C3C5C5".                     CEEIGZCT
00444  88  CEE3D4        VALUE X"00030DA459C3C5C5".                     CEEIGZCT
00445  88  CEE3D5        VALUE X"00010DA549C3C5C5".                     CEEIGZCT
00446  88  CEE3D6        VALUE X"00030DA659C3C5C5".                     CEEIGZCT
00447  88  CEE3D7        VALUE X"00030DA759C3C5C5".                     CEEIGZCT
00448  88  CEE3D8        VALUE X"00000DA841C3C5C5".                     CEEIGZCT
00449  88  CEE3D9        VALUE X"00020DA951C3C5C5".                     CEEIGZCT
00450  88  CEE3DA        VALUE X"00000DAA41C3C5C5".                     CEEIGZCT
00451  88  CEE3DB        VALUE X"00020DAB51C3C5C5".                     CEEIGZCT
00452  88  CEE3DC        VALUE X"00030DAC59C3C5C5".                     CEEIGZCT
00453  88  CEE3DD        VALUE X"00030DAD59C3C5C5".                     CEEIGZCT
00454  88  CEE3DE        VALUE X"00030DAE59C3C5C5".                     CEEIGZCT
00455  88  CEE3DF        VALUE X"00030DAF59C3C5C5".                     CEEIGZCT
00456  88  CEE3DG        VALUE X"00030DB059C3C5C5".                     CEEIGZCT
00457  88  CEE3DH        VALUE X"00030DB159C3C5C5".                     CEEIGZCT
00458  88  CEE3DI        VALUE X"00030DB259C3C5C5".                     CEEIGZCT
00459  88  CEE3DJ        VALUE X"00030DB359C3C5C5".                     CEEIGZCT
00460  88  CEE3DK        VALUE X"00030DB459C3C5C5".                     CEEIGZCT
00461  88  CEE3DL        VALUE X"00030DB559C3C5C5".                     CEEIGZCT
00462  88  CEE3DM        VALUE X"00030DB659C3C5C5".                     CEEIGZCT
00463  88  CEE3DN        VALUE X"00030DB759C3C5C5".                     CEEIGZCT
00464  88  CEE3DP        VALUE X"00030DB959C3C5C5".                     CEEIGZCT
00465  88  CEE3DQ        VALUE X"00040DBA61C3C5C5".                     CEEIGZCT
00466  88  CEE3DR        VALUE X"00000DBB41C3C5C5".                     CEEIGZCT
00467  88  CEE3DS        VALUE X"00000DBC41C3C5C5".                     CEEIGZCT
00468  88  CEE3GH        VALUE X"00000E1141C3C5C5".                     CEEIGZCT
00469  88  CEE3GI        VALUE X"00000E1241C3C5C5".                     CEEIGZCT
00470  88  CEE3GJ        VALUE X"00000E1341C3C5C5".                     CEEIGZCT
00471  88  CEE3GK        VALUE X"00000E1441C3C5C5".                     CEEIGZCT
00472  88  CEE3GL        VALUE X"00000E1541C3C5C5".                     CEEIGZCT
00473  88  CEE3GM        VALUE X"00000E1641C3C5C5".                     CEEIGZCT
00474  88  CEE3GN        VALUE X"00000E1741C3C5C5".                     CEEIGZCT
00475  88  CEE3GO        VALUE X"00000E1841C3C5C5".                     CEEIGZCT
00476  88  CEE3GP        VALUE X"00000E1941C3C5C5".                     CEEIGZCT
00477  88  CEE3GQ        VALUE X"00000E1A41C3C5C5".                     CEEIGZCT
00478  88  CEE3GR        VALUE X"00000E1B41C3C5C5".                     CEEIGZCT
00479  88  CEE3GS        VALUE X"00000E1C41C3C5C5".                     CEEIGZCT
00480  88  CEE3GT        VALUE X"00000E1D41C3C5C5".                     CEEIGZCT
00481  88  CEE3GU        VALUE X"00000E1E41C3C5C5".                     CEEIGZCT
00482  88  CEE3GV        VALUE X"00000E1F41C3C5C5".                     CEEIGZCT
00483  88  CEE3H0        VALUE X"00000E2041C3C5C5".                     CEEIGZCT
00484  88  CEE3H1        VALUE X"00000E2141C3C5C5".                     CEEIGZCT
00485  88  CEE3H2        VALUE X"00000E2241C3C5C5".                     CEEIGZCT
00486  88  CEE3H3        VALUE X"00000E2341C3C5C5".                     CEEIGZCT
00487  88  CEE3H4        VALUE X"00000E2441C3C5C5".                     CEEIGZCT
00488  88  CEE3H5        VALUE X"00000E2541C3C5C5".                     CEEIGZCT
00489  88  CEE3H6        VALUE X"00000E2641C3C5C5".                     CEEIGZCT
00490  88  CEE3H7        VALUE X"00000E2741C3C5C5".                     CEEIGZCT
00491  88  CEE3H8        VALUE X"00000E2841C3C5C5".                     CEEIGZCT
00492  88  CEE3H9        VALUE X"00000E2941C3C5C5".                     CEEIGZCT
00493  88  CEE3HA        VALUE X"00000E2A41C3C5C5".                     CEEIGZCT
00494  88  CEE3HB        VALUE X"00000E2B41C3C5C5".                     CEEIGZCT
00495  88  CEE3HC        VALUE X"00000E2C41C3C5C5".                     CEEIGZCT
00496  88  CEE3HD        VALUE X"00000E2D41C3C5C5".                     CEEIGZCT
00497  88  CEE3HE        VALUE X"00000E2E41C3C5C5".                     CEEIGZCT
00498  88  CEE3HF        VALUE X"00000E2F41C3C5C5".                     CEEIGZCT
00499  88  CEE3HG        VALUE X"00000E3041C3C5C5".                     CEEIGZCT
00500  88  CEE3HH        VALUE X"00010E3149C3C5C5".                     CEEIGZCT
00501  88  CEE3HI        VALUE X"00000E3241C3C5C5".                     CEEIGZCT
00502  88  CEE3HJ        VALUE X"00000E3341C3C5C5".                     CEEIGZCT
00503  88  CEE3HK        VALUE X"00000E3441C3C5C5".                     CEEIGZCT
00504  88  CEE3HL        VALUE X"00000E3541C3C5C5".                     CEEIGZCT
00505  88  CEE3HM        VALUE X"00000E3641C3C5C5".                     CEEIGZCT
00506  88  CEE3HN        VALUE X"00000E3741C3C5C5".                     CEEIGZCT
00507  88  CEE3HO        VALUE X"00010E3849C3C5C5".                     CEEIGZCT
00508  88  CEE3I1        VALUE X"00010E4149C3C5C5".                     CEEIGZCT
00509  88  CEE3I2        VALUE X"00000E4241C3C5C5".                     CEEIGZCT
00510  88  CEE3I3        VALUE X"00000E4341C3C5C5".                     CEEIGZCT
00511  88  CEE3I4        VALUE X"00000E4441C3C5C5".                     CEEIGZCT
00512  88  CEE3I5        VALUE X"00000E4541C3C5C5".                     CEEIGZCT
00513  88  CEE3I6        VALUE X"00000E4641C3C5C5".                     CEEIGZCT
00514  88  CEE3I7        VALUE X"00000E4741C3C5C5".                     CEEIGZCT
00515  88  CEE3I8        VALUE X"00000E4841C3C5C5".                     CEEIGZCT
00516  88  CEE3I9        VALUE X"00000E4941C3C5C5".                     CEEIGZCT
00517  88  CEE3IA        VALUE X"00000E4A41C3C5C5".                     CEEIGZCT
00518  88  CEE3IB        VALUE X"00000E4B41C3C5C5".                     CEEIGZCT
00519  88  CEE3IC        VALUE X"00000E4C41C3C5C5".                     CEEIGZCT
00520  88  CEE3ID        VALUE X"00000E4D41C3C5C5".                     CEEIGZCT
00521  88  CEE3IE        VALUE X"00000E4E41C3C5C5".                     CEEIGZCT
00522  88  CEE3IF        VALUE X"00000E4F41C3C5C5".                     CEEIGZCT
00523  88  CEE3IG        VALUE X"00000E5041C3C5C5".                     CEEIGZCT
00524  88  CEE3IH        VALUE X"00000E5141C3C5C5".                     CEEIGZCT
00525  88  CEE3II        VALUE X"00000E5241C3C5C5".                     CEEIGZCT
00526  88  CEE3IJ        VALUE X"00000E5341C3C5C5".                     CEEIGZCT
00527  88  CEE3IK        VALUE X"00000E5441C3C5C5".                     CEEIGZCT
00528  88  CEE3IL        VALUE X"00000E5541C3C5C5".                     CEEIGZCT
00529  88  CEE3IM        VALUE X"00000E5641C3C5C5".                     CEEIGZCT
00530  88  CEE3IN        VALUE X"00000E5741C3C5C5".                     CEEIGZCT
00531  88  CEE3IO        VALUE X"00000E5841C3C5C5".                     CEEIGZCT
00532  88  CEE3IP        VALUE X"00000E5941C3C5C5".                     CEEIGZCT
00533  88  CEE3IQ        VALUE X"00000E5A41C3C5C5".                     CEEIGZCT
00534  88  CEE3IR        VALUE X"00000E5B41C3C5C5".                     CEEIGZCT
00535  88  CEE3IS        VALUE X"00000E5C41C3C5C5".                     CEEIGZCT
00536  88  CEE3IT        VALUE X"00000E5D41C3C5C5".                     CEEIGZCT
00537  88  CEE3IU        VALUE X"00000E5E41C3C5C5".                     CEEIGZCT
00538  88  CEE3IV        VALUE X"00000E5F41C3C5C5".                     CEEIGZCT
00539  88  CEE3J0        VALUE X"00000E6041C3C5C5".                     CEEIGZCT
00540  88  CEE3J1        VALUE X"00000E6141C3C5C5".                     CEEIGZCT
00541  88  CEE3J2        VALUE X"00000E6241C3C5C5".                     CEEIGZCT
00542  88  CEE3J3        VALUE X"00000E6341C3C5C5".                     CEEIGZCT
00543  88  CEE3J4        VALUE X"00000E6441C3C5C5".                     CEEIGZCT
00544  88  CEE3J5        VALUE X"00000E6541C3C5C5".                     CEEIGZCT
00545  88  CEE3JK        VALUE X"00000E7441C3C5C5".                     CEEIGZCT
00546  88  CEE3MO        VALUE X"00030ED859C3C5C5".                     CEEIGZCT
00547  88  CEE3PS        VALUE X"00030F3C59C3C5C5".                     CEEIGZCT
00548  88  CEE3PT        VALUE X"00030F3D59C3C5C5".                     CEEIGZCT
00549  88  CEE3T1        VALUE X"00030FA159C3C5C5".                     CEEIGZCT
00550  88  CEE3TF        VALUE X"00030FAF59C3C5C5".                     CEEIGZCT
00551  88  CEE3VM        VALUE X"00030FF659C3C5C5".                     CEEIGZCT
00552  88  CEE4S9        VALUE X"0003138959C3C5C5".                     CEEIGZCT
00553  88  CEE4SA        VALUE X"0003138A59C3C5C5".                     CEEIGZCT
00554  88  CEE4VD        VALUE X"000413ED61C3C5C5".                     CEEIGZCT
00555  88  CEE4VE        VALUE X"000213EE51C3C5C5".                     CEEIGZCT
00556  88  CEE4VF        VALUE X"000113EF49C3C5C5".                     CEEIGZCT
00557  88  CEE4VG        VALUE X"000313F059C3C5C5".                     CEEIGZCT
00558  88  CEE4VH        VALUE X"000313F159C3C5C5".                     CEEIGZCT
00559  88  CEE4VI        VALUE X"000313F259C3C5C5".                     CEEIGZCT
00560  88  CEE50V        VALUE X"0003141F59C3C5C5".                     CEEIGZCT
00561  88  CEE510        VALUE X"0003142059C3C5C5".                     CEEIGZCT
00562  88  CEE512        VALUE X"0003142259C3C5C5".                     CEEIGZCT
00563  88  CEE513        VALUE X"0003142359C3C5C5".                     CEEIGZCT
00564  88  CEE519        VALUE X"0003142959C3C5C5".                     CEEIGZCT
00565  88  CEE51A        VALUE X"0003142A59C3C5C5".                     CEEIGZCT
00566  88  CEE51O        VALUE X"0003143859C3C5C5".                     CEEIGZCT
00567  88  CEE51P        VALUE X"0003143959C3C5C5".                     CEEIGZCT
00568  88  CEE51Q        VALUE X"0003143A59C3C5C5".                     CEEIGZCT
00569  88  CEE51R        VALUE X"0003143B59C3C5C5".                     CEEIGZCT
00570  88  CEE51S        VALUE X"0000143C41C3C5C5".                     CEEIGZCT
00571  88  CEE52H        VALUE X"0003145159C3C5C5".                     CEEIGZCT
00572  88  CEE52I        VALUE X"0003145259C3C5C5".                     CEEIGZCT
00573  88  CEE52J        VALUE X"0003145359C3C5C5".                     CEEIGZCT
00574  88  CEE52K        VALUE X"0003145459C3C5C5".                     CEEIGZCT
00575  88  CEE52L        VALUE X"0003145559C3C5C5".                     CEEIGZCT
00576  88  CEE52M        VALUE X"0003145659C3C5C5".                     CEEIGZCT
00577  88  CEE52N        VALUE X"0002145751C3C5C5".                     CEEIGZCT
00578  88  CEE52O        VALUE X"0003145859C3C5C5".                     CEEIGZCT
00579  88  CEE52P        VALUE X"0003145959C3C5C5".                     CEEIGZCT
00580  88  CEE52Q        VALUE X"0003145A59C3C5C5".                     CEEIGZCT
00581  88  CEE52R        VALUE X"0003145B59C3C5C5".                     CEEIGZCT
00582  88  CEE52S        VALUE X"0004145C61C3C5C5".                     CEEIGZCT
00583  88  CEE52T        VALUE X"0003145D59C3C5C5".                     CEEIGZCT
00584  88  CEE52U        VALUE X"0003145E59C3C5C5".                     CEEIGZCT
00585  88  CEE52V        VALUE X"0001145F49C3C5C5".                     CEEIGZCT
00586  88  CEE530        VALUE X"0001146049C3C5C5".                     CEEIGZCT
00587  88  CEE531        VALUE X"0003146159C3C5C5".                     CEEIGZCT
00588  88  CEE532        VALUE X"0003146259C3C5C5".                     CEEIGZCT
00589  88  CEE533        VALUE X"0001146349C3C5C5".                     CEEIGZCT
00590  88  CEE534        VALUE X"0003146459C3C5C5".                     CEEIGZCT
00591  88  CEE535        VALUE X"0003146559C3C5C5".                     CEEIGZCT
00592  88  CEE536        VALUE X"0003146659C3C5C5".                     CEEIGZCT
00593  88  CEE537        VALUE X"0001146749C3C5C5".                     CEEIGZCT
00594  88  CEE538        VALUE X"0001146849C3C5C5".                     CEEIGZCT
00595  88  CEE539        VALUE X"0003146959C3C5C5".                     CEEIGZCT
00596  88  CEE53A        VALUE X"0003146A59C3C5C5".                     CEEIGZCT
00597  88  CEE53B        VALUE X"0003146B59C3C5C5".                     CEEIGZCT
00598  88  CEE53C        VALUE X"0003146C59C3C5C5".                     CEEIGZCT
00599  88  CEE53D        VALUE X"0001146D49C3C5C5".                     CEEIGZCT
00600  88  CEE53E        VALUE X"0003146E59C3C5C5".                     CEEIGZCT
00601  88  CEE53F        VALUE X"0003146F59C3C5C5".                     CEEIGZCT
00602  88  CEE53G        VALUE X"0003147059C3C5C5".                     CEEIGZCT
00603  88  CEE53H        VALUE X"0003147159C3C5C5".                     CEEIGZCT
00604  88  CEE55L        VALUE X"000314B559C3C5C5".                     CEEIGZCT
00605  88  CEE55M        VALUE X"000314B659C3C5C5".                     CEEIGZCT
00606  88  CEE58P        VALUE X"0003151959C3C5C5".                     CEEIGZCT
00607  88  CEE58Q        VALUE X"0000151A41C3C5C5".                     CEEIGZCT
00608  88  CEE58R        VALUE X"0000151B41C3C5C5".                     CEEIGZCT
00609  88  CEE58S        VALUE X"0000151C41C3C5C5".                     CEEIGZCT
00610  88  CEE58T        VALUE X"0000151D41C3C5C5".                     CEEIGZCT
00611  88  CEE5CM        VALUE X"0003159659C3C5C5".                     CEEIGZCT
00612  88  CEE5CN        VALUE X"0003159759C3C5C5".                     CEEIGZCT
00613  88  CEE5CO        VALUE X"0003159859C3C5C5".                     CEEIGZCT
00614  88  CEE5CP        VALUE X"0003159959C3C5C5".                     CEEIGZCT
00615  88  CEE5CQ        VALUE X"0003159A59C3C5C5".                     CEEIGZCT
00616  88  CEE5CR        VALUE X"0003159B59C3C5C5".                     CEEIGZCT
00617  88  CEE5CS        VALUE X"0003159C59C3C5C5".                     CEEIGZCT
00618  88  CEE5CT        VALUE X"0003159D59C3C5C5".                     CEEIGZCT
00619  88  CEE5DF        VALUE X"000315AF59C3C5C5".                     CEEIGZCT
00620  88  CEE5DG        VALUE X"000315B059C3C5C5".                     CEEIGZCT
00621  88  CEE5DH        VALUE X"000315B159C3C5C5".                     CEEIGZCT
00622  88  CEE5F1        VALUE X"000315E159C3C5C5".                     CEEIGZCT
00623  88  CEE5F2        VALUE X"000315E259C3C5C5".                     CEEIGZCT
00624  88  CEE5F3        VALUE X"000315E359C3C5C5".                     CEEIGZCT
00625  88  CEE5F4        VALUE X"000315E459C3C5C5".                     CEEIGZCT
00626  88  CEE5F5        VALUE X"000315E559C3C5C5".                     CEEIGZCT
00627  88  CEE5F6        VALUE X"000315E659C3C5C5".                     CEEIGZCT
00628  88  CEE5F7        VALUE X"000315E759C3C5C5".                     CEEIGZCT
00629  88  CEE5F8        VALUE X"000315E859C3C5C5".                     CEEIGZCT
00630  88  CEE5F9        VALUE X"000315E959C3C5C5".                     CEEIGZCT
00631  88  CEE5FC        VALUE X"000315EC59C3C5C5".                     CEEIGZCT
00632  88  CEE5FD        VALUE X"000315ED59C3C5C5".                     CEEIGZCT
00633  88  CEE5FQ        VALUE X"000315FA59C3C5C5".                     CEEIGZCT
00634  88  CEE5FR        VALUE X"000315FB59C3C5C5".                     CEEIGZCT
00635  88  CEE5FS        VALUE X"000315FC59C3C5C5".                     CEEIGZCT
00636  88  CEE5FT        VALUE X"000315FD59C3C5C5".                     CEEIGZCT
00637  88  CEE5GJ        VALUE X"0003161359C3C5C5".                     CEEIGZCT
00638  88  CEE5I5        VALUE X"0003164559C3C5C5".                     CEEIGZCT
00639  88  CEE5I6        VALUE X"0003164659C3C5C5".                     CEEIGZCT
00640  88  CEE5I7        VALUE X"0003164759C3C5C5".                     CEEIGZCT
00641  88  CEE5I8        VALUE X"0004164861C3C5C5".                     CEEIGZCT
00642  88  CEE5I9        VALUE X"0003164959C3C5C5".                     CEEIGZCT
00643  88  CEE5IA        VALUE X"0003164A59C3C5C5".                     CEEIGZCT
00644  88  CEE5IB        VALUE X"0000164B41C3C5C5".                     CEEIGZCT
00645  88  CEE5IC        VALUE X"0003164C59C3C5C5".                     CEEIGZCT
00646  88  CEE5ID        VALUE X"0003164D59C3C5C5".                     CEEIGZCT
00647  88  CEE5IE        VALUE X"0003164E59C3C5C5".                     CEEIGZCT
00648  88  CEE5IF        VALUE X"0003164F59C3C5C5".                     CEEIGZCT
00649  88  CEE5IG        VALUE X"0003165059C3C5C5".                     CEEIGZCT
00650  88  CEE5IH        VALUE X"0003165159C3C5C5".                     CEEIGZCT
00651  88  CEE5II        VALUE X"0003165259C3C5C5".                     CEEIGZCT
00652  88  CEE5IJ        VALUE X"0003165359C3C5C5".                     CEEIGZCT
00653  88  CEE5IK        VALUE X"0004165461C3C5C5".                     CEEIGZCT
00654  88  CEE5IL        VALUE X"0004165561C3C5C5".                     CEEIGZCT
00655  88  CEE5IM        VALUE X"0004165661C3C5C5".                     CEEIGZCT
00656  88  CEE5IN        VALUE X"0003165759C3C5C5".                     CEEIGZCT
00657  88  CEE5IO        VALUE X"0004165861C3C5C5".                     CEEIGZCT
00658  88  CEE5IU        VALUE X"0003165E59C3C5C5".                     CEEIGZCT
00659  88  CEE5IV        VALUE X"0003165F59C3C5C5".                     CEEIGZCT
00660  88  CEE5J0        VALUE X"0004166061C3C5C5".                     CEEIGZCT
00661  88  CEE5J1        VALUE X"0003166159C3C5C5".                     CEEIGZCT
00662  88  CEE5J2        VALUE X"0003166259C3C5C5".                     CEEIGZCT
00663  88  CEE5J3        VALUE X"0003166359C3C5C5".                     CEEIGZCT
00664  88  CEE5J4        VALUE X"0003166459C3C5C5".                     CEEIGZCT
00665  88  CEE5J5        VALUE X"0003166559C3C5C5".                     CEEIGZCT
00666  88  CEE5J6        VALUE X"0003166659C3C5C5".                     CEEIGZCT
00667  88  CEE5J7        VALUE X"0003166759C3C5C5".                     CEEIGZCT
00668  88  CEE5J8        VALUE X"0000166841C3C5C5".                     CEEIGZCT
00669  88  CEE5J9        VALUE X"0003166959C3C5C5".                     CEEIGZCT
00670  88  CEE5JA        VALUE X"0003166A59C3C5C5".                     CEEIGZCT
00671  88  CEE5JB        VALUE X"0003166B59C3C5C5".                     CEEIGZCT
00672  88  CEE5JC        VALUE X"0003166C59C3C5C5".                     CEEIGZCT
00673  88  CEE5JD        VALUE X"0004166D61C3C5C5".                     CEEIGZCT
00674  88  CEE5JE        VALUE X"0003166E59C3C5C5".                     CEEIGZCT
00675  88  CEE5JF        VALUE X"0003166F59C3C5C5".                     CEEIGZCT
00676  88  CEE5JG        VALUE X"0003167059C3C5C5".                     CEEIGZCT
00677  88  CEE5JH        VALUE X"0004167161C3C5C5".                     CEEIGZCT
00678  88  CEE5JI        VALUE X"0003167259C3C5C5".                     CEEIGZCT
00679  88  CEE5JJ        VALUE X"0003167359C3C5C5".                     CEEIGZCT
00680  88  CEE5JK        VALUE X"0003167459C3C5C5".                     CEEIGZCT
00681  88  CEE5JL        VALUE X"0003167559C3C5C5".                     CEEIGZCT
00682  88  CEE5JM        VALUE X"0003167659C3C5C5".                     CEEIGZCT
00683  88  CEE5JN        VALUE X"0004167761C3C5C5".                     CEEIGZCT
00684  88  CEE5K1        VALUE X"0004168161C3C5C5".                     CEEIGZCT
00685  88  CEE5K2        VALUE X"0004168261C3C5C5".                     CEEIGZCT
00686  88  CEE5K3        VALUE X"0004168361C3C5C5".                     CEEIGZCT
00019          03  CASE-1-CONDITION-ID.                                 DDTRNWS 
00020              04  SEVERITY        PIC S9(4) COMP.                  DDTRNWS 
00021              04  MSG-NO          PIC S9(4) COMP.                  DDTRNWS 
00022          03  CASE-2-CONDITION-ID                                  DDTRNWS 
00023                    REDEFINES CASE-1-CONDITION-ID.                 DDTRNWS 
00024              04  CLASS-CODE      PIC S9(4) COMP.                  DDTRNWS 
00025              04  CAUSE-CODE      PIC S9(4) COMP.                  DDTRNWS 
00026          03  CASE-SEV-CTL    PIC X.                               DDTRNWS 
00027          03  FACILITY-ID     PIC XXX.                             DDTRNWS 
00028      02  I-S-INFO            PIC S9(9) COMP.                      DDTRNWS 
00029                                                                   DDTRNWS 
00810                                                                   P442288 
00811  01  WS-DATE-TIME-WORK-AREAS.                                     P442288 
00812      05  WS-CURRENT-DATE-DT.                                      P442288 
00813          10  MM                    PIC 9(02).                     P442288 
00814          10  FILLER                PIC X     VALUE '/'.           P442288 
00815          10  DD                    PIC 9(02).                     P442288 
00816          10  FILLER                PIC X     VALUE '/'.           P442288 
00817          10  YYYY                  PIC 9(04).                     P442288 
00818      05  WS-CRNT-DATE-DT.                                         P442288 
00819          10  WS-CRNT-DATE-CC       PIC 9(02) VALUE  ZEROS.        P442288 
00820          10  WS-CRNT-DATE-YYMMDD.                                 P442288 
00821              15  WS-CRNT-DATE-YY   PIC 9(02).                     P442288 
00822              15  WS-CRNT-DATE-MM   PIC 9(02).                     P442288 
00823              15  WS-CRNT-DATE-DD   PIC 9(02).                     P442288 
00824      05  FILLER REDEFINES WS-CRNT-DATE-DT.                        P442288 
00825          10  WS-CRNT-DATE-YYYYMM   PIC 9(6).                      P442288 
00826          10  FILLER                PIC 9(2).                      P442288 
00827      05  WS-CRNT-DATE-YYYYMMDD REDEFINES WS-CRNT-DATE-DT          P442288 
00828                                    PIC 9(08).                     P442288 
00829                                                                   P442288 
00830      05  WS-CRNT-DATE-X REDEFINES WS-CRNT-DATE-DT.                P442288 
00831          10  WS-CRNT-DATE-YYYY     PIC 9(04).                     P442288 
00832          10  FILLER                PIC 9(04).                     P442288 
00833      05  WS-CRNT-DATE-YYDDD.                                      P442288 
00834          10  FILLER                PIC 9(02).                     P442288 
00835          10  WS-CRNT-DATE-DDD      PIC 9(03).                     P442288 
00836      05  WS-DAY-OF-WEEK            PIC 9(1).                      P442288 
00837              88  TODAY-IS-SUNDAY             VALUE 1.             P442288 
00838              88  TODAY-IS-MONDAY             VALUE 2.             P442288 
00839              88  TODAY-IS-TUESDAY            VALUE 3.             P442288 
00840              88  TODAY-IS-WEDNESDAY          VALUE 4.             P442288 
00841              88  TODAY-IS-THURSDAY           VALUE 5.             P442288 
00842              88  TODAY-IS-FRIDAY             VALUE 6.             P442288 
00843              88  TODAY-IS-SATURDAY           VALUE 7.             P442288 
00844      05  WS-DAYS-NAME.                                            P442288 
00845          07  FILLER-X-SUNDAY       PIC X(4) VALUE 'SUN'.          P442288 
00846          07  FILLER-X-MONDAY       PIC X(4) VALUE 'MON'.          P442288 
00847          07  FILLER-X-TUESDAY      PIC X(4) VALUE 'TUES'.         P442288 
00848          07  FILLER-X-WEDNESDAY    PIC X(4) VALUE 'WEDS'.         P442288 
00849          07  FILLER-X-THURSDAY     PIC X(4) VALUE 'THUR'.         P442288 
00850          07  FILLER-X-FRIDAY       PIC X(4) VALUE 'FRI'.          P442288 
00851          07  FILLER-X-SATURDAY     PIC X(4) VALUE 'SAT'.          P442288 
00852      05  FILLER REDEFINES WS-DAYS-NAME.                           P442288 
00853          07  WS-DAYS-TABLE         PIC X(4) OCCURS 7 TIMES.       P442288 
00854      05  WS-PREV-YEAR-REG-EXP-DT           PIC 9(6)  VALUE ZERO.  P442288 
00855      05  FILLER REDEFINES WS-PREV-YEAR-REG-EXP-DT.                P442288 
00856          10  WS-PREV-YEAR-REG-EXP-DT-YYYY  PIC 9(4).              P442288 
00857          10  WS-PREV-YEAR-REG-EXP-DT-MM    PIC 9(2).              P442288 
00858      05  WS-CRNT-REG-UPD-DT                PIC 9(6)  VALUE ZERO.  P442288 
00859      05  FILLER REDEFINES WS-CRNT-REG-UPD-DT.                     P442288 
00860          10  WS-CRNT-REG-UPD-DT-YYYY       PIC 9(4).              P442288 
00861          10  WS-CRNT-REG-UPD-DT-MM         PIC 9(2).              P442288 
00862 *---------------------------------------------------------------* P442288 
00863 *      THIS IS 1 MONTH PRIOR TO THE WS-REGIS-RTS-REG-EXP-DT     *    CL**9
00864 *      -OR- 1 MONTH PRIOR TO THE END OF THE REGISTRATION.       * P442288 
00865 *        IT WAS THOUGHT THAT ATTEMPTED "IRPRNW" BEFORE THIS DATE* P442288 
00866 *        WOULD RESULT IN RTS EDIT ERROR, SO THE EXISTING        * P442288 
00867 *        REGISTRATION IS CANCELLED (IRPIVD) PRIOR TO ATTEMPTING * P442288 
00868 *        AN IRPREN TO EXTEND THE REGISTRATION                   * P442288 
00869 *---------------------------------------------------------------* P442288 
00870      05  WS-MIN-BEGIN-REG-DT               PIC 9(6)  VALUE ZERO.  P442288 
00871      05  FILLER REDEFINES WS-MIN-BEGIN-REG-DT.                    P442288 
00872          10  WS-MIN-BEGIN-REG-DT-YYYY      PIC 9(4).              P442288 
00873          10  WS-MIN-BEGIN-REG-DT-MM        PIC 9(2).              P442288 
00874      05  WS-DAYS-TO-01-01-1900     PIC 9(07) VALUE 693960.        P442288 
00875      05  WS-DAYS-SINCE-01-01-0000  PIC 9(07) VALUE ZERO.          P442288 
00876      05  WS-DAYS-SINCE-01-01-1900  PIC 9(07) VALUE ZERO.          P442288 
00877                                                                   P442288 
00878      05  WS-TIME-A.                                               P442288 
00879          10  WS-TIME               PIC 9(06).                     P442288 
00880          10  FILLER                PIC 9(02).                     P442288 
00881                                                                   P442288 
00882      05  WS-DAY-MON-TABLE.                                        P442288 
00883          10  FILLER-JAN-31         PIC S9(4) COMP VALUE +000.     P442288 
00884          10  FILLER-FEB-28         PIC S9(4) COMP VALUE +031.     P442288 
00885          10  FILLER-MAR-31         PIC S9(4) COMP VALUE +059.     P442288 
00886          10  FILLER-APR-30         PIC S9(4) COMP VALUE +090.     P442288 
00887          10  FILLER-MAY-31         PIC S9(4) COMP VALUE +120.     P442288 
00888          10  FILLER-JUN-30         PIC S9(4) COMP VALUE +151.     P442288 
00889          10  FILLER-JUL-31         PIC S9(4) COMP VALUE +181.     P442288 
00890          10  FILLER-AUG-31         PIC S9(4) COMP VALUE +212.     P442288 
00891          10  FILLER-SEP-30         PIC S9(4) COMP VALUE +243.     P442288 
00892          10  FILLER-OCT-31         PIC S9(4) COMP VALUE +273.     P442288 
00893          10  FILLER-NOV-30         PIC S9(4) COMP VALUE +304.     P442288 
00894          10  FILLER-DEC-31         PIC S9(4) COMP VALUE +334.     P442288 
00895      05  FILLER REDEFINES WS-DAY-MON-TABLE.                       P442288 
00896          10  WS-DAYS-TO-MONTH     PIC S9(4) COMP OCCURS 12 TIMES. P442288 
00897 *---------------------------------------------------------------* P442288 
00898 *    05  WS-TODAYS-DAY-NO          PIC S9(4) COMP VALUE +0.     * P442288 
00899 *    05  WS-BATCHES-MONTH-NO       PIC S9(4) COMP VALUE +0.     * P442288 
00900 *    05  WS-BATCHES-DAY-NO         PIC S9(4) COMP VALUE +0.     * P442288 
00901 *    05  WS-WORKING-DAY-NO         PIC S9(4) COMP VALUE +0.     * P442288 
00902 *    05  WS-BATCHES-AGE            PIC S9(4) COMP VALUE +0.     * P442288 
00903 *    05  WS-MIN-BATCH-AGE          PIC S9(4) COMP VALUE +3.     * P442288 
00904 *---------------------------------------------------------------* P442288 
00905      05  WS-TODAYS-DAY-NO          PIC  9(4)  VALUE 0.            P442288 
00906      05  WS-BATCHES-MONTH-NO       PIC  9(4)  VALUE 0.            P442288 
00907      05  WS-WORKING-DAY-NO         PIC  9(4)  VALUE 0.            P442288 
00908      05  WS-BATCHES-DAY-NO         PIC  9(4)  VALUE 0.            P442288 
00909      05  WS-BATCHES-AGE            PIC  S9(4)  VALUE 0.           P442288 
00910      05  WS-BATCHES-AGE-D          PIC  -9(4).                    P442288 
00911      05  WS-MIN-BATCH-AGE          PIC  9(4)  VALUE 0.            P442288 
00912      05  WS-MAX-INTERFACE-ATTEMPTS PIC  9(2)  COMP VALUE 0.       P442288 
00913      05  WS-BATCH-ADDED-DATE.                                     P442288 
00914          10  WS-BATCH-ADDED-YYYY       PIC 9(4).                  P442288 
00915          10  WS-BATCH-ADDED-MONTH      PIC 99.                    P442288 
00916          10  WS-BATCH-ADDED-DAY        PIC 99.                    P442288 
00917      05  WS-MISC-NUM                   PIC 9(4).                  P442288 
00918      05  WS-LEAP-FLAG              PIC 9.                         P442288 
00919          88  WS-LEAP-YEAR              VALUE 0.                   P442288 
00920          88  WS-LAST-YEAR-LEAP         VALUE 3.                   P442288 
00921                                                                   P442288 
00922  01  WS-TRANS-ERR-DIAG.                                           P442288 
00923      02  WS-TRANS-KEY.                                            P442288 
00924          05  WS-TRANSID.                                          P442288 
00925              10  WS-OFCISSUANCENO      PIC 9(03).                 P442288 
00926              10  WS-TRANSWSID          PIC 9(03).                 P442288 
00927              10  WS-TR-DATE-TIME.                                 P442288 
00928                  15  WS-TRANSAMDATE    PIC 9(05).                 P442288 
00929                  15  WS-TRANSTIME      PIC 9(06).                 P442288 
00930      02  FILLER                        PIC X   VALUE SPACE.       P442288 
00931      02  WS-TRANS-DIAG                 PIC X(132).                P442288 
00932                                                                   P442288 
00933  01  WS-TRANS-PREV-KEY.                                           P442288 
00934        05  FILLER                  PIC 9(03) VALUE ZERO.          P442288 
00935        05  WS-PREV-TRANSWSID       PIC 9(03) VALUE ZERO.          P442288 
00936        05  WS-PREV-TR-DATE-TIME.                                  P442288 
00937            10  WS-PREV-TR-DATE     PIC 9(05) VALUE ZERO.          P442288 
00938            10  WS-PREV-TR-TIME     PIC 9(06) VALUE ZERO.          P442288 
00939                                                                   P442288 
00940  01  WS-TRANS-VALUES.                                             P442288 
00941      05  WS-TRANSCD                PIC X(06).                     P442288 
00942 *---------------------------------------------------------------* P442288 
00943 *       WS-RTS-DOCNO IS SET IN Z-2-READ-IRP-RTS-FILE            * P442288 
00944 *---------------------------------------------------------------* P442288 
00945      05  WS-RTS-DOCNO.                                            P442288 
00946          10  FILLER                PIC  X(6).                     P442288 
00947          10  WS-RTS-DOCNO-AMDATE   PIC  9(5).                     P442288 
00948          10  WS-RTS-DOCNO-AMTIME   PIC  9(6).                     P442288 
00949      05  WS-RTS-DOCNO-NUM REDEFINES WS-RTS-DOCNO                  P442288 
00950                                    PIC  9(17).                    P442288 
00951      05  WS-OFCISSUANCECD          PIC 9(01).                     P442288 
00952      05  WS-RGN-OFFICE-FLAG        PIC X     VALUE '*'.           P442288 
00953 *            USE-SUPPLEMENT-BILL-REGION      VALUE 'S'.         * P442288 
00954 *            USE-ACCT-NAME-ADDR-REGION       VALUE 'A'.         * P442288 
00955 *            USE-BATCH-CHANGE-REGION         VALUE 'B'.         * P442288 
00956 *            USE-EXPLORE-P442288F-DATA       VALUE 'X'.         * P442288 
00957 *---------------------------------------------------------------* P442288 
00958      05  WS-RGN-OFFICE             PIC X(02) VALUE '..'.          P442288 
00959      05  WS-RGN-OFC-BILLING REDEFINES WS-RGN-OFFICE PIC X(02).    P442288 
00960      05  WS-RGN-OFC-ISS-NO         PIC 9(3)  VALUE ZERO.          P442288 
00961 *---------------------------------------------------------------* P442288 
00962 *        WS-RGN-OFC-ISS PREFIX FIELDS PREVIOUSLY USED IN        * P442288 
00963 *          21138-FIND-RGN-OFC-ISS-NO - NO LONGER NEEDED         * P442288 
00964 *---------------------------------------------------------------* P442288 
00965 *    05  WS-RGN-OFC-ISS-PREV       PIC X(02) VALUE '..'.        * P442288 
00966 *    05  WS-RGN-OFC-ISS-CRNT       PIC X(02) VALUE '..'.        * P442288 
00967 *    05  WS-RGN-OFC-ISS-NO         PIC 9(03) VALUE 296.         * P442288 
00968 *    05  WS-RGN-OFC-ISS-TABLE-NDX  PIC 9(2) COMP VALUE ZERO.    * P442288 
00969 *    05  WS-RGN-OFC-TABLE-NDX      PIC 9(2) COMP VALUE ZERO.    * P442288 
00970 *---------------------------------------------------------------* P442288 
00971 *---------------------------------------------------------------* P442288 
00972      05  WS-REGCLASSCD             PIC 9(03) VALUE 006.           P442288 
00973      05  WS-ACCTITMCD              PIC X(08).                     P442288 
00974      05  WS-ITMCD                  PIC X(08).                     P442288 
00975      05  WS-ITMPRICE               PIC S9(07)V99.                 P442288 
00976      05  WS-CUSTACTULREGFEE        PIC S9(07)V99.                 P442288 
00977      05  WS-INVITMYR               PIC 9(04).                     P442288 
00978      05  WS-REGPLTAGE              PIC 9(04)  VALUE ZERO.         P442288 
00979      05  WS-REGEFFDATE             PIC 9(08).                     P442288 
00980      05  FILLER REDEFINES  WS-REGEFFDATE.                         P442288 
00981          10  WS-REGEFF-DT          PIC 9(06).                     P442288 
00982          10  FILLER REDEFINES WS-REGEFF-DT.                       P442288 
00983              15  WS-REGEFF-DT-YYYY PIC 9(04).                     P442288 
00984              15  WS-REGEFF-DT-MM   PIC 9(02).                     P442288 
00985          10  WS-REGEFFDATE-DD      PIC 9(02).                     P442288 
00986      05  WS-YRMO-RECIPROCAL-LIMITS     PIC 9(6)   VALUE 999912.   P442288 
00987      05  FILLER REDEFINES WS-YRMO-RECIPROCAL-LIMITS.              P442288 
00988          10  WS-YYYY-RECIP             PIC 9(4).                  P442288 
00989          10  WS-MM-RECIP               PIC 99.                    P442288 
00990      05  WS-HOLD-NORML-REGIS-DT        PIC 9(6)   VALUE ZEROS.    P442288 
00991      05  WS-HOLD-REG-INV-DT            PIC 9(6)   VALUE ZEROS.    P442288 
00992                                                                   P442288 
00993      05  WS-EXP-DT-YYYYMMDD.                                      P442288 
00994          10  WS-REGEXPYR           PIC 9(04).                     P442288 
00995          10  WS-REGEXPMO           PIC 9(02) VALUE 03.            P442288 
00996          10  WS-REGEXPDD           PIC 9(02) VALUE 31.            P442288 
00997      05  FILLER REDEFINES WS-EXP-DT-YYYYMMDD.                     P442288 
00998          10  WS-EXP-DT-YYYYMM      PIC 9(06).                     P442288 
00999          10  FILLER                PIC 9(02).                     P442288 
01000      05  WS-CUST-NAME              PIC X(30).                     P442288 
01001 *---------------------------------------------------------------* P442288 
01002 * RTS-REGIS FILE FIELDS DEALING WITH PLT & EXP DT               * P442288 
01003 * REGPLTNO, REGEXPYR, REGEXPMO,  FROM RTS-REGIS      RTS-ARDR   * P442288 
01004 *GR  01  REGPLTID                                               * P442288 
01005 *        02 REGPLTNO  A 7                                       * P442288 
01006 *        02 REGEXPYR  N 4                                       * P442288 
01007 *GR  01  REGDATA                                                * P442288 
01008 *        02 REGEXPMO  N 2                                       * P442288 
01009 *---------------------------------------------------------------* P442288 
01010      05  WS-REGIS-RTS-REGPLTID.                                      CL**9
01011          10  WS-REGIS-RTS-REGPLTNO.                                  CL**9
01012              15  WS-REGIS-RTS-REGPLT5  PIC X(5).                     CL**9
01013              15  FILLER                PIC X(2).                  P442288 
01014          10  WS-REGIS-RTS-REG-EXP-DT   PIC 9(6).                     CL**9
01015          10  FILLER REDEFINES WS-REGIS-RTS-REG-EXP-DT.               CL**9
01016              15  WS-REGIS-RTS-REGEXPYR PIC 9(4).                     CL**9
01017              15  WS-REGIS-RTS-REGEXPMO PIC 9(2).                     CL**9
01018      05  WS-REGIS-RTS-REGCLASSCD       PIC 9(3).                     CL**9
01019      05  WS-REGIS-RTS-REGPLATECD       PIC X(8).                     CL**9
01020      05  WS-REGIS-RTS-REG-GWT          PIC 9(7)  VALUE ZERO.         CL*23
01021      05  WS-REGIS-RTS-BIRTHDATE        PIC 9(6)  VALUE ZERO.         CL**9
01022      05  WS-REGIS-PLTBIRTHDATE         PIC 9(6)  VALUE ZERO.      P442288 
01023      05  FILLER REDEFINES WS-REGIS-PLTBIRTHDATE.                  P442288 
01024          10  WS-PLTBIRTHDATE-YYYY      PIC 9(4).                  P442288 
01025          10  WS-PLTBIRTHDATE-MM        PIC 9(2).                  P442288 
01026      05  WS-REGIS-RTS-PRVPLTNO         PIC X(7).                     CL**9
01027      05  WS-REGIS-RTS-PRVEXPYR         PIC 9(4).                     CL**9
01028      05  WS-REGIS-RTS-PRVEXPMO         PIC 9(2).                     CL**9
01029      05  WS-COMPT-COUNTY           PIC 9(3) VALUE ZERO.           P442288 
01030      05  WS-TTLPRVPLTNO            PIC X(7)  VALUE SPACES.        P442288 
01031      05  WS-FILETIERCD             PIC 9     VALUE ZERO.          P442288 
01032          88  WS-ACTIVE-TITLE           VALUE 0.                   P442288 
01033          88  WS-INACT-TITLE            VALUE 1.                   P442288 
01034          88  WS-ARCHV-TITLE            VALUE 2.                   P442288 
01035                                                                   P442288 
01036 *---------------------------------------------------------------* P442288 
01037 *        THE FOLLOWING TITLE AND REG DATA IS PICKED UP FROM     * P442288 
01038 *        THE RTS FILES AND RETURNED IN THE RTS TRANSACTIONS.    * P442288 
01039 *        WHAT A REDICULOUS INTERFACE REQUIREMENT!               * P442288 
01040 *---------------------------------------------------------------* P442288 
01041      05  WS-TITLE-DATA.                                           P442288 
01042          10  WS-OWNRID             PIC X(9)  VALUE SPACES.        P442288 
01043          10  WS-TTLPROCSCD         PIC X     VALUE SPACES.        P442288 
01044          10  WS-OWNRTTLNAME1       PIC X(30)    VALUE SPACES.     P442288 
01045          10  WS-OWNRTTLNAME2       PIC X(30)    VALUE SPACES.     P442288 
01046          10  WS-OWNRST1            PIC X(30) VALUE SPACES.        P442288 
01047          10  WS-OWNRST2            PIC X(30) VALUE SPACES.        P442288 
01048          10  WS-OWNRCITY           PIC X(19) VALUE SPACES.        P442288 
01049          10  WS-OWNRSTATE          PIC X(02) VALUE SPACES.        P442288 
01050          10  WS-OWNRZIPCODE.                                      P442288 
01051              15  WS-OWNRZPCD           PIC X(05) VALUE SPACES.    P442288 
01052              15  WS-OWNRZPCDP4         PIC X(04) VALUE SPACES.    P442288 
01053          10  WS-OWNRCNTRY          PIC X(04) VALUE SPACES.        P442288 
01054          10  WS-PRIVACYOPTCD       PIC X(1)  VALUE ZERO.          P442288 
01055          10  WS-VEHCLASSCD         PIC X(08)    VALUE SPACES.     P442288 
01056          10  WS-VIN                PIC X(22)    VALUE SPACES.     P442288 
01057          10  WS-VEHMK              PIC X(04)    VALUE SPACES.     P442288 
01058          10  WS-VEHMODLYR          PIC 9(04)    VALUE ZEROS.      P442288 
01059          10  WS-AUDITTRAILTRANSID  PIC X(17)    VALUE SPACES.     P442288 
01060          10  WS-DIESELINDI         PIC X(01)    VALUE SPACES.     P442288 
01061          10  WS-VEHTON             PIC 9(02)V99 VALUE ZEROS.      P442288 
01062          10  WS-VEHUNITNO          PIC X(06)    VALUE SPACES.     P442288 
01063          10  WS-VEHEMPTYWT         PIC 9(06)    VALUE ZEROS.      P442288 
01064          10  WS-TTLVEHLOCST1       PIC X(30)    VALUE SPACES.     P442288 
01065          10  WS-TTLVEHLOCST2       PIC X(30)    VALUE SPACES.     P442288 
01066          10  WS-TTLVEHLOCCITY      PIC X(19)    VALUE SPACES.     P442288 
01067          10  WS-TTLVEHLOCSTATE     PIC X(02)    VALUE SPACES.     P442288 
01068          10  WS-TTLVEHLOCZPCD      PIC X(05)    VALUE SPACES.     P442288 
01069          10  WS-TTLVEHLOCZPCDP4    PIC X(04)    VALUE SPACES.     P442288 
01070      05  WS-REGIS-DATA.                                           P442288 
01071          10  WS-REGSTKRNO          PIC X(10)    VALUE SPACES.     P442288 
01072          10  WS-REGSTKRCD          PIC X(08)    VALUE SPACES.     P442288 
01073          10  WS-REGPLTCD           PIC X(08)    VALUE SPACES.     P442288 
01074          10  WS-RECPNTNAME         PIC X(30)    VALUE SPACES.     P442288 
01075          10  WS-DPSSAFTYSUSPNINDI  PIC 9(01)    VALUE ZEROS.      P442288 
01076          10  WS-HVYVEHUSETAXINDI   PIC 9(01)    VALUE ZEROS.      P442288 
01077          10  WS-REGINVLDINDI       PIC 9(01)    VALUE ZEROS.      P442288 
01078          10  WS-REGHOTCKINDI       PIC 9(01)    VALUE ZEROS.      P442288 
01079          10  WS-PLTSSEIZDINDI      PIC 9(01)    VALUE ZEROS.      P442288 
01080          10  WS-STKRSEIZDINDI      PIC 9(01)    VALUE ZEROS.      P442288 
01081          10  WS-REGREFAMT          PIC 9(04)V99 VALUE ZEROS.      P442288 
01082          10  WS-NOTFYNGCITY        PIC X(19)    VALUE SPACES.     P442288 
01083          10  WS-VEHGROSSWT         PIC 9(06)    VALUE ZEROS.      P442288 
01084          10  WS-TIRETYPECD         PIC X(01)    VALUE SPACES.     P442288 
01085          10  WS-REGPLTOWNRNAME     PIC X(30)    VALUE SPACES.     P442288 
01086 *---------------------------------------------------------------* P442288 
01087 *         THE FOLLOWING FIELDS ARE PRESET TO ZERO OR SPACES     * P442288 
01088 *---------------------------------------------------------------* P442288 
01089          10  WS-Z-IND              PIC X(01)    VALUE ZERO.       P442288 
01090          10  WS-Z-AMT              PIC 9(04)V99 VALUE ZERO.       P442288 
01091          10  WS-Z-FLD              PIC X        VALUE SPACE.      P442288 
01092                                                                   P442288 
01093  01  COUNTERS.                                                    P442288 
01094      05  UPD-PAGE-CTR              PIC  99   VALUE ZERO.          P442288 
01095      05  UPD-LINE-CTR              PIC  99   VALUE   51.          P442288 
01096      05  MVU-PAGE-CTR              PIC  99   VALUE ZERO.          P442288 
01097      05  MVU-LINE-CTR              PIC  99   VALUE   51.          P442288 
01098      05  MSG-PAGE-CTR              PIC  99   VALUE ZERO.          P442288 
01099      05  MSG-LINE-CTR              PIC  99   VALUE   51.          P442288 
01100      05  ERR-PAGE-CTR              PIC  99   VALUE ZERO.          P442288 
01101      05  ERR-LINE-CTR              PIC  99   VALUE   51.          P442288 
01102      05  WS-ERRORS-CTR             PIC  9(2).                     P442288 
01103      05  PROPER-SPACING            PIC  X(1).                     P442288 
01104      05  WS-FETCH-COUNT            PIC  9(4) COMP VALUE ZERO.     P442288 
01105      05  WS-RTS-NO-PR-TRANS        PIC  9(5) VALUE ZERO.          P442288 
01106      05  WS-RTS-NO-PR-BATCHES      PIC  9(4) VALUE ZERO.          P442288 
01107      05  WS-RTS-NO-BATCHES         PIC  9(4) VALUE ZERO.          P442288 
01108      05  WS-RTS-NO-TRANS-BATCH     PIC  9(5) VALUE ZERO.          P442288 
01109      05  WS-BATCH-NOTITLE-ERRS     PIC  9(4) VALUE ZERO.          P442288 
01110      05  WS-NUM-SKIP-AUDIT-MILEAGE PIC  9(4) VALUE ZERO.          P442288 
01111      05  WS-NUM-SKIP-WEIGHT-CHANGE PIC  9(4) VALUE ZERO.          P442288 
01112      05  WS-NUM-SKIP-MILE-CHANGE   PIC  9(4) VALUE ZERO.          P442288 
01113      05  WS-NUM-SKIP-REFUND        PIC  9(4) VALUE ZERO.          P442288 
01114      05  WS-NUM-SKIP-CONV-GEAR     PIC  9(4) VALUE ZERO.          P442288 
01115      05  WS-NUM-SKIP-XPLATE        PIC  9(4) VALUE ZERO.             CL**9
01116      05  WS-NUM-SKIP-DELETED       PIC  9(4) VALUE ZERO.          P442288 
01117      05  WS-DUMMY-RECORDS-COUNT    PIC  9(6) COMP VALUE ZERO.     P442288 
01118      05  WS-UPIRP-RECORDS-COUNT    PIC  9(6) COMP VALUE ZERO.     P442288 
01119      05  WS-UPIRP-DUMMY-COUNT      PIC  9(6) COMP VALUE ZERO.     P442288 
01120      05  WS-INDEX1                 PIC  9(4) COMP VALUE ZERO.     P442288 
01121                                                                   P442288 
01122  01  WS-VIN-SQL-INQ-WORK-AREA.                                    P442288 
01123      05  WS-VIN-VIN                PIC X(17).                     P442288 
01124      05  WS-2115-RETURN-CODE       PIC 9(2).                      P442288 
01125      05  WS-2115-ADABAS-RC         PIC 9(8)  COMP.                P442288 
01126      05  WS-VIN-MORE-IND           PIC X(1).                      P442288 
01127      05  WS-VIN-ACTIVE-COUNT       PIC 9(2).                      P442288 
01128      05  WS-THIS-VIN-DOCNO.                                       P442288 
01129          10  WS-THIS-VIN-OFF-WSID  PIC X(6).                      P442288 
01130          10  WS-THIS-VIN-AM-DT-TM  PIC X(11).                     P442288 
01131      05  WS-BEST-VIN-DATA-HOLD.                                   P442288 
01132          10  WS-VIN-DOCNO.                                        P442288 
01133              15  WS-VIN-OFF-WSID   PIC X(6).                      P442288 
01134              15  WS-VIN-AM-DT-TM.                                 P442288 
01135                  20 WS-VIN-AMDT-DT PIC X(5).                      P442288 
01136                  20 WS-VIN-AMDT-TM PIC X(6).                      P442288 
01137          10  WS-BEST-VIN-VIN       PIC X(22).                     P442288 
01138          10  WS-VIN-TTLPROCSCD     PIC X(2).                      P442288 
01139          10  WS-VIN-TTLDATE        PIC 9(8).                      P442288 
01140                                                                   P442288 
01141                                                                   P442288 
01142      EJECT                                                        P442288 
01143 *MISC WORK AREA*                                                  P442288 
01144  01  WS-MISCELLANEOUS.                                            P442288 
01145      05  RTS-TTL-SKIPPED-FLAG          PIC X      VALUE 'N'.      P442288 
01146      05  RTS-TTL-2111-INQ-FLAG         PIC X      VALUE 'I'.      P442288 
01147           88   RTS-TTL-STANDARD-INQ      VALUE  'I' 'S'.          P442288 
01148           88   RTS-TTL-EXC-CANCEL-INQ    VALUE  'C'.              P442288 
01149      05  RTS-TTL-SKIPPED-OLD-DOCNO     PIC X(17)  VALUE SPACES.   P442288 
01150      05  WS-IRP-VEHL-UPDATE-AMDATE     PIC 9(5)   VALUE ZEROS.    P442288 
01151      05  WS-IRP-VEHL-UPDATE-WINDOW     PIC 9(5)   VALUE ZEROS.    P442288 
01152      05  WS-R5-TITL-YYMMDD.                                       P442288 
01153          10  WS-R5-TITL-YYMMDD-YY  PIC 9(02).                     P442288 
01154          10  WS-R5-TITL-YYMMDD-MM  PIC 9(02).                     P442288 
01155          10  WS-R5-TITL-YYMMDD-DD  PIC 9(02).                     P442288 
01156      05  WS-IRP-5YR-ACCT-FLAG      PIC X     VALUE SPACE.         P442288 
01157      05  WS-REG-PLTNO-REGEXPYR.                                   P442288 
01158          10  WS-SUPERZA-REGPLTNO PIC X(7).                        P442288 
01159          10  WS-SUPERZA-REGEXPYR PIC 9(4).                        P442288 
01160 *---------------------------------------------------------------* P442288 
01161 *        SPECIAL ACCOUNT NUMBERS INDICATE SPECIAL PROCESSING    * P442288 
01162 *        ---------------------------------------------------    * P442288 
01163 *        THIS IS NO LONER USED - WE NOW USE WRKRTS-ACT-TYPE     *    CL*18
01164 *                                                               *    CL**2
01165 *    5 YR TOKEN TRAILER ACCOUNT NUMBERS ARE > 500000 & < 600000 * P442288 
01166 *        WITH FIRST CHARACTER ALWAYS A '5'                      * P442288 
01167 *        OTHER FLEET '5 ' ACCOUNTS ARE APPORTIONED.             * P442288 
01168 *    ALL TRUE 5 YEAR ACCOUNTS HAVE FLEET NUMBER '5 '            * P442288 
01169 *        AND THEY ARE BILLED FOR 5 YEARS OF TEXAS FEES, WHICH IS* P442288 
01170 *        CONTROLLED BY 'YEARS TO BILL' ON ACCOUNT (PF10) SCREEN * P442288 
01171 *    5 YR RENTAL TRAILER ACCOUNT NUMBERS ARE > 800000 & < 900000* P442288 
01172 *    FORESTRY ACCOUNT NUMBERS ARE > 900000 & =< 999999          * P442288 
01173 *---------------------------------------------------------------* P442288 
01174 *---------------------------------------------------------------*    CL**2
01175 *     05  WS-IRP-ACCOUNT-NO-ANALYSIS.                           *    CL**2
01176 *        10  FILLER-SSS-SUPPL-NBR  PIC X(3).                    *    CL**2
01177 *        10  FILLER-ACCT-YR        PIC X(4).                    *    CL**2
01178 *        10  FILLER-ACCT-MO        PIC X(2).                    *    CL**2
01179 *        10  FILLER-A-1            PIC X(1).                    *    CL**2
01180 *            88  WS-TAG-5-YR-TKN-TR-ACCOUNT   VALUE '5'.        *    CL**2
01181 *            88  WS-TAG-RENTAL-TRLR-ACCOUNT   VALUE '8'.        *    CL**2
01182 *            88  WS-TAG-FORESTRY-ACCOUNT      VALUE '9'.        *    CL**2
01183 *        10  FILLER-A-2-5          PIC X(5).                    *    CL**2
01184 *        10  FILLER-FL             PIC X(2).                    *    CL**2
01185 *            88  WS-TAG-5-YR-APP-ACCOUNT      VALUE '5 '.       *    CL**2
01186 *        10  FILLER-TYPE           PIC X(1).                    *    CL**2
01187 *            88  WS-TAG-POWER-UNIT-SUFFIX     VALUE 'P'.        *    CL**2
01188 *            88  WS-TAG-TRAILER-UNIT-SUFFIX   VALUE 'T'.        *    CL**2
01189 *---------------------------------------------------------------*    CL**2
01190  01  WS-PLATE-NO-IRP.                                             P442288 
01191      05  WS-IRP-PLATE.                                            P442288 
01192          10  WS-IRP-LIC-CHAR-1     PIC X(1).                      P442288 
01193          10  WS-IRP-LIC-NUM-2      PIC X(1).                      P442288 
01194          10  WS-IRP-LIC-NUM-3-6    PIC X(4).                      P442288 
01195      05  WS-FORM2-PLATE   REDEFINES WS-IRP-PLATE.                 P442288 
01196          10  WS-IRP-LIC-NUM-1-2    PIC X(2).                      P442288 
01197          10  WS-IRP-LIC-CHAR-3     PIC X(1).                      P442288 
01198          10  WS-IRP-LIC-NUM-4      PIC X(1).                      P442288 
01199          10  WS-IRP-LIC-NUM-5-6    PIC X(2).                      P442288 
01200                                                                   P442288 
01201  01  WS-PLATE-NO-RTS.                                             P442288 
01202      05  WS-RTS-1-PLATE.                                          P442288 
01203          10  WS-RTS-1-PREF         PIC X(1).                      P442288 
01204          10  WS-RTS-NUM-2-6        PIC X(5).                      P442288 
01205          10  WS-RTS-1-NUM-7        PIC X(1).                      P442288 
01206      05  WS-RTS-2-PLATE   REDEFINES WS-RTS-1-PLATE.               P442288 
01207          10  WS-RTS-2-PREF         PIC X(2).                      P442288 
01208          10  WS-RTS-2-ID           PIC X(1).                      P442288 
01209          10  WS-RTS-NUM-4-6        PIC X(3).                      P442288 
01210          10  WS-RTS-2-NUM-7        PIC X(1).                      P442288 
01211                                                                   P442288 
01212  01   WS-RETURN-CODE               PIC 9(04) COMP VALUE  ZERO.    P442288 
01213                                                                   P442288 
01214  01  WS-SCRT                       PIC X(08)  VALUE '........'.   P442288 
01215  01  WS-ADABAS-DBIDS.                                             P442288 
01216 *---------------------------------------------------------------* P442288 
01217 *        PRD2 DBID = 141                                        * P442288 
01218 *        TST3 DBID = 193                                        * P442288 
01219 *        TST5 DBID = 194                                        * P442288 
01220 *---------------------------------------------------------------* P442288 
01221       04  WS-INQUIRY-ADABAS-DBID   PIC 9(3) COMP VALUE   0.       P442288 
01222       04  WS-SHOW-DBID-VALUE       PIC 9(3)      VALUE   000.     P442288 
01223                                                                   P442288 
01224 *---------------------------------------------------------------* P442288 
01225 *                                                               * P442288 
01226 *   THE FILE NUMBER ORDER IN THE WORKING STORAGE AREA BUILT BY  * P442288 
01227 *   THE 'ADABAS BEGIN DECLARE SECTION' COMMAND IS DETERMINED BY * P442288 
01228 *   THE ORDER IN WHICH THE ADABAS FILE COMMANDS OCCUR IN THE    * P442288 
01229 *   PROGRAM.  IF THIS ORDER IS CHANGED, THE WORKING STORAGE AREA* P442288 
01230 *   NAMES 'FILE-NUMBER-0-1'....'FILE-NUMBER-0-X' WILL REFERENCE * P442288 
01231 *   DIFFERENT FILES.  THE TEST ENVIRONMENT SETUP PARAGRAPH NAMED* P442288 
01232 *   '00-SET-TEST5-ENVIRONMENT' WILL HAVE TO BE CHANGED TO MATCH * P442288 
01233 *   THE NEW FILE REFERENCE ORDER.  CURRENT REFERENCES ARE:      * P442288 
01234 *                                                               * P442288 
01235 *---------------------------------------------------------------* P442288 
01236 *                                                               * P442288 
01237 *   FILE-NUMBER-0-1 BUFFER NAME RTS-TITLE                       * P442288 
01238 *        ADABAS PROD. FILE ID   1 = RTS-TITLE                   * P442288 
01239 *   FILE-NUMBER-0-2 BUFFER NAME RTS-INACT                       * P442288 
01240 *        ADABAS PROD. FILE ID   1 = RTS-INACT                   * P442288 
01241 *   FILE-NUMBER-0-3 BUFFER NAME RTS-FUND                        * P442288 
01242 *        ADABAS PROD. FILE ID  23 = RTS_FUND_FUNCT_TANS         * P442288 
01243 *   FILE-NUMBER-0-4 BUFFER NAME RTS-TR-FDS                      * P442288 
01244 *        ADABAS PROD. FILE ID  25 = RTS_TR_FDS_DETAIL           * P442288 
01245 *   FILE-NUMBER-0-5 BUFFER NAME RTS-INV                         * P442288 
01246 *        ADABAS PROD. FILE ID  21 = RTS_INV_FUNC_TRANS          * P442288 
01247 *   FILE-NUMBER-0-6 BUFFER NAME RTS-TR-INV                      * P442288 
01248 *        ADABAS PROD. FILE ID  24 = RTS_TR_INV_DETAIL           * P442288 
01249 *   FILE-NUMBER-0-7 BUFFER NAME RTS-XO-TRANS                    * P442288 
01250 *        ADABAS PROD. FILE ID  22 = RTS_MV_FUNC_TRANS           * P442288 
01251 *   FILE-NUMBER-0-8 BUFFER NAME RTS-MV-TRANS                    * P442288 
01252 *        ADABAS PROD. FILE ID  22 = RTS_MV_FUNC_TRANS           * P442288 
01253 *   FILE-NUMBER-0-9 BUFFER NAME RTS-TRANS                       * P442288 
01254 *        ADABAS PROD. FILE ID  20 = RTS_TRANS                   * P442288 
01255 *        ADABAS TEST. FILE ID 198 = RTS_TRANS5                  * P442288 
01256 *---------------------------------------------------------------* P442288 
01257                                                                   P442288 
01258       EXEC ADABAS                                                 P442288 
01259            BEGIN  DECLARE SECTION                                 P442288 
01260       END-EXEC                                                    P442288 
01261                                                                   P442288 
01262 ***************************************************************** P442288 
01263 *  ACTIVE REGIS DOCNO RECORD (ARDR) - LATEST REGISTERED DOCNO REC P442288 
01264 ***************************************************************** P442288 
01265                                                                   P442288 
01266      EXEC ADABAS                                                  P442288 
01267           FIND                                                    P442288 
01268           DECLARE ARDR CURSOR FOR                                 P442288 
01269           SELECT DOCNO,                                           P442288 
01270                 REGSTKRNO,                                        P442288 
01271                 REGSTKRCD,                                        P442288 
01272                 REGPLTNO,                                         P442288 
01273                 PREVPLTNO,                                        P442288 
01274                 PREVEXPMO,                                           CL**9
01275                 PREVEXPYR,                                           CL**9
01276                 REGEFFDATE,                                       P442288 
01277                 REGEXPMO,                                         P442288 
01278                 REGEXPYR,                                         P442288 
01279                 PLTBIRTHDATE,                                     P442288 
01280                 REGCLASSCD,                                       P442288 
01281                 REGPLTCD,                                         P442288 
01282                 RECPNTNAME,                                       P442288 
01283                 DPSSAFTYSUSPNINDI,                                P442288 
01284                 HVYVEHUSETAXINDI,                                 P442288 
01285                 REGINVLDINDI,                                     P442288 
01286                 REGHOTCKINDI,                                     P442288 
01287                 PLTSSEIZDINDI,                                    P442288 
01288                 STKRSEIZDINDI,                                    P442288 
01289                 REGREFAMT,                                        P442288 
01290                 NOTFYNGCITY,                                      P442288 
01291                 VEHGROSSWT,                                       P442288 
01292                 CUSTACTULREGFEE,                                  P442288 
01293                 TIRETYPECD,                                       P442288 
01294                 REGPLTOWNRNAME                                    P442288 
01295              FROM RTS-REGIS      RTS-ARDR                         P442288 
01296                 WHERE DOCNO   = :WS-RTS-DOCNO                     P442288 
01297              OPTIONS PASSWORD = :WS-SCRT                          P442288 
01298                 PREFIX        = :RTS-ARDR-                        P442288 
01299                 DBID          = :WS-INQUIRY-ADABAS-DBID           P442288 
01300      END-EXEC.                                                    P442288 
01301                                                                   P442288 
01302 ***************************************************************** P442288 
01303 * INACTIVE REGIS DOCNO RECORD (ARDR) -LATEST REGISTERED DOCNO REC P442288 
01304 * INACTIVE REGIS DOCNO RECORD (IRDR) -LATEST REGISTERED DOCNO REC P442288 
01305 ***************************************************************** P442288 
01306                                                                   P442288 
01307      EXEC ADABAS                                                  P442288 
01308           FIND                                                    P442288 
01309           DECLARE IRDR CURSOR FOR                                 P442288 
01310           SELECT DOCNO,                                           P442288 
01311                 REGSTKRNO,                                        P442288 
01312                 REGSTKRCD,                                        P442288 
01313                 REGPLTNO,                                         P442288 
01314                 PREVPLTNO,                                        P442288 
01315                 PREVEXPMO,                                           CL**9
01316                 PREVEXPYR,                                           CL**9
01317                 REGEFFDATE,                                       P442288 
01318                 REGEXPMO,                                         P442288 
01319                 REGEXPYR,                                         P442288 
01320                 PLTBIRTHDATE,                                     P442288 
01321                 REGCLASSCD,                                       P442288 
01322                 REGPLTCD,                                         P442288 
01323                 RECPNTNAME,                                       P442288 
01324                 DPSSAFTYSUSPNINDI,                                P442288 
01325                 HVYVEHUSETAXINDI,                                 P442288 
01326                 REGINVLDINDI,                                     P442288 
01327                 REGHOTCKINDI,                                     P442288 
01328                 PLTSSEIZDINDI,                                    P442288 
01329                 STKRSEIZDINDI,                                    P442288 
01330                 REGREFAMT,                                        P442288 
01331                 NOTFYNGCITY,                                      P442288 
01332                 VEHGROSSWT,                                       P442288 
01333                 CUSTACTULREGFEE,                                  P442288 
01334                 TIRETYPECD,                                       P442288 
01335                 REGPLTOWNRNAME                                    P442288 
01336              FROM RTS-INACT-REGIS    RTS-IRDR                     P442288 
01337                 WHERE DOCNO   = :WS-RTS-DOCNO                     P442288 
01338              OPTIONS PASSWORD = :WS-SCRT                          P442288 
01339                 PREFIX        = :RTS-IRDR-                        P442288 
01340                 DBID          = :WS-INQUIRY-ADABAS-DBID           P442288 
01341      END-EXEC.                                                    P442288 
01342                                                                   P442288 
01343                                                                   P442288 
01344 ***************************************************************** P442288 
01345 * ARCHIVES REGIS DOCNO RECORD (RRDR) -LATEST REGISTERED DOCNO REC P442288 
01346 ***************************************************************** P442288 
01347                                                                   P442288 
01348      EXEC ADABAS                                                  P442288 
01349           FIND                                                    P442288 
01350           DECLARE RRDR CURSOR FOR                                 P442288 
01351           SELECT DOCNO,                                           P442288 
01352                 REGSTKRNO,                                        P442288 
01353                 REGSTKRCD,                                        P442288 
01354                 REGPLTNO,                                         P442288 
01355                 PREVPLTNO,                                        P442288 
01356                 PREVEXPMO,                                           CL**9
01357                 PREVEXPYR,                                           CL**9
01358                 REGEFFDATE,                                       P442288 
01359                 REGEXPMO,                                         P442288 
01360                 REGEXPYR,                                         P442288 
01361                 PLTBIRTHDATE,                                     P442288 
01362                 REGCLASSCD,                                       P442288 
01363                 REGPLTCD,                                         P442288 
01364                 RECPNTNAME,                                       P442288 
01365                 DPSSAFTYSUSPNINDI,                                P442288 
01366                 HVYVEHUSETAXINDI,                                 P442288 
01367                 REGINVLDINDI,                                     P442288 
01368                 REGHOTCKINDI,                                     P442288 
01369                 PLTSSEIZDINDI,                                    P442288 
01370                 STKRSEIZDINDI,                                    P442288 
01371                 REGREFAMT,                                        P442288 
01372                 NOTFYNGCITY,                                      P442288 
01373                 VEHGROSSWT,                                       P442288 
01374                 CUSTACTULREGFEE,                                  P442288 
01375                 TIRETYPECD,                                       P442288 
01376                 REGPLTOWNRNAME                                    P442288 
01377              FROM RTS-ARCHIVE-REGIS    RTS-RRDR                   P442288 
01378                 WHERE DOCNO   = :WS-RTS-DOCNO                     P442288 
01379              OPTIONS PASSWORD = :WS-SCRT                          P442288 
01380                 PREFIX        = :RTS-RRDR-                        P442288 
01381                 DBID          = :WS-INQUIRY-ADABAS-DBID           P442288 
01382      END-EXEC.                                                    P442288 
01383                                                                   P442288 
01384 ***************************************************************** P442288 
01385 *  ACTIVE REGIS SUPERZA 'PLATE + EXP' FOR DUPLICATE REGISTRATION  P442288 
01386 ***************************************************************** P442288 
01387                                                                   P442288 
01388      EXEC ADABAS                                                  P442288 
01389           FIND                                                    P442288 
01390           DECLARE DREG CURSOR FOR                                 P442288 
01391           SELECT DOCNO,                                           P442288 
01392              FROM RTS-REGIS      RTS-DREG                         P442288 
01393                 WHERE SUPERZB = :WS-REG-PLTNO-REGEXPYR            P442288 
01394              OPTIONS PASSWORD = :WS-SCRT                          P442288 
01395                 PREFIX        = :RTS-DREG-                        P442288 
01396                 DBID     = :WS-INQUIRY-ADABAS-DBID                P442288 
01397      END-EXEC.                                                    P442288 
01398                                                                   P442288 
01399      EXEC ADABAS                                                  P442288 
01400           FIND                                                    P442288 
01401           DECLARE CVIN CURSOR FOR                                 P442288 
01402           SELECT DOCNO,                                           P442288 
01403                  VIN,                                             P442288 
01404                  TTLPROCSCD,                                      P442288 
01405                  TTLISSUEDATE,                                    P442288 
01406           FROM RTS-TITLE  ACCESS-BY-CVIN                          P442288 
01407                 WHERE VIN = :WS-VIN-VIN                           P442288 
01408                  OPTIONS PASSWORD = :WS-SCRT                      P442288 
01409                  DBID             = :WS-INQUIRY-ADABAS-DBID       P442288 
01410      END-EXEC.                                                    P442288 
01411                                                                   P442288 
01412      EXEC ADABAS                                                  P442288 
01413           FIND                                                    P442288 
01414           DECLARE IVIN CURSOR FOR                                 P442288 
01415           SELECT DOCNO,                                           P442288 
01416                  VIN,                                             P442288 
01417                  TTLPROCSCD,                                      P442288 
01418                  TTLISSUEDATE,                                    P442288 
01419           FROM RTS-INACT-TITLE  ACCESS-BY-IVIN                    P442288 
01420                 WHERE VIN = :WS-VIN-VIN                           P442288 
01421                  OPTIONS PASSWORD = :WS-SCRT                      P442288 
01422                  DBID             = :WS-INQUIRY-ADABAS-DBID       P442288 
01423      END-EXEC.                                                    P442288 
01424                                                                   P442288 
01425                                                                   P442288 
01426      EXEC ADABAS                                                  P442288 
01427           FIND                                                    P442288 
01428           DECLARE RVIN CURSOR FOR                                 P442288 
01429           SELECT DOCNO,                                           P442288 
01430                  VIN,                                             P442288 
01431                  TTLPROCSCD,                                      P442288 
01432                  TTLISSUEDATE,                                    P442288 
01433           FROM RTS-ARCHIVE-TITLE  ACCESS-BY-RVIN                  P442288 
01434                 WHERE VIN = :WS-VIN-VIN                           P442288 
01435                  OPTIONS PASSWORD = :WS-SCRT                      P442288 
01436                  DBID             = :WS-INQUIRY-ADABAS-DBID       P442288 
01437      END-EXEC.                                                    P442288 
01438                                                                   P442288 
01439         EJECT                                                     P442288 
01440         EJECT                                                     P442288 
01441                                                                   P442288 
01442 ******************************************************************P442288 
01443 *     ERROR MESSAGES FOR EDIT CONTROL PROCESSING,                *P442288 
01444 *     DATA ERROR PROCESSING, AND RECORD NOT FOUND PROCESSING     *P442288 
01445 ******************************************************************P442288 
01446                                                                   P442288 
01447  01  UPDAT-HDR-1.                                                 P442288 
01448      02  FILLER                    PIC X(48) VALUE                P442288 
01449          ' IRP.8802        INTERNATIONAL REGISTRATION PLAN'.      P442288 
01450      02  FILLER                    PIC X(19) VALUE                P442288 
01451          '              DATE '.                                   P442288 
01452      02  UPDAT-CURRENT-DT          PIC X(10).                     P442288 
01453      02  FILLER                    PIC X     VALUE SPACES.        P442288 
01454      02  UPDAT-CURRENT-DAY-WK      PIC X(05) VALUE SPACES.        P442288 
01455      02  FILLER                    PIC X(5)  VALUE ' '.           P442288 
01456      02  FILLER                    PIC X(5)  VALUE 'PAGE '.       P442288 
01457      02  UPDAT-PAGE-NUMBER         PIC ZZ9.                       P442288 
01458                                                                   P442288 
01459  01  UPDAT-HDR-2.                                                 P442288 
01460      02  FILLER                    PIC X(50) VALUE                P442288 
01461          '  RTS TRANS / IRP FILE     UPDATE REPORT - P442288'.    P442288 
01462                                                                   P442288 
01463      02  FILLER                    PIC X(2)  VALUE SPACES.        P442288 
01464      02  FILLER                    PIC X(12) VALUE 'FOR AMDATE'.  P442288 
01465      02  RTS-UPD-AMDATE            PIC X(7)  VALUE ALL '#'.       P442288 
01466      02  FILLER                    PIC X(11) VALUE 'TRANSTIME'.   P442288 
01467      02  RTS-UPD-TRANSTIME         PIC 9(6)  VALUE ZEROS.         P442288 
01468                                                                   P442288 
01469  01  MVUPD-HDR-1.                                                 P442288 
01470      02  FILLER                    PIC X(48) VALUE                P442288 
01471          ' IRP.8803        INTERNATIONAL REGISTRATION PLAN'.      P442288 
01472      02  FILLER                    PIC X(19) VALUE                P442288 
01473          '              DATE '.                                   P442288 
01474      02  MVUPD-CURRENT-DT          PIC X(10).                     P442288 
01475      02  FILLER                    PIC X     VALUE SPACES.        P442288 
01476      02  MVUPD-CURRENT-DAY-WK      PIC X(05) VALUE SPACES.        P442288 
01477      02  FILLER                    PIC X(5)  VALUE ' '.           P442288 
01478      02  FILLER                    PIC X(5)  VALUE 'PAGE '.       P442288 
01479      02  MVUPD-PAGE-NUMBER         PIC ZZ9.                       P442288 
01480                                                                   P442288 
01481  01  MVUPD-HDR-2.                                                 P442288 
01482      02  FILLER                    PIC X(50) VALUE                P442288 
01483          '  RTS TRANS / MVUPDT       UPDATE REPORT - P442288'.    P442288 
01484                                                                   P442288 
01485      02  FILLER                    PIC X(2)  VALUE SPACES.        P442288 
01486      02  FILLER                    PIC X(12) VALUE 'FOR AMDATE'.  P442288 
01487      02  RTS-MVU-AMDATE            PIC X(7)  VALUE ALL '#'.       P442288 
01488      02  FILLER                    PIC X(11) VALUE 'TRANSTIME'.   P442288 
01489      02  RTS-MVU-TRANSTIME         PIC 9(6)  VALUE ZEROS.         P442288 
01490                                                                   P442288 
01491  01  MSGOUT-HDR-1.                                                P442288 
01492      02  FILLER                    PIC X(48) VALUE                P442288 
01493          ' IRP.8804        INTERNATIONAL REGISTRATION PLAN'.      P442288 
01494      02  FILLER                    PIC X(19) VALUE                P442288 
01495          '              DATE '.                                   P442288 
01496      02  MSGOUT-CURRENT-DT         PIC X(10).                     P442288 
01497      02  FILLER                    PIC X     VALUE SPACES.        P442288 
01498      02  MSGOUT-CURRENT-DAY-WK     PIC X(05) VALUE SPACES.        P442288 
01499      02  FILLER                    PIC X(5)  VALUE ' '.           P442288 
01500      02  FILLER                    PIC X(5)  VALUE 'PAGE '.       P442288 
01501      02  MSGOUT-PAGE-NUMBER        PIC ZZ9.                       P442288 
01502                                                                   P442288 
01503  01  MSGOUT-HDR-2.                                                P442288 
01504      02  FILLER                    PIC X(50) VALUE                P442288 
01505          '  RTS TRANS / MISC MESSAGES FILE  REPORT - P442288'.    P442288 
01506                                                                   P442288 
01507      02  FILLER                    PIC X(2)  VALUE SPACES.        P442288 
01508      02  FILLER                    PIC X(12) VALUE 'FOR AMDATE'.  P442288 
01509      02  RTS-MSG-AMDATE            PIC X(7)  VALUE ALL '#'.       P442288 
01510      02  FILLER                    PIC X(11) VALUE 'TRANSTIME'.   P442288 
01511      02  RTS-MSG-TRANSTIME         PIC 9(6)  VALUE ZEROS.         P442288 
01512                                                                   P442288 
01513  01  ERROR-HDR-1.                                                 P442288 
01514      02  FILLER                    PIC X(48) VALUE                P442288 
01515          ' IRP.8801        INTERNATIONAL REGISTRATION PLAN'.      P442288 
01516      02  FILLER                    PIC X(19) VALUE                P442288 
01517          '              DATE '.                                   P442288 
01518      02  ERROR-CURRENT-DT          PIC X(10).                     P442288 
01519      02  FILLER                    PIC X     VALUE SPACES.        P442288 
01520      02  ERROR-CURRENT-DAY-WK      PIC X(05) VALUE SPACES.        P442288 
01521      02  FILLER                    PIC X(5)  VALUE ' '.           P442288 
01522      02  FILLER                    PIC X(5)  VALUE 'PAGE '.       P442288 
01523      02  ERROR-PAGE-NUMBER         PIC ZZ9.                       P442288 
01524                                                                   P442288 
01525  01  ERROR-HDR-1-RUNNING.                                         P442288 
01526      02  FILLER                    PIC X(14) VALUE SPACES.        P442288 
01527      02  FILLER                    PIC X(8)  VALUE 'RUNNING '.    P442288 
01528      02  WRKRTS-VERSION.                                          P442288 
01529          04  RTS-PROG-ID           PIC X(7)  VALUE 'P442288'.     P442288 
01530          04  FILLER                PIC X(5)  VALUE ' VER '.       P442288 
01531                                                                   P442288 
01532          04  RTS-VER-DATE          PIC X(10) VALUE ALL '#'.       P442288 
01533          04  RTS-VER-TIME          PIC X(11) VALUE ALL '#'.       P442288 
01534                                                                   P442288 
01535  01  ERROR-HDR-2.                                                 P442288 
01536      02  FILLER                    PIC X(49) VALUE                P442288 
01537          '  RTS TRANSACTION CREATION ERROR REPORT - P442288'.     P442288 
01538                                                                   P442288 
01539      02  FILLER                    PIC X(2)  VALUE SPACES.        P442288 
01540      02  FILLER                    PIC X(12) VALUE 'FOR AMDATE'.  P442288 
01541      02  RTS-RPT-AMDATE            PIC X(7)  VALUE ALL '#'.       P442288 
01542      02  FILLER                    PIC X(11) VALUE 'TRANSTIME'.   P442288 
01543      02  RTS-RPT-TRANSTIME         PIC 9(6)  VALUE ZEROS.         P442288 
01544  01  ERROR-HDR-3.                                                 P442288 
01545      02  FILLER                    PIC X(5)  VALUE SPACES.        P442288 
01546      02  FILLER                    PIC X(55) VALUE                P442288 
01547          'ACCOUNT           LICENSE ERR RTS             ERROR'.   P442288 
01548                                                                   P442288 
01549  01  ERROR-HDR-4.                                                 P442288 
01550      02  FILLER                    PIC X(5)  VALUE SPACES.        P442288 
01551      02  FILLER                    PIC X(61) VALUE                P442288 
01552          '  KEY             NUMBER  NBR UPD            MESSAGES'. P442288 
01553      02  FILLER                    PIC X(23) VALUE SPACES.        P442288 
01554      02  FILLER                    PIC X(19) VALUE SPACES.        P442288 
01555      02  FILLER                    PIC X(20) VALUE                P442288 
01556          'BATCH OR OTHER KEYS'.                                   P442288 
01557                                                                   P442288 
01558  01  ERROR-HDR-5.                                                 P442288 
01559      02  FILLER                    PIC X(01) VALUE SPACES.        P442288 
01560      02  FILLER                    PIC X(53) VALUE                P442288 
01561          '--------------------- ------- --- -- ----------------'. P442288 
01562      02  FILLER                    PIC X(44) VALUE                P442288 
01563          '--------------------------------------------'.          P442288 
01564      02  FILLER                    PIC X(4)  VALUE SPACES.        P442288 
01565      02  FILLER                    PIC X(28) VALUE                P442288 
01566          '----------------------------'.                          P442288 
01567                                                                   P442288 
01568  01  ERROR-HDR-5B.                                                P442288 
01569      02  FILLER                    PIC X(2)  VALUE SPACES.        P442288 
01570      02  FILLER                    PIC X(30) VALUE SPACES.        P442288 
01571      02  FILLER                    PIC X(38) VALUE                P442288 
01572          '** - RECORDS WERE NOT UPDATED AND/OR '.                 P442288 
01573      02  FILLER                    PIC X(20) VALUE                P442288 
01574          'SHOULD BE RESEARCHED'.                                  P442288 
01575  01  ERROR-DET-1.                                                 P442288 
01576      02  FILLER                    PIC X     VALUE SPACES.        P442288 
01577      02  ERR-KEY.                                                 P442288 
01578          05  ERR-ACCOUNT-DT        PIC 9(4)/9(2).                 P442288 
01579          05  FILLER                PIC X     VALUE SPACES.        P442288 
01580          05  ERR-ACCOUNT-NUMBER    PIC X(6).                      P442288 
01581          05  FILLER                PIC X     VALUE SPACES.        P442288 
01582          05  ERR-FLEET-NUMBER      PIC XX.                        P442288 
01583          05  FILLER                PIC X     VALUE SPACES.        P442288 
01584          05  ERR-SUPPLEMENT-NUMBER PIC X(3).                      P442288 
01585      02  FILLER                    PIC X     VALUE SPACE.         P442288 
01586      02  ERR-LICN                  PIC X(7)  VALUE SPACE.         P442288 
01587      02  FILLER                    PIC X     VALUE SPACE.         P442288 
01588      02  ERR-ERROR-MESSAGE-1.                                     P442288 
01589          05  ERR-ERROR-NUMBER      PIC 9(3)  VALUE ZERO.          P442288 
01590          05  FILLER                PIC X     VALUE SPACES.        P442288 
01591          05  ERR-NON-UPD           PIC X(2).                      P442288 
01592          05  FILLER                PIC X     VALUE SPACES.        P442288 
01593          05  ERR-MESSAGE           PIC X(60).                     P442288 
01594      02  FILLER                    PIC X(04) VALUE SPACES.        P442288 
01595      02  ERR-MESS-REGION           PIC X(02) VALUE SPACES.        P442288 
01596      02  FILLER                    PIC X     VALUE SPACES.        P442288 
01597      02  ERR-MESS-DATA             PIC X(60) VALUE SPACES.        P442288 
01598                                                                   P442288 
01599  01  ERROR-MESSAGES-TABLE.                                        P442288 
01600      05  ERROR-MSG-000-TEXT.                                      P442288 
01601          10  FILLER                PIC X(4)  VALUE '600 '.        P442288 
01602          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01603          10  ERR-MSG-000-BATCHES   PIC Z,ZZ9.                     P442288 
01604          10  FILLER                PIC X(11) VALUE                P442288 
01605          ' BATCHES & '.                                           P442288 
01606          10  ERR-MSG-000-TRANS     PIC ZZ,ZZ9.                    P442288 
01607          10  FILLER                PIC X(13) VALUE                P442288 
01608          ' TRANSACTIONS'.                                         P442288 
01609          10  FILLER                PIC X(27) VALUE                P442288 
01610          ' - END OF IRP/RTS INTERFACE'.                           P442288 
01611      05  ERROR-MSG-001-TEXT.                                      P442288 
01612          10  FILLER                PIC X(4)  VALUE '601 '.        P442288 
01613          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01614          10  FILLER                PIC X(22) VALUE                P442288 
01615          'NO REQUEST CARDS FOUND'.                                P442288 
01616          10  FILLER                PIC X(18) VALUE SPACES.        P442288 
01617      05  ERROR-MSG-002-TEXT.                                      P442288 
01618          10  FILLER                PIC X(4)  VALUE '602 '.        P442288 
01619          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01620          10  FILLER                PIC X(28) VALUE                P442288 
01621          'INVALID CONTROL BATCH NUMBER'.                          P442288 
01622          10  FILLER                PIC X(12) VALUE SPACES.        P442288 
01623      05  ERROR-MSG-003-TEXT.                                      P442288 
01624          10  FILLER                PIC X(4)  VALUE '603 '.        P442288 
01625          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01626          10  FILLER                PIC X(39) VALUE                P442288 
01627          'IRP RTS NUMBER IS NOT ON THE BATCH FILE'.               P442288 
01628          10  FILLER                PIC X     VALUE SPACES.        P442288 
01629      05  ERROR-MSG-004-TEXT.                                      P442288 
01630          10  FILLER                PIC X(4)  VALUE '604 '.        P442288 
01631          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01632          10  FILLER                PIC X(38) VALUE                P442288 
01633          'BATCH FILE I/O ERROR -- STATUS ='.                      P442288 
01634          10  EMT-BTCH-ERR-CODE     PIC X(2).                      P442288 
01635      05  ERROR-MSG-005-TEXT.                                      P442288 
01636          10  FILLER                PIC X(4)  VALUE '605 '.        P442288 
01637          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01638          10  FILLER                PIC X(38) VALUE                P442288 
01639          'RTS INTERFACE FILE I/O ERROR--STATUS='.                 P442288 
01640          10  EMT-ERR-IRPRTS        PIC X(2).                      P442288 
01641      05  ERROR-MSG-006-TEXT.                                      P442288 
01642          10  FILLER                PIC X(4)  VALUE '606 '.        P442288 
01643          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01644          10  FILLER                PIC X(30) VALUE                P442288 
01645          'BATCH TYPE OF REQUEST MUST = R'.                        P442288 
01646          10  FILLER                PIC X(10) VALUE SPACES.        P442288 
01647      05  ERROR-MSG-007-TEXT.                                      P442288 
01648          10  FILLER                PIC X(4)  VALUE '607 '.        P442288 
01649          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01650          10  FILLER                PIC X(40) VALUE                P442288 
01651          'MULTIPLE EDIT ERRORS ON CONTROL FILE    '.              P442288 
01652      05  ERROR-MSG-008-TEXT.                                      P442288 
01653          10  FILLER                PIC X(4)  VALUE '608 '.        P442288 
01654          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01655          10  FILLER                PIC X(54) VALUE                P442288 
01656          'NO RTS RECORDS FOR THIS ACCNT/YEAR/SUPP WERE COMPLETED'.P442288 
01657      05  ERROR-MSG-009-TEXT.                                      P442288 
01658          10  FILLER                PIC X(4)  VALUE '609 '.        P442288 
01659          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01660          10  FILLER                PIC X(40) VALUE                P442288 
01661          'NO BATCH CONTROL RECORDS FOR THIS BATCH:'.              P442288 
01662      05  ERROR-MSG-010-TEXT.                                      P442288 
01663          10  FILLER                PIC X(4)  VALUE '610 '.        P442288 
01664          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01665          10  FILLER                PIC X(23) VALUE                P442288 
01666          'NO TITLE FOR IRP VIN # '.                               P442288 
01667          10  ERROR-MSG-010-VIN     PIC X(17) VALUE SPACES.        P442288 
01668          10  FILLER                PIC X(3)  VALUE ' - '.         P442288 
01669          10  ERROR-MSG-010-TITLE   PIC X(17) VALUE SPACES.        P442288 
01670          10  ERROR-MSG-010-RETRY   PIC X(14) VALUE                P442288 
01671          'LAST ATTEMPT'.                                          P442288 
01672      05  ERROR-MSG-011-TEXT.                                      P442288 
01673          10  FILLER                PIC X(4)  VALUE '611 '.        P442288 
01674          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01675          10  FILLER                PIC X(39) VALUE                P442288 
01676          'RECORD NOT PROCESSED;   CHANGE  FLAG='.                 P442288 
01677          10  ERR-TYPE-OF-CHANGE    PIC X     VALUE SPACES.        P442288 
01678      05  ERROR-MSG-012-TEXT.                                      P442288 
01679          10  FILLER                PIC X(4)  VALUE '612 '.        P442288 
01680          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01681          10  FILLER                PIC X(27) VALUE                P442288 
01682          'CONTROL YEAR IS NOT NUMERIC'.                           P442288 
01683          10  FILLER                PIC X(13) VALUE SPACES.        P442288 
01684      05  ERROR-MSG-013-TEXT.                                      P442288 
01685          10  FILLER                PIC X(4)  VALUE '613 '.        P442288 
01686          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01687          10  ERROR-MSG-013-NUMB    PIC Z(2)9 VALUE ZEROS.         P442288 
01688          10  FILLER                PIC X(39) VALUE                P442288 
01689          ' ERRORS WILL BE REPROCESSED ON NEXT RUN'.               P442288 
01690      05  ERROR-MSG-014-TEXT.                                      P442288 
01691          10  FILLER                PIC X(4)  VALUE '614 '.        P442288 
01692          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01693          10  ERROR-MSG-014-FILE    PIC X(14) VALUE SPACES.        P442288 
01694          10  FILLER                PIC X(26) VALUE                P442288 
01695          ' FILE WITH ADABAS ERROR = '.                            P442288 
01696          10  ERROR-MSG-014-ERRNUMB PIC Z(2)9 VALUE ZEROS.         P442288 
01697      05  ERROR-MSG-015-TEXT.                                      P442288 
01698          10  FILLER                PIC X(4)  VALUE '615 '.        P442288 
01699          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01700          10  FILLER                PIC X(10) VALUE 'RTS TITLE '.  P442288 
01701          10  ERROR-MSG-015-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01702          10  FILLER                PIC X(9)  VALUE ' EXPIRES '.   P442288 
01703          10  ERROR-MSG-015-EXP-T   PIC 9999/99.                   P442288 
01704          10  FILLER                PIC X(13) VALUE                P442288 
01705          ' NOT = REGIS '.                                         P442288 
01706          10  ERROR-MSG-015-EXP-R   PIC 9999/99.                   P442288 
01707      05  ERROR-MSG-016-TEXT.                                      P442288 
01708          10  FILLER                PIC X(4)  VALUE '616 '.        P442288 
01709          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01710          10  ERROR-MSG-016-TYPE    PIC X(15) VALUE                P442288 
01711          'CANCELLED REG: '.                                       P442288 
01712          10  ERROR-MSG-016-PLT     PIC X(8)  VALUE ZERO.          P442288 
01713          10  ERROR-MSG-016-EXP-DT  PIC 9999/99.                   P442288 
01714          10  FILLER                PIC X     VALUE SPACE.         P442288 
01715          10  ERROR-MSG-016-NOW     PIC X(12) VALUE                P442288 
01716          'NOW INVALID '.                                          P442288 
01717          10  ERROR-MSG-016-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01718      05  ERROR-MSG-017-TEXT.                                      P442288 
01719          10  FILLER                PIC X(4)  VALUE '617 '.        P442288 
01720          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01721          10  FILLER                PIC X(11) VALUE 'RTS TITLE '.  P442288 
01722          10  FILLER                PIC X(32) VALUE                P442288 
01723          'IDENTICAL RE-UPDATE BYPASSED  #'.                       P442288 
01724          10  ERROR-MSG-017-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01725      05  ERROR-MSG-018-TEXT.                                      P442288 
01726          10  FILLER                PIC X(4)  VALUE '618 '.        P442288 
01727          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01728          10  FILLER                PIC X(11) VALUE 'RTS REGIS '.  P442288 
01729          10  ERROR-MSG-018-PLT     PIC X(8)  VALUE ZERO.          P442288 
01730          10  ERROR-MSG-018-EXP     PIC 9(4)  VALUE ZERO.          P442288 
01731          10  FILLER                PIC X(17) VALUE                P442288 
01732          ' REGISTERED TO: '.                                      P442288 
01733          10  ERROR-MSG-018-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01734      05  ERROR-MSG-019-TEXT.                                      P442288 
01735          10  FILLER                PIC X(4)  VALUE '619 '.        P442288 
01736          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01737          10  FILLER                PIC X(40) VALUE                P442288 
01738          'NON TITLED 00000000000000000 (IRP DUMMY)'.              P442288 
01739      05  ERROR-MSG-019B-TEXT.                                     P442288 
01740          10  FILLER                PIC X(4)  VALUE '619 '.        P442288 
01741          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01742          10  ERR-MSG-019-COUNT     PIC ZZZ,ZZ9.                   P442288 
01743          10  FILLER                PIC X(50) VALUE                P442288 
01744          ' NON TITLED 00000000000000000 (IRP DUMMY) COUNT'.       P442288 
01745      05  ERROR-MSG-019C-TEXT.                                     P442288 
01746          10  FILLER                PIC X(4)  VALUE '619 '.        P442288 
01747          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01748          10  ERR-MSG-019C-COUNT1   PIC ZZZ,ZZ9.                   P442288 
01749          10  FILLER                PIC X(29) VALUE                P442288 
01750          ' UPDATED IRP DOCNO COUNT WITH'.                         P442288 
01751          10  ERR-MSG-019C-COUNT2   PIC ZZZ,ZZ9.                   P442288 
01752          10  FILLER                PIC X(11) VALUE                P442288 
01753          ' NON TITLED'.                                           P442288 
01754      05  ERROR-MSG-020-TEXT.                                      P442288 
01755          10  FILLER                PIC X(4)  VALUE '620 '.        P442288 
01756          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01757          10  FILLER                PIC X(08) VALUE                P442288 
01758          'UNKNOWN '.                                              P442288 
01759          10  ERROR-MSG-020-TYPE    PIC X(04) VALUE '****'.        P442288 
01760          10  FILLER                PIC X(13) VALUE                P442288 
01761          ' REGION ID = '.                                         P442288 
01762          10  ERROR-MSG-020-REGION  PIC X(2)  VALUE '**'.          P442288 
01763          10  ERROR-MSG-020-USING   PIC X(7)  VALUE ' USING '.     P442288 
01764          10  ERROR-MSG-020-ACTRGN  PIC X(2)  VALUE '**'.          P442288 
01765      05  ERROR-MSG-021-TEXT.                                      P442288 
01766          10  FILLER                PIC X(4)  VALUE '621 '.        P442288 
01767          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01768          10  FILLER                PIC X(24) VALUE                P442288 
01769          'NON NUMERIC TRANS KEY = '.                              P442288 
01770          10  ERROR-MSG-021-OFFNO   PIC X(4)  VALUE '**'.          P442288 
01771          10  ERROR-MSG-021-WSID    PIC X(4)  VALUE '**'.          P442288 
01772          10  ERROR-MSG-021-DATE    PIC X(6)  VALUE '**'.          P442288 
01773          10  ERROR-MSG-021-TIME    PIC X(7)  VALUE '**'.          P442288 
01774          10  ERROR-MSG-021-OFFCD   PIC X(2)  VALUE '**'.          P442288 
01775      05  ERROR-MSG-022-TEXT.                                      P442288 
01776          10  FILLER                PIC X(4)  VALUE '622 '.        P442288 
01777          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01778          10  FILLER                PIC X(11) VALUE 'RTS TITLE '.  P442288 
01779          10  ERROR-MSG-022-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01780          10  FILLER                PIC X(33) VALUE                P442288 
01781          ' NON NUMERIC DOCNO REPL BY ZEROES'.                     P442288 
01782      05  ERROR-MSG-023-TEXT.                                      P442288 
01783          10  FILLER                PIC X(4)  VALUE '623 '.        P442288 
01784          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01785          10  FILLER                PIC X(35) VALUE                P442288 
01786          'IRP MASTER FILE I/O ERROR--STATUS='.                    P442288 
01787          10  EMT-ERR-IRPMSTR       PIC X(2).                      P442288 
01788      05  ERROR-MSG-024-TEXT.                                      P442288 
01789          10  FILLER                PIC X(4)  VALUE '624 '.        P442288 
01790          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01791          10  FILLER                PIC X(7)  VALUE 'SUPP # '.     P442288 
01792          10  ERR-MSG-024-SUPP      PIC 9(3)  VALUE ZERO.          P442288 
01793          10  FILLER                PIC X(18) VALUE                P442288 
01794          ' CANNOT BE LOCATED'.                                    P442288 
01795      05  ERROR-MSG-025-TEXT.                                      P442288 
01796          10  FILLER                PIC X(4)  VALUE '625 '.        P442288 
01797          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01798          10  FILLER                PIC X(40) VALUE                P442288 
01799          'THIS JOB SHOULD ONLY BE RUN ON THURSDAY'.               P442288 
01800                                                                   P442288 
01801                                                                   P442288 
01802      05  ERROR-MSG-026-TEXT.                                      P442288 
01803          10  FILLER                PIC X(4)  VALUE '626 '.        P442288 
01804          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01805          10  ERROR-MSG-026-TYPE    PIC X(09) VALUE 'REFUNDED '.   P442288 
01806          10  FILLER                PIC X(34) VALUE                P442288 
01807          ' REGISTRATION NOT FOUND FOR DOC # '.                    P442288 
01808          10  ERROR-MSG-026-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01809      05  ERROR-MSG-027-TEXT.                                      P442288 
01810          10  FILLER                PIC X(4)  VALUE '627 '.        P442288 
01811          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01812          10  ERROR-MSG-027-TYPE    PIC X(04) VALUE 'RTS '.        P442288 
01813          10  FILLER                PIC X(07) VALUE 'RECORD'.      P442288 
01814          10  ERROR-MSG-027-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01815          10  FILLER                PIC X(16) VALUE                P442288 
01816          ' REGISTERED TO: '.                                      P442288 
01817          10  ERROR-MSG-027-PLT     PIC X(8)  VALUE ZERO.          P442288 
01818          10  ERROR-MSG-027-EXP     PIC 9999/99.                   P442288 
01819                                                                   P442288 
01820      05  ERROR-MSG-028-TEXT.                                      P442288 
01821          10  FILLER                PIC X(4)  VALUE '628 '.        P442288 
01822          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01823          10  FILLER                PIC X(10) VALUE 'IRP DOCNO '.  P442288 
01824          10  ERROR-MSG-028-DOCNO-O PIC X(17) VALUE SPACES.        P442288 
01825          10  FILLER                PIC X(16) VALUE                P442288 
01826          '<---REPLACED BY '.                                      P442288 
01827          10  ERROR-MSG-028-DOCNO-N PIC X(17) VALUE SPACES.        P442288 
01828                                                                   P442288 
01829      05  ERROR-MSG-029-TEXT.                                      P442288 
01830          10  FILLER                PIC X(4)  VALUE '629 '.        P442288 
01831          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01832          10  ERROR-MSG-029-OWNER-O PIC X(22) VALUE SPACES.        P442288 
01833          10  FILLER REDEFINES ERROR-MSG-029-OWNER-O.              P442288 
01834              15  EMSG29-FIRST-10-O PIC X(10).                     P442288 
01835              15  FILLER            PIC X(12).                     P442288 
01836          10  FILLER                PIC X(16) VALUE                P442288 
01837          '<---REPLACED BY '.                                      P442288 
01838          10  ERROR-MSG-029-OWNER-N PIC X(22) VALUE SPACES.        P442288 
01839          10  FILLER REDEFINES ERROR-MSG-029-OWNER-N.              P442288 
01840              15  EMSG29-FIRST-10-N PIC X(10).                     P442288 
01841              15  FILLER            PIC X(12).                     P442288 
01842                                                                   P442288 
01843      05  ERROR-MSG-030-TEXT.                                      P442288 
01844          10  FILLER                PIC X(4)  VALUE '630 '.        P442288 
01845          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01846          10  FILLER                PIC X(07) VALUE 'UPDATE '.     P442288 
01847          10  ERROR-MSG-030-MSG     PIC X(11) VALUE 'TRANSMITTED'. P442288 
01848          10  FILLER                PIC X(04) VALUE ' ON '.        P442288 
01849          10  ERROR-MSG-030-CODE    PIC X(11) VALUE                P442288 
01850          'SUPERSEDED '.                                           P442288 
01851          10  FILLER                PIC X(10) VALUE ' TITLE  # '.  P442288 
01852          10  ERROR-MSG-030-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01853                                                                   P442288 
01854      05  ERROR-MSG-031-TEXT.                                      P442288 
01855          10  FILLER                PIC X(4)  VALUE '631 '.        P442288 
01856          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01857          10  FILLER                PIC X(43) VALUE 'XXXXXXXXX'.   P442288 
01858          10  ERROR-MSG-031-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01859                                                                   P442288 
01860      05  ERROR-MSG-032-TEXT.                                      P442288 
01861          10  FILLER                PIC X(4)  VALUE '632 '.        P442288 
01862          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01863          10  FILLER                PIC X(30) VALUE                P442288 
01864          'NO IRP VEHICLE RECORD - KEY = '.                        P442288 
01865          10  ERROR-MSG-032-KEY     PIC X(27) VALUE SPACES.        P442288 
01866                                                                   P442288 
01867      05  ERROR-MSG-033-TEXT.                                      P442288 
01868          10  FILLER                PIC X(4)  VALUE '633 '.        P442288 
01869          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01870          10  ERROR-MSG-033-FILE    PIC X(9)  VALUE 'IRPVEHL  '.   P442288 
01871          10  FILLER                PIC X(5)  VALUE 'FILE '.       P442288 
01872          10  ERROR-MSG-033-FUNC    PIC X(5)  VALUE 'XXXX '.       P442288 
01873          10  FILLER                PIC X(18) VALUE                P442288 
01874          ' ERROR - STATUS = '.                                    P442288 
01875          10  ERROR-MSG-033-STATUS  PIC X(22) VALUE SPACES.        P442288 
01876      05  ERROR-MSG-034-TEXT.                                      P442288 
01877          10  FILLER                PIC X(4)  VALUE '634 '.        P442288 
01878          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01879          10  FILLER                PIC X(45) VALUE                P442288 
01880          ' INVALID NEGATIVE FUNDS (REFLECT, AUTOMATE)'.           P442288 
01881                                                                   P442288 
01882                                                                   P442288 
01883      05  ERROR-MSG-035-TEXT.                                      P442288 
01884          10  FILLER                PIC X(4)  VALUE '635 '.        P442288 
01885          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01886          10  FILLER                PIC X(10) VALUE 'IRP DOCNO '.  P442288 
01887          10  ERROR-MSG-035-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01888          10  FILLER                PIC X(33) VALUE                P442288 
01889          '<---IRP VIN DOES NOT MATCH RTS   '.                     P442288 
01890                                                                   P442288 
01891      05  ERROR-MSG-036-TEXT.                                      P442288 
01892          10  FILLER                PIC X(4)  VALUE '636 '.        P442288 
01893          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01894          10  FILLER                PIC X(10) VALUE 'IRP  DOC: '.  P442288 
01895          10  ERROR-MSG-036-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01896          10  FILLER                PIC X(10) VALUE ' IRP VIN: '.  P442288 
01897          10  ERROR-MSG-036-VIN     PIC X(17) VALUE SPACES.        P442288 
01898                                                                   P442288 
01899      05  ERROR-MSG-037-TEXT.                                      P442288 
01900          10  FILLER                PIC X(4)  VALUE '637 '.        P442288 
01901          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01902          10  FILLER                PIC X(10) VALUE 'RTS  DOC: '.  P442288 
01903          10  ERROR-MSG-037-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01904          10  FILLER                PIC X(10) VALUE ' RTS VIN: '.  P442288 
01905          10  ERROR-MSG-037-VIN     PIC X(17) VALUE SPACES.        P442288 
01906                                                                   P442288 
01907      05  ERROR-MSG-038-TEXT.                                      P442288 
01908          10  FILLER                PIC X(4)  VALUE '638 '.        P442288 
01909          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01910          10  FILLER                PIC X(10) VALUE 'IRP DOCNO '.  P442288 
01911          10  ERROR-MSG-038-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01912          10  FILLER                PIC X(38) VALUE                P442288 
01913          '<---IRP DOCNO (OLD NDX) INVALID KEY '.                  P442288 
01914                                                                   P442288 
01915      05  ERROR-MSG-039-TEXT.                                      P442288 
01916          10  FILLER                PIC X(4)  VALUE '639 '.        P442288 
01917          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01918          10  FILLER                PIC X(10) VALUE 'IRP DOCNO '.  P442288 
01919          10  ERROR-MSG-039-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01920          10  FILLER                PIC X(33) VALUE                P442288 
01921          '<--NOT UPDATED - OLD TITLE IN RTS'.                     P442288 
01922                                                                   P442288 
01923      05  ERROR-MSG-040-TEXT.                                      P442288 
01924          10  FILLER                PIC X(4)  VALUE '640 '.        P442288 
01925          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01926          10  FILLER                PIC X(43) VALUE                   CL**7
01927          ' NULL TRANS - NO FUNDS AND NO MVUPDATE'.                   CL**7
01928          10  ERROR-MSG-040-DOCNO   PIC X(17) VALUE SPACES.           CL**7
01929                                                                   P442288 
01930      05  ERROR-MSG-041-TEXT.                                      P442288 
01931          10  FILLER                PIC X(4)  VALUE '641 '.        P442288 
01932          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01933          10  FILLER                PIC X(10) VALUE 'IRP DOCNO '.  P442288 
01934          10  ERROR-MSG-041-DOCNO-I PIC X(17) VALUE SPACES.        P442288 
01935          10  FILLER                PIC X(16) VALUE                P442288 
01936          '--->REPLACE RTS '.                                      P442288 
01937          10  ERROR-MSG-041-DOCNO-R PIC X(17) VALUE SPACES.        P442288 
01938                                                                   P442288 
01939      05  ERROR-MSG-042-TEXT.                                      P442288 
01940          10  FILLER                PIC X(4)  VALUE '642 '.        P442288 
01941          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01942          10  FILLER                PIC X(08) VALUE 'RTS VIN '.    P442288 
01943          10  ERROR-MSG-042-VIN     PIC X(18) VALUE SPACES.        P442288 
01944          10  FILLER                PIC X(03) VALUE 'ON '.         P442288 
01945          10  ERROR-MSG-042-SOURCE  PIC X(09) VALUE 'INACTIVE '.   P442288 
01946          10  FILLER                PIC X(05) VALUE 'FILE '.       P442288 
01947          10  ERROR-MSG-042-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01948                                                                   P442288 
01949      05  ERROR-MSG-043-TEXT.                                      P442288 
01950          10  FILLER                PIC X(4)  VALUE '643 '.        P442288 
01951          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01952          10  FILLER                PIC X(54) VALUE                P442288 
01953          'DELETED SUPPLEMENT WAS MARKED PAID - NO PROCESSING    '.P442288 
01954                                                                   P442288 
01955      05  ERROR-MSG-044-TEXT.                                      P442288 
01956          10  FILLER                PIC X(4)  VALUE '644 '.        P442288 
01957          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01958          10  FILLER                PIC X(19) VALUE                P442288 
01959          'INTERFACE EXP DATE '.                                   P442288 
01960          10  ERROR-MSG-044-EXPDTA  PIC 9999/99.                   P442288 
01961          10  FILLER                PIC X(13) VALUE                P442288 
01962          ' CHANGE TYPE '.                                         P442288 
01963          10  ERROR-MSG-044-TYPE    PIC X.                         P442288 
01964          10  FILLER                PIC X(11) VALUE                P442288 
01965          ' SHOULD BE '.                                           P442288 
01966          10  ERROR-MSG-044-EXPDTC  PIC 9999/99.                   P442288 
01967       05  ERROR-MSG-045-TEXT.                                     P442288 
01968           10  FILLER                PIC X(4)  VALUE '645 '.       P442288 
01969           10  FILLER                PIC X(3)  VALUE SPACES.       P442288 
01970           10  FILLER                PIC X(54) VALUE               P442288 
01971           'ITEM CODE / REG CLASS CODE UNDETERMINED BY ACCOUNT #'. P442288 
01972      05  ERROR-MSG-046-TEXT.                                      P442288 
01973          10  FILLER                PIC X(4)  VALUE '646 '.        P442288 
01974          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01975          10  FILLER                PIC X(44) VALUE                P442288 
01976          'MONDAY FEATURE - AMDATE ADVANCED ONE DAY TO '.          P442288 
01977          10  ERROR-MSG-046-AMDATE  PIC X(07) VALUE SPACES.        P442288 
01978      05  ERROR-MSG-047-TEXT.                                      P442288 
01979          10  FILLER                PIC X(4)  VALUE '647 '.        P442288 
01980          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01981          10  FILLER                PIC X(13) VALUE                P442288 
01982          'EXPIRED REG: '.                                         P442288 
01983          10  ERROR-MSG-047-PLT     PIC X(8)  VALUE ZERO.          P442288 
01984          10  ERROR-MSG-047-EXP-DT  PIC 9999/99.                   P442288 
01985          10  FILLER                PIC X(4)  VALUE                P442288 
01986          'NOW '.                                                  P442288 
01987          10  ERROR-MSG-047-NEW-EXP PIC 9999/99.                   P442288 
01988          10  FILLER                PIC X     VALUE SPACE.         P442288 
01989          10  ERROR-MSG-047-DOCNO   PIC X(17) VALUE SPACES.        P442288 
01990      05  ERROR-MSG-048-TEXT.                                      P442288 
01991          10  FILLER                PIC X(4)  VALUE '648 '.        P442288 
01992          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01993          10  FILLER                PIC X(34) VALUE                P442288 
01994          'AUDIT REFUND - COUNTY WILL REFUND:'.                    P442288 
01995          10  ERROR-MSG-048-REFUND  PIC Z(5)9.99.                  P442288 
01996      05  ERROR-MSG-049-TEXT.                                      P442288 
01997          10  FILLER                PIC X(4)  VALUE '649 '.           CL*12
01998          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
01999          10  FILLER                PIC X(34) VALUE                P442288 
02000          'TERP  REFUND - BY TX COMPTROLLER :'.                    P442288 
02001          10  ERROR-MSG-049-REFUND  PIC Z(5)9.99.                  P442288 
02002                                                                   P442288 
02003      05  ERROR-MSG-050-TEXT.                                         CL**9
02004          10  FILLER                PIC X(4)  VALUE '650 '.           CL*12
02005          10  FILLER                PIC X(3)  VALUE SPACES.           CL**9
02006          10  ERROR-MSG-050-VIN     PIC X(18) VALUE SPACES.           CL*13
02007          10  ERROR-MSG-050-PLT     PIC X(8)  VALUE ZERO.             CL*13
02008          10  FILLER                PIC X(36) VALUE                   CL*13
02009          'NEWER REG / IRP NOW PREVIOUS PLATE'.                       CL*13
02010      05  ERROR-MSG-051-TEXT.                                         CL**9
02011          10  FILLER                PIC X(4)  VALUE '651 '.           CL*12
02012          10  FILLER                PIC X(3)  VALUE SPACES.           CL**9
02013          10  FILLER                PIC X(23) VALUE                   CL**9
02014          'NO PLATE FOR IRP VIN # '.                                  CL**9
02015          10  ERROR-MSG-051-VIN     PIC X(17) VALUE SPACES.           CL**9
02016          10  FILLER                PIC X(3)  VALUE ' - '.            CL**9
02017          10  ERROR-MSG-051-TITLE   PIC X(17) VALUE SPACES.           CL**9
02018          10  FILLER                PIC X(6)  VALUE ' RGN:'.          CL**9
02019          10  ERROR-MSG-051-RGN     PIC X(03) VALUE SPACES.           CL**9
02020      05  ERROR-MSG-052-TEXT.                                         CL*12
02021          10  FILLER                PIC X(4)  VALUE '652 '.           CL*12
02022          10  FILLER                PIC X(3)  VALUE SPACES.           CL*12
02023          10  FILLER                PIC X(40) VALUE                   CL*12
02024          'TXIRP AUDIT - TRANSACTION BYPASSED      '.                 CL*12
02025      05  ERROR-MSG-053-TEXT.                                         CL*12
02026          10  FILLER                PIC X(4)  VALUE '653 '.           CL*12
02027          10  FILLER                PIC X(3)  VALUE SPACES.           CL*12
02028          10  FILLER                PIC X(40) VALUE                   CL*12
02029          '  AUDITS HANDLED IN A FUTURE ENHANCEMENT'.                 CL*12
02030      05  ERROR-MSG-054-TEXT.                                         CL*14
02031          10  FILLER                PIC X(4)  VALUE '654 '.           CL*14
02032          10  FILLER                PIC X(3)  VALUE SPACES.           CL*14
02033          10  FILLER                PIC X(40) VALUE                   CL*14
02034          ' INVALID NEGATIVE FUNDS - REJECTED TRANS'.                 CL*14
02035      05  ERROR-MSG-055-TEXT.                                         CL*23
02036          10  FILLER                PIC X(4)  VALUE '655 '.           CL*23
02037          10  FILLER                PIC X(3)  VALUE SPACES.           CL*23
02038          10  FILLER                PIC X(50) VALUE                   CL*23
02039          ' INVALID TXIRP GWT ZERO - USED RTS GWT/EWT'.               CL*23
02040                                                                      CL*12
02041      05  ERROR-MSG-092-TEXT.                                      P442288 
02042          10  FILLER                PIC X(4)  VALUE '692 '.        P442288 
02043          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
02044          10  FILLER                PIC X(28) VALUE                P442288 
02045          ' RTS TRANSACTIONS STOPPED = '.                          P442288 
02046          10  ERROR-MSG-092-FUNC    PIC X(20) VALUE                P442288 
02047          'ALL-OK-WHY-STOP'.                                       P442288 
02048      05  ERROR-MSG-093-TEXT.                                      P442288 
02049          10  FILLER                PIC X(4)  VALUE '693 '.        P442288 
02050          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
02051          10  ERROR-MSG-093-FILE    PIC X(09) VALUE 'FILENAME '.   P442288 
02052          10  FILLER                PIC X(28) VALUE                P442288 
02053          ' FILE HELD BY ANOTHER JOB - '.                          P442288 
02054      05  ERROR-MSG-093B-TEXT.                                     P442288 
02055          10  FILLER                PIC X(4)  VALUE '693 '.        P442288 
02056          10  FILLER                PIC X(3)  VALUE SPACES.        P442288 
02057          10  FILLER                PIC X(40) VALUE                P442288 
02058          '==> CLOSE CICS FILES AND RERUN THIS JOB'.               P442288 
02059         EJECT                                                     P442288 
02060         EJECT                                                     P442288 
02061                                                                   P442288 
02062  01   WS-SWITCHES.                                                P442288 
02063       05  RTS-IF-RUN-TYPE          PIC X VALUE 'N'.               P442288 
02064           88  RTS-SYNC-REG-UPDATE-RUN    VALUE 'S'.               P442288 
02065           88  RTS-NORM-IRP-UPDATE-RUN    VALUE 'N' 'T' 'M'.          CL*17
02066           88  RTS-NORM-IRP-MON-OVERRIDE  VALUE 'M'.                  CL*17
02067       05  RTS-OVERRIDE-INQ-DBID    PIC X VALUE 'N'.               P442288 
02068           88  OVERRIDE-RTS-INQ-DBID      VALUE 'Y' '2' '5'.       P442288 
02069       05  TESTING-DISPLAYS-SW      PIC X VALUE 'N'.               P442288 
02070           88   TESTING-SHOW-DISPLAYS     VALUE 'D' 'Y'.           P442288 
02071       05  RTS-IRP-UPDATE-BYPASS-SW PIC X VALUE 'X'.               P442288 
02072           88  NO-UPDATES-RUN             VALUE 'N'.               P442288 
02073           88  NO-ADABAS-WRITE-RUN        VALUE 'N'.               P442288 
02074           88  NO-IRP-UPDATE-RUN          VALUE 'I'.               P442288 
02075           88  BYPASS-RTS-STOP-CHECK      VALUE 'B'.               P442288 
02076       05  RTS-IRP-RESTART-SW       PIC X VALUE 'X'.               P442288 
02077           88  RTS-TRANS-RESTART          VALUE 'R'.               P442288 
02078       05  CONTROL-RECORDS-SW       PIC X VALUE 'Y'.               P442288 
02079           88   MORE-CONTROL-RECORDS      VALUE 'Y'.               P442288 
02080           88   NO-MORE-CONTROL-RECORDS   VALUE 'N'.               P442288 
02081       05  PROCESS-EDIT-CHECK-SW    PIC X VALUE 'N'.               P442288 
02082           88   EDIT-TEST-OK              VALUE 'Y'.               P442288 
02083           88   EDIT-TEST-FAILED          VALUE 'N'.               P442288 
02084       05  BATCH-RECORD-SW          PIC X VALUE 'N'.               P442288 
02085           88   NO-MORE-BTCH-RECORDS      VALUE 'Y'.               P442288 
02086       05  WS-BATCH-NOTITLE-FLAG    PIC 9 VALUE  ZERO.             P442288 
02087           88   BATCH-NO-TITLE            VALUE  1.                P442288 
02088       05  SW-REGPLATE             PIC 9 COMP-3.                   P442288 
02089           88  REGPLATE-FOUND  VALUE 1.                            P442288 
02090 *---------------------------------------------------------------* P442288 
02091 *        ERROR-PROCESSING-SW CONTROLS WHAT INFORMATION IS TO BE * P442288 
02092 *        PLACED ON THE ERROR MESSAGE DETAIL LINE.               * P442288 
02093 *---------------------------------------------------------------* P442288 
02094       05  ERROR-PROCESSING-SW      PIC 9.                         P442288 
02095           88  NORMAL-ERROR               VALUE  1.                P442288 
02096           88  FILE-IO-ERROR              VALUE  2.                P442288 
02097           88  BATCH-CNTL-ERROR           VALUE  3.                P442288 
02098           88  SHOW-RGN-IF-KEY-ON-ERROR   VALUE  4.                P442288 
02099 *---------------------------------------------------------------* P442288 
02100 *        IF RTS-TRANS-ERROR-FLAG > 0 :                          * P442288 
02101 *            THEN NO RTS TRANSACTION WILL BE WRITTEN            * P442288 
02102 *---------------------------------------------------------------* P442288 
02103 *---------------------------------------------------------------* P442288 
02104 *        RTS-TRANS-ERROR-FLAG CONTROLS WHICH, IF ANY, RTS TRANS-* P442288 
02105 *        ACTION RECORDS WILL BE WRITTEN.                        * P442288 
02106 *---------------------------------------------------------------* P442288 
02107       05  RTS-TRANS-ERROR-FLAG     PIC 9 VALUE  ZERO.             P442288 
02108           88   RTS-NO-ERROR              VALUE  ZERO.             P442288 
02109           88   RTS-CHANGE-FLAG           VALUE  2.                P442288 
02110           88   RTS-TRANS-KEY-ERROR       VALUE  3.                P442288 
02111           88   RTS-BYPASS-FLAG           VALUE  9.                P442288 
02112 *---------------------------------------------------------------* P442288 
02113 *        RTS-UPDT-ERROR-FLAG > 0    -----    INDICATES THAT     * P442288 
02114 *        RTS WILL NOT COMPLETELY PROCESS THE TRANSACTIONS THAT  * P442288 
02115 *        IS BEING SENT.  THERE IS SOMETHING WRONG.  GENREALLY IT* P442288 
02116 *        IS THAT A DOCNO RECORD COULD NOT BE LOCATED.  THE FUNDS* P442288 
02117 *        RECORDS SHOULD BE PROCESSED CORRECTLY, WHICH MEANS THAT* P442288 
02118 *        THE COUNTY IRP FUNDS REPORT SHOULD REFLECT ALL MONIES  * P442288 
02119 *        ASSOCIATED WITH A PAID SUPPLEMENT.                     * P442288 
02120 *---------------------------------------------------------------* P442288 
02121       05  RTS-UPDT-ERROR-FLAG      PIC 9 VALUE  ZERO.             P442288 
02122           88   RTS-VALID-UPDATE          VALUE  0.                P442288 
02123           88   RTS-NO-TITLE              VALUE  1.                P442288 
02124           88   RTS-NO-TITLE-DUMMY        VALUE  2.                P442288 
02125           88   RTS-REDUNDANT-UPD         VALUE  5.                P442288 
02126           88   RTS-NOGEN-MV-FUNC         VALUE  3 THRU 5.         P442288 
02127           88   RTS-RENTAL-TRAILER-REC    VALUE  4.                P442288 
02128           88   RTS-INVALID-UPDATE        VALUE  1 THRU 5.         P442288 
02129       05  RTS-IRPINV-FLAG          PIC X VALUE  SPACES.           P442288 
02130                                                                   P442288 
02131  01  HDR-0-PARAMETERS.                                            P442288 
02132      05  FILLER                    PIC X(15) VALUE                P442288 
02133          '  PARM LENGTH: '.                                       P442288 
02134      05  RPT-HDR-0-LENGTH          PIC Z9.                        P442288 
02135      05  FILLER                    PIC X(09) VALUE                P442288 
02136          ', PARMS: '.                                             P442288 
02137      05  RPT-HDR-0-PARMS           PIC X(60) VALUE SPACES.        P442288 
02138      05  FILLER REDEFINES RPT-HDR-0-PARMS.                        P442288 
02139          10  SHOW-RUN-TYPE                 PIC X.                 P442288 
02140          10  SHOW-INPUT-DBID               PIC X.                 P442288 
02141          10  SHOW-DISPLAY-TEST             PIC X.                 P442288 
02142          10  SHOW-SELECT-UPDATE            PIC X.                 P442288 
02143          10  SHOW-THIS-DAY-OF-WEEK         PIC X(3).              P442288 
02144          10  SHOW-THIS-YYYYMMDD            PIC 9(8).              P442288 
02145          10  SHOW-THIS-YYDDD               PIC 9(5).              P442288 
02146          10  FILLER                        PIC X(40).             P442288 
02147      05  FILLER REDEFINES RPT-HDR-0-PARMS.                        P442288 
02148          10  RPT-HDR-0-BYTE        PIC X OCCURS 60 TIMES.         P442288 
02149  01   FILLER                       PIC X(42) VALUE                P442288 
02150       'PROGRAM P442288  WORKING STORAGE ENDS HERE'.               P442288 
02151      EJECT                                                        P442288 
02152 *****************                                                 P442288 
02153  LINKAGE SECTION.                                                 P442288 
02154 *****************                                                 P442288 
02155                                                                   P442288 
02156  01  P442288-LINKAGE-AREA.                                        P442288 
02157      05  LINK-AREA-LENGTH                  PIC S9(4) COMP.        P442288 
02158      05  LINK-VALUES.                                             P442288 
02159          10  LINK-RUN-TYPE                 PIC X.                 P442288 
02160          10  LINK-INPUT-DBID               PIC X.                 P442288 
02161          10  LINK-DISPLAY-TEST             PIC X.                 P442288 
02162          10  LINK-NO-UPDATE-TRANS          PIC X.                 P442288 
02163          10  LINK-YYYYMMDD                 PIC 9(8).              P442288 
02164          10  LINK-XXXX                     PIC X(4).              P442288 
02165      05  FILLER REDEFINES LINK-VALUES.                            P442288 
02166          10  LINK-BYPASS-THR-CHECK.                               P442288 
02167              15  FILLER                    PIC X(3).              P442288 
02168              15  LINK-RESTART              PIC X.                 P442288 
02169              15  FILLER                    PIC X(4).              P442288 
02170          10  FILLER                        PIC X(04).             P442288 
02171          10  FILLER                        PIC X(04).             P442288 
02172                                                                   P442288 
02173      05  FILLER REDEFINES LINK-VALUES.                            P442288 
02174          10  PARM-BYTE OCCURS 16 TIMES     PIC X.                 P442288 
02175                                                                   P442288 
02176                                                                   P442288 
02177 ******************************************************************P442288 
02178  PROCEDURE DIVISION USING P442288-LINKAGE-AREA.                   P442288 
02179 ******************************************************************P442288 
02180                                                                   P442288 
02181                                                                   P442288 
02182  0-MAINLINE-PROCESSING-P442288.                                   P442288 
02183                                                                   P442288 
02184      MOVE WHEN-COMPILED     TO  WS-WHEN-COMPILED.                 P442288 
02185      MOVE WS-COMP-DATE      TO  RTS-VER-DATE.                     P442288 
02186      MOVE WS-COMP-TIME      TO  RTS-VER-TIME.                     P442288 
02187                                                                   P442288 
02188      PERFORM 00-CHECK-LINKAGE-PARMS.                              P442288 
02189      IF TESTING-SHOW-DISPLAYS                                     P442288 
02190          DISPLAY ' ****  EXECUTING : ' WS-THIS-PGM                P442288 
02191                  ' - LAST VERSION COMPILED ON: ' WS-COMP-DATE     P442288 
02192                  ' '  WS-COMP-TIME  ' ****'.                      P442288 
02193                                                                   P442288 
02194      PERFORM 1-INITIALIZATION-MODULE.                             P442288 
02195      IF RTS-SYNC-REG-UPDATE-RUN                                   P442288 
02196          MOVE 'R'                      TO BTCH-BILLING-RECAP-DEP  P442288 
02197          MOVE '99000'                  TO BTCH-NUMBER             P442288 
02198      ELSE                                                         P442288 
02199          MOVE 'R'                      TO BTCH-BILLING-RECAP-DEP  P442288 
02200          MOVE ZEROS                    TO BTCH-NUMBER.            P442288 
                                                                                
      D    DISPLAY 'INITIAL BTCH-KEY <' BTCH-KEY '>'.                           
                                                                                
02201      START  IRP-BATCH-FILE KEY GREATER THAN BTCH-KEY.             P442288 
      D    PERFORM Z-7-WRKRTS-FILE-STATUS-CK.                           P442288 
02202                                                                   P442288 
02203      IF TESTING-SHOW-DISPLAYS                                     P442288 
02204          DISPLAY '@0-MAINLINE...START RTS BATCH FILE STATUS='     P442288 
02205              BATCH-FILE-STATUS ' FOR KEY=' BTCH-KEY               P442288 
02206      END-IF.                                                      P442288 
02207                                                                   P442288 
02208                                                                   P442288 
02209      MOVE 'N'                      TO BATCH-RECORD-SW             P442288 
02210      PERFORM Z-1-READ-IRP-BATCH-FILE.                             P442288 
02211      PERFORM 2-MAIN-LOOP THRU                                     P442288 
02212              2-MAIN-LOOP-EXIT                                     P442288 
02213          UNTIL NO-MORE-BTCH-RECORDS.                              P442288 
02214                                                                   P442288 
02215      MOVE WS-DUMMY-RECORDS-COUNT   TO ERR-MSG-019-COUNT.          P442288 
02216      MOVE 1                        TO ERROR-PROCESSING-SW.        P442288 
02217      MOVE ERROR-MSG-019B-TEXT      TO ERR-ERROR-MESSAGE-1.        P442288 
02218      PERFORM Z-4-CONTROL-EDIT-ERROR.                              P442288 
02219      MOVE WS-UPIRP-RECORDS-COUNT   TO ERR-MSG-019C-COUNT1.        P442288 
02220      MOVE WS-UPIRP-DUMMY-COUNT     TO ERR-MSG-019C-COUNT2.        P442288 
02221      MOVE 1                        TO ERROR-PROCESSING-SW.        P442288 
02222      MOVE ERROR-MSG-019C-TEXT      TO ERR-ERROR-MESSAGE-1.        P442288 
02223      PERFORM Z-4-CONTROL-EDIT-ERROR.                              P442288 
02224 *---------------------------------------------------------------* P442288 
02225 *    REMOVE THIS FOR NOW....TOO MANY DUMMYS FROM NON-IMPLEMENTED* P442288 
02226 *    COUNTIES.....REPLACE IT AFTER COMPLETE CONVERSION TO RTS   * P442288 
02227 *---------------------------------------------------------------* P442288 
02228      MOVE WS-RTS-NO-PR-BATCHES     TO ERR-MSG-000-BATCHES.        P442288 
02229      MOVE WS-RTS-NO-PR-TRANS       TO ERR-MSG-000-TRANS.          P442288 
02230      MOVE 1                        TO ERROR-PROCESSING-SW.        P442288 
02231      MOVE ERROR-MSG-000-TEXT       TO ERR-ERROR-MESSAGE-1.        P442288 
02232      PERFORM Z-4-CONTROL-EDIT-ERROR.                              P442288 
02233      MOVE SPACES                   TO ERR-LICN.                   P442288 
02234                                                                   P442288 
02235      IF TESTING-SHOW-DISPLAYS                                     P442288 
02236          DISPLAY 'TOTAL # OF BATCHES:    '  WS-RTS-NO-BATCHES     P442288 
02237          DISPLAY 'TOTAL # PROCESSED :    '  WS-RTS-NO-PR-BATCHES  P442288 
02238          DISPLAY 'TOTAL # OF RTS COUNT:  '  WS-RTS-NO-PR-TRANS    P442288 
02239          DISPLAY '------------------'                             P442288 
02240          DISPLAY 'END OF P442288'.                                P442288 
02241      PERFORM Z-Z-TERMINATION.                                     P442288 
02242                                                                   P442288 
02243 ******GOBACK.                                                        CL*22
02244 ******EJECT                                                          CL*22
02245                                                                   P442288 
02246  00-CHECK-LINKAGE-PARMS.                                          P442288 
02247                                                                   P442288 
      D    DISPLAY 'LINK-AREA-LENGTH <' LINK-AREA-LENGTH '>'            P442288 
      D    DISPLAY 'LINK-RUN-TYPE <' LINK-RUN-TYPE '>'                  P442288 
      D    DISPLAY 'LINK-INPUT-DBID <' LINK-INPUT-DBID '>'              P442288 
      D    DISPLAY 'LINK-DISPLAY-TEST <' LINK-DISPLAY-TEST '>'          P442288 
      D    DISPLAY 'LINK-NO-UPDATE-TRANS <' LINK-NO-UPDATE-TRANS '>'    P442288 
      D    DISPLAY 'LINK-YYYYMMDD <' LINK-YYYYMMDD '>'                  P442288 
      D    DISPLAY 'LINK-XXXX <' LINK-XXXX '>'                          P442288 
      D    DISPLAY 'LINK-RESTART <' LINK-RESTART '>'                    P442288 
                                                                                
02248      IF LINK-AREA-LENGTH > 2                                      P442288 
02249          IF LINK-DISPLAY-TEST = 'Y' OR 'D'                        P442288 
02250              MOVE 'D'              TO TESTING-DISPLAYS-SW.        P442288 
02251                                                                   P442288 
02252      IF LINK-AREA-LENGTH > ZERO                                   P442288 
02253          MOVE LINK-AREA-LENGTH     TO RPT-HDR-0-LENGTH            P442288 
02254          PERFORM 001-SHOW-PARMS                                   P442288 
02255              VARYING WS-INDEX1                                    P442288 
02256                  FROM 1                                           P442288 
02257                  BY 1                                             P442288 
02258                  UNTIL WS-INDEX1 > LINK-AREA-LENGTH               P442288 
02259          IF TESTING-SHOW-DISPLAYS                                 P442288 
02260              DISPLAY ' PARM LENGTH ' LINK-AREA-LENGTH             P442288 
02261                      ' INDEX VALUE ' WS-INDEX1                    P442288 
02262              DISPLAY HDR-0-PARAMETERS.                            P442288 
02263                                                                   P442288 
02264      IF LINK-AREA-LENGTH > 0                                      P442288 
                                                                                
      D    DISPLAY 'RTS-IF-RUN-TYPE <' RTS-IF-RUN-TYPE '>'.                     
                                                                                
02265          IF LINK-RUN-TYPE     = 'N' OR                            P442288 
02266             (LINK-AREA-LENGTH > 7 AND                             P442288 
02267              LINK-BYPASS-THR-CHECK = 'THURSDAY')                  P442288 
                                                                                
      D        DISPLAY '!TC006A - IF LINK-RUN-TYPE = N <'                       
      D                LINK-RUN-TYPE '>'                                        
      D        DISPLAY ' OR (LINK-AREA-LENGTH <' LINK-AREA-LENGTH '> '          
      D                '  > 7 AND LINK-BYPASS-THR-CHECK <'                      
      D                           LINK-BYPASS-THR-CHECK '> '                    
      D                ' = THURSDAY)'                                           
                                                                                
02268              MOVE LINK-RUN-TYPE    TO RTS-IF-RUN-TYPE             P442288 
02269              DISPLAY 'RUN TYPE "'  RTS-IF-RUN-TYPE '" SPECIFIED'  P442288 
02270                      ' - NORMAL PROCESSING RUN'                   P442288 
02271          ELSE                                                     P442288 
02272          IF LINK-RUN-TYPE     = 'S'                               P442288 
                                                                                
      D        DISPLAY '!TC006B - IF LINK-RUN-TYPE = N <'                       
      D                LINK-RUN-TYPE '>'                                        
                                                                                
02273              MOVE LINK-RUN-TYPE    TO RTS-IF-RUN-TYPE             P442288 
02274              DISPLAY 'RUN TYPE "'  RTS-IF-RUN-TYPE '" SPECIFIED'  P442288 
02275                      ' - SYNCHRONIZE REGISTRATION RUN'            P442288 
02276          ELSE                                                        CL*17
02277          IF LINK-RUN-TYPE     = 'M'                                  CL*17
                                                                                
      D        DISPLAY '!TC006B - IF LINK-RUN-TYPE = M <'                       
      D                LINK-RUN-TYPE '>'                                        
                                                                                
02278              MOVE LINK-RUN-TYPE    TO RTS-IF-RUN-TYPE                CL*17
02279              DISPLAY ' MONDAY BYPASS AMDATE INCR (TESTING) : '       CL*17
02280                  LINK-RUN-TYPE ' - DEFAULT TO NORMAL PROCESS'        CL*17
02281          ELSE                                                     P442288 
                                                                                
      D     DISPLAY '!TC006D - IF NOT LINK-RUN-TYPE = M N S THURSDAY <'         
      D                LINK-RUN-TYPE '>'                                        
                                                                                
02282              MOVE 'N'              TO RTS-IF-RUN-TYPE             P442288 
02283              DISPLAY ' INVALID RUN TYPE SPECIFIED : '             P442288 
02284                  LINK-RUN-TYPE ' - DEFAULT TO NORMAL PROCESS'     P442288 
02285      END-IF.                                                      P442288 
02286                                                                   P442288 
02287                                                                   P442288 
02288 *---------------------------------------------------------------* P442288 
02289 *  ? SET DEFAULT TO PRODUCTION DBID - USE UNLESS OVERRIDE       * P442288 
02290 *  ? SHOULD THIS BE IN THE P442276 VERSION ALSO ? ? ?           * P442288 
02291 *---------------------------------------------------------------* P442288 
02292 *    MOVE 141                      TO WS-INQUIRY-ADABAS-DBID.   * P442288 
02293 *    MOVE '2'                      TO RTS-OVERRIDE-INQ-DBID.    * P442288 
02294 *---------------------------------------------------------------* P442288 
02295      IF LINK-AREA-LENGTH > 1                                      P442288 
02296          IF LINK-INPUT-DBID NUMERIC                               P442288 
02297              IF LINK-INPUT-DBID = '2'                             P442288 
02298                  MOVE 141          TO WS-INQUIRY-ADABAS-DBID      P442288 
02299                  MOVE '2'          TO RTS-OVERRIDE-INQ-DBID       P442288 
02300              ELSE                                                 P442288 
02301              IF LINK-INPUT-DBID = '3'                             P442288 
02302                  MOVE 193          TO WS-INQUIRY-ADABAS-DBID      P442288 
02303                  MOVE '3'          TO RTS-OVERRIDE-INQ-DBID       P442288 
02304              ELSE                                                 P442288 
02305              IF LINK-INPUT-DBID = '5'                             P442288 
02306                  MOVE 194          TO WS-INQUIRY-ADABAS-DBID      P442288 
02307                  MOVE '5'          TO RTS-OVERRIDE-INQ-DBID       P442288 
02308      END-IF.                                                      P442288 
02309                                                                   P442288 
      D    MOVE 196          TO WS-INQUIRY-ADABAS-DBID                  P442288 
      D    MOVE '8'          TO RTS-OVERRIDE-INQ-DBID                   P442288 
                                                                                
02310      IF LINK-AREA-LENGTH > 3              AND                     P442288 
02311         (LINK-RUN-TYPE     = 'N' OR 'S')  AND                     P442288 
02312         LINK-RESTART = 'R'                                        P442288 
02313          MOVE 'R'                      TO RTS-IRP-RESTART-SW.     P442288 
02314      IF LINK-AREA-LENGTH > 3                                      P442288 
                                                                                
      D    DISPLAY '!TC004 - <' LINK-NO-UPDATE-TRANS '>'                        
                                                                                
02315          MOVE LINK-NO-UPDATE-TRANS   TO RTS-IRP-UPDATE-BYPASS-SW  P442288 
02316          IF TESTING-SHOW-DISPLAYS                                 P442288 
02317              DISPLAY ' NO-UPDATE-RUN FLAG = '                     P442288 
02318                  '"' RTS-IRP-UPDATE-BYPASS-SW '"'.                P442288 
02319                                                                   P442288 
02320      IF TESTING-SHOW-DISPLAYS                                     P442288 
02321      DISPLAY ' ************************************************'  P442288 
02322      DISPLAY ' INQUIRY  ADABAS DBID = ' WS-INQUIRY-ADABAS-DBID    P442288 
02323      DISPLAY ' INQUIRY  ADABAS DBID = WILL BE OVERRIDEN BY EX PRM'P442288 
02324      DISPLAY ' UPDATE   ADABAS DBID = MUST BE SPECIFIED BY DDCARD'P442288 
02325      DISPLAY ' ************************************************'  P442288 
02326      END-IF.                                                      P442288 
02327 *---------------------------------------------------------------* P442288 
02328 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
02329 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
02330 *---------------------------------------------------------------* P442288 
02331      IF OVERRIDE-RTS-INQ-DBID                                     P442288 
02332          DISPLAY 'INQUIRY DBID OVERRIDE '                         P442288 
02333              WS-INQUIRY-ADABAS-DBID                               P442288 
02334          DISPLAY '------------------------'.                      P442288 
02335                                                                   P442288 
02336  001-SHOW-PARMS.                                                  P442288 
02337      MOVE PARM-BYTE (WS-INDEX1)  TO RPT-HDR-0-BYTE (WS-INDEX1).   P442288 
02338                                                                   P442288 
02339      EJECT                                                        P442288 
02340                                                                   P442288 
02341  1-INITIALIZATION-MODULE.                                         P442288 
02342                                                                   P442288 
02343 ******************************************************************P442288 
02344 *    THIS MODULE OPENS ALL FILES, SETS THE CURRENT DATE          *P442288 
02345 ******************************************************************P442288 
02346      MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CRNT-DATE-DT.          P442288 
02347      MOVE EXPLORE-TXIRP-TRANS-CODES  TO C-TRANS-CODE.             P442288 
02348                                                                   P442288 
02349      IF LINK-AREA-LENGTH > 11                                     P442288 
02350          IF LINK-YYYYMMDD NUMERIC                                 P442288 
02351              MOVE LINK-YYYYMMDD    TO WS-CRNT-DATE-YYYYMMDD       P442288 
02352              MOVE WS-TIME          TO WS-TRANSTIME                P442288 
02353              DISPLAY 'TEST CURRENT DATE=' WS-CRNT-DATE-YYYYMMDD   P442288 
02354                                           ' TIME = ' WS-TRANSTIME P442288 
02355          ELSE                                                     P442288 
02356              DISPLAY 'INVALID DATE BYPASSED =' LINK-YYYYMMDD      P442288 
02357      END-IF.                                                      P442288 
02358                                                                   P442288 
02359      MOVE   WS-CRNT-DATE-MM          TO MM.                       P442288 
02360      MOVE   WS-CRNT-DATE-DD          TO DD.                       P442288 
02361      MOVE   WS-CRNT-DATE-YYYY        TO YYYY.                     P442288 
02362                                                                   P442288 
02363      MOVE FUNCTION CURRENT-DATE(9:6) TO WS-TIME.                  P442288 
02364      MOVE WS-TIME                    TO WS-TRANSTIME.             P442288 
02365      IF RTS-TRANS-RESTART                                         P442288 
02366          MOVE WS-TIME                TO WS-TRANSTIME              P442288 
02367      ELSE                                                         P442288 
02368      IF WS-TRANSTIME > 170000                                     P442288 
02369          MOVE 000001                 TO WS-TRANSTIME.             P442288 
      D    DISPLAY 'WS-TRANSTIME <' WS-TRANSTIME '>'.                           
02370                                                                   P442288 
02371 *---------------------------------------------------------------* P442288 
02372 * CALL CEEDAYS TO CONVERT CURRENT DATE TO LILIAN REPRESENTATION.* P442288 
02373 *---------------------------------------------------------------* P442288 
02374      MOVE 8                  TO VSTRING-LENGTH OF IN-DATE.        P442288 
02375      MOVE WS-CRNT-DATE-DT    TO VSTRING-TEXT OF IN-DATE(1:8).     P442288 
02376      MOVE 8                  TO VSTRING-LENGTH OF PICSTR.         P442288 
02377      MOVE 'YYYYMMDD'         TO VSTRING-TEXT OF PICSTR(1:8).      P442288 
02378                                                                   P442288 
02379      CALL 'CEEDAYS' USING IN-DATE                                 P442288 
02380                           PICSTR                                  P442288 
02381                           LILIAN                                  P442288 
02382                           FC.                                     P442288 
02383                                                                   P442288 
02384      IF CEE000 OF FC                                              P442288 
      D        MOVE LILIAN TO WS-LILIAN                                         
      D        DISPLAY 'WS-LILIAN <' WS-LILIAN '>'                              
02386      ELSE                                                         P442288 
02387          DISPLAY ' ' WS-THIS-PGM ' CEEDAYS FAILED WITH MSG '      P442288 
02388              MSG-NO OF FC                                         P442288 
02389          CALL 'ILBOABN0'.                                         P442288 
02390                                                                   P442288 
02391 *---------------------------------------------------------------* P442288 
02392 * SPECIFY PICTURE STRING THAT DESCRIBES THE DESIRED FORMAT OF   * P442288 
02393 * THE OUTPUT FROM CEEDATE AND THE PICTURE STRING LENGTH.        * P442288 
02394 *---------------------------------------------------------------* P442288 
02395      MOVE 5                  TO VSTRING-LENGTH OF PICSTR.         P442288 
02396      MOVE 'YYDDD'            TO VSTRING-TEXT OF PICSTR(1:5).      P442288 
02397                                                                   P442288 
02398 *---------------------------------------------------------------* P442288 
02399 * CALL CEEDATE TO CONVERT THE LILIAN DATE TO A PICTURE STRING.  * P442288 
02400 *---------------------------------------------------------------* P442288 
      D        MOVE LILIAN TO WS-LILIAN                                         
      D        DISPLAY 'BEFORE CEEDATE - WS-LILIAN <' WS-LILIAN '>'             
      D        DISPLAY 'CHRDATE <' CHRDATE '>'                                  
                                                                                
02401      CALL 'CEEDATE' USING LILIAN                                  P442288 
02402                           PICSTR                                  P442288 
02403                           CHRDATE                                 P442288 
02404                           FC.                                     P442288 
02405                                                                   P442288 
02406      IF CEE000 OF FC                                              P442288 
02407          MOVE CHRDATE            TO WS-CRNT-DATE-YYDDD            P442288 
                                                                                
      D        MOVE LILIAN TO WS-LILIAN                                         
      D        DISPLAY 'AFTER CEEDATE - WS-LILIAN <' WS-LILIAN '>'              
      D        DISPLAY 'CHRDATE <' CHRDATE '>'                                  
                                                                                
02408      ELSE                                                         P442288 
02409          DISPLAY ' ' WS-THIS-PGM ' CEEDATE FAILED WITH MSG '      P442288 
02410              MSG-NO OF FC                                         P442288 
02411          CALL 'ILBOABN0'.                                         P442288 
02412                                                                   P442288 
02413 *---------------------------------------------------------------* P442288 
02414 * CALL CEEDYWK USING LILIAN DATE TO GET DAY-OF-WEEK.            * P442288 
02415 * 1=SUN, 2=MON, 3=TUE, .... 7=SAT                               * P442288 
02416 *---------------------------------------------------------------* P442288 
02417      CALL 'CEEDYWK' USING LILIAN                                  P442288 
02418                           DAYNUM                                  P442288 
02419                           FC.                                     P442288 
02420                                                                   P442288 
02421      IF CEE000 OF FC                                              P442288 
02422          MOVE DAYNUM             TO WS-DAY-OF-WEEK                P442288 
      D        MOVE LILIAN TO WS-LILIAN                                         
      D        DISPLAY 'WS-LILIAN <' WS-LILIAN '>'                              
02423      ELSE                                                         P442288 
02424          DISPLAY ' ' WS-THIS-PGM ' CEEDYWK FAILED WITH MSG '      P442288 
02425              MSG-NO OF FC                                         P442288 
02426          CALL 'ILBOABN0'.                                         P442288 
02427                                                                   P442288 
02428      IF TESTING-SHOW-DISPLAYS                                     P442288 
02429          DISPLAY ' ****  TODAYS DATE = ' WS-CURRENT-DATE-DT       P442288 
02430                  ' JULIAN DAY = ' WS-CRNT-DATE-DDD                P442288 
02431                  ' DAY-OF-WEEK NUM = ' WS-DAY-OF-WEEK             P442288 
02432                  ' TIME ' WS-TIME                                 P442288 
02433          IF TODAY-IS-MONDAY                                       P442288 
02434              DISPLAY ' ****  TODAY IS -- MONDAY -- '              P442288 
02435          ELSE                                                     P442288 
02436          IF TODAY-IS-TUESDAY                                      P442288 
02437              DISPLAY ' ****  TODAY IS -- TUESDAY -- '             P442288 
02438          ELSE                                                     P442288 
02439          IF TODAY-IS-WEDNESDAY                                    P442288 
02440              DISPLAY ' ****  TODAY IS -- WEDNESDAY -- '           P442288 
02441          ELSE                                                     P442288 
02442          IF TODAY-IS-THURSDAY                                     P442288 
02443              DISPLAY ' ****  TODAY IS -- THURSDAY -- '            P442288 
02444          ELSE                                                     P442288 
02445          IF TODAY-IS-FRIDAY                                       P442288 
02446              DISPLAY ' ****  TODAY IS -- FRIDAY -- '              P442288 
02447          ELSE                                                     P442288 
02448          IF TODAY-IS-SATURDAY                                     P442288 
02449              DISPLAY ' ****  TODAY IS -- SATURDAY -- '            P442288 
02450          ELSE                                                     P442288 
02451          IF TODAY-IS-SUNDAY                                       P442288 
02452              DISPLAY ' ****  TODAY IS -- SUNDAY -- '              P442288 
02453          ELSE                                                     P442288 
02454              DISPLAY ' ****  TODAY IS -- UNKNOWN ** ERROR -- '    P442288 
02455          END-IF.                                                  P442288 
02456                                                                   P442288 
02457      MOVE WS-DAYS-TABLE(WS-DAY-OF-WEEK)                           P442288 
02458                                      TO UPDAT-CURRENT-DAY-WK      P442288 
02459                                         MVUPD-CURRENT-DAY-WK      P442288 
02460                                         MSGOUT-CURRENT-DAY-WK     P442288 
02461                                         ERROR-CURRENT-DAY-WK.     P442288 
02462                                                                   P442288 
02463      OPEN OUTPUT PROCESS-CONTROL-REPORT.                          P442288 
02464      OPEN OUTPUT PROCESS-UPDATE-REPORT.                           P442288 
02465      OPEN OUTPUT PROCESS-MVUPDT-REPORT.                           P442288 
02466      OPEN OUTPUT PROCESS-MSGOUT-REPORT.                           P442288 
02467      OPEN OUTPUT TRANSMITTAL-RPT-FILE.                            P442288 
02468      OPEN OUTPUT UPDATES-ARCHIVE-FILE.                            P442288 
02469      OPEN I-O    IRP-BATCH-FILE.                                  P442288 
02470      OPEN I-O    WORK-FD-INTERFACE-FILE.                          P442288 
02471      OPEN I-O    INTERFACE-RESTART.                               P442288 
02472                                                                   P442288 
02473      MOVE 'OPEN'                   TO ERROR-MSG-033-FUNC.         P442288 
02474                                                                   P442288 
02475 *---------------------------------------------------------------* P442288 
02476 *    CHECK FOR RTS TRANSACTION STOP                             * P442288 
02477 *    THIS CAN BE BYPASSED WHEN TESTING                          * P442288 
02478 *---------------------------------------------------------------* P442288 
02479      IF BYPASS-RTS-STOP-CHECK                                     P442288 
                                                                                
      D        DISPLAY '!TC005A - BYPASS-RTS-STOP-CHECK(B) <'                   
                                        RTS-IRP-UPDATE-BYPASS-SW '>'            
                                                                                
02480 *        NEXT SENTENCE                                            P442288 
02481      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC005B - IF NOT BYPASS-RTS-STOP-CHECK(B) <'            
                                        RTS-IRP-UPDATE-BYPASS-SW '>'            
                                                                                
02482          PERFORM 11-BRAKE-CHECK THRU                              P442288 
02483                  11-BRAKE-CHECK-EXIT.                             P442288 
02484                                                                   P442288 
02485                                                                   P442288 
      D    DISPLAY 'INSIDE 1-INITIALIZATION-MODULE'                             
02486      PERFORM Z-6-VSAM-FILE-STATUS-CK.                             P442288 
02487      PERFORM Z-7-WRKRTS-FILE-STATUS-CK.                           P442288 
02488                                                                   P442288 
02489 *---------------------------------------------------------------* P442288 
02490 *        THE TIME DELAY HAS BEEN REMOVED...                     * P442288 
02491 *        .NO NEED FOR WEEK-END ADVANCE                          * P442288 
02492 *            IF LINK-DAY-OF-WEEK = 'MON' OR                     * P442288 
02493 *               LINK-DAY-OF-WEEK = 'TUE' OR                     * P442288 
02494 *               LINK-DAY-OF-WEEK = 'WED'                        * P442288 
02495 *                MOVE 5            TO WS-MIN-BATCH-AGE          * P442288 
02496 *            END-IF                                             * P442288 
02497 *---------------------------------------------------------------* P442288 
02498                                                                   P442288 
02499      MOVE ALL '.'                  TO WS-SCRT.                    P442288 
02500                                                                   P442288 
02501 *---------------------------------------------------------------* P442288 
02502 *   ANY REGISTRATION > WS-EXPIRED-REGIS-YYYY IS CURRENT AND WILL* P442288 
02503 *   HAVE TO BE CANCELLED OR RTS WILL REJECT THE TRANSACTION     * P442288 
02504 *   NOTE:                                                       * P442288 
02505 *   THIS WILL HAVE TO BE EXPANDED TO YYYY-MM FOR STAGGERED REG  * P442288 
02506 *---------------------------------------------------------------* P442288 
02507 *---------------------------------------------------------------* P442288 
02508 *   SET A LEAP YAR FLAG WHICH IS USED IN CALCULATIONS OF THE AGE* P442288 
02509 *   OF A BATCH RECORD BEFORE IT WILL BE PROCESSED / SCRAPPED.   * P442288 
02510 *   NOTE:                                                       * P442288 
02511 *   THIS WAS ORGINALLY DESIGNED, BUT WEDNESDAY PROCESSING WAS   * P442288 
02512 *   DECIDED ON INSTEAD.  CODE LEFT HERE JUST IN CASE IT IS EVER * P442288 
02513 *   NEEDED IN THE FUTURE.                                       * P442288 
02514 *---------------------------------------------------------------* P442288 
02515                                                                   P442288 
02516      MOVE WS-CRNT-DATE-DDD         TO WS-TODAYS-DAY-NO.           P442288 
02517      COMPUTE WS-LEAP-FLAG =                                       P442288 
02518          WS-CRNT-DATE-YYYY - ((WS-CRNT-DATE-YYYY / 4) * 4).       P442288 
02519                                                                   P442288 
02520 *---------------------------------------------------------------* P442288 
02521 *    BUILD RTS TRANSACTION BASE KEY                             * P442288 
02522 *    TIME IS INCREMENTED TO BUILD A UNIQUE TRANSACTION KEY      * P442288 
02523 *---------------------------------------------------------------* P442288 
02524                                                                   P442288 
02525 *---------------------------------------------------------------* P442288 
02526 *-- THIS WAS CHANGED TO SHOW REGIONAL OFFICE AS OFCISSUANCENO --* P442288 
02527 *    MOVE  C-OFC-ISSUANCE-NO       TO WS-OFCISSUANCENO.         * P442288 
02528 *    MOVE  C-OFC-ISSUANCE-CODE     TO WS-OFCISSUANCECD          * P442288 
02529 *  THE ABOVE DEFAULTS WERE DELETED.  THE OFFICE ISSUANCE NUMBER * P442288 
02530 *  IS NOW THE VTR REGIONAL OFFICE THAT PRESIDES OVER THE ACCOUNT. P442288 
02531 *  SEE EXECUTION OF 21138-FIND.. IN PARA 2113-CREATE-RTS-TRANS. * P442288 
02532 *---------------------------------------------------------------* P442288 
02533      MOVE  C-TRANS-WS-ID           TO WS-TRANSWSID.               P442288 
02534                                                                   P442288 
02535 *---------------------------------------------------------------* P442288 
02536 *    BUILD-RTS-TRANSACTION-AMDATE                               * P442288 
02537 *---------------------------------------------------------------* P442288 
02538                                                                   P442288 
02539      COMPUTE WS-DAYS-SINCE-01-01-0000 =                           P442288 
02540          (WS-CRNT-DATE-YYYY * 365) + WS-CRNT-DATE-DDD - 1         P442288 
02541          + ((WS-CRNT-DATE-YYYY - 1) / 4)                          P442288 
02542          - ((WS-CRNT-DATE-YYYY - 1) / 100)                        P442288 
02543          + ((WS-CRNT-DATE-YYYY - 1) / 400).                       P442288 
02544      COMPUTE WS-DAYS-SINCE-01-01-1900 =                           P442288 
02545          WS-DAYS-SINCE-01-01-0000 - WS-DAYS-TO-01-01-1900.        P442288 
02546      MOVE WS-DAYS-SINCE-01-01-1900 TO WS-TRANSAMDATE.             P442288 
02547                                                                      CL*17
02548      IF RTS-SYNC-REG-UPDATE-RUN AND                               P442288 
02549         TODAY-IS-MONDAY                                           P442288 
02550          NEXT SENTENCE                                            P442288 
02551      ELSE                                                            CL*17
02552      IF RTS-NORM-IRP-MON-OVERRIDE AND                                CL*17
02553         TODAY-IS-MONDAY                                              CL*17
02554          NEXT SENTENCE                                               CL*17
02555      ELSE                                                            CL*17
02556      IF TODAY-IS-MONDAY                                           P442288 
02557          IF TESTING-SHOW-DISPLAYS                                 P442288 
02558              DISPLAY ' ****  TODAYS DATE = ' WS-CURRENT-DATE-DT   P442288 
02559                      ' CURRENT AMDATE ' WS-TRANSAMDATE            P442288 
02560              ADD 1                     TO WS-TRANSAMDATE          P442288 
02561              DISPLAY ' **** CURRENT DATE = ' WS-CURRENT-DATE-DT   P442288 
02562                      ' REVISED AMDATE ' WS-TRANSAMDATE            P442288 
02563              DISPLAY ' '                                          P442288 
02564              DISPLAY 'MONDAY FEATURE - AMDATE ADVANCED '          P442288 
02565                      'ONE DAY TO ' WS-TRANSAMDATE                 P442288 
02566              DISPLAY ' '                                          P442288 
02567          ELSE                                                     P442288 
02568              ADD 1                     TO WS-TRANSAMDATE          P442288 
02569              DISPLAY 'MONDAY FEATURE - AMDATE ADVANCED '          P442288 
02570                      'ONE DAY TO ' WS-TRANSAMDATE                    CL**9
02571              MOVE 1                    TO ERROR-PROCESSING-SW        CL**9
02572          END-IF                                                      CL**9
02573          MOVE WS-TRANSAMDATE       TO ERROR-MSG-046-AMDATE           CL**9
02574          MOVE ERROR-MSG-046-TEXT   TO ERR-ERROR-MESSAGE-1            CL**9
02575          MOVE WS-TRANSAMDATE       TO RTS-RPT-AMDATE                 CL**9
02576          MOVE WS-TRANSTIME         TO RTS-RPT-TRANSTIME              CL**9
02577          PERFORM Z-4-CONTROL-EDIT-ERROR                              CL**9
02578          MOVE SPACES               TO ERR-LICN.                      CL**9
02579                                                                   P442288 
02580                                                                   P442288 
02581 *---------------------------------------------------------------* P442288 
02582 *  N  WS-TRANS-KEY = WS-TRANSID (WS-OFCISSUANCENO WS-TRANSWSID  * P442288 
02583 *     WS-TR-DATE-TIME (WS-TRANSAMDATE  WS-TRANSTIME))           * P442288 
02584 *  MOVE C-OFC-ISSUANCE-NO-HQ     TO WS-OFCISSUANCENO            * P442288 
02585 *---------------------------------------------------------------* P442288 
02586                                                                   P442288 
02587                                                                   P442288 
02588      MOVE  C-OFC-ISSUANCE-NO-HQ    TO INITIAL-OFCISSUANCENO.      P442288 
02589      MOVE  C-TRANS-WS-ID           TO INITIAL-TRANSWSID.          P442288 
02590      MOVE  WS-TRANSAMDATE          TO INITIAL-TRANSAMDATE         P442288 
02591                                                                   P442288 
02592      MOVE LOW-VALUES             TO INTERFACE-RESTART-VSAM-REC.   P442288 
02593      MOVE 1                      TO RESTART-REC-NUM.              P442288 
02594                                                                   P442288 
02595      READ INTERFACE-RESTART INTO RESTART-IMAGE                    P442288 
02596          INVALID KEY                                              P442288 
02597             MOVE 'Y'                  TO RESTART-DIAG-READ-ERROR. P442288 
02598                                                                   P442288 
02599      IF RESTART-DIAG-READ-ERROR EQUAL 'Y'                         P442288 
02600         OR RESTART-STATUS NOT = ZEROS                             P442288 
02601          MOVE 'RESTART'          TO ERROR-MSG-033-FILE            P442288 
02602          MOVE 'READ'             TO ERROR-MSG-033-FUNC            P442288 
02603          MOVE RESTART-STATUS     TO ERROR-MSG-033-STATUS          P442288 
02604          MOVE ERROR-MSG-033-TEXT TO ERR-ERROR-MESSAGE-1           P442288 
02605          MOVE 2 TO ERROR-PROCESSING-SW                            P442288 
02606          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
02607          IF TESTING-SHOW-DISPLAYS                                 P442288 
02608              DISPLAY RESTART-STATUS  '=RESTART STATUS AFTER READ' P442288 
02609              DISPLAY RESTART-REC-NUM                              P442288 
02610                 ' NOT LOCATED ON INTERFACE-RESTART FILE'          P442288 
02611                 ' - PROGRAM WILL ATTEMPT INITIAL WRITE'.          P442288 
02612                                                                   P442288 
02613      IF RESTART-TRANSAMDATE   NOT = INITIAL-TRANSAMDATE           P442288 
02614          MOVE INITIAL-IMAGE           TO RESTART-IMAGE            P442288 
02615          MOVE ZERO                    TO RESTART-UNIQUE-SEQ.      P442288 
02616                                                                   P442288 
02617      IF RESTART-UNIQUE-SEQ NUMERIC                                P442288 
02618          ADD 1                        TO RESTART-UNIQUE-SEQ       P442288 
02619      ELSE                                                         P442288 
02620          MOVE 1                       TO RESTART-UNIQUE-SEQ.      P442288 
02621                                                                   P442288 
02622      MOVE RESTART-UNIQUE-SEQ          TO WS-TRANSTIME.            P442288 
02622 D    DISPLAY 'WS-TRANSTIME <' WS-TRANSTIME '>'.                   P442288 
02623                                                                   P442288 
02624                                                                   P442288 
02625      MOVE WS-TRANSAMDATE           TO RTS-RPT-AMDATE              P442288 
02626                                       RTS-UPD-AMDATE              P442288 
02627                                       RTS-MVU-AMDATE              P442288 
02628                                       RTS-MSG-AMDATE.             P442288 
02629      MOVE WS-TRANSTIME             TO RTS-RPT-TRANSTIME           P442288 
02630                                       RTS-UPD-TRANSTIME           P442288 
02631                                       RTS-MVU-TRANSTIME           P442288 
02632                                       RTS-MSG-TRANSTIME.          P442288 
02633      MOVE SPACES                   TO ERR-ERROR-MESSAGE-1.        P442288 
02634      MOVE ZERO                     TO ERROR-PROCESSING-SW.        P442288 
02635      PERFORM Z-4-CONTROL-EDIT-ERROR.                              P442288 
02636      PERFORM Z-5-WRITE-UPDATE-REPORT.                             P442288 
02637      PERFORM Z-V-WRITE-MVUPDT-REPORT.                             P442288 
02638      PERFORM Z-W-WRITE-MSGOUT-REPORT.                             P442288 
02639                                                                   P442288 
02640 *---------------------------------------------------------------* P442288 
02641      IF TESTING-SHOW-DISPLAYS                                     P442288 
02642          DISPLAY 'EXECUTION TRANSAMDATE='                         P442288 
02643              WS-TRANSAMDATE  ' TRANSTIME=' WS-TRANSTIME           P442288 
02644      END-IF.                                                      P442288 
02645 *---------------------------------------------------------------* P442288 
02646      EJECT                                                        P442288 
02647                                                                   P442288 
02648 *---------------------------------------------------------------* P442288 
02649 *    CHECK FOR RTS TRANSACTION STOP - IF ON: QUIT WITH RC=16    * P442288 
02650 *---------------------------------------------------------------* P442288 
02651  11-BRAKE-CHECK.                                                  P442288 
02652                                                                   P442288 
02653      IF TESTING-SHOW-DISPLAYS                                     P442288 
02654          DISPLAY '@ 11-BRAKE-CHECK :'                             P442288 
02655      END-IF.                                                      P442288 
02656                                                                   P442288 
02657      EXEC ADABAS                                                  P442288 
02658          FIND                                                     P442288 
02659          SELECT JOBFUNCAREA,                                      P442288 
02660            FROM  RTS-REFERENCE-POINTS                             P442288 
02661            WHERE JOBFUNCAREA = :'STOPTRANSACTION'                 P442288 
02662            OPTIONS PASSWORD = :WS-SCRT                            P442288 
02663                    PREFIX= RTS-STOPIT-                            P442288 
02664       END-EXEC                                                    P442288 
02665                                                                   P442288 
02666 *---------------------------------------------------------------* P442288 
02667      IF TESTING-SHOW-DISPLAYS                                     P442288 
02668          DISPLAY '@ 11- ADABAS RESPONSE='                         P442288 
02669              RESPONSE-CODE OF RTS-REFERENCE-POINTS                P442288 
02670              ' WITH QUANTITY=' QUANTITY OF RTS-REFERENCE-POINTS   P442288 
02671      END-IF.                                                      P442288 
02672      IF TESTING-SHOW-DISPLAYS                                     P442288 
02673          IF  RESPONSE-CODE OF RTS-REFERENCE-POINTS = ZERO         P442288 
02674              IF QUANTITY OF RTS-REFERENCE-POINTS > ZERO           P442288 
02675                  DISPLAY '@ 11-BRAKE-CHECK- ADABAS FOUND '        P442288 
02676                      RTS-STOPIT-JOBFUNCAREA ' '                   P442288 
02677              ELSE                                                 P442288 
02678                  DISPLAY '@ 11-STOP QUANTITY = ZERO'              P442288 
02679          ELSE                                                     P442288 
02680              DISPLAY '@ 11-ADABAS ERROR '                         P442288 
02681                  RESPONSE-CODE OF RTS-REFERENCE-POINTS            P442288 
02682      END-IF.                                                      P442288 
02683 *---------------------------------------------------------------* P442288 
02684                                                                   P442288 
02685      IF  RESPONSE-CODE OF RTS-REFERENCE-POINTS  = ZERO            P442288 
02686          IF QUANTITY   OF RTS-REFERENCE-POINTS  > 0               P442288 
                                                                                
      D    DISPLAY '!TC005C - IF RESPONSE-CODE OF RTS-REFERENCE-POINTS'         
      D      ' = 0 <' RESPONSE-CODE OF RTS-REFERENCE-POINTS '>'                 
      D      'AND IF QUANTITY OF RTS-REFERENCE-POINTS > 0 <'                    
      D              QUANTITY OF RTS-REFERENCE-POINTS '>'                       
                                                                                
02687              PERFORM 11-MOVE-BRAKE-CHECK THRU                     P442288 
02688                      11-MOVE-BRAKE-CHECK-EXIT                     P442288 
02689              MOVE 16           TO WS-RETURN-CODE                  P442288 
02690              PERFORM Z-Z-TERMINATION                              P442288 
02691          ELSE                                                     P442288 
02692              NEXT SENTENCE                                           CL*14
02693      ELSE                                                         P442288 
02694          MOVE 3                TO ERROR-PROCESSING-SW             P442288 
02695          MOVE 'RTS-REF-PT '    TO ERROR-MSG-014-FILE              P442288 
02696          MOVE RESPONSE-CODE OF RTS-REFERENCE-POINTS               P442288 
02697                                TO ERROR-MSG-014-ERRNUMB           P442288 
02698          MOVE ERROR-MSG-014-TEXT   TO ERR-ERROR-MESSAGE-1         P442288 
02699          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
02700          MOVE 20               TO WS-RETURN-CODE                  P442288 
02701          PERFORM Z-Z-TERMINATION.                                 P442288 
02702                                                                   P442288 
02703      EXEC ADABAS                                                     CL*14
02704          FIND                                                        CL*14
02705          SELECT JOBFUNCAREA,                                         CL*14
02706                 COUNTERNO,                                           CL*14
02707                 REFERENCEDATE,                                       CL*14
02708                 COUNTERNO2,                                          CL*14
02709                 REFERENCEDATE2,                                      CL*14
02710            FROM  RTS-REFERENCE-POINTS     RTS-REFERENCE-POINT2       CL*14
02711            WHERE JOBFUNCAREA = :'TRANSVERSIONCD'                     CL*14
02712            OPTIONS PASSWORD = :WS-SCRT                               CL*14
02713                    PREFIX= RTS-TRANSV-                               CL*14
02714       END-EXEC                                                       CL*14
02715                                                                      CL*14
02716 *---------------------------------------------------------------*    CL*14
02717      IF TESTING-SHOW-DISPLAYS                                        CL*14
02718          DISPLAY '@ 11-TVCD ADABAS RESPONSE='                        CL*14
02719              RESPONSE-CODE OF RTS-REFERENCE-POINT2                   CL*14
02720              ' WITH QUANTITY=' QUANTITY OF RTS-REFERENCE-POINT2      CL*14
02721      END-IF.                                                         CL*14
02722      IF TESTING-SHOW-DISPLAYS                                        CL*14
02723          IF  RESPONSE-CODE OF RTS-REFERENCE-POINT2 = ZERO            CL*14
02724              IF QUANTITY OF RTS-REFERENCE-POINT2 > ZERO              CL*14
02725                  DISPLAY '@ 11-TRANSVERSIONCD- ADABAS FOUND '        CL*14
02726                  DISPLAY '  '  RTS-TRANSV-JOBFUNCAREA                CL*14
02727                  DISPLAY '  '  RTS-TRANSV-COUNTERNO                  CL*14
02728                  DISPLAY '  '  RTS-TRANSV-REFERENCEDATE              CL*14
02729                  DISPLAY '  '  RTS-TRANSV-COUNTERNO2                 CL*14
02730                  DISPLAY '  '  RTS-TRANSV-REFERENCEDATE2             CL*14
02731              ELSE                                                    CL*14
02732                  DISPLAY '@ 11-TRANSVERSIONCD = ZERO'                CL*14
02733          ELSE                                                        CL*14
02734              DISPLAY '@ 11-ADABAS ERROR '                            CL*14
02735                  RESPONSE-CODE OF RTS-REFERENCE-POINT2               CL*14
02736      END-IF.                                                         CL*14
02737 *---------------------------------------------------------------*    CL*14
02738                                                                      CL*14
02739      IF  RESPONSE-CODE OF RTS-REFERENCE-POINT2  = ZERO               CL*14
02740          IF QUANTITY   OF RTS-REFERENCE-POINT2  > 0                  CL*14
                                                                                
      D     DISPLAY '!TC005D - IF RESPONSE-CODE OF RTS-REFERENCE-POINT2'        
      D      ' = 0 <' RESPONSE-CODE OF RTS-REFERENCE-POINT2 '>'                 
      D      'AND IF QUANTITY OF RTS-REFERENCE-POINT2 > 0 <'                    
      D              QUANTITY OF RTS-REFERENCE-POINT2 '>'                       
                                                                                
02741              MOVE RTS-TRANSV-COUNTERNO    TO   C-TRANS-VERSIONCD.    CL*14
02742                                                                      CL*14
02743      IF TESTING-SHOW-DISPLAYS                                     P442288 
02744          DISPLAY '@ 11-BRAKE-CHECK EXIT'                          P442288 
02745      END-IF.                                                      P442288 
02746                                                                   P442288 
02747  11-BRAKE-CHECK-EXIT.                                             P442288 
02748      EXIT.                                                        P442288 
02749                                                                   P442288 
02750  11-MOVE-BRAKE-CHECK.                                             P442288 
02751                                                                   P442288 
02752      MOVE 3                        TO ERROR-PROCESSING-SW.        P442288 
02753      MOVE 'RTS-REF-PT '            TO ERROR-MSG-014-FILE.         P442288 
02754      MOVE RTS-STOPIT-JOBFUNCAREA   TO ERROR-MSG-092-FUNC.         P442288 
02755      MOVE ERROR-MSG-092-TEXT       TO ERR-ERROR-MESSAGE-1.        P442288 
02756      PERFORM Z-4-CONTROL-EDIT-ERROR.                              P442288 
02757      MOVE 16                       TO WS-RETURN-CODE.             P442288 
02758      PERFORM Z-Z-TERMINATION.                                     P442288 
02759                                                                   P442288 
02760  11-MOVE-BRAKE-CHECK-EXIT.                                        P442288 
02761      EXIT.                                                        P442288 
02762                                                                   P442288 
02763                                                                   P442288 
02764  2-MAIN-LOOP.                                                     P442288 
02765                                                                   P442288 
                                                                                
      D    DISPLAY '!TC008A - 2-MAIN-LOOP'                                      
      D    DISPLAY 'BATCH-BILLING-RECAP-DEP <'                                  
      D             BATCH-BILLING-RECAP-DEP '>'                                 
                                                                                
02766      IF  BATCH-BILLING-RECAP-DEP NOT = 'R'                        P442288 
02767          IF TESTING-SHOW-DISPLAYS                                 P442288 
02768            DISPLAY 'BATCH ' BATCH-KEY ' NOT "R" -- WAS REJECTED ' P442288 
02769          END-IF                                                   P442288 
                                                                                
      D    DISPLAY '!TC008B - 2-MAIN-LOOP'                                      
                                                                                
02770          PERFORM Z-1-READ-IRP-BATCH-FILE                          P442288 
02771          GO TO 2-MAIN-LOOP-EXIT.                                  P442288 
02772                                                                   P442288 
02773 *---------------------------------------------------------------* P442288 
02774 *    HOLD THE BATCHES FOR X NUMBER OF DAYS TO ALLOW FOR         * P442288 
02775 *    CHANGES THAT ARE OFTEN MADE.                               * P442288 
02776 *---------------------------------------------------------------* P442288 
02777                                                                   P442288 
02778      MOVE BATCH-ADDED-DATE         TO WS-BATCH-ADDED-DATE         P442288 
02779      MOVE WS-BATCH-ADDED-MONTH     TO WS-BATCHES-MONTH-NO.        P442288 
02780      MOVE WS-BATCH-ADDED-DAY       TO WS-WORKING-DAY-NO.          P442288 
02781      MOVE WS-DAYS-TO-MONTH (WS-BATCHES-MONTH-NO) TO               P442288 
02782                                       WS-BATCHES-DAY-NO.          P442288 
02783      IF WS-LEAP-YEAR                                              P442288 
02784          IF (WS-BATCHES-MONTH-NO > 2)       OR                    P442288 
02785             (WS-BATCHES-MONTH-NO = 2 AND                          P442288 
02786              WS-BATCH-ADDED-DAY > 28)                             P442288 
02787              ADD 1                 TO WS-BATCHES-DAY-NO.          P442288 
02788      ADD WS-WORKING-DAY-NO         TO WS-BATCHES-DAY-NO.          P442288 
02789                                                                   P442288 
02790      IF WS-CRNT-DATE-YYYY NOT = WS-BATCH-ADDED-YYYY               P442288 
02791          COMPUTE WS-BATCHES-AGE =                                 P442288 
02792              (365 * (WS-CRNT-DATE-YYYY - WS-BATCH-ADDED-YYYY))    P442288 
02793              - WS-BATCHES-DAY-NO                                  P442288 
02794              + WS-TODAYS-DAY-NO                                   P442288 
02795          IF WS-BATCHES-MONTH-NO > 2 AND                           P442288 
02796             WS-LAST-YEAR-LEAP                                     P442288 
02797             ADD 1                  TO WS-BATCHES-AGE              P442288 
02798          END-IF                                                   P442288 
02799          IF TESTING-SHOW-DISPLAYS                                 P442288 
02800            MOVE WS-BATCHES-AGE     TO WS-BATCHES-AGE-D            P442288 
02801            DISPLAY 'DIFFERENT YEAR '                              P442288 
02802            WS-CRNT-DATE-YYYY ' NOT = ' WS-BATCH-ADDED-YYYY        P442288 
02803            ': BATCH AGE = ' WS-BATCHES-AGE-D                      P442288 
02804          END-IF                                                   P442288 
02805      ELSE                                                         P442288 
02806          COMPUTE WS-BATCHES-AGE =                                 P442288 
02807              WS-TODAYS-DAY-NO - WS-BATCHES-DAY-NO.                P442288 
02808                                                                   P442288 
02809      IF TESTING-SHOW-DISPLAYS                                     P442288 
02810          MOVE WS-BATCHES-AGE       TO WS-BATCHES-AGE-D            P442288 
02811          DISPLAY 'DAYS PRIOR TO MONTH ' WS-BATCHES-MONTH-NO       P442288 
02812                ' = ' WS-DAYS-TO-MONTH (WS-BATCHES-MONTH-NO)       P442288 
02813          DISPLAY 'TODAYS DAY # ' WS-TODAYS-DAY-NO                 P442288 
02814                   ' BATCHS DAY # ' WS-BATCHES-DAY-NO              P442288 
02815                   ' YY ' WS-BATCH-ADDED-YYYY                      P442288 
02816                   ' MM ' WS-BATCH-ADDED-MONTH                     P442288 
02817                   ' DD ' WS-BATCH-ADDED-DAY                       P442288 
02818                   ': BATCH AGE = ' WS-BATCHES-AGE-D               P442288 
02819          IF WS-LEAP-YEAR                                          P442288 
02820              DISPLAY 'THIS IS LEAP YEAR'                          P442288 
02821          ELSE                                                     P442288 
02822          IF WS-LAST-YEAR-LEAP                                     P442288 
02823              DISPLAY 'LAST YEAR WAS LEAP YEAR'                    P442288 
02824      END-IF.                                                      P442288 
02825                                                                   P442288 
02826      IF WS-BATCHES-AGE < ZERO                                     P442288 
02827          MOVE ZERO                 TO WS-BATCHES-AGE.             P442288 
02828                                                                   P442288 
02829      IF TESTING-SHOW-DISPLAYS                                     P442288 
02830        MOVE WS-BATCHES-AGE         TO WS-BATCHES-AGE-D            P442288 
02831        DISPLAY 'MIN AGE ' WS-MIN-BATCH-AGE                        P442288 
02832               ' BATCHES AGE ' WS-BATCHES-AGE-D                    P442288 
02833      END-IF.                                                      P442288 
02834                                                                   P442288 
02835      IF WS-MIN-BATCH-AGE > WS-BATCHES-AGE                         P442288 
02836         IF TESTING-SHOW-DISPLAYS                                  P442288 
02837             MOVE WS-BATCHES-AGE   TO WS-BATCHES-AGE-D             P442288 
02838             DISPLAY 'BATCH TOO YOUNG '                            P442288 
02839               BATCH-KEY ' ADDED ' BATCH-ADDED-DATE                P442288 
02840               ' MIN AGE ' WS-MIN-BATCH-AGE ' > ' WS-BATCHES-AGE-D P442288 
02841         END-IF                                                    P442288 
02842         PERFORM Z-1-READ-IRP-BATCH-FILE                           P442288 
02843         GO TO 2-MAIN-LOOP-EXIT.                                   P442288 
02844                                                                   P442288 
02845      IF  BATCH-BILLING-RECAP-DEP = 'R'                            P442288 
                                                                                
      D        DISPLAY '!TC009 - IF BATCH-BILLING-RECAP-DEP = R <'              
      D                             BATCH-BILLING-RECAP-DEP '>'                 
                                                                                
02846          PERFORM   21-PROCESS-BATCH-RECORD THRU                   P442288 
02847                    21-PROCESS-BATCH-RECORD-EXIT                   P442288 
02848          IF TESTING-SHOW-DISPLAYS                                 P442288 
02849              DISPLAY '@ AFTER 21-PROCESSING-BATCH COMPLETE: '     P442288 
02850                      'IRP ACCOUNT: '  BATCH-ACCOUNT-NUMBER        P442288 
02851                       ' SUPP: '  BATCH-SUPPLEMENT-NUMBER          P442288 
02852              DISPLAY ' IRP YEAR: '   BATCH-REGIS-DT               P442288 
02853                  ' ACCOUNT: '  BATCH-ACCOUNT-NUMBER               P442288 
02854                  ' FLEET: '  BATCH-FLEET-NBR                      P442288 
02855                  ' SUPPL: '  BATCH-SUPPLEMENT-NUMBER              P442288 
02856                  '   RTS TRANSACTIONS COUNT= '                    P442288 
02857                   WS-RTS-NO-TRANS-BATCH                           P442288 
02858          END-IF                                                   P442288 
02859 *---------------------------------------------------------------* P442288 
02860 *     THE BATCH RECORD WILL REMAIN ON THE FILE IF INTERFACE     * P442288 
02861 *     RECORDS ARE LEFT TO BE PROCESSED, DUE TO LACT OF A        * P442288 
02862 *     A TITLE RECORD ON THE RTS_TITLE FILE.  AFTER THESE HAVE   * P442288 
02863 *     REMAINED ON THE FILE FOR A MAXIMUM NUMBER OF DAYS, THEN   * P442288 
02864 *     THE TRANSACTIONS AND BATCH RECORDS ARE DELETED.           * P442288 
02865 *---------------------------------------------------------------* P442288 
02866          ADD WS-RTS-NO-TRANS-BATCH TO WS-RTS-NO-PR-TRANS          P442288 
02867          MOVE ZERO                 TO WS-RTS-NO-TRANS-BATCH       P442288 
02868 *---------------------------------------------------------------* P442288 
02869 *     AN INCREMENTED BATCH NUMBER RECORD IS WRITTEN IF THERE    * P442288 
02870 *     ARE ERRORS IN THE BATCH.  IT WILL BE INCREMENTED FROM     * P442288 
02871 *     0 - 6 WHICH WILL CAUSE IT TO BE PROCESSED 7 TIMES, AFTER  * P442288 
02872 *     WHICH IT WILL BE DELETED.  RECORDS SHOULD HAVE BEEN IN    * P442288 
02873 *     THE TITLE FILE BY THAT TIME.  THE IRP-RTS TRANSACTIONS    * P442288 
02874 *     ARE DELETED AFTER THEY ARE PROCESSED CORRECTLY.           * P442288 
02875 *     ONLY THE INCOMPLETED ONES WILL BE RE-PROCESSED            * P442288 
02876 *     WITHIN THIS 7 DAY LIMITED ATTEMPT TO UPDATE THE FILES     * P442288 
02877 *---------------------------------------------------------------* P442288 
02878 *********************  PROGRAM LOGIC CHANGE ********************* P442288 
02879 *---------------------------------------------------------------* P442288 
02880 *    THE LOGIC DEALING WITH RECORDS THAT COULD NOT BE LOCATED ON* P442288 
02881 *    THE RTS DATABASE FILES HAS BEEN CHANGED.  ERROR MESSAGES   * P442288 
02882 *    WILL BE SEND, BUT THE TRANSACTIONS ARE TRANSMITTED TO RTS  * P442288 
02883 *    ANYWAY.  THERE IS NO LONGER ANY ATTEMPT TO HOLD THE UPDATES* P442288 
02884 *    FOR SUBSEQUENT PROCESSING.  THE DOCNOS SHOULD BE LOCATABLE * P442288 
02885 *    ON THE TITLE DATABASE BY THE TIME THIS TRANSACTION PROCESS * P442288 
02886 *    IS RUN.  BY SENDING THE ERRONEOUS TRANSACTIONS THROUGH THEN* P442288 
02887 *    THE FUNDS ARE ACCURATELY REPORTED AND SHOULD MATCH BATCH   * P442288 
02888 *    BILLING AMOUNTS.    THE APPLICATION LOGIC FOR REPROCESSING * P442288 
02889 *    PREVIOUSLY UNATTAINABLE DOCUMENTS (TITLES) IS LEFT HERE,   * P442288 
02890 *    SINCE IT WAS THOROUGHLY TESTED,  JUST IN CASE THE DECISION * P442288 
02891 *    IS LATER CHANGED.                                          * P442288 
02892 *---------------------------------------------------------------* P442288 
02893          PERFORM   22-DELETE-BATCH-RECORD                         P442288 
02894      END-IF.                                                      P442288 
02895                                                                   P442288 
02896      PERFORM Z-1-READ-IRP-BATCH-FILE.                             P442288 
02897      MOVE ZERO                     TO WS-BATCH-NOTITLE-FLAG.      P442288 
02898      MOVE ZERO                     TO WS-BATCH-NOTITLE-ERRS.      P442288 
02899 *                                                                 P442288 
02900  2-MAIN-LOOP-EXIT.                                                P442288 
02901      EXIT.                                                        P442288 
02902      EJECT                                                        P442288 
02903                                                                   P442288 
02904  21-PROCESS-BATCH-RECORD.                                         P442288 
02905 ***************************************************************** P442288 
02906 *   READ RTS INTERFACE RECORDS AND GENERATE RTS TRANSACTIONS    * P442288 
02907 ***************************************************************** P442288 
02908                                                                   P442288 
02909      IF TESTING-SHOW-DISPLAYS                                     P442288 
02910          DISPLAY '@ 21- BATCH RECORD ===>' IRP-BATCH-REC          P442288 
02911          DISPLAY ' BTCH-KEY==>' BTCH-KEY                          P442288 
02912            ' BATCH-KEY==>' BATCH-KEY                              P442288 
02913          DISPLAY 'SUP=' BATCH-SUPPLEMENT-NUMBER                   P442288 
02914            ' YR =' BATCH-REGIS-DT                                 P442288 
02915            ' ACT=' BATCH-ACCOUNT-NUMBER                           P442288 
02916            ' FLT=' BATCH-FLEET-NBR                                P442288 
02917            ' SFX=' BATCH-SUFFIX-NUMBER                            P442288 
02918      END-IF.                                                      P442288 
02919                                                                   P442288 
02920                                                                   P442288 
                                                                                
      D        DISPLAY '!TC009 - 21-PROCESS-BATCH-RECORD'                       
                                                                                
02921      MOVE ZERO                     TO  WRKRTS-FILE-STATUS.        P442288 
02922      MOVE BATCH-SUPPLEMENT-NUMBER  TO  WORK-FD-SUPP-NBR.          P442288 
02923      MOVE BATCH-REGIS-DT           TO  WORK-FD-REGIS-DT.          P442288 
02924      MOVE BATCH-ACCOUNT-NUMBER     TO  WORK-FD-ACCOUNT-NBR.       P442288 
02925      MOVE BATCH-FLEET-NBR          TO  WORK-FD-FLEET-NBR.         P442288 
02926      MOVE BATCH-SUFFIX-NUMBER      TO  WORK-FD-SUFFIX-NBR.        P442288 
02927      MOVE SPACES                   TO  WORK-FD-OWNER-EQUIP-NBR    P442288 
02928      MOVE 9                        TO  WS-FILETIERCD.             P442288 
02929      MOVE ZERO                     TO WORK-FD-OWNER-EQUIP-SEQ-NBR.P442288 
02930      MOVE ZEROS                    TO WS-NUM-SKIP-AUDIT-MILEAGE   P442288 
02931                                       WS-NUM-SKIP-WEIGHT-CHANGE   P442288 
02932                                       WS-NUM-SKIP-MILE-CHANGE     P442288 
02933                                       WS-NUM-SKIP-REFUND          P442288 
02934                                       WS-NUM-SKIP-DELETED         P442288 
02935                                       WS-NUM-SKIP-XPLATE             CL**9
02936                                       WS-NUM-SKIP-CONV-GEAR.      P442288 
02937                                                                   P442288 
02938      START  WORK-FD-INTERFACE-FILE KEY GREATER THAN WORK-FD-KEY.  P442288 
02939      IF TESTING-SHOW-DISPLAYS                                     P442288 
02940          DISPLAY '@21-PROCESS-BATCH...START RTS INTERFACE STATUS='P442288 
02941              WRKRTS-FILE-STATUS ' FOR KEY=' WORK-FD-KEY           P442288 
02942      END-IF.                                                      P442288 
02943                                                                   P442288 
02944      IF TESTING-SHOW-DISPLAYS                                     P442288 
02945          IF RTS-SYNC-REG-UPDATE-RUN                               P442288 
02946              DISPLAY ' SPECIAL RTS-SYNC-REG-UPDATE-RUN PROCESS *' P442288 
02947          ELSE                                                     P442288 
02948              DISPLAY ' NORMAL WEDNESDAY BATCH RUN PROCESS *'      P442288 
02949          END-IF.                                                  P442288 
02950                                                                   P442288 
02951      IF WRKRTS-FILE-STATUS = ZERO                                 P442288 
                                                                                
      D        DISPLAY '!TC268 - IF WRKRTS-FILE-STATUS > ZERO <'                
      D                             WRKRTS-FILE-STATUS '>'                      
                                                                                
02952         PERFORM   Z-2-READ-IRP-RTS-FILE THRU                      P442288 
02953                   Z-2-READ-IRP-RTS-FILE-EXIT                      P442288 
02954         PERFORM   211-PROCESS-RTS-INTERFACE THRU                  P442288 
02955                   211-PROCESS-RTS-INTERFACE-EXIT                  P442288 
02956         UNTIL BATCH-SUPPLEMENT-NUMBER NOT = WRKRTS-SUPP-NBR       P442288 
02957            OR BATCH-ACCOUNT-NUMBER NOT = WRKRTS-ACCOUNT-NBR       P442288 
02958            OR BATCH-FLEET-NBR      NOT = WRKRTS-FLEET-NBR         P442288 
02959            OR BATCH-REGIS-DT          NOT = WRKRTS-REGIS-DT       P442288 
02960            OR WRKRTS-END-OF-FILE                                  P442288 
02961      ELSE                                                         P442288 
02962         IF NO-WRKRTS-REC    AND                                   P442288 
02963            RTS-BYPASS-FLAG                                        P442288 
                                                                                
      D        DISPLAY '!TCxxc - IF WRKRTS-FILE-STATUS > ZERO <'                
      D                             WRKRTS-FILE-STATUS '>'                      
      D        DISPLAY ' ELSE IF NO-WRKRTS-REC <'                               
      D                          WRKRTS-FILE-STATUS '>'                         
      D        DISPLAY '     AND RTS-BYPASS-FLAG(9) <'                          
      D                             RTS-TRANS-ERROR-FLAG '>'                    
                                                                                
                  DISPLAY 'NEXT SENTENCE'                                       
02964 *           NEXT SENTENCE                                         P442288 
02965         ELSE                                                      P442288 
02966         IF NO-WRKRTS-REC                                          P442288 
                                                                                
      D        DISPLAY '!TCxxd IF NO-WRKRTS-REC <'                              
      D                          WRKRTS-FILE-STATUS '>'                         
                                                                                
02967 *            NO RECORDS ON IRPRTS FOR THIS ACCOUNT/YEAR/SUPPL     P442288 
02968              MOVE 3                TO ERROR-PROCESSING-SW         P442288 
02969              MOVE ERROR-MSG-008-TEXT TO ERR-ERROR-MESSAGE-1       P442288 
02970              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
02971         ELSE                                                      P442288 
                                                                                
      D        DISPLAY '!TCxxe ELSE IF NO-WRKRTS-REC <'                         
      D                          WRKRTS-FILE-STATUS '>'                         
                                                                                
02972              MOVE WRKRTS-FILE-STATUS TO EMT-ERR-IRPRTS            P442288 
02973              MOVE 3                  TO ERROR-PROCESSING-SW       P442288 
02974              MOVE ERROR-MSG-005-TEXT TO ERR-ERROR-MESSAGE-1       P442288 
02975              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
02976              PERFORM Z-Z-TERMINATION.                             P442288 
02977                                                                   P442288 
02978 *---------------------------------------------------------------* P442288 
02979 *    THE COUNTER  "WS-RTS-NO-TRANS-BATCH"  IS INCREMENTED IN    * P442288 
02980 *    2113-CREATE-RTS-TRANSACTIONS AT (4.0) ONLY AFTER THE RTS   * P442288 
02981 *    ADABAS TRANSACTIONS ARE WRITTEN AND THE INTERFACE RECORD   * P442288 
02982 *    IS ARCHIVED.                                               * P442288 
02983 *---------------------------------------------------------------* P442288 
02984      IF RTS-BYPASS-FLAG                                           P442288 
02985         OR NO-WRKRTS-REC                                          P442288 
02986         OR WS-NUM-SKIP-AUDIT-MILEAGE  > ZERO                      P442288 
02987         OR WS-NUM-SKIP-WEIGHT-CHANGE  > ZERO                      P442288 
02988         OR WS-NUM-SKIP-MILE-CHANGE    > ZERO                      P442288 
02989         OR WS-NUM-SKIP-REFUND         > ZERO                      P442288 
02990         OR WS-NUM-SKIP-DELETED        > ZERO                      P442288 
02991         OR WS-NUM-SKIP-XPLATE         > ZERO                         CL**9
02992         OR WS-NUM-SKIP-CONV-GEAR      > ZERO                      P442288 
                                                                                
      D        DISPLAY '!TCxxf IF RTS-BYPASS-FLAG '                             
      D                                                                         
                                                                                
02993 *        NEXT SENTENCE                                            P442288 
02994      ELSE                                                         P442288 
02995      IF WS-RTS-NO-TRANS-BATCH = ZERO                              P442288 
02996 *    NO RECORDS ON IRPRTS FOR THIS ACCOUNT/YEAR/SUPPL             P442288 
                                                                                
      D        DISPLAY '!TCxxg IF WS-RTS-NO-TRANS-BATCH = ZERO <'               
      D                           WS-RTS-NO-TRANS-BATCH '>'                     
                                                                                
02997          MOVE 3                TO ERROR-PROCESSING-SW             P442288 
02998          MOVE ERROR-MSG-008-TEXT TO ERR-ERROR-MESSAGE-1           P442288 
02999          PERFORM Z-4-CONTROL-EDIT-ERROR.                          P442288 
03000  21-PROCESS-BATCH-RECORD-EXIT.                                    P442288 
03001      EXIT.                                                        P442288 
03002                                                                   P442288 
03003      EJECT                                                        P442288 
03004                                                                   P442288 
03005  211-PROCESS-RTS-INTERFACE.                                       P442288 
03006                                                                   P442288 
03007      IF TESTING-SHOW-DISPLAYS                                     P442288 
03008          DISPLAY '@ 211-PROCESS-RTS-INTERFACE RECORD '            P442288 
03009                  ' AMDATE='    WS-TRANSAMDATE                     P442288 
03010                  ' TIME='      WS-TRANSTIME                       P442288 
03011                  '-- EXP-ACT-TYPE :' WRKRTS-ACT-TYPE                 CL**2
03012                  ' CHANGE-TYPE:' WRKRTS-TYPE-OF-CHANGE-FLAG       P442288 
03013          DISPLAY ' IF RGN: ' WRKRTS-REGION                        P442288 
03014                  ' #: ' WRKRTS-REGION-NUMBER                      P442288 
03015                  ' RGN-OFFICE-FLAG: ' WS-RGN-OFFICE-FLAG          P442288 
03016                  ' PREV RGN: ' WS-RGN-OFFICE                      P442288 
03017                  ' OFC ISS NO: ' WS-RGN-OFC-ISS-NO                P442288 
03018      END-IF.                                                      P442288 
03019      INITIALIZE                         WS-TITLE-DATA.            P442288 
03020      INITIALIZE                         WS-REGIS-DATA.            P442288 
03021 *---------------------------------------------------------------*    CL**9
03022 *    MOVE  ALL '-'                   TO WS-REGIS-RTS-REGPLTNO.  *    CL**9
03023 *    MOVE  ZEROS                     TO WS-REGIS-RTS-REG-EXP-DT.*    CL**9
03024 *---------------------------------------------------------------*    CL**9
03025      MOVE  ALL '-'                   TO WS-REGIS-RTS-REGPLTNO.       CL**9
03026      MOVE  ALL '-'                   TO WS-REGIS-RTS-REGPLATECD      CL**9
03027      MOVE  ZEROS                     TO WS-REGIS-PLTBIRTHDATE.    P442288 
03028      MOVE  ZEROS                     TO WS-REGIS-RTS-REG-EXP-DT.     CL**9
03029      MOVE  ZERO                      TO RTS-UPDT-ERROR-FLAG.      P442288 
03030      MOVE SPACES                     TO RTS-TTL-SKIPPED-FLAG.     P442288 
03031      MOVE SPACES                     TO RTS-IRPINV-FLAG.          P442288 
03032      MOVE  ZEROS                     TO ERROR-PROCESSING-SW.         CL**7
03033      MOVE  1                         TO RTS-TRANS-ERROR-FLAG.     P442288 
03034      MOVE 'P'                        TO WS-TIRETYPECD.            P442288 
03035                                                                   P442288 
03036      MOVE 'X'                        TO WS-RGN-OFFICE-FLAG.       P442288 
03037      MOVE WRKRTS-REGION              TO WS-RGN-OFFICE.            P442288 
03038      MOVE WRKRTS-REGION-NUMBER       TO WS-RGN-OFC-ISS-NO.        P442288 
03039                                                                   P442288 
03040      INSPECT WS-RTS-DOCNO REPLACING ALL SPACES BY ZEROS.          P442288 
03041                                                                   P442288 
03042 *---------------------------------------------------------------* P442288 
03043 *       CONVERT LICENCE NUMBER TO RTS  FORMAT                     P442288 
03044 *--- THIS SHOULD HAVE CHANGED WITH REGISTRATION YEAR 1993 ------* P442288 
03045 *--- THE PLATES WERE TO BE MANUFACTURED WITH THE LEADING ZERO --* P442288 
03046 *--- THE PERFORM WOULD NO LONGER BE NEEDED........BUT ----------* P442288 
03047 *--- THE 94 PLATES WERE ORDERED WITH NO LEADING ZEROS........---* P442288 
03048 *--- THE 95 PLATES SHOULD BE ORDERED CORRECTLY.      ........---* P442288 
03049 *---------------------------------------------------------------* P442288 
03050                                                                   P442288 
03051      MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR     TO WS-PLATE-NO-IRP.    P442288 
03052      MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR     TO WS-PLATE-NO-RTS.    P442288 
03053                                                                   P442288 
03054      IF WRKRTS-REGIS-DT < 199501                                  P442288 
                                                                                
      D        DISPLAY '!TC012 - IF WRKRTS-REGIS-DT < 199501 <'                 
      D                             WRKRTS-REGIS-DT '>'                         
                                                                                
03055          PERFORM 2114-IRP-TO-RTS-LICE.                            P442288 
03056                                                                   P442288 
03057                                                                   P442288 
03058 *---------------------------------------------------------------* P442288 
03059 *   THE FOLLOWING RTS VSAM TRANSACTION RECORDS ARE SKIPPED AND  * P442288 
03060 *   NOT REPORTED TO RTS.  THEY ARE NOT PERTINENT TO RTS FOR     * P442288 
03061 *   VARIOUS REASONS.                                            * P442288 
03062 *   1.)  CONVERTER GEAR UNITS ARE NOT REAL PLATES AND ARE ONLY  * P442288 
03063 *        IN IRP BECAUSE CALIFORNIA CHARGES FOR THEM.  THEY TIE  * P442288 
03064 *        MULTIPLE TRAILERS TOGETHER.                            * P442288 
03065 *   2.)  WEIGHT INCREASES WITH NO IMPACT ON TEXAS WEIGHT OR FEE,* P442288 
03066 *          SUCH AS INCREASED WEIGHTS IN OTHER STATES            * P442288 
03067 *   3.)  LOGICALLY DELETED SUPPLEMENTS                          * P442288 
03068 *   4.)  MILEAGE-CHANGE SUPPLEMENTS -                           * P442288 
03069 *            WITH NO IMPACT ON TEXAS WEIGHT OR FEE              * P442288 
03070 *   5.)  AUDIT-MILEAGE CHANGE SUPPLEMENTS -                     * P442288 
03071 *            WITH NO IMPACT ON TEXAS WEIGHT OR FEE              * P442288 
03072 *---------------------------------------------------------------* P442288 
03073       IF TESTING-SHOW-DISPLAYS                                       CL**7
03074           DISPLAY '@ 211-PROCESSS AFTER INITIALIZATIONS: '           CL**7
03075               ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG                CL**7
03076               ' UPDT ERR FL  = ' RTS-UPDT-ERROR-FLAG                 CL**7
03077               ' ERROR SW     = ' ERROR-PROCESSING-SW.                CL**7
03078                                                                      CL**7
03079 *---------------------------------------------------------------* P442288 
03080 *  REFUND SUPPLEMENT INTERFACE RECORDS CREATED BY AUDIT BILLING * P442288 
03081 *  PROGRAM (P442259) DOES NOT ADJUST COLLECTIONS FOR REFUNDED   * P442288 
03082 *  UNIT(S).  IT IS CURRENTLY CREATING A COMPLETELY SEPARATE RTS-* P442288 
03083 *  INTERFACE RECORD FOR THE REFUND SUPPLEMENT.  THIS CREATES A  * P442288 
03084 *  RECORD WITH NEGATIVE (-) VALUES, WHICH WILL SEND NEGATIVE    * P442288 
03085 *  FUNDS DATA TO COUNTIES AS WELL AS BAD TRANSMITTAL REPORTS.   * P442288 
03086 *                                                               * P442288 
03087 *    REMOVE THIS BYPASS WHEN THE INTERFACE RECORD PROBLEM HAS   * P442288 
03088 *    BEEN RESOLVED.                                             * P442288 
03089 *                                                               * P442288 
03090 *---------------------------------------------------------------* P442288 
03091      EJECT                                                           CL**9
03092                                                                   P442288 
03093 *---------------------------------------------------------------* P442288 
03094 *  SET PROCESSING FLAGS TO HANDLE SPECIAL CASES,                * P442288 
03095 *  MOSTLY SETUP TO SKIP THESE INTERFACE RECORDS.                * P442288 
03096 *                                                               * P442288 
03097 *    REMOVE THIS BYPASS WHEN THE INTERFACE RECORDS CAN BE       * P442288 
03098 *    PROPERLY PROCESSED.                                        * P442288 
03099 *                                                               * P442288 
03100 *---------------------------------------------------------------* P442288 
03101                                                                      CL**9
03102      IF WS-PLATE-NO-RTS = SPACES                                     CL**9
03103 *---------------------------------------------------------------*    CL**9
03104 *       MOVE 9                         TO RTS-TRANS-ERROR-FLAG  *    CL**9
03105 *---------------------------------------------------------------*    CL**9
                                                                                
      D       DISPLAY '!TC013: WS-PLATE-NO-RTS = SPACES <'                      
      D             WS-PLATE-NO-RTS '>'                                         
                                                                                
03106         MOVE 1                         TO ERROR-PROCESSING-SW        CL**9
03107         MOVE 5                         TO RTS-UPDT-ERROR-FLAG        CL**9
03108         ADD 1                          TO WS-NUM-SKIP-XPLATE         CL**9
03109         MOVE WRKRTS-IDENTIFICATION-NBR TO ERROR-MSG-051-VIN          CL**9
03110         MOVE WS-RTS-DOCNO              TO ERROR-MSG-051-TITLE        CL**9
03111         MOVE WS-PLATE-NO-RTS           TO ERR-LICN                   CL**9
03112         MOVE ERROR-MSG-051-TEXT        TO ERR-ERROR-MESSAGE-1        CL**9
03113         PERFORM Z-4-CONTROL-EDIT-ERROR                               CL**9
03114         MOVE SPACES                   TO ERR-LICN.                   CL**9
03115                                                                      CL**9
03116      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'D' OR 'O'                   P442288 
      D        DISPLAY '!TC014 - WRKRTS-TYPE-OF-CHANGE-FLAG = D OR O'           
      D                ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                      
03117          MOVE WRKRTS-TYPE-OF-CHANGE-FLAG TO WRKRTS-PROCESSING-FLAGP442288 
03118          MOVE 9                    TO RTS-TRANS-ERROR-FLAG        P442288 
03119          ADD 1                     TO WS-NUM-SKIP-DELETED         P442288 
03120      ELSE                                                         P442288 
03121      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'R'                          P442288 
      D        DISPLAY '!TC015 - WRKRTS-TYPE-OF-CHANGE-FLAG = R'                
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
03122          MOVE 'R'                  TO WRKRTS-PROCESSING-FLAG      P442288 
03123          MOVE 9                    TO RTS-TRANS-ERROR-FLAG        P442288 
03124          ADD 1                     TO WS-NUM-SKIP-REFUND          P442288 
03125      ELSE                                                         P442288 
03126      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'A' OR                       P442288 
03127         WRKRTS-AUDIT-SUPPLEMENT-VEHL                              P442288 
                                                                                
      D        DISPLAY '!TC016 - WRKRTS-TYPE-OF-CHANGE-FLAG = A'                
      D                ' OR WRKRTS-AUDIT-SUPPLEMENT-VEHL'                       
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
                                                                                
03128          MOVE 'A'                  TO WRKRTS-PROCESSING-FLAG      P442288 
03129          MOVE 9                    TO RTS-TRANS-ERROR-FLAG        P442288 
03130          ADD 1                     TO WS-NUM-SKIP-AUDIT-MILEAGE   P442288 
03131      ELSE                                                         P442288 
03132      IF WRKRTS-WEIGHT-CHANGE-VEHL                                 P442288 
03133         AND ((WRKRTS-APPORTIONED-FEE = ZERO OR                    P442288 
03134               WRKRTS-APPORTIONED-FEE < ZERO)  AND                 P442288 
03135              (WRKRTS-CNTY-ADD-ON-FEE = ZERO OR                    P442288 
03136               WRKRTS-CNTY-ADD-ON-FEE < ZERO))                     P442288 
                                                                                
      D        DISPLAY '!TC017 - WRKRTS-WEIGHT-CHANGE-VEHL'                     
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '> '                            
      D        DISPLAY ' AND ((WRKRTS-APPORTIONED-FEE = ZERO OR '               
      D        DISPLAY '       WRKRTS-APPORTIONED-FEE < ZERO) '                 
      D         ' <' WRKRTS-APPORTIONED-FEE '> AND'                             
      D        DISPLAY ' (WRKRTS-CNTY-ADD-ON-FEE = ZERO OR '                    
      D        DISPLAY '  WRKRTS-CNTY-ADD-ON-FEE < ZERO))'                      
      D         ' <' WRKRTS-CNTY-ADD-ON-FEE '>'                                 
                                                                                
03137          MOVE 'W'                  TO WRKRTS-PROCESSING-FLAG      P442288 
03138          MOVE 9                    TO RTS-TRANS-ERROR-FLAG        P442288 
03139          ADD 1                     TO WS-NUM-SKIP-WEIGHT-CHANGE   P442288 
03140      ELSE                                                         P442288 
03141      IF WRKRTS-ADD-VEHL-FULL-BILL                                    CL*14
03142         AND WRKRTS-APPORTIONED-FEE < ZERO                            CL*14
                                                                                
      D        DISPLAY '!TC018 - WRKRTS-ADD-VEHL-FULL-BILL '                    
      D                'AND WRKRTS-APPORTIONED-FEE < ZERO '                     
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
      D         ' <' WRKRTS-APPORTIONED-FEE '>'                                 
                                                                                
03143          MOVE 4                    TO ERROR-PROCESSING-SW            CL*14
03144          MOVE ERROR-MSG-054-TEXT   TO ERR-ERROR-MESSAGE-1            CL*14
03145          PERFORM Z-W-WRITE-MSGOUT-REPORT                             CL*14
03146          MOVE 'A'                  TO WRKRTS-PROCESSING-FLAG         CL*14
03147          MOVE 9                    TO RTS-TRANS-ERROR-FLAG           CL*14
03148          ADD 1                     TO WS-NUM-SKIP-AUDIT-MILEAGE      CL*14
03149      ELSE                                                            CL*14
03150      IF (WRKRTS-AUDIT-SUPPLEMENT-VEHL OR                             CL**5
03151          WRKRTS-TYPE-OF-CHANGE-FLAG = 'X' OR                         CL*16
03152          WRKRTS-TYPE-OF-CHANGE-FLAG = 'K')                           CL*16
03153         AND WRKRTS-APPORTIONED-FEE < ZERO                            CL**5
                                                                                
      D        DISPLAY '!TC019 - (WRKRTS-AUDIT-SUPPLEMENT-VEHL OR '             
      D                '  WRKRTS-TYPE-OF-CHANGE-FLAG = X  OR '                  
      D                '  WRKRTS-TYPE-OF-CHANGE-FLAG = K)'                      
      D                '  AND WRKRTS-APPORTIONED-FEE < ZERO'                    
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
      D         ' <' WRKRTS-APPORTIONED-FEE '>'                                 
                                                                                
03154          MOVE 4                    TO ERROR-PROCESSING-SW         P442288 
03155          SUBTRACT WRKRTS-TERP-PORTION FROM WRKRTS-APPORTIONED-FEE P442288 
03156          MOVE WRKRTS-APPORTIONED-FEE TO ERROR-MSG-048-REFUND      P442288 
03157          MOVE ERROR-MSG-048-TEXT   TO ERR-ERROR-MESSAGE-1         P442288 
03158          PERFORM Z-W-WRITE-MSGOUT-REPORT                          P442288 
03159          MOVE WRKRTS-TERP-PORTION  TO ERROR-MSG-049-REFUND        P442288 
03160          MOVE ERROR-MSG-049-TEXT   TO ERR-ERROR-MESSAGE-1         P442288 
03161          PERFORM Z-W-WRITE-MSGOUT-REPORT                          P442288 
03162          MOVE 'A'                  TO WRKRTS-PROCESSING-FLAG      P442288 
03163          MOVE 9                    TO RTS-TRANS-ERROR-FLAG        P442288 
03164          ADD 1                     TO WS-NUM-SKIP-AUDIT-MILEAGE   P442288 
03165      ELSE                                                         P442288 
03166 *---------------------------------------------------------------* P442288 
03167 *          IRPRTS-APPORTIONED-FEE IS > 0 HERE WE SET FLAG TO    * P442288 
03168 *          SEND FUNDS DATA, BUT NO MVUPDATE                     * P442288 
03169 *---------------------------------------------------------------* P442288 
03170      IF WRKRTS-AUDIT-SUPPLEMENT-VEHL                              P442288 
                                                                                
      D        DISPLAY '!TC020 - WRKRTS-AUDIT-SUPPLEMENT-VEHL'                  
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
                                                                                
03171          MOVE 3                    TO RTS-UPDT-ERROR-FLAG         P442288 
03172          MOVE ZERO                 TO RTS-TRANS-ERROR-FLAG        P442288 
03173      ELSE                                                         P442288 
03174      IF WRKRTS-MILEAGE-SUPPLEMENT-VEHL                            P442288 
03175         AND ((WRKRTS-APPORTIONED-FEE = ZERO OR                    P442288 
03176               WRKRTS-APPORTIONED-FEE < ZERO)  AND                 P442288 
03177              (WRKRTS-CNTY-ADD-ON-FEE = ZERO OR                    P442288 
03178               WRKRTS-CNTY-ADD-ON-FEE < ZERO))                     P442288 
                                                                                
      D        DISPLAY '!TC021 - WRKRTS-MILEAGE-SUPPLEMENT-VEHL '               
      D                ' AND ((WRKRTS-APPORTIONED-FEE = ZERO OR '               
      D                '       WRKRTS-APPORTIONED-FEE < ZERO)  AND '            
      D                '      (WRKRTS-CNTY-ADD-ON-FEE = ZERO OR '               
      D                '       WRKRTS-CNTY-ADD-ON-FEE < ZERO))'                 
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
      D         ' <' WRKRTS-APPORTIONED-FEE '>'                                 
      D         ' <' WRKRTS-CNTY-ADD-ON-FEE '>'                                 
                                                                                
03179          MOVE 'M'                  TO WRKRTS-PROCESSING-FLAG      P442288 
03180          MOVE 9                    TO RTS-TRANS-ERROR-FLAG        P442288 
03181          ADD 1                     TO WS-NUM-SKIP-MILE-CHANGE     P442288 
03182      ELSE                                                         P442288 
03183 *---------------------------------------------------------------* P442288 
03184 *    MATCHING RTS TITLE AND REGISTRATION INFORMATION IS NOT     * P442288 
03185 *    NEEDED FOR FUNDS TRANSACTIONS TO GO THROUGH                * P442288 
03186 *---------------------------------------------------------------* P442288 
03187 *---------------------------------------------------------------* P442288 
03188 *    HANDLE REFUNDS BY IRPINV (CANCELLING THE PLATE) ONLY -     * P442288 
03189 *     NO FUNDS ADJUSTMENTS ARE MADE FOR REGIS OR COUNTY FEES.   * P442288 
03190 *     CARRIER REQUESTS REFUNDS FROM THE COUNTY, WHEN APPROPRIATE* P442288 
03191 *---------------------------------------------------------------* P442288 
I3192      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'R'                          P442288 
                                                                                
      D        DISPLAY '!TC022 - WRKRTS-TYPE-OF-CHANGE-FLAG = R'                
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
                                                                                
03193          MOVE 'R'                  TO WRKRTS-PROCESSING-FLAG      P442288 
03194          MOVE 9                    TO RTS-TRANS-ERROR-FLAG        P442288 
03195          ADD 1                     TO WS-NUM-SKIP-REFUND          P442288 
03196      ELSE                                                         P442288 
03197      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'W'                          P442288 
03198         AND ((WRKRTS-APPORTIONED-FEE = ZERO OR                    P442288 
03199               WRKRTS-APPORTIONED-FEE < ZERO)  AND                 P442288 
03200              (WRKRTS-CNTY-ADD-ON-FEE = ZERO OR                    P442288 
03201               WRKRTS-CNTY-ADD-ON-FEE < ZERO))                     P442288 
                                                                                
      D        DISPLAY '!TC023 - WRKRTS-TYPE-OF-CHANGE-FLAG =  W '              
      D                ' AND ((WRKRTS-APPORTIONED-FEE = ZERO OR '               
      D                '       WRKRTS-APPORTIONED-FEE < ZERO)  AND '            
      D                '      (WRKRTS-CNTY-ADD-ON-FEE = ZERO OR '               
      D                '       WRKRTS-CNTY-ADD-ON-FEE < ZERO))'                 
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
      D         ' <' WRKRTS-APPORTIONED-FEE '>'                                 
      D         ' <' WRKRTS-CNTY-ADD-ON-FEE '>'                                 
                                                                                
03202          MOVE 'W'                  TO WRKRTS-PROCESSING-FLAG      P442288 
03203          MOVE 9                    TO RTS-TRANS-ERROR-FLAG        P442288 
03204          ADD 1                     TO WS-NUM-SKIP-WEIGHT-CHANGE   P442288 
03205      ELSE                                                         P442288 
03206 *---------------------------------------------------------------*    CL*16
03207 *     AUDIT SUPPLEMENT SUMMARY RECORD - NOT HANDLED YET         *    CL*16
03208 *     TO ALLOW THESE TESTED 'AUDIT SUMMARY' RECORDS TO BE       *    CL*16
03209 *     PROCESSED, REMOVE THESE COMMENTS AND THE FOLLOWING        *    CL*16
03210 *     IF - - ELSE PARAGRAPH ( 10 LINES OF CODE)                 *    CL*16
03211 *---------------------------------------------------------------*    CL*16
03212 *    IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'X'                        *    CL*16
03213 *        MOVE 4                    TO ERROR-PROCESSING-SW       *    CL*16
03214 *        MOVE ERROR-MSG-052-TEXT   TO ERR-ERROR-MESSAGE-1       *    CL*16
03215 *        PERFORM Z-W-WRITE-MSGOUT-REPORT                        *    CL*16
03216 *        MOVE ERROR-MSG-053-TEXT   TO ERR-ERROR-MESSAGE-1       *    CL*16
03217 *        PERFORM Z-W-WRITE-MSGOUT-REPORT                        *    CL*16
03218 *        MOVE 'A'                  TO WRKRTS-PROCESSING-FLAG    *    CL*16
03219 *        MOVE 9                    TO RTS-TRANS-ERROR-FLAG      *    CL*16
03220 *        ADD 1                     TO WS-NUM-SKIP-AUDIT-MILEAGE *    CL*16
03221 *    ELSE                                                       *    CL*16
03222 *---------------------------------------------------------------*    CL*16
03223 *---------------------------------------------------------------*    CL*16
03224 *     AUDIT SUPPLEMENT SUMMARY RECORD - NO SPECIFIC VEHL INFO   *    CL*16
03225 *---------------------------------------------------------------*    CL*16
03226      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'X'                             CL*16
                                                                                
      D        DISPLAY '!TC024 - WRKRTS-TYPE-OF-CHANGE-FLAG =  X'               
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
                                                                                
03227          MOVE ZEROS                    TO RTS-TRANS-ERROR-FLAG       CL*16
03228          MOVE 3                        TO RTS-UPDT-ERROR-FLAG        CL*16
03229          MOVE ALL '8'                  TO WS-RTS-DOCNO               CL*16
03230          MOVE 'NETTED AUDIT SUPP'      TO WS-CUST-NAME               CL*16
03231          MOVE 'AUDITX'                 TO WS-REGIS-RTS-REGPLTNO      CL*16
03232          MOVE 99                       TO WS-REGCLASSCD              CL*16
03233          MOVE ZEROS                    TO WS-OWNRID                  CL*16
03234          MOVE 'NOVINAUDIT'             TO WS-VIN                     CL*16
03235          MOVE 'AUDIT'                  TO WS-VEHMK                   CL*16
03236          MOVE 2007                     TO WS-VEHMODLYR               CL*16
03237      ELSE                                                            CL*16
03238 *---------------------------------------------------------------*    CL*16
03239 *    CORRECTION ('K') SUPPLEMENT SUMMARY - NO SPECIFIC VEHL INFO*    CL*16
03240 *---------------------------------------------------------------*    CL*16
03241      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'K'                             CL*16
                                                                                
      D        DISPLAY '!TC025 - WRKRTS-TYPE-OF-CHANGE-FLAG = k '               
      D         ' <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                             
                                                                                
03242          MOVE ZEROS                    TO RTS-TRANS-ERROR-FLAG       CL*16
03243          MOVE 3                        TO RTS-UPDT-ERROR-FLAG        CL*16
03244          MOVE ALL '7'                  TO WS-RTS-DOCNO               CL*16
03245          MOVE 'NETTED BILL CORRECTION' TO WS-CUST-NAME               CL*16
03246          MOVE 'BILCOR'                 TO WS-REGIS-RTS-REGPLTNO      CL*16
03247          MOVE 99                       TO WS-REGCLASSCD              CL*16
03248          MOVE ZEROS                    TO WS-OWNRID                  CL*16
03249          MOVE 'NOVINBILLCOR'           TO WS-VIN                     CL*16
03250          MOVE 'BILCOR'                 TO WS-VEHMK                   CL*16
03251          MOVE 2007                     TO WS-VEHMODLYR               CL*16
03252      ELSE                                                            CL*16
03253 *---------------------------------------------------------------*    CL**2
03254 *    IF (WS-TAG-RENTAL-TRLR-ACCOUNT AND                         *    CL**2
03255 *        WRKRTS-GROSS-WEIGHT < 4000)                            *    CL**2
03256 *---------------------------------------------------------------*    CL**2
03257      IF (WRKRTS-ACT-TYPE = 'R' AND                                   CL**2
03258          WRKRTS-GROSS-WEIGHT < 4000)                                 CL**2
                                                                                
      D        DISPLAY '!TC026 - WRKRTS-ACT-TYPE = R '                          
      D                 '<' WRKRTS-ACT-TYPE '> '                                
      D      'AND WRKRTS-GROSS-WEIGHT <' WRKRTS-GROSS-WEIGHT '> < 4000'         
                                                                                
03259 *---------------------------------------------------------------* P442288 
03260 *     THERE ARE NO RENTAL-TRAILERS ON THE RTS TITLE FILE,       * P442288 
03261 *         EXCEPT FOR THOSE WITH GROSS WEIGHTS OVER 4,000 LBS,   * P442288 
03262 *     MOVE IRP DATE INTO MVTRANS FIELDS                         * P442288 
03263 *     SET  INDICATOR FLAG SO THAT MVFUNC DATA IS BYPASSED       * P442288 
03264 *---------------------------------------------------------------* P442288 
03265          MOVE 4                         TO RTS-UPDT-ERROR-FLAG    P442288 
03266          MOVE ZERO                      TO RTS-TRANS-ERROR-FLAG   P442288 
03267 *---------------------------------------------------------------*    CL**9
03268 *        MOVE SPACES                    TO WS-REGIS-RTS-REGPLTNO*    CL**9
03269 *---------------------------------------------------------------*    CL**9
03270          MOVE SPACES                    TO WS-REGIS-RTS-REGPLTNO     CL**9
03271          MOVE WRKRTS-VEHICLE-YEAR       TO WS-VEHMODLYR           P442288 
03272          MOVE WRKRTS-MAKE-OF-VEHICLE    TO WS-VEHMK               P442288 
03273          MOVE WRKRTS-IDENTIFICATION-NBR TO WS-VIN                 P442288 
03274      ELSE                                                         P442288 
03275      IF WS-RTS-DOCNO = ZEROS                                      P442288 
                                                                                
      D        DISPLAY '!TC027 - IF WS-RTS-DOCNO = ZEROS '                      
      D                ' <' WS-RTS-DOCNO '>'                                    
                                                                                
03276          MOVE 9                        TO RTS-TRANS-ERROR-FLAG    P442288 
03277          MOVE 1                        TO RTS-UPDT-ERROR-FLAG     P442288 
03278 *---------------------------------------------------------------* P442288 
03279 *        ALLOW THIS TO GO THROUGH FOR NOW.  RTS TRANS WITH FUNDS* P442288 
03280 *        INFO WILL BE SENT TO RTS.  THE RECORD SHOULD HAVE BEEN * P442288 
03281 *        UPDATED ON THE RTS DATABAS, FOR IMPLEMENTED COUNTIES,  * P442288 
03282 *        BY NOW.  THIS COULD BE A DATA ENTRY ERROR.  IT WILL BE * P442288 
03283 *        REJECTED BY RTS WITH ERROR MESSAGE SENT TO THE REGIONAL* P442288 
03284 *        OFFICE.  FOR NON-IMPLEMENTED COUNTIES, THIS WILL BE    * P442288 
03285 *        HANDLED BY VTRD TITLE CONTROL SECTION (TCS).           * P442288 
03286 *---------------------------------------------------------------* P442288 
03287          MOVE ZERO                     TO RTS-TRANS-ERROR-FLAG    P442288 
03288 *        SET  RTS-NO-TITLE-DUMMY   INDICATOR FLAG               * P442288 
03289 *        ALSO RTS-INVALID-UPDATE   INDICATOR FLAG               * P442288 
03290 *        MOVE 2                        TO RTS-UPDT-ERROR-FLAG     P442288 
03291 *        MOVE 4                        TO ERROR-PROCESSING-SW     P442288 
03292 *        MOVE WS-PLATE-NO-RTS          TO ERR-LICN                P442288 
03293 *        MOVE ERROR-MSG-019-TEXT       TO ERR-ERROR-MESSAGE-1     P442288 
03294 *        PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
03295 *        MOVE SPACES                   TO  ERR-LICN               P442288 
03296          ADD 1                         TO WS-DUMMY-RECORDS-COUNT  P442288 
03297      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC028 - PERFORM 2111-GET-RTS-TITLE-DATA'               
                                                                                
03298          MOVE 'I'          TO RTS-TTL-2111-INQ-FLAG               P442288 
03299          PERFORM 2111-GET-RTS-TITLE-DATA THRU                     P442288 
03300                  2111-GET-RTS-TITLE-DATA-EXIT                     P442288 
                                                                                
      D        DISPLAY 'AFTER CALL OF 2111-GET-RTS-TITLE-DATA'                  
      D        DISPLAY 'AFTER 2111- RTS-TRANS-ERROR-FLAG <'                     
      D           RTS-TRANS-ERROR-FLAG '>'                                      
      D        DISPLAY 'AFTER 2111- RTS-UPDT-ERROR-FLAG <'                      
      D           RTS-UPDT-ERROR-FLAG '>'                                       
                                                                                
03301          IF RTS-NO-ERROR      AND                                 P442288 
03302             RTS-VALID-UPDATE                                      P442288 
                                                                                
      D        DISPLAY '!TC047 - IF RTS-NO-ERROR(0) <'                          
      D                   RTS-TRANS-ERROR-FLAG '>  '                            
      D            'AND RTS-VALID-UPDATE(0) <' RTS-UPDT-ERROR-FLAG '>'          
                                                                                
03303              PERFORM 2117-CHECK-DOCNO-VIN                         P442288 
                                                                                
      D        DISPLAY 'AFTER CALL OF 2117-CHECK-DOCNO-VIN'                     
      D        DISPLAY 'AFTER 2117- RTS-TRANS-ERROR-FLAG <'                     
      D           RTS-TRANS-ERROR-FLAG '>'                                      
      D        DISPLAY 'AFTER 2117- RTS-UPDT-ERROR-FLAG <'                      
      D           RTS-UPDT-ERROR-FLAG '>'                                       
                                                                                
03304          END-IF                                                   P442288 
03305          IF RTS-NO-ERROR      AND                                 P442288 
03306             RTS-VALID-UPDATE                                      P442288 
                                                                                
      D        DISPLAY '!TC054 - IF RTS-NO-ERROR(0) <'                          
      D                   RTS-TRANS-ERROR-FLAG '>'                              
      D            '> AND RTS-VALID-UPDATE(0) <' RTS-UPDT-ERROR-FLAG '>'        
                                                                                
03307              PERFORM 2112-GET-RTS-REGIS-DATA THRU                 P442288 
03308                      2112-GET-RTS-REGIS-DATA-EXIT                 P442288 
03309          END-IF                                                   P442288 
03310      END-IF.                                                      P442288 
03311                                                                   P442288 
03312      IF TESTING-SHOW-DISPLAYS                                     P442288 
03313          DISPLAY '@ INSIDE 211-PROCESS BEFORE PERFORM 2115--VIN:' P442288 
03314            ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG                P442288 
03315            ' UPDT ERR FL  = ' RTS-UPDT-ERROR-FLAG                 P442288 
03316            ' ERROR SW     = ' ERROR-PROCESSING-SW                 P442288 
03317          DISPLAY ' INV=' RTS-IRPINV-FLAG                          P442288 
03318            '-- EXP-ACT-TYPE :' WRKRTS-ACT-TYPE                       CL**2
03319      END-IF.                                                      P442288 
03320                                                                   P442288 
03321      IF RTS-NO-ERROR                    AND                       P442288 
03322         (RTS-NO-TITLE                   OR                        P442288 
03323          RTS-TTL-SKIPPED-FLAG     = 'Y' OR                        P442288 
03324 *---------------------------------------------------------------*    CL**2
03325 *        (WS-RTS-DOCNO  =  ZEROS AND                            *    CL**2
03326 *         NOT WS-TAG-RENTAL-TRLR-ACCOUNT))                      *    CL**2
03327 *---------------------------------------------------------------*    CL**2
03328          (WS-RTS-DOCNO  =  ZEROS AND                                 CL**2
03329           WRKRTS-ACT-TYPE NOT = 'R'))                                CL**2
                                                                                
      D        DISPLAY '!TC080 - IF RTS-NO-ERROR(0) <'                          
      D                 RTS-TRANS-ERROR-FLAG '> '                               
      D                'AND RTS-NO-TITLE(1) <' RTS-UPDT-ERROR-FLAG '>'          
      D                ' OR RTS-TTL-SKIPPED-FLAG = Y <'                         
      D                     RTS-TTL-SKIPPED-FLAG '> '                           
      D                'OR (WS-RTS-DOCNO = 0 <' WS-RTS-DOCNO '> AND '           
      D                'WRKRTS-ACT-TYPE NOT = R)) <'                            
      D                 WRKRTS-ACT-TYPE '>'                                     
                                                                                
03330          PERFORM 2115-FIND-RTS-BY-VIN THRU                        P442288 
03331                  2115-FIND-RTS-BY-VIN-EXIT                        P442288 
03332          IF WS-2115-RETURN-CODE = ZERO                            P442288 
                                                                                
      D            DISPLAY '!TC126 - IF WS-2115-RETURN-CODE = ZERO '            
      D                ' <' WS-2115-RETURN-CODE '>'                             
                                                                                
03333              MOVE WRKRTS-DOCNO TO ERROR-MSG-028-DOCNO-O           P442288 
03334                                   WRKRTS-REPLACED-DOCNO           P442288 
03335              MOVE 'I'          TO WRKRTS-PROGRAM-CHG-FLAG            CL**4
03336              MOVE WS-VIN-DOCNO TO WRKRTS-DOCNO                    P442288 
03337                                   WS-RTS-DOCNO                    P442288 
03338                                   ERROR-MSG-028-DOCNO-N           P442288 
03339              MOVE ZERO         TO RTS-TRANS-ERROR-FLAG            P442288 
03340                                   RTS-UPDT-ERROR-FLAG             P442288 
03341              MOVE 'I'                    TO RTS-TTL-2111-INQ-FLAG P442288 
03342              PERFORM 2111-GET-RTS-TITLE-DATA THRU                 P442288 
03343                      2111-GET-RTS-TITLE-DATA-EXIT                 P442288 
03344              PERFORM 2118-CHECK-LONGEVITY                         P442288 
03345              IF RTS-NO-ERROR                                      P442288 
                                                                                
      D            DISPLAY '!TC130 - IF RTS-NO-ERROR(0) <'                      
      D                ' <' RTS-TRANS-ERROR-FLAG '>'                            
                                                                                
03346                  PERFORM 2112-GET-RTS-REGIS-DATA THRU             P442288 
03347                          2112-GET-RTS-REGIS-DATA-EXIT             P442288 
03348              END-IF                                               P442288 
03349              IF RTS-NO-ERROR                                      P442288 
                                                                                
      D            DISPLAY '!TC131 - IF RTS-NO-ERROR(0) <'                      
      D                ' <' RTS-TRANS-ERROR-FLAG '>'                            
                                                                                
03350                  PERFORM 2116-UPDATE-IRP-DOCNO THRU               P442288 
03351                          2116-UPDATE-IRP-DOCNO-EXIT               P442288 
03352              END-IF                                               P442288 
03353          END-IF                                                   P442288 
03354      END-IF.                                                      P442288 
03355                                                                   P442288 
03356      IF RTS-NO-TITLE    AND                                       P442288 
03357         RTS-TTL-SKIPPED-FLAG NOT = 'Y'                            P442288 
                                                                                
      D        DISPLAY '!TC135 - IF RTS-NO-TITLE '                              
      D          ' <' RTS-UPDT-ERROR-FLAG '>'                           P442288 
      D        DISPLAY 'AND RTS-TTL-SKIPPED-FLAG NOT = Y'                       
      D          ' <' RTS-TRANS-ERROR-FLAG '>'                          P442288 
                                                                                
03358         IF TESTING-SHOW-DISPLAYS                                  P442288 
03359             DISPLAY '@211 NO TITLE --- '                          P442288 
03359                 ' <' RTS-TTL-SKIPPED-FLAG '>'                     P442288 
03360         END-IF                                                    P442288 
03361         MOVE 'P'                        TO WRKRTS-PROCESSING-FLAG P442288 
03362         MOVE WS-CRNT-DATE-DT            TO WRKRTS-LAST-CHG-DT     P442288 
03363 *   *   GEN ERR MESSAGE BUT SEND OUT RTS TRANSACTIONS ANYWAY*     P442288 
03364         MOVE 1                        TO ERROR-PROCESSING-SW      P442288 
03365 *   *   SET  RTS-NO-TITLE         INDICATOR FLAG            *     P442288 
03366 *   *   ALSO RTS-INVALID-UPDATE   INDICATOR FLAG            *     P442288 
03367         MOVE 1                        TO RTS-UPDT-ERROR-FLAG      P442288 
03368         MOVE WRKRTS-IDENTIFICATION-NBR TO ERROR-MSG-010-VIN       P442288 
03369         MOVE WS-RTS-DOCNO              TO ERROR-MSG-010-TITLE     P442288 
03370         MOVE WS-PLATE-NO-RTS           TO ERR-LICN                P442288 
03371         IF WS-TTLPROCSCD = 'S '                                   P442288 
                                                                                
      D        DISPLAY '!TC136 - IF WS-TTLPROCSCD =  S  <'                      
      D                        WS-TTLPROCSCD '>'                                
                                                                                
03372             MOVE 'SUPERSEDED'          TO ERROR-MSG-010-TITLE     P442288 
03373         END-IF                                                    P442288 
03374         IF WS-TTLPROCSCD = 'I '                                   P442288 
                                                                                
      D        DISPLAY '!TC137 -  IF WS-TTLPROCSCD =  I  <'                     
      D                        WS-TTLPROCSCD '>'                                
                                                                                
03375             MOVE 'DELETE PENDING'     TO ERROR-MSG-010-RETRY      P442288 
03376         END-IF                                                    P442288 
03377         MOVE ERROR-MSG-010-TEXT       TO ERR-ERROR-MESSAGE-1      P442288 
03378         PERFORM Z-4-CONTROL-EDIT-ERROR                            P442288 
03379         MOVE SPACES                   TO ERR-LICN                 P442288 
03380         END-IF.                                                   P442288 
03381                                                                   P442288 
03382      IF RTS-NO-ERROR    AND                                          CL**9
03383 *---------------------------------------------------------------*    CL**9
03384 *      USE REGIS FILE PREV PLATE - NOT FROM TITLE FILE          *    CL**9
03385 *---------------------------------------------------------------*    CL**9
03386         WS-PLATE-NO-RTS = WS-REGIS-RTS-PRVPLTNO      AND             CL**9
03387         WS-PLATE-NO-RTS NOT = WS-REGIS-RTS-REGPLTNO  AND             CL*13
03388         WS-REGIS-RTS-REG-EXP-DT > WRKRTS-BEGINNING-REG-DT            CL*13
                                                                                
      D        DISPLAY '!TC138 - IF RTS-NO-ERROR '                              
      D          ' <' RTS-UPDT-ERROR-FLAG '>'                           P442288 
      D        DISPLAY 'AND WS-PLATE-NO-RTS <' WS-PLATE-NO-RTS '>  =  '         
      D          'WS-REGIS-RTS-PRVPLTNO <' WS-REGIS-RTS-PRVPLTNO '>'            
      D        DISPLAY 'AND WS-PLATE-NO-RTS <' WS-PLATE-NO-RTS '>  NOT'         
      D          ' = WS-REGIS-RTS-REGPLTNO <' WS-REGIS-RTS-REGPLTNO '>'         
      D        DISPLAY 'AND WS-REGIS-RTS-REG-EXP-DT <'                          
      D                     WS-REGIS-RTS-REG-EXP-DT '>  >  '                    
      D                'WRKRTS-BEGINNING-REG-DT <'                              
      D                 WRKRTS-BEGINNING-REG-DT '>'                             
                                                                                
03389         IF TESTING-SHOW-DISPLAYS                                     CL**9
03390             DISPLAY '@211 IRP = PREVIOUS PLATE - DO NO MVUPDT '      CL**9
03391             DISPLAY '  IRP REG BEGINS ' WRKRTS-BEGINNING-REG-DT      CL*13
03392             DISPLAY '  RTS REG ENDS ' WS-REGIS-RTS-REG-EXP-DT        CL*13
03393                ' FOR ' WS-REGIS-RTS-REGPLTNO                         CL*13
03394         END-IF                                                       CL**9
03395         MOVE 'P'                        TO WRKRTS-PROCESSING-FLAG    CL**9
03396         MOVE WS-CRNT-DATE-DT            TO WRKRTS-LAST-CHG-DT        CL**9
03397 *   *   GEN ERR MESSAGE BUT SEND OUT RTS TRANSACTIONS ANYWAY*        CL**9
03398         MOVE 1                        TO ERROR-PROCESSING-SW         CL**9
03399 *   *   ALSO RTS-INVALID-UPDATE SKIP RTS MVUPDT TRANSACTION *        CL**9
03400         MOVE 5                        TO RTS-UPDT-ERROR-FLAG         CL**9
03401         MOVE SPACE                    TO RTS-IRPINV-FLAG             CL*11
03402         MOVE WS-PLATE-NO-RTS          TO ERR-LICN                    CL**9
03403         MOVE WRKRTS-IDENTIFICATION-NBR TO ERROR-MSG-050-VIN          CL*13
03404         MOVE WS-REGIS-RTS-REGPLTNO    TO ERROR-MSG-050-PLT           CL**9
03405         MOVE ERROR-MSG-050-TEXT       TO ERR-ERROR-MESSAGE-1         CL**9
03406         PERFORM Z-4-CONTROL-EDIT-ERROR                               CL**9
03407         MOVE SPACES                   TO ERR-LICN                    CL**9
03408         END-IF.                                                      CL**9
03409                                                                      CL**9
03410      IF RTS-NORM-IRP-UPDATE-RUN                 AND                  CL**7
03411         RTS-NOGEN-MV-FUNC                       AND                  CL**7
03412          WRKRTS-APPORTIONED-FEE         = ZERO  AND                  CL**7
03413          WRKRTS-INS-PORTION             = ZERO  AND                  CL**7
03414          WRKRTS-TERP-PORTION            = ZERO  AND                  CL**7
03415          WRKRTS-REFLECT-PORTION         = ZERO  AND                  CL**7
03416          WRKRTS-AUTOMATE-PORTION        = ZERO  AND                  CL**7
03417          WRKRTS-CNTY-CHILD-SAFE-PORTION = ZERO  AND                  CL**7
03418          WRKRTS-CNTY-MOBILITY-PORTION   = ZERO  AND                  CL*14
03419          WRKRTS-CNTY-ADD-ON-FEE         = ZERO  AND                  CL**7
03420          WRKRTS-REPLACEMENT-FEE         = ZERO                       CL**7
                                                                                
      D        DISPLAY '!TC139 - IF RTS-NORM-IRP-UPDATE-RUN (N,T,M) <'          
      D                RTS-IF-RUN-TYPE '>'                                      
      D        DISPLAY 'AND RTS-NOGEN-MV-FUNC (3 THRU 5) <'                     
      D                RTS-UPDT-ERROR-FLAG '>'                                  
      D        DISPLAY 'AND WRKRTS-APPORTIONED-FEE <'                           
      D                     WRKRTS-APPORTIONED-FEE '>'                          
      D        DISPLAY 'AND WRKRTS-INS-PORTION <' WRKRTS-INS-PORTION '>'        
      D        DISPLAY 'AND WRKRTS-TERP-PORTION <'                              
      D                     WRKRTS-TERP-PORTION '>'                             
      D        DISPLAY 'AND WRKRTS-REFLECT-PORTION <'                           
      D                     WRKRTS-REFLECT-PORTION '>'                          
      D        DISPLAY 'AND WRKRTS-AUTOMATE-PORTION <'                          
      D                     WRKRTS-AUTOMATE-PORTION '>'                         
      D        DISPLAY 'AND WRKRTS-CNTY-CHILD-SAFE-PORTION <'                   
      D                     WRKRTS-CNTY-CHILD-SAFE-PORTION '>'                  
      D        DISPLAY 'AND WRKRTS-CNTY-MOBILITY-PORTION <'                     
      D                     WRKRTS-CNTY-MOBILITY-PORTION '>'                    
      D        DISPLAY 'AND WRKRTS-CNTY-ADD-ON-FEE <'                           
      D                     WRKRTS-CNTY-ADD-ON-FEE '>'                          
      D        DISPLAY 'AND WRKRTS-REPLACEMENT-FEE <'                           
      D                     WRKRTS-REPLACEMENT-FEE '>'                          
      D                                                                         
                                                                                
03421          MOVE 'N'                  TO WRKRTS-PROCESSING-FLAG         CL**7
03422          MOVE 1                    TO ERROR-PROCESSING-SW            CL**7
03423          MOVE WS-RTS-DOCNO         TO ERROR-MSG-040-DOCNO            CL**7
03424          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN               CL**7
03425          MOVE ERROR-MSG-040-TEXT   TO ERR-ERROR-MESSAGE-1            CL**7
03426          PERFORM Z-W-WRITE-MSGOUT-REPORT                             CL**7
03427          PERFORM Z-10-ARCHIVE-RTS-RECORDS                            CL**7
03428      ELSE                                                            CL**7
03429      IF RTS-NO-ERROR             AND                              P442288 
03430         RTS-NORM-IRP-UPDATE-RUN                                   P442288 
                                                                                
      D        DISPLAY '!TC140 - IF RTS-NO-ERROR(0) <'                          
      D                   RTS-TRANS-ERROR-FLAG '>'                              
      D            '> AND RTS-NORM-IRP-UPDATE-RUN (N,T,M) <'                    
      D                   RTS-IF-RUN-TYPE '>'                                   
                                                                                
03431          PERFORM 2113-CREATE-RTS-TRANSACTIONS                     P442288 
03432             THRU 2113-CREATE-RTS-TRANS-EXIT                       P442288 
03433      ELSE                                                         P442288 
03434      IF RTS-NO-ERROR                                              P442288 
03435          IF RTS-SYNC-REG-UPDATE-RUN  AND                          P442288 
03436             RTS-VALID-UPDATE                                      P442288 
                                                                                
      D        DISPLAY '!TC231 - IF RTS-NO-ERROR(0) <'                          
      D                   RTS-TRANS-ERROR-FLAG '>'                              
      D        DISPLAY 'AND IF RTS-SYNC-REG-UPDATE-RUN(S) <'                    
      D                RTS-IF-RUN-TYPE '> AND'                                  
      D        DISPLAY 'RTS-VALID-UPDATE(0) <' RTS-UPDT-ERROR-FLAG '>'          
                                                                                
03437              PERFORM 2113-CREATE-RTS-TRANSACTIONS                 P442288 
03438                 THRU 2113-CREATE-RTS-TRANS-EXIT                   P442288 
03439          ELSE                                                     P442288 
                                                                                
      D        DISPLAY '!TC232 - PERFORM  Z-10-ARCHIVE-RTS-RECORDS'             
      D                                                                         
                                                                                
03440              PERFORM Z-10-ARCHIVE-RTS-RECORDS                     P442288 
03441              IF TESTING-SHOW-DISPLAYS                             P442288 
03442                  DISPLAY '@211- SKIP 2113-CREATE TRANS ===>'      P442288 
03443                      ' UPDATE FLAG = ' RTS-UPDT-ERROR-FLAG        P442288 
03444                      ' RTS-KEY====>' WORK-FD-KEY                  P442288 
03445                  DISPLAY 'SUP=' WRKRTS-SUPP-NBR                   P442288 
03446                        ' DT =' WRKRTS-REGIS-DT                    P442288 
03447                        ' ACT=' WRKRTS-ACCOUNT-NBR                 P442288 
03448                        ' FLT=' WRKRTS-FLEET-NBR                   P442288 
03449                        ' SFX=' WRKRTS-SUFFIX-NBR                  P442288 
03450                        ' BILL=' WRKRTS-TYPE-OF-CHANGE-FLAG        P442288 
03451                        ' RGN='  WRKRTS-REGION                     P442288 
03452                        ' # '   WRKRTS-REGION-NUMBER               P442288 
03453                        ' EQP=' WRKRTS-OWNER-EQUIP-NBR             P442288 
03454                        ' FLG=' WRKRTS-PROCESSING-FLAG             P442288 
03455                        ' LST UPD=' WRKRTS-LAST-CHG-DT             P442288 
03456                  DISPLAY 'DATA = ' WRKRTS-CURRENT-TEXAS-LIC-NBR   P442288 
03457                     ' ' WRKRTS-REG-EXP-DT                         P442288 
03458                     '-' WRKRTS-BEGINNING-REG-DT                   P442288 
03459                     ' ' WS-RTS-DOCNO ' ' WRKRTS-IDENTIFICATION-NBRP442288 
03460                     ' CHG TYPE ' WRKRTS-TYPE-OF-CHANGE-FLAG       P442288 
03461                     ' APP= $' WRKRTS-APPORTIONED-FEE              P442288 
03462                     ' CTY= $' WRKRTS-CNTY-ADD-ON-FEE              P442288 
03463              END-IF                                               P442288 
03464          END-IF                                                   P442288 
03465      ELSE                                                         P442288 
03466      IF RTS-BYPASS-FLAG                                           P442288 
                                                                                
      D        DISPLAY '!TC233 - IF RTS-BYPASS-FLAG(9) <'                       
      D                             RTS-TRANS-ERROR-FLAG '>'                    
                                                                                
03467          PERFORM Z-10-ARCHIVE-RTS-RECORDS                         P442288 
03468          IF TESTING-SHOW-DISPLAYS                                 P442288 
03469              DISPLAY '@211- BYPASS INITIAL FUNDS & MVUPDT '       P442288 
03470                      'PROCESS ===>'                               P442288 
03471                  ' BYPASS FLAG = ' WRKRTS-PROCESSING-FLAG         P442288 
03472                  ' RTS-KEY====>' WORK-FD-KEY                      P442288 
03473              DISPLAY 'SUP=' WRKRTS-SUPP-NBR                       P442288 
03474                    ' DT =' WRKRTS-REGIS-DT                        P442288 
03475                    ' ACT=' WRKRTS-ACCOUNT-NBR                     P442288 
03476                    ' FLT=' WRKRTS-FLEET-NBR                       P442288 
03477                    ' SFX=' WRKRTS-SUFFIX-NBR                      P442288 
03478                    ' BILL=' WRKRTS-TYPE-OF-CHANGE-FLAG            P442288 
03479                    ' RGN='  WRKRTS-REGION                         P442288 
03480                    ' # '   WRKRTS-REGION-NUMBER                   P442288 
03481                    ' EQP=' WRKRTS-OWNER-EQUIP-NBR                 P442288 
03482                    ' FLG=' WRKRTS-PROCESSING-FLAG                 P442288 
03483                    ' LST UPD=' WRKRTS-LAST-CHG-DT                 P442288 
03484              DISPLAY 'DATA = ' WRKRTS-CURRENT-TEXAS-LIC-NBR ' '   P442288 
03485                 WRKRTS-REG-EXP-DT '-' WRKRTS-BEGINNING-REG-DT     P442288 
03486                 ' ' WS-RTS-DOCNO ' ' WRKRTS-IDENTIFICATION-NBR    P442288 
03487                 ' CHG TYPE ' WRKRTS-TYPE-OF-CHANGE-FLAG           P442288 
03488                 ' APP= $' WRKRTS-APPORTIONED-FEE                  P442288 
03489                 ' CTY= $' WRKRTS-CNTY-ADD-ON-FEE                  P442288 
03490      END-IF.                                                      P442288 
03491                                                                   P442288 
03492 *---------------------------------------------------------------* P442288 
03493 *    CURRENTLY ONLY EVEN EXCHANGES ('E') GO PAST THIS POINT     * P442288 
03494 *---------------------------------------------------------------* P442288 
03495 *---------------------------------------------------------------* P442288 
03496 *    IF (REFUND-TEXAS-FEES-SUPP  AND                            * P442288 
03497 *        WS-RTS-DOCNO NOT = ZEROS)               OR             * P442288 
03498 *---------------------------------------------------------------* P442288 
03499      IF (WRKRTS-TYPE-OF-CHANGE-FLAG = 'E'  AND                    P442288 
03500          WRKRTS-EXCHANGED-DOCNO NOT = ZEROS)   OR                 P442288 
03501          WRKRTS-TYPE-OF-CHANGE-FLAG = 'R'                         P442288 
03502          NEXT SENTENCE                                            P442288 
03503      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC234 - '                                              
      D           'IF (WRKRTS-TYPE-OF-CHANGE-FLAG <'                            
      D               WRKRTS-TYPE-OF-CHANGE-FLAG '> = E'                        
      D        DISPLAY 'WRKRTS-EXCHANGED-DOCNO <'                               
      D                 WRKRTS-EXCHANGED-DOCNO '> NOT = ZEROES)'                
      D        DISPLAY 'OR WRKRTS-TYPE-OF-CHANGE-FLAG = R'                      
                                                                                
03504          PERFORM   Z-2-READ-IRP-RTS-FILE THRU                     P442288 
03505                    Z-2-READ-IRP-RTS-FILE-EXIT                     P442288 
03506          MOVE  ZERO                      TO RTS-UPDT-ERROR-FLAG   P442288 
03507          MOVE  ZERO                      TO RTS-TRANS-ERROR-FLAG  P442288 
03508          MOVE  SPACES                    TO RTS-IRPINV-FLAG       P442288 
03509          GO TO 211-PROCESS-RTS-INTERFACE-EXIT.                    P442288 
03510                                                                   P442288 
03511 *---------------------------------------------------------------* P442288 
03512 *    CHECK FOR REFUNDS OR EXCHANGES WHICH CANCEL REGISTRATION   * P442288 
03513 *    AND DO NOTHING ELSE.                                       * P442288 
03514 *---------------------------------------------------------------* P442288 
03515                                                                   P442288 
03516 *---------------------------------------------------------------* P442288 
03517 *       RTS-VALID-UPDATE                                        * P442288 
03518 *       SHOULD THIS BE CHECKED FOR SOME REASON                  * P442288 
03519 *---------------------------------------------------------------* P442288 
03520                                                                   P442288 
03521      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'E'                          P442288 
                                                                                
      D        DISPLAY '!TC235 - IF WRKRTS-TYPE-OF-CHANGE-FLAG <'               
      D          WRKRTS-TYPE-OF-CHANGE-FLAG '> = E'                             
                                                                                
03522          MOVE WRKRTS-EXCHANGED-DOCNO  TO WS-RTS-DOCNO             P442288 
03523          MOVE 'EXCHANGED REG: '       TO ERROR-MSG-016-TYPE       P442288 
03524          MOVE 'E'                     TO RTS-IRPINV-FLAG          P442288 
03525      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC236 - IF NOT WRKRTS-TYPE-OF-CHANGE-FLAG <'           
      D          WRKRTS-TYPE-OF-CHANGE-FLAG '> = E'                             
                                                                                
03526          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR                        P442288 
03527                                       TO WRKRTS-EXCHANGED-LICNO   P442288 
03528          MOVE 'REFUNDED  REG: '       TO ERROR-MSG-016-TYPE       P442288 
03529          MOVE 'R'                     TO RTS-IRPINV-FLAG.         P442288 
03530                                                                   P442288 
                                                                                
      D    DISPLAY '!TC237 - PERFORM 2111-GET-RTS-TITLE-DATA '                  
                                                                                
03531      MOVE 'C'                         TO RTS-TTL-2111-INQ-FLAG.   P442288 
03532      PERFORM 2111-GET-RTS-TITLE-DATA THRU                         P442288 
03533              2111-GET-RTS-TITLE-DATA-EXIT.                        P442288 
03534                                                                   P442288 
03535      IF RTS-NO-ERROR                                              P442288 
                                                                                
      D        DISPLAY '!TC238 - IF RTS-NO-ERROR(0) <'                          
      D                   RTS-TRANS-ERROR-FLAG '>'                              
                                                                                
03536          PERFORM 2112-GET-RTS-REGIS-DATA THRU                     P442288 
03537                  2112-GET-RTS-REGIS-DATA-EXIT                     P442288 
03538      END-IF.                                                      P442288 
03539                                                                   P442288 
                                                                                
      D    DISPLAY '!TCXXN - RTS-NO-ERROR(0) <'                                 
      D             RTS-TRANS-ERROR-FLAG '> '                                   
      D            'RTS-VALID-UPDATE(0) <' RTS-UPDT-ERROR-FLAG '>'              
      D    DISPLAY 'WRKRTS-EXCHANGED-LICNO <' WRKRTS-EXCHANGED-LICNO '>'        
      D           ' WS-REGIS-RTS-REGPLTNO <' WS-REGIS-RTS-REGPLTNO '>'          
      D    DISPLAY 'WS-REGINVLDINDI <' WS-REGINVLDINDI '>'                      
      D    DISPLAY 'RTS-IF-RUN-TYPE <' RTS-IF-RUN-TYPE '>'                      
      D    DISPLAY 'WRKRTS-IDENTIFICATION-NBR <'                                
      D             WRKRTS-IDENTIFICATION-NBR '>'                               
      D           ' WS-VIN <' WS-VIN '>'                                        
      D    DISPLAY 'WRKRTS-TYPE-OF-CHANGE-FLAG <'                               
      D             WRKRTS-TYPE-OF-CHANGE-FLAG '>'                              
                                                                                
03540      IF RTS-NO-ERROR      AND                                     P442288 
03541         RTS-VALID-UPDATE  AND                                     P442288 
03542         WRKRTS-EXCHANGED-LICNO = WS-REGIS-RTS-REGPLTNO               CL**9
03543          IF WS-REGINVLDINDI = ZEROS                               P442288 
03544              IF RTS-NORM-IRP-UPDATE-RUN OR                        P442288 
03545                 RTS-SYNC-REG-UPDATE-RUN                           P442288 
03546                  IF WRKRTS-IDENTIFICATION-NBR = WS-VIN   AND      P442288 
03547                     WRKRTS-TYPE-OF-CHANGE-FLAG = 'E'              P442288 
                                                                                
      D        DISPLAY '!TC239 - IF WRKRTS-IDENTIFICATION-NBR <'                
      D           WRKRTS-IDENTIFICATION-NBR '> = WS-VIN <'                      
      D                WS-VIN '> AND'                                           
      D        DISPLAY 'AND WRKRTS-TYPE-OF-CHANGE-FLAG <'                       
      D                     WRKRTS-TYPE-OF-CHANGE-FLAG '> = E'                  
                                                                                
03548                      PERFORM 2119-SAME-VIN-EXCHANGE               P442288 
03549                  ELSE                                             P442288 
                                                                                
      D        DISPLAY '!TC264.a - IF NOT WRKRTS-IDENTIFICATION-NBR <'          
      D           WRKRTS-IDENTIFICATION-NBR '> = WS-VIN <'                      
      D                WS-VIN '> '                                              
      D        DISPLAY 'AND NOT WRKRTS-TYPE-OF-CHANGE-FLAG <'                   
      D                     WRKRTS-TYPE-OF-CHANGE-FLAG '> = E'                  
                                                                                
03550                      PERFORM 2113-CREATE-RTS-TRANSACTIONS         P442288 
03551                         THRU 2113-CREATE-RTS-TRANS-EXIT           P442288 
03552              ELSE                                                 P442288 
03553                  NEXT SENTENCE                                    P442288 
03554          ELSE                                                     P442288 
                                                                                
      D        DISPLAY '!TC264 - IF NOT WS-REGINVLDINDI <'                      
      D                                 WS-REGINVLDINDI '> = ZEROS'             
                                                                                
03555              MOVE 4                       TO ERROR-PROCESSING-SW  P442288 
03556              MOVE 3                       TO RTS-UPDT-ERROR-FLAG  P442288 
03557              MOVE WS-REGIS-RTS-REGPLTNO   TO ERROR-MSG-016-PLT       CL**9
03558              MOVE WS-REGIS-RTS-REG-EXP-DT TO ERROR-MSG-016-EXP-DT    CL**9
03559              MOVE WS-RTS-DOCNO            TO ERROR-MSG-016-DOCNO  P442288 
03560              MOVE WS-PLATE-NO-RTS         TO ERR-LICN             P442288 
03561              MOVE 'ALREADY INV'           TO ERROR-MSG-016-NOW    P442288 
03562              MOVE ERROR-MSG-016-TEXT      TO ERR-ERROR-MESSAGE-1  P442288 
03563              PERFORM Z-V-WRITE-MVUPDT-REPORT                      P442288 
03564              MOVE SPACES                  TO  ERR-LICN            P442288 
03565      ELSE                                                         P442288 
03566          IF WRKRTS-TYPE-OF-CHANGE-FLAG  = 'R'                     P442288 
                                                                                
      D        DISPLAY '!TC265 - IF WRKRTS-TYPE-OF-CHANGE-FLAG <'               
      D                             WRKRTS-TYPE-OF-CHANGE-FLAG '> = R'          
                                                                                
03567              MOVE 'REFUNDED '         TO ERROR-MSG-026-TYPE       P442288 
03568          ELSE                                                     P442288 
                                                                                
      D        DISPLAY '!TC266 - IF WRKRTS-TYPE-OF-CHANGE-FLAG <'               
      D                             WRKRTS-TYPE-OF-CHANGE-FLAG '> = R'          
                                                                                
03569              MOVE 'EXCHANGED'         TO ERROR-MSG-026-TYPE       P442288 
03570          END-IF                                                   P442288 
03571          MOVE ZERO                    TO RTS-TRANS-ERROR-FLAG     P442288 
03572          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
03573          MOVE ZERO                    TO RTS-UPDT-ERROR-FLAG      P442288 
03574          MOVE WS-RTS-DOCNO            TO ERROR-MSG-026-DOCNO      P442288 
03575          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN            P442288 
03576          MOVE ERROR-MSG-026-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
03577          PERFORM Z-W-WRITE-MSGOUT-REPORT                          P442288 
03578          MOVE SPACES                  TO ERR-LICN.                P442288 
03579                                                                   P442288 
03580      PERFORM   Z-2-READ-IRP-RTS-FILE THRU                         P442288 
03581                Z-2-READ-IRP-RTS-FILE-EXIT.                        P442288 
03582                                                                   P442288 
03583      MOVE  ZERO                      TO RTS-UPDT-ERROR-FLAG.      P442288 
03584      MOVE  ZERO                      TO RTS-TRANS-ERROR-FLAG.     P442288 
03585      MOVE SPACES                     TO RTS-IRPINV-FLAG.          P442288 
03586                                                                   P442288 
03587  211-PROCESS-RTS-INTERFACE-EXIT.                                  P442288 
03588      EXIT.                                                        P442288 
03589    EJECT                                                             CL**9
03590                                                                   P442288 
03591  2111-GET-RTS-TITLE-DATA.                                         P442288 
03592                                                                   P442288 
03593      IF TESTING-SHOW-DISPLAYS                                     P442288 
03594          DISPLAY '@ 2111-GET-RTS-TITLE :'                         P442288 
03595            ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG                P442288 
03596            ' UPDT ERR FL  = ' RTS-UPDT-ERROR-FLAG                 P442288 
03597            ' ERROR SW     = ' ERROR-PROCESSING-SW                 P442288 
03598          DISPLAY ' INV=' RTS-IRPINV-FLAG                          P442288 
03599            '-- EXP-ACT-TYPE :' WRKRTS-ACT-TYPE                       CL**2
03600      END-IF.                                                      P442288 
03601                                                                   P442288 
03602                                                                   P442288 
03603 *  ---------------------------------------------------------*     P442288 
03604 *  (1.0) CHECK IF TITLE    (DOCNO) EXIST IN RTS-TITLE DB    *     P442288 
03605 *  ---------------------------------------------------------*     P442288 
03606                                                                   P442288 
03607 *---------------------------------------------------------------* P442288 
03608 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
03609 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
03610 *---------------------------------------------------------------* P442288 
03611      IF TESTING-SHOW-DISPLAYS                                     P442288 
03612          DISPLAY '@ 2111- GET RTS MATCH FOR==>'                   P442288 
03613            '"' WRKRTS-DOCNO '"'                                   P442288 
03614            ' SUP =' WRKRTS-SUPP-NBR                               P442288 
03615            ' DT  =' WRKRTS-REGIS-DT                               P442288 
03616            ' ACT =' WRKRTS-ACCOUNT-NBR                            P442288 
03617            ' FLT =' WRKRTS-FLEET-NBR                              P442288 
03618            ' SFX =' WRKRTS-SUFFIX-NBR                             P442288 
03619            ' EQP =' WRKRTS-OWNER-EQUIP-NBR                        P442288 
03620          DISPLAY '        RTS-TITLE DBIDS '                       P442288 
03621              WS-INQUIRY-ADABAS-DBID                               P442288 
03622      END-IF.                                                      P442288 
03623 *---------------------------------------------------------------* P442288 
03624                                                                   P442288 
03625      EXEC ADABAS                                                  P442288 
03626          FIND                                                     P442288 
03627          SELECT TTLREGPLTID(1),                                      CL**9
03628                 OWNRID,                                           P442288 
03629                 OWNRTTLNAME(1-2),                                 P442288 
03630                 OWNRTTLNAME(COUNT),                               P442288 
03631                 OWNRST(1-2),                                      P442288 
03632                 OWNRST(COUNT),                                    P442288 
03633                 OWNRCITY,                                         P442288 
03634                 OWNRSTATE,                                        P442288 
03635                 OWNRZIPCODE,                                      P442288 
03636                 OWNRCNTRY,                                        P442288 
03637                 PRIVACYOPTCD,                                     P442288 
03638                 VEHCLASSCD,                                       P442288 
03639                 VIN,                                              P442288 
03640                 VEHMK,                                            P442288 
03641                 VEHMODLYR,                                        P442288 
03642                 AUDITTRAILTRANSID,                                P442288 
03643                 DIESELINDI,                                       P442288 
03644                 VEHTON,                                           P442288 
03645                 VEHUNITNO,                                        P442288 
03646                 VEHEMPTYWT,                                       P442288 
03647                 TTLVEHLOCST(1-2),                                 P442288 
03648                 TTLVEHLOCST(COUNT),                               P442288 
03649                 TTLVEHLOCCITY,                                    P442288 
03650                 TTLVEHLOCSTATE,                                   P442288 
03651                 TTLVEHLOCZPCD,                                    P442288 
03652                 TTLVEHLOCZPCDP4,                                  P442288 
03653                 TTLPROCSCD,                                       P442288 
03654            FROM  RTS-TITLE                                        P442288 
03655            WHERE  DOCNO = :WS-RTS-DOCNO                           P442288 
03656            OPTIONS PASSWORD = :WS-SCRT                            P442288 
03657                    PREFIX=RTS-TITLE-                              P442288 
03658                    DBID     = :WS-INQUIRY-ADABAS-DBID             P442288 
03659       END-EXEC                                                    P442288 
03660                                                                   P442288 
03661 *---------------------------------------------------------------* P442288 
03662      IF TESTING-SHOW-DISPLAYS                                     P442288 
03663          DISPLAY '@ 2111- ADABAS RESPONSE='                       P442288 
03664              RESPONSE-CODE OF RTS-TITLE                           P442288 
03665              ' WITH QUANTITY=' QUANTITY OF RTS-TITLE              P442288 
03666              ' RTS-TRANS-ERROR-FLAG=' RTS-TRANS-ERROR-FLAG        P442288 
03667              ' FOR WRKRTS-DOCNO = ' WS-RTS-DOCNO                  P442288 
03668      END-IF.                                                      P442288 
03669      IF TESTING-SHOW-DISPLAYS                                     P442288 
03670          IF  RESPONSE-CODE OF RTS-TITLE   = ZERO                  P442288 
03671              IF QUANTITY OF RTS-TITLE > ZERO                      P442288 
03672                  DISPLAY '@ 2111- ADABAS FOUND '                  P442288 
03673                      RTS-TITLE-TTLREGPLTNO-1   ' '                   CL**9
03674                      RTS-TITLE-TTLREGEXPYR-1   ' '                   CL**9
03675                      RTS-TITLE-TTLREGEXPMO-1   ' '                   CL**9
03676                      RTS-TITLE-OWNRTTLNAME (1)' '                 P442288 
03677                      RTS-TITLE-VIN         ' '                    P442288 
03678                      RTS-TITLE-VEHMK       ' '                    P442288 
03679                      RTS-TITLE-VEHMODLYR   ' '                    P442288 
03680                      ' FOR KEY ' WS-RTS-DOCNO                     P442288 
03681              ELSE                                                 P442288 
03682                  DISPLAY '@ 2111- QUANTITY = ZERO'                P442288 
03683          ELSE                                                     P442288 
03684              DISPLAY '@ 2111- ADABAS ERROR '                      P442288 
03685                  RESPONSE-CODE OF RTS-TITLE                       P442288 
03686      END-IF.                                                      P442288 
03687 *---------------------------------------------------------------* P442288 
03688                                                                   P442288 
03689      IF  RESPONSE-CODE OF RTS-TITLE   = ZERO                      P442288 
03690          IF QUANTITY   OF RTS-TITLE   > 0                         P442288 
                                                                                
      D        DISPLAY '!TC028.a - RESPONSE-CODE OF RTS-TITLE <'                
      D                     RESPONSE-CODE OF RTS-TITLE '>'                      
      D                   ' QUANTITY   OF RTS-TITLE <'                          
      D                     QUANTITY   OF RTS-TITLE '>'                         
                                                                                
03691              PERFORM 21111-MOVE-TITLE-DATA THRU                   P442288 
03692                      21111-MOVE-TITLE-DATA-EXIT                   P442288 
03693              GO TO 2111-GET-RTS-TITLE-DATA-EXIT                   P442288 
03694          ELSE                                                     P442288 
                                                                                
      D        DISPLAY '!TCXXA - RESPONSE-CODE OF RTS-TITLE <'                  
      D                     RESPONSE-CODE OF RTS-TITLE '>'                      
      D                   ' QUANTITY   OF RTS-TITLE <'                          
      D                     QUANTITY   OF RTS-TITLE '>'                         
                                                                                
03695 *            NEXT SENTENCE                                        P442288 
03696      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TCXXB - RESPONSE-CODE OF RTS-TITLE <'                  
      D                     RESPONSE-CODE OF RTS-TITLE '>'                      
      D                   ' QUANTITY   OF RTS-TITLE <'                          
      D                     QUANTITY   OF RTS-TITLE '>'                         
                                                                                
03697          MOVE 3                TO ERROR-PROCESSING-SW             P442288 
03698          MOVE 'RTS-TITLE  '    TO ERROR-MSG-014-FILE              P442288 
03699          MOVE RESPONSE-CODE OF RTS-TITLE TO ERROR-MSG-014-ERRNUMB P442288 
03700          MOVE ERROR-MSG-014-TEXT   TO ERR-ERROR-MESSAGE-1         P442288 
03701          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
03702          MOVE 20               TO WS-RETURN-CODE                  P442288 
03703          PERFORM Z-Z-TERMINATION.                                 P442288 
03704                                                                   P442288 
03705 *---------------------------------------------------------------* P442288 
03706 *      FALL THROUGH AND DO 2111I-CHECK-INACTIVE-TITLES.         * P442288 
03707 *---------------------------------------------------------------* P442288 
03708                                                                   P442288 
03709 *---------------------------------------------------------------* P442288 
03710 *2111I-CHECK-INACTIVE-TITLES.                                   * P442288 
03711 *---------------------------------------------------------------* P442288 
03712      IF TESTING-SHOW-DISPLAYS                                     P442288 
03713          DISPLAY '@ 2111I-CHECK INACTIVE TITLES'                  P442288 
03714      END-IF.                                                      P442288 
03715 *  ---------------------------------------------------------*     P442288 
03716 *  (1.0) CHECK IF TITLE    (DOCNO) EXIST IN RTS-INACTIVE    *     P442288 
03717 *  ---------------------------------------------------------*     P442288 
03718                                                                   P442288 
03719 *---------------------------------------------------------------* P442288 
03720 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
03721 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
03722 *---------------------------------------------------------------* P442288 
03723                                                                   P442288 
03724      EXEC ADABAS                                                  P442288 
03725          FIND                                                     P442288 
03726          SELECT TTLREGPLTID(1),                                      CL**9
03727                 OWNRID,                                           P442288 
03728                 OWNRTTLNAME(1-2),                                 P442288 
03729                 OWNRTTLNAME(COUNT),                               P442288 
03730                 OWNRST(1-2),                                      P442288 
03731                 OWNRST(COUNT),                                    P442288 
03732                 OWNRCITY,                                         P442288 
03733                 OWNRSTATE,                                        P442288 
03734                 OWNRZIPCODE,                                      P442288 
03735                 OWNRCNTRY,                                        P442288 
03736                 PRIVACYOPTCD,                                     P442288 
03737                 VEHCLASSCD,                                       P442288 
03738                 VIN,                                              P442288 
03739                 VEHMK,                                            P442288 
03740                 VEHMODLYR,                                        P442288 
03741                 AUDITTRAILTRANSID,                                P442288 
03742                 DIESELINDI,                                       P442288 
03743                 VEHTON,                                           P442288 
03744                 VEHUNITNO,                                        P442288 
03745                 VEHEMPTYWT,                                       P442288 
03746                 TTLVEHLOCST(1-2),                                 P442288 
03747                 TTLVEHLOCST(COUNT),                               P442288 
03748                 TTLVEHLOCCITY,                                    P442288 
03749                 TTLVEHLOCSTATE,                                   P442288 
03750                 TTLVEHLOCZPCD,                                    P442288 
03751                 TTLVEHLOCZPCDP4,                                  P442288 
03752                 TTLPROCSCD,                                       P442288 
03753            FROM  RTS-INACT-TITLE                                  P442288 
03754            WHERE  DOCNO = :WS-RTS-DOCNO                           P442288 
03755            OPTIONS PASSWORD = :WS-SCRT                            P442288 
03756                    PREFIX=RTS-INACT-                              P442288 
03757                    DBID     = :WS-INQUIRY-ADABAS-DBID             P442288 
03758       END-EXEC                                                    P442288 
03759                                                                   P442288 
03760 *---------------------------------------------------------------* P442288 
03761      IF TESTING-SHOW-DISPLAYS                                     P442288 
03762          DISPLAY '@ 2111-INACT- ADABAS RESPONSE=<'                P442288 
03763              RESPONSE-CODE OF RTS-INACT-TITLE                     P442288 
03764              '> WITH QUANTITY=<' QUANTITY OF RTS-INACT-TITLE      P442288 
03765              '> RTS-TRANS-ERROR-FLAG=<' RTS-TRANS-ERROR-FLAG      P442288 
03766              '> FOR WRKRTS-DOCNO =<' WS-RTS-DOCNO '>'             P442288 
03767          DISPLAY '        RTS-INACT DBIDS '                       P442288 
03768                  SAVE-DBID-1-0-2 ' ' FILE-NUMBER-0-2              P442288 
03769      END-IF.                                                      P442288 
03770                                                                   P442288 
03771      IF TESTING-SHOW-DISPLAYS                                     P442288 
03772          IF  RESPONSE-CODE OF RTS-INACT-TITLE   = ZERO            P442288 
03773              IF QUANTITY OF RTS-INACT-TITLE > ZERO                P442288 
03774                  DISPLAY '@ 2111- INACT ADABAS FOUND '            P442288 
03775                      RTS-INACT-TTLREGPLTNO-1   ' '                   CL**9
03776                      RTS-INACT-TTLREGEXPYR-1   ' '                   CL**9
03777                      RTS-INACT-OWNRTTLNAME (1) ' '                P442288 
03778                      RTS-INACT-VIN         ' '                    P442288 
03779                      RTS-INACT-VEHMK       ' '                    P442288 
03780                      RTS-INACT-VEHMODLYR   ' '                    P442288 
03781                      ' FOR KEY ' WS-RTS-DOCNO                     P442288 
03782                      ' PREVKEY '                                     CL**9
03783              ELSE                                                 P442288 
03784                  DISPLAY '@ 2111I- QUANTITY = ZERO'               P442288 
03785          ELSE                                                     P442288 
03786              DISPLAY '@ 2111I- ADABAS ERROR '                     P442288 
03787                  RESPONSE-CODE OF RTS-INACT-TITLE                 P442288 
03788      END-IF.                                                      P442288 
03789 *---------------------------------------------------------------* P442288 
03790                                                                   P442288 
03791      IF  RESPONSE-CODE OF RTS-INACT-TITLE   = ZERO                P442288 
03792          IF QUANTITY   OF RTS-INACT-TITLE > 0                     P442288 
                                                                                
      D        DISPLAY '!TC034 - RESPONSE-CODE OF RTS-INACT-TITLE = 0 <'        
      D                     RESPONSE-CODE OF RTS-INACT-TITLE '>'                
      D         ' AND QUANTITY OF RTS-INACT-TITLE > 0 <'                P442288 
      D               QUANTITY OF RTS-INACT-TITLE '>'                   P442288 
                                                                                
03793              PERFORM 21112-MOVE-INACT-DATA THRU                   P442288 
03794                      21112-MOVE-INACT-DATA-EXIT                   P442288 
03795              GO TO 2111-GET-RTS-TITLE-DATA-EXIT                   P442288 
03796          ELSE                                                     P442288 
03797              NEXT SENTENCE                                        P442288 
03798 *---------------------------------------------------------------* P442288 
03799 *            MOVE   1            TO RTS-TRANS-ERROR-FLAG        * P442288 
03800 *---------------------------------------------------------------* P442288 
03801 *        ALLOW THIS TO GO THROUGH FOR NOW.  RTS TRANS WITH FUNDS* P442288 
03802 *        INFO WILL BE SENT TO RTS.  THE RECORD SHOULD HAVE BEEN * P442288 
03803 *        ON THE RTS DATABASE.  IT WILL BE REJECTED BY RTS WITH  * P442288 
03804 *        AN ERROR MESSAGE SENT TO THE REGIONAL OFFICE.  THIS    * P442288 
03805 *        WILL BE HANDLED BY VTRD TITLE CONTROL SECTION (TCS).   * P442288 
03806 *---------------------------------------------------------------* P442288 
03807 *            MOVE ZERO                    TO RTS-TRANS-ERROR-FLA* P442288 
03808 *---------------------------------------------------------------* P442288 
03809 *            SET  RTS-NO-TITLE        INDICATOR FLAG            * P442288 
03810 *            THIS WILL BE RESET IF THE DOCNO IS LOCATED         * P442288 
03811 *            MOVE IRP DATE INTO MVTRANS FIELDS                  * P442288 
03812 *---------------------------------------------------------------* P442288 
03813 *            MOVE 1                       TO RTS-UPDT-ERROR-FLAG* P442288 
03814 *            MOVE SPACES                  TO WS-REGIS-RTS-REGPLT*O   CL**9
03815 *            MOVE WRKRTS-VEHICLE-YEAR       TO WS-VEHMODLYR     * P442288 
03816 *            MOVE WRKRTS-MAKE-OF-VEHICLE    TO WS-VEHMK         * P442288 
03817 *            MOVE WRKRTS-IDENTIFICATION-NBR TO WS-VIN           * P442288 
03818 *---------------------------------------------------------------* P442288 
03819      ELSE                                                         P442288 
03820          MOVE 3                TO ERROR-PROCESSING-SW             P442288 
03821          MOVE 'INACT-TITLE'    TO ERROR-MSG-014-FILE              P442288 
03822          MOVE RESPONSE-CODE OF RTS-INACT-TITLE TO                 P442288 
03823                                             ERROR-MSG-014-ERRNUMB P442288 
03824          MOVE ERROR-MSG-014-TEXT         TO ERR-ERROR-MESSAGE-1   P442288 
03825          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
03826          MOVE 20               TO WS-RETURN-CODE                  P442288 
03827          PERFORM Z-Z-TERMINATION.                                 P442288 
03828 *---------------------------------------------------------------* P442288 
03829 *      FALL THROUGH AND DO 211A-CHECK-ARCHIVES-TITLES.          * P442288 
03830 *---------------------------------------------------------------* P442288 
03831                                                                   P442288 
03832 *---------------------------------------------------------------* P442288 
03833 *2111A-CHECK-ARCHIVES-TITLES.                                   * P442288 
03834 *---------------------------------------------------------------* P442288 
03835      IF TESTING-SHOW-DISPLAYS                                     P442288 
03836          DISPLAY '@ 2111A-CHECK ARCHIVES TITLES'                  P442288 
03837      END-IF.                                                      P442288 
03838 *  ---------------------------------------------------------*     P442288 
03839 *  (1.0) CHECK IF TITLE    (DOCNO) EXIST IN RTS-ARCHIVES    *     P442288 
03840 *  ---------------------------------------------------------*     P442288 
03841                                                                   P442288 
03842 *---------------------------------------------------------------* P442288 
03843 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
03844 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
03845 *---------------------------------------------------------------* P442288 
03846                                                                   P442288 
03847      EXEC ADABAS                                                  P442288 
03848          FIND                                                     P442288 
03849          SELECT TTLREGPLTID(1),                                      CL**9
03850                 OWNRID,                                           P442288 
03851                 OWNRTTLNAME(1-2),                                 P442288 
03852                 OWNRTTLNAME(COUNT),                               P442288 
03853                 OWNRST(1-2),                                      P442288 
03854                 OWNRST(COUNT),                                    P442288 
03855                 OWNRCITY,                                         P442288 
03856                 OWNRSTATE,                                        P442288 
03857                 OWNRZIPCODE,                                      P442288 
03858                 OWNRCNTRY,                                        P442288 
03859                 PRIVACYOPTCD,                                     P442288 
03860                 VEHCLASSCD,                                       P442288 
03861                 VIN,                                              P442288 
03862                 VEHMK,                                            P442288 
03863                 VEHMODLYR,                                        P442288 
03864                 AUDITTRAILTRANSID,                                P442288 
03865                 DIESELINDI,                                       P442288 
03866                 VEHTON,                                           P442288 
03867                 VEHUNITNO,                                        P442288 
03868                 VEHEMPTYWT,                                       P442288 
03869                 TTLVEHLOCST(1-2),                                 P442288 
03870                 TTLVEHLOCST(COUNT),                               P442288 
03871                 TTLVEHLOCCITY,                                    P442288 
03872                 TTLVEHLOCSTATE,                                   P442288 
03873                 TTLVEHLOCZPCD,                                    P442288 
03874                 TTLVEHLOCZPCDP4,                                  P442288 
03875                 TTLPROCSCD,                                       P442288 
03876            FROM  RTS-ARCHIVE-TITLE                                P442288 
03877            WHERE  DOCNO = :WS-RTS-DOCNO                           P442288 
03878            OPTIONS PASSWORD = :WS-SCRT                            P442288 
03879                    PREFIX=RTS-ARCHV-                              P442288 
03880                    DBID     = :WS-INQUIRY-ADABAS-DBID             P442288 
03881       END-EXEC                                                    P442288 
03882                                                                   P442288 
03883 *---------------------------------------------------------------* P442288 
03884      IF TESTING-SHOW-DISPLAYS                                     P442288 
03885          DISPLAY '@ 2111-ARCHV- ADABAS RESPONSE='                 P442288 
03886              RESPONSE-CODE OF RTS-ARCHIVE-TITLE                   P442288 
03887              ' WITH QUANTITY=' QUANTITY OF RTS-ARCHIVE-TITLE      P442288 
03888              ' RTS-TRANS-ERROR-FLAG=' RTS-TRANS-ERROR-FLAG        P442288 
03889              ' FOR WRKRTS-DOCNO = ' WS-RTS-DOCNO                  P442288 
03890          DISPLAY '        RTS-ARCHV DBIDS '                       P442288 
03891                  SAVE-DBID-1-0-2 ' ' FILE-NUMBER-0-2              P442288 
03892      END-IF.                                                      P442288 
03893                                                                   P442288 
03894      IF TESTING-SHOW-DISPLAYS                                     P442288 
03895          IF  RESPONSE-CODE OF RTS-ARCHIVE-TITLE   = ZERO          P442288 
03896              IF QUANTITY OF RTS-ARCHIVE-TITLE > ZERO              P442288 
03897                  DISPLAY '@ 2111I- ARCHV ADABAS FOUND '           P442288 
03898                      RTS-ARCHV-TTLREGPLTNO-1   ' '                   CL**9
03899                      RTS-ARCHV-TTLREGEXPYR-1   ' '                   CL**9
03900                      RTS-ARCHV-OWNRTTLNAME (1) ' '                P442288 
03901                      RTS-ARCHV-VIN         ' '                    P442288 
03902                      RTS-ARCHV-VEHMK       ' '                    P442288 
03903                      RTS-ARCHV-VEHMODLYR   ' '                    P442288 
03904                      ' FOR KEY ' WS-RTS-DOCNO                     P442288 
03905              ELSE                                                 P442288 
03906                  DISPLAY '@ 2111I- QUANTITY = ZERO'               P442288 
03907          ELSE                                                     P442288 
03908              DISPLAY '@ 2111I- ADABAS ERROR '                     P442288 
03909                  RESPONSE-CODE OF RTS-ARCHIVE-TITLE               P442288 
03910      END-IF.                                                      P442288 
03911 *---------------------------------------------------------------* P442288 
03912                                                                   P442288 
03913      IF  RESPONSE-CODE OF RTS-ARCHIVE-TITLE   = ZERO              P442288 
03914          IF QUANTITY  OF RTS-ARCHIVE-TITLE > 0                    P442288 
                                                                                
      D            DISPLAY '!TC042 - '                                          
      D            ' IF RESPONSE-CODE OF RTS-ARCHIVE-TITLE = 0 <'               
      D             RESPONSE-CODE OF RTS-ARCHIVE-TITLE '> '                     
      D            DISPLAY 'AND QUANTITY  OF RTS-ARCHIVE-TITLE > 0 <'           
      D             QUANTITY OF RTS-ARCHIVE-TITLE '>'                           
                                                                                
03915              PERFORM 21113-MOVE-ARCHV-DATA THRU                   P442288 
03916                      21113-MOVE-ARCHV-DATA-EXIT                   P442288 
03917              GO TO 2111-GET-RTS-TITLE-DATA-EXIT                   P442288 
03918          ELSE                                                     P442288 
                                                                                
      D            DISPLAY '!TC042.a - '                                        
      D            'IF RESPONSE-CODE OF RTS-ARCHIVE-TITLE = 0 <'                
      D             RESPONSE-CODE OF RTS-ARCHIVE-TITLE '> '                     
      D            DISPLAY 'AND QUANTITY  OF RTS-ARCHIVE-TITLE <= 0 <'          
      D             QUANTITY OF RTS-ARCHIVE-TITLE '>'                           
                                                                                
03919              MOVE   1            TO RTS-TRANS-ERROR-FLAG          P442288 
03920 *---------------------------------------------------------------* P442288 
03921 *        ALLOW THIS TO GO THROUGH FOR NOW.  RTS TRANS WITH FUNDS* P442288 
03922 *        INFO WILL BE SENT TO RTS.  THE RECORD SHOULD HAVE BEEN * P442288 
03923 *        ON THE RTS DATABASE.  IT WILL BE REJECTED BY RTS WITH  * P442288 
03924 *        AN ERROR MESSAGE SENT TO THE REGIONAL OFFICE.  THIS    * P442288 
03925 *        WILL BE HANDLED BY VTRD TITLE CONTROL SECTION (TCS).   * P442288 
03926 *---------------------------------------------------------------* P442288 
03927              MOVE ZERO                    TO RTS-TRANS-ERROR-FLAG P442288 
03928 *---------------------------------------------------------------* P442288 
03929 *            SET  RTS-NO-TITLE        INDICATOR FLAG            * P442288 
03930 *            THIS WILL BE RESET IF THE DOCNO IS LOCATED         * P442288 
03931 *            MOVE IRP DATE INTO MVTRANS FIELDS                  * P442288 
03932 *---------------------------------------------------------------* P442288 
03933              MOVE 1                       TO RTS-UPDT-ERROR-FLAG  P442288 
03934              MOVE SPACES                  TO WS-REGIS-RTS-REGPLTNO   CL**9
03935              MOVE WRKRTS-VEHICLE-YEAR       TO WS-VEHMODLYR       P442288 
03936              MOVE WRKRTS-MAKE-OF-VEHICLE    TO WS-VEHMK           P442288 
03937              MOVE WRKRTS-IDENTIFICATION-NBR TO WS-VIN             P442288 
03938      ELSE                                                         P442288 
03939          MOVE 3                TO ERROR-PROCESSING-SW             P442288 
03940          MOVE 'INACT-TITLE'    TO ERROR-MSG-014-FILE              P442288 
03941          MOVE RESPONSE-CODE OF RTS-ARCHIVE-TITLE TO               P442288 
03942                                             ERROR-MSG-014-ERRNUMB P442288 
03943          MOVE ERROR-MSG-014-TEXT         TO ERR-ERROR-MESSAGE-1   P442288 
03944          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
03945          MOVE 20               TO WS-RETURN-CODE                  P442288 
03946          PERFORM Z-Z-TERMINATION.                                 P442288 
03947                                                                   P442288 
03948  2111-GET-RTS-TITLE-DATA-EXIT.                                    P442288 
03949      EXIT.                                                        P442288 
03950    EJECT                                                          P442288 
03951                                                                   P442288 
03952  21111-MOVE-TITLE-DATA.                                           P442288 
03953                                                                   P442288 
03954      IF TESTING-SHOW-DISPLAYS                                     P442288 
03955          DISPLAY '@ 21111-MOVE TITLE DATA'                        P442288 
03956      END-IF.                                                      P442288 
03957      MOVE ZERO                        TO RTS-TRANS-ERROR-FLAG.    P442288 
03958      MOVE RTS-TITLE-OWNRTTLNAME (1)   TO WS-OWNRTTLNAME1.         P442288 
03959      MOVE RTS-TITLE-OWNRTTLNAME (2)   TO WS-OWNRTTLNAME2.         P442288 
03960      MOVE RTS-TITLE-OWNRID            TO WS-OWNRID.               P442288 
03961      MOVE RTS-TITLE-OWNRST (1)        TO WS-OWNRST1.              P442288 
03962      MOVE RTS-TITLE-OWNRST (2)        TO WS-OWNRST2.              P442288 
03963      MOVE RTS-TITLE-OWNRCITY          TO WS-OWNRCITY.             P442288 
03964      MOVE RTS-TITLE-OWNRSTATE         TO WS-OWNRSTATE.            P442288 
03965      MOVE RTS-TITLE-OWNRZIPCODE       TO WS-OWNRZIPCODE.          P442288 
03966      MOVE RTS-TITLE-OWNRCNTRY         TO WS-OWNRCNTRY.            P442288 
03967      MOVE RTS-TITLE-PRIVACYOPTCD      TO WS-PRIVACYOPTCD.         P442288 
03968      MOVE RTS-TITLE-VEHCLASSCD        TO WS-VEHCLASSCD.           P442288 
03969      MOVE RTS-TITLE-VIN               TO WS-VIN.                  P442288 
03970      MOVE RTS-TITLE-VEHMK             TO WS-VEHMK.                P442288 
03971      MOVE RTS-TITLE-VEHMODLYR         TO WS-VEHMODLYR.            P442288 
03972      MOVE RTS-TITLE-AUDITTRAILTRANSID TO                          P442288 
03973                                       WS-AUDITTRAILTRANSID.       P442288 
03974      MOVE RTS-TITLE-DIESELINDI        TO WS-DIESELINDI.           P442288 
03975      MOVE RTS-TITLE-VEHTON            TO WS-VEHTON.               P442288 
03976      MOVE RTS-TITLE-VEHUNITNO         TO WS-VEHUNITNO.            P442288 
03977      MOVE RTS-TITLE-VEHEMPTYWT        TO WS-VEHEMPTYWT.           P442288 
03978      MOVE RTS-TITLE-TTLVEHLOCST (1)   TO WS-TTLVEHLOCST1.         P442288 
03979      MOVE RTS-TITLE-TTLVEHLOCST (2)   TO WS-TTLVEHLOCST2.         P442288 
03980      MOVE RTS-TITLE-TTLVEHLOCCITY     TO WS-TTLVEHLOCCITY         P442288 
03981      MOVE RTS-TITLE-TTLVEHLOCSTATE    TO WS-TTLVEHLOCSTATE.       P442288 
03982      MOVE RTS-TITLE-TTLVEHLOCZPCD     TO WS-TTLVEHLOCZPCD.        P442288 
03983      MOVE RTS-TITLE-TTLVEHLOCZPCDP4   TO WS-TTLVEHLOCZPCDP4.      P442288 
03984      MOVE RTS-TITLE-TTLPROCSCD        TO WS-TTLPROCSCD.           P442288 
03985      MOVE ZERO                        TO WS-FILETIERCD.           P442288 
03986                                                                   P442288 
03987 *---------------------------------------------------------------*    CL*11
03988 *    INITIALIZE REGIS FIELDS THAT WILL BE REPLACED BY DATA IN   *    CL*11
03989 *    THE REGIS RECORD - IF ONE EXISTS                           *    CL*11
03990 *---------------------------------------------------------------*    CL*11
03991      MOVE RTS-TITLE-TTLREGPLTNO-1 TO WS-REGIS-RTS-REGPLTNO.          CL*11
03992      MOVE RTS-TITLE-TTLREGEXPYR-1 TO WS-REGIS-RTS-REGEXPYR.          CL*11
03993      MOVE RTS-TITLE-TTLREGEXPMO-1 TO WS-REGIS-RTS-REGEXPMO.          CL*11
03994                                                                      CL*11
03995      IF WS-TTLPROCSCD = 'S' OR                                    P442288 
03996         WS-TTLPROCSCD = 'I'                                       P442288 
                                                                                
      D        DISPLAY '!TC029:'                                                
      D                ' WS-TTLPROCSCD = S OR I  <' WS-TTLPROCSCD '>'           
                                                                                
03997          MOVE 'Y'                TO RTS-TTL-SKIPPED-FLAG          P442288 
03998          MOVE WS-RTS-DOCNO       TO RTS-TTL-SKIPPED-OLD-DOCNO     P442288 
03999          MOVE 1                  TO RTS-UPDT-ERROR-FLAG.          P442288 
04000      IF TESTING-SHOW-DISPLAYS                                     P442288 
04001         DISPLAY ' RTS TITLE OWNER DATA FOR DOCNO ' WS-RTS-DOCNO   P442288 
04002                 ' FOR OWNER ' WS-OWNRID                           P442288 
04003                 ' FOR DOCNO ' WS-RTS-DOCNO ' TIER ' WS-FILETIERCD P442288 
04004                 ' CRNT PLATE ' RTS-TITLE-TTLREGPLTNO-1               CL**9
04005                 ' ' RTS-TITLE-TTLREGEXPYR-1                          CL**9
04006                 ' ' RTS-TITLE-TTLREGEXPMO-1                          CL**9
04007         DISPLAY WS-OWNRTTLNAME1 ' ' WS-OWNRTTLNAME2               P442288 
04008         DISPLAY WS-OWNRST1 ' ' WS-OWNRST2                         P442288 
04009         DISPLAY WS-OWNRCITY ' ' WS-OWNRSTATE ' ' WS-OWNRZIPCODE   P442288 
04010         DISPLAY ' RTS TITLE VEHICLE DATA FOR ' WS-VIN ' ' WS-VEHMKP442288 
04011         DISPLAY WS-TTLVEHLOCST1 ' ' WS-TTLVEHLOCST1               P442288 
04012         DISPLAY WS-TTLVEHLOCCITY ' ' WS-TTLVEHLOCSTATE ' '        P442288 
04013             WS-TTLVEHLOCZPCD                                      P442288 
04014         DISPLAY ' TITLE IN PROCESS CD = "' WS-TTLPROCSCD '"'      P442288 
04015                 ' REG INVALID CD = "' WS-REGINVLDINDI '"'         P442288 
04016                 ' TTL SKIP FL = "' RTS-TTL-SKIPPED-FLAG '"'       P442288 
      D     DISPLAY 'RTS-TTL-2111-INQ-FLAG <' RTS-TTL-2111-INQ-FLAG '>'         
04017      END-IF.                                                      P442288 
04018      IF RTS-TTL-EXC-CANCEL-INQ                                    P442288 
                                                                                
      D        DISPLAY '!TC030 - IF RTS-TTL-EXC-CANCEL-INQ(C) <'                
      D                     RTS-TTL-2111-INQ-FLAG '>'                           
                                                                                
04019          GO TO 21111-MOVE-TITLE-DATA-EXIT.                        P442288 
04020      IF WS-TTLPROCSCD = 'R'                                       P442288 
                                                                                
      D        DISPLAY '!TC031 - WS-TTLPROCSCD = R <'                           
      D                     WS-TTLPROCSCD '>'                                   
                                                                                
04021          MOVE WS-RTS-DOCNO            TO ERROR-MSG-030-DOCNO      P442288 
04022          MOVE '-REJECTED-'            TO ERROR-MSG-030-CODE       P442288 
04023          MOVE 'TRANSMITTED'           TO ERROR-MSG-030-MSG        P442288 
04024          MOVE WS-PLATE-NO-RTS         TO ERR-LICN                 P442288 
04025          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
04026          MOVE ZERO                    TO RTS-UPDT-ERROR-FLAG      P442288 
04027          MOVE ERROR-MSG-030-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
04028          PERFORM Z-V-WRITE-MVUPDT-REPORT                          P442288 
04029          MOVE SPACES                  TO  ERR-LICN                P442288 
04030      ELSE                                                         P442288 
04031      IF WS-TTLPROCSCD = 'I'                                       P442288 
                                                                                
      D        DISPLAY '!TC032 - WS-TTLPROCSCD = I <'                           
      D                     WS-TTLPROCSCD '>'                                   
                                                                                
04032          MOVE WS-RTS-DOCNO            TO ERROR-MSG-030-DOCNO      P442288 
04033          MOVE 'DELETED   '            TO ERROR-MSG-030-CODE       P442288 
04034          MOVE 'BYPASSED'              TO ERROR-MSG-030-MSG        P442288 
04035          MOVE WS-PLATE-NO-RTS         TO ERR-LICN                 P442288 
04036          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
04037          MOVE ERROR-MSG-030-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
04038          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04039          MOVE SPACES                  TO  ERR-LICN                P442288 
04040      ELSE                                                         P442288 
04041      IF WS-TTLPROCSCD = 'S'                                       P442288 
                                                                                
      D        DISPLAY '!TC033 - WS-TTLPROCSCD = S <'                           
      D                     WS-TTLPROCSCD '>'                                   
                                                                                
04042          MOVE WS-RTS-DOCNO            TO ERROR-MSG-030-DOCNO      P442288 
04043          MOVE 'SUPERSEDED'            TO ERROR-MSG-030-CODE       P442288 
04044          MOVE 'BYPASSED'              TO ERROR-MSG-030-MSG        P442288 
04045          MOVE WS-PLATE-NO-RTS         TO ERR-LICN                 P442288 
04046          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
04047          MOVE ERROR-MSG-030-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
04048          PERFORM Z-V-WRITE-MVUPDT-REPORT                          P442288 
04049          MOVE SPACES                  TO  ERR-LICN                P442288 
04050      END-IF.                                                      P442288 
04051  21111-MOVE-TITLE-DATA-EXIT.                                      P442288 
04052      EXIT.                                                        P442288 
04053      EJECT                                                        P442288 
04054                                                                   P442288 
04055                                                                   P442288 
04056  21112-MOVE-INACT-DATA.                                           P442288 
04057                                                                   P442288 
04058      IF TESTING-SHOW-DISPLAYS                                     P442288 
04059          DISPLAY '@ 21112-MOVE INACTIVE DATA'                     P442288 
04060      END-IF.                                                      P442288 
04061      MOVE ZERO                        TO RTS-TRANS-ERROR-FLAG.    P442288 
04062      MOVE RTS-INACT-OWNRTTLNAME (1)   TO WS-OWNRTTLNAME1.         P442288 
04063      MOVE RTS-INACT-OWNRTTLNAME (2)   TO WS-OWNRTTLNAME2.         P442288 
04064      MOVE RTS-INACT-OWNRID            TO WS-OWNRID.               P442288 
04065      MOVE RTS-INACT-OWNRST (1)        TO WS-OWNRST1.              P442288 
04066      MOVE RTS-INACT-OWNRST (2)        TO WS-OWNRST2.              P442288 
04067      MOVE RTS-INACT-OWNRCITY          TO WS-OWNRCITY.             P442288 
04068      MOVE RTS-INACT-OWNRSTATE         TO WS-OWNRSTATE.            P442288 
04069      MOVE RTS-INACT-OWNRZIPCODE       TO WS-OWNRZIPCODE.          P442288 
04070      MOVE RTS-INACT-OWNRCNTRY         TO WS-OWNRCNTRY.            P442288 
04071      MOVE RTS-INACT-PRIVACYOPTCD      TO WS-PRIVACYOPTCD.         P442288 
04072      MOVE RTS-INACT-VEHCLASSCD        TO WS-VEHCLASSCD.           P442288 
04073      MOVE RTS-INACT-VIN               TO WS-VIN.                  P442288 
04074      MOVE RTS-INACT-VEHMK             TO WS-VEHMK.                P442288 
04075      MOVE RTS-INACT-VEHMODLYR         TO WS-VEHMODLYR.            P442288 
04076      MOVE RTS-INACT-AUDITTRAILTRANSID TO                          P442288 
04077                                       WS-AUDITTRAILTRANSID.       P442288 
04078      MOVE RTS-INACT-DIESELINDI        TO WS-DIESELINDI.           P442288 
04079      MOVE RTS-INACT-VEHTON            TO WS-VEHTON.               P442288 
04080      MOVE RTS-INACT-VEHUNITNO         TO WS-VEHUNITNO.            P442288 
04081      MOVE RTS-INACT-VEHEMPTYWT        TO WS-VEHEMPTYWT.           P442288 
04082      MOVE RTS-INACT-TTLVEHLOCST (1)   TO WS-TTLVEHLOCST1.         P442288 
04083      MOVE RTS-INACT-TTLVEHLOCST (2)   TO WS-TTLVEHLOCST2.         P442288 
04084      MOVE RTS-INACT-TTLVEHLOCCITY     TO WS-TTLVEHLOCCITY         P442288 
04085      MOVE RTS-INACT-TTLVEHLOCSTATE    TO WS-TTLVEHLOCSTATE.       P442288 
04086      MOVE RTS-INACT-TTLVEHLOCZPCD     TO WS-TTLVEHLOCZPCD.        P442288 
04087      MOVE RTS-INACT-TTLPROCSCD        TO WS-TTLPROCSCD.           P442288 
04088      MOVE 1                           TO WS-FILETIERCD.           P442288 
04089                                                                   P442288 
04090 *---------------------------------------------------------------*    CL*11
04091 *    INITIALIZE REGIS FIELDS THAT WILL BE REPLACED BY DATA IN   *    CL*11
04092 *    THE REGIS RECORD - IF ONE EXISTS                           *    CL*11
04093 *---------------------------------------------------------------*    CL*11
04094      MOVE RTS-INACT-TTLREGPLTNO-1 TO WS-REGIS-RTS-REGPLTNO.          CL*11
04095      MOVE RTS-INACT-TTLREGEXPYR-1 TO WS-REGIS-RTS-REGEXPYR.          CL*11
04096      MOVE RTS-INACT-TTLREGEXPMO-1 TO WS-REGIS-RTS-REGEXPMO.          CL*11
04097                                                                      CL*11
04098      IF WS-TTLPROCSCD = 'S' OR                                    P442288 
04099         WS-TTLPROCSCD = 'I'                                       P442288 
                                                                                
      D        DISPLAY '!TC035:'                                                
      D                ' WS-TTLPROCSCD = S OR I  <' WS-TTLPROCSCD '>'           
                                                                                
04100          MOVE 'Y'                TO RTS-TTL-SKIPPED-FLAG          P442288 
04101          MOVE WS-RTS-DOCNO       TO RTS-TTL-SKIPPED-OLD-DOCNO     P442288 
04102          MOVE 1                  TO RTS-UPDT-ERROR-FLAG.          P442288 
04103      IF TESTING-SHOW-DISPLAYS                                     P442288 
04104         DISPLAY 'FOR RTS-INACT DOCNO ' WS-RTS-DOCNO               P442288 
04105         DISPLAY ' RTS INACT OWNER DATA FOR OWNER ' WS-OWNRID      P442288 
04106                 ' FOR DOCNO ' WS-RTS-DOCNO ' TIER ' WS-FILETIERCD P442288 
04107                 ' CRNT PLATE ' RTS-INACT-TTLREGPLTNO-1               CL**9
04108                 ' ' RTS-INACT-TTLREGEXPYR-1                          CL**9
04109                 ' ' RTS-INACT-TTLREGEXPMO-1                          CL**9
04110         DISPLAY WS-OWNRTTLNAME1 ' ' WS-OWNRTTLNAME2               P442288 
04111         DISPLAY WS-OWNRST1 ' ' WS-OWNRST2                         P442288 
04112         DISPLAY WS-OWNRCITY ' ' WS-OWNRSTATE ' ' WS-OWNRZIPCODE   P442288 
04113         DISPLAY ' RTS INACT VEHICLE DATA FOR ' WS-VIN ' ' WS-VEHMKP442288 
04114         DISPLAY WS-TTLVEHLOCST1 ' ' WS-TTLVEHLOCST1               P442288 
04115         DISPLAY WS-TTLVEHLOCCITY ' ' WS-TTLVEHLOCSTATE ' '        P442288 
04116             WS-TTLVEHLOCZPCD                                      P442288 
04117         DISPLAY ' TITLE IN PROCESS CD = "' WS-TTLPROCSCD '"'      P442288 
04118                 ' REG INVALID CD = "' WS-REGINVLDINDI '"'         P442288 
04119      END-IF.                                                      P442288 
04120      IF RTS-TTL-EXC-CANCEL-INQ                                    P442288 
                                                                                
      D        DISPLAY '!TC036:'                                                
      D                ' IF RTS-TTL-EXC-CANCEL-INQ <'                           
      D                     RTS-TTL-2111-INQ-FLAG '>'                           
                                                                                
04121          GO TO 21112-MOVE-INACT-DATA-EXIT.                        P442288 
04122      IF WS-TTLPROCSCD = 'I' OR 'S'   AND                          P442288 
04123         RTS-TTL-SKIPPED-FLAG NOT = 'Y'                            P442288 
                                                                                
      D        DISPLAY '!TC037:'                                                
      D                ' WS-TTLPROCSCD = S OR I  <' WS-TTLPROCSCD '>'           
      D        DISPLAY 'AND RTS-TTL-SKIPPED-FLAG NOT = Y'                       
      D              ' <' RTS-TTL-SKIPPED-FLAG '>'                              
                                                                                
04124          MOVE 'Y'                TO RTS-TTL-SKIPPED-FLAG          P442288 
04125          MOVE WS-RTS-DOCNO       TO RTS-TTL-SKIPPED-OLD-DOCNO     P442288 
04126          MOVE 1                  TO RTS-UPDT-ERROR-FLAG.          P442288 
04127      IF WS-TTLPROCSCD = 'R'                                       P442288 
                                                                                
      D        DISPLAY '!TC038: IF WS-TTLPROCSCD = R <'                         
      D                                 WS-TTLPROCSCD '>'                       
                                                                                
04128          MOVE WS-RTS-DOCNO            TO ERROR-MSG-030-DOCNO      P442288 
04129          MOVE 'SUPERSEDED'            TO ERROR-MSG-030-CODE       P442288 
04130          MOVE 'TRANSMITTED'           TO ERROR-MSG-030-MSG        P442288 
04131          MOVE WS-PLATE-NO-RTS         TO ERR-LICN                 P442288 
04132          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
04133          MOVE 3                       TO RTS-UPDT-ERROR-FLAG      P442288 
04134          MOVE ERROR-MSG-030-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
04135          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04136          MOVE SPACES                  TO  ERR-LICN                P442288 
04137      ELSE                                                         P442288 
04138      IF WS-TTLPROCSCD = 'I'                                       P442288 
                                                                                
      D        DISPLAY '!TC039: IF WS-TTLPROCSCD = I <'                         
      D                                WS-TTLPROCSCD '>'                        
                                                                                
04139          MOVE WS-RTS-DOCNO            TO ERROR-MSG-030-DOCNO      P442288 
04140          MOVE 'DELETED   '            TO ERROR-MSG-030-CODE       P442288 
04141          MOVE 'BYPASSED'              TO ERROR-MSG-030-MSG        P442288 
04142          MOVE WS-PLATE-NO-RTS         TO ERR-LICN                 P442288 
04143          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
04144          MOVE 3                       TO RTS-UPDT-ERROR-FLAG      P442288 
04145          MOVE ERROR-MSG-030-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
04146          PERFORM Z-V-WRITE-MVUPDT-REPORT                          P442288 
04147          MOVE SPACES                  TO  ERR-LICN                P442288 
04148      ELSE                                                         P442288 
04149      IF WS-TTLPROCSCD = 'S'                                       P442288 
                                                                                
      D        DISPLAY '!TC040: IF WS-TTLPROCSCD = S <'                         
      D                                WS-TTLPROCSCD '>'                        
                                                                                
04150          MOVE WS-RTS-DOCNO            TO ERROR-MSG-030-DOCNO      P442288 
04151          MOVE 'SUPERSEDED'            TO ERROR-MSG-030-CODE       P442288 
04152          MOVE 'BYPASSED'              TO ERROR-MSG-030-MSG        P442288 
04153          MOVE WS-PLATE-NO-RTS         TO ERR-LICN                 P442288 
04154          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
04155          MOVE 3                       TO RTS-UPDT-ERROR-FLAG      P442288 
04156          MOVE ERROR-MSG-030-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
04157          PERFORM Z-V-WRITE-MVUPDT-REPORT                          P442288 
04158          MOVE SPACES                  TO  ERR-LICN                P442288 
04159      END-IF.                                                      P442288 
04160                                                                   P442288 
                                                                                
      D        DISPLAY '!TC041: 21112-MOVE-INACT-DATA processing'               
      D                                                                         
                                                                                
04161      MOVE ZERO                         TO RTS-TRANS-ERROR-FLAG    P442288 
04162      MOVE 4                            TO ERROR-PROCESSING-SW     P442288 
04163      MOVE WS-RTS-DOCNO                 TO ERROR-MSG-042-DOCNO     P442288 
04164      MOVE WS-VIN                       TO ERROR-MSG-042-VIN       P442288 
04165      MOVE 'INACTIVE'                   TO ERROR-MSG-042-SOURCE    P442288 
04166      MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN                P442288 
04167      MOVE ERROR-MSG-042-TEXT           TO ERR-ERROR-MESSAGE-1     P442288 
04168      PERFORM Z-W-WRITE-MSGOUT-REPORT                              P442288 
04169      MOVE SPACES                       TO ERR-LICN.               P442288 
04170  21112-MOVE-INACT-DATA-EXIT.                                      P442288 
04171      EXIT.                                                        P442288 
04172      EJECT                                                        P442288 
04173                                                                   P442288 
04174 ***************************************************************** P442288 
04175 *  HOPE TO FIND LATEST REGISTRATION (PLATE NUMBER AND REGEXPYR)   P442288 
04176 *  FROM CURSOR:  ACTIVE REGIS DOCNO RECORD (ARDR)                 P442288 
04177 ***************************************************************** P442288 
04178                                                                   P442288 
04179                                                                   P442288 
04180  21113-MOVE-ARCHV-DATA.                                           P442288 
04181                                                                   P442288 
04182      IF TESTING-SHOW-DISPLAYS                                     P442288 
04183          DISPLAY '@ 21113-MOVE ARCHIVES DATA'                     P442288 
04184      END-IF.                                                      P442288 
04185      MOVE ZERO                        TO RTS-TRANS-ERROR-FLAG.    P442288 
04186      MOVE RTS-ARCHV-OWNRTTLNAME (1)   TO WS-OWNRTTLNAME1.         P442288 
04187      MOVE RTS-ARCHV-OWNRTTLNAME (2)   TO WS-OWNRTTLNAME2.         P442288 
04188      MOVE RTS-ARCHV-OWNRID            TO WS-OWNRID.               P442288 
04189      MOVE RTS-ARCHV-OWNRST (1)        TO WS-OWNRST1.              P442288 
04190      MOVE RTS-ARCHV-OWNRST (2)        TO WS-OWNRST2.              P442288 
04191      MOVE RTS-ARCHV-OWNRCITY          TO WS-OWNRCITY.             P442288 
04192      MOVE RTS-ARCHV-OWNRSTATE         TO WS-OWNRSTATE.            P442288 
04193      MOVE RTS-ARCHV-OWNRZIPCODE       TO WS-OWNRZIPCODE.          P442288 
04194      MOVE RTS-ARCHV-OWNRCNTRY         TO WS-OWNRCNTRY.            P442288 
04195      MOVE RTS-ARCHV-PRIVACYOPTCD      TO WS-PRIVACYOPTCD.         P442288 
04196      MOVE RTS-ARCHV-VEHCLASSCD        TO WS-VEHCLASSCD.           P442288 
04197      MOVE RTS-ARCHV-VIN               TO WS-VIN.                  P442288 
04198      MOVE RTS-ARCHV-VEHMK             TO WS-VEHMK.                P442288 
04199      MOVE RTS-ARCHV-VEHMODLYR         TO WS-VEHMODLYR.            P442288 
04200      MOVE RTS-ARCHV-AUDITTRAILTRANSID TO                          P442288 
04201                                       WS-AUDITTRAILTRANSID.       P442288 
04202      MOVE RTS-ARCHV-DIESELINDI        TO WS-DIESELINDI.           P442288 
04203      MOVE RTS-ARCHV-VEHTON            TO WS-VEHTON.               P442288 
04204      MOVE RTS-ARCHV-VEHUNITNO         TO WS-VEHUNITNO.            P442288 
04205      MOVE RTS-ARCHV-VEHEMPTYWT        TO WS-VEHEMPTYWT.           P442288 
04206      MOVE RTS-ARCHV-TTLVEHLOCST (1)   TO WS-TTLVEHLOCST1.         P442288 
04207      MOVE RTS-ARCHV-TTLVEHLOCST (2)   TO WS-TTLVEHLOCST2.         P442288 
04208      MOVE RTS-ARCHV-TTLVEHLOCCITY     TO WS-TTLVEHLOCCITY         P442288 
04209      MOVE RTS-ARCHV-TTLVEHLOCSTATE    TO WS-TTLVEHLOCSTATE.       P442288 
04210      MOVE RTS-ARCHV-TTLVEHLOCZPCD     TO WS-TTLVEHLOCZPCD.        P442288 
04211      MOVE RTS-ARCHV-TTLPROCSCD        TO WS-TTLPROCSCD.           P442288 
04212      MOVE 2                           TO WS-FILETIERCD.           P442288 
04213                                                                   P442288 
04214 *---------------------------------------------------------------*    CL*11
04215 *    INITIALIZE REGIS FIELDS THAT WILL BE REPLACED BY DATA IN   *    CL*11
04216 *    THE REGIS RECORD - IF ONE EXISTS                           *    CL*11
04217 *---------------------------------------------------------------*    CL*11
04218      MOVE RTS-ARCHV-TTLREGPLTNO-1 TO WS-REGIS-RTS-REGPLTNO.          CL*11
04219      MOVE RTS-ARCHV-TTLREGEXPYR-1 TO WS-REGIS-RTS-REGEXPYR.          CL*11
04220      MOVE RTS-ARCHV-TTLREGEXPMO-1 TO WS-REGIS-RTS-REGEXPMO.          CL*11
04221                                                                      CL*11
04222      IF TESTING-SHOW-DISPLAYS                                     P442288 
04223         DISPLAY 'FOR RTS-ARCHV DOCNO ' WS-RTS-DOCNO               P442288 
04224         DISPLAY ' RTS ARCHV OWNER DATA FOR OWNER ' WS-OWNRID      P442288 
04225                 ' FOR DOCNO ' WS-RTS-DOCNO ' TIER ' WS-FILETIERCD P442288 
04226                 ' CRNT PLATE ' RTS-ARCHV-TTLREGPLTNO-1               CL**9
04227                 ' ' RTS-ARCHV-TTLREGEXPYR-1                          CL**9
04228                 ' ' RTS-ARCHV-TTLREGEXPMO-1                          CL**9
04229         DISPLAY WS-OWNRTTLNAME1 ' ' WS-OWNRTTLNAME2               P442288 
04230         DISPLAY WS-OWNRST1 ' ' WS-OWNRST2                         P442288 
04231         DISPLAY WS-OWNRCITY ' ' WS-OWNRSTATE ' ' WS-OWNRZIPCODE   P442288 
04232         DISPLAY ' RTS ARCHV VEHICLE DATA FOR ' WS-VIN ' ' WS-VEHMKP442288 
04233         DISPLAY WS-TTLVEHLOCST1 ' ' WS-TTLVEHLOCST1               P442288 
04234         DISPLAY WS-TTLVEHLOCCITY ' ' WS-TTLVEHLOCSTATE ' '        P442288 
04235             WS-TTLVEHLOCZPCD                                      P442288 
04236         DISPLAY ' TITLE IN PROCESS CD = "' WS-TTLPROCSCD '"'      P442288 
04237                 ' REG INVALID CD = "' WS-REGINVLDINDI '"'         P442288 
04238      END-IF.                                                      P442288 
04239      IF WS-TTLPROCSCD = 'I' OR 'S'   AND                          P442288 
04240         RTS-TTL-SKIPPED-FLAG NOT = 'Y'                            P442288 
                                                                                
      D        DISPLAY '!TC043:'                                                
      D                ' WS-TTLPROCSCD = S OR I  <' WS-TTLPROCSCD '>'           
      D        DISPLAY 'AND RTS-TTL-SKIPPED-FLAG NOT = Y'                       
      D              ' <' RTS-TTL-SKIPPED-FLAG '>'                              
                                                                                
04241          MOVE 'Y'                TO RTS-TTL-SKIPPED-FLAG          P442288 
04242          MOVE WS-RTS-DOCNO       TO RTS-TTL-SKIPPED-OLD-DOCNO     P442288 
04243          MOVE 1                  TO RTS-UPDT-ERROR-FLAG.          P442288 
04244      IF WS-TTLPROCSCD = 'R'                                       P442288 
                                                                                
      D        DISPLAY '!TC044: IF WS-TTLPROCSCD = R <'                         
      D                           WS-TTLPROCSCD '>'                             
                                                                                
04245          MOVE WS-RTS-DOCNO            TO ERROR-MSG-030-DOCNO      P442288 
04246          MOVE 'SUPERSEDED'            TO ERROR-MSG-030-CODE       P442288 
04247          MOVE 'TRANSMITTED'           TO ERROR-MSG-030-MSG        P442288 
04248          MOVE WS-PLATE-NO-RTS         TO ERR-LICN                 P442288 
04249          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
04250          MOVE 3                       TO RTS-UPDT-ERROR-FLAG      P442288 
04251          MOVE ERROR-MSG-030-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
04252          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04253          MOVE SPACES                  TO  ERR-LICN                P442288 
04254      ELSE                                                         P442288 
04255      IF WS-TTLPROCSCD = 'I'                                       P442288 
                                                                                
      D        DISPLAY '!TC045: IF WS-TTLPROCSCD = I <'                         
      D                           WS-TTLPROCSCD '>'                             
                                                                                
04256          MOVE WS-RTS-DOCNO            TO ERROR-MSG-030-DOCNO      P442288 
04257          MOVE 'DELETED   '            TO ERROR-MSG-030-CODE       P442288 
04258          MOVE 'BYPASSED'              TO ERROR-MSG-030-MSG        P442288 
04259          MOVE WS-PLATE-NO-RTS         TO ERR-LICN                 P442288 
04260          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
04261          MOVE 3                       TO RTS-UPDT-ERROR-FLAG      P442288 
04262          MOVE ERROR-MSG-030-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
04263          PERFORM Z-V-WRITE-MVUPDT-REPORT                          P442288 
04264          MOVE SPACES                  TO  ERR-LICN                P442288 
04265      ELSE                                                         P442288 
04266      IF WS-TTLPROCSCD = 'S'                                       P442288 
                                                                                
      D        DISPLAY '!TC046: IF WS-TTLPROCSCD = S <'                         
      D                           WS-TTLPROCSCD '>'                             
                                                                                
04267          MOVE WS-RTS-DOCNO            TO ERROR-MSG-030-DOCNO      P442288 
04268          MOVE 'SUPERSEDED'            TO ERROR-MSG-030-CODE       P442288 
04269          MOVE 'BYPASSED'              TO ERROR-MSG-030-MSG        P442288 
04270          MOVE WS-PLATE-NO-RTS         TO ERR-LICN                 P442288 
04271          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
04272          MOVE 3                       TO RTS-UPDT-ERROR-FLAG      P442288 
04273          MOVE ERROR-MSG-030-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
04274          PERFORM Z-V-WRITE-MVUPDT-REPORT                          P442288 
04275          MOVE SPACES                  TO  ERR-LICN                P442288 
04276      END-IF.                                                      P442288 
04277                                                                   P442288 
                                                                                
      D        DISPLAY '!TC046.a: 21113-MOVE-ARCHV-DATA'                        
                                                                                
04278      MOVE ZERO                         TO RTS-TRANS-ERROR-FLAG    P442288 
04279      MOVE 4                            TO ERROR-PROCESSING-SW     P442288 
04280      MOVE WS-RTS-DOCNO                 TO ERROR-MSG-042-DOCNO     P442288 
04281      MOVE WS-VIN                       TO ERROR-MSG-042-VIN       P442288 
04282      MOVE 'ARCHIVES'                   TO ERROR-MSG-042-SOURCE    P442288 
04283      MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN                P442288 
04284      MOVE ERROR-MSG-042-TEXT           TO ERR-ERROR-MESSAGE-1     P442288 
04285      PERFORM Z-W-WRITE-MSGOUT-REPORT                              P442288 
04286      MOVE SPACES                       TO ERR-LICN.               P442288 
04287  21113-MOVE-ARCHV-DATA-EXIT.                                      P442288 
04288      EXIT.                                                        P442288 
04289      EJECT                                                        P442288 
04290                                                                   P442288 
04291 ***************************************************************** P442288 
04292 *  HOPE TO FIND LATEST REGISTRATION (PLATE NUMBER AND REGEXPYR)   P442288 
04293 *  FROM CURSOR:  ARCHIVE REGIS DOCNO RECORD (RRDR)                P442288 
04294 ***************************************************************** P442288 
04295                                                                   P442288 
04296                                                                   P442288 
04297  2112-GET-RTS-REGIS-DATA.                                         P442288 
04298                                                                   P442288 
04299 *---------------------------------------------------------------* P442288 
04300 * THERE ARE SEVERAL POSSIBILITIES:                              * P442288 
04301 *  1.)  THERE IS ONE (AND ONLY ONE) REGISTRATION RECORD         * P442288 
04302 *       FOR THE CURRENT REGISTRATION YEAR.  WE THEN NEED TO     * P442288 
04303 *       BUILD DATA FOR THE NEXT REGISTRATION YEAR.  THIS WOULD  * P442288 
04304 *       NORMALLY BE THE CASE FOR A REG. RENEWAL SITUATION.      * P442288 
04305 *  2.)  THERE ARE REGISTRATIONS FOR MULTIPLE YEARS - PROBABLY   * P442288 
04306 *       THE CURRENT ONE AND ONE ALREADY THERE FOR THE NEXT YEAR.* P442288 
04307 *       THIS RECORD HAS ALREADY BEEN UPDATED.  THIS IS PROBABLY * P442288 
04308 *       AN ACCIDENTAL RERUN.  MAKE SURE THE VEHICLE INFORMATION * P442288 
04309 *       (VIN, LICNO) IS THE SAME.  IF SO SEND DUP UPDATE MSG.   * P442288 
04310 *       IF NOT IDENTICAL, ISSUE ERROR MESSAGE.                  * P442288 
04311 *  3.)  THERE IS NO REGISTRATION RECORD AT ALL.  THIS WOULD BE  * P442288 
04312 *       THE CASE WITH A NEW VEHICLE BEING TITLED FOR THE FIRST  * P442288 
04313 *       TIME.  IRP REGISTRATION IS IT'S FIRST REGISTRATION.     * P442288 
04314 *  4.)  THERE IS ALREADY A REGISTRATION RECORD FOR THE NEXT     * P442288 
04315 *       REGISTRATION PERIOD.  THIS IS THE CASE FOR COMBINATION  * P442288 
04316 *       PLATES THAT ARE BEING EXCHANGED FOR IRP PLATES, AND FOR * P442288 
04317 *       INDEPENDENT TRUCKERS CHANGING IRP COMPANIES.  THERE ALSO* P442288 
04318 *       SHOULD BE A REGISTRATION RECORD FOR A REFUND TRANSACTION* P442288 
04319 *                                                               * P442288 
04320 * NOTE: THERE MAY BE A REFUND TRANSACTION CREATING A "REGINV"   * P442288 
04321 *       FOR A LICENSE AND A NEW REGISTRATION REPLACING DURING   * P442288 
04322 *       THE SAME RUN (ONCE A WEEK RUN).  THIS WILL PROBABLY GEN-* P442288 
04323 *       ERAGE A 2ND "REGINV" TRANSACTION FOR THAT OLD PLATE.    * P442288 
04324 *---------------------------------------------------------------* P442288 
04325                                                                   P442288 
04326      IF TESTING-SHOW-DISPLAYS                                     P442288 
04327          DISPLAY '@ 2112-GET-RTS-REGIS :'                         P442288 
04328              ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG              P442288 
04329              ' UPDT ERR FL  = ' RTS-UPDT-ERROR-FLAG               P442288 
04330              ' ERROR SW     = ' ERROR-PROCESSING-SW               P442288 
04331          DISPLAY ' INV=' RTS-IRPINV-FLAG                          P442288 
04332                '-- EXP-ACT-TYPE :' WRKRTS-ACT-TYPE                   CL**2
04333      END-IF.                                                      P442288 
04334                                                                   P442288 
04335      IF WS-ACTIVE-TITLE                                           P442288 
                                                                                
      D        DISPLAY '!TC055 - IF WS-ACTIVE-TITLE(0) <'                       
      D                   WS-FILETIERCD '>'                                     
                                                                                
04336          PERFORM 21121-GET-ACTIV-REGIS                            P442288 
04337      ELSE                                                         P442288 
04338      IF WS-INACT-TITLE                                            P442288 
                                                                                
      D        DISPLAY '!TC063 - IF WS-INACT-TITLE(1) <'                        
      D                   WS-FILETIERCD '>'                                     
                                                                                
04339          PERFORM 21122-GET-INACT-REGIS                            P442288 
04340      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC071 - IF WS-ARCHV-TITLE(2) <'                        
      D                   WS-FILETIERCD '>'                                     
                                                                                
04341          PERFORM 21123-GET-ARCHV-REGIS.                           P442288 
04342                                                                   P442288 
04343 *---------------------------------------------------------------* P442288 
04344 *    INSURE THAT THE DOCNO FOR THIS EXPIRATION PERIOD IS        * P442288 
04345 *    NOT ALREADY UPDATED WITH THIS OR SOME OTHER LICENSE NUMBER.* P442288 
04346 *                                                               * P442288 
04347 *    IF IT IS: SEND AN ERROR MESSAGE EXPECIALLY IF THE UNIT     * P442288 
04348 *    (LICNO & VIN) ARE NOT THE SAME AS WE ARE TRYING TO UPDATE. * P442288 
04349 *---------------------------------------------------------------* P442288 
04350                                                                   P442288 
04351      IF RTS-IRPINV-FLAG = SPACES                                  P442288 
                                                                                
      D    DISPLAY '!TC079 - IF RTS-IRPINV-FLAG = SPACES <'                     
      D                         RTS-IRPINV-FLAG '>'                             
                                                                                
04352          PERFORM 21123-VALIDATE-REG-DATA.                         P442288 
04353                                                                   P442288 
04354  2112-GET-RTS-REGIS-DATA-EXIT.                                    P442288 
04355      EXIT.                                                        P442288 
04356    EJECT                                                          P442288 
04357  21121-GET-ACTIV-REGIS.                                           P442288 
04358                                                                   P442288 
04359 *---------------------------------------------------------------* P442288 
04360 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
04361 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
04362 *---------------------------------------------------------------* P442288 
04363                                                                   P442288 
04364      EXEC ADABAS                                                  P442288 
04365            OPEN  ARDR                                             P442288 
04366      END-EXEC                                                     P442288 
04367          .                                                        P442288 
04368                                                                   P442288 
04369      IF TESTING-SHOW-DISPLAYS                                     P442288 
04370          DISPLAY '@ 21121-GET-ACTIV-REGIS FOR ' WS-RTS-DOCNO      P442288 
04371             ' RESP=' RESPONSE-CODE OF RTS-ARDR                    P442288 
04372             ' QUAN=' QUANTITY OF RTS-ARDR                         P442288 
04373      END-IF.                                                      P442288 
04374      IF RESPONSE-CODE OF RTS-ARDR > ZERO                          P442288 
04375          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
04376          MOVE 'OPEN  REGIS'        TO ERROR-MSG-014-FILE          P442288 
04377          MOVE RESPONSE-CODE OF RTS-ARDR TO ERROR-MSG-014-ERRNUMB  P442288 
04378          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
04379          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04380          MOVE 20                   TO WS-RETURN-CODE              P442288 
04381          PERFORM Z-Z-TERMINATION.                                 P442288 
04382                                                                   P442288 
04383      IF QUANTITY OF RTS-ARDR > ZERO                               P442288 
                                                                                
      D        DISPLAY '!TC056 - IF QUANTITY OF RTS-ARDR > ZERO <'              
      D                    QUANTITY OF RTS-ARDR '>'                             
                                                                                
04384          INITIALIZE                ADACODE                        P442288 
04385          MOVE ZERO                 TO WS-FETCH-COUNT              P442288 
04386          MOVE ZERO                 TO SW-REGPLATE                 P442288 
04387          PERFORM 211211-FETCH-LAST-ACTIV THRU                     P442288 
04388                  211211-FETCH-LAST-ACTIV-EXIT                     P442288 
04389              UNTIL ADACODE NOT = ZEROS                            P442288 
04390      END-IF.                                                      P442288 
04391      IF TESTING-SHOW-DISPLAYS                                     P442288 
04392          DISPLAY 'AFT 211211-FETCH ' WS-RTS-DOCNO                 P442288 
04393               ' ' RTS-ARDR-DOCNO                                  P442288 
04394               ' PLATE ' RTS-ARDR-REGPLTNO ' EXPIRES '             P442288 
04395               RTS-ARDR-REGEXPYR ' ' RTS-ARDR-REGEXPMO             P442288 
04396      END-IF.                                                      P442288 
04397                                                                   P442288 
04398      EXEC ADABAS                                                  P442288 
04399            CLOSE ARDR                                             P442288 
04400      END-EXEC                                                     P442288 
04401          .                                                        P442288 
04402                                                                   P442288 
04403  21121-GET-ACTIV-REGIS-EXIT.                                      P442288 
04404      EXIT.                                                        P442288 
04405                                                                   P442288 
04406  211211-FETCH-LAST-ACTIV.                                         P442288 
04407                                                                   P442288 
04408      ADD 1                       TO WS-FETCH-COUNT.               P442288 
04409                                                                   P442288 
04410 *---------------------------------------------------------------* P442288 
04411 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
04412 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
04413 *---------------------------------------------------------------* P442288 
04414                                                                   P442288 
04415      EXEC ADABAS                                                  P442288 
04416           FETCH ARDR                                              P442288 
04417      END-EXEC                                                     P442288 
04418         .                                                         P442288 
04419      IF RESPONSE-CODE OF RTS-ARDR > ZERO AND                      P442288 
04420         RESPONSE-CODE OF RTS-ARDR NOT = 3                         P442288 
04421          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
04422          MOVE 'FETCH REGIS'        TO ERROR-MSG-014-FILE          P442288 
04423          MOVE RESPONSE-CODE OF RTS-ARDR TO ERROR-MSG-014-ERRNUMB  P442288 
04424          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
04425          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04426          MOVE 20                   TO WS-RETURN-CODE              P442288 
04427          PERFORM Z-Z-TERMINATION.                                 P442288 
04428                                                                   P442288 
04429      IF ADACODE EQUAL ZERO                                        P442288 
04430          IF WS-FETCH-COUNT = 1                                    P442288 
                                                                                
      D        DISPLAY '!TC057 - IF ADACODE EQUAL ZERO <'                       
      D                    ADACODE '> AND IF WS-FETCH-COUNT = 1 <'              
      D                    WS-FETCH-COUNT '>'                                   
                                                                                
04431              MOVE RTS-ARDR-REGPLTNO    TO WS-SUPERZA-REGPLTNO     P442288 
04432              MOVE RTS-ARDR-REGEXPYR    TO WS-SUPERZA-REGEXPYR     P442288 
04433              PERFORM 2112111-REFORMAT-ARDR-REGBUF                 P442288 
04434              IF TESTING-SHOW-DISPLAYS                             P442288 
04435                  DISPLAY RTS-ARDR-DOCNO ' FIRST  REG '            P442288 
04436                      RTS-ARDR-REGPLTNO ' ' RTS-ARDR-REGEXPYR      P442288 
04437                      ' ' RTS-ARDR-REGEXPMO                        P442288 
04438              END-IF                                               P442288 
04439          ELSE                                                     P442288 
04440          IF  RTS-ARDR-REGEXPYR > WS-SUPERZA-REGEXPYR   OR         P442288 
04441             (RTS-ARDR-REGEXPYR = WS-SUPERZA-REGEXPYR AND          P442288 
04442              RTS-ARDR-REGEXPMO > WS-REGEXPMO)                     P442288 
                                                                                
      D        DISPLAY '!TC062 - IF ADACODE EQUAL ZERO <'                       
      D                    ADACODE '> AND IF NOT WS-FETCH-COUNT = 1 <'          
      D                    WS-FETCH-COUNT '>'                                   
      D       DISPLAY 'AND IF RTS-ARDR-REGEXPYR <' RTS-ARDR-REGEXPYR '>'        
      D         ' > WS-SUPERZA-REGEXPYR <'                                      
      D             WS-SUPERZA-REGEXPYR '>'                                     
      D         ' AND RTS-ARDR-REGEXPMO <' RTS-ARDR-REGEXPMO '> > '             
      D         ' WS-REGEXPMO <' WS-REGEXPMO '>'                                
                                                                                
04443              MOVE RTS-ARDR-REGPLTNO    TO WS-SUPERZA-REGPLTNO     P442288 
04444              MOVE RTS-ARDR-REGEXPYR    TO WS-SUPERZA-REGEXPYR     P442288 
04445              PERFORM 2112111-REFORMAT-ARDR-REGBUF                 P442288 
04446              IF TESTING-SHOW-DISPLAYS                             P442288 
04447                  DISPLAY RTS-ARDR-DOCNO ' LATEST REG '            P442288 
04448                      RTS-ARDR-REGPLTNO ' ' RTS-ARDR-REGEXPYR      P442288 
04449                      ' ' RTS-ARDR-REGEXPMO                        P442288 
04450              END-IF                                               P442288 
04451      END-IF.                                                      P442288 
04452                                                                   P442288 
04453  211211-FETCH-LAST-ACTIV-EXIT.                                    P442288 
04454      EXIT.                                                        P442288 
04455                                                                   P442288 
04456  2112111-REFORMAT-ARDR-REGBUF.                                    P442288 
04457      IF TESTING-SHOW-DISPLAYS                                     P442288 
04458          DISPLAY '@ 2112111-REFORMAT-ARDR-REGBUF'                 P442288 
04459      END-IF.                                                      P442288 
04460                                                                   P442288 
04461 *    MOVE REGPLTNO OF RTS-ARDR TO REGPLTNO OF C-RTS-REGIS.        P442288 
04462 *---------------------------------------------------------------* P442288 
04463 *    WHAT IS THE OLD REGCLASSCD TO BE USED FOR                  * P442288 
04464 *    WHEN IS OLD DATA TO BE SENT TO RTS AND WHEN IS NEW DATA ?  * P442288 
04465 *---------------------------------------------------------------* P442288 
04466      MOVE   RTS-ARDR-REGCLASSCD        TO WS-REGIS-RTS-REGCLASSCD.   CL**9
04467      MOVE   RTS-ARDR-VEHGROSSWT        TO WS-REGIS-RTS-REG-GWT.      CL*23
04468      MOVE   RTS-ARDR-REGPLTNO          TO WS-REGIS-RTS-REGPLTNO.     CL**9
04469      MOVE   RTS-ARDR-REGEXPYR          TO WS-REGIS-RTS-REGEXPYR.     CL**9
04470      MOVE  RTS-ARDR-REGEXPYR           TO WS-REGEXPYR             P442288 
04471                                           WS-REGIS-RTS-REGEXPYR.     CL**9
04472      MOVE  RTS-ARDR-REGEXPMO           TO WS-REGEXPMO             P442288 
04473                                           WS-REGIS-RTS-REGEXPMO.     CL**9
04474      IF RTS-ARDR-REGEXPMO = 2                                     P442288 
                                                                                
      D        DISPLAY '!TC058 - IF RTS-ARDR-REGEXPMO = 2 <'                    
      D                    RTS-ARDR-REGEXPMO '>'                                
                                                                                
04475          MOVE 28                       TO WS-REGEXPDD             P442288 
04476      ELSE                                                         P442288 
04477      IF RTS-ARDR-REGEXPMO = 9 OR 4 OR 6 OR 11                     P442288 
                                                                                
      D        DISPLAY '!TC059 - IF RTS-ARDR-REGEXPMO = 9 OR 4 OR 6 OR '        
      D            '11 <' RTS-ARDR-REGEXPMO '>'                                 
                                                                                
04478          MOVE 30                       TO WS-REGEXPDD             P442288 
04479      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC060 - IF NOT RTS-ARDR-REGEXPMO 2 9 4 6 11 <'         
      D                   RTS-ARDR-REGEXPMO '>'                                 
                                                                                
04480          MOVE 31                       TO WS-REGEXPDD.            P442288 
04481      MOVE  RTS-ARDR-REGEFFDATE         TO WS-REGEFFDATE.          P442288 
04482      MOVE   RTS-ARDR-REGSTKRNO         TO WS-REGSTKRNO.           P442288 
04483      MOVE   RTS-ARDR-REGSTKRCD         TO WS-REGSTKRCD.           P442288 
04484      MOVE   RTS-ARDR-REGPLTCD          TO WS-REGPLTCD.            P442288 
04485      MOVE   RTS-ARDR-REGPLTCD          TO WS-REGIS-RTS-REGPLATECD.   CL**9
04486      MOVE   RTS-ARDR-PLTBIRTHDATE      TO WS-REGIS-PLTBIRTHDATE.  P442288 
04487 *---------------------------------------------------------------*    CL**9
04488 *    MOVE   RTS-ARDR-PREVPLTNO         TO WS-TITLE-OLD-PRVPLTNO.*    CL**9
04489 *    MOVE   RTS-ARDR-PREVEXPMO         TO WS-REGIS-RTS-REGEXPYR.*    CL**9
04490 *    MOVE   RTS-ARDR-PREVEXPYR         TO WS-REGIS-RTS-REGEXPMO.*    CL**9
04491 *---------------------------------------------------------------*    CL**9
04492      MOVE   RTS-ARDR-PREVPLTNO         TO WS-REGIS-RTS-PRVPLTNO      CL**9
04493      MOVE   RTS-ARDR-PREVEXPMO         TO WS-REGIS-RTS-PRVEXPYR      CL**9
04494      MOVE   RTS-ARDR-PREVEXPYR         TO WS-REGIS-RTS-PRVEXPMO      CL**9
04495      MOVE   RTS-ARDR-RECPNTNAME        TO WS-RECPNTNAME.          P442288 
04496      MOVE   RTS-ARDR-DPSSAFTYSUSPNINDI TO WS-DPSSAFTYSUSPNINDI.   P442288 
04497      MOVE   RTS-ARDR-HVYVEHUSETAXINDI  TO WS-HVYVEHUSETAXINDI.    P442288 
04498      MOVE   RTS-ARDR-REGINVLDINDI      TO WS-REGINVLDINDI.        P442288 
04499      MOVE   RTS-ARDR-REGHOTCKINDI      TO WS-REGHOTCKINDI.        P442288 
04500      MOVE   RTS-ARDR-PLTSSEIZDINDI     TO WS-PLTSSEIZDINDI.       P442288 
04501      MOVE   RTS-ARDR-STKRSEIZDINDI     TO WS-STKRSEIZDINDI.       P442288 
04502      MOVE   RTS-ARDR-REGREFAMT         TO WS-REGREFAMT.           P442288 
04503      MOVE   RTS-ARDR-NOTFYNGCITY       TO WS-NOTFYNGCITY.         P442288 
04504      MOVE   RTS-ARDR-VEHGROSSWT        TO WS-VEHGROSSWT.          P442288 
04505      MOVE   RTS-ARDR-CUSTACTULREGFEE   TO WS-CUSTACTULREGFEE.     P442288 
04506      MOVE   RTS-ARDR-REGPLTOWNRNAME    TO WS-REGPLTOWNRNAME.      P442288 
04507      MOVE   RTS-ARDR-TIRETYPECD        TO WS-TIRETYPECD.          P442288 
04508      IF TESTING-SHOW-DISPLAYS                                     P442288 
04509           DISPLAY '@ ARDR TIRE TYPE : ' WS-TIRETYPECD             P442288 
04510                   ' RTS TIRETYPECD: ' RTS-ARDR-TIRETYPECD.        P442288 
04511      IF WS-TIRETYPECD NUMERIC                                     P442288 
                                                                                
      D        DISPLAY '!TC061 - IF WS-TIRETYPECD NUMERIC <'                    
      D                   WS-TIRETYPECD '>'                                     
                                                                                
04512          MOVE   WS-TIRE-TYPE-CD-P      TO WS-TIRETYPECD.          P442288 
04513      MOVE 'P'                          TO WS-TIRETYPECD.          P442288 
04514      MOVE   1                          TO SW-REGPLATE.            P442288 
04515         EJECT.                                                    P442288 
04516                                                                   P442288 
04517 ***************************************************************** P442288 
04518 *  HOPE TO FIND LATEST REGISTRATION (PLATE NUMBER AND REGEXPYR)   P442288 
04519 *  FROM CURSOR:  INACTIVE REGIS DOCNO RECORD (IRDR)               P442288 
04520 ***************************************************************** P442288 
04521                                                                   P442288 
04522  21122-GET-INACT-REGIS.                                           P442288 
04523                                                                   P442288 
04524 *---------------------------------------------------------------* P442288 
04525 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
04526 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
04527 *---------------------------------------------------------------* P442288 
04528                                                                   P442288 
04529      EXEC ADABAS                                                  P442288 
04530            OPEN  IRDR                                             P442288 
04531      END-EXEC                                                     P442288 
04532          .                                                        P442288 
04533                                                                   P442288 
04534      IF TESTING-SHOW-DISPLAYS                                     P442288 
04535          DISPLAY '@ 21122-GET-INACT-REGIS FOR ' WS-RTS-DOCNO      P442288 
04536             ' RESP=' RESPONSE-CODE OF RTS-ARDR                    P442288 
04537             ' QUAN=' QUANTITY OF RTS-ARDR                         P442288 
04538      END-IF.                                                      P442288 
04539      IF RESPONSE-CODE OF RTS-IRDR > ZERO                          P442288 
04540          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
04541          MOVE 'OPEN INACTREG'      TO ERROR-MSG-014-FILE          P442288 
04542          MOVE RESPONSE-CODE OF RTS-ARDR TO ERROR-MSG-014-ERRNUMB  P442288 
04543          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
04544          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04545          MOVE 20                   TO WS-RETURN-CODE              P442288 
04546          PERFORM Z-Z-TERMINATION.                                 P442288 
04547                                                                   P442288 
04548      IF QUANTITY OF RTS-IRDR > ZERO                               P442288 
                                                                                
      D        DISPLAY '!TC064 - IF QUANTITY OF RTS-IRDR > ZERO <'              
      D                    QUANTITY OF RTS-IRDR '>'                             
                                                                                
04549          INITIALIZE                ADACODE                        P442288 
04550          MOVE ZERO                 TO WS-FETCH-COUNT              P442288 
04551          MOVE ZERO                 TO SW-REGPLATE                 P442288 
04552          PERFORM 211221-FETCH-LAST-INACT THRU                     P442288 
04553                  211221-FETCH-LAST-INACT-EXIT                     P442288 
04554              UNTIL ADACODE NOT = ZEROS                            P442288 
04555      END-IF.                                                      P442288 
04556      IF TESTING-SHOW-DISPLAYS                                     P442288 
04557      DISPLAY 'AFT 211222-FETCH ' WS-RTS-DOCNO                     P442288 
04558           ' ' RTS-IRDR-DOCNO                                      P442288 
04559           ' PLATE ' RTS-IRDR-REGPLTNO ' EXPIRES '                 P442288 
04560           RTS-IRDR-REGEXPYR                                       P442288 
04561      END-IF.                                                      P442288 
04562                                                                   P442288 
04563      EXEC ADABAS                                                  P442288 
04564            CLOSE IRDR                                             P442288 
04565      END-EXEC                                                     P442288 
04566          .                                                        P442288 
04567                                                                   P442288 
04568  21122-GET-INACT-REGIS-EXIT.                                      P442288 
04569      EXIT.                                                        P442288 
04570                                                                   P442288 
04571  211221-FETCH-LAST-INACT.                                         P442288 
04572                                                                   P442288 
04573      ADD 1                       TO WS-FETCH-COUNT.               P442288 
04574                                                                   P442288 
04575 *---------------------------------------------------------------* P442288 
04576 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
04577 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
04578 *---------------------------------------------------------------* P442288 
04579                                                                   P442288 
04580      EXEC ADABAS                                                  P442288 
04581           FETCH IRDR                                              P442288 
04582      END-EXEC                                                     P442288 
04583         .                                                         P442288 
04584      IF RESPONSE-CODE OF RTS-IRDR > ZERO AND                      P442288 
04585         RESPONSE-CODE OF RTS-IRDR NOT = 3                         P442288 
04586          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
04587          MOVE 'FETCH INACTREG'     TO ERROR-MSG-014-FILE          P442288 
04588          MOVE RESPONSE-CODE OF RTS-IRDR TO ERROR-MSG-014-ERRNUMB  P442288 
04589          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
04590          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04591          MOVE 20                   TO WS-RETURN-CODE              P442288 
04592          PERFORM Z-Z-TERMINATION.                                 P442288 
04593                                                                   P442288 
04594      IF ADACODE EQUAL ZERO                                        P442288 
04595          IF WS-FETCH-COUNT = 1                                    P442288 
                                                                                
      D        DISPLAY '!TC065 - IF ADACODE EQUAL ZERO <'                       
      D                    ADACODE '> AND IF WS-FETCH-COUNT = 1 <'              
      D                    WS-FETCH-COUNT '>'                                   
                                                                                
04596              MOVE RTS-IRDR-REGPLTNO    TO WS-SUPERZA-REGPLTNO     P442288 
04597              MOVE RTS-IRDR-REGEXPYR    TO WS-SUPERZA-REGEXPYR     P442288 
04598              PERFORM 2112211-REFORMAT-IRDR-REGBUF                 P442288 
04599              IF TESTING-SHOW-DISPLAYS                             P442288 
04600                  DISPLAY RTS-IRDR-DOCNO ' FIRST  REG '            P442288 
04601                      RTS-IRDR-REGPLTNO ' ' RTS-IRDR-REGEXPYR      P442288 
04602              END-IF                                               P442288 
04603          ELSE                                                     P442288 
04604          IF  RTS-IRDR-REGEXPYR > WS-SUPERZA-REGEXPYR   OR         P442288 
04605             (RTS-IRDR-REGEXPYR = WS-SUPERZA-REGEXPYR AND          P442288 
04606              RTS-IRDR-REGEXPMO > WS-REGEXPMO)                     P442288 
04607              IF TESTING-SHOW-DISPLAYS                             P442288 
04608                  DISPLAY RTS-IRDR-DOCNO ' LATEST REG '            P442288 
04609                      RTS-IRDR-REGPLTNO ' ' RTS-IRDR-REGEXPYR      P442288 
04610              END-IF                                               P442288 
                                                                                
      D        DISPLAY '!TC070 - IF ADACODE EQUAL ZERO <'                       
      D                    ADACODE '> AND IF WS-FETCH-COUNT NOT = 1 <'          
      D                    WS-FETCH-COUNT '>'                                   
      D       DISPLAY 'AND IF RTS-IRDR-REGEXPYR <' RTS-IRDR-REGEXPYR '>'        
      D         ' > WS-SUPERZA-REGEXPYR <'                                      
      D             WS-SUPERZA-REGEXPYR '>'                                     
      D       DISPLAY 'OR IF RTS-IRDR-REGEXPYR <' RTS-IRDR-REGEXPYR '>'         
      D         ' = WS-SUPERZA-REGEXPYR <'                                      
      D             WS-SUPERZA-REGEXPYR '>'                                     
      D         ' AND RTS-IRDR-REGEXPMO <' RTS-IRDR-REGEXPMO '> > '             
      D         ' WS-REGEXPMO <' WS-REGEXPMO '>'                                
                                                                                
04611              MOVE RTS-IRDR-REGPLTNO    TO WS-SUPERZA-REGPLTNO     P442288 
04612              MOVE RTS-IRDR-REGEXPYR    TO WS-SUPERZA-REGEXPYR     P442288 
04613              PERFORM 2112211-REFORMAT-IRDR-REGBUF                 P442288 
04614      END-IF.                                                      P442288 
04615                                                                   P442288 
04616  211221-FETCH-LAST-INACT-EXIT.                                    P442288 
04617      EXIT.                                                        P442288 
04618                                                                   P442288 
04619  2112211-REFORMAT-IRDR-REGBUF.                                    P442288 
04620      IF TESTING-SHOW-DISPLAYS                                     P442288 
04621          DISPLAY '@ 2112211-REFORMAT-IRDR-REGBUF'                 P442288 
04622      END-IF.                                                      P442288 
04623                                                                   P442288 
04624 *    MOVE REGPLTNO OF RTS-IRDR TO REGPLTNO OF C-RTS-REGIS.        P442288 
04625                                                                   P442288 
04626 *---------------------------------------------------------------* P442288 
04627 *    WHAT IS THE OLD REGCLASSCD TO BE USED FOR                  * P442288 
04628 *    WHEN IS OLD DATA TO BE SENT TO RTS AND WHEN IS NEW DATA ?  * P442288 
04629 *---------------------------------------------------------------* P442288 
04630      MOVE   RTS-IRDR-REGCLASSCD        TO WS-REGIS-RTS-REGCLASSCD.   CL**9
04631      MOVE   RTS-IRDR-VEHGROSSWT        TO WS-REGIS-RTS-REG-GWT.      CL*23
04632      MOVE   RTS-IRDR-REGPLTNO          TO WS-REGIS-RTS-REGPLTNO.     CL**9
04633      MOVE   RTS-IRDR-REGEXPYR          TO WS-REGIS-RTS-REGEXPYR.     CL**9
04634      MOVE  RTS-IRDR-REGEXPYR           TO WS-REGEXPYR             P442288 
04635                                           WS-REGIS-RTS-REGEXPYR.     CL**9
04636      MOVE  RTS-IRDR-REGEXPMO           TO WS-REGEXPMO             P442288 
04637                                           WS-REGIS-RTS-REGEXPMO.     CL**9
04638      IF RTS-IRDR-REGEXPMO = 2                                     P442288 
                                                                                
      D        DISPLAY '!TC066 - IF RTS-IRDR-REGEXPMO = 2 <'                    
      D                    RTS-IRDR-REGEXPMO '>'                                
                                                                                
04639          MOVE 28                       TO WS-REGEXPDD             P442288 
04640      ELSE                                                         P442288 
04641      IF RTS-IRDR-REGEXPMO = 9 OR 4 OR 6 OR 11                     P442288 
                                                                                
      D        DISPLAY '!TC067 - IF RTS-IRDR-REGEXPMO = 9 4 6 11 <'             
      D                    RTS-IRDR-REGEXPMO '>'                                
                                                                                
04642          MOVE 30                       TO WS-REGEXPDD             P442288 
04643      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC068 - IF RTS-IRDR-REGEXPMO NOT = 9 4 6 11 <'         
      D                    RTS-IRDR-REGEXPMO '>'                                
                                                                                
04644          MOVE 31                       TO WS-REGEXPDD.            P442288 
04645      MOVE  RTS-IRDR-REGEFFDATE         TO WS-REGEFFDATE.          P442288 
04646      MOVE   RTS-IRDR-REGSTKRNO         TO WS-REGSTKRNO.           P442288 
04647      MOVE   RTS-IRDR-REGSTKRCD         TO WS-REGSTKRCD.           P442288 
04648      MOVE   RTS-IRDR-REGPLTCD          TO WS-REGPLTCD.            P442288 
04649      MOVE   RTS-IRDR-REGPLTCD          TO WS-REGIS-RTS-REGPLATECD.   CL**9
04650      MOVE   RTS-IRDR-PLTBIRTHDATE      TO WS-REGIS-PLTBIRTHDATE.  P442288 
04651 *---------------------------------------------------------------*    CL**9
04652 *    MOVE   RTS-IRDR-PREVPLTNO         TO WS-TITLE-OLD-PRVPLTNO.*    CL**9
04653 *    MOVE   RTS-IRDR-PREVEXPMO         TO WS-REGIS-RTS-REGEXPYR.*    CL**9
04654 *    MOVE   RTS-IRDR-PREVEXPYR         TO WS-REGIS-RTS-REGEXPMO.*    CL**9
04655 *---------------------------------------------------------------*    CL**9
04656      MOVE   RTS-RRDR-PREVPLTNO         TO WS-REGIS-RTS-PRVPLTNO      CL**9
04657      MOVE   RTS-RRDR-PREVEXPMO         TO WS-REGIS-RTS-PRVEXPYR      CL**9
04658      MOVE   RTS-RRDR-PREVEXPYR         TO WS-REGIS-RTS-PRVEXPMO      CL**9
04659      MOVE   RTS-IRDR-RECPNTNAME        TO WS-RECPNTNAME.          P442288 
04660      MOVE   RTS-IRDR-DPSSAFTYSUSPNINDI TO WS-DPSSAFTYSUSPNINDI.   P442288 
04661      MOVE   RTS-IRDR-HVYVEHUSETAXINDI  TO WS-HVYVEHUSETAXINDI.    P442288 
04662      MOVE   RTS-IRDR-REGINVLDINDI      TO WS-REGINVLDINDI.        P442288 
04663      MOVE   RTS-IRDR-REGHOTCKINDI      TO WS-REGHOTCKINDI.        P442288 
04664      MOVE   RTS-IRDR-PLTSSEIZDINDI     TO WS-PLTSSEIZDINDI.       P442288 
04665      MOVE   RTS-IRDR-STKRSEIZDINDI     TO WS-STKRSEIZDINDI.       P442288 
04666      MOVE   RTS-IRDR-REGREFAMT         TO WS-REGREFAMT.           P442288 
04667      MOVE   RTS-IRDR-NOTFYNGCITY       TO WS-NOTFYNGCITY.         P442288 
04668      MOVE   RTS-IRDR-VEHGROSSWT        TO WS-VEHGROSSWT.          P442288 
04669      MOVE   RTS-IRDR-CUSTACTULREGFEE   TO WS-CUSTACTULREGFEE.     P442288 
04670      MOVE   RTS-IRDR-REGPLTOWNRNAME    TO WS-REGPLTOWNRNAME.      P442288 
04671      MOVE   RTS-IRDR-TIRETYPECD        TO WS-TIRETYPECD.          P442288 
04672      IF TESTING-SHOW-DISPLAYS                                     P442288 
04673           DISPLAY '@ IRDR TIRE TYPE : ' WS-TIRETYPECD             P442288 
04674                   ' RTS TIRETYPECD: ' RTS-IRDR-TIRETYPECD.        P442288 
04675      IF WS-TIRETYPECD NUMERIC                                     P442288 
                                                                                
      D        DISPLAY '!TC069 - IF WS-TIRETYPECD NUMERIC <'                    
      D                    WS-TIRETYPECD '>'                                    
                                                                                
04676          MOVE   WS-TIRE-TYPE-CD-P      TO WS-TIRETYPECD.          P442288 
04677      MOVE 'P'                          TO WS-TIRETYPECD.          P442288 
04678      MOVE   1                          TO SW-REGPLATE.            P442288 
04679                                                                   P442288 
04680  21123-GET-ARCHV-REGIS.                                           P442288 
04681                                                                   P442288 
04682 *---------------------------------------------------------------* P442288 
04683 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
04684 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-ARCHV-TITLE.      * P442288 
04685 *---------------------------------------------------------------* P442288 
04686                                                                   P442288 
04687      EXEC ADABAS                                                  P442288 
04688            OPEN  RRDR                                             P442288 
04689      END-EXEC                                                     P442288 
04690          .                                                        P442288 
04691                                                                   P442288 
04692      IF TESTING-SHOW-DISPLAYS                                     P442288 
04693          DISPLAY '@ 21123-GET-ARCHV-REGIS FOR ' WS-RTS-DOCNO      P442288 
04694             ' RESP=' RESPONSE-CODE OF RTS-ARDR                    P442288 
04695             ' QUAN=' QUANTITY OF RTS-ARDR                         P442288 
04696      END-IF.                                                      P442288 
04697      IF RESPONSE-CODE OF RTS-RRDR > ZERO                          P442288 
04698          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
04699          MOVE 'OPEN ARCHVREG'      TO ERROR-MSG-014-FILE          P442288 
04700          MOVE RESPONSE-CODE OF RTS-ARDR TO ERROR-MSG-014-ERRNUMB  P442288 
04701          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
04702          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04703          MOVE 20                   TO WS-RETURN-CODE              P442288 
04704          PERFORM Z-Z-TERMINATION.                                 P442288 
04705                                                                   P442288 
                                                                                
      D    DISPLAY '21123-RESPONSE-CODE OF RTS-RRDR <'                  P442288 
                          RESPONSE-CODE OF RTS-RRDR '>'                 P442288 
      D    DISPLAY '21123-QUANTITY OF RTS-RRDR <'                       P442288 
      D                   QUANTITY OF RTS-RRDR '>'                      P442288 
      D    DISPLAY '21123-WS-RTS-DOCNO <' WS-RTS-DOCNO '>'              P442288 
                                                                                
04706      IF QUANTITY OF RTS-RRDR > ZERO                               P442288 
                                                                                
      D        DISPLAY '!TC072 - IF QUANTITY OF RTS-RRDR > ZERO <'              
      D                             QUANTITY OF RTS-RRDR '>'                    
                                                                                
04707          INITIALIZE                ADACODE                        P442288 
04708          MOVE ZERO                 TO WS-FETCH-COUNT              P442288 
04709          MOVE ZERO                 TO SW-REGPLATE                 P442288 
04710          PERFORM 211231-FETCH-LAST-ARCHV THRU                     P442288 
04711                  211231-FETCH-LAST-ARCHV-EXIT                     P442288 
04712              UNTIL ADACODE NOT = ZEROS                            P442288 
04713      END-IF.                                                      P442288 
04714      IF TESTING-SHOW-DISPLAYS                                     P442288 
04715      DISPLAY 'AFT 211231-FETCH ' WS-RTS-DOCNO                     P442288 
04716           ' ' RTS-RRDR-DOCNO                                      P442288 
04717           ' PLATE ' RTS-RRDR-REGPLTNO ' EXPIRES '                 P442288 
04718           RTS-RRDR-REGEXPYR                                       P442288 
04719      END-IF.                                                      P442288 
04720                                                                   P442288 
04721      EXEC ADABAS                                                  P442288 
04722            CLOSE RRDR                                             P442288 
04723      END-EXEC                                                     P442288 
04724          .                                                        P442288 
04725                                                                   P442288 
04726  21123-GET-ARCHV-REGIS-EXIT.                                      P442288 
04727      EXIT.                                                        P442288 
04728                                                                   P442288 
04729  211231-FETCH-LAST-ARCHV.                                         P442288 
04730                                                                   P442288 
04731      ADD 1                       TO WS-FETCH-COUNT.               P442288 
04732                                                                   P442288 
04733 *---------------------------------------------------------------* P442288 
04734 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
04735 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-ARCHV-TITLE.      * P442288 
04736 *---------------------------------------------------------------* P442288 
04737                                                                   P442288 
04738      EXEC ADABAS                                                  P442288 
04739           FETCH RRDR                                              P442288 
04740      END-EXEC                                                     P442288 
04741         .                                                         P442288 
04742      IF RESPONSE-CODE OF RTS-RRDR > ZERO AND                      P442288 
04743         RESPONSE-CODE OF RTS-RRDR NOT = 3                         P442288 
04744          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
04745          MOVE 'FETCH ARCHVREG'     TO ERROR-MSG-014-FILE          P442288 
04746          MOVE RESPONSE-CODE OF RTS-RRDR TO ERROR-MSG-014-ERRNUMB  P442288 
04747          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
04748          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04749          MOVE 20                   TO WS-RETURN-CODE              P442288 
04750          PERFORM Z-Z-TERMINATION.                                 P442288 
04751                                                                   P442288 
04752      IF ADACODE EQUAL ZERO                                        P442288 
04753          IF WS-FETCH-COUNT = 1                                    P442288 
                                                                                
      D    DISPLAY '!TC073 - IF ADACODE EQUAL ZERO <'                           
      D                         ADACODE '> AND IF WS-FETCH-COUNT = 1 <'         
      D                         WS-FETCH-COUNT '>'                              
                                                                                
04754              MOVE RTS-RRDR-REGPLTNO    TO WS-SUPERZA-REGPLTNO     P442288 
04755              MOVE RTS-RRDR-REGEXPYR    TO WS-SUPERZA-REGEXPYR     P442288 
04756              PERFORM 2112311-REFORMAT-RRDR-REGBUF                 P442288 
04757              IF TESTING-SHOW-DISPLAYS                             P442288 
04758                  DISPLAY RTS-RRDR-DOCNO ' FIRST  REG '            P442288 
04759                      RTS-RRDR-REGPLTNO ' ' RTS-RRDR-REGEXPYR      P442288 
04760              END-IF                                               P442288 
04761          ELSE                                                     P442288 
04762          IF  RTS-RRDR-REGEXPYR > WS-SUPERZA-REGEXPYR   OR         P442288 
04763             (RTS-RRDR-REGEXPYR = WS-SUPERZA-REGEXPYR AND          P442288 
04764              RTS-RRDR-REGEXPMO > WS-REGEXPMO)                     P442288 
                                                                                
      D        DISPLAY '!TC078 - IF ADACODE EQUAL ZERO <'                       
      D                    ADACODE '> AND IF WS-FETCH-COUNT NOT = 1 <'          
      D                    WS-FETCH-COUNT '>'                                   
      D       DISPLAY 'AND IF RTS-RRDR-REGEXPYR <' RTS-RRDR-REGEXPYR '>'        
      D         ' > WS-SUPERZA-REGEXPYR <'                                      
      D             WS-SUPERZA-REGEXPYR '>'                                     
      D       DISPLAY 'OR IF RTS-RRDR-REGEXPYR <' RTS-RRDR-REGEXPYR '>'         
      D         ' = WS-SUPERZA-REGEXPYR <'                                      
      D             WS-SUPERZA-REGEXPYR '>'                                     
      D         ' AND RTS-RRDR-REGEXPMO <' RTS-RRDR-REGEXPMO '> > '             
      D         ' WS-REGEXPMO <' WS-REGEXPMO '>'                                
                                                                                
04765              IF TESTING-SHOW-DISPLAYS                             P442288 
04766                  DISPLAY RTS-RRDR-DOCNO ' LATEST REG '            P442288 
04767                      RTS-RRDR-REGPLTNO ' ' RTS-RRDR-REGEXPYR      P442288 
04768              END-IF                                               P442288 
04769              MOVE RTS-RRDR-REGPLTNO    TO WS-SUPERZA-REGPLTNO     P442288 
04770              MOVE RTS-RRDR-REGEXPYR    TO WS-SUPERZA-REGEXPYR     P442288 
04771              PERFORM 2112311-REFORMAT-RRDR-REGBUF                 P442288 
04772      END-IF.                                                      P442288 
04773                                                                   P442288 
04774  211231-FETCH-LAST-ARCHV-EXIT.                                    P442288 
04775      EXIT.                                                        P442288 
04776                                                                   P442288 
04777  2112311-REFORMAT-RRDR-REGBUF.                                    P442288 
04778      IF TESTING-SHOW-DISPLAYS                                     P442288 
04779          DISPLAY '@ 2112311-REFORMAT-RRDR-REGBUF'                 P442288 
04780      END-IF.                                                      P442288 
04781                                                                   P442288 
04782 *    MOVE REGPLTNO OF RTS-RRDR TO REGPLTNO OF C-RTS-REGIS.        P442288 
04783                                                                   P442288 
04784 *---------------------------------------------------------------* P442288 
04785 *    WHAT IS THE OLD REGCLASSCD TO BE USED FOR                  * P442288 
04786 *    WHEN IS OLD DATA TO BE SENT TO RTS AND WHEN IS NEW DATA ?  * P442288 
04787 *---------------------------------------------------------------* P442288 
04788      MOVE  RTS-RRDR-REGCLASSCD         TO WS-REGIS-RTS-REGCLASSCD.   CL**9
04789      MOVE  RTS-RRDR-REGPLTNO           TO WS-REGIS-RTS-REGPLTNO.     CL**9
04790      MOVE  RTS-RRDR-REGEXPYR           TO WS-REGIS-RTS-REGEXPYR.     CL**9
04791      MOVE  RTS-RRDR-REGEXPYR           TO WS-REGEXPYR             P442288 
04792                                           WS-REGIS-RTS-REGEXPYR.     CL**9
04793      MOVE  RTS-RRDR-REGEXPMO           TO WS-REGEXPMO             P442288 
04794                                           WS-REGIS-RTS-REGEXPMO.     CL**9
04795      IF RTS-RRDR-REGEXPMO = 2                                     P442288 
                                                                                
      D        DISPLAY '!TC074 - IF RTS-RRDR-REGEXPMO = 2 <'                    
      D                    RTS-RRDR-REGEXPMO '>'                                
                                                                                
04796          MOVE 28                       TO WS-REGEXPDD             P442288 
04797      ELSE                                                         P442288 
04798      IF RTS-RRDR-REGEXPMO = 9 OR 4 OR 6 OR 11                     P442288 
                                                                                
      D        DISPLAY '!TC075 - IF RTS-RRDR-REGEXPMO = 9 4 6 11 <'             
      D                    RTS-RRDR-REGEXPMO '>'                                
                                                                                
04799          MOVE 30                       TO WS-REGEXPDD             P442288 
04800      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC076 - IF RTS-RRDR-REGEXPMO NOT = 9 4 6 11 <'         
      D                    RTS-RRDR-REGEXPMO '>'                                
                                                                                
04801          MOVE 31                       TO WS-REGEXPDD.            P442288 
04802      MOVE  RTS-RRDR-REGEFFDATE         TO WS-REGEFFDATE.          P442288 
04803      MOVE  RTS-RRDR-REGSTKRNO          TO WS-REGSTKRNO.           P442288 
04804      MOVE  RTS-RRDR-REGSTKRCD          TO WS-REGSTKRCD.           P442288 
04805      MOVE  RTS-RRDR-REGPLTCD           TO WS-REGPLTCD.            P442288 
04806      MOVE  RTS-RRDR-REGPLTCD           TO WS-REGIS-RTS-REGPLATECD.   CL**9
04807      MOVE  RTS-RRDR-PLTBIRTHDATE       TO WS-REGIS-PLTBIRTHDATE.  P442288 
04808 *---------------------------------------------------------------*    CL**9
04809 *    MOVE  RTS-RRDR-PREVPLTNO          TO WS-TITLE-OLD-PRVPLTNO.*    CL**9
04810 *    MOVE  RTS-RRDR-PREVEXPMO          TO WS-REGIS-RTS-REGEXPYR.*    CL**9
04811 *    MOVE  RTS-RRDR-PREVEXPYR          TO WS-REGIS-RTS-REGEXPMO.*    CL**9
04812 *---------------------------------------------------------------*    CL**9
04813      MOVE  RTS-RRDR-PREVPLTNO          TO WS-REGIS-RTS-PRVPLTNO      CL**9
04814      MOVE  RTS-RRDR-PREVEXPMO          TO WS-REGIS-RTS-PRVEXPYR      CL**9
04815      MOVE  RTS-RRDR-PREVEXPYR          TO WS-REGIS-RTS-PRVEXPMO      CL**9
04816      MOVE  RTS-RRDR-RECPNTNAME         TO WS-RECPNTNAME.          P442288 
04817      MOVE  RTS-RRDR-DPSSAFTYSUSPNINDI  TO WS-DPSSAFTYSUSPNINDI.   P442288 
04818      MOVE  RTS-RRDR-HVYVEHUSETAXINDI   TO WS-HVYVEHUSETAXINDI.    P442288 
04819      MOVE  RTS-RRDR-REGINVLDINDI       TO WS-REGINVLDINDI.        P442288 
04820      MOVE  RTS-RRDR-REGHOTCKINDI       TO WS-REGHOTCKINDI.        P442288 
04821      MOVE  RTS-RRDR-PLTSSEIZDINDI      TO WS-PLTSSEIZDINDI.       P442288 
04822      MOVE  RTS-RRDR-STKRSEIZDINDI      TO WS-STKRSEIZDINDI.       P442288 
04823      MOVE  RTS-RRDR-REGREFAMT          TO WS-REGREFAMT.           P442288 
04824      MOVE  RTS-RRDR-NOTFYNGCITY        TO WS-NOTFYNGCITY.         P442288 
04825      MOVE  RTS-RRDR-VEHGROSSWT         TO WS-VEHGROSSWT.          P442288 
04826      MOVE  RTS-RRDR-CUSTACTULREGFEE    TO WS-CUSTACTULREGFEE.     P442288 
04827      MOVE  RTS-RRDR-REGPLTOWNRNAME     TO WS-REGPLTOWNRNAME.      P442288 
04828      MOVE  RTS-RRDR-TIRETYPECD         TO WS-TIRETYPECD.          P442288 
04829      IF TESTING-SHOW-DISPLAYS                                     P442288 
04830           DISPLAY '@ RRDR TIRE TYPE : ' WS-TIRETYPECD             P442288 
04831                   ' RTS TIRETYPECD: ' RTS-RRDR-TIRETYPECD.        P442288 
04832      IF WS-TIRETYPECD NUMERIC                                     P442288 
                                                                                
      D        DISPLAY '!TC077 - IF WS-TIRETYPECD NUMERIC <'                    
      D                    WS-TIRETYPECD '>'                                    
                                                                                
04833          MOVE   WS-TIRE-TYPE-CD-P      TO WS-TIRETYPECD.          P442288 
04834      MOVE 'P'                          TO WS-TIRETYPECD.          P442288 
04835      MOVE   1                          TO SW-REGPLATE.            P442288 
04836  EJECT                                                            P442288 
04837                                                                   P442288 
04838 *---------------------------------------------------------------* P442288 
04839 *     CHECK FOR PROBLEMS WITH DUPLICATE CURRENT REGISTRATIONS   * P442288 
04840 *---------------------------------------------------------------* P442288 
04841                                                                   P442288 
04842  21123-VALIDATE-REG-DATA.                                         P442288 
04843 *---------------------------------------------------------------* P442288 
04844 *     SET MV-FUNC VALUES FOR CURRENT REGIS VALIDATION           * P442288 
04845 *---------------------------------------------------------------* P442288 
04846                                                                   P442288 
04847 *-----------------------------------------------------------*     P442288 
04848 *     SET REG CLASS CODE & ITMCD    IN RTS_TR_INV_DETAIL RECORD * P442288 
04849 *-----------------------------------------------------------*     P442288 
04850                                                                   P442288 
04851      MOVE SPACES                       TO WS-IRP-5YR-ACCT-FLAG.   P442288 
04852 *---------------------------------------------------------------*    CL**2
04853 *    IF  WS-TAG-POWER-UNIT-SUFFIX                               *    CL**2
04854 *---------------------------------------------------------------*    CL**2
04855      IF WRKRTS-ACT-TYPE = 'A' OR 'H' OR 'P' OR 'F'                   CL**2
04856 *---------------------------------------------------------------*    CL**2
04857 *          IF WS-TAG-FORESTRY-ACCOUNT                           *    CL**2
04858 *---------------------------------------------------------------*    CL**2
04859            IF WRKRTS-ACT-TYPE = 'F'                                  CL**2
                                                                                
      D    DISPLAY '!TC240 - IF WRKRTS-ACT-TYPE <'                              
      D            WRKRTS-ACT-TYPE '> = A H P F AND WRKRTS-ACT-TYPE = F'        
                                                                                
04860                MOVE C-TRANS-ITM-FOREST    TO WS-ITMCD             P442288 
04861                MOVE C-REGCLASSCD-FOREST   TO WS-REGCLASSCD        P442288 
04862            ELSE                                                   P442288 
04863 ***REG-CLASS 006 APPORTION                                      *    CL*24
04864                IF WRKRTS-ACT-TYPE = 'A'                              CL*24
04865                   IF TESTING-SHOW-DISPLAYS                           CL*24
04866                      DISPLAY SPACES                                  CL*24
04867                      DISPLAY                                         CL*24
04868                      '---PARAGRAPH 21123-VALIDATE-REG-DATA---'       CL*24
04869                      DISPLAY 'WRKRTS-DOCNO ' WRKRTS-DOCNO            CL*24
04870                      DISPLAY 'WRKRTS-IDENTIFICATION-NBR '            CL*24
04871                               WRKRTS-IDENTIFICATION-NBR              CL*24
04872                      DISPLAY 'WRKRTS-FLEET-PLT-ITMCD '               CL*24
04873                               WRKRTS-FLEET-PLT-ITMCD                 CL*24
04874                      DISPLAY 'WRKRTS-FLEET-REGCL-CD '                CL*24
04875                               WRKRTS-FLEET-REGCL-CD                  CL*24
04876                   END-IF                                             CL*24
04877                   IF WRKRTS-FLEET-PLT-ITMCD NOT = SPACES             CL*24
                                                                                
      D    DISPLAY '!TC241 - IF WRKRTS-ACT-TYPE <'                              
      D            WRKRTS-ACT-TYPE '> = A H P F AND WRKRTS-ACT-TYPE = A'        
      D    DISPLAY 'AND WRKRTS-FLEET-PLT-ITMCD <'                               
      D                 WRKRTS-FLEET-PLT-ITMCD '> NOT = SPACES'                 
                                                                                
04878                      MOVE WRKRTS-FLEET-PLT-ITMCD TO WS-ITMCD         CL*24
04879                      MOVE WRKRTS-FLEET-REGCL-CD  TO WS-REGCLASSCD    CL*24
04880                   ELSE                                               CL*24
                                                                                
      D    DISPLAY '!TC242 - IF WRKRTS-ACT-TYPE <'                              
      D            WRKRTS-ACT-TYPE '> = A H P F AND WRKRTS-ACT-TYPE = A'        
      D    DISPLAY 'AND WRKRTS-FLEET-PLT-ITMCD <'                               
      D                 WRKRTS-FLEET-PLT-ITMCD '> = SPACES'                     
                                                                                
04881                      MOVE C-TRANS-ITM-APP-TRUCK TO WS-ITMCD          CL*24
04882                      MOVE C-REGCLASSCD-IRP      TO WS-REGCLASSCD     CL*24
04883                   END-IF                                             CL*24
04884                END-IF                                                CL*24
04885       ELSE                                                        P442288 
04886 *---------------------------------------------------------------* P442288 
04887 *        SPECIAL ACCOUNT NUMBERS INDICATE SPECIAL PROCESSING    * P442288 
04888 *        ---------------------------------------------------    * P442288 
04889 *        SEE NOTES ASSOCIATED WITH WS-IRP-ACCOUNT-NO-ANALYSIS   * P442288 
04890 *        AND THE ASSOCIATGED "88" LEVELS AND USAGES             * P442288 
04891 *---------------------------------------------------------------* P442288 
04892 *---------------------------------------------------------------*    CL**2
04893 *         IF WS-TAG-5-YR-TKN-TR-ACCOUNT                         *    CL**2
04894 *---------------------------------------------------------------*    CL**2
04895           IF (WRKRTS-ACT-TYPE = 'T' OR '5')                          CL**2
04896               IF WRKRTS-FLEET-PLT-ITMCD NOT = SPACES                 CL*19
                                                                                
      D    DISPLAY '!TC243 - IF WRKRTS-ACT-TYPE <' WRKRTS-ACT-TYPE              
      D            '> = T OR 5'                                                 
      D    DISPLAY 'AND IF WRKRTS-FLEET-PLT-ITMCD <'                            
      D                 WRKRTS-FLEET-PLT-ITMCD '> NOT = SPACES'                 
                                                                                
04897                   MOVE WRKRTS-FLEET-PLT-ITMCD  TO WS-ITMCD           CL*19
04898                   MOVE WRKRTS-FLEET-REGCL-CD   TO WS-REGCLASSCD      CL*19
04899               ELSE                                                   CL*19
                                                                                
      D    DISPLAY '!TC244 - IF WRKRTS-ACT-TYPE <' WRKRTS-ACT-TYPE              
      D            '> = T OR 5'                                                 
      D    DISPLAY 'AND IF WRKRTS-FLEET-PLT-ITMCD <'                            
      D                 WRKRTS-FLEET-PLT-ITMCD '> = SPACES'                     
                                                                                
04900                   MOVE C-TRANS-ITM-5-TKN-TRLR  TO WS-ITMCD           CL*19
04901                   MOVE C-REGCLASSCD-5TT        TO WS-REGCLASSCD      CL*19
04902                   MOVE 'R'                TO WS-IRP-5YR-ACCT-FLAG    CL*19
04903           ELSE                                                    P442288 
04904 *---------------------------------------------------------------*    CL**2
04905 *         IF WS-TAG-5-YR-APP-ACCOUNT                            *    CL**2
04906 *             MOVE C-TRANS-ITM-5-APP-TRLR TO WS-ITMCD           *    CL**2
04907 *             MOVE C-REGCLASSCD-5AT       TO WS-REGCLASSCD      *    CL**2
04908 *             MOVE 'R'                    TO WS-IRP-5YR-ACCT-FLA*    CL**2
04909 *         ELSE                                                  *    CL**2
04910 *---------------------------------------------------------------*    CL**2
04911 *---------------------------------------------------------------*    CL**2
04912 *         IF WS-TAG-RENTAL-TRLR-ACCOUNT                         *    CL**2
04913 *---------------------------------------------------------------*    CL**2
04914           IF (WRKRTS-ACT-TYPE = 'R')                                 CL**2
                                                                                
      D    DISPLAY '!TC245 - IF WRKRTS-ACT-TYPE <' WRKRTS-ACT-TYPE '>'          
                                                                                
04915 *---------------------------------------------------------------* P442288 
04916 *        CHECK ON THIS: WOULD BOTH USE REGCLASS-CD 01 = RENT TRL* P442288 
04917               MOVE C-REGCLASSCD-5RT       TO WS-REGCLASSCD        P442288 
04918 *---------------------------------------------------------------* P442288 
04919 *        REMOVE TEMP USE OF 5 YR APP TRLR IN PLACE OF 5 RENT TRL* P442288 
04920 *---------------------------------------------------------------* P442288 
04921               MOVE 'R'                    TO WS-IRP-5YR-ACCT-FLAG P442288 
04922               IF WRKRTS-GROSS-WEIGHT < 4000                       P442288 
                                                                                
      D    DISPLAY '!TC246 - IF WRKRTS-GROSS-WEIGHT <'                          
      D            WRKRTS-GROSS-WEIGHT '>  < 4000'                              
                                                                                
04923                   MOVE C-TRANS-ITM-5-RNT-TRLR   TO WS-ITMCD       P442288 
04924                   MOVE C-REGCLASSCD-5RT         TO WS-REGCLASSCD  P442288 
04925               ELSE                                                P442288 
                                                                                
      D    DISPLAY '!TC247 - IF WRKRTS-GROSS-WEIGHT <'                          
      D            WRKRTS-GROSS-WEIGHT '>  >= 4000'                             
                                                                                
04926                   MOVE C-TRANS-ITM-5-RTPHVY-TRLR TO WS-ITMCD      P442288 
04927                   MOVE C-REGCLASSCD-5RTPHVY     TO WS-REGCLASSCD  P442288 
04928               END-IF                                              P442288 
04929           ELSE                                                       CL*17
04930               IF WRKRTS-ACT-TYPE = 'E' OR                            CL*17
04931                  WRKRTS-ACT-TYPE = 'U'                               CL*17
                                                                                
      D    DISPLAY '!TC248 - IF WRKRTS-ACT-TYPE <'                              
      D            WRKRTS-ACT-TYPE '> = E U'                                    
                                                                                
04932                   MOVE WRKRTS-FLEET-PLT-ITMCD TO WS-ITMCD            CL*17
04933                   MOVE WRKRTS-FLEET-REGCL-CD  TO WS-REGCLASSCD       CL*17
04934               ELSE                                                P442288 
                                                                                
      D    DISPLAY '!TC249 - IF WRKRTS-ACT-TYPE <'                              
      D            WRKRTS-ACT-TYPE '> NOT = E U'                                
                                                                                
04935                   PERFORM 211234-UNDET-ITEMCD                     P442288 
04936                   MOVE C-TRANS-ITM-APP-TRLR   TO WS-ITMCD         P442288 
04937                   MOVE C-REGCLASSCD-IRP       TO WS-REGCLASSCD.   P442288 
04938                                                                   P442288 
04939                                                                   P442288 
04940      MOVE WS-CRNT-DATE-YYYY        TO WS-CRNT-REG-UPD-DT-YYYY.    P442288 
04941      MOVE WS-CRNT-DATE-MM          TO WS-CRNT-REG-UPD-DT-MM.      P442288 
04942                                                                   P442288 
04943      MOVE WS-REGIS-RTS-REG-EXP-DT  TO WS-MIN-BEGIN-REG-DT.           CL**9
04944      IF WS-MIN-BEGIN-REG-DT-MM > 1                                P442288 
                                                                                
      D    DISPLAY '!TC250 - IF WS-MIN-BEGIN-REG-DT-MM <'                       
      D                         WS-MIN-BEGIN-REG-DT-MM '>  > 1'                 
                                                                                
04945          SUBTRACT 1                FROM WS-MIN-BEGIN-REG-DT-MM    P442288 
04946      ELSE                                                         P442288 
                                                                                
      D    DISPLAY '!TC251 - IF WS-MIN-BEGIN-REG-DT-MM <'                       
      D                         WS-MIN-BEGIN-REG-DT-MM '>  <= 1'                
                                                                                
04947          SUBTRACT 1                FROM WS-MIN-BEGIN-REG-DT-YYYY  P442288 
04948          MOVE 12                   TO WS-MIN-BEGIN-REG-DT-MM.     P442288 
04949                                                                   P442288 
04950      MOVE WS-REGIS-RTS-REG-EXP-DT  TO WS-PREV-YEAR-REG-EXP-DT.       CL**9
04951      IF WS-PREV-YEAR-REG-EXP-DT-MM > 5                            P442288 
                                                                                
      D    DISPLAY '!TC252 - IF WS-PREV-YEAR-REG-EXP-DT-MM <'                   
      D                     WS-PREV-YEAR-REG-EXP-DT-MM '>  > 5'                 
                                                                                
04952          SUBTRACT 5              FROM WS-PREV-YEAR-REG-EXP-DT-MM  P442288 
04953      ELSE                                                         P442288 
                                                                                
      D    DISPLAY '!TC253 - IF WS-PREV-YEAR-REG-EXP-DT-MM <'                   
      D                     WS-PREV-YEAR-REG-EXP-DT-MM '>  <= 5'                
                                                                                
04954          SUBTRACT 1              FROM WS-PREV-YEAR-REG-EXP-DT-YYYYP442288 
04955          ADD 7                   TO   WS-PREV-YEAR-REG-EXP-DT-MM. P442288 
04956                                                                   P442288 
04957      MOVE WRKRTS-REG-EXP-DT-YYYY           TO WS-SUPERZA-REGEXPYR P442288 
04958      MOVE WS-PLATE-NO-RTS                  TO WS-SUPERZA-REGPLTNO.P442288 
04959                                                                   P442288 
04960 *---------------------------------------------------------------* P442288 
04961 *    THESE CHECKS ARE PERFORMED LOOKING AT THE TITLE AND LATEST * P442288 
04962 *    REGISTRATION INFORMATION RETURNED FROM RTS FILES USING THE * P442288 
04963 *    DOCNO AS THE KEY                                           * P442288 
04964 *---------------------------------------------------------------* P442288 
04965                                                                   P442288 
04966      IF TESTING-SHOW-DISPLAYS                                     P442288 
04967          DISPLAY '@ 21123-VALIDATE-REG-DATA - OLD REG='           P442288 
04968            WS-REGIS-RTS-REGPLTNO ' ' WS-REGIS-RTS-REG-EXP-DT ' '     CL**9
04969           'TTL=' WS-REGIS-RTS-REGPLTNO ' ' WS-REGIS-RTS-REG-EXP-DT   CL**9
04970           ' BIRTH DT=' WS-REGIS-PLTBIRTHDATE                      P442288 
04971          DISPLAY ' IRPRTS DATA = ' WS-PLATE-NO-RTS                P442288 
04972            WRKRTS-REG-EXP-DT ' '                                  P442288 
04973            ' FOR IRP LICN = ' WRKRTS-CURRENT-TEXAS-LIC-NBR        P442288 
04974      END-IF.                                                      P442288 
04975                                                                   P442288 
04976 *---------------------------------------------------------------* P442288 
04977 *    CHECKING TO SEE IF REGIS RECORD AND TITLE RECORD BOTH      * P442288 
04978 *    CONTAIN THE SAME REGIS EXP DT...STRANGE RTS ERROR IF NOT   * P442288 
04979 *---------------------------------------------------------------* P442288 
04980      IF REGPLATE-FOUND AND                                        P442288 
04981         (WS-REGIS-RTS-REGEXPYR NOT = WS-REGIS-RTS-REGEXPYR OR        CL**9
04982          WS-REGIS-RTS-REGEXPMO NOT = WS-REGIS-RTS-REGEXPMO)          CL**9
                                                                                
      D    DISPLAY '!TC254 - IF REGPLATE-FOUND(1) <'                            
      D                     SW-REGPLATE '>'                                     
      D        'AND (WS-REGIS-RTS-REGEXPYR <' WS-REGIS-RTS-REGEXPYR             
      D        '> NOT = WS-REGIS-RTS-REGEXPYR <'                                
      D                 WS-REGIS-RTS-REGEXPYR '>'                               
      D    DISPLAY 'OR WS-REGIS-RTS-REGEXPMO <' WS-REGIS-RTS-REGEXPMO           
      D        '> NOT = WS-REGIS-RTS-REGEXPMO <' WS-REGIS-RTS-REGEXPMO          
      D        '>'                                                              
                                                                                
04983          MOVE 4                            TO ERROR-PROCESSING-SW P442288 
04984          MOVE WS-REGIS-RTS-REG-EXP-DT      TO ERROR-MSG-015-EXP-T    CL**9
04985          MOVE WS-REGIS-RTS-REG-EXP-DT      TO ERROR-MSG-015-EXP-R    CL**9
04986          MOVE WS-RTS-DOCNO                 TO ERROR-MSG-015-DOCNO P442288 
04987          MOVE WS-RTS-DOCNO                 TO ERROR-MSG-015-DOCNO P442288 
04988          MOVE WS-PLATE-NO-RTS              TO ERR-LICN            P442288 
04989          MOVE ERROR-MSG-015-TEXT           TO ERR-ERROR-MESSAGE-1 P442288 
04990          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
04991          MOVE SPACES                       TO  ERR-LICN.          P442288 
04992                                                                   P442288 
04993 *---------------------------------------------------------------* P442288 
04994 *    NOTE: REG INVALID INDI INDICATES THAT THE REGISTRATION HAS * P442288 
04995 *    BEEN CANCELLED.  THIS IS OFTEN DONE TO RE-REGISTER UNDER   * P442288 
04996 *    THE IRP SYSTEM.  THE PREVIOUS UNEXPENDED REGISTRATION FEES * P442288 
04997 *    ARE REFUNDED.                                              * P442288 
04998 *---------------------------------------------------------------* P442288 
04999                                                                   P442288 
05000      IF TESTING-SHOW-DISPLAYS                                        CL**7
05001          DISPLAY '@ 21123-VALIDATE-REG -'                            CL**7
05002                  '- CHECK REDUNAANT/CANCEL IFNOT '                   CL**7
05003            WS-REGINVLDINDI ' > ZERO; OR L= '                         CL**7
05004            WRKRTS-TYPE-OF-CHANGE-FLAG                                CL**7
05005          DISPLAY 'OLD ' WS-REGIS-RTS-REGPLTNO                        CL**9
05006              ' ' WS-REGIS-RTS-REG-EXP-DT                             CL**9
05007              ' ' WS-REGIS-RTS-REGCLASSCD                             CL**9
05008              ' ' WS-REGIS-RTS-REGPLATECD                             CL**9
05009          DISPLAY 'WRK ' WS-PLATE-NO-RTS                              CL**7
05010              ' ' WRKRTS-REG-EXP-DT ' ' WS-REGCLASSCD ' ' WS-ITMCD    CL**7
05011          DISPLAY ' =  MUST ALL BE SAME '                             CL**7
05012                  'FOR REDUNDANT MSG AND MVUPDT SKIP'.                CL**7
05013                                                                      CL**7
05014      IF WS-REGINVLDINDI        NOT = ZEROS    OR                  P442288 
05015         WRKRTS-TYPE-OF-CHANGE-FLAG = 'L'                          P442288 
05016          NEXT SENTENCE                                            P442288 
05017      ELSE                                                         P442288 
05018      IF (WS-REGIS-RTS-REG-EXP-DT = WRKRTS-REG-EXP-DT                 CL**9
05019      OR  WS-REGIS-RTS-REG-EXP-DT > WRKRTS-REG-EXP-DT) AND            CL*10
05020         WS-REGIS-RTS-REGPLTNO   = WS-PLATE-NO-RTS   AND              CL**9
05021         WS-REGIS-RTS-REGCLASSCD = WS-REGCLASSCD     AND              CL**9
05022         WS-REGIS-RTS-REGPLATECD = WS-ITMCD          AND              CL*23
05023         (WS-REGIS-RTS-REG-GWT    = WRKRTS-GROSS-WEIGHT AND           CL*23
05024          WS-REGIS-RTS-REG-GWT > ZERO)                                CL*23
05025          IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'M'                         CL**7
05026             OR WRKRTS-TYPE-OF-CHANGE-FLAG = 'W'                      CL**7
05027             OR WRKRTS-TYPE-OF-CHANGE-FLAG = 'L'                   P442288 
05028              NEXT SENTENCE                                        P442288 
05029          ELSE                                                     P442288 
                                                                                
      D    DISPLAY '!TC255 - IF WRKRTS-TYPE-OF-CHANGE-FLAG <'                   
      D                    WRKRTS-TYPE-OF-CHANGE-FLAG '> NOT = M W L'           
                                                                                
05030              PERFORM 211231-REDUNDANT-UPDATE                      P442288 
05031          END-IF                                                   P442288 
05032      ELSE                                                         P442288 
05033      IF (WS-REGIS-RTS-REG-EXP-DT = WRKRTS-REG-EXP-DT AND             CL**9
05034          WS-REGIS-RTS-REGPLTNO   = WS-PLATE-NO-RTS)        AND       CL**9
05035          WS-REGIS-RTS-REGCLASSCD NOT = WS-REGCLASSCD        OR       CL**9
05036          WS-REGIS-RTS-REGPLATECD NOT = WS-ITMCD                      CL**9
                                                                                
      D    DISPLAY '!TC256 - IF (WS-REGIS-RTS-REG-EXP-DT <'                     
      D                          WS-REGIS-RTS-REG-EXP-DT '> = '                 
      D         'WRKRTS-REG-EXP-DT <' WRKRTS-REG-EXP-DT '>'                     
                                                                                
05037          PERFORM 211233-CHECK-TO-CANCEL THRU                      P442288 
05038                  211233-CHECK-TO-CANCEL-EXIT                      P442288 
05039      ELSE                                                         P442288 
05040 *---------------------------------------------------------------*    CL**9
05041 *     CHECK TO BYPASS MVUPDT IF THE PLATE                       *    CL**9
05042 *     HAS ALREADY BEEN REPLACED BY ANOTHER - LATE UPDATE        *    CL**9
05043 *---------------------------------------------------------------*    CL**9
05044      IF WS-REGIS-RTS-REGPLTNO   = WS-PLATE-NO-RTS                    CL**9
05045          NEXT SENTENCE                                            P442288 
05046      ELSE                                                         P442288 
05047 *---------------------------------------------------------------* P442288 
05048 *    IF WS-REGIS-RTS-REG-EXP-DT NOT < WRKRTS-REG-EXP-DT AND     *    CL**9
05049 *       WRKRTS-TYPE-OF-CHANGE-FLAG NOT = 'L'       AND          * P442288 
05050 *       WS-IRP-5YR-ACCT-FLAG = 'R'                              * P442288 
05051 *        PERFORM 211230-5YR-REGIS-CHANGED                       * P442288 
05052 *    ELSE                                                       * P442288 
05053 *---------------------------------------------------------------* P442288 
                                                                                
      D    DISPLAY '!TC259 - IF WS-REGIS-RTS-REGPLTNO <'                        
      D                         WS-REGIS-RTS-REGPLTNO '> NOT = '                
      D         'WS-PLATE-NO-RTS <' WS-PLATE-NO-RTS '>'                         
                                                                                
05054          PERFORM 211233-CHECK-TO-CANCEL THRU                      P442288 
05055                  211233-CHECK-TO-CANCEL-EXIT.                     P442288 
05056                                                                   P442288 
                                                                                
      D    DISPLAY '!TC260 - PERFORM 211232-CHECK-DUPLICATE-REGIS'              
                                                                                
05057      PERFORM 211232-CHECK-DUPLICATE-REGIS.                        P442288 
05058                                                                   P442288 
05059  211230-5YR-REGIS-CHANGED.                                        P442288 
05060                                                                   P442288 
05061 *   ALLOW THIS TO GO THROUGH.  DO NOT SEND RTS-MV-FUNC-TRANS BUT* P442288 
05062 *   DO SEND RTS-FUNDS DATA                                      * P442288 
05063 *   SET  RTS-INVALID-UPDATE   INDICATOR FLAG                    * P442288 
05064      MOVE ZERO                         TO RTS-TRANS-ERROR-FLAG.   P442288 
05065 *---------------------------------------------------------------* P442288 
05066 *    MOVE 5                            TO RTS-UPDT-ERROR-FLAG   * P442288 
05067 *---------------------------------------------------------------* P442288 
05068      MOVE 4                            TO ERROR-PROCESSING-SW.    P442288 
05069      MOVE WS-RTS-DOCNO                 TO ERROR-MSG-027-DOCNO.    P442288 
05070      MOVE WS-REGIS-RTS-REGPLTNO        TO ERROR-MSG-027-PLT.         CL**9
05071      MOVE WS-REGIS-RTS-REG-EXP-DT      TO ERROR-MSG-027-EXP.         CL**9
05072      MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN.               P442288 
05073      IF WS-IRP-5YR-ACCT-FLAG = 'R'                                P442288 
05074          MOVE '5YR '                   TO ERROR-MSG-027-TYPE      P442288 
05075      ELSE                                                         P442288 
05076          MOVE 'RTS '                   TO ERROR-MSG-027-TYPE.     P442288 
05077      MOVE ERROR-MSG-027-TEXT           TO ERR-ERROR-MESSAGE-1.    P442288 
05078      PERFORM Z-4-CONTROL-EDIT-ERROR.                              P442288 
05079      MOVE SPACES                       TO  ERR-LICN.              P442288 
05080      IF TESTING-SHOW-DISPLAYS                                     P442288 
05081          DISPLAY '5YR TRAILER DOCNO CONTAINS OTHER DATA'          P442288 
05082      END-IF.                                                      P442288 
05083                                                                   P442288 
05084  211231-REDUNDANT-UPDATE.                                         P442288 
05085                                                                   P442288 
                                                                                
      D    DISPLAY '!TC255 - IF WRKRTS-TYPE-OF-CHANGE-FLAG <'                   
05086      MOVE ZERO                         TO RTS-TRANS-ERROR-FLAG.   P442288 
05087 *   ALLOW THIS TO GO THROUGH.  DO NOT SEND RTS-MV-FUNC-TRANS BUT* P442288 
05088 *   DO SEND RTS-FUNDS DATA                                      * P442288 
05089 *   SET  RTS-INVALID-UPDATE   INDICATOR FLAG                    * P442288 
05090      MOVE 5                            TO RTS-UPDT-ERROR-FLAG     P442288 
05091      MOVE 1                            TO ERROR-PROCESSING-SW.    P442288 
05092      MOVE WS-RTS-DOCNO                 TO ERROR-MSG-017-DOCNO.    P442288 
05093      MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN.               P442288 
05094      MOVE ERROR-MSG-017-TEXT           TO ERR-ERROR-MESSAGE-1.    P442288 
05095      PERFORM Z-W-WRITE-MSGOUT-REPORT.                             P442288 
05096      MOVE SPACES                       TO  ERR-LICN.              P442288 
05097      IF TESTING-SHOW-DISPLAYS                                     P442288 
05098          DISPLAY 'REDUNDANT UPDATE OF EXISTING DATA'              P442288 
05099      END-IF.                                                      P442288 
05100                                                                   P442288 
05101  211232-CHECK-DUPLICATE-REGIS.                                    P442288 
05102                                                                   P442288 
05103 *---------------------------------------------------------------* P442288 
05104 *  MATCH WS-REG-PLTNO-REGEXPYR AS  THE SUPERZA ON RTS-REGIS     * P442288 
05105 *---------------------------------------------------------------* P442288 
05106                                                                   P442288 
05107 *---------------------------------------------------------------* P442288 
05108 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
05109 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
05110 *---------------------------------------------------------------* P442288 
05111                                                                   P442288 
05112      EXEC ADABAS                                                  P442288 
05113            OPEN  DREG                                             P442288 
05114      END-EXEC                                                     P442288 
05115          .                                                        P442288 
05116                                                                   P442288 
05117      IF TESTING-SHOW-DISPLAYS                                     P442288 
05118          DISPLAY '@ 211232-CHECK-DUP-REGIS FOR '                  P442288 
05119            WS-REG-PLTNO-REGEXPYR                                  P442288 
05120           ' RESP=' RESPONSE-CODE OF RTS-DREG                      P442288 
05121           ' QUAN=' QUANTITY OF RTS-DREG                           P442288 
05122      END-IF.                                                      P442288 
05123      IF RESPONSE-CODE OF RTS-DREG > ZERO                          P442288 
05124          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
05125          MOVE 'OPEN  REGIS'        TO ERROR-MSG-014-FILE          P442288 
05126          MOVE RESPONSE-CODE OF RTS-DREG TO ERROR-MSG-014-ERRNUMB  P442288 
05127          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
05128          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
05129          MOVE 20                   TO WS-RETURN-CODE              P442288 
05130          PERFORM Z-Z-TERMINATION.                                 P442288 
05131                                                                   P442288 
05132      IF QUANTITY OF RTS-DREG > ZERO                               P442288 
                                                                                
      D    DISPLAY '!TC261 - IF QUANTITY OF RTS-DREG <'                         
      D                         QUANTITY OF RTS-DREG '>  > 0'                   
                                                                                
05133          INITIALIZE                ADACODE                        P442288 
05134          MOVE ZERO                 TO WS-FETCH-COUNT              P442288 
05135          MOVE ZERO                 TO SW-REGPLATE                 P442288 
05136          PERFORM 2112321-FETCH-DUP THRU                           P442288 
05137                  2112321-FETCH-DUP-EXIT                           P442288 
05138              UNTIL ADACODE NOT = ZEROS                            P442288 
05139      END-IF.                                                      P442288 
05140      IF TESTING-SHOW-DISPLAYS                                     P442288 
05141          DISPLAY 'AFT 2112321-FETCH '                             P442288 
05142             ' ' RTS-DREG-DOCNO                                    P442288 
05143             ' FOR ' WS-REG-PLTNO-REGEXPYR                         P442288 
05144      END-IF.                                                      P442288 
05145                                                                   P442288 
05146      EXEC ADABAS                                                  P442288 
05147            CLOSE DREG                                             P442288 
05148      END-EXEC                                                     P442288 
05149          .                                                        P442288 
05150                                                                   P442288 
05151  211232-GET-ACTIV-REGIS-EXIT.                                     P442288 
05152      EXIT.                                                        P442288 
05153                                                                   P442288 
05154  2112321-FETCH-DUP.                                               P442288 
05155                                                                   P442288 
05156 *---------------------------------------------------------------* P442288 
05157 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
05158 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
05159 *---------------------------------------------------------------* P442288 
05160                                                                   P442288 
05161      EXEC ADABAS                                                  P442288 
05162           FETCH DREG                                              P442288 
05163      END-EXEC                                                     P442288 
05164         .                                                         P442288 
05165      IF RESPONSE-CODE OF RTS-DREG > ZERO AND                      P442288 
05166         RESPONSE-CODE OF RTS-DREG NOT = 3                         P442288 
05167          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
05168          MOVE 'FETCH REGIS'        TO ERROR-MSG-014-FILE          P442288 
05169          MOVE RESPONSE-CODE OF RTS-DREG TO ERROR-MSG-014-ERRNUMB  P442288 
05170          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
05171          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
05172          MOVE 20                   TO WS-RETURN-CODE              P442288 
05173          PERFORM Z-Z-TERMINATION.                                 P442288 
05174                                                                   P442288 
05175      IF ADACODE NOT EQUAL ZERO                                    P442288 
05176          GO TO 2112321-FETCH-DUP-EXIT.                            P442288 
05177                                                                   P442288 
05178      IF TESTING-SHOW-DISPLAYS                                     P442288 
05179          DISPLAY '@ 2112321-FETCH '                               P442288 
05180             ' ' RTS-DREG-DOCNO                                    P442288 
05181             ' FOR ' WS-REG-PLTNO-REGEXPYR                         P442288 
05182          DISPLAY '@ 2112321- SKIP '                               P442288 
05183             ' ' RTS-TTL-SKIPPED-OLD-DOCNO                         P442288 
05184             ' ' RTS-TTL-SKIPPED-FLAG                              P442288 
05185      END-IF.                                                      P442288 
05186                                                                   P442288 
05187      IF WS-RTS-DOCNO = RTS-DREG-DOCNO  OR                         P442288 
05188          RTS-DREG-DOCNO = RTS-TTL-SKIPPED-OLD-DOCNO               P442288 
05189          NEXT SENTENCE                                            P442288 
05190      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC262 - IF WS-RTS-DOCNO <'                             
      D                  WS-RTS-DOCNO '>  = RTS-DREG-DOCNO <'                   
      D                  RTS-DREG-DOCNO '>'                                     
      D        DISPLAY 'OR RTS-DREG-DOCNO <' RTS-DREG-DOCNO '>  = '             
      D                'RTS-TTL-SKIPPED-OLD-DOCNO <'                            
      D                 RTS-TTL-SKIPPED-OLD-DOCNO '>'                           
                                                                                
05191          MOVE 4                            TO ERROR-PROCESSING-SW P442288 
05192 *---------------------------------------------------------------* P442288 
05193 *        ALLOW THIS TO GO THROUGH.  THERE IS A SYNCHRONIZATION  * P442288 
05194 *        ERROR WITHIN RTS.  TITLE AND REGIS REGIS RECORDS DO NOT* P442288 
05195 *        MATCH.  THIS SHOULD NOT HAPPEN IN PRODUCTION, BUT CAN  * P442288 
05196 *        EASILY OCCUR ON TEST FILES USING NATS TRANS 'RFAA1'    * P442288 
05197 *        IF THE TESTOR DOES NOT UPDATE BOTH REGIS AND TITLE     * P442288 
05198 *        RECORDS WITH THE SAME LICENSE NUMBERS AND EXPIRATION   * P442288 
05199 *        DATES.  THE UPDATES WILL BE REJECTED, MESSING UP TEST  * P442288 
05200 *        RESULTS.  THIS ERROR MESSAGE LETS YOU KNOW ABOUT THAT. * P442288 
05201 *---------------------------------------------------------------* P442288 
05202          MOVE WS-SUPERZA-REGPLTNO          TO ERROR-MSG-018-PLT   P442288 
05203          MOVE WS-SUPERZA-REGEXPYR          TO ERROR-MSG-018-EXP   P442288 
05204          MOVE RTS-DREG-DOCNO               TO ERROR-MSG-018-DOCNO P442288 
05205          MOVE WS-PLATE-NO-RTS              TO ERR-LICN            P442288 
05206          MOVE ERROR-MSG-018-TEXT           TO ERR-ERROR-MESSAGE-1 P442288 
05207          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
05208          MOVE SPACES                       TO  ERR-LICN.          P442288 
05209                                                                   P442288 
05210  2112321-FETCH-DUP-EXIT.                                          P442288 
05211      EXIT.                                                        P442288 
05212                                                                   P442288 
05213  211233-CHECK-TO-CANCEL.                                          P442288 
05214                                                                   P442288 
05215      IF TESTING-SHOW-DISPLAYS                                     P442288 
05216          DISPLAY '@   211233-CHECK-TO-CANCEL PLT '                P442288 
05217            WS-REGIS-RTS-REGPLTNO ' EXP ' WS-REGIS-RTS-REG-EXP-DT     CL**9
05218            ' EARLY BEFORE ' WS-MIN-BEGIN-REG-DT                   P442288 
05219            ' LAST YEAR IF>' WS-PREV-YEAR-REG-EXP-DT               P442288 
05220          DISPLAY  ' IRP ' WS-PLATE-NO-RTS                         P442288 
05221            ' FROM ' WRKRTS-BEGINNING-REG-DT                       P442288 
05222            ' TO ' WRKRTS-REG-EXP-DT  '  INV=' RTS-IRPINV-FLAG     P442288 
05223      END-IF.                                                      P442288 
05224                                                                   P442288 
05225      IF WRKRTS-REG-EXP-DT     < WS-PREV-YEAR-REG-EXP-DT AND       P442288 
05226         (WS-REGIS-RTS-REGPLTNO = WS-PLATE-NO-RTS OR                  CL**9
05227          WS-REGIS-RTS-PRVPLTNO = WS-PLATE-NO-RTS)                    CL**9
05228 *       NEW YEARS REGISTRATION HAS ALREADY BEEN APPLIED - DO    * P442288 
05229 *       NOT WIPE IT OUT.  ISSUE MESSAGE AND SKIP MV-FUNC UPDATE.* P442288 
05230 *       THIS TYPICALLY HAPPENS IN THE JAN 1 - MAR 31 PERIOD.    * P442288 
05231 *       OLD YEAR UPDATES ARE HAPPENING CONCURRENTLY WITH NEW    * P442288 
05232 *       YEARS UPDATES, OR EVEN AFTER A NEW YEAR UPDATE PROCESS. * P442288 
                                                                                
      D        DISPLAY '!TC257 - IF WRKRTS-REG-EXP-DT <'                        
      D               WRKRTS-REG-EXP-DT '>  < WS-PREV-YEAR-REG-EXP-DT'          
      D                 WS-PREV-YEAR-REG-EXP-DT '>'                             
      D        DISPLAY 'AND (WS-REGIS-RTS-REGPLTNO <'                           
      D                      WS-REGIS-RTS-REGPLTNO '>  = '                      
      D          'WS-PLATE-NO-RTS <' WS-PLATE-NO-RTS '> '                       
      D        DISPLAY 'OR WS-REGIS-RTS-PRVPLTNO <'                             
      D                    WS-REGIS-RTS-PRVPLTNO '>  = '                        
      D                   'WS-PLATE-NO-RTS) <'                                  
      D                    WS-PLATE-NO-RTS '>'                                  
                                                                                
05233          MOVE 4                           TO ERROR-PROCESSING-SW  P442288 
05234          MOVE 3                           TO RTS-UPDT-ERROR-FLAG  P442288 
05235          MOVE WS-REGIS-RTS-REGPLTNO       TO ERROR-MSG-016-PLT       CL**9
05236          MOVE WS-REGIS-RTS-REG-EXP-DT     TO ERROR-MSG-016-EXP-DT    CL**9
05237          MOVE WS-RTS-DOCNO                TO ERROR-MSG-016-DOCNO  P442288 
05238          MOVE WS-PLATE-NO-RTS             TO ERR-LICN             P442288 
05239          MOVE 'BYPASSED--REG: '           TO ERROR-MSG-016-TYPE   P442288 
05240          MOVE 'CURRENT FOR'               TO ERROR-MSG-016-NOW    P442288 
05241          MOVE ERROR-MSG-016-TEXT          TO ERR-ERROR-MESSAGE-1  P442288 
05242          PERFORM Z-V-WRITE-MVUPDT-REPORT                          P442288 
05243          MOVE SPACES                      TO  ERR-LICN            P442288 
05244          GO TO 211233-CHECK-TO-CANCEL-EXIT.                       P442288 
05245                                                                   P442288 
05246 *---------------------------------------------------------------* P442288 
05247 *        REGISTRATION WILL BE CANCELLED AND AN ERROR MESSAGE    * P442288 
05248 *        GENERATED BY ANOTHER PARAGRAPH (21139-CANCEL-CURRENT..)* P442288 
05249 *---------------------------------------------------------------* P442288 
05250      IF WS-CRNT-REG-UPD-DT < WS-MIN-BEGIN-REG-DT OR               P442288 
05251         WS-REGIS-RTS-REG-EXP-DT > WRKRTS-BEGINNING-REG-DT            CL**9
                                                                                
      D        DISPLAY '!TC258 - IF WS-CRNT-REG-UPD-DT <'                       
      D          WS-CRNT-REG-UPD-DT '>  < WS-MIN-BEGIN-REG-DT <'                
      D                                   WS-MIN-BEGIN-REG-DT '>'               
      D        DISPLAY 'OR WS-REGIS-RTS-REG-EXP-DT <'                           
      D                    WS-REGIS-RTS-REG-EXP-DT '>  > '                      
      D         'WRKRTS-BEGINNING-REG-DT <' WRKRTS-BEGINNING-REG-DT '>'         
                                                                                
05252          MOVE 'Y'                          TO RTS-IRPINV-FLAG     P442288 
05253          GO TO 211233-CHECK-TO-CANCEL-EXIT.                       P442288 
05254                                                                   P442288 
05255      IF TESTING-SHOW-DISPLAYS                                     P442288 
05256          DISPLAY 'AFT 211233-CHECK-TO-CANCEL PLT '                P442288 
05257            WS-REGIS-RTS-REGPLTNO ' EXP ' WS-REGIS-RTS-REG-EXP-DT     CL**9
05258            ' EARLY BEFORE ' WS-MIN-BEGIN-REG-DT                   P442288 
05259            ' LAST YEAR IF>' WS-PREV-YEAR-REG-EXP-DT               P442288 
05260          DISPLAY ' IRP ' WS-PLATE-NO-RTS ' ON ' WS-CRNT-REG-UPD-DTP442288 
05261            ' FROM ' WRKRTS-BEGINNING-REG-DT                       P442288 
05262            ' TO ' WRKRTS-REG-EXP-DT  '  INV=' RTS-IRPINV-FLAG     P442288 
05263      END-IF.                                                      P442288 
05264  211233-CHECK-TO-CANCEL-EXIT.                                     P442288 
05265      EXIT.                                                        P442288 
05266  EJECT                                                            P442288 
05267                                                                   P442288 
05268  211234-UNDET-ITEMCD.                                             P442288 
05269                                                                   P442288 
05270 *   ALLOW THIS TO GO THROUGH.  DO NOT SEND RTS-MV-FUNC-TRANS BUT* P442288 
05271 *   DO SEND RTS-FUNDS DATA                                      * P442288 
05272 *   SET  RTS-INVALID-UPDATE   INDICATOR FLAG                    * P442288 
                                                                                
      D    DISPLAY '211234-UNDET-ITEMCD'                                        
                                                                                
05273      MOVE ZERO                         TO RTS-TRANS-ERROR-FLAG.   P442288 
05274      MOVE 1                            TO RTS-UPDT-ERROR-FLAG     P442288 
05275      MOVE 4                            TO ERROR-PROCESSING-SW.    P442288 
05276      MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN.               P442288 
05277      MOVE ERROR-MSG-045-TEXT           TO ERR-ERROR-MESSAGE-1.    P442288 
05278      PERFORM Z-4-CONTROL-EDIT-ERROR.                              P442288 
05279      MOVE SPACES                       TO  ERR-LICN.              P442288 
05280      IF TESTING-SHOW-DISPLAYS                                     P442288 
05281          DISPLAY ' LOGIC ERROR - ITEMCD / ACCOUNT NUMBER ASSOC'   P442288 
05282      END-IF.                                                      P442288 
05283                                                                   P442288 
05284                                                                   P442288 
05285  2113-CREATE-RTS-TRANSACTIONS.                                    P442288 
05286                                                                   P442288 
05287      IF RTS-NOGEN-MV-FUNC                                            CL*11
                                                                                
      D      DISPLAY '!TC141 - IF RTS-NOGEN-MV-FUNC(3 THRU 5) <'                
      D                 RTS-UPDT-ERROR-FLAG '>'                                 
                                                                                
05288          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR                           CL*11
05289                                         TO WS-REGIS-RTS-REGPLTNO.    CL*11
05290                                                                      CL*11
05291         IF TESTING-SHOW-DISPLAYS                                  P442288 
05292             DISPLAY '@2113 CREATE-RTS  TRANS FOR IF KEY: '        P442288 
05293               WRKRTS-KEY ' LICN: ' WRKRTS-CURRENT-TEXAS-LIC-NBR   P442288 
05294             DISPLAY ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG       P442288 
05295                 ' UPDT ERR FL  = ' RTS-UPDT-ERROR-FLAG            P442288 
05296                 ' ERROR SW     = ' ERROR-PROCESSING-SW            P442288 
05297                 ' INV=' RTS-IRPINV-FLAG                           P442288 
05298                 '-- EXP-ACT-TYPE :' WRKRTS-ACT-TYPE                  CL**2
05299                 ' CHANGE-TYPE:' WRKRTS-TYPE-OF-CHANGE-FLAG        P442288 
05300             DISPLAY ' RGN-OFFICE-FLAG: ' WS-RGN-OFFICE-FLAG       P442288 
05301                     ' RGN: ' WS-RGN-OFFICE                        P442288 
05302                     ' #: ' WS-RGN-OFC-ISS-NO                      P442288 
05303                     ' OFC ISS NO: ' WS-RGN-OFC-ISS-NO             P442288 
05304             DISPLAY ' TRANS KEYS: '                               P442288 
05305                     ' OFCISSNO='  WS-OFCISSUANCENO                P442288 
05306                     ' TRANSWSID=' WS-TRANSWSID                    P442288 
05307                     ' AMDATE='    WS-TRANSAMDATE                  P442288 
05308                     ' TIME='      WS-TRANSTIME                    P442288 
05309             DISPLAY ' DATA ITEMS: '                                  CL*17
05310                 WRKRTS-FLEET-PLT-ITMCD ' <WRKRTS--ITMCD  '           CL*17
05311                 WRKRTS-FLEET-REGCL-CD  ' <WRKRTS--REGCL  '           CL*17
05312             DISPLAY ' WGT  ITEMS: '                                  CL*23
05313                 WS-VEHGROSSWT          ' <RTS PREV-GWT   '           CL*23
05314                 WRKRTS-GROSS-WEIGHT    ' <WRKRTS-NEW-GWT '           CL*23
05315          DISPLAY 'CRNT TRANS KEY="' WS-TRANS-KEY '" '             P442288 
05316                  'PREV TRANS KEY="' WS-TRANS-PREV-KEY '" '        P442288 
05317         END-IF.                                                   P442288 
05318                                                                      CL*23
05319         IF WRKRTS-GROSS-WEIGHT = ZERO                                CL*23
05320             IF WS-VEHGROSSWT > ZERO                                  CL*23
                                                                                
      D               DISPLAY '!TC142 - IF WRKRTS-GROSS-WEIGHT = ZERO <'        
      D                  WRKRTS-GROSS-WEIGHT '>'                                
      D               DISPLAY 'AND IF WS-VEHGROSSWT > ZERO <'                   
      D                               WS-VEHGROSSWT '>'                         
                                                                                
05321                 MOVE WS-VEHGROSSWT        TO WRKRTS-GROSS-WEIGHT     CL*23
05322             ELSE                                                     CL*23
                                                                                
      D              DISPLAY '!TC143 - IF WRKRTS-GROSS-WEIGHT = ZERO <'         
      D                  WRKRTS-GROSS-WEIGHT '>'                                
      D               DISPLAY 'AND IF WS-VEHGROSSWT NOT > ZERO <'               
      D                               WS-VEHGROSSWT '>'                         
                                                                                
05323                 MOVE WS-VEHEMPTYWT        TO WRKRTS-GROSS-WEIGHT     CL*23
05324             END-IF                                                   CL*23
                                                                                
      D           DISPLAY '!TC144 - IF WRKRTS-GROSS-WEIGHT = ZERO <'            
      D               WRKRTS-GROSS-WEIGHT '>'                                   
                                                                                
05325             MOVE WRKRTS-GROSS-WEIGHT      TO WS-VEHGROSSWT           CL*23
05326             MOVE 4                        TO ERROR-PROCESSING-SW     CL*23
05327             MOVE WS-PLATE-NO-RTS          TO ERR-LICN                CL*23
05328             MOVE ERROR-MSG-055-TEXT       TO ERR-ERROR-MESSAGE-1     CL*23
05329             PERFORM Z-5-WRITE-UPDATE-REPORT                          CL*23
05330             MOVE SPACES                   TO  ERR-LICN               CL*23
05331         ELSE                                                         CL*23
                                                                                
      D           DISPLAY '!TC145 - IF WRKRTS-GROSS-WEIGHT > ZERO <'            
      D               WRKRTS-GROSS-WEIGHT '>'                                   
                                                                                
05332                 MOVE WRKRTS-GROSS-WEIGHT  TO WS-VEHGROSSWT           CL*23
05333         END-IF.                                                      CL*23
05334 *  ---------------------------------------------------------*     P442288 
05335 *   CREATE RTS TRANSACTION FOR ONE INTERFACE RECORD         *     P442288 
05336 *   (2.0)  RTS DOES NOT NEED TRANSACTIONS IN CASE OF        *     P442288 
05337 *          MILEAGE CHANGE (FLAG='M')                        *     P442288 
05338 *   (3.0)  IN CASE OF WIGHT CHANGE NO NEED FOR INVENTORY    *     P442288 
05339 *          TRANSACTIONS.                                    *     P442288 
05340 *   (4.0)  RTS INTERFACE RECORD IS MOVED TO ARCHIVE FILE    *     P442288 
05341 *          WITH WRKRTS-PROCESSING-FLAG AS FOLLOWS:          *     P442288 
05342 *           'F'  PROCESSING WAS OK                          *     P442288 
05343 *           'P'  PLATE ISSUE LATE FOR TX-ON-LINE            *        CL**8
05344 *           'V'  NO VIN  - NO TITLE FOUND IN RTS-TITLE FILE *        CL**8
05345 *           'D'  DUMMY (NO-TITLE) RECORD                    *     P442288 
05346 *           'S'  SKIPPED INTERFACE RECORD - CONVERTER GEARS *     P442288 
05347 *           'W'  SKIPPED INTERFACE RECORD - WEIGHT-CHANGE   *     P442288 
05348 *           'A'  SKIPPED INTERFACE RECORD - AUDIT-MILEAGE   *     P442288 
05349 *           'M'  SKIPPED INTERFACE RECORD - MILEAGE-CHANGE  *     P442288 
05350 *           'E'  RECORD REJECTED DUE TO TYPE OF CHANGE,     *     P442288 
05351 *                E.G. 'M' MILEAGE CHANGE -  NOT NEEDED BY RTS     P442288 
05352 *                OR WRONG TYPE OF CHANGE.                   *     P442288 
05353 *  ---------------------------------------------------------*     P442288 
05354                                                                   P442288 
05355 *  ---------------------------------------------------------*     P442288 
05356 *   INCREMENT TIME IN THE RTS TRANSACTION KEY                  *  P442288 
05357 *   THIS GENERATES A UNIQUE TRANSACTION KEY                    *  P442288 
05358 *  ---------------------------------------------------------*     P442288 
05359                                                                   P442288 
05360 *---------------------------------------------------------------* P442288 
05361 *   REGION ID IS FROM WS-RTS-REGION WHICH SHOULD COME FROM THE  * P442288 
05362 *   SUPP-CHANGE-REGION FIELD OF THE IRP MASTER FILE SUPPLEMENT  * P442288 
05363 *   CONTROL RECORD.  THE PROBLEM IS, CURRENTLY, THAT FIELD IS   * P442288 
05364 *   FROM THE 'MVSC' TERMINAL OR USER SECURITY RECORD AND IS NOT * P442288 
05365 *   VALIDATED. IF IT IS NOT A VALID REGION ID THEN USE THE FIELD* P442288 
05366 *   WRKRTS-REGION - THIS IS INSERTED BY THE BILLING PROGRAM FROM* P442288 
05367 *   THE ACCOUNT ADDRESS RECORD IN THE IRP MASTER FILE.          * P442288 
05368 *---------------------------------------------------------------* P442288 
05369                                                                   P442288 
05370      IF WS-RGN-OFFICE = SPACE                                     P442288 
                                                                                
      D    DISPLAY '!TC146 - IF WS-RGN-OFFICE = SPACE <'                        
      D                         WS-RGN-OFFICE '>'                               
                                                                                
05371          MOVE C-OFC-ISSUANCE-CODE-HQ   TO WS-OFCISSUANCECD        P442288 
05372          MOVE C-OFC-ISSUANCE-NO-HQ     TO WS-OFCISSUANCENO        P442288 
05373      ELSE                                                         P442288 
                                                                                
      D    DISPLAY '!TC147 - IF WS-RGN-OFFICE > SPACE <'                        
      D                         WS-RGN-OFFICE '>'                               
                                                                                
05374          MOVE C-OFC-ISSUANCE-CODE-RGN  TO WS-OFCISSUANCECD        P442288 
05375          MOVE WS-RGN-OFC-ISS-NO        TO WS-OFCISSUANCENO.       P442288 
05376                                                                   P442288 
05377 *-----------------------------------------------------------*     P442288 
05378 *GET CUSNAME                                                      P442288 
05379 *-----------------------------------------------------------*     P442288 
05380      IF  WRKRTS-OWNER-OPERATOR = 'SAME'                           P442288 
                                                                                
      D    DISPLAY '!TC148 - IF  WRKRTS-OWNER-OPERATOR = SAME <'                
      D                          WRKRTS-OWNER-OPERATOR '>'                      
                                                                                
05381          MOVE WRKRTS-NAME-OF-APPLICANT TO WS-CUST-NAME            P442288 
05382      ELSE                                                         P442288 
                                                                                
      D    DISPLAY '!TC149 - IF  WRKRTS-OWNER-OPERATOR NOT = SAME <'            
      D                          WRKRTS-OWNER-OPERATOR '>'                      
                                                                                
05383          MOVE WRKRTS-OWNER-OPERATOR    TO WS-CUST-NAME.           P442288 
05384                                                                   P442288 
05385 *  ---------------------------------------------------------*     P442288 
05386 *   MAKE TRANSACTION KEY UNIQUE                             *     P442288 
05387 *  ---------------------------------------------------------*     P442288 
05388                                                                   P442288 
05389      ADD 1                         TO WS-TRANSTIME.               P442288 
      D    DISPLAY 'ADD 1 TO WS-TRANSTIME <' WS-TRANSTIME '>'.          P442288 
05390                                                                   P442288 
05391      MOVE WS-TRANS-KEY             TO WS-TRANS-PREV-KEY.          P442288 
05392                                                                   P442288 
05393 *  ---------------------------------------------------------*     P442288 
05394 *   VALIDATE RTS TRANSACTION KEY AND KEY FIELDS             *     P442288 
05395 *  ---------------------------------------------------------*     P442288 
05396                                                                   P442288 
05397      IF WS-OFCISSUANCENO  NOT NUMERIC OR                          P442288 
05398         WS-TRANSWSID      NOT NUMERIC OR                          P442288 
05399         WS-TRANSAMDATE    NOT NUMERIC OR                          P442288 
05400         WS-TRANSTIME      NOT NUMERIC OR                          P442288 
05401         WS-OFCISSUANCECD  NOT NUMERIC                             P442288 
                                                                                
      D    DISPLAY '!TC150 - IF WS-OFCISSUANCENO  NOT NUMERIC <'                
      D                         WS-OFCISSUANCENO '>'                            
      D    DISPLAY 'OR WS-TRANSWSID NOT NUMERIC <'                              
      D                WS-TRANSWSID '>'                                         
      D    DISPLAY 'OR WS-TRANSAMDATE NOT NUMERIC <'                            
      D                WS-TRANSAMDATE '>'                                       
      D    DISPLAY 'OR WS-TRANSTIME NOT NUMERIC <'                              
      D                WS-TRANSTIME '>'                                         
      D    DISPLAY 'OR WS-OFCISSUANCECD NOT NUMERIC <'                          
      D                WS-OFCISSUANCECD '>'                                     
                                                                                
05402          MOVE 3                    TO RTS-TRANS-ERROR-FLAG        P442288 
05403          MOVE WS-OFCISSUANCENO     TO ERROR-MSG-021-OFFNO         P442288 
05404          MOVE WS-TRANSWSID         TO ERROR-MSG-021-WSID          P442288 
05405          MOVE WS-TRANSAMDATE       TO ERROR-MSG-021-DATE          P442288 
05406          MOVE WS-TRANSTIME         TO ERROR-MSG-021-TIME          P442288 
05407          MOVE WS-OFCISSUANCECD     TO ERROR-MSG-021-OFFCD         P442288 
05408          MOVE 4                    TO ERROR-PROCESSING-SW         P442288 
05409          MOVE ERROR-MSG-021-TEXT   TO ERR-ERROR-MESSAGE-1         P442288 
05410          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
05411          MOVE SPACES               TO  ERR-LICN                   P442288 
05412 *---------------------------------------------------------------* P442288 
05413 *       ERROR MESSAGE FOR INVALID TRANSACTION KEY FIELDS        * P442288 
05414 *---------------------------------------------------------------* P442288 
05415          IF TESTING-SHOW-DISPLAYS                                 P442288 
05416             DISPLAY ' TRANSACTION REJECTED - NON NUMERIC KEYS '   P442288 
05417             DISPLAY ' TRANS KEYS: '                               P442288 
05418                     ' OFCISSNO='  WS-OFCISSUANCENO                P442288 
05419                     ' TRANSWSID=' WS-TRANSWSID                    P442288 
05420                     ' AMDATE='    WS-TRANSAMDATE                  P442288 
05421                     ' TIME='      WS-TRANSTIME                    P442288 
05422                     ' OFCISSCD='  WS-OFCISSUANCECD.               P442288 
05423                                                                   P442288 
                                                                                
      D      DISPLAY '!TCXXM - IF RTS-NOGEN-MV-FUNC(3 THRU 5) <'                
      D                 RTS-UPDT-ERROR-FLAG '> RTS-IRPINV-FLAG <'               
      D                                        RTS-IRPINV-FLAG '>'              
                                                                                
05424      IF RTS-NOGEN-MV-FUNC                                            CL*11
05425          NEXT SENTENCE                                               CL**9
05426      ELSE                                                            CL**9
05427      IF RTS-IRPINV-FLAG = 'Y'                                     P442288 
                                                                                
      D        DISPLAY '!TC151 - IF NOT RTS-NOGEN-MV-FUNC(3 THRU 5) <'          
      D              RTS-UPDT-ERROR-FLAG '>'                                    
      D              ' AND IF RTS-IRPINV-FLAG = Y <'                            
      D                      RTS-IRPINV-FLAG '>'                                
                                                                                
05428          PERFORM 21139-CANCEL-CURRENT-REGIS                       P442288 
05429          ADD 1                     TO WS-TRANSTIME                P442288 
      D    DISPLAY 'ADD 1 TO WS-TRANSTIME <' WS-TRANSTIME '>'           P442288 
05430      ELSE                                                         P442288 
05431      IF RTS-IRPINV-FLAG = 'R'                                     P442288 
                                                                                
      D        DISPLAY '!TC156 - IF RTS-IRPINV-FLAG = R <'                      
      D                      RTS-IRPINV-FLAG '>'                                
                                                                                
05432          PERFORM 21139-CANCEL-CURRENT-REGIS                       P442288 
05433          PERFORM Z-11-COMMIT-ADABAS                               P442288 
05434          ADD 1                     TO WS-RTS-NO-TRANS-BATCH       P442288 
05435          ADD 1                     TO WS-TRANSTIME                P442288 
      D    DISPLAY 'ADD 1 TO WS-TRANSTIME <' WS-TRANSTIME '>'           P442288 
05436          GO TO 2113-CREATE-RTS-TRANS-EXIT                         P442288 
05437      ELSE                                                         P442288 
05438      IF RTS-IRPINV-FLAG = 'E'                                     P442288 
                                                                                
      D        DISPLAY '!TC157 - IF RTS-IRPINV-FLAG = E <'                      
      D                      RTS-IRPINV-FLAG '>'                                
                                                                                
05439          PERFORM 21139-CANCEL-CURRENT-REGIS                       P442288 
05440          PERFORM Z-11-COMMIT-ADABAS                               P442288 
05441          ADD 1                     TO WS-TRANSTIME                P442288 
      D    DISPLAY 'ADD 1 TO WS-TRANSTIME <' WS-TRANSTIME '>'           P442288 
  442          GO TO 2113-CREATE-RTS-TRANS-EXIT.                        P442288 
05443                                                                   P442288 
05444 *---------------------------------------------------------------* P442288 
05445 *        THE "L" (LICN-REPL), "TONLY" CHECK FOR VALID           * P442288 
05446 *        REGIS (CURRENT) IS IN THE 21137- PARA, JUST            * P442288 
05447 *        BEFORE THE IRPXCH TRANS IS WRITTEN                     * P442288 
05448 *---------------------------------------------------------------* P442288 
05449                                                                   P442288 
                                                                                
      D        DISPLAY '!TC158 - PERFORM 21131-SETUP-TRANSCD-ETC'               
                                                                                
05450       PERFORM 21131-SETUP-TRANSCD-ETC.                               CL*17
05451                                                                   P442288 
05452         IF TESTING-SHOW-DISPLAYS                                  P442288 
05453             DISPLAY '@ 2113 WS-ITMCD CHECK: ' WS-ITMCD            P442288 
05454               ' LICN: ' WRKRTS-CURRENT-TEXAS-LIC-NBR              P442288 
05455               ' ' WS-ITMCD ' ' WS-REGCLASSCD                      P442288 
05456               '-- EXP-ACT-TYPE :' WRKRTS-ACT-TYPE                    CL**2
05457               ' CHANGE-TYPE:' WRKRTS-TYPE-OF-CHANGE-FLAG          P442288 
05458                 ' CHANGE-TYPE:' WRKRTS-TYPE-OF-CHANGE-FLAG        P442288 
05459               ' BATCH: ' BATCH-KEY                                P442288 
05460             DISPLAY 'FEES: APP:' WRKRTS-APPORTIONED-FEE           P442288 
05461                ',REF:' WRKRTS-REFLECT-PORTION                     P442288 
05462                ',ATM:' WRKRTS-AUTOMATE-PORTION                    P442288 
05463                ',INS:' WRKRTS-INS-PORTION                         P442288 
05464                ',SB5:' WRKRTS-TERP-PORTION                        P442288 
05465                ',CMF:' WRKRTS-CNTY-MOBILITY-PORTION                  CL*14
05466                ',REP:' WRKRTS-REPLACEMENT-FEE                     P442288 
05467                ',CRB:' WRKRTS-CNTY-ADD-ON-FEE                     P442288 
05468                ',CSF:' WRKRTS-CNTY-CHILD-SAFE-PORTION             P442288 
05469 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD3" OR "TST5" * P442288 
05470 *---------------------------------------------------------------*    CL**2
05471 *           IF  WS-TAG-POWER-UNIT-SUFFIX                        *    CL**2
05472 *---------------------------------------------------------------*    CL**2
05473             IF WRKRTS-ACT-TYPE = 'A' OR 'H' OR 'P' OR 'F'            CL**2
05474 *---------------------------------------------------------------*    CL**2
05475 *                IF WS-TAG-FORESTRY-ACCOUNT                     *    CL**2
05476 *---------------------------------------------------------------*    CL**2
05477                  IF WRKRTS-ACT-TYPE = 'F'                            CL**2
05478                      DISPLAY 'SET FORESTRY TRUCK'                 P442288 
05479                  ELSE                                             P442288 
05480                      DISPLAY 'SET POWER TRUCK'                    P442288 
05481             ELSE                                                  P442288 
05482             IF WRKRTS-ACT-TYPE = 'E' OR                              CL*17
05483                WRKRTS-ACT-TYPE = 'U'                                 CL*17
05484                  DISPLAY 'EXTENDED FLEET REGISTRATION'               CL*17
05485             ELSE                                                     CL*17
05486 *---------------------------------------------------------------*    CL**2
05487 *               IF WS-TAG-5-YR-TKN-TR-ACCOUNT                   *    CL**2
05488 *---------------------------------------------------------------*    CL**2
05489                 IF (WRKRTS-ACT-TYPE = 'T' OR 'T')                    CL**2
05490                     DISPLAY 'SET TKN TRAILER'                     P442288 
05491                 ELSE                                              P442288 
05492 *---------------------------------------------------------------*    CL**2
05493 *               IF WS-TAG-5-YR-APP-ACCOUNT                      *    CL**2
05494 *                   DISPLAY 'SET APP TRAILER'                   *    CL**2
05495 *               ELSE                                            *    CL**2
05496 *---------------------------------------------------------------*    CL**2
05497 *---------------------------------------------------------------*    CL**2
05498 *               IF WS-TAG-RENTAL-TRLR-ACCOUNT                   *    CL**2
05499 *---------------------------------------------------------------*    CL**2
05500                 IF WRKRTS-ACT-TYPE = 'R'                             CL**2
05501                     DISPLAY 'SET 5 RENT TRLR'                     P442288 
05502                 ELSE                                              P442288 
05503                     DISPLAY 'SET APP TRAILER'.                    P442288 
05504 *-----------------------------------------------------------*     P442288 
05505 *INVITMYR   IN RTS_TR_INV_DETAIL                                  P442288 
05506 *-----------------------------------------------------------*     P442288 
05507 *---------------------------------------------------------------* P442288 
05508 *   SET EFFECTIVE DATE, & EXPIRATION DATE                       * P442288 
05509 *---------------------------------------------------------------* P442288 
05510                                                                   P442288 
05511      MOVE WS-CRNT-DATE-DT          TO WS-REGEFFDATE.              P442288 
05512      MOVE WRKRTS-BEGINNING-REG-DT  TO WS-REGEFF-DT.               P442288 
05513      MOVE 01                       TO WS-REGEFFDATE-DD.           P442288 
05514                                                                   P442288 
05515 *---------------------------------------------------------------* P442288 
05516 *    THIS ADJUSTMENT FOR REG EXP YR (INVITMYR) SHOULD BE REMOVED* P442288 
05517 *    AFTER ALL EARLY 1999 CREATED RTS INTERFACE RECORDS HAVE    * P442288 
05518 *    BEEN PROCESSED.  CAREFULLY TEST BEFORE REMOVING THIS CODE  * P442288 
05519 *    TO ENSURE THAT ALL ACCOUNT TYPES ARE HANDLED CORRECTLY.    * P442288 
05520 *    EARLY 1999 VERSION OF P442253 WAS NOT CORRECTLY SETTTING   * P442288 
05521 *    THE WRKRTS-REG-EXP-DT-YYYY                                 * P442288 
05522 *---------------------------------------------------------------* P442288 
05523                                                                   P442288 
05524      MOVE WRKRTS-REG-EXP-DT-YYYY       TO WS-INVITMYR.            P442288 
05525      MOVE WRKRTS-REG-EXP-DT-YYYY       TO WS-REGEXPYR.            P442288 
05526      MOVE WRKRTS-REG-EXP-DT-MM         TO WS-REGEXPMO.            P442288 
05527                                                                   P442288 
05528      IF WRKRTS-REG-EXP-DT-YYYY NOT = WS-INVITMYR OR               P442288 
05529         WRKRTS-REG-EXP-DT-YYYY NOT = WS-REGEXPYR                  P442288 
                                                                                
      D       DISPLAY '!TC185 - IF WRKRTS-REG-EXP-DT-YYYY <'                    
      D         WRKRTS-REG-EXP-DT-YYYY '> NOT = WS-INVITMYR <'                  
      D                                         WS-INVITMYR '>'                 
      D       DISPLAY 'OR IF WRKRTS-REG-EXP-DT-YYYY NOT = WS-REGEXPYR <'        
      D               WS-REGEXPYR '>'                                           
                                                                                
05530 *---------------------------------------------------------------* P442288 
05531 *-- WHAT WAS THIS ? ?                 --------------------------* P442288 
05532 *        MOVE WRKRTS-REGION            TO SUPP-CHANGE-REGION    * P442288 
05533 *                                         WS-RGN-OFFICE         * P442288 
05534 *                                         ERROR-MSG-020-ACTRGN  * P442288 
05535 *        MOVE 'SUPP'                   TO ERROR-MSG-020-TYPE    * P442288 
05536 *        MOVE ' USING '                TO ERROR-MSG-020-USING   * P442288 
05537 *---------------------------------------------------------------* P442288 
05538          MOVE WRKRTS-REG-EXP-DT            TO ERROR-MSG-044-EXPDTAP442288 
05539          MOVE WRKRTS-TYPE-OF-CHANGE-FLAG   TO ERROR-MSG-044-TYPE  P442288 
05540          MOVE WS-EXP-DT-YYYYMM             TO ERROR-MSG-044-EXPDTCP442288 
05541          MOVE 4                            TO ERROR-PROCESSING-SW P442288 
05542          MOVE ERROR-MSG-044-TEXT           TO ERR-ERROR-MESSAGE-1 P442288 
05543          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN            P442288 
05544          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
05545          MOVE SPACES                       TO  ERR-LICN.          P442288 
05546                                                                   P442288 
05547                                                                   P442288 
05548      MOVE WRKRTS-REG-EXP-DT-YYYY   TO WS-INVITMYR.                P442288 
05549      MOVE WRKRTS-REG-EXP-DT-YYYY   TO WS-REGEXPYR.                P442288 
05550      MOVE WRKRTS-REG-EXP-DT-MM     TO WS-REGEXPMO.                P442288 
05551                                                                   P442288 
05552      MOVE WS-REGIS-PLTBIRTHDATE        TO WS-REGIS-RTS-BIRTHDATE.    CL**9
05553                                                                   P442288 
05554      IF WS-REGIS-RTS-REGPLTNO NOT = WS-PLATE-NO-RTS                  CL**9
                                                                                
      D       DISPLAY '!TC186 - IF WS-REGIS-RTS-REGPLTNO <'                     
      D         WS-REGIS-RTS-REGPLTNO '> NOT = WS-PLATE-NO-RTS <'               
      D                                       WS-PLATE-NO-RTS '>'               
                                                                                
05555          MOVE WS-REGEFF-DT             TO WS-REGIS-PLTBIRTHDATE   P442288 
05556          MOVE ZERO                     TO WS-REGPLTAGE            P442288 
05557          IF TESTING-SHOW-DISPLAYS                                 P442288 
05558              DISPLAY ' NEW PLATES ASSIGNED - AGE=0'.              P442288 
05559                                                                   P442288 
05560      IF WS-REGIS-PLTBIRTHDATE > WS-CRNT-DATE-YYYYMM OR            P442288 
05561         WS-REGIS-PLTBIRTHDATE NOT NUMERIC           OR            P442288 
05562         WS-REGIS-PLTBIRTHDATE = ZERO                OR            P442288 
05563         WS-PLTBIRTHDATE-MM > 13                                   P442288 
                                                                                
      D       DISPLAY '!TC187 - IF WS-REGIS-PLTBIRTHDATE <'                     
      D         WS-REGIS-PLTBIRTHDATE '>  >  WS-CRNT-DATE-YYYYMM <'             
      D                                      WS-CRNT-DATE-YYYYMM '>'            
      D       DISPLAY 'OR WS-REGIS-PLTBIRTHDATE <'                              
      D                   WS-REGIS-PLTBIRTHDATE '> NOT NUMERIC'                 
      D       DISPLAY 'OR WS-REGIS-PLTBIRTHDATE <'                              
      D                   WS-REGIS-PLTBIRTHDATE '> = ZERE'                      
      D       DISPLAY 'OR WS-PLTBIRTHDATE-MM <'                                 
      D                   WS-PLTBIRTHDATE-MM '>  >  13'                         
                                                                                
05564          MOVE WS-CRNT-DATE-YYYYMM  TO WS-REGIS-PLTBIRTHDATE       P442288 
05565          MOVE 0                    TO WS-REGPLTAGE                P442288 
05566      END-IF.                                                      P442288 
05567                                                                   P442288 
05568      COMPUTE WS-REGPLTAGE =                                       P442288 
05569          ((WS-CRNT-DATE-YYYY - WS-PLTBIRTHDATE-YYYY) * 12         P442288 
05570          + WS-CRNT-DATE-MM - WS-PLTBIRTHDATE-MM) / 12.            P442288 
05571      IF  WS-REGPLTAGE > 99                                        P442288 
                                                                                
      D    DISPLAY '!TC188 - IF  WS-REGPLTAGE <' WS-REGPLTAGE '>  >  99'        
                                                                                
05572              MOVE 99         TO WS-REGPLTAGE.                     P442288 
05573                                                                   P442288 
05574                                                                   P442288 
05575      IF TESTING-SHOW-DISPLAYS                                     P442288 
05576          DISPLAY ' OLD BDT: ' WS-REGIS-RTS-BIRTHDATE                 CL**9
05577                  ' NEW AGE: ' WS-REGPLTAGE                        P442288 
05578                  ' NEW BDT: ' WS-REGIS-PLTBIRTHDATE.              P442288 
05579                                                                   P442288 
05580      IF WS-REGEXPMO = 2                                           P442288 
                                                                                
      D    DISPLAY '!TC189 - IF WS-REGEXPMO <' WS-REGEXPMO '>  = 2'             
                                                                                
05581          MOVE 28                       TO WS-REGEXPDD             P442288 
05582      ELSE                                                         P442288 
05583      IF WS-REGEXPMO = 9 OR 4 OR 6 OR 11                           P442288 
                                                                                
      D    DISPLAY '!TC190 - IF WS-REGEXPMO <' WS-REGEXPMO                      
      D            '>  =  9 4 6 11'                                             
                                                                                
05584          MOVE 30                       TO WS-REGEXPDD             P442288 
05585      ELSE                                                         P442288 
                                                                                
      D    DISPLAY '!TC191 - IF WS-REGEXPMO <' WS-REGEXPMO                      
      D            '>  NOT =  2 9 4 6 11'                                       
                                                                                
05586          MOVE 31                       TO WS-REGEXPDD.            P442288 
05587                                                                   P442288 
05588 *-----------------------------------------------------------*     P442288 
05589 *GET CUSNAME                                                      P442288 
05590 *-----------------------------------------------------------*     P442288 
05591      IF  WRKRTS-OWNER-OPERATOR = 'SAME'                           P442288 
                                                                                
      D    DISPLAY '!TC192 - IF WRKRTS-OWNER-OPERATOR <'                        
      D                         WRKRTS-OWNER-OPERATOR '> = SAME'                
                                                                                
05592          MOVE WRKRTS-NAME-OF-APPLICANT TO WS-CUST-NAME            P442288 
05593      ELSE                                                         P442288 
                                                                                
      D    DISPLAY '!TC193 - IF NOT WRKRTS-OWNER-OPERATOR <'                    
      D                         WRKRTS-OWNER-OPERATOR '> = SAME'                
                                                                                
05594          MOVE WRKRTS-OWNER-OPERATOR    TO WS-CUST-NAME.           P442288 
05595         EJECT                                                     P442288 
05596                                                                   P442288 
05597      IF NO-ADABAS-WRITE-RUN                                       P442288 
05598         IF TESTING-SHOW-DISPLAYS                                  P442288 
05599              DISPLAY ' BYPASS GENERATING RTS TRANSACTIONS '       P442288 
05600              DISPLAY ' BYPASS UPDATES TO IRP VEHICLE FILE '       P442288 
05601          END-IF                                                   P442288 
                                                                                
      D        DISPLAY '!TC194 - IF NO-ADABAS-WRITE-RUN(N) <'                   
      D                             RTS-IRP-UPDATE-BYPASS-SW '>'                
                                                                                
05602          MOVE 'F'                  TO WRKRTS-PROCESSING-FLAG      P442288 
05603          PERFORM  Z-10-ARCHIVE-RTS-RECORDS                        P442288 
05604          ADD 1                     TO WS-RTS-NO-TRANS-BATCH       P442288 
05605          GO TO 2113-CREATE-RTS-TRANS-EXIT.                        P442288 
05606                                                                   P442288 
05607 *  ---------------------------------------------------------*     P442288 
05608 *   BUILD RTS FUND TRANSACTIONS                             *     P442288 
05609 *  ---------------------------------------------------------*     P442288 
05610                                                                   P442288 
05611      IF TESTING-SHOW-DISPLAYS                                     P442288 
05612          DISPLAY ' WE SHOULD BE GENERATING RTS TRANSACTIONS '     P442288 
05613      END-IF.                                                      P442288 
05614                                                                   P442288 
05615      IF TESTING-SHOW-DISPLAYS                                     P442288 
05616         DISPLAY '@ 2113-.. BEFORE GEN TRANS   '                   P442288 
05617            WRKRTS-TYPE-OF-CHANGE-FLAG  ' = ' WS-TRANSCD ' -- '    P442288 
05618            ' TITLE REG: ' WS-REGIS-RTS-REGPLTNO                      CL**9
05619            ' REGIS REG: '                                         P442288 
05620            WS-REGIS-RTS-REGPLTNO  '/' WS-REGIS-RTS-REG-EXP-DT '-'    CL**9
05621            ' MIN ' WS-MIN-BEGIN-REG-DT                            P442288 
05622            ' CUR ' WS-CRNT-REG-UPD-DT                             P442288 
05623         DISPLAY ' NEW AGE: ' WS-REGPLTAGE                         P442288 
05624            ' NEW BDT: ' WS-REGIS-PLTBIRTHDATE                     P442288 
05625            ' BATCH #: ' BATCH-NUMBER.                             P442288 
05626                                                                   P442288 
05627                                                                   P442288 
05628 *---------------------------------------------------------------* P442288 
05629 *        REGISTRATION FOR "TONLY" LICENSE PLATES WILL BE RENEWED* P442288 
05630 *        TO CURRENT DATE AND THEN THE LICENSE PLATE REPLACEMENT * P442288 
05631 *        TRANSACTION WILL BE WRITTEN - CURRENTLY BEING REJECTED * P442288 
05632 *        BECAUSE THE REGISTRATION WAS NOT CURRENTLY ACTIVE      * P442288 
05633 *        THEREFORE A PLATE EXCHANGE COULD NOT TAKE PLACE.       * P442288 
05634 *---------------------------------------------------------------* P442288 
05635      IF RTS-NOGEN-MV-FUNC                                            CL*11
05636          NEXT SENTENCE                                               CL*11
05637      ELSE                                                            CL*11
                                                                                
      D    DISPLAY '!TCXXO - IF WS-TRANSCD <' WS-TRANSCD '>  =  '               
      D      'C-TRANS-IRPXCH <' C-TRANS-IRPXCH '>'                              
      D    DISPLAY 'AND WRKRTS-TYPE-OF-CHANGE-FLAG <'                           
      D                 WRKRTS-TYPE-OF-CHANGE-FLAG '>  =  L'                    
      D    DISPLAY 'AND ((WS-REGIS-RTS-REGPLT5 <'                               
      D                   WS-REGIS-RTS-REGPLT5 '>  = TONLY OR T0NLY)'           
      D    DISPLAY 'OR (WS-REGIS-RTS-REGPLT5 = TONLY OR T0NLY))'                
      D    DISPLAY 'AND WS-REGIS-RTS-REG-EXP-DT <'                              
      D           WS-REGIS-RTS-REG-EXP-DT '>  <  WS-CRNT-REG-UPD-DT <'          
      D           WS-CRNT-REG-UPD-DT '>'                                        
                                                                                
05638      IF WS-TRANSCD = C-TRANS-IRPXCH                    AND        P442288 
05639         WRKRTS-TYPE-OF-CHANGE-FLAG  = 'L'              AND        P442288 
05640         ((WS-REGIS-RTS-REGPLT5 = 'TONLY' OR 'T0NLY') OR              CL**9
05641          (WS-REGIS-RTS-REGPLT5 = 'TONLY' OR 'T0NLY'))  AND           CL**9
05642         WS-REGIS-RTS-REG-EXP-DT  <  WS-CRNT-REG-UPD-DT               CL**9
                                                                                
      D    DISPLAY '!TC196 - IF WS-TRANSCD <' WS-TRANSCD '>  =  '               
      D      'C-TRANS-IRPXCH <' C-TRANS-IRPXCH '>'                              
      D    DISPLAY 'AND WRKRTS-TYPE-OF-CHANGE-FLAG <'                           
      D                 WRKRTS-TYPE-OF-CHANGE-FLAG '>  =  L'                    
      D    DISPLAY 'AND ((WS-REGIS-RTS-REGPLT5 <'                               
      D                   WS-REGIS-RTS-REGPLT5 '>  = TONLY OR T0NLY)'           
      D    DISPLAY 'OR (WS-REGIS-RTS-REGPLT5 = TONLY OR T0NLY))'                
      D    DISPLAY 'AND WS-REGIS-RTS-REG-EXP-DT <'                              
      D           WS-REGIS-RTS-REG-EXP-DT '>  <  WS-CRNT-REG-UPD-DT <'          
      D           WS-CRNT-REG-UPD-DT '>'                                        
                                                                                
05643 *---------------------------------------------------------------* P442288 
05644 *         SUZANNE SUGGESTS CHANGING THE IRPXCH TO IRPRNW INSTEAD* P442288 
05645 *---------------------------------------------------------------* P442288 
05646          MOVE C-TRANS-IRPRNW           TO  WS-TRANSCD             P442288 
05647          MOVE WS-REGIS-RTS-REGPLTNO    TO WS-PLATE-NO-RTS            CL**9
05648          PERFORM 21135-RTS-INV-FUNC-TRNS                          P442288 
05649          PERFORM 21136-RTS-INV-TR-DETAIL                          P442288 
05650          PERFORM 21137-RTS-MV-FUNC-TRANS                          P442288 
05651          PERFORM 2113A-RENEW-DIAGNOSTIC                           P442288 
05652          MOVE C-TRANS-IRPXCH           TO  WS-TRANSCD             P442288 
05653          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR  TO WS-PLATE-NO-RTS    P442288 
05654          ADD 1                         TO  WS-TRANSTIME.          P442288 
      D        DISPLAY 'ADD 1 TO WS-TRANSTIME <' WS-TRANSTIME '>'.              
05655                                                                   P442288 
05656                                                                   P442288 
05657 *---------------------------------------------------------------* P442288 
05658 ***  CHECK TO BYPASS RTS "FUNDS" TRANSACTION GENERATION:        * P442288 
05659 *    THIS CAN ALLOW SPECIAL BATCHES OF RTS REGISTRATION UPDATE  * P442288 
05660 *    TRANSACTIONS TO BE PRODUCED,  WITHOUT ALSO GENERATING THE  * P442288 
05661 *    ASSOCIATED RTS FUNDS TRANSACTIONS.  THIS CAN BE USED FOR   * P442288 
05662 *    A IRP / RTS SYNCHRONIZATION OF REGISTRATIONS PROCESS. A    * P442288 
05663 *    FUNCTION WHICH SHOULD NOT BE NECESSARY ONCE ALL OF THE     * P442288 
05664 *    COUNTIES ARE UP UNDER THE RTS ENVIRONMENT.                 * P442288 
05665 *---------------------------------------------------------------* P442288 
05666      IF RTS-NO-ERROR   AND                                        P442288 
05667         (RTS-SYNC-REG-UPDATE-RUN  OR                              P442288 
05668          BATCH-NUMBER    >  99000)                                P442288 
05669           NEXT SENTENCE                                           P442288 
05670      ELSE                                                         P442288 
05671 *---------------------------------------------------------------* P442288 
05672 *        THIS CODE COULD PROBABLY BE REDONE IN A MORE STRUCTURED* P442288 
05673 *        MANNER.  SEND CRB,CSF,REF,ATM FIRST AND THEN CHECK FOR * P442288 
05674 *        IRP VS FORESTRY.  ORDER OF WRITES SHOULDN'T MATTER.    * P442288 
05675 *---------------------------------------------------------------* P442288 
05676 *---------------------------------------------------------------*    CL**8
05677 *    BUILD TRANS FOR FUNDS DATA, IF PRESENT,                    *    CL**8
05678 *    BUT SKIP FORESTRY ACCOUNTS AND PLATE-ISSUE-LATE-XO         *    CL**8
05679 *---------------------------------------------------------------*    CL**8
05680      IF  RTS-NO-ERROR AND                                            CL**6
05681          NOT WRKRTS-FOREST-ACCT          AND                         CL**8
05682          WRKRTS-TYPE-OF-CHANGE-FLAG NOT = 'P' AND                    CL**8
05683 *---------------------------------------------------------------*    CL**8
05684 *        NOT WRKRTS-PLATE-ISSUE-LATE-XO  AND                    *    CL**8
05685 *---------------------------------------------------------------*    CL**8
05686          (WS-ITMPRICE          > 0.0  OR                             CL**7
05687          (WS-ITMPRICE          = 0.0  AND                            CL**7
05688           WRKRTS-SUPP-NBR  NOT = 000  AND                            CL**7
05689           WRKRTS-ACT-TYPE  NOT = 'R'  AND                            CL**7
05690           WRKRTS-ACT-TYPE  NOT = '5'  AND                            CL**7
05691           WRKRTS-ACT-TYPE  NOT = 'T') )                              CL**7
                                                                                
      D    DISPLAY '!TC201 - IF RTS-NO-ERROR(0) <'                              
      D                         RTS-TRANS-ERROR-FLAG '>'                        
      D            'AND NOT WRKRTS-FOREST-ACCT(F) <'                            
      D                     WRKRTS-ACT-TYPE '>'                                 
      D    DISPLAY 'AND WRKRTS-TYPE-OF-CHANGE-FLAG <'                           
      D            WRKRTS-TYPE-OF-CHANGE-FLAG '> NOT = P'                       
      D    DISPLAY 'AND (WS-ITMPRICE <' WS-ITMPRICE '>  > 0.0'                  
      D    DISPLAY 'AND (WS-ITMPRICE = 0.0'                                     
      D    DISPLAY 'AND  WRKRTS-SUPP-NBR <' WRKRTS-SUPP-NBR '> NOT = 0'         
      D    DISPLAY 'AND  WRKRTS-ACT-TYPE <' WRKRTS-ACT-TYPE '> NOT '            
      D            ' = A R T ))'                                                
      D                                                                         
                                                                                
05692          PERFORM  21132-RTS-FUND-FUNC-TRNS                        P442288 
05693          IF WRKRTS-REFLECT-PORTION  > ZERO OR                     P442288 
05694             WRKRTS-AUTOMATE-PORTION > ZERO OR                     P442288 
05695             WRKRTS-INS-PORTION      > ZERO OR                     P442288 
05696             WRKRTS-CNTY-MOBILITY-PORTION > ZERO OR                   CL*14
05697             WRKRTS-TERP-PORTION     > ZERO                        P442288 
05698 *---------------------------------------------------------------*    CL*14
05699 *        OR WRKRTS-MOBILITY-PORTION > ZERO                      *    CL*14
05700 *---------------------------------------------------------------*    CL*14
05701              SUBTRACT WRKRTS-REFLECT-PORTION  FROM WS-ITMPRICE    P442288 
05702              SUBTRACT WRKRTS-AUTOMATE-PORTION FROM WS-ITMPRICE    P442288 
05703              SUBTRACT WRKRTS-INS-PORTION      FROM WS-ITMPRICE    P442288 
05704              SUBTRACT WRKRTS-TERP-PORTION     FROM WS-ITMPRICE    P442288 
05705 *---------------------------------------------------------------*    CL*15
05706 *--------    DO NOT SUBTRACT ADD ON FEES          --------------*    CL*15
05707 *            SUBTRACT WRKRTS-CNTY-MOBILITY-PORTION              *    CL*15
05708 *                                             FROM WS-ITMPRICE  *    CL*15
05709 *---------------------------------------------------------------*    CL*15
05710          END-IF                                                   P442288 
05711          PERFORM  21133-RTS-FUND-TR-DETAIL                        P442288 
05712          PERFORM  21134-TRANSMITTAL-RPT-DATA                      P442288 
05713          IF WRKRTS-REFLECT-PORTION  > ZERO                        P442288 
                                                                                
      D        DISPLAY '!TC205 - IF WRKRTS-REFLECT-PORTION <'                   
      D                             WRKRTS-REFLECT-PORTION '>  > 0'             
                                                                                
05714              MOVE WRKRTS-REFLECT-PORTION   TO WS-ITMPRICE         P442288 
05715              MOVE C-ACCT-ITMCD-REFLECT     TO WS-ACCTITMCD        P442288 
05716              PERFORM  21133-RTS-FUND-TR-DETAIL                    P442288 
05717              PERFORM  21134-TRANSMITTAL-RPT-DATA                  P442288 
05718          END-IF                                                   P442288 
05719          IF WRKRTS-AUTOMATE-PORTION > ZERO                        P442288 
                                                                                
      D        DISPLAY '!TC206 - IF WRKRTS-AUTOMATE-PORTION <'                  
      D                             WRKRTS-AUTOMATE-PORTION '>  > 0'            
                                                                                
05720              MOVE WRKRTS-AUTOMATE-PORTION  TO WS-ITMPRICE         P442288 
05721              MOVE C-ACCT-ITMCD-AUTOMATE    TO WS-ACCTITMCD        P442288 
05722              PERFORM  21133-RTS-FUND-TR-DETAIL                    P442288 
05723              PERFORM  21134-TRANSMITTAL-RPT-DATA                  P442288 
05724          END-IF                                                   P442288 
05725          IF WRKRTS-INS-PORTION      > ZERO                        P442288 
                                                                                
      D        DISPLAY '!TC207 - IF WRKRTS-INS-PORTION <'                       
      D                             WRKRTS-INS-PORTION '>  > 0'                 
                                                                                
05726              MOVE WRKRTS-INS-PORTION       TO WS-ITMPRICE         P442288 
05727              MOVE C-ACCT-ITMCD-INS         TO WS-ACCTITMCD        P442288 
05728              PERFORM  21133-RTS-FUND-TR-DETAIL                    P442288 
05729              PERFORM  21134-TRANSMITTAL-RPT-DATA                  P442288 
05730          END-IF                                                   P442288 
05731          IF WRKRTS-TERP-PORTION     > ZERO                        P442288 
                                                                                
      D        DISPLAY '!TC208 - IF WRKRTS-TERP-PORTION <'                      
      D                             WRKRTS-TERP-PORTION '>  > 0'                
                                                                                
05732              MOVE WRKRTS-TERP-PORTION     TO WS-ITMPRICE          P442288 
05733              MOVE C-ACCT-ITMCD-SB005       TO WS-ACCTITMCD        P442288 
05734              PERFORM  21133-RTS-FUND-TR-DETAIL                    P442288 
05735              PERFORM  21134-TRANSMITTAL-RPT-DATA                  P442288 
05736          END-IF                                                   P442288 
05737          IF WRKRTS-CNTY-MOBILITY-PORTION > ZERO                      CL*14
                                                                                
      D        DISPLAY '!TC209 - IF WRKRTS-CNTY-MOBILITY-PORTION <'             
      D                             WRKRTS-CNTY-MOBILITY-PORTION '> > 0'        
                                                                                
05738              MOVE WRKRTS-CNTY-MOBILITY-PORTION TO WS-ITMPRICE        CL*14
05739              MOVE C-ACCT-ITMCD-MOBILITY        TO WS-ACCTITMCD       CL*14
05740              PERFORM  21133-RTS-FUND-TR-DETAIL                       CL*14
05741              PERFORM  21134-TRANSMITTAL-RPT-DATA                     CL*14
05742          END-IF                                                      CL*14
05743          IF WRKRTS-CNTY-CHILD-SAFE-PORTION > ZERO                    CL*14
                                                                                
      D        DISPLAY '!TC210 - IF WRKRTS-CNTY-CHILD-SAFE-PORTION <'           
      D                   WRKRTS-CNTY-CHILD-SAFE-PORTION '> > 0'                
                                                                                
05744             MOVE C-ACCT-ITMCD-CSF      TO WS-ACCTITMCD               CL*14
05745             MOVE WRKRTS-CNTY-CHILD-SAFE-PORTION TO WS-ITMPRICE       CL*14
05746             PERFORM  21133-RTS-FUND-TR-DETAIL                        CL*14
05747             PERFORM  21134-TRANSMITTAL-RPT-DATA                      CL*14
05748          END-IF                                                      CL*14
                                                                                
      D        DISPLAY '!TC211 - 2113-CREATE-RTS-TRANSACTIONS'                  
                                                                                
05749          MOVE WRKRTS-CNTY-ADD-ON-FEE             TO WS-ITMPRICE      CL*14
05750          SUBTRACT WRKRTS-CNTY-MOBILITY-PORTION   FROM WS-ITMPRICE    CL*14
05751          SUBTRACT WRKRTS-CNTY-CHILD-SAFE-PORTION FROM WS-ITMPRICE    CL*14
05752          MOVE C-ACCT-ITMCD-CRBF                  TO WS-ACCTITMCD     CL*14
05753          PERFORM  21133-RTS-FUND-TR-DETAIL                           CL*14
05754          PERFORM  21134-TRANSMITTAL-RPT-DATA                         CL*14
05755      ELSE                                                         P442288 
05756      IF  RTS-NO-ERROR                                             P442288 
05757      AND WRKRTS-TYPE-OF-CHANGE-FLAG NOT = 'P'                        CL**8
05758          IF WRKRTS-CNTY-ADD-ON-FEE NOT = ZERO                     P442288 
                                                                                
      D    DISPLAY '!TC212 - IF RTS-NO-ERROR(0) <'                              
      D                         RTS-TRANS-ERROR-FLAG '>'                        
      D            'AND WRKRTS-TYPE-OF-CHANGE-FLAG <'                           
      D            WRKRTS-TYPE-OF-CHANGE-FLAG '> NOT = P'                       
      D    DISPLAY 'IF WRKRTS-CNTY-ADD-ON-FEE <'                                
      D                WRKRTS-CNTY-ADD-ON-FEE '> NOT = 0'                       
                                                                                
05759              PERFORM  21132-RTS-FUND-FUNC-TRNS                    P442288 
05760          IF WRKRTS-REFLECT-PORTION  > ZERO                        P442288 
                                                                                
      D    DISPLAY '!TC213 - IF WRKRTS-REFLECT-PORTION <'                       
      D                         WRKRTS-REFLECT-PORTION '>  > 0'                 
                                                                                
05761              MOVE WRKRTS-REFLECT-PORTION   TO WS-ITMPRICE         P442288 
05762              MOVE C-ACCT-ITMCD-REFLECT     TO WS-ACCTITMCD        P442288 
05763              PERFORM  21133-RTS-FUND-TR-DETAIL                    P442288 
05764              PERFORM  21134-TRANSMITTAL-RPT-DATA                  P442288 
05765          END-IF                                                   P442288 
05766          IF WRKRTS-AUTOMATE-PORTION > ZERO                        P442288 
                                                                                
      D    DISPLAY '!TC214 - IF WRKRTS-AUTOMATE-PORTION <'                      
      D                         WRKRTS-AUTOMATE-PORTION '>  > 0'                
                                                                                
05767              MOVE WRKRTS-AUTOMATE-PORTION  TO WS-ITMPRICE         P442288 
05768              MOVE C-ACCT-ITMCD-AUTOMATE    TO WS-ACCTITMCD        P442288 
05769              PERFORM  21133-RTS-FUND-TR-DETAIL                    P442288 
05770              PERFORM  21134-TRANSMITTAL-RPT-DATA                  P442288 
05771          END-IF                                                   P442288 
05772          IF WRKRTS-INS-PORTION      > ZERO                        P442288 
                                                                                
      D    DISPLAY '!TC215 - IF WRKRTS-INS-PORTION <'                           
      D                         WRKRTS-INS-PORTION '>  > 0'                     
                                                                                
05773              MOVE WRKRTS-INS-PORTION       TO WS-ITMPRICE         P442288 
05774              MOVE C-ACCT-ITMCD-INS         TO WS-ACCTITMCD        P442288 
05775              PERFORM  21133-RTS-FUND-TR-DETAIL                    P442288 
05776              PERFORM  21134-TRANSMITTAL-RPT-DATA                  P442288 
05777          END-IF                                                   P442288 
05778          IF WRKRTS-TERP-PORTION     > ZERO                        P442288 
                                                                                
      D    DISPLAY '!TC216 - IF WRKRTS-TERP-PORTION <'                          
      D                         WRKRTS-TERP-PORTION '>  > 0'                    
                                                                                
05779              MOVE WRKRTS-TERP-PORTION      TO WS-ITMPRICE         P442288 
05780              MOVE C-ACCT-ITMCD-SB005       TO WS-ACCTITMCD        P442288 
05781              PERFORM  21133-RTS-FUND-TR-DETAIL                    P442288 
05782              PERFORM  21134-TRANSMITTAL-RPT-DATA                  P442288 
05783          END-IF                                                   P442288 
05784          IF WRKRTS-CNTY-MOBILITY-PORTION > ZERO                      CL*14
                                                                                
      D    DISPLAY '!TC217 - IF WRKRTS-CNTY-MOBILITY-PORTION <'                 
      D                         WRKRTS-CNTY-MOBILITY-PORTION '>  > 0'           
                                                                                
05785              MOVE WRKRTS-CNTY-MOBILITY-PORTION TO WS-ITMPRICE        CL*14
05786              MOVE C-ACCT-ITMCD-MOBILITY        TO WS-ACCTITMCD       CL*14
05787              PERFORM  21133-RTS-FUND-TR-DETAIL                       CL*14
05788              PERFORM  21134-TRANSMITTAL-RPT-DATA                     CL*14
05789          END-IF                                                      CL*14
05790          IF WRKRTS-CNTY-CHILD-SAFE-PORTION > ZERO                    CL*14
                                                                                
      D    DISPLAY '!TC218 - IF WRKRTS-CNTY-CHILD-SAFE-PORTION <'               
      D                         WRKRTS-CNTY-CHILD-SAFE-PORTION '>  > 0'         
                                                                                
05791             MOVE C-ACCT-ITMCD-CSF      TO WS-ACCTITMCD               CL*14
05792             MOVE WRKRTS-CNTY-CHILD-SAFE-PORTION TO WS-ITMPRICE       CL*14
05793             PERFORM  21133-RTS-FUND-TR-DETAIL                        CL*14
05794             PERFORM  21134-TRANSMITTAL-RPT-DATA                      CL*14
05795          END-IF                                                      CL*14
                                                                                
      D    DISPLAY '!TC219 - 2113-CREATE-RTS-TRANSACTIONS'                      
                                                                                
05796          MOVE WRKRTS-CNTY-ADD-ON-FEE             TO WS-ITMPRICE      CL*14
05797          SUBTRACT WRKRTS-CNTY-MOBILITY-PORTION   FROM WS-ITMPRICE    CL*14
05798          SUBTRACT WRKRTS-CNTY-CHILD-SAFE-PORTION FROM WS-ITMPRICE    CL*14
05799          MOVE C-ACCT-ITMCD-CRBF        TO WS-ACCTITMCD               CL*14
05800          PERFORM  21133-RTS-FUND-TR-DETAIL                           CL*14
05801          PERFORM  21134-TRANSMITTAL-RPT-DATA.                        CL*14
05802                                                                   P442288 
05803 *  ---------------------------------------------------------*     P442288 
05804 *  (3.0)    BUILD INVENTORY TRANSACTIONS                    *     P442288 
05805 *           EXCEPT FOR WEIGHT CHANGE, MILEAGE CHANGE, OR AUDITS * P442288 
05806 *  ---------------------------------------------------------*     P442288 
05807      IF RTS-NOGEN-MV-FUNC                                            CL*11
05808          NEXT SENTENCE                                               CL*11
05809      ELSE                                                            CL*11
05810      IF   WRKRTS-TYPE-OF-CHANGE-FLAG  NOT = 'W'   AND             P442288 
05811           WRKRTS-TYPE-OF-CHANGE-FLAG  NOT = 'A'   AND             P442288 
05812           WRKRTS-TYPE-OF-CHANGE-FLAG  NOT = 'M'                   P442288 
05813         IF  RTS-NO-ERROR             AND                             CL**9
05814             WS-PLATE-NO-RTS NOT = SPACES                             CL**9
                                                                                
      D         DISPLAY '!TC220 - IF not RTS-NOGEN-MV-FUNC (3 THRU 5) <'        
      D                RTS-UPDT-ERROR-FLAG '>'                                  
      D          DISPLAY 'AND WRKRTS-TYPE-OF-CHANGE-FLAG NOT = W A M'           
      D          DISPLAY 'AND IF RTS-NO-ERROR(0) <'                             
      D                   RTS-TRANS-ERROR-FLAG '>'                              
      D          DISPLAY 'AND WS-PLATE-NO-RTS NOT = SPACES <'                   
      D                  WS-PLATE-NO-RTS '>'                                    
                                                                                
05815            PERFORM  21135-RTS-INV-FUNC-TRNS                       P442288 
05816            PERFORM  21136-RTS-INV-TR-DETAIL.                      P442288 
05817                                                                   P442288 
05818 *  ---------------------------------------------------------*     P442288 
05819 *   BUILD RTS_TRANS TRANSACTION TO TIE EVERYTHING TOGETHER      * P442288 
05820 *   BUILD MV_FUNC TRANSACTIONS MOST OF THE TIME                 * P442288 
05821 *      EXCEPT FOR MILEAGE CHANGE, OR AUDITS                     * P442288 
05822 *  ---------------------------------------------------------*     P442288 
05823                                                                   P442288 
05824      IF  (RTS-NOGEN-MV-FUNC  AND                                  P442288 
05825           RTS-NO-ERROR)               OR                          P442288 
05826          (RTS-NO-TITLE-DUMMY AND                                  P442288 
05827           RTS-NO-ERROR)                                           P442288 
                                                                                
      D         DISPLAY '!TC221 - IF (RTS-NOGEN-MV-FUNC (3 THRU 5) <'           
      D                RTS-UPDT-ERROR-FLAG '>'                                  
      D         DISPLAY 'AND RTS-NO-ERROR (0) <'                                
      D                   RTS-TRANS-ERROR-FLAG '> )'                            
      D         DISPLAY 'OR  (RTS-NO-TITLE-DUMMY (2) <'                         
      D                RTS-UPDT-ERROR-FLAG '> AND '                             
      D                 'AND IF RTS-NO-ERROR (0) <'                             
      D                   RTS-TRANS-ERROR-FLAG '> )'                            
                                                                                
05828           PERFORM Z-9-GEN-RTS-TRANS                               P442288 
05829           MOVE 'D'                 TO WRKRTS-PROCESSING-FLAG      P442288 
05830           PERFORM  Z-10-ARCHIVE-RTS-RECORDS                       P442288 
05831           ADD 1                    TO WS-RTS-NO-TRANS-BATCH       P442288 
05832      ELSE                                                         P442288 
05833      IF   RTS-INVALID-UPDATE                                      P442288 
                                                                                
      D         DISPLAY '!TC225 - IF RTS-INVALID-UPDATE (1 THRU 5) <'           
      D                RTS-UPDT-ERROR-FLAG '>'                                  
                                                                                
05834           PERFORM Z-9-GEN-RTS-TRANS                               P442288 
05835           MOVE 'E'                 TO WRKRTS-PROCESSING-FLAG      P442288 
05836           PERFORM  Z-10-ARCHIVE-RTS-RECORDS                       P442288 
05837           ADD 1                    TO WS-RTS-NO-TRANS-BATCH       P442288 
05838      ELSE                                                         P442288 
05839      IF   WRKRTS-TYPE-OF-CHANGE-FLAG      = 'A'   OR              P442288 
05840           WRKRTS-TYPE-OF-CHANGE-FLAG      = 'R'   OR              P442288 
05841           WRKRTS-TYPE-OF-CHANGE-FLAG      = 'M'                   P442288 
                                                                                
      D    DISPLAY '!TC226 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = A R M <'           
      D                              WRKRTS-TYPE-OF-CHANGE-FLAG '>'             
                                                                                
05842           PERFORM Z-9-GEN-RTS-TRANS                               P442288 
05843           PERFORM  Z-10-ARCHIVE-RTS-RECORDS                       P442288 
05844           ADD 1                    TO WS-RTS-NO-TRANS-BATCH       P442288 
05845      ELSE                                                         P442288 
05846      IF  RTS-NO-ERROR                                             P442288 
                                                                                
      D         DISPLAY '!TC227 - IF RTS-NO-ERROR(0) <'                         
      D                   RTS-TRANS-ERROR-FLAG '>'                              
                                                                                
05847           PERFORM  21137-RTS-MV-FUNC-TRANS                        P442288 
05848           MOVE 'F'                 TO WRKRTS-PROCESSING-FLAG      P442288 
05849           PERFORM  Z-10-ARCHIVE-RTS-RECORDS                       P442288 
05850           ADD 1                    TO WS-RTS-NO-TRANS-BATCH       P442288 
05851      ELSE                                                         P442288 
05852      IF RTS-CHANGE-FLAG                                           P442288 
05853          IF   WRKRTS-TYPE-OF-CHANGE-FLAG  NOT = 'M'               P442288 
                                                                                
      D         DISPLAY '!TC228 - IF RTS-CHANGE-FLAG(2) <'                      
      D                              RTS-TRANS-ERROR-FLAG '>'                   
      D                 'AND WRKRTS-TYPE-OF-CHANGE-FLAG  NOT = M <'             
      D                      WRKRTS-TYPE-OF-CHANGE-FLAG '>'                     
                                                                                
05854              MOVE 4           TO ERROR-PROCESSING-SW              P442288 
05855              MOVE ERROR-MSG-011-TEXT TO ERR-ERROR-MESSAGE-1       P442288 
05856              MOVE WRKRTS-TYPE-OF-CHANGE-FLAG TO                   P442288 
05857                                             ERR-TYPE-OF-CHANGE    P442288 
05858              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
05859          END-IF                                                   P442288 
           ELSE                                                                 
      D        DISPLAY '!TC229 - IF NOT RTS-CHANGE-FLAG(2) <'                   
      D                              RTS-TRANS-ERROR-FLAG '>'                   
                                                                                
05860          ADD 1               TO WS-RTS-NO-TRANS-BATCH             P442288 
05861          MOVE 'E'            TO WRKRTS-PROCESSING-FLAG            P442288 
05862          MOVE WS-CRNT-DATE-DT     TO WRKRTS-LAST-CHG-DT           P442288 
05863          PERFORM  Z-3-REWRITE-RTS-FILE                            P442288 
05864          PERFORM  Z-10-ARCHIVE-RTS-RECORDS                        P442288 
05865      END-IF.                                                      P442288 
05866                                                                   P442288 
05867      PERFORM Z-11-COMMIT-ADABAS.                                  P442288 
05868  2113-CREATE-RTS-TRANS-EXIT.                                      P442288 
05869      EXIT.                                                        P442288 
05870      EJECT                                                        P442288 
05871                                                                      CL*17
05872  21131-SETUP-TRANSCD-ETC.                                            CL*17
05873                                                                      CL*17
05874       IF WRKRTS-ACT-TYPE = 'A'                                       CL*17
                                                                                
      D         DISPLAY '!TC159 - IF WRKRTS-ACT-TYPE = A <'                     
      D                              WRKRTS-ACT-TYPE '>'                        
                                                                                
05875           PERFORM 211311-SETUP-APPORTIONED-TRANS                     CL*17
05876       ELSE                                                           CL*17
05877       IF WRKRTS-ACT-TYPE = 'E' OR                                    CL*17
05878          WRKRTS-ACT-TYPE = 'U'                                       CL*17
                                                                                
      D         DISPLAY '!TC160 - IF WRKRTS-ACT-TYPE = E OR U <'                
      D                              WRKRTS-ACT-TYPE '>'                        
                                                                                
05879           PERFORM 211312-SETUP-INTRA-FLEET-TRANS                     CL*17
05880       ELSE                                                           CL*17
                                                                                
      D         DISPLAY '!TC161 - IF WRKRTS-ACT-TYPE NOT = E U A <'             
      D                              WRKRTS-ACT-TYPE '>'                        
                                                                                
05881           PERFORM 211311-SETUP-APPORTIONED-TRANS.                    CL*17
05882                                                                      CL*17
05883  211311-SETUP-APPORTIONED-TRANS.                                     CL*17
05884 *---------------------------------------------------------------*    CL*17
05885 *    FORMERLY P442288 CODE LINES 05346 - 05472                  *    CL*17
05886 *---------------------------------------------------------------*    CL*17
05887                                                                      CL*17
05888 *  ---------------------------------------------------------*        CL*17
05889 *   SET CODES NEEDED TO GENERATED: TRANSCD, ACCTITMCD       *        CL*17
05890 *---------------------------------------------------------------*    CL*17
05891                                                                      CL*17
05892 *-----------------------------------------------------------*        CL*17
05893 *     CHECK FOR   AUDIT BILLING SUPPLEMENT                  *        CL*17
05894 *     CHECK THIS FIRST BECAUSE THE WRKRTS-TYPE-OF-CHANGE    *        CL*17
05895 *     WILL GENERALLY BE 'F' OR 'W'.                         *        CL*17
05896 *-----------------------------------------------------------*        CL*17
05897 *---------------------------------------------------------------*    CL*17
05898 *  WHERE IS THE FOLLOWING MILEAGE SUPLEMENT (POSSIBLE INCREASED *    CL*17
05899 *  FEES ? ACTUALLY TX FEE SHOULD NOT CHANGE - ONLY INCREASES FOR*    CL*17
05900 *  OTHER JURISDICTIONS BECAUSE THE (2) TYPE MILES (ESTIMATED)   *    CL*17
05901 *  ARE 100% + ADDITIONAL FEES FOR THE ADDED JURISDICTIONS.      *    CL*17
05902 *                                                               *    CL*17
05903 *    IF WRKRTS-MILEAGE-SUPPLEMENT-VEHL                          *    CL*17
05904 *---------------------------------------------------------------*    CL*17
05905         IF TESTING-SHOW-DISPLAYS                                     CL*17
05906             DISPLAY '@ 211311-SETUP-APPORTIONED-TRANS'.              CL*17
05907                                                                      CL*17
05908      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'K'                             CL*17
                                                                                
      D        DISPLAY '!TC162 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = K'             
      D                             WRKRTS-TYPE-OF-CHANGE-FLAG '>'              
                                                                                
05909          MOVE C-TRANS-IRPRNW          TO  WS-TRANSCD                 CL*17
05910          MOVE C-ACCT-ITMCD-IRPFND     TO  WS-ACCTITMCD               CL*17
05911          MOVE WRKRTS-APPORTIONED-FEE  TO  WS-ITMPRICE                CL*17
05912                                           WS-CUSTACTULREGFEE         CL*17
05913          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR                           CL*17
05914                                         TO WS-REGIS-RTS-REGPLTNO     CL*17
05915          MOVE WRKRTS-VEHICLE-YEAR       TO WS-VEHMODLYR              CL*17
05916          MOVE WRKRTS-MAKE-OF-VEHICLE    TO WS-VEHMK                  CL*17
05917          MOVE WRKRTS-IDENTIFICATION-NBR TO WS-VIN                    CL*17
05918      ELSE                                                            CL*17
05919      IF WRKRTS-AUDIT-SUPPLEMENT-VEHL                                 CL*17
05920      OR WRKRTS-TYPE-OF-CHANGE-FLAG = 'X'                             CL*17
                                                                                
      D        DISPLAY '!TC163 - IF WRKRTS-AUDIT-SUPPLEMENT-VEHL(A)'            
      D                'OR IF WRKRTS-TYPE-OF-CHANGE-FLAG = X'                   
      D                             WRKRTS-TYPE-OF-CHANGE-FLAG '>'              
                                                                                
05921          MOVE C-TRANS-IRPAUD            TO WS-TRANSCD                CL*17
05922          MOVE C-ACCT-ITMCD-AUDIT        TO WS-ACCTITMCD              CL*17
05923          MOVE WRKRTS-APPORTIONED-FEE    TO WS-ITMPRICE               CL*17
05924          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR                           CL*17
05925                                         TO WS-REGIS-RTS-REGPLTNO     CL*17
05926          MOVE WRKRTS-VEHICLE-YEAR       TO WS-VEHMODLYR              CL*17
05927          MOVE WRKRTS-MAKE-OF-VEHICLE    TO WS-VEHMK                  CL*17
05928          MOVE WRKRTS-IDENTIFICATION-NBR TO WS-VIN                    CL*17
05929      ELSE                                                            CL*17
05930      IF WRKRTS-TYPE-OF-CHANGE-FLAG  = 'W' OR 'M'                     CL*17
                                                                                
      D        DISPLAY '!TC164 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = W OR M'        
      D                ' <'         WRKRTS-TYPE-OF-CHANGE-FLAG '>'              
                                                                                
05931          MOVE C-TRANS-IRPAW           TO  WS-TRANSCD                 CL*17
05932          MOVE C-ACCT-ITMCD-AW         TO  WS-ACCTITMCD               CL*17
05933          MOVE WRKRTS-APPORTIONED-FEE  TO  WS-ITMPRICE                CL*17
05934      ELSE                                                            CL*17
05935 *-----------------------------------------------------------*        CL*17
05936 *     CHECK FOR   'RTS REPLACE PLATES'                               CL*17
05937 *-----------------------------------------------------------*        CL*17
05938      IF   WRKRTS-TYPE-OF-CHANGE-FLAG  = 'L'                          CL*17
                                                                                
      D        DISPLAY '!TC165 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = L <'           
      D                             WRKRTS-TYPE-OF-CHANGE-FLAG '>'              
                                                                                
05939           MOVE C-TRANS-IRPXCH          TO  WS-TRANSCD                CL*22
05940           MOVE C-ACCT-ITMCD-REPL1      TO  WS-ACCTITMCD              CL*22
05941           MOVE WRKRTS-REPLACEMENT-FEE  TO  WS-ITMPRICE               CL*22
05942      ELSE                                                            CL*17
05943 *-----------------------------------------------------------*        CL*17
05944 *     CHECK FOR   'RTS RENEWAL  '                                    CL*17
05945 *-----------------------------------------------------------*        CL*17
05946      IF WRKRTS-TYPE-OF-CHANGE-FLAG  = 'F' OR 'T' OR 'E' OR 'P'       CL*17
                                                                                
      D        DISPLAY '!TC166 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = F '            
      D                'T E P <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'                 
                                                                                
05947          MOVE C-TRANS-IRPRNW          TO  WS-TRANSCD                 CL*17
05948          MOVE C-ACCT-ITMCD-IRPFND     TO  WS-ACCTITMCD               CL*17
05949          MOVE WRKRTS-APPORTIONED-FEE  TO  WS-ITMPRICE                CL*17
05950                                           WS-CUSTACTULREGFEE         CL*17
05951      ELSE                                                            CL*17
05952 *-----------------------------------------------------------*        CL*17
05953 * (2.0)                 'OTHER TYPES NOT NEEDED BY RTS   '           CL*17
05954 *-----------------------------------------------------------*        CL*17
                                                                                
      D        DISPLAY '!TC167 - IF NOT WRKRTS-TYPE-OF-CHANGE-FLAG = F '        
      D              'L T E P K X W M <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'         
                                                                                
05955          MOVE 2                       TO RTS-TRANS-ERROR-FLAG        CL*17
05956          MOVE SPACES                  TO WS-TRANSCD                  CL*17
05957      END-IF.                                                         CL*17
05958                                                                      CL*17
05959 *-----------------------------------------------------------*        CL*17
05960 *     SET REG CLASS CODE & ITMCD    IN RTS_TR_INV_DETAIL RECORD *    CL*17
05961 *-----------------------------------------------------------*        CL*17
05962                                                                      CL*17
05963 *---------------------------------------------------------------*    CL*17
05964 *     IF  WS-TAG-POWER-UNIT-SUFFIX                              *    CL*17
05965 *---------------------------------------------------------------*    CL*17
05966       IF WRKRTS-ACT-TYPE = 'A' OR 'H' OR 'P' OR 'F'                  CL*17
05967 *---------------------------------------------------------------*    CL*17
05968 *          IF WS-TAG-FORESTRY-ACCOUNT                           *    CL*17
05969 *---------------------------------------------------------------*    CL*17
05970            IF WRKRTS-ACT-TYPE = 'F'                                  CL*17
                                                                                
      D        DISPLAY '!TC168 - IF WRKRTS-ACT-TYPE = A H P F AND '             
      D        'IF WRKRTS-ACT-TYPE = F <' WRKRTS-ACT-TYPE '>'                   
                                                                                
05971                MOVE C-TRANS-ITM-FOREST    TO WS-ITMCD                CL*17
05972                MOVE C-REGCLASSCD-FOREST   TO WS-REGCLASSCD           CL*17
05973            ELSE                                                      CL*17
05974 ***REG-CLASS 006 APPORTION                                      *    CL*24
05975               IF WRKRTS-ACT-TYPE = 'A'                               CL*24
05976                   IF TESTING-SHOW-DISPLAYS                           CL*24
05977                      DISPLAY SPACES                                  CL*24
05978                      DISPLAY                                         CL*24
05979                      '---211311-SETUP-APPORTIONED-TRANS---'          CL*24
05980                      DISPLAY 'WRKRTS-DOCNO ' WRKRTS-DOCNO            CL*24
05981                      DISPLAY 'WRKRTS-IDENTIFICATION-NBR '            CL*24
05982                               WRKRTS-IDENTIFICATION-NBR              CL*24
05983                      DISPLAY 'WRKRTS-FLEET-PLT-ITMCD '               CL*24
05984                               WRKRTS-FLEET-PLT-ITMCD                 CL*24
05985                      DISPLAY 'WRKRTS-FLEET-REGCL-CD '                CL*24
05986                               WRKRTS-FLEET-REGCL-CD                  CL*24
05987                   END-IF                                             CL*24
05988                   IF WRKRTS-FLEET-PLT-ITMCD NOT = SPACES             CL*24
                                                                                
      D        DISPLAY '!TC169 - IF WRKRTS-ACT-TYPE = A H P F AND '             
      D        'IF WRKRTS-ACT-TYPE = A <' WRKRTS-ACT-TYPE '>'                   
      D        DISPLAY 'IF WRKRTS-FLEET-PLT-ITMCD NOT = SPACES <'               
      D                    WRKRTS-FLEET-PLT-ITMCD '>'                           
                                                                                
05989                      MOVE WRKRTS-FLEET-PLT-ITMCD TO WS-ITMCD         CL*24
05990                      MOVE WRKRTS-FLEET-REGCL-CD  TO WS-REGCLASSCD    CL*24
05991                   ELSE                                               CL*24
                                                                                
      D        DISPLAY '!TC170 - IF WRKRTS-ACT-TYPE = A H P F AND '             
      D        'IF WRKRTS-ACT-TYPE = A <' WRKRTS-ACT-TYPE '>'                   
      D        DISPLAY 'IF WRKRTS-FLEET-PLT-ITMCD = SPACES <'                   
      D                    WRKRTS-FLEET-PLT-ITMCD '>'                           
                                                                                
05992                      MOVE C-TRANS-ITM-APP-TRUCK TO WS-ITMCD          CL*24
05993                      MOVE C-REGCLASSCD-IRP      TO WS-REGCLASSCD     CL*24
05994                   END-IF                                             CL*24
05995                END-IF                                                CL*24
05996       ELSE                                                           CL*17
05997 *---------------------------------------------------------------*    CL*17
05998 *        SPECIAL ACCOUNT NUMBERS INDICATE SPECIAL PROCESSING    *    CL*17
05999 *        ---------------------------------------------------    *    CL*17
06000 *        SEE NOTES ASSOCIATED WITH WS-IRP-ACCOUNT-NO-ANALYSIS   *    CL*17
06001 *        AND THE ASSOCIATGED "88" LEVELS AND USAGES             *    CL*17
06002 *---------------------------------------------------------------*    CL*17
06003 *---------------------------------------------------------------*    CL*17
06004 *         IF WS-TAG-5-YR-TKN-TR-ACCOUNT                         *    CL*17
06005 *---------------------------------------------------------------*    CL*17
06006 *      TRAILER FLEETS CHANGES GO HERE:                          *    CL*18
06007 *---------------------------------------------------------------*    CL*18
06008           IF (WRKRTS-ACT-TYPE = 'T' OR '5')                          CL*17
                                                                                
      D        DISPLAY '!TC171 - IF WRKRTS-ACT-TYPE = T OR 5 <'                 
      D                             WRKRTS-ACT-TYPE '>'                         
                                                                                
06009               PERFORM 2113111-NEW-TOKEN-TRLR-FLEETS                  CL*18
06010           ELSE                                                       CL*17
06011 *---------------------------------------------------------------*    CL*17
06012 *         IF WS-TAG-5-YR-APP-ACCOUNT                            *    CL*17
06013 *             MOVE C-TRANS-ITM-5-APP-TRLR TO WS-ITMCD           *    CL*17
06014 *             MOVE C-REGCLASSCD-5AT       TO WS-REGCLASSCD      *    CL*17
06015 *         ELSE                                                  *    CL*17
06016 *---------------------------------------------------------------*    CL*17
06017 *---------------------------------------------------------------*    CL*17
06018 *         IF WS-TAG-RENTAL-TRLR-ACCOUNT                         *    CL*17
06019 *---------------------------------------------------------------*    CL*17
06020           IF (WRKRTS-ACT-TYPE = 'R')                                 CL*17
                                                                                
      D             DISPLAY '!TC174 - IF WRKRTS-ACT-TYPE = R <'                 
      D                                  WRKRTS-ACT-TYPE '>'                    
                                                                                
06021 *---------------------------------------------------------------*    CL*17
06022 *        CHECK ON THIS: WOULD BOTH USE REGCLASS-CD 01 = RENT TRL*    CL*17
06023               MOVE C-REGCLASSCD-5RT       TO WS-REGCLASSCD           CL*17
06024 *---------------------------------------------------------------*    CL*17
06025 *        REMOVE TEMP USE OF 5 YR APP TRLR IN PLACE OF 5 RENT TRL*    CL*17
06026 *---------------------------------------------------------------*    CL*17
06027               IF WRKRTS-GROSS-WEIGHT < 4000                          CL*17
                                                                                
      D             DISPLAY '!TC175 - IF WRKRTS-GROSS-WEIGHT < 4000 <'          
      D                                  WRKRTS-GROSS-WEIGHT '>'                
                                                                                
06028                   MOVE C-TRANS-ITM-5-RNT-TRLR   TO WS-ITMCD          CL*17
06029                   MOVE C-REGCLASSCD-5RT         TO WS-REGCLASSCD     CL*17
06030               ELSE                                                   CL*17
                                                                                
      D             DISPLAY '!TC176 - IF WRKRTS-GROSS-WEIGHT >= 4000 <'         
      D                                  WRKRTS-GROSS-WEIGHT '>'                
                                                                                
06031                   MOVE C-TRANS-ITM-5-RTPHVY-TRLR TO WS-ITMCD         CL*17
06032                   MOVE C-REGCLASSCD-5RTPHVY     TO WS-REGCLASSCD     CL*17
06033               END-IF                                                 CL*17
06034           ELSE                                                       CL*17
                                                                                
      D             DISPLAY '!TC177 - IF WRKRTS-ACT-TYPE NOT = R <'             
      D                                  WRKRTS-ACT-TYPE '>'                    
                                                                                
06035               MOVE C-TRANS-ITM-APP-TRLR   TO WS-ITMCD                CL*17
06036               MOVE C-REGCLASSCD-IRP       TO WS-REGCLASSCD.          CL*17
06037                                                                      CL*17
06038  2113111-NEW-TOKEN-TRLR-FLEETS.                                      CL*18
06039                                                                      CL*18
06040      IF TESTING-SHOW-DISPLAYS                                        CL*19
06041             DISPLAY '@ 2113111-NEW-TOKEN-TRLR-FLEETS '               CL*19
06042                 'WRKRTS-FLEET ITMCD: >'                              CL*19
06043                  WRKRTS-FLEET-PLT-ITMCD '<'                          CL*19
06044      END-IF.                                                         CL*19
06045                                                                      CL*19
06046      IF WRKRTS-FLEET-PLT-ITMCD NOT = SPACES                          CL*18
                                                                                
      D        DISPLAY '!TC172 - IF WRKRTS-FLEET-PLT-ITMCD NOT = SPACES'        
      D         ' <'                WRKRTS-FLEET-PLT-ITMCD '>'                  
                                                                                
06047          MOVE WRKRTS-FLEET-PLT-ITMCD      TO WS-ITMCD                CL*18
06048          MOVE WRKRTS-FLEET-REGCL-CD       TO WS-REGCLASSCD           CL*18
06049      ELSE                                                            CL*18
                                                                                
      D        DISPLAY '!TC173 - IF WRKRTS-FLEET-PLT-ITMCD = SPACES'            
      D         ' <'                WRKRTS-FLEET-PLT-ITMCD '>'                  
                                                                                
06050          MOVE C-TRANS-ITM-5-TKN-TRLR      TO WS-ITMCD                CL*18
06051          MOVE C-REGCLASSCD-5TT            TO WS-REGCLASSCD           CL*18
06052      END-IF.                                                         CL*18
06053                                                                      CL*18
06054  211312-SETUP-INTRA-FLEET-TRANS.                                     CL*17
06055 *---------------------------------------------------------------*    CL*17
06056 *   INTRASTATE FLEETS - SETUP TRANSCT, ITMCD, ETC.              *    CL*17
06057 *---------------------------------------------------------------*    CL*17
06058                                                                      CL*17
06059 *---------------------------------------------------------------*    CL*17
06060 *   SET CODES NEEDED TO GENERATED: TRANSCD, ACCTITMCD           *    CL*17
06061 *---------------------------------------------------------------*    CL*17
06062         IF TESTING-SHOW-DISPLAYS                                     CL*17
06063             DISPLAY '@ 211312-SETUP-INTRA-FLEET-TRANS'.              CL*17
06064                                                                      CL*17
06065      IF WRKRTS-FLEET-PLT-ITMCD = SPACES                              CL*17
                                                                                
      D        DISPLAY '!TC178 - IF WRKRTS-FLEET-PLT-ITMCD = SPACES <'          
      D                             WRKRTS-FLEET-PLT-ITMCD '>'                  
                                                                                
06066          MOVE 'WXYZ'                  TO WRKRTS-FLEET-PLT-ITMCD.     CL*17
06067      IF WRKRTS-FLEET-REGCL-CD  NOT NUMERIC                           CL*17
                                                                                
      D        DISPLAY '!TC179 - IF WRKRTS-FLEET-REGCL-CD NOT NUMERIC <'        
      D                             WRKRTS-FLEET-REGCL-CD '>'                   
                                                                                
06068          MOVE 999                     TO WRKRTS-FLEET-REGCL-CD.      CL*17
06069                                                                      CL*17
06070      IF WRKRTS-TYPE-OF-CHANGE-FLAG = 'K'                             CL*17
                                                                                
      D        DISPLAY '!TC180 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = K <'           
      D                             WRKRTS-TYPE-OF-CHANGE-FLAG '>'              
                                                                                
06071          MOVE C-TRANS-IRPRNW          TO  WS-TRANSCD                 CL*17
06072          MOVE C-ACCT-ITMCD-IRPFND     TO  WS-ACCTITMCD               CL*17
06073          MOVE WRKRTS-APPORTIONED-FEE  TO  WS-ITMPRICE                CL*17
06074                                           WS-CUSTACTULREGFEE         CL*17
06075          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR                           CL*17
06076                                         TO WS-REGIS-RTS-REGPLTNO     CL*17
06077          MOVE WRKRTS-VEHICLE-YEAR       TO WS-VEHMODLYR              CL*17
06078          MOVE WRKRTS-MAKE-OF-VEHICLE    TO WS-VEHMK                  CL*17
06079          MOVE WRKRTS-IDENTIFICATION-NBR TO WS-VIN                    CL*17
06080      ELSE                                                            CL*17
06081      IF WRKRTS-TYPE-OF-CHANGE-FLAG  = 'W'                            CL*17
                                                                                
      D        DISPLAY '!TC181 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = W <'           
      D                             WRKRTS-TYPE-OF-CHANGE-FLAG '>'              
                                                                                
06082          MOVE X-TRANS-XFLAW           TO  WS-TRANSCD                 CL*17
06083          MOVE C-ACCT-ITMCD-AW         TO  WS-ACCTITMCD               CL*17
06084          MOVE WRKRTS-APPORTIONED-FEE  TO  WS-ITMPRICE                CL*17
06085      ELSE                                                            CL*17
06086 *-----------------------------------------------------------*        CL*17
06087 *     CHECK FOR   'RTS REPLACE PLATES'                               CL*17
06088 *-----------------------------------------------------------*        CL*17
06089      IF   WRKRTS-TYPE-OF-CHANGE-FLAG  = 'L'                          CL*17
                                                                                
      D        DISPLAY '!TC182 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = L <'           
      D                             WRKRTS-TYPE-OF-CHANGE-FLAG '>'              
                                                                                
06090           MOVE X-TRANS-XFLXCH          TO  WS-TRANSCD                CL*22
06091           MOVE C-ACCT-ITMCD-REPL1      TO  WS-ACCTITMCD              CL*22
06092           MOVE WRKRTS-REPLACEMENT-FEE  TO  WS-ITMPRICE               CL*22
06093      ELSE                                                            CL*17
06094 *-----------------------------------------------------------*        CL*17
06095 *     CHECK FOR   'RTS RENEWAL  '                                    CL*17
06096 *-----------------------------------------------------------*        CL*17
06097      IF WRKRTS-TYPE-OF-CHANGE-FLAG  = 'F' OR 'T' OR 'E' OR 'P'       CL*17
                                                                                
      D        DISPLAY '!TC183 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = F T '          
      D                'E P <'      WRKRTS-TYPE-OF-CHANGE-FLAG '>'              
                                                                                
06098          MOVE X-TRANS-XFLRNW          TO  WS-TRANSCD                 CL*17
06099          MOVE X-ACCT-ITMCD-FLEETFND   TO  WS-ACCTITMCD               CL*17
06100          MOVE WRKRTS-APPORTIONED-FEE  TO  WS-ITMPRICE                CL*17
06101                                           WS-CUSTACTULREGFEE         CL*17
06102      ELSE                                                            CL*17
06103 *-----------------------------------------------------------*        CL*17
06104 * (2.0)                 'OTHER TYPES NOT NEEDED BY RTS   '           CL*17
06105 *-----------------------------------------------------------*        CL*17
                                                                                
      D        DISPLAY '!TC184 - IF WRKRTS-TYPE-OF-CHANGE-FLAG = '              
      D                'F T E P <'  WRKRTS-TYPE-OF-CHANGE-FLAG '>'              
                                                                                
06106          MOVE 2                       TO RTS-TRANS-ERROR-FLAG        CL*17
06107          MOVE SPACES                  TO WS-TRANSCD                  CL*17
06108      END-IF.                                                         CL*17
06109                                                                      CL*17
06110 *-----------------------------------------------------------*        CL*17
06111 *     SET REG CLASS CODE & ITMCD    IN RTS_TR_INV_DETAIL RECORD *    CL*17
06112 *-----------------------------------------------------------*        CL*17
06113                                                                      CL*17
06114 *---------------------------------------------------------------*    CL*17
06115 *     IF  WS-TAG-POWER-UNIT-SUFFIX                              *    CL*17
06116 *---------------------------------------------------------------*    CL*17
06117 *---------------------------------------------------------------*    CL*24
06118 *MOST OF THE CODE REMAINING IN THIS PARAGRAPH IS DEAD CODE AS        CL*24
06119 *WRKRTS-ACT-TYPE MUST EQUAL 'E' OR 'U' TO ENTER THIS PARAGRAPH.      CL*24
06120 *---------------------------------------------------------------*    CL*24
06121       IF WRKRTS-ACT-TYPE = 'A' OR 'H' OR 'P' OR 'F'                  CL*17
06122 *---------------------------------------------------------------*    CL*17
06123 *          IF WS-TAG-FORESTRY-ACCOUNT                           *    CL*17
06124 *---------------------------------------------------------------*    CL*17
06125            IF WRKRTS-ACT-TYPE = 'F'                                  CL*17
06126                MOVE C-TRANS-ITM-FOREST    TO WS-ITMCD                CL*17
06127                MOVE C-REGCLASSCD-FOREST   TO WS-REGCLASSCD           CL*17
06128            ELSE                                                      CL*17
06129                MOVE C-TRANS-ITM-APP-TRUCK TO WS-ITMCD                CL*17
06130                MOVE C-REGCLASSCD-IRP      TO WS-REGCLASSCD           CL*17
06131       ELSE                                                           CL*17
06132 *---------------------------------------------------------------*    CL*17
06133 *        SPECIAL ACCOUNT NUMBERS INDICATE SPECIAL PROCESSING    *    CL*17
06134 *        ---------------------------------------------------    *    CL*17
06135 *        SEE NOTES ASSOCIATED WITH WS-IRP-ACCOUNT-NO-ANALYSIS   *    CL*17
06136 *        AND THE ASSOCIATGED "88" LEVELS AND USAGES             *    CL*17
06137 *---------------------------------------------------------------*    CL*17
06138 *---------------------------------------------------------------*    CL*17
06139 *         IF WS-TAG-5-YR-TKN-TR-ACCOUNT                         *    CL*17
06140 *---------------------------------------------------------------*    CL*17
06141           IF (WRKRTS-ACT-TYPE = 'T' OR '5')                          CL*17
06142               MOVE C-TRANS-ITM-5-TKN-TRLR TO WS-ITMCD                CL*17
06143               MOVE C-REGCLASSCD-5TT       TO WS-REGCLASSCD           CL*17
06144           ELSE                                                       CL*17
06145 *---------------------------------------------------------------*    CL*17
06146 *         IF WS-TAG-5-YR-APP-ACCOUNT                            *    CL*17
06147 *             MOVE C-TRANS-ITM-5-APP-TRLR TO WS-ITMCD           *    CL*17
06148 *             MOVE C-REGCLASSCD-5AT       TO WS-REGCLASSCD      *    CL*17
06149 *         ELSE                                                  *    CL*17
06150 *---------------------------------------------------------------*    CL*17
06151 *---------------------------------------------------------------*    CL*17
06152 *         IF WS-TAG-RENTAL-TRLR-ACCOUNT                         *    CL*17
06153 *---------------------------------------------------------------*    CL*17
06154           IF (WRKRTS-ACT-TYPE = 'R')                                 CL*17
06155 *---------------------------------------------------------------*    CL*17
06156 *        CHECK ON THIS: WOULD BOTH USE REGCLASS-CD 01 = RENT TRL*    CL*17
06157               MOVE C-REGCLASSCD-5RT       TO WS-REGCLASSCD           CL*17
06158 *---------------------------------------------------------------*    CL*17
06159 *        REMOVE TEMP USE OF 5 YR APP TRLR IN PLACE OF 5 RENT TRL*    CL*17
06160 *---------------------------------------------------------------*    CL*17
06161               IF WRKRTS-GROSS-WEIGHT < 4000                          CL*17
06162                   MOVE C-TRANS-ITM-5-RNT-TRLR   TO WS-ITMCD          CL*17
06163                   MOVE C-REGCLASSCD-5RT         TO WS-REGCLASSCD     CL*17
06164               ELSE                                                   CL*17
06165                   MOVE C-TRANS-ITM-5-RTPHVY-TRLR TO WS-ITMCD         CL*17
06166                   MOVE C-REGCLASSCD-5RTPHVY     TO WS-REGCLASSCD     CL*17
06167               END-IF                                                 CL*17
06168           ELSE                                                       CL*17
06169           IF WRKRTS-ACT-TYPE = 'E' OR                                CL*17
06170              WRKRTS-ACT-TYPE = 'U'                                   CL*17
06171               MOVE WRKRTS-FLEET-PLT-ITMCD TO WS-ITMCD                CL*17
06172               MOVE WRKRTS-FLEET-REGCL-CD  TO WS-REGCLASSCD           CL*17
06173           ELSE                                                       CL*17
06174               MOVE C-TRANS-ITM-APP-TRLR   TO WS-ITMCD                CL*17
06175               MOVE C-REGCLASSCD-IRP       TO WS-REGCLASSCD.          CL*17
06176      EJECT                                                           CL*17
06177  21132-RTS-FUND-FUNC-TRNS.                                        P442288 
06178                                                                   P442288 
                                                                                
      D    DISPLAY '!TC202 - 21132-RTS-FUND-FUNC-TRNS'                          
                                                                                
06179   EXEC ADABAS                                                     P442288 
06180       INSERT                                                      P442288 
06181       INTO  RTS_FUND_FUNC_TRNS    RTS-FUND                        P442288 
06182       SET   TRANSCD       = :WS-TRANSCD                           P442288 
06183             OFCISSUANCENO = :WS-OFCISSUANCENO                     P442288 
06184             OFCISSUANCECD = :WS-OFCISSUANCECD                     P442288 
06185             TRANSWSID     = :WS-TRANSWSID                         P442288 
06186             TRANSAMDATE   = :WS-TRANSAMDATE                       P442288 
06187             TRANSTIME     = :WS-TRANSTIME                         P442288 
06188             BSNDATE              = :WS-TRANSAMDATE                P442288 
06189             BSNDATETOTALAMT      = :C-TRANS-EMPTY-ZERO            P442288 
06190             TRANSEMPID           = :C-TRANS-EMPTY-SPACES          P442288 
06191             SUBSTAID             = :C-TRANS-EMPTY-ZERO            P442288 
06192             CASHWSID             = :C-TRANS-EMPTY-ZERO            P442288 
06193             VOIDTRANSCD          = :C-TRANS-EMPTY-SPACES          P442288 
06194             FUNDSADJREASN        = :C-TRANS-EMPTY-SPACES          P442288 
06195             VERSIONCD            = :C-TRANS-VERSIONCD             P442288 
06196             COMPTCNTYNO   = :WS-COMPT-COUNTY                      P442288 
06197       OPTIONS PASSWORD =    :WS-SCRT                              P442288 
06198               PREFIX = RTS-FUND-                                  P442288 
06199       END-EXEC                                                    P442288 
06200       .                                                           P442288 
06201                                                                   P442288 
06202      IF TESTING-SHOW-DISPLAYS                                     P442288 
06203          DISPLAY '@ 21132-RTS-FUND-FUNC-TRNS: '                   P442288 
06204                  'RESP=' SQLRSP ', QTY=' SQLQTY ', ISN=' SQLISN.  P442288 
06205                                                                   P442288 
06206      IF RESPONSE-CODE OF RTS-FUND > ZERO                          P442288 
06207          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
06208          MOVE 'FUND_FUNC_TRNS'     TO ERROR-MSG-014-FILE          P442288 
06209          MOVE RESPONSE-CODE OF RTS-FUND TO ERROR-MSG-014-ERRNUMB  P442288 
06210          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
06211          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
06212          MOVE 20                   TO WS-RETURN-CODE              P442288 
06213          PERFORM Z-Z-TERMINATION.                                 P442288 
06214                                                                   P442288 
06215  21133-RTS-FUND-TR-DETAIL.                                        P442288 
                                                                                
      D    DISPLAY '!TC203 - 21133-RTS-FUND-TR-DETAIL'                          
                                                                                
06216      IF TESTING-SHOW-DISPLAYS                                        CL*16
06217          DISPLAY '@ 21133-RTS-FUND-TR-DETAIL (BEF INSERT) : '        CL*16
06218                  'RESP=' SQLRSP, ' QTY=' SQLQTY, ' ISN=' SQLISN      CL*16
06219          DISPLAY '        ITMCD: ' WS-ACCTITMCD,                     CL*16
06220                  ' ITM$: ' WS-ITMPRICE,  ' TRNCD: ' WS-TRANSCD       CL*16
06221                  ' OFC: ' WS-OFCISSUANCENO.                          CL*16
06222                                                                      CL*16
06223 *                                                                 P442288 
06224   EXEC ADABAS                                                     P442288 
06225       INSERT                                                      P442288 
06226       INTO  RTS_TR_FDS_DETAIL     RTS-TR-FDS                      P442288 
06227       SET   TRANSCD       = :WS-TRANSCD                           P442288 
06228             OFCISSUANCENO = :WS-OFCISSUANCENO                     P442288 
06229             TRANSWSID     = :WS-TRANSWSID                         P442288 
06230             TRANSAMDATE   = :WS-TRANSAMDATE                       P442288 
06231             TRANSTIME     = :WS-TRANSTIME                         P442288 
06232             ACCTITMCD     = :WS-ACCTITMCD                         P442288 
06233             ITMPRICE      = :WS-ITMPRICE                          P442288 
06234             ITMQTY        = :C-TRANS-QUANTITY-1                   P442288 
06235       OPTIONS PASSWORD =    :WS-SCRT                              P442288 
06236               PREFIX = RTS-TR-FDS-                                P442288 
06237       END-EXEC                                                    P442288 
06238       .                                                           P442288 
06239                                                                   P442288 
06240      IF TESTING-SHOW-DISPLAYS                                     P442288 
06241          DISPLAY '@ 21133-RTS-FUND-TR-DETAIL: '                   P442288 
06242                  'RESP=' SQLRSP, ' QTY=' SQLQTY, ' ISN=' SQLISN      CL*14
06243          DISPLAY '        ITMCD: ' WS-ACCTITMCD,                     CL*14
06244                  ' ITM$: ' WS-ITMPRICE,  ' TRNCD: ' WS-TRANSCD       CL*14
06245                  ' OFC: ' WS-OFCISSUANCENO.                          CL*14
06246                                                                   P442288 
06247      IF RESPONSE-CODE OF RTS-TR-FDS > ZERO                        P442288 
06248          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
06249          MOVE 'TR_FDS_DETAIL '     TO ERROR-MSG-014-FILE          P442288 
06250          MOVE RESPONSE-CODE OF RTS-TR-FDS TO ERROR-MSG-014-ERRNUMBP442288 
06251          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
06252          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
06253          MOVE 20                   TO WS-RETURN-CODE              P442288 
06254          PERFORM Z-Z-TERMINATION.                                 P442288 
06255      EJECT                                                        P442288 
06256                                                                   P442288 
06257  21134-TRANSMITTAL-RPT-DATA.                                      P442288 
06258 ******************************************************************P442288 
06259 *  CREATE SEQUENTIAL FILE FOR RTS                                *P442288 
06260 ******************************************************************P442288 
06261                                                                   P442288 
                                                                                
      D    DISPLAY '!TC204 - 21134-TRANSMITTAL-RPT-DATA'                        
                                                                                
06262       MOVE WS-TRANSID              TO IRPRPT-TRANSID.             P442288 
06263       MOVE WS-COMPT-COUNTY         TO IRPRPT-CNTYNO.              P442288 
06264       MOVE WS-ITMPRICE             TO IRPRPT-ITMPRICE.            P442288 
06265       MOVE WS-ITMCD                TO IRPRPT-ITMCD.               P442288 
06266       MOVE WS-INVITMYR             TO IRPRPT-INVITMYR.            P442288 
06267       MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO IRPRPT-INVITMNO.       P442288 
06268 *---------------------------------------------------------------* P442288 
06269 *     THIS MAY NEED TO BE THE ORIGINAL IRP PLATE NUMBER IF IT   * P442288 
06270 *     CAUSES THE RTS TRANSMITTAL REPORT AND OTHERS TO ABEND     * P442288 
06271       MOVE WRKRTS-ACCOUNT-NBR      TO IRPRPT-IRPACCTNO.           P442288 
06272       MOVE WS-ACCTITMCD            TO IRPRPT-ACCTITMCD.           P442288 
06273       MOVE WRKRTS-FLEET-NBR        TO IRPRPT-FLEETNO.             P442288 
06274       MOVE WRKRTS-SUPP-NBR         TO IRPRPT-SUPPLMNTNO.          P442288 
06275       MOVE WRKRTS-VOUCHER-NUM      TO IRPRPT-VOUCHER-NUM             CL**4
06276       MOVE WRKRTS-LAST-CHG-DT      TO IRPRPT-TRANS-DATE.             CL**4
06277       WRITE  TRANSMITTAL-RPT-DATA  FROM IRPSYS-RPT-DATA.          P442288 
06278                                                                   P442288 
06279 *                                                                 P442288 
06280  21135-RTS-INV-FUNC-TRNS.                                         P442288 
                                                                                
      D    DISPLAY '!TC197 - 21135-RTS-INV-FUNC-TRNS'                           
                                                                                
06281   EXEC ADABAS                                                     P442288 
06282       INSERT                                                      P442288 
06283       INTO  RTS_INV_FUNC_TRANS    RTS-INV                         P442288 
06284       SET   TRANSCD       = :WS-TRANSCD                           P442288 
06285             OFCISSUANCENO = :WS-OFCISSUANCENO                     P442288 
06286             OFCISSUANCECD = :WS-OFCISSUANCECD                     P442288 
06287             TRANSWSID     = :WS-TRANSWSID                         P442288 
06288             TRANSAMDATE   = :WS-TRANSAMDATE                       P442288 
06289             TRANSTIME     = :WS-TRANSTIME                         P442288 
06290             VERSIONCD     = :C-TRANS-VERSIONCD                    P442288 
06291             SUBSTAID      = :C-TRANS-EMPTY-ZERO                   P442288 
06292             BSNDATE       = :WS-TRANSAMDATE                       P442288 
06293       OPTIONS PASSWORD =    :WS-SCRT                              P442288 
06294               PREFIX = RTS-INV-                                   P442288 
06295       END-EXEC                                                    P442288 
06296       .                                                           P442288 
06297                                                                   P442288 
06298      IF TESTING-SHOW-DISPLAYS                                     P442288 
06299          DISPLAY '@ 21135-RTS-INV-FUNC-TRNS: '                    P442288 
06300                  'RESP=' SQLRSP ', QTY=' SQLQTY ', ISN=' SQLISN.  P442288 
06301                                                                   P442288 
06302      IF RESPONSE-CODE OF RTS-INV > ZERO                           P442288 
06303          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
06304          MOVE 'INV_FUNC_TRANS'     TO ERROR-MSG-014-FILE          P442288 
06305          MOVE RESPONSE-CODE OF RTS-INV TO ERROR-MSG-014-ERRNUMB   P442288 
06306          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
06307          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
06308          MOVE 20                   TO WS-RETURN-CODE              P442288 
06309          PERFORM Z-Z-TERMINATION.                                 P442288 
06310                                                                   P442288 
06311 *                                                                 P442288 
06312  21136-RTS-INV-TR-DETAIL.                                         P442288 
                                                                                
      D    DISPLAY '!TC198 - 21136-RTS-INV-TR-DETAIL'                           
                                                                                
06313   EXEC ADABAS                                                     P442288 
06314       INSERT                                                      P442288 
06315       INTO  RTS_TR_INV_DETAIL     RTS-TR-INV                      P442288 
06316       SET   TRANSCD       = :WS-TRANSCD                           P442288 
06317             OFCISSUANCENO = :WS-OFCISSUANCENO                     P442288 
06318             TRANSWSID     = :WS-TRANSWSID                         P442288 
06319             TRANSAMDATE   = :WS-TRANSAMDATE                       P442288 
06320             TRANSTIME     = :WS-TRANSTIME                         P442288 
06321             ITMCD         = :WS-ITMCD                             P442288 
06322             INVITMYR      = :WS-INVITMYR                          P442288 
06323             INVITMNO      = :WS-PLATE-NO-RTS                      P442288 
06324             INVID         = :C-TRANS-EMPTY-SPACES                 P442288 
06325             DETAILSTATUSCD  = :C-TRANS-EMPTY-ZERO                 P442288 
06326       OPTIONS PASSWORD =    :WS-SCRT                              P442288 
06327               PREFIX = RTS-TR-INV-                                P442288 
06328       END-EXEC                                                    P442288 
06329       .                                                           P442288 
06330                                                                   P442288 
06331      IF WS-ITMCD = C-TRANS-ITM-5-FLT-TRLR                            CL*18
06332          PERFORM 211361-RTS-INV-WS-DETAIL                            CL*18
06333      END-IF.                                                         CL*18
06334                                                                      CL*18
06335      IF TESTING-SHOW-DISPLAYS                                     P442288 
06336          DISPLAY '@ 21136-RTS-INV-TR-DETAIL: '                    P442288 
06337                  'RESP=' SQLRSP ', QTY=' SQLQTY ', ISN=' SQLISN.  P442288 
06338                                                                   P442288 
06339      IF RESPONSE-CODE OF RTS-TR-INV > ZERO                        P442288 
06340          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
06341          MOVE 'TR_INV_DETAIL '     TO ERROR-MSG-014-FILE          P442288 
06342          MOVE RESPONSE-CODE OF RTS-TR-INV TO                         CL*18
06343                                       ERROR-MSG-014-ERRNUMB          CL*18
06344          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
06345          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
06346          MOVE 20                   TO WS-RETURN-CODE              P442288 
06347          PERFORM Z-Z-TERMINATION.                                 P442288 
06348                                                                      CL*18
06349 *                                                                    CL*18
06350  211361-RTS-INV-WS-DETAIL.                                           CL*18
06351                                                                      CL*18
                                                                                
      D    DISPLAY '!TC??? - 211361-RTS-INV-WS-DETAIL'                          
                                                                                
06352   EXEC ADABAS                                                        CL*18
06353       INSERT                                                         CL*18
06354       INTO  RTS_TR_INV_DETAIL     RTS-TR-WS-INV                      CL*18
06355       SET   TRANSCD       = :WS-TRANSCD                              CL*18
06356             OFCISSUANCENO = :WS-OFCISSUANCENO                        CL*18
06357             TRANSWSID     = :WS-TRANSWSID                            CL*18
06358             TRANSAMDATE   = :WS-TRANSAMDATE                          CL*18
06359             TRANSTIME     = :WS-TRANSTIME                            CL*18
06360             ITMCD         = :C-TRANS-ITM-US                          CL*18
06361             INVITMYR      = :WS-INVITMYR                             CL*18
06362             INVID         = :C-TRANS-EMPTY-SPACES                    CL*18
06363             DETAILSTATUSCD  = :C-TRANS-EMPTY-ZERO                    CL*18
06364       OPTIONS PASSWORD =    :WS-SCRT                                 CL*18
06365               PREFIX = RTS-TR-WS-INV-                                CL*18
06366       END-EXEC                                                       CL*18
06367       .                                                              CL*18
06368                                                                      CL*18
06369      IF TESTING-SHOW-DISPLAYS                                        CL*18
06370          DISPLAY '@ 211361-RTS-INV-WS-DETAIL: '                      CL*18
06371                  'RESP=' SQLRSP ', QTY=' SQLQTY ', ISN=' SQLISN.     CL*18
06372                                                                      CL*18
06373      IF RESPONSE-CODE OF RTS-TR-WS-INV > ZERO                        CL*18
06374          MOVE 3                    TO ERROR-PROCESSING-SW            CL*18
06375          MOVE 'TR_INV_DETAIL '     TO ERROR-MSG-014-FILE             CL*18
06376          MOVE RESPONSE-CODE OF RTS-TR-WS-INV TO                      CL*18
06377                                       ERROR-MSG-014-ERRNUMB          CL*18
06378          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1       CL*18
06379          PERFORM Z-4-CONTROL-EDIT-ERROR                              CL*18
06380          MOVE 20                   TO WS-RETURN-CODE                 CL*18
06381          PERFORM Z-Z-TERMINATION.                                    CL*18
06382                                                                      CL*18
06383      EJECT                                                        P442288 
06384                                                                   P442288 
06385 *  ---------------------------------------------------------*     P442288 
06386 *   BUILD MV_FUNC   TRANSACTIONS  AND RTS_TRANS  TRANSACTION*     P442288 
06387 *  ---------------------------------------------------------*     P442288 
06388                                                                   P442288 
06389  21137-RTS-MV-FUNC-TRANS.                                         P442288 
06390                                                                   P442288 
                                                                                
      D    DISPLAY '!TC199 - 21137-RTS-MV-FUNC-TRANS'                           
                                                                                
06391      IF TESTING-SHOW-DISPLAYS                                     P442288 
06392         DISPLAY '@ 21137-RTS-MV-FUNC-TRANS '                      P442288 
06393            WRKRTS-TYPE-OF-CHANGE-FLAG  ' = ' WS-TRANSCD           P442288 
06394            WS-REGIS-RTS-REGPLTNO  '/' WS-REGIS-RTS-REG-EXP-DT '-'    CL**9
06395            ' <-- ' WS-REGEXPYR '/' WS-REGEXPMO                    P442288 
06396            ' EFF ' WS-REGEFFDATE                                  P442288 
06397            ' NEW AGE: ' WS-REGPLTAGE                              P442288 
06398            ' NEW BDT: ' WS-REGIS-PLTBIRTHDATE                     P442288 
06399         DISPLAY ' WS-VEHGROSSWT              ' WS-VEHGROSSWT         CL*23
06400         DISPLAY ' RTS MV_FUNC OWNER DATA FOR ' WS-OWNRID          P442288 
06401         DISPLAY WS-OWNRTTLNAME1 ' ' WS-OWNRTTLNAME2               P442288 
06402         DISPLAY WS-OWNRST1 ' ' WS-OWNRST2                         P442288 
06403         DISPLAY WS-OWNRCITY ' ' WS-OWNRSTATE ' ' WS-OWNRZIPCODE   P442288 
06404         DISPLAY ' RTS MV_FUNC VEHL  DATA FOR ' WS-VIN ' ' WS-VEHMKP442288 
06405             ' DOC ' WS-RTS-DOCNO ' TIER ' WS-FILETIERCD           P442288 
06406         DISPLAY WS-TTLVEHLOCST1 ' ' WS-TTLVEHLOCST1               P442288 
06407         DISPLAY WS-TTLVEHLOCCITY ' ' WS-TTLVEHLOCSTATE ' '        P442288 
06408             WS-TTLVEHLOCZPCD                                      P442288 
06409         DISPLAY ' TRANS KEYS: '                                   P442288 
06410                 ' FOR TRANSCD='   WS-TRANSCD                      P442288 
06411         DISPLAY ' OFCISSNO='  WS-OFCISSUANCENO                    P442288 
06412                 ' TRANSWSID=' WS-TRANSWSID                        P442288 
06413                 ' AMDATE='    WS-TRANSAMDATE                      P442288 
06414                 ' TIME='      WS-TRANSTIME                        P442288 
06415                 ' OFCISSCD='  WS-OFCISSUANCECD                    P442288 
06416         DISPLAY '@ ADAWRT TIRETPCD: ' WS-TIRETYPECD               P442288 
06417      END-IF.                                                      P442288 
06418                                                                   P442288 
06419 *---------------------------------------------------------------* P442288 
06420 *       LATER VERSION WILL UPDATE REGFEE                        * P442288 
06421 *               CUSTACTULREGFEE  = :WS-CUSTACTULREGFEE          * P442288 
06422 *---------------------------------------------------------------* P442288 
06423      EXEC ADABAS                                                  P442288 
06424          INSERT                                                   P442288 
06425          INTO RTS_MV_FUNC_TRANS RTS-MV-TRANS                      P442288 
06426          SET    TRANSCD          = :WS-TRANSCD                    P442288 
06427                 OFCISSUANCENO    = :WS-OFCISSUANCENO              P442288 
06428                 TRANSWSID        = :WS-TRANSWSID                  P442288 
06429                 OFCISSUANCECD    = :WS-OFCISSUANCECD              P442288 
06430          TRANSAMDATE = :WS-TRANSAMDATE TRANSTIME = :WS-TRANSTIME  P442288 
06431          DOCNO = :WS-RTS-DOCNO                                    P442288 
06432          REGPLTNO = :WS-REGIS-RTS-REGPLTNO                           CL**9
06433          OWNRID = :WS-OWNRID           OWNRST (1) = :WS-OWNRST1   P442288 
06434          OWNRST (2) = :WS-OWNRST2      OWNRCITY = :WS-OWNRCITY    P442288 
06435          OWNRSTATE = :WS-OWNRSTATE     OWNRZPCD = :WS-OWNRZPCD    P442288 
06436          OWNRZPCDP4 = :WS-OWNRZPCDP4   OWNRCNTRY = :WS-OWNRCNTRY  P442288 
06437          VEHCLASSCD = :WS-VEHCLASSCD   DIESELINDI = :WS-DIESELINDIP442288 
06438          VIN = :WS-VIN                 VEHMODLYR = :WS-VEHMODLYR  P442288 
06439          VEHTON = :WS-VEHTON           VEHUNITNO = :WS-VEHUNITNO  P442288 
06440                 AUDITTRAILTRANSID = :WS-AUDITTRAILTRANSID         P442288 
06441                 VEHEMPTYWT       = :WS-VEHEMPTYWT                 P442288 
06442                 TTLVEHLOCST (1)  = :WS-TTLVEHLOCST1               P442288 
06443                 TTLVEHLOCST (2)  = :WS-TTLVEHLOCST2               P442288 
06444                 TTLVEHLOCCITY    = :WS-TTLVEHLOCCITY              P442288 
06445                 TTLVEHLOCSTATE   = :WS-TTLVEHLOCSTATE             P442288 
06446                 TTLVEHLOCZPCD    = :WS-TTLVEHLOCZPCD              P442288 
06447                 TTLVEHLOCZPCDP4  = :WS-TTLVEHLOCZPCDP4            P442288 
06448                 FILETIERCD       = :WS-FILETIERCD                 P442288 
06449                 REGSTKRCD        = :WS-REGSTKRCD                  P442288 
06450                 RESCOMPTCNTYNO   = :WS-COMPT-COUNTY               P442288 
06451          REGCLASSCD = :WS-REGCLASSCD   REGEFFDATE = :WS-REGEFFDATEP442288 
06452          REGEXPMO   = :WS-REGEXPMO     REGEXPYR   = :WS-REGEXPYR  P442288 
06453          REGPLTCD   = :WS-REGPLTCD                                P442288 
06454          PLTBIRTHDATE = :WS-REGIS-PLTBIRTHDATE                    P442288 
06455                 RECPNTNAME       = :WS-RECPNTNAME                 P442288 
06456                 HVYVEHUSETAXINDI = :WS-HVYVEHUSETAXINDI           P442288 
06457        PRIVACYOPTCD      = :WS-Z-IND DPSSAFTYSUSPNINDI = :WS-Z-INDP442288 
06458        REGINVLDINDI      = :WS-Z-IND REGHOTCKINDI      = :WS-Z-INDP442288 
06459        PLTSSEIZDINDI     = :WS-Z-IND STKRSEIZDINDI     = :WS-Z-INDP442288 
06460        RENWLMAILRTRNINDI = :WS-Z-IND REGREFAMT         = :WS-Z-AMTP442288 
06461        NOTFYNGCITY       = :WS-Z-FLD                              P442288 
06462                 BSNDATE          = :WS-TRANSAMDATE                P442288 
06463                 VERSIONCD        = :C-TRANS-VERSIONCD             P442288 
06464                 VEHGROSSWT       = :WS-VEHGROSSWT                    CL*23
06465                 TIRETYPECD       = :WS-TIRETYPECD                 P442288 
06466                 REGPLTOWNRNAME   = :WS-REGPLTOWNRNAME             P442288 
06467       OPTIONS PASSWORD =    :WS-SCRT                              P442288 
06468               PREFIX = RTS-MV-                                    P442288 
06469       END-EXEC                                                    P442288 
06470       .                                                           P442288 
06471                                                                   P442288 
06472      IF TESTING-SHOW-DISPLAYS                                     P442288 
06473           DISPLAY '@ 21137-RTS-MV-FUNC-TRANS: AFTER INSERT '      P442288 
06474                  'RESP=' SQLRSP ', QTY=' SQLQTY ', ISN=' SQLISN.  P442288 
06475                                                                   P442288 
06476      IF RESPONSE-CODE OF RTS-MV-TRANS > ZERO                      P442288 
06477        MOVE 3                    TO ERROR-PROCESSING-SW           P442288 
06478        MOVE 'MV_FUNC_TRANS '     TO ERROR-MSG-014-FILE            P442288 
06479        MOVE RESPONSE-CODE OF RTS-MV-TRANS TO ERROR-MSG-014-ERRNUMBP442288 
06480        MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1      P442288 
06481        PERFORM Z-4-CONTROL-EDIT-ERROR                             P442288 
06482          MOVE 20                   TO WS-RETURN-CODE              P442288 
06483        PERFORM Z-Z-TERMINATION.                                   P442288 
06484                                                                   P442288 
06485      PERFORM Z-9-GEN-RTS-TRANS.                                   P442288 
06486                                                                   P442288 
06487      EJECT                                                        P442288 
06488                                                                   P442288 
06489  21139-CANCEL-CURRENT-REGIS.                                      P442288 
06490                                                                   P442288 
                                                                                
      D    DISPLAY '21139-CANCEL-CURRENT-REGIS ENTRY'                           
                                                                                
06491      IF WRKRTS-ACT-TYPE = 'E' OR                                     CL*17
06492         WRKRTS-ACT-TYPE = 'U'                                        CL*17
                                                                                
      D        DISPLAY '!TC152 - IF WRKRTS-ACT-TYPE = E OR U <'                 
      D                             WRKRTS-ACT-TYPE '>'                         
                                                                                
06493          MOVE X-TRANS-XFLIVD       TO WS-TRANSCD                     CL*17
06494      ELSE                                                            CL*17
                                                                                
      D        DISPLAY '!TC153 - IF NOT WRKRTS-ACT-TYPE = E OR U <'             
      D                             WRKRTS-ACT-TYPE '>'                         
                                                                                
06495          MOVE C-TRANS-IRPIVD       TO WS-TRANSCD.                    CL*17
06496                                                                      CL*17
06497      MOVE WS-REGIS-RTS-REGCLASSCD  TO WS-REGCLASSCD.                 CL**9
06498 *---------------------------------------------------------------* P442288 
06499 *    MOVE ZEROS                    TO WS-REGEFFDATE             * P442288 
06500 *                                     WS-EXPDATE-N.             * P442288 
06501 *---------------------------------------------------------------* P442288 
06502                                                                   P442288 
06503      IF TESTING-SHOW-DISPLAYS                                     P442288 
06504         DISPLAY '@ 21139-CANCEL-CURRENT-REGIS FOR '               P442288 
06505                 WS-REGIS-RTS-REGPLTNO ' ON DOCNO ' WS-RTS-DOCNO      CL**9
06506         DISPLAY ' RTS MV_FUNC OWNER DATA FOR ' WS-OWNRID          P442288 
06507         DISPLAY WS-OWNRTTLNAME1 ' ' WS-OWNRTTLNAME2               P442288 
06508         DISPLAY WS-OWNRST1 ' ' WS-OWNRST2                         P442288 
06509         DISPLAY WS-OWNRCITY ' ' WS-OWNRSTATE ' ' WS-OWNRZIPCODE   P442288 
06510         DISPLAY ' RTS MV_FUNC VEHL  DATA FOR ' WS-VIN ' ' WS-VEHMKP442288 
06511         DISPLAY WS-TRANSCD ' FOR ' WS-REGIS-RTS-REGPLTNO ' '         CL**9
06512                 WS-REGEXPMO ' ' WS-REGEXPYR ' '                   P442288 
06513                 'REGINVLDINDI=' C-SET-REG-INVALID-INDI            P442288 
06514                 'INACTIV FLG ' WS-FILETIERCD                      P442288 
06515         DISPLAY ' TRANS KEYS: '                                   P442288 
06516                 ' FOR TRANSCD='   WS-TRANSCD                      P442288 
06517         DISPLAY ' OFCISSNO='  WS-OFCISSUANCENO                    P442288 
06518                 ' TRANSWSID=' WS-TRANSWSID                        P442288 
06519                 ' AMDATE='    WS-TRANSAMDATE                      P442288 
06520                 ' TIME='      WS-TRANSTIME                        P442288 
06521                 ' OFCISSCD='  WS-OFCISSUANCECD                    P442288 
06522      END-IF.                                                      P442288 
06523                                                                   P442288 
06524      IF NO-ADABAS-WRITE-RUN                                       P442288 
06525          NEXT SENTENCE                                            P442288 
06526      ELSE                                                         P442288 
06527      IF RTS-NORM-IRP-UPDATE-RUN OR                                P442288 
06528         RTS-SYNC-REG-UPDATE-RUN                                   P442288 
                                                                                
      D       DISPLAY '!TC154 - IF NOT NO-ADABAS-WRITE-RUN(N) <'                
      D                                RTS-IRP-UPDATE-BYPASS-SW '>'             
      D       DISPLAY 'AND IF RTS-NORM-IRP-UPDATE-RUN(N,T,M) <'                 
      D            '> AND RTS-NORM-IRP-UPDATE-RUN (N,T,M) <'                    
      D                   RTS-IF-RUN-TYPE '>'                                   
                                                                                
06529          PERFORM 211391-WRITE-IRPIVD-TRANS.                       P442288 
06530                                                                   P442288 
                                                                                
      D    DISPLAY '!TC155 - 21139-CANCEL-CURRENT-REGIS'                        
                                                                                
06531                                                                   P442288 
06532      MOVE 4                               TO ERROR-PROCESSING-SW. P442288 
06533      MOVE ZERO                            TO RTS-TRANS-ERROR-FLAG.P442288 
06534      MOVE ZERO                            TO RTS-UPDT-ERROR-FLAG. P442288 
06535      MOVE WS-REGIS-RTS-REGPLTNO           TO ERROR-MSG-016-PLT.      CL**9
06536      MOVE WS-REGIS-RTS-REG-EXP-DT         TO ERROR-MSG-016-EXP-DT.   CL**9
06537      MOVE WS-RTS-DOCNO                    TO ERROR-MSG-016-DOCNO. P442288 
06538      MOVE 'NOW INVALID'                   TO ERROR-MSG-016-NOW.   P442288 
06539      MOVE WS-PLATE-NO-RTS                 TO ERR-LICN.            P442288 
06540      IF RTS-IRPINV-FLAG = 'E'                                     P442288 
06541          MOVE 'EXCHANGED REG: '           TO ERROR-MSG-016-TYPE   P442288 
06542      ELSE                                                         P442288 
06543      IF RTS-IRPINV-FLAG = 'R'                                     P442288 
06544          MOVE 'REFUNDED  REG: '           TO ERROR-MSG-016-TYPE   P442288 
06545      ELSE                                                         P442288 
06546          MOVE 'CANCELLED REG: '           TO ERROR-MSG-016-TYPE.  P442288 
06547      MOVE ERROR-MSG-016-TEXT              TO ERR-ERROR-MESSAGE-1. P442288 
06548      PERFORM Z-V-WRITE-MVUPDT-REPORT.                             P442288 
06549      MOVE SPACES                          TO  ERR-LICN.           P442288 
                                                                                
      D    DISPLAY '21139-CANCEL-CURRENT-REGIS EXIT'.                           
                                                                                
06550                                                                   P442288 
06551  211391-WRITE-IRPIVD-TRANS.                                       P442288 
06552                                                                   P442288 
06553      EXEC ADABAS                                                  P442288 
06554          INSERT                                                   P442288 
06555          INTO RTS_MV_FUNC_TRANS RTS-XO-TRANS                      P442288 
06556          SET    TRANSCD           = :WS-TRANSCD                   P442288 
06557                 OFCISSUANCENO     = :WS-OFCISSUANCENO             P442288 
06558                 TRANSWSID         = :WS-TRANSWSID                 P442288 
06559                 OFCISSUANCECD     = :WS-OFCISSUANCECD             P442288 
06560                 TRANSAMDATE       = :WS-TRANSAMDATE               P442288 
06561                 TRANSTIME         = :WS-TRANSTIME                 P442288 
06562                 DOCNO             = :WS-RTS-DOCNO                 P442288 
06563                 REGPLTNO          = :WS-REGIS-RTS-REGPLTNO           CL**9
06564                 VIN               = :WS-VIN                       P442288 
06565                 AUDITTRAILTRANSID = :WS-AUDITTRAILTRANSID         P442288 
06566                 REGEXPMO          = :WS-REGEXPMO                  P442288 
06567                 REGEXPYR          = :WS-REGIS-RTS-REGEXPYR           CL**9
06568                 FILETIERCD        = :WS-FILETIERCD                P442288 
06569                 REGINVLDINDI      = :C-SET-REG-INVALID-INDI       P442288 
06570                 AUTHCD            = :C-TRANS-EMPTY-SPACES         P442288 
06571                 MFDWNCD           = :C-TRANS-EMPTY-ZERO           P442288 
06572                 SUBSTAID          = :C-TRANS-EMPTY-ZERO           P442288 
06573                 TRANSEMPID        = :C-TRANS-EMPTY-SPACES         P442288 
06574                 VOIDEDTRANSINDI   = :C-TRANS-EMPTY-ZERO           P442288 
06575                 BSNDATE           = :WS-TRANSAMDATE               P442288 
06576                 VERSIONCD         = :C-TRANS-VERSIONCD            P442288 
06577       OPTIONS PASSWORD            = :WS-SCRT                      P442288 
06578               PREFIX              = RTS-XO-                       P442288 
06579       END-EXEC                                                    P442288 
06580       .                                                           P442288 
06581                                                                   P442288 
06582 *---------------------------------------------------------------* P442288 
06583 *     THESE FIELDS NOT NEEDED FOR IRPINV CODE                   * P442288 
06584 *---------------------------------------------------------------* P442288 
06585 *                                                               * P442288 
06586 *        OWNRID = :WS-OWNRID           OWNRST (1) = :WS-OWNRST1 * P442288 
06587 *        OWNRST (2) = :WS-OWNRST2      OWNRCITY = :WS-OWNRCITY  * P442288 
06588 *        OWNRSTATE = :WS-OWNRSTATE     OWNRZPCD = :WS-OWNRZPCD  * P442288 
06589 *        OWNRZPCDP4 = :WS-OWNRZPCDP4   OWNRCNTRY = :WS-OWNRCNTRY* P442288 
06590 *        VEHCLASSCD = :WS-VEHCLASSCD   DIESELINDI = :WS-DIESELIN*IP442288 
06591 *        VEHTON = :WS-VEHTON           VEHUNITNO = :WS-VEHUNITNO* P442288 
06592 *               PRIVACYOPTCD     = :WS-PRIVACYOPTCD             * P442288 
06593 *               VEHEMPTYWT       = :WS-VEHEMPTYWT               * P442288 
06594 *               TTLVEHLOCST (1)  = :WS-TTLVEHLOCST1             * P442288 
06595 *               TTLVEHLOCST (2)  = :WS-TTLVEHLOCST2             * P442288 
06596 *               TTLVEHLOCCITY    = :WS-TTLVEHLOCCITY            * P442288 
06597 *               TTLVEHLOCSTATE   = :WS-TTLVEHLOCSTATE           * P442288 
06598 *               TTLVEHLOCZPCD    = :WS-TTLVEHLOCZPCD            * P442288 
06599 *               TTLVEHLOCZPCDP4  = :WS-TTLVEHLOCZPCDP4          * P442288 
06600 *               REGSTKRCD        = :WS-REGSTKRCD                * P442288 
06601 *               RESCOMPTCNTYNO   = :WS-COMPT-COUNTY             * P442288 
06602 *               REGCLASSCD       = :WS-REGCLASSCD               * P442288 
06603 *               REGEFFDATE       = :WS-REGEFFDATE               * P442288 
06604 *            :  REGPLTAGE REPLACED BY PLTBIRTHDATE              * P442288 
06605 *               REGPLTCD         = :WS-REGPLTCD                 * P442288 
06606 *               RECPNTNAME       = :WS-RECPNTNAME               * P442288 
06607 *               DPSSAFTYSUSPNINDI = :WS-DPSSAFTYSUSPNINDI       * P442288 
06608 *               HVYVEHUSETAXINDI = :WS-HVYVEHUSETAXINDI         * P442288 
06609 *               REGHOTCKINDI     = :WS-REGHOTCKINDI             * P442288 
06610 *               PLTSSEIZDINDI    = :WS-PLTSSEIZDINDI            * P442288 
06611 *               STKRSEIZDINDI    = :WS-STKRSEIZDINDI            * P442288 
06612 *               REGREFAMT        = :WS-REGREFAMT                * P442288 
06613 *               NOTFYNGCITY      = :WS-NOTFYNGCITY              * P442288 
06614 *               VEHGROSSWT       = :WRKRTS-GROSS-WEIGHT         * P442288 
06615 *               TIRETYPECD       = :WRKRTS-TIRE-TYPE-CD         * P442288 
06616 *               REGPLTOWNRNAME   = :WS-REGPLTOWNRNAME           * P442288 
06617 *---------------------------------------------------------------* P442288 
06618                                                                   P442288 
06619      IF TESTING-SHOW-DISPLAYS                                     P442288 
06620           DISPLAY '@ 211391-WRITE-IRPIVD-TRANS WITHIN '           P442288 
06621                   '@ 21139-CANCEL-CURRENT-REGIS: '                P442288 
06622                  'RESP=' SQLRSP ', QTY=' SQLQTY ', ISN=' SQLISN.  P442288 
06623                                                                   P442288 
06624      IF RESPONSE-CODE OF RTS-XO-TRANS > ZERO                      P442288 
06625        MOVE 3                    TO ERROR-PROCESSING-SW           P442288 
06626        MOVE 'MV_FUNC_TRANS '     TO ERROR-MSG-014-FILE            P442288 
06627        MOVE RESPONSE-CODE OF RTS-XO-TRANS TO ERROR-MSG-014-ERRNUMBP442288 
06628        MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1      P442288 
06629        PERFORM Z-4-CONTROL-EDIT-ERROR                             P442288 
06630        PERFORM Z-Z-TERMINATION.                                   P442288 
06631                                                                   P442288 
06632      PERFORM Z-9-GEN-RTS-TRANS.                                   P442288 
06633                                                                   P442288 
06634                                                                   P442288 
06635  2113A-RENEW-DIAGNOSTIC.                                          P442288 
06636                                                                   P442288 
                                                                                
      D    DISPLAY '!TC200 - 2113A-RENEW-DIAGNOSTIC'                            
                                                                                
06637      MOVE 4                               TO ERROR-PROCESSING-SW. P442288 
06638      MOVE ZERO                            TO RTS-TRANS-ERROR-FLAG.P442288 
06639      MOVE ZERO                            TO RTS-UPDT-ERROR-FLAG. P442288 
06640      MOVE WS-REGIS-RTS-REGPLTNO           TO ERROR-MSG-047-PLT.      CL**9
06641      MOVE WS-REGIS-RTS-REG-EXP-DT         TO ERROR-MSG-047-EXP-DT.   CL**9
06642      MOVE WS-REGIS-RTS-REG-EXP-DT         TO ERROR-MSG-047-NEW-EXP   CL**9
06643      MOVE WS-RTS-DOCNO                    TO ERROR-MSG-047-DOCNO. P442288 
06644      MOVE ERROR-MSG-047-TEXT              TO ERR-ERROR-MESSAGE-1. P442288 
06645      PERFORM Z-V-WRITE-MVUPDT-REPORT.                             P442288 
06646      MOVE SPACES                          TO  ERR-LICN.           P442288 
06647                                                                   P442288 
06648    EJECT                                                          P442288 
06649                                                                   P442288 
06650  2114-IRP-TO-RTS-LICE.                                            P442288 
06651                                                                   P442288 
06652 ******************************************************************P442288 
06653 * CONVERT LICENCE FROM  IRP FORMAT TO RTS FORMAT                 *P442288 
06654 ******************************************************************P442288 
06655                                                                   P442288 
06656 ***************************************************************** P442288 
06657 * CONVERT LICENCE FROM  IRP FORMAT TO RTS FORMAT ONLY FOR       * P442288 
06658 *  REGISTRATION YEARS PRIOR TO 1995.  1995 AND LATER IRP PLATES * P442288 
06659 *  WILL BE MANUFACTURED WITH THE FULL NUMBER, INCLUDING LEADING * P442288 
06660 *  ZEROS (AS THEY WERE IN 1993...A MINOR D12 REVERSAL).         * P442288 
06661 ***************************************************************** P442288 
06662 *                                                               * P442288 
06663 *   FOR A LICENSE PLATE WITH LEADING ALPHA CHARACTER, THE       * P442288 
06664 * SECOND CHARACTER CANNOT BE A ZERO.  A POWER UNIT PLATE SUCH   * P442288 
06665 * AS 'R01234' BECOMES 'R1234 '.                                 * P442288 
06666 *                                                               * P442288 
06667 *   FOR A LICENSE PLATE WITH ALPHA CHARACTER IN THE THIRD       * P442288 
06668 * POSITION, THE FOURTH CHARACTER CANNOT BE A ZERO.  A TRAILER   * P442288 
06669 * UNIT SUCH AS '34R012' BECOMES '34R12 '.                       * P442288 
06670 *                                                               * P442288 
06671 ***************************************************************** P442288 
06672                                                                   P442288 
06673       IF WS-IRP-LIC-CHAR-1 ALPHABETIC-UPPER    AND                P442288 
06674          WS-IRP-LIC-CHAR-1 NOT = SPACES                           P442288 
06675          IF  WS-IRP-LIC-NUM-2  = '0'                              P442288 
06676              MOVE WS-IRP-LIC-NUM-3-6 TO  WS-RTS-NUM-2-6           P442288 
06677          END-IF                                                   P442288 
06678       ELSE                                                        P442288 
06679       IF WS-IRP-LIC-CHAR-3 ALPHABETIC-UPPER    AND                P442288 
06680          WS-IRP-LIC-CHAR-3 NOT = SPACES                           P442288 
06681          IF  WS-IRP-LIC-NUM-4 = '0'                               P442288 
06682              MOVE WS-IRP-LIC-NUM-5-6 TO  WS-RTS-NUM-4-6           P442288 
06683          END-IF                                                   P442288 
06684       ELSE                                                        P442288 
06685       DISPLAY 'ILLOGICAL LICENSE PLATE ERROR ' WS-PLATE-NO-IRP    P442288 
06686       END-IF.                                                     P442288 
06687                                                                   P442288 
06688      EJECT                                                        P442288 
06689                                                                   P442288 
06690  2115-FIND-RTS-BY-VIN.                                            P442288 
06691                                                                   P442288 
                                                                                
      D    DISPLAY '!TC081 - INSIDE 2115-FIND-RTS-BY-VIN'                       
                                                                                
06692      PERFORM 21151-FIND-RTS-BY-AVIN THRU                          P442288 
06693              21151-FIND-RTS-BY-AVIN-EXIT.                         P442288 
06694                                                                   P442288 
06695      IF WS-2115-RETURN-CODE = 4                                   P442288 
                                                                                
      D    DISPLAY '!TC096 - INSIDE 2115-FIND-RTS-BY-VIN'                       
                                                                                
06696          PERFORM 21152-FIND-RTS-BY-IVIN THRU                      P442288 
06697                  21152-FIND-RTS-BY-IVIN-EXIT.                     P442288 
06698                                                                   P442288 
06699      IF WS-2115-RETURN-CODE = 4                                   P442288 
                                                                                
      D    DISPLAY '!TC111 - IF WS-2115-RETURN-CODE = 4 <'                      
      D                         WS-2115-RETURN-CODE '>'                         
                                                                                
06700          PERFORM 21153-FIND-RTS-BY-RVIN THRU                      P442288 
06701                  21153-FIND-RTS-BY-RVIN-EXIT.                     P442288 
06702                                                                   P442288 
06703  2115-FIND-RTS-BY-VIN-EXIT.                                       P442288 
06704      EXIT.                                                        P442288 
06705                                                                   P442288 
06706  21151-FIND-RTS-BY-AVIN.                                          P442288 
06707                                                                   P442288 
06708      IF TESTING-SHOW-DISPLAYS                                     P442288 
06709          DISPLAY '@ 21151-FIND-RTS-BY-AVIN:'                      P442288 
06710              ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG              P442288 
06711              ' UPDT ERR FL  = ' RTS-UPDT-ERROR-FLAG               P442288 
06712              ' ERROR SW     = ' ERROR-PROCESSING-SW               P442288 
06713          DISPLAY ' INV=' RTS-IRPINV-FLAG                          P442288 
06714      END-IF.                                                      P442288 
06715                                                                   P442288 
06716      IF WRKRTS-IDENTIFICATION-NBR = SPACES                        P442288 
                                                                                
      D    DISPLAY '!TC082 - IF WRKRTS-IDENTIFICATION-NBR = SPACES <'           
      D                         WRKRTS-IDENTIFICATION-NBR '>'                   
                                                                                
06717          MOVE 4                  TO WS-2115-RETURN-CODE           P442288 
06718          GO TO 21151-FIND-RTS-BY-AVIN-EXIT.                       P442288 
06719                                                                   P442288 
06720      MOVE WRKRTS-IDENTIFICATION-NBR TO WS-VIN-VIN.                P442288 
06721      MOVE ZEROS                     TO WS-2115-RETURN-CODE        P442288 
06722                                        WS-2115-ADABAS-RC.         P442288 
06723      MOVE ZEROS                     TO WS-FETCH-COUNT.            P442288 
06724      MOVE ZEROS                     TO WS-VIN-DOCNO.              P442288 
06725      MOVE SPACES                    TO WS-VIN-TTLPROCSCD.         P442288 
06726      MOVE ZEROS                     TO WS-VIN-TTLDATE.            P442288 
06727                                                                   P442288 
06728 ***************************************************************** P442288 
06729 *  INQUIRY BY VIN NUMBER                                          P442288 
06730 ***************************************************************** P442288 
06731                                                                   P442288 
06732 *---------------------------------------------------------------* P442288 
06733 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
06734 *---------------------------------------------------------------* P442288 
06735                                                                   P442288 
06736      IF TESTING-SHOW-DISPLAYS                                     P442288 
06737        DISPLAY ' @2115-INQ-VINR..NEXT IS ADABAS OPEN CVIN CALL'   P442288 
06738      END-IF.                                                      P442288 
06739      EXEC ADABAS                                                  P442288 
06740           OPEN  CVIN                                              P442288 
06741      END-EXEC                                                     P442288 
06742      .                                                            P442288 
06743                                                                   P442288 
06744      IF RESPONSE-CODE OF ACCESS-BY-CVIN > ZERO                    P442288 
06745          MOVE 16                           TO WS-2115-RETURN-CODE P442288 
06746          MOVE RESPONSE-CODE OF ACCESS-BY-CVIN TO WS-2115-ADABAS-RCP442288 
06747          IF TESTING-SHOW-DISPLAYS                                 P442288 
06748              DISPLAY ' ADABAS OPEN ACCESS-BY-CVIN ERROR '         P442288 
06749                  RESPONSE-CODE OF ACCESS-BY-CVIN                  P442288 
06750                  ' OPENING FILE NUMBERCVIN ' FILE-NUMBERCVIN      P442288 
06751          END-IF                                                   P442288 
06752          GO TO 21151-FIND-RTS-BY-AVIN-EXIT.                       P442288 
06753                                                                   P442288 
06754      IF TESTING-SHOW-DISPLAYS                                     P442288 
06755          DISPLAY 'VIN   QUAN = ' QUANTITY OF ACCESS-BY-CVIN       P442288 
06756               ' FOR VIN   ' WS-VIN-VIN ' ADACODE=' ADACODE        P442288 
06757          DISPLAY ' DOCNO = ' DOCNO OF ACCESS-BY-CVIN              P442288 
06758              ' ' VIN OF ACCESS-BY-CVIN                            P442288 
06759              ' TITL DATE ' TTLISSUEDATE OF ACCESS-BY-CVIN         P442288 
06760              ' TITL CODE ' TTLPROCSCD   OF ACCESS-BY-CVIN         P442288 
06761      END-IF.                                                      P442288 
06762                                                                   P442288 
06763      IF QUANTITY OF ACCESS-BY-CVIN > 1                            P442288 
                                                                                
      D        DISPLAY '!TC083 - IF QUANTITY OF ACCESS-BY-CVIN > 1 <'           
      D                             QUANTITY OF ACCESS-BY-CVIN '>'              
                                                                                
06764          MOVE ZERO               TO ADACODE                       P442288 
06765          MOVE ZERO               TO WS-FETCH-COUNT                P442288 
06766          MOVE ZERO               TO WS-VIN-ACTIVE-COUNT           P442288 
06767          PERFORM 211511-INQ-AVIN-FETCH THRU                       P442288 
06768                  211511-INQ-AVIN-FETCH-EXIT                       P442288 
06769              UNTIL ADACODE NOT = ZEROS  OR                        P442288 
06770                 WS-FETCH-COUNT = QUANTITY OF ACCESS-BY-CVIN OR    P442288 
06771                 WS-FETCH-COUNT > QUANTITY OF ACCESS-BY-CVIN       P442288 
06772      ELSE                                                         P442288 
06773      IF QUANTITY OF ACCESS-BY-CVIN = 1                            P442288 
                                                                                
      D        DISPLAY '!TC091 - IF QUANTITY OF ACCESS-BY-CVIN = 1 <'           
      D                             QUANTITY OF ACCESS-BY-CVIN '>'              
                                                                                
06774          MOVE ZERO                         TO WS-2115-RETURN-CODE P442288 
06775          MOVE 1                            TO WS-FETCH-COUNT      P442288 
06776          MOVE 1                            TO WS-VIN-ACTIVE-COUNT P442288 
06777          MOVE 'L'                          TO WS-VIN-MORE-IND     P442288 
06778          MOVE DOCNO OF ACCESS-BY-CVIN      TO WS-VIN-DOCNO        P442288 
06779          MOVE TTLPROCSCD OF ACCESS-BY-CVIN TO WS-VIN-TTLPROCSCD   P442288 
06780          MOVE TTLISSUEDATE OF ACCESS-BY-CVIN TO WS-VIN-TTLDATE    P442288 
06781          MOVE ACCESS-BY-CVIN            TO WS-BEST-VIN-DATA-HOLD  P442288 
06782      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC092:'                                                
      D        DISPLAY 'IF NOT QUANTITY OF ACCESS-BY-CVIN = 1 <'                
      D                       QUANTITY OF ACCESS-BY-CVIN '>'                    
                                                                                
06783          MOVE 04                 TO WS-2115-RETURN-CODE           P442288 
06784          MOVE 1                  TO RTS-UPDT-ERROR-FLAG           P442288 
06785          GO TO 21151-FIND-RTS-BY-AVIN-EXIT                        P442288 
06786      END-IF.                                                      P442288 
06787                                                                   P442288 
06788 *---------------------------------------------------------------* P442288 
06789 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
06790 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
06791 *---------------------------------------------------------------* P442288 
06792                                                                   P442288 
06793      EXEC ADABAS                                                  P442288 
06794            CLOSE CVIN                                             P442288 
06795      END-EXEC                                                     P442288 
06796          .                                                        P442288 
06797                                                                   P442288 
06798      IF WS-2115-RETURN-CODE = ZERO  AND                           P442288 
06799         WS-VIN-ACTIVE-COUNT > 0     AND                           P442288 
06800         WS-VIN-MORE-IND = 'L'       AND                           P442288 
06801         WS-VIN-TTLPROCSCD NOT = 'S' AND                           P442288 
06802         WS-VIN-TTLPROCSCD NOT = 'I'                               P442288 
                                                                                
      D        DISPLAY '!TC093 - '                                              
      D                'IF WS-2115-RETURN-CODE = ZERO <'                        
      D                    WS-2115-RETURN-CODE '> AND'                          
      D        DISPLAY 'WS-VIN-ACTIVE-COUNT > 0 <'                              
      D                 WS-VIN-ACTIVE-COUNT '> AND '                            
      D        DISPLAY 'WS-VIN-MORE-IND = L <'                                  
      D                 WS-VIN-MORE-IND '> AND '                                
      D        DISPLAY 'WS-VIN-TTLPROCSCD NOT = S AND NOT = I <'                
      D                 WS-VIN-TTLPROCSCD '>'                                   
                                                                                
06803          MOVE ZERO               TO WS-2115-RETURN-CODE           P442288 
06804      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC094 - '                                              
      D                'IF NOT WS-2115-RETURN-CODE = ZERO <'                    
      D                    WS-2115-RETURN-CODE '> AND'                          
      D        DISPLAY 'NOT WS-VIN-ACTIVE-COUNT > 0 <'                          
      D                 WS-VIN-ACTIVE-COUNT '> AND '                            
      D        DISPLAY 'NOT WS-VIN-MORE-IND = L <'                              
      D                 WS-VIN-MORE-IND '> AND '                                
      D        DISPLAY 'NOT WS-VIN-TTLPROCSCD NOT = S AND NOT = I <'            
      D                 WS-VIN-TTLPROCSCD '>'                                   
                                                                                
06805          MOVE 04                 TO WS-2115-RETURN-CODE.          P442288 
06806                                                                   P442288 
06807 *---------------------------------------------------------------* P442288 
06808 *EXTRA CHECKS TO SEE IF WE REALLY WANT TO UPDATE IRP WITH THIS  * P442288 
06809 *RECORD WITH UNIQUE VIN . . . DO NOT UPDATE WITH OLD DATA       * P442288 
06810 *---------------------------------------------------------------* P442288 
06811      IF WS-VIN-TTLPROCSCD = 'H'  OR                               P442288 
06812         WS-VIN-TTLPROCSCD = 'D'                                   P442288 
06813          NEXT SENTENCE                                            P442288 
06814      ELSE                                                         P442288 
06815      IF WRKRTS-LAST-CHG-DT > WS-VIN-TTLDATE    AND                P442288 
06816         WRKRTS-DOCNO = ZEROS                                      P442288 
                                                                                
      D        DISPLAY '!TC095 - IF NOT WS-VIN-TTLPROCSCD = H OR D <'           
      D                                 WS-VIN-TTLPROCSCD '> AND '              
      D        DISPLAY 'IF WRKRTS-LAST-CHG-DT <'                                
      D                    WRKRTS-LAST-CHG-DT '>  >  WS-VIN-TTLDATE <'          
      D                    WS-VIN-TTLDATE '> AND '                              
      D        DISPLAY 'WRKRTS-DOCNO = ZEROS <' WRKRTS-DOCNO '>'                
                                                                                
06817          MOVE 04                 TO WS-2115-RETURN-CODE           P442288 
06818          IF TESTING-SHOW-DISPLAYS                                 P442288 
06819              DISPLAY ' @2115-- SKIP RTS TOO OLD TITLE DATE '      P442288 
06820                  WS-VIN-TTLDATE ' LESS THAN '                     P442288 
06821                  WRKRTS-LAST-CHG-DT ' (VEHL SVC)'                 P442288 
06822      END-IF.                                                      P442288 
06823      IF TESTING-SHOW-DISPLAYS                                     P442288 
06824          DISPLAY ' @2115-FIND--VIN-EXIT RC=' WS-2115-RETURN-CODE  P442288 
06825              ' FOR VIN =' WS-VIN-VIN                              P442288 
06826          DISPLAY ' ACTIVE # =' WS-VIN-ACTIVE-COUNT                P442288 
06827              ' LAST FLAG =' WS-VIN-MORE-IND                       P442288 
06828              ' DOCNO=' WS-VIN-DOCNO                               P442288 
06829              ' TTL CODE=' WS-VIN-TTLPROCSCD                       P442288 
06830      END-IF.                                                      P442288 
06831                                                                   P442288 
06832  21151-FIND-RTS-BY-AVIN-EXIT.                                     P442288 
06833      EXIT.                                                        P442288 
06834                                                                   P442288 
06835  211511-INQ-AVIN-FETCH.                                           P442288 
06836                                                                   P442288 
06837      ADD 1                       TO WS-FETCH-COUNT.               P442288 
06838      ADD 1                       TO WS-VIN-ACTIVE-COUNT.          P442288 
06839                                                                   P442288 
06840      IF TESTING-SHOW-DISPLAYS                                     P442288 
06841          DISPLAY ' @Z21151-INQ-VIN-FETCH # ' WS-FETCH-COUNT.      P442288 
06842                                                                   P442288 
06843 *---------------------------------------------------------------* P442288 
06844 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
06845 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
06846 *---------------------------------------------------------------* P442288 
06847                                                                   P442288 
06848      EXEC ADABAS                                                  P442288 
06849           FETCH CVIN                                              P442288 
06850      END-EXEC                                                     P442288 
06851      .                                                            P442288 
06852                                                                   P442288 
06853      IF TESTING-SHOW-DISPLAYS                                     P442288 
06854          DISPLAY ' @Z21115-INQ-VINR....AFTER "FETCH CVIN" '       P442288 
06855              ' WHERE ADACODE = ' ADACODE                          P442288 
06856              ' FOR VIN ' WS-VIN-VIN 'DOCNO = '                    P442288 
06857          DISPLAY ' DOCNO = ' DOCNO OF ACCESS-BY-CVIN              P442288 
06858              ' ' VIN OF ACCESS-BY-CVIN                            P442288 
06859          DISPLAY ' TITL DATE ' TTLISSUEDATE OF ACCESS-BY-CVIN     P442288 
06860              ' TITL CODE ' TTLPROCSCD   OF ACCESS-BY-CVIN         P442288 
06861      END-IF.                                                      P442288 
06862                                                                   P442288 
06863      IF ADACODE = 3                                               P442288 
                                                                                
      D        DISPLAY '!TC084 - IF ADACODE = 3 <' ADACODE '>'                  
                                                                                
06864          MOVE 'L'                     TO WS-VIN-MORE-IND          P442288 
06865          SUBTRACT 1                   FROM WS-VIN-ACTIVE-COUNT    P442288 
06866      ELSE                                                         P442288 
06867      IF ADACODE NOT EQUAL ZERO                                    P442288 
06868          MOVE 4                       TO WS-2115-RETURN-CODE      P442288 
06869      ELSE                                                         P442288 
06870      IF TTLPROCSCD OF ACCESS-BY-CVIN NOT = 'S' AND                P442288 
06871         TTLPROCSCD OF ACCESS-BY-CVIN NOT = 'I'                    P442288 
                                                                                
      D      DISPLAY '!TC085 - IF TTLPROCSCD OF ACCESS-BY-CVIN NOT = S'         
      D              ' <' TTLPROCSCD OF ACCESS-BY-CVIN '>'                      
      D      DISPLAY 'AND TTLPROCSCD OF ACCESS-BY-CVIN NOT = I'                 
                                                                                
06872          MOVE DOCNO OF ACCESS-BY-CVIN      TO WS-THIS-VIN-DOCNO   P442288 
06873          IF WS-THIS-VIN-AM-DT-TM  >  WS-VIN-AM-DT-TM              P442288 
                                                                                
      D      DISPLAY '!TC086 - IF WS-THIS-VIN-AM-DT-TM <'                       
      D                           WS-THIS-VIN-AM-DT-TM '> > '                   
      D                          'WS-VIN-AM-DT-TM <'                            
      D                           WS-VIN-AM-DT-TM '>'                           
                                                                                
06874              MOVE WS-VIN-DOCNO        TO RTS-TTL-SKIPPED-OLD-DOCNOP442288 
06875              MOVE ACCESS-BY-CVIN      TO WS-BEST-VIN-DATA-HOLD    P442288 
06876          ELSE                                                     P442288 
                                                                                
      D      DISPLAY '!TC087 - IF NOT WS-THIS-VIN-AM-DT-TM <'                   
      D                           WS-THIS-VIN-AM-DT-TM '> > '                   
      D                          'WS-VIN-AM-DT-TM <'                            
      D                           WS-VIN-AM-DT-TM '>'                           
                                                                                
06877              MOVE WS-THIS-VIN-DOCNO   TO RTS-TTL-SKIPPED-OLD-DOCNOP442288 
06878          END-IF                                                   P442288 
06879      ELSE                                                         P442288 
06880      IF WS-FETCH-COUNT = 1                                        P442288 
                                                                                
      D      DISPLAY '!TC088 - IF WS-FETCH-COUNT = 1 <'                         
      D                           WS-FETCH-COUNT '>'                            
                                                                                
06881          MOVE DOCNO OF ACCESS-BY-CVIN      TO WS-VIN-DOCNO        P442288 
06882          MOVE TTLPROCSCD OF ACCESS-BY-CVIN TO WS-VIN-TTLPROCSCD   P442288 
06883          MOVE TTLISSUEDATE OF ACCESS-BY-CVIN TO WS-VIN-TTLDATE    P442288 
06884          MOVE ACCESS-BY-CVIN            TO WS-BEST-VIN-DATA-HOLD  P442288 
06885                                                                   P442288 
06886      END-IF.                                                      P442288 
06887                                                                   P442288 
06888      IF TTLPROCSCD OF ACCESS-BY-CVIN  = 'S' OR 'I'                P442288 
                                                                                
      D      DISPLAY '!TC089 - IF TTLPROCSCD OF ACCESS-BY-CVIN  = S'            
      D              ' OR I <' TTLPROCSCD OF ACCESS-BY-CVIN '>'                 
                                                                                
06889          MOVE DOCNO OF ACCESS-BY-CVIN TO RTS-TTL-SKIPPED-OLD-DOCNOP442288 
06890          SUBTRACT 1              FROM WS-VIN-ACTIVE-COUNT.        P442288 
06891                                                                   P442288 
06892      IF WS-FETCH-COUNT = QUANTITY OF ACCESS-BY-CVIN               P442288 
                                                                                
      D      DISPLAY '!TC090 - WS-FETCH-COUNT <' WS-FETCH-COUNT '> = '          
      D              'QUANTITY OF ACCESS-BY-CVIN <'                             
      D               QUANTITY OF ACCESS-BY-CVIN '>'                            
                                                                                
06893          MOVE 'L'                     TO WS-VIN-MORE-IND.         P442288 
06894                                                                   P442288 
06895  211511-INQ-AVIN-FETCH-EXIT.                                      P442288 
06896                                                                   P442288 
06897      EXIT.                                                        P442288 
06898                                                                   P442288 
06899  21152-FIND-RTS-BY-IVIN.                                          P442288 
06900                                                                   P442288 
06901      IF TESTING-SHOW-DISPLAYS                                     P442288 
06902          DISPLAY '@ 21152-FIND-RTS-BY-IVIN:'                      P442288 
06903              ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG              P442288 
06904              ' UPDT ERR FL  = ' RTS-UPDT-ERROR-FLAG               P442288 
06905              ' ERROR SW     = ' ERROR-PROCESSING-SW               P442288 
06906          DISPLAY ' INV=' RTS-IRPINV-FLAG                          P442288 
06907      END-IF.                                                      P442288 
06908                                                                   P442288 
06909      IF WRKRTS-IDENTIFICATION-NBR = SPACES                        P442288 
                                                                                
      D      DISPLAY '!TC097 - IF WRKRTS-IDENTIFICATION-NBR = SPACES <'         
      D                           WRKRTS-IDENTIFICATION-NBR '>'                 
                                                                                
06910          MOVE 4                  TO WS-2115-RETURN-CODE           P442288 
06911          GO TO 21152-FIND-RTS-BY-IVIN-EXIT.                       P442288 
06912                                                                   P442288 
06913      MOVE WRKRTS-IDENTIFICATION-NBR TO WS-VIN-VIN.                P442288 
06914      MOVE ZEROS                     TO WS-2115-RETURN-CODE        P442288 
06915                                        WS-2115-ADABAS-RC.         P442288 
06916      MOVE ZEROS                     TO WS-FETCH-COUNT.            P442288 
06917      MOVE ZEROS                     TO WS-VIN-DOCNO.              P442288 
06918      MOVE SPACES                    TO WS-VIN-TTLPROCSCD.         P442288 
06919      MOVE ZEROS                     TO WS-VIN-TTLDATE.            P442288 
06920                                                                   P442288 
06921 ***************************************************************** P442288 
06922 *  INQUIRY BY VIN NUMBER                                          P442288 
06923 ***************************************************************** P442288 
06924                                                                   P442288 
06925 *---------------------------------------------------------------* P442288 
06926 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
06927 *---------------------------------------------------------------* P442288 
06928                                                                   P442288 
06929      IF TESTING-SHOW-DISPLAYS                                     P442288 
06930        DISPLAY ' @21152-INQ-IVIN..NEXT IS ADABAS OPEN IVIN CALL'  P442288 
06931      END-IF.                                                      P442288 
06932      EXEC ADABAS                                                  P442288 
06933           OPEN  IVIN                                              P442288 
06934      END-EXEC                                                     P442288 
06935      .                                                            P442288 
06936                                                                   P442288 
06937      IF RESPONSE-CODE OF ACCESS-BY-IVIN > ZERO                    P442288 
06938          MOVE 16                           TO WS-2115-RETURN-CODE P442288 
06939          MOVE RESPONSE-CODE OF ACCESS-BY-IVIN TO WS-2115-ADABAS-RCP442288 
06940          IF TESTING-SHOW-DISPLAYS                                 P442288 
06941              DISPLAY ' ADABAS OPEN ACCESS-BY-IVIN ERROR '         P442288 
06942                  RESPONSE-CODE OF ACCESS-BY-IVIN                  P442288 
06943                  ' OPENING FILE NUMBERIVIN ' FILE-NUMBERIVIN      P442288 
06944          END-IF                                                   P442288 
06945          GO TO 21152-FIND-RTS-BY-IVIN-EXIT.                       P442288 
06946                                                                   P442288 
06947      IF TESTING-SHOW-DISPLAYS                                     P442288 
06948          DISPLAY 'VIN   QUAN = ' QUANTITY OF ACCESS-BY-IVIN       P442288 
06949               ' FOR VIN   ' WS-VIN-VIN ' ADACODE=' ADACODE        P442288 
06950          DISPLAY ' DOCNO = ' DOCNO OF ACCESS-BY-IVIN              P442288 
06951              ' ' VIN OF ACCESS-BY-IVIN                            P442288 
06952              ' TITL DATE ' TTLISSUEDATE OF ACCESS-BY-IVIN         P442288 
06953              ' TITL CODE ' TTLPROCSCD   OF ACCESS-BY-IVIN         P442288 
06954      END-IF.                                                      P442288 
06955                                                                   P442288 
06956      IF QUANTITY OF ACCESS-BY-IVIN > 1                            P442288 
                                                                                
      D      DISPLAY '!TC098 - IF QUANTITY OF ACCESS-BY-IVIN > 1 <'             
      D                           QUANTITY OF ACCESS-BY-IVIN '>'                
                                                                                
06957          MOVE ZERO               TO ADACODE                       P442288 
06958          MOVE ZERO               TO WS-FETCH-COUNT                P442288 
06959          MOVE ZERO               TO WS-VIN-ACTIVE-COUNT           P442288 
06960          PERFORM 211521-INQ-IVIN-FETCH                            P442288 
06961              UNTIL ADACODE NOT = ZEROS  OR                        P442288 
06962                 WS-FETCH-COUNT = QUANTITY OF ACCESS-BY-IVIN OR    P442288 
06963                 WS-FETCH-COUNT > QUANTITY OF ACCESS-BY-IVIN       P442288 
06964      ELSE                                                         P442288 
06965      IF QUANTITY OF ACCESS-BY-IVIN = 1                            P442288 
                                                                                
      D      DISPLAY '!TC106 - IF QUANTITY OF ACCESS-BY-IVIN = 1 <'             
      D                           QUANTITY OF ACCESS-BY-IVIN '>'                
                                                                                
06966          MOVE ZERO                         TO WS-2115-RETURN-CODE P442288 
06967          MOVE 1                            TO WS-FETCH-COUNT      P442288 
06968          MOVE 1                            TO WS-VIN-ACTIVE-COUNT P442288 
06969          MOVE 'L'                          TO WS-VIN-MORE-IND     P442288 
06970          MOVE DOCNO OF ACCESS-BY-IVIN      TO WS-VIN-DOCNO        P442288 
06971          MOVE TTLPROCSCD OF ACCESS-BY-IVIN TO WS-VIN-TTLPROCSCD   P442288 
06972          MOVE TTLISSUEDATE OF ACCESS-BY-IVIN TO WS-VIN-TTLDATE    P442288 
06973          MOVE ACCESS-BY-IVIN            TO WS-BEST-VIN-DATA-HOLD  P442288 
06974      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC107 - '                                              
      D        DISPLAY 'IF NOT QUANTITY OF ACCESS-BY-IVIN = 1'                  
      D        DISPLAY 'QUANTITY OF ACCESS-BY-IVIN <'                           
      D                       QUANTITY OF ACCESS-BY-IVIN '>'                    
                                                                                
06975          MOVE 04                 TO WS-2115-RETURN-CODE           P442288 
06976          MOVE 1                  TO RTS-UPDT-ERROR-FLAG           P442288 
06977          GO TO 21152-FIND-RTS-BY-IVIN-EXIT                        P442288 
06978      END-IF.                                                      P442288 
06979                                                                   P442288 
06980 *---------------------------------------------------------------* P442288 
06981 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
06982 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
06983 *---------------------------------------------------------------* P442288 
06984                                                                   P442288 
06985      EXEC ADABAS                                                  P442288 
06986            CLOSE IVIN                                             P442288 
06987      END-EXEC                                                     P442288 
06988          .                                                        P442288 
06989                                                                   P442288 
06990      IF WS-2115-RETURN-CODE = ZERO  AND                           P442288 
06991         WS-VIN-ACTIVE-COUNT > 0     AND                           P442288 
06992         WS-VIN-MORE-IND = 'L'       AND                           P442288 
06993         WS-VIN-TTLPROCSCD NOT = 'S' AND                           P442288 
06994         WS-VIN-TTLPROCSCD NOT = 'I'                               P442288 
                                                                                
      D      DISPLAY '!TC108 - IF WS-2115-RETURN-CODE = ZERO <'                 
      D                           WS-2115-RETURN-CODE '> AND '                  
      D              'WS-VIN-ACTIVE-COUNT > 0 <'                                
      D               WS-VIN-ACTIVE-COUNT '> AND '                              
      D              'WS-VIN-MORE-IND = L <' WS-VIN-MORE-IND '> AND '           
      D              'WS-VIN-TTLPROCSCD NOT = S AND NOT = I <'                  
      D               WS-VIN-TTLPROCSCD '>'                                     
                                                                                
06995          MOVE ZERO               TO WS-2115-RETURN-CODE           P442288 
06996      ELSE                                                         P442288 
                                                                                
      D      DISPLAY '!TC109 - IF NOT WS-2115-RETURN-CODE = ZERO <'             
      D                           WS-2115-RETURN-CODE '> AND '                  
      D              'WS-VIN-ACTIVE-COUNT > 0 <'                                
      D               WS-VIN-ACTIVE-COUNT '> AND '                              
      D              'WS-VIN-MORE-IND = L <' WS-VIN-MORE-IND '> AND '           
      D              'WS-VIN-TTLPROCSCD NOT = S AND NOT = I <'                  
      D               WS-VIN-TTLPROCSCD '>'                                     
                                                                                
06997          MOVE 04                 TO WS-2115-RETURN-CODE.          P442288 
06998                                                                   P442288 
06999 *---------------------------------------------------------------* P442288 
07000 *EXTRA CHECKS TO SEE IF WE REALLY WANT TO UPDATE IRP WITH THIS  * P442288 
07001 *RECORD WITH UNIQUE VIN . . . DO NOT UPDATE WITH OLD DATA       * P442288 
07002 *---------------------------------------------------------------* P442288 
07003      IF WS-VIN-TTLPROCSCD = 'H'  OR                               P442288 
07004         WS-VIN-TTLPROCSCD = 'D'                                   P442288 
07005          NEXT SENTENCE                                            P442288 
07006      ELSE                                                         P442288 
07007      IF WRKRTS-LAST-CHG-DT > WS-VIN-TTLDATE    AND                P442288 
07008         WRKRTS-DOCNO = ZEROS                                      P442288 
                                                                                
      D      DISPLAY '!TC110 - IF NOT WS-VIN-TTLPROCSCD = H OR D <'             
      D                               WS-VIN-TTLPROCSCD '> AND '                
      D      DISPLAY 'IF WRKRTS-LAST-CHG-DT <' WRKRTS-LAST-CHG-DT '>  >'        
      D              '  WS-VIN-TTLDATE <' WS-VIN-TTLDATE '> AND '               
      D      DISPLAY 'WRKRTS-DOCNO = ZEROS <' WRKRTS-DOCNO '>'                  
                                                                                
07009          MOVE 04                 TO WS-2115-RETURN-CODE           P442288 
07010          IF TESTING-SHOW-DISPLAYS                                 P442288 
07011              DISPLAY ' @21152-- SKIP RTS TOO OLD TITLE DATE '     P442288 
07012                  WS-VIN-TTLDATE ' LESS THAN '                     P442288 
07013                  WRKRTS-LAST-CHG-DT ' (VEHL SVC)'                 P442288 
07014      END-IF.                                                      P442288 
07015      IF TESTING-SHOW-DISPLAYS                                     P442288 
07016          DISPLAY ' @21152-FIND-IVIN-EXIT RC=' WS-2115-RETURN-CODE P442288 
07017              ' FOR VIN =' WS-VIN-VIN                              P442288 
07018          DISPLAY ' ACTIVE # =' WS-VIN-ACTIVE-COUNT                P442288 
07019              ' LAST FLAG =' WS-VIN-MORE-IND                       P442288 
07020              ' DOCNO=' WS-VIN-DOCNO                               P442288 
07021              ' TTL CODE=' WS-VIN-TTLPROCSCD                       P442288 
07022      END-IF.                                                      P442288 
07023                                                                   P442288 
07024  21152-FIND-RTS-BY-IVIN-EXIT.                                     P442288 
07025      EXIT.                                                        P442288 
07026                                                                   P442288 
07027  211521-INQ-IVIN-FETCH.                                           P442288 
07028                                                                   P442288 
07029      ADD 1                       TO WS-FETCH-COUNT.               P442288 
07030      ADD 1                       TO WS-VIN-ACTIVE-COUNT.          P442288 
07031                                                                   P442288 
07032      IF TESTING-SHOW-DISPLAYS                                     P442288 
07033          DISPLAY ' @211521-INQ-IVIN-FETCH # ' WS-FETCH-COUNT.     P442288 
07034                                                                   P442288 
07035 *---------------------------------------------------------------* P442288 
07036 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
07037 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
07038 *---------------------------------------------------------------* P442288 
07039                                                                   P442288 
07040      EXEC ADABAS                                                  P442288 
07041           FETCH IVIN                                              P442288 
07042      END-EXEC                                                     P442288 
07043      .                                                            P442288 
07044                                                                   P442288 
07045      IF TESTING-SHOW-DISPLAYS                                     P442288 
07046          DISPLAY ' @211521-INQ-IVIN....AFTER "FETCH IVIN" '       P442288 
07047              ' WHERE ADACODE = ' ADACODE                          P442288 
07048              ' FOR VIN ' WS-VIN-VIN 'DOCNO = '                    P442288 
07049          DISPLAY ' DOCNO = ' DOCNO OF ACCESS-BY-IVIN              P442288 
07050              ' ' VIN OF ACCESS-BY-IVIN                            P442288 
07051          DISPLAY ' TITL DATE ' TTLISSUEDATE OF ACCESS-BY-IVIN     P442288 
07052              ' TITL CODE ' TTLPROCSCD   OF ACCESS-BY-IVIN         P442288 
07053      END-IF.                                                      P442288 
07054                                                                   P442288 
07055      IF ADACODE = 3                                               P442288 
                                                                                
      D        DISPLAY '!TC099 - IF ADACODE = 3  <' ADACODE '>'                 
                                                                                
07056          MOVE 'L'                     TO WS-VIN-MORE-IND          P442288 
07057          SUBTRACT 1                   FROM WS-VIN-ACTIVE-COUNT    P442288 
07058      ELSE                                                         P442288 
07059      IF ADACODE NOT EQUAL ZERO                                    P442288 
07060          MOVE 4                       TO WS-2115-RETURN-CODE      P442288 
07061      ELSE                                                         P442288 
07062      IF TTLPROCSCD OF ACCESS-BY-IVIN NOT = 'S' AND                P442288 
07063         TTLPROCSCD OF ACCESS-BY-IVIN NOT = 'I'                    P442288 
                                                                                
      D        DISPLAY '!TC100 - IF TTLPROCSCD OF ACCESS-BY-IVIN NOT '          
      D                ' = S OR I <' TTLPROCSCD OF ACCESS-BY-IVIN '>'           
                                                                                
07064          MOVE DOCNO OF ACCESS-BY-IVIN      TO WS-THIS-VIN-DOCNO   P442288 
07065          IF WS-THIS-VIN-AM-DT-TM  >  WS-VIN-AM-DT-TM              P442288 
                                                                                
      D        DISPLAY '!TC101 - IF WS-THIS-VIN-AM-DT-TM <'                     
      D                             WS-THIS-VIN-AM-DT-TM '>  > '                
      D                'WS-VIN-AM-DT-TM <' WS-VIN-AM-DT-TM '>'                  
                                                                                
07066              MOVE WS-VIN-DOCNO        TO RTS-TTL-SKIPPED-OLD-DOCNOP442288 
07067              MOVE ACCESS-BY-IVIN      TO WS-BEST-VIN-DATA-HOLD    P442288 
07068          ELSE                                                     P442288 
                                                                                
      D        DISPLAY '!TC102 - IF NOT WS-THIS-VIN-AM-DT-TM <'                 
      D                             WS-THIS-VIN-AM-DT-TM '>  > '                
      D                'WS-VIN-AM-DT-TM <' WS-VIN-AM-DT-TM '>'                  
                                                                                
07069              MOVE WS-THIS-VIN-DOCNO   TO RTS-TTL-SKIPPED-OLD-DOCNOP442288 
07070          END-IF                                                   P442288 
07071      ELSE                                                         P442288 
07072      IF WS-FETCH-COUNT = 1                                        P442288 
                                                                                
      D        DISPLAY '!TC103 - IF WS-FETCH-COUNT = 1 <'                       
      D                             WS-FETCH-COUNT '>'                          
                                                                                
07073          MOVE DOCNO OF ACCESS-BY-IVIN      TO WS-VIN-DOCNO        P442288 
07074          MOVE TTLPROCSCD OF ACCESS-BY-IVIN TO WS-VIN-TTLPROCSCD   P442288 
07075          MOVE TTLISSUEDATE OF ACCESS-BY-IVIN TO WS-VIN-TTLDATE    P442288 
07076          MOVE ACCESS-BY-IVIN            TO WS-BEST-VIN-DATA-HOLD  P442288 
07077                                                                   P442288 
07078      END-IF.                                                      P442288 
07079                                                                   P442288 
07080      IF TTLPROCSCD OF ACCESS-BY-IVIN  = 'S' OR 'I'                P442288 
                                                                                
      D        DISPLAY '!TC104 - IF TTLPROCSCD OF ACCESS-BY-IVIN  = S '         
      D                ' OR I <' TTLPROCSCD OF ACCESS-BY-IVIN '>'               
                                                                                
07081          MOVE DOCNO OF ACCESS-BY-IVIN TO RTS-TTL-SKIPPED-OLD-DOCNOP442288 
07082          SUBTRACT 1              FROM WS-VIN-ACTIVE-COUNT.        P442288 
07083                                                                   P442288 
07084      IF WS-FETCH-COUNT = QUANTITY OF ACCESS-BY-IVIN               P442288 
                                                                                
      D        DISPLAY '!TC105 - IF WS-FETCH-COUNT = 1 <'                       
      D         WS-FETCH-COUNT '> = QUANTITY OF ACCESS-BY-IVIN <'               
      D                             QUANTITY OF ACCESS-BY-IVIN '>'              
                                                                                
07085          MOVE 'L'                     TO WS-VIN-MORE-IND.         P442288 
07086                                                                   P442288 
07087  211521-INQ-IVIN-FETCH-EXIT.                                      P442288 
07088      EXIT.                                                        P442288 
07089      EJECT                                                        P442288 
07090                                                                   P442288 
07091                                                                   P442288 
07092  21153-FIND-RTS-BY-RVIN.                                          P442288 
07093                                                                   P442288 
07094      IF TESTING-SHOW-DISPLAYS                                     P442288 
07095          DISPLAY '@ 21153-FIND-RTS-RVIN:'                         P442288 
07096              ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG              P442288 
07097              ' UPDT ERR FL  = ' RTS-UPDT-ERROR-FLAG               P442288 
07098              ' ERROR SW     = ' ERROR-PROCESSING-SW               P442288 
07099          DISPLAY ' INV=' RTS-IRPINV-FLAG                          P442288 
07100      END-IF.                                                      P442288 
07101                                                                   P442288 
07102      IF WRKRTS-IDENTIFICATION-NBR = SPACES                        P442288 
                                                                                
      D        DISPLAY '!TC112 - IF WRKRTS-IDENTIFICATION-NBR = SPACES'         
      D                        ' <' WRKRTS-IDENTIFICATION-NBR '>'               
                                                                                
07103          MOVE 4                  TO WS-2115-RETURN-CODE           P442288 
07104          GO TO 21153-FIND-RTS-BY-RVIN-EXIT.                       P442288 
07105                                                                   P442288 
07106      MOVE WRKRTS-IDENTIFICATION-NBR TO WS-VIN-VIN.                P442288 
07107      MOVE ZEROS                     TO WS-2115-RETURN-CODE        P442288 
07108                                        WS-2115-ADABAS-RC.         P442288 
07109      MOVE ZEROS                     TO WS-FETCH-COUNT.            P442288 
07110      MOVE ZEROS                     TO WS-VIN-DOCNO.              P442288 
07111      MOVE SPACES                    TO WS-VIN-TTLPROCSCD.         P442288 
07112      MOVE ZEROS                     TO WS-VIN-TTLDATE.            P442288 
07113                                                                   P442288 
07114 ***************************************************************** P442288 
07115 *  INQUIRY BY VIN NUMBER                                          P442288 
07116 ***************************************************************** P442288 
07117                                                                   P442288 
07118 *---------------------------------------------------------------* P442288 
07119 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
07120 *---------------------------------------------------------------* P442288 
07121                                                                   P442288 
07122      IF TESTING-SHOW-DISPLAYS                                     P442288 
07123        DISPLAY ' @21153-INQ-RVIN..NEXT IS ADABAS OPEN RVIN CALL'  P442288 
07124      END-IF.                                                      P442288 
07125      EXEC ADABAS                                                  P442288 
07126           OPEN  RVIN                                              P442288 
07127      END-EXEC                                                     P442288 
07128      .                                                            P442288 
07129                                                                   P442288 
07130      IF RESPONSE-CODE OF ACCESS-BY-RVIN > ZERO                    P442288 
07131          MOVE 16                           TO WS-2115-RETURN-CODE P442288 
07132          MOVE RESPONSE-CODE OF ACCESS-BY-RVIN TO WS-2115-ADABAS-RCP442288 
07133          IF TESTING-SHOW-DISPLAYS                                 P442288 
07134              DISPLAY ' ADABAS OPEN ACCESS-BY-RVIN ERROR '         P442288 
07135                  RESPONSE-CODE OF ACCESS-BY-RVIN                  P442288 
07136                  ' OPENING FILE NUMBERRVIN ' FILE-NUMBERRVIN      P442288 
07137          END-IF                                                   P442288 
07138          GO TO 21153-FIND-RTS-BY-RVIN-EXIT.                       P442288 
07139                                                                   P442288 
07140      IF TESTING-SHOW-DISPLAYS                                     P442288 
07141          DISPLAY 'VIN   QUAN = ' QUANTITY OF ACCESS-BY-RVIN       P442288 
07142               ' FOR VIN   ' WS-VIN-VIN ' ADACODE=' ADACODE        P442288 
07143          DISPLAY ' DOCNO = ' DOCNO OF ACCESS-BY-RVIN              P442288 
07144              ' ' VIN OF ACCESS-BY-RVIN                            P442288 
07145              ' TITL DATE ' TTLISSUEDATE OF ACCESS-BY-RVIN         P442288 
07146              ' TITL CODE ' TTLPROCSCD   OF ACCESS-BY-RVIN         P442288 
07147      END-IF.                                                      P442288 
07148                                                                   P442288 
07149      IF QUANTITY OF ACCESS-BY-RVIN > 1                            P442288 
                                                                                
      D        DISPLAY '!TC113 - IF QUANTITY OF ACCESS-BY-RVIN > 1 <'           
      D                     QUANTITY OF ACCESS-BY-RVIN '>'                      
                                                                                
07150          MOVE ZERO               TO ADACODE                       P442288 
07151          MOVE ZERO               TO WS-FETCH-COUNT                P442288 
07152          MOVE ZERO               TO WS-VIN-ACTIVE-COUNT           P442288 
07153          PERFORM 211531-INQ-RVIN-FETCH                            P442288 
07154              UNTIL ADACODE NOT = ZEROS  OR                        P442288 
07155                 WS-FETCH-COUNT = QUANTITY OF ACCESS-BY-RVIN OR    P442288 
07156                 WS-FETCH-COUNT > QUANTITY OF ACCESS-BY-RVIN       P442288 
07157      ELSE                                                         P442288 
07158      IF QUANTITY OF ACCESS-BY-RVIN = 1                            P442288 
                                                                                
      D        DISPLAY '!TC121 - IF QUANTITY OF ACCESS-BY-RVIN = 1 <'           
      D                             QUANTITY OF ACCESS-BY-RVIN '>'              
                                                                                
07159          MOVE ZERO                         TO WS-2115-RETURN-CODE P442288 
07160          MOVE 1                            TO WS-FETCH-COUNT      P442288 
07161          MOVE 1                            TO WS-VIN-ACTIVE-COUNT P442288 
07162          MOVE 'L'                          TO WS-VIN-MORE-IND     P442288 
07163          MOVE DOCNO OF ACCESS-BY-RVIN      TO WS-VIN-DOCNO        P442288 
07164          MOVE TTLPROCSCD OF ACCESS-BY-RVIN TO WS-VIN-TTLPROCSCD   P442288 
07165          MOVE TTLISSUEDATE OF ACCESS-BY-RVIN TO WS-VIN-TTLDATE    P442288 
07166          MOVE ACCESS-BY-RVIN            TO WS-BEST-VIN-DATA-HOLD  P442288 
07167      ELSE                                                         P442288 
                                                                                
      D        DISPLAY '!TC122 - '                                              
      D                'IF NOT QUANTITY OF ACCESS-BY-RVIN = 1'                  
      D        DISPLAY 'QUANTITY OF ACCESS-BY-RVIN <'                           
      D                       QUANTITY OF ACCESS-BY-RVIN '>'                    
                                                                                
07168          MOVE 04                 TO WS-2115-RETURN-CODE           P442288 
07169          MOVE 1                  TO RTS-UPDT-ERROR-FLAG           P442288 
07170          GO TO 21153-FIND-RTS-BY-RVIN-EXIT                        P442288 
07171      END-IF.                                                      P442288 
07172                                                                   P442288 
07173 *---------------------------------------------------------------* P442288 
07174 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
07175 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
07176 *---------------------------------------------------------------* P442288 
07177                                                                   P442288 
07178      EXEC ADABAS                                                  P442288 
07179            CLOSE RVIN                                             P442288 
07180      END-EXEC                                                     P442288 
07181          .                                                        P442288 
07182                                                                   P442288 
07183      IF WS-2115-RETURN-CODE = ZERO  AND                           P442288 
07184         WS-VIN-ACTIVE-COUNT > 0     AND                           P442288 
07185         WS-VIN-MORE-IND = 'L'       AND                           P442288 
07186         WS-VIN-TTLPROCSCD NOT = 'S' AND                           P442288 
07187         WS-VIN-TTLPROCSCD NOT = 'I'                               P442288 
                                                                                
      D      DISPLAY '!TC123 - IF WS-2115-RETURN-CODE = ZERO <'                 
      D                           WS-2115-RETURN-CODE '> AND '                  
      D              'WS-VIN-ACTIVE-COUNT > 0 <'                                
      D               WS-VIN-ACTIVE-COUNT '> AND '                              
      D              'WS-VIN-MORE-IND = L <' WS-VIN-MORE-IND '> AND '           
      D              'WS-VIN-TTLPROCSCD NOT = S AND NOT = I <'                  
      D               WS-VIN-TTLPROCSCD '>'                                     
                                                                                
07188          MOVE ZERO               TO WS-2115-RETURN-CODE           P442288 
07189      ELSE                                                         P442288 
                                                                                
      D      DISPLAY '!TC124 - IF NOT WS-2115-RETURN-CODE = ZERO <'             
      D                           WS-2115-RETURN-CODE '> AND '                  
      D              'WS-VIN-ACTIVE-COUNT > 0 <'                                
      D               WS-VIN-ACTIVE-COUNT '> AND '                              
      D              'WS-VIN-MORE-IND = L <' WS-VIN-MORE-IND '> AND '           
      D              'WS-VIN-TTLPROCSCD NOT = S AND NOT = I <'                  
      D               WS-VIN-TTLPROCSCD '>'                                     
                                                                                
07190          MOVE 04                 TO WS-2115-RETURN-CODE.          P442288 
07191                                                                   P442288 
07192 *---------------------------------------------------------------* P442288 
07193 *EXTRA CHECKS TO SEE IF WE REALLY WANT TO UPDATE IRP WITH THIS  * P442288 
07194 *RECORD WITH UNIQUE VIN . . . DO NOT UPDATE WITH OLD DATA       * P442288 
07195 *---------------------------------------------------------------* P442288 
07196      IF WS-VIN-TTLPROCSCD = 'H'  OR                               P442288 
07197         WS-VIN-TTLPROCSCD = 'D'                                   P442288 
07198          NEXT SENTENCE                                            P442288 
07199      ELSE                                                         P442288 
07200      IF WRKRTS-LAST-CHG-DT > WS-VIN-TTLDATE    AND                P442288 
07201         WRKRTS-DOCNO = ZEROS                                      P442288 
                                                                                
      D      DISPLAY '!TC125 - IF NOT WS-VIN-TTLPROCSCD = H OR D <'             
      D                               WS-VIN-TTLPROCSCD '> AND '                
      D      DISPLAY 'IF WRKRTS-LAST-CHG-DT <' WRKRTS-LAST-CHG-DT '>  >'        
      D              '  WS-VIN-TTLDATE <' WS-VIN-TTLDATE '> AND '               
      D      DISPLAY 'WRKRTS-DOCNO = ZEROS <' WRKRTS-DOCNO '>'                  
                                                                                
07202          MOVE 04                 TO WS-2115-RETURN-CODE           P442288 
07203          IF TESTING-SHOW-DISPLAYS                                 P442288 
07204              DISPLAY ' @21153-- SKIP RTS TOO OLD TITLE DATE '     P442288 
07205                  WS-VIN-TTLDATE ' LESS THAN '                     P442288 
07206                  WRKRTS-LAST-CHG-DT ' (VEHL SVC)'                 P442288 
07207      END-IF.                                                      P442288 
07208      IF TESTING-SHOW-DISPLAYS                                     P442288 
07209          DISPLAY ' @21153-FIND-RVIN-EXIT RC=' WS-2115-RETURN-CODE P442288 
07210              ' FOR VIN =' WS-VIN-VIN                              P442288 
07211          DISPLAY ' ACTIVE # =' WS-VIN-ACTIVE-COUNT                P442288 
07212              ' LAST FLAG =' WS-VIN-MORE-IND                       P442288 
07213              ' DOCNO=' WS-VIN-DOCNO                               P442288 
07214              ' TTL CODE=' WS-VIN-TTLPROCSCD                       P442288 
07215      END-IF.                                                      P442288 
07216                                                                   P442288 
07217  21153-FIND-RTS-BY-RVIN-EXIT.                                     P442288 
07218      EXIT.                                                        P442288 
07219                                                                   P442288 
07220  211531-INQ-RVIN-FETCH.                                           P442288 
07221                                                                   P442288 
07222      ADD 1                       TO WS-FETCH-COUNT.               P442288 
07223      ADD 1                       TO WS-VIN-ACTIVE-COUNT.          P442288 
07224                                                                   P442288 
07225      IF TESTING-SHOW-DISPLAYS                                     P442288 
07226          DISPLAY ' @211531-INQ-RVIN-FETCH # ' WS-FETCH-COUNT.     P442288 
07227                                                                   P442288 
07228 *---------------------------------------------------------------* P442288 
07229 *    OVERRIDE DBID'S HERE .. USED TO TEST WITH "PRD2" OR "TST5" * P442288 
07230 *      USE INQUIRY DBID FOR RTS-TITLE AND RTS-INACT-TITLE.      * P442288 
07231 *---------------------------------------------------------------* P442288 
07232                                                                   P442288 
07233      EXEC ADABAS                                                  P442288 
07234           FETCH RVIN                                              P442288 
07235      END-EXEC                                                     P442288 
07236      .                                                            P442288 
07237                                                                   P442288 
07238      IF TESTING-SHOW-DISPLAYS                                     P442288 
07239          DISPLAY ' @211531-INQ-RVIN....AFTER "FETCH RVIN" '       P442288 
07240              ' WHERE ADACODE = ' ADACODE                          P442288 
07241              ' FOR VIN ' WS-VIN-VIN 'DOCNO = '                    P442288 
07242          DISPLAY ' DOCNO = ' DOCNO OF ACCESS-BY-RVIN              P442288 
07243              ' ' VIN OF ACCESS-BY-RVIN                            P442288 
07244          DISPLAY ' TITL DATE ' TTLISSUEDATE OF ACCESS-BY-RVIN     P442288 
07245              ' TITL CODE ' TTLPROCSCD   OF ACCESS-BY-RVIN         P442288 
07246      END-IF.                                                      P442288 
07247                                                                   P442288 
07248      IF ADACODE = 3                                               P442288 
                                                                                
      D        DISPLAY '!TC114 - IF ADACODE = 3 <' ADACODE '>'                  
                                                                                
07249          MOVE 'L'                     TO WS-VIN-MORE-IND          P442288 
07250          SUBTRACT 1                   FROM WS-VIN-ACTIVE-COUNT    P442288 
07251      ELSE                                                         P442288 
07252      IF ADACODE NOT EQUAL ZERO                                    P442288 
07253          MOVE 4                       TO WS-2115-RETURN-CODE      P442288 
07254      ELSE                                                         P442288 
07255      IF TTLPROCSCD OF ACCESS-BY-RVIN NOT = 'S' AND                P442288 
07256         TTLPROCSCD OF ACCESS-BY-RVIN NOT = 'I'                    P442288 
                                                                                
      D        DISPLAY '!TC115 -  IF TTLPROCSCD OF ACCESS-BY-RVIN'              
      D                ' NOT = S AND NOT = I <'                                 
      D                TTLPROCSCD OF ACCESS-BY-RVIN '>'                         
                                                                                
07257          MOVE DOCNO OF ACCESS-BY-RVIN      TO WS-THIS-VIN-DOCNO   P442288 
07258          IF WS-THIS-VIN-AM-DT-TM  >  WS-VIN-AM-DT-TM              P442288 
                                                                                
      D        DISPLAY '!TC116 -  IF WS-THIS-VIN-AM-DT-TM <'                    
      D                              WS-THIS-VIN-AM-DT-TM '>  >  '              
      D                'WS-VIN-AM-DT-TM <' WS-VIN-AM-DT-TM '>'                  
                                                                                
07259              MOVE WS-VIN-DOCNO        TO RTS-TTL-SKIPPED-OLD-DOCNOP442288 
07260              MOVE ACCESS-BY-RVIN      TO WS-BEST-VIN-DATA-HOLD    P442288 
07261          ELSE                                                     P442288 
                                                                                
      D        DISPLAY '!TC117 -  IF NOT WS-THIS-VIN-AM-DT-TM <'                
      D                              WS-THIS-VIN-AM-DT-TM '>  >  '              
      D                'WS-VIN-AM-DT-TM <' WS-VIN-AM-DT-TM '>'                  
                                                                                
07262              MOVE WS-THIS-VIN-DOCNO   TO RTS-TTL-SKIPPED-OLD-DOCNOP442288 
07263          END-IF                                                   P442288 
07264      ELSE                                                         P442288 
07265      IF WS-FETCH-COUNT = 1                                        P442288 
                                                                                
      D        DISPLAY '!TC118 - IF WS-FETCH-COUNT = 1 <'                       
      D                             WS-FETCH-COUNT '>'                          
                                                                                
07266          MOVE DOCNO OF ACCESS-BY-RVIN      TO WS-VIN-DOCNO        P442288 
07267          MOVE TTLPROCSCD OF ACCESS-BY-RVIN TO WS-VIN-TTLPROCSCD   P442288 
07268          MOVE TTLISSUEDATE OF ACCESS-BY-RVIN TO WS-VIN-TTLDATE    P442288 
07269          MOVE ACCESS-BY-RVIN            TO WS-BEST-VIN-DATA-HOLD  P442288 
07270                                                                   P442288 
07271      END-IF.                                                      P442288 
07272                                                                   P442288 
07273      IF TTLPROCSCD OF ACCESS-BY-RVIN  = 'S' OR 'I'                P442288 
                                                                                
      D    DISPLAY '!TC119 - IF TTLPROCSCD OF ACCESS-BY-RVIN = S OR = I'        
      D                    ' <' TTLPROCSCD OF ACCESS-BY-RVIN '>'                
                                                                                
07274          MOVE DOCNO OF ACCESS-BY-RVIN TO RTS-TTL-SKIPPED-OLD-DOCNOP442288 
07275          SUBTRACT 1              FROM WS-VIN-ACTIVE-COUNT.        P442288 
07276                                                                   P442288 
07277      IF WS-FETCH-COUNT = QUANTITY OF ACCESS-BY-RVIN               P442288 
                                                                                
      D    DISPLAY '!TC120 - IF WS-FETCH-COUNT <' WS-FETCH-COUNT '>  = '        
      D            ' QUANTITY OF ACCESS-BY-RVIN <'                              
      D              QUANTITY OF ACCESS-BY-RVIN '>'                             
                                                                                
07278          MOVE 'L'                     TO WS-VIN-MORE-IND.         P442288 
07279                                                                   P442288 
07280  211531-INQ-RVIN-FETCH-EXIT.                                      P442288 
07281      EXIT.                                                        P442288 
07282      EJECT                                                        P442288 
07283                                                                   P442288 
07284      EJECT                                                        P442288 
07285      EJECT                                                        P442288 
07286  2116-UPDATE-IRP-DOCNO.                                           P442288 
07287                                                                   P442288 
07288                                                                   P442288 
07289 *---------------------------------------------------------------* P442288 
07290 * CALL CEEDAYS TO CONVERT CURRENT DATE TO LILIAN REPRESENTATION.* P442288 
07291 *---------------------------------------------------------------* P442288 
07292      MOVE 8                  TO VSTRING-LENGTH OF IN-DATE.        P442288 
07293      MOVE WRKRTS-LAST-CHG-DT TO VSTRING-TEXT OF IN-DATE(1:8).     P442288 
07294      MOVE 8                  TO VSTRING-LENGTH OF PICSTR.         P442288 
07295      MOVE 'YYYYMMDD'         TO VSTRING-TEXT OF PICSTR(1:8).      P442288 
      D    DISPLAY 'WRKRTS-LAST-CHG-DT <' WRKRTS-LAST-CHG-DT '>'                
07296                                                                   P442288 
07297      CALL 'CEEDAYS' USING IN-DATE                                 P442288 
07298                           PICSTR                                  P442288 
07299                           LILIAN                                  P442288 
07300                           FC.                                     P442288 
07301                                                                   P442288 
07302      IF CEE000 OF FC                                              P442288 
      D        MOVE LILIAN TO WS-LILIAN                                         
      D        DISPLAY 'WS-LILIAN <' WS-LILIAN '>'                              
07304      ELSE                                                         P442288 
07305          DISPLAY ' ' WS-THIS-PGM ' CEEDAYS FAILED WITH MSG '      P442288 
07306              MSG-NO OF FC                                         P442288 
07307          CALL 'ILBOABN0'.                                         P442288 
07308                                                                   P442288 
07309 *---------------------------------------------------------------* P442288 
07310 * SPECIFY PICTURE STRING THAT DESCRIBES THE DESIRED FORMAT OF   * P442288 
07311 * THE OUTPUT FROM CEEDATE AND THE PICTURE STRING LENGTH.        * P442288 
07312 *---------------------------------------------------------------* P442288 
07313      MOVE 5                  TO VSTRING-LENGTH OF PICSTR.         P442288 
07314      MOVE 'YYDDD'            TO VSTRING-TEXT OF PICSTR(1:5).      P442288 
07315                                                                   P442288 
07316 *---------------------------------------------------------------* P442288 
07317 * CALL CEEDATE TO CONVERT THE LILIAN DATE TO A PICTURE STRING.  * P442288 
07318 *---------------------------------------------------------------* P442288 
      D        MOVE LILIAN TO WS-LILIAN                                         
      D        DISPLAY 'BEFORE CEEDATE - WS-LILIAN <' WS-LILIAN '>'             
      D        DISPLAY 'CHRDATE <' CHRDATE '>'                                  
07319      CALL 'CEEDATE' USING LILIAN                                  P442288 
07320                           PICSTR                                  P442288 
07321                           CHRDATE                                 P442288 
07322                           FC.                                     P442288 
07323                                                                   P442288 
07324      IF CEE000 OF FC                                              P442288 
07325          MOVE CHRDATE            TO WS-CRNT-DATE-YYDDD            P442288 
      D        MOVE LILIAN TO WS-LILIAN                                         
      D        DISPLAY 'AFTER CEEDATE - WS-LILIAN <' WS-LILIAN '>'              
      D        DISPLAY 'CHRDATE <' CHRDATE '>'                                  
07326      ELSE                                                         P442288 
07327          DISPLAY ' ' WS-THIS-PGM ' CEEDATE FAILED WITH MSG '      P442288 
07328              MSG-NO OF FC                                         P442288 
07329          CALL 'ILBOABN0'.                                         P442288 
07330                                                                   P442288 
07331 *---------------------------------------------------------------* P442288 
07332 *    BUILD-RTS-TRANSACTION-AMDATE                               * P442288 
07333 *---------------------------------------------------------------* P442288 
07334 *---------------------------------------------------------------* P442288 
07335 *      ALLOW 30 DAY BACK WINDOW TO VALIDATE AMDATES             * P442288 
07336 *---------------------------------------------------------------* P442288 
                                                                                
      D    DISPLAY '!TCXXP - WRKRTS-LAST-CHG-YR <'                              
      D                      WRKRTS-LAST-CHG-YR '>'                             
      D    DISPLAY 'WS-CRNT-DATE-DDD <' WS-CRNT-DATE-DDD '>'                    
      D    DISPLAY 'WS-DAYS-TO-01-01-1900 <' WS-DAYS-TO-01-01-1900 '>'          
                                                                                
07337      COMPUTE WS-DAYS-SINCE-01-01-0000 =                           P442288 
07338          (WRKRTS-LAST-CHG-YR * 365) + WS-CRNT-DATE-DDD - 1        P442288 
07339          + ((WRKRTS-LAST-CHG-YR - 1) / 4)                         P442288 
07340          - ((WRKRTS-LAST-CHG-YR - 1) / 100)                       P442288 
07341          + ((WRKRTS-LAST-CHG-YR - 1) / 400).                      P442288 
07342      COMPUTE WS-DAYS-SINCE-01-01-1900 =                           P442288 
07343          WS-DAYS-SINCE-01-01-0000 - WS-DAYS-TO-01-01-1900.        P442288 
07344      MOVE WS-DAYS-SINCE-01-01-1900 TO WS-IRP-VEHL-UPDATE-AMDATE.  P442288 
07345      SUBTRACT 30                   FROM WS-IRP-VEHL-UPDATE-AMDATE.P442288 
07346                                                                   P442288 
07347 *---------------------------------------------------------------* P442288 
07348      IF TESTING-SHOW-DISPLAYS                                     P442288 
07349          DISPLAY ' IRPV-UPDATE = ' WRKRTS-LAST-CHG-DT             P442288 
07350                 ' = AMDATE: ' WS-IRP-VEHL-UPDATE-AMDATE           P442288 
07351              ' <DOC AMDATE: ' WS-VIN-AMDT-DT '>'                  P442288 
07352              ' <TRANSAMDATE: ' WS-TRANSAMDATE '>'                 P442288 
07353      END-IF.                                                      P442288 
07354 *---------------------------------------------------------------* P442288 
07355                                                                   P442288 
07356      SUBTRACT 10                FROM WS-IRP-VEHL-UPDATE-AMDATE    P442288 
07357                                 GIVING WS-IRP-VEHL-UPDATE-WINDOW. P442288 
07358      IF WS-IRP-VEHL-UPDATE-WINDOW  > WS-VIN-AMDT-DT               P442288 
                                                                                
      D            DISPLAY '!TC132 - IF WS-IRP-VEHL-UPDATE-WINDOW <'            
      D                    WS-IRP-VEHL-UPDATE-WINDOW '>  >  '                   
      D                'WS-VIN-AMDT-DT <' WS-VIN-AMDT-DT '>'                    
                                                                                
07359          IF TESTING-SHOW-DISPLAYS                                 P442288 
07360              DISPLAY ' BYPASS IRP VEHICLE UNIT UPDATE'            P442288 
07361              DISPLAY ' VEH UPDATED ' WS-IRP-VEHL-UPDATE-AMDATE    P442288 
07362                 ' AFTER  VIN CREATED ' WS-VIN-AMDT-DT             P442288 
07363          END-IF                                                   P442288 
07364          GO TO 2116-UPDATE-IRP-DOCNO-EXIT                         P442288 
07365      ELSE                                                         P442288 
07366          IF TESTING-SHOW-DISPLAYS                                 P442288 
07367              DISPLAY ' UPDATE IRP VEHICLE UNIT RECORD'            P442288 
07368              DISPLAY ' VEH UPDATED ' WS-IRP-VEHL-UPDATE-AMDATE    P442288 
07369                 ' BEFORE VIN CREATED ' WS-VIN-AMDT-DT             P442288 
07370          END-IF                                                   P442288 
07371      END-IF.                                                      P442288 
07372                                                                   P442288 
07373                                                                   P442288 
07374      MOVE WRKRTS-OWNER-OPERATOR    TO ERROR-MSG-029-OWNER-O.      P442288 
07375      MOVE WS-OWNRTTLNAME1          TO WRKRTS-OWNER-OPERATOR.      P442288 
07376      MOVE WS-VIN-DOCNO             TO WRKRTS-DOCNO.               P442288 
07377      MOVE WS-CRNT-DATE-DT          TO WRKRTS-LAST-CHG-DT.         P442288 
07378 *---------------------------------------------------------------* P442288 
07379 *    MOVE 'P442288'                TO WRKRTS-LAST-CHG-ID.       * P442288 
07380 *---------------------------------------------------------------* P442288 
07381      MOVE 'I'                      TO WRKRTS-PROGRAM-CHG-FLAG.       CL**4
07382      MOVE WRKRTS-OWNER-OPERATOR    TO ERROR-MSG-029-OWNER-N.      P442288 
07383      ADD 1                         TO WS-UPIRP-RECORDS-COUNT.     P442288 
07384      IF ERROR-MSG-028-DOCNO-O = ZEROS                             P442288 
                                                                                
      D            DISPLAY '!TC133 - IF ERROR-MSG-028-DOCNO-O = ZEROS <'        
      D                    ERROR-MSG-028-DOCNO-O '>'                            
                                                                                
07385          ADD 1                     TO WS-UPIRP-DUMMY-COUNT.       P442288 
07386      IF NO-UPDATES-RUN                                            P442288 
07387          IF TESTING-SHOW-DISPLAYS                                 P442288 
07388              DISPLAY ' BYPASS IRP VEHICLE FILE UPDATE'            P442288 
07389          END-IF                                                   P442288 
07390      END-IF.                                                      P442288 
07391                                                                   P442288 
                                                                                
      D    DISPLAY '!TC134 - 2116-UPDATE-IRP-DOCNO'                             
                                                                                
07392      MOVE 'UPDT'                   TO ERROR-MSG-033-FUNC.         P442288 
07393                                                                   P442288 
07394      MOVE 4                        TO ERROR-PROCESSING-SW.        P442288 
07395      MOVE WS-PLATE-NO-RTS          TO ERR-LICN.                   P442288 
07396      MOVE ERROR-MSG-029-TEXT       TO ERR-ERROR-MESSAGE-1.        P442288 
07397      PERFORM Z-5-WRITE-UPDATE-REPORT.                             P442288 
07398      MOVE SPACES                   TO  ERR-LICN.                  P442288 
07399                                                                   P442288 
07400  2116-UPDATE-IRP-DOCNO-EXIT.                                      P442288 
07401      EXIT.                                                        P442288 
07402                                                                   P442288 
07403  2117-CHECK-DOCNO-VIN.                                            P442288 
07404                                                                   P442288 
07405 *---------------------------------------------------------------* P442288 
07406 *    THE VIN ON THE TITLE POINTED BY THE IRP STORED DOCNO       * P442288 
07407 *  OCCASIONALLY DOES NOT MATCH THE VIN STORED IN THE IRP RECORD.* P442288 
07408 *    IN THE CASE OF NEWLY TITLED RECORDS, THIS IS GENERALLY A   * P442288 
07409 *  BAD VIN KEYED IN BY THE REGIONAL OFFICE (THEY OVERRIDE THE   * P442288 
07410 *  CHECK-DIGIT HIGHLIGHT)                                       * P442288 
07411 *---------------------------------------------------------------* P442288 
07412      IF TESTING-SHOW-DISPLAYS                                     P442288 
07413          DISPLAY '@ 2117- CHECK-DOCNO-VIN ===>'                   P442288 
07414          DISPLAY  ' IRP-DOC==>' WRKRTS-DOCNO                      P442288 
07415            ' IRP-VIN==>' WRKRTS-IDENTIFICATION-NBR                P442288 
07416            ' GET-DOCNO==>' WS-RTS-DOCNO                           P442288 
07417            ' DOCNO-VIN==>' WS-VIN                                 P442288 
07418      END-IF.                                                      P442288 
07419                                                                   P442288 
                                                                                
      D    DISPLAY '!TCXXH -  WRKRTS-DOCNO <' WRKRTS-DOCNO '> '                 
      D                  ' WS-RTS-DOCNO <' WS-RTS-DOCNO '> '                    
      D    DISPLAY 'WS-VIN <' WS-VIN '> '                                       
      D            'WRKRTS-IDENTIFICATION-NBR <'                                
      D             WRKRTS-IDENTIFICATION-NBR '>'                               
                                                                                
07420      IF WRKRTS-DOCNO = WS-RTS-DOCNO                               P442288 
07421        IF WS-VIN NOT = WRKRTS-IDENTIFICATION-NBR                  P442288 
07422          IF RTS-SYNC-REG-UPDATE-RUN                               P442288 
                                                                                
      D          DISPLAY '!TC048 - '                                            
      D                  'WRKRTS-DOCNO <' WRKRTS-DOCNO '>'                      
      D                  ' = WS-RTS-DOCNO <' WS-RTS-DOCNO '>'                   
      D          DISPLAY 'WS-VIN NOT = WRKRTS-IDENTIFICATION-NBR'               
      D          DISPLAY 'WS-VIN <' WS-VIN '>  NOT = '                          
      D                  'WRKRTS-IDENTIFICATION-NBR <'                          
      D                   WRKRTS-IDENTIFICATION-NBR '>'                         
      D          DISPLAY 'AND IF RTS-SYNC-REG-UPDATE-RUN(S) <'                  
                               RTS-IF-RUN-TYPE '>'                              
      D          DISPLAY 'MOVE 1 TO RTS-TRANS-ERROR-FLAG'                       
                                                                                
07423            MOVE   1             TO RTS-TRANS-ERROR-FLAG           P442288 
07424          ELSE                                                     P442288 
07425 *---------------------------------------------------------------* P442288 
07426 *        ALLOW THIS TO GO THROUGH FOR NOW.  RTS TRANS WITH FUNDS* P442288 
07427 *        INFO WILL BE SENT TO RTS.  THE RECORD SHOULD HAVE BEEN * P442288 
07428 *        ON THE RTS DATABASE.  IT WILL BE REJECTED BY RTS WITH  * P442288 
07429 *        AN ERROR MESSAGE SENT TO THE REGIONAL OFFICE.  THIS    * P442288 
07430 *        WILL BE HANDLED BY VTRD TITLE CONTROL SECTION (TCS).   * P442288 
07431 *---------------------------------------------------------------* P442288 
                                                                                
      D          DISPLAY '!TC049 - '                                            
      D                  'IF WRKRTS-DOCNO <' WRKRTS-DOCNO '>'                   
      D                  ' = WS-RTS-DOCNO <' WS-RTS-DOCNO '>'                   
      D          DISPLAY 'AND IF WS-VIN <' WS-VIN '>'                           
      D                  ' NOT = WRKRTS-IDENTIFICATION-NBR <'                   
      D                          WRKRTS-IDENTIFICATION-NBR '>'                  
      D          DISPLAY 'AND IF RTS-SYNC-REG-UPDATE-RUN(S) <'                  
                              RTS-IF-RUN-TYPE '>'                               
      D          DISPLAY 'MOVE 0 TO RTS-TRANS-ERROR-FLAG'                       
                                                                                
07432            MOVE ZERO            TO RTS-TRANS-ERROR-FLAG           P442288 
07433          END-IF                                                   P442288 
07434 *---------------------------------------------------------------* P442288 
07435 *            SET  RTS-NO-TITLE        INDICATOR FLAG            * P442288 
07436 *            THIS WILL BE RESET IF PROPER DOCNO IS LOCATED LATER* P442288 
07437 *            MOVE IRP DATE INTO MVTRANS FIELDS                  * P442288 
07438 *---------------------------------------------------------------* P442288 
                                                                                
      D        DISPLAY '!TC050 - '                                              
      D                'IF WRKRTS-DOCNO <' WRKRTS-DOCNO '>'                     
      D                ' = WS-RTS-DOCNO <' WS-RTS-DOCNO '>'                     
      D        DISPLAY 'AND IF WS-VIN <' WS-VIN '>  NOT = '                     
      D                'WRKRTS-IDENTIFICATION-NBR <'                            
      D                 WRKRTS-IDENTIFICATION-NBR '>'                           
                                                                                
07439          MOVE 1                       TO RTS-UPDT-ERROR-FLAG      P442288 
07440          MOVE WS-RTS-DOCNO            TO ERROR-MSG-035-DOCNO      P442288 
07441          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
07442          MOVE ERROR-MSG-035-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
07443          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN            P442288 
07444          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
07445          MOVE WRKRTS-IDENTIFICATION-NBR                           P442288 
07446                                       TO ERROR-MSG-036-VIN        P442288 
07447          MOVE WRKRTS-DOCNO            TO ERROR-MSG-036-DOCNO      P442288 
07448          MOVE WS-RTS-DOCNO            TO ERROR-MSG-037-DOCNO      P442288 
07449          MOVE WS-VIN                  TO ERROR-MSG-037-VIN        P442288 
07450          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
07451          MOVE ERROR-MSG-036-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
07452          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN            P442288 
07453          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
07454          MOVE SPACES                  TO  ERR-LICN                P442288 
07455          MOVE ERROR-MSG-037-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
07456          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN            P442288 
07457          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
07458          MOVE SPACES                  TO  ERR-LICN                P442288 
07459        END-IF                                                     P442288 
07460      ELSE                                                         P442288 
07461        IF WS-VIN NOT = WRKRTS-IDENTIFICATION-NBR                  P442288 
07462          IF RTS-SYNC-REG-UPDATE-RUN                               P442288 
                                                                                
      D          DISPLAY '!TC051 - '                                            
      D                  'WS-VIN NOT = WRKRTS-IDENTIFICATION-NBR'               
      D          DISPLAY 'AND IF RTS-SYNC-REG-UPDATE-RUN'                       
      D          DISPLAY 'MOVE 1 TO RTS-TRANS-ERROR-FLAG'                       
                                                                                
07463            MOVE   1             TO RTS-TRANS-ERROR-FLAG           P442288 
07464          ELSE                                                     P442288 
07465 *---------------------------------------------------------------* P442288 
07466 *        ALLOW THIS TO GO THROUGH FOR NOW.  RTS TRANS WITH FUNDS* P442288 
07467 *        INFO WILL BE SENT TO RTS.  THE RECORD SHOULD HAVE BEEN * P442288 
07468 *        ON THE RTS DATABASE.  IT WILL BE REJECTED BY RTS WITH  * P442288 
07469 *        AN ERROR MESSAGE SENT TO THE REGIONAL OFFICE.  THIS    * P442288 
07470 *        WILL BE HANDLED BY VTRD TITLE CONTROL SECTION (TCS).   * P442288 
07471 *---------------------------------------------------------------* P442288 
07472            MOVE ZERO            TO RTS-TRANS-ERROR-FLAG           P442288 
                                                                                
      D          DISPLAY '!TC052 - '                                            
      D                  'WS-VIN NOT = WRKRTS-IDENTIFICATION-NBR'               
      D          DISPLAY 'AND IF not RTS-SYNC-REG-UPDATE-RUN'                   
      D          DISPLAY 'MOVE 0 TO RTS-TRANS-ERROR-FLAG'                       
                                                                                
07473          END-IF                                                   P442288 
                                                                                
      D        DISPLAY '!TC053 - IF NOT WRKRTS-DOCNO <'                         
      D                                 WRKRTS-DOCNO                            
      D                '> NOT WS-RTS-DOCNO <' WS-RTS-DOCNO '>'                  
      D        DISPLAY 'AND WS-VIN <' WS-VIN '> NOT = '                         
      D                    'WRKRTS-IDENTIFICATION-NBR <'                P442288 
      D                     WRKRTS-IDENTIFICATION-NBR '>'               P442288 
                                                                                
07474          MOVE 1                       TO RTS-UPDT-ERROR-FLAG      P442288 
07475          MOVE WS-RTS-DOCNO            TO ERROR-MSG-038-DOCNO      P442288 
07476          MOVE 4                       TO ERROR-PROCESSING-SW      P442288 
07477          MOVE ERROR-MSG-038-TEXT      TO ERR-ERROR-MESSAGE-1      P442288 
07478          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
07479          MOVE SPACES                  TO  ERR-LICN                P442288 
07480        END-IF                                                     P442288 
07480      END-IF.                                                      P442288 
07481                                                                   P442288 
07482  2118-CHECK-LONGEVITY.                                            P442288 
07483                                                                   P442288 
07484      IF TESTING-SHOW-DISPLAYS                                     P442288 
07485          DISPLAY '@ 2118- CHECK-LONGEVITY ===>'                   P442288 
07486          DISPLAY  ' IRP-DOC===>' WRKRTS-DOCNO                     P442288 
07487            ' IRP-VIN===>' WRKRTS-IDENTIFICATION-NBR               P442288 
07488            ' GET-DOCNO===>' WS-RTS-DOCNO                          P442288 
07489            ' DOCNO-VIN==>' WS-VIN                                 P442288 
07490      END-IF.                                                      P442288 
07491                                                                   P442288 
07492      IF WRKRTS-DOCNO-AMDATE > WS-RTS-DOCNO-AMDATE                 P442288 
07493              IF RTS-SYNC-REG-UPDATE-RUN                           P442288 
                                                                                
      D       DISPLAY '!TC127 - IF WRKRTS-DOCNO-AMDATE <'                       
      D                            WRKRTS-DOCNO-AMDATE '>  >  '                 
      D               'WS-RTS-DOCNO-AMDATE <' WS-RTS-DOCNO-AMDATE '>'           
      D       DISPLAY 'AND IF RTS-SYNC-REG-UPDATE-RUN(S) <'                     
                              RTS-IF-RUN-TYPE '>'                               
                                                                                
07494                  MOVE   1             TO RTS-TRANS-ERROR-FLAG     P442288 
07495              ELSE                                                 P442288 
07496 *---------------------------------------------------------------* P442288 
07497 *        ALLOW THIS TO GO THROUGH FOR NOW.  RTS TRANS WITH FUNDS* P442288 
07498 *        INFO WILL BE SENT TO RTS.  THE RECORD SHOULD HAVE BEEN * P442288 
07499 *        ON THE RTS DATABASE.  IT WILL BE REJECTED BY RTS WITH  * P442288 
07500 *        AN ERROR MESSAGE SENT TO THE REGIONAL OFFICE.  THIS    * P442288 
07501 *        WILL BE HANDLED BY VTRD TITLE CONTROL SECTION (TCS).   * P442288 
07502 *---------------------------------------------------------------* P442288 
                                                                                
      D       DISPLAY '!TC128 - IF WRKRTS-DOCNO-AMDATE <'                       
      D                            WRKRTS-DOCNO-AMDATE '>  >  '                 
      D               'WS-RTS-DOCNO-AMDATE <' WS-RTS-DOCNO-AMDATE '>'           
      D       DISPLAY 'AND IF NOT RTS-SYNC-REG-UPDATE-RUN(S) <'                 
                              RTS-IF-RUN-TYPE '>'                               
                                                                                
07503                  MOVE ZERO            TO RTS-TRANS-ERROR-FLAG     P442288 
07504              END-IF                                               P442288 
07505 *---------------------------------------------------------------* P442288 
07506 *            SET  RTS-NO-TITLE        INDICATOR FLAG            * P442288 
07507 *            THIS WILL BE RESET IF PROPER DOCNO IS LOCATED LATER* P442288 
07508 *            MOVE IRP DATE INTO MVTRANS FIELDS                  * P442288 
07509 *---------------------------------------------------------------* P442288 
                                                                                
      D            DISPLAY '!TC129:'                                            
      D            DISPLAY 'IF WRKRTS-DOCNO-AMDATE <'                           
      D                        WRKRTS-DOCNO-AMDATE '> > '                       
      D            DISPLAY 'WS-RTS-DOCNO-AMDATE <'                              
      D                     WS-RTS-DOCNO-AMDATE                                 
                                                                                
07510              MOVE 1                       TO RTS-UPDT-ERROR-FLAG  P442288 
07511              MOVE WRKRTS-IDENTIFICATION-NBR                       P442288 
07512                                           TO ERROR-MSG-036-VIN    P442288 
07513              MOVE WRKRTS-DOCNO            TO ERROR-MSG-036-DOCNO  P442288 
07514              MOVE WS-RTS-DOCNO            TO ERROR-MSG-037-DOCNO  P442288 
07515              MOVE WS-VIN                  TO ERROR-MSG-037-VIN    P442288 
07516              MOVE 4                       TO ERROR-PROCESSING-SW  P442288 
07517              MOVE ERROR-MSG-036-TEXT      TO ERR-ERROR-MESSAGE-1  P442288 
07518              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07519              MOVE ERROR-MSG-037-TEXT      TO ERR-ERROR-MESSAGE-1  P442288 
07520              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07521              MOVE WS-RTS-DOCNO            TO ERROR-MSG-039-DOCNO  P442288 
07522              MOVE 4                       TO ERROR-PROCESSING-SW  P442288 
07523              MOVE ERROR-MSG-039-TEXT      TO ERR-ERROR-MESSAGE-1  P442288 
07524              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07525              MOVE SPACES                  TO  ERR-LICN            P442288 
07526      END-IF.                                                      P442288 
07527                                                                   P442288 
07528  2119-SAME-VIN-EXCHANGE.                                          P442288 
                                                                                
      D    DISPLAY '!TC263 - 2119-SAME-VIN-EXCHANGE'                            
                                                                                
07529      MOVE 4                           TO ERROR-PROCESSING-SW.     P442288 
07530      MOVE ZERO                        TO RTS-TRANS-ERROR-FLAG.    P442288 
07531      MOVE ZERO                        TO RTS-UPDT-ERROR-FLAG.     P442288 
07532      MOVE WS-REGIS-RTS-REGPLTNO       TO ERROR-MSG-016-PLT.          CL**9
07533      MOVE WS-REGIS-RTS-REG-EXP-DT     TO ERROR-MSG-016-EXP-DT.       CL**9
07534      MOVE WS-RTS-DOCNO                TO ERROR-MSG-016-DOCNO.     P442288 
07535      MOVE WS-PLATE-NO-RTS             TO ERR-LICN.                P442288 
07536      MOVE 'SAME VIN   '               TO ERROR-MSG-016-NOW.       P442288 
07537      MOVE ERROR-MSG-016-TEXT          TO ERR-ERROR-MESSAGE-1.     P442288 
07538      PERFORM Z-V-WRITE-MVUPDT-REPORT.                             P442288 
07539      MOVE SPACES                      TO  ERR-LICN.               P442288 
07540                                                                   P442288 
07541      EJECT                                                        P442288 
07542                                                                   P442288 
07543  22-DELETE-BATCH-RECORD.                                          P442288 
07544 *---------------------------------------------------------------* P442288 
07545 *     THE BATCH RECORD WILL REMAIN ON THE FILE IF INTERFACE     * P442288 
07546 *     RECORDS ARE LEFT TO BE PROCESSED, DUE TO LACT OF A        * P442288 
07547 *     A TITLE RECORD ON THE RTS_TITLE FILE.  AFTER THESE HAVE   * P442288 
07548 *     REMAINED ON THE FILE FOR A MAXIMUM NUMBER OF DAYS, THEN   * P442288 
07549 *     THEY ARE DELETED.                                         * P442288 
07550 *---------------------------------------------------------------* P442288 
07551      IF BATCH-NO-TITLE                                            P442288 
07552          MOVE WS-BATCH-NOTITLE-ERRS  TO ERROR-MSG-013-NUMB        P442288 
07553          MOVE ERROR-MSG-013-TEXT     TO ERR-ERROR-MESSAGE-1       P442288 
07554          PERFORM  Z-4-CONTROL-EDIT-ERROR                          P442288 
07555      ELSE                                                         P442288 
07556          DELETE  IRP-BATCH-FILE                                   P442288 
07557          ADD 1                     TO WS-RTS-NO-PR-BATCHES        P442288 
     D         DISPLAY 'INSIDE 22-DELETE-BATCH-RECORD'                          
07558          PERFORM Z-6-VSAM-FILE-STATUS-CK.                         P442288 
07559      EJECT                                                        P442288 
07560 ******************************************************************P442288 
07561 *              ALL Z MODULES FOLLOW BELOW                        *P442288 
07562 ******************************************************************P442288 
07563                                                                   P442288 
07564  Z-1-READ-IRP-BATCH-FILE.                                         P442288 
07565                                                                   P442288 
07566      READ IRP-BATCH-FILE NEXT INTO IRP-BATCH-REC                  P442288 
07567      AT END                                                       P442288 
                                                                                
      D        DISPLAY 'END OF FILE REACHED FOR IRP-BATCH-FILE'                 
      D        DISPLAY '!TC007A - END OF FILE REACHED'                          
                                                                                
07568          MOVE 'Y' TO BATCH-RECORD-SW.                             P442288 
                                                                                
      D    DISPLAY 'INSIDE Z-1-READ-IRP-BATCH-FILE'                             
                                                                                
07569      PERFORM Z-6-VSAM-FILE-STATUS-CK.                             P442288 
07570                                                                   P442288 
                                                                                
      D    DISPLAY ' '                                                          
      D    DISPLAY '**************************************************'         
      D            '  NEW IRP-BATCH-FILE RECORD '                               
      D            '**************************************************'         
      D    DISPLAY ' '                                                          
                                                                                
07571      IF BATCH-RECORD-SW NOT = 'Y'      AND                        P442288 
07572         BATCH-BILLING-RECAP-DEP = 'R'                             P442288 
                                                                                
      D    DISPLAY '!TC007B - IF BATCH-RECORD-SW NOT = Y <'                     
      D          BATCH-RECORD-SW '> AND BATCH-BILLING-RECAP-DEP = R <'          
      D          BATCH-BILLING-RECAP-DEP '>'                                    
                                                                                
07573          ADD 1                     TO WS-RTS-NO-BATCHES.          P442288 
07574                                                                   P442288 
07575  Z-2-READ-IRP-RTS-FILE.                                           P442288 
07576      READ  WORK-FD-INTERFACE-FILE NEXT                            P442288 
07577             INTO WRKRTS-INTERFACE-RECORD.                         P442288 
07578      PERFORM Z-7-WRKRTS-FILE-STATUS-CK.                           P442288 
07579      IF WRKRTS-FILE-STATUS NOT = ZERO                             P442288 
07580          GO TO Z-2-READ-IRP-RTS-FILE-EXIT.                        P442288 
07581      IF WRKRTS-REGIS-DT = 999999                                  P442288 
07582          IF WRKRTS-SUPP-NBR = 999                                 P442288 
07583              IF WRKRTS-ACCOUNT-NBR = ALL '9'                      P442288 
07584                  IF TESTING-SHOW-DISPLAYS                         P442288 
07585                      DISPLAY 'END OF IRPRTS DATA = ' WRKRTS-KEY   P442288 
07586                  END-IF                                           P442288 
07587                  GO TO Z-2-READ-IRP-RTS-FILE.                     P442288 
07588      IF TESTING-SHOW-DISPLAYS                                     P442288 
07589          DISPLAY '==============================================='P442288 
                       ' READ  WORK-FD-INTERFACE-FILE '                         
07589          DISPLAY '@Z-2  IRP-RTS RECORD READ  OK===>'              P442288 
07590              ' RTS-KEY====<' WORK-FD-KEY '>'                      P442288 
07591          DISPLAY 'SUP=<' WRKRTS-SUPP-NBR '>'                      P442288 
07592          DISPLAY 'DT =<' WRKRTS-REGIS-DT '>'                      P442288 
07593          DISPLAY 'ACT=<' WRKRTS-ACCOUNT-NBR '>'                   P442288 
07594          DISPLAY 'FLT=<' WRKRTS-FLEET-NBR '>'                     P442288 
07595          DISPLAY 'SFX=<' WRKRTS-SUFFIX-NBR '>'                    P442288 
07596          DISPLAY 'ACCT TYPE=<' WRKRTS-ACT-TYPE '>'                   CL*17
07597          DISPLAY 'BILL=<' WRKRTS-TYPE-OF-CHANGE-FLAG '>'          P442288 
07598          DISPLAY 'RGN=<'  WRKRTS-REGION '>'                       P442288 
07599          DISPLAY '# <'   WRKRTS-REGION-NUMBER '>'                 P442288 
07600          DISPLAY 'EQP=<' WRKRTS-OWNER-EQUIP-NBR '>'               P442288 
07601          DISPLAY 'FLG=<' WRKRTS-PROCESSING-FLAG '>'               P442288 
07602          DISPLAY 'LST UPD=<' WRKRTS-LAST-CHG-DT '>'               P442288 
07603          DISPLAY 'LIC NBR =<' WRKRTS-CURRENT-TEXAS-LIC-NBR '> '   P442288 
07604          DISPLAY 'WRKRTS--ITMCD <' WRKRTS-FLEET-PLT-ITMCD '>'        CL*17
07605          DISPLAY 'WRKRTS--REGCL <' WRKRTS-FLEET-REGCL-CD '>'         CL*17
07606          DISPLAY 'WRKRTS-REG-EXP-DT <' WRKRTS-REG-EXP-DT '>'      P442288 
07606          DISPLAY 'WRKRTS-BEGINNING-REG-DT <'                      P442288 
07606                                        WRKRTS-BEGINNING-REG-DT '>'P442288 
07607          DISPLAY 'WRKRTS-DOCNO <'  WRKRTS-DOCNO '>'               P442288 
07607          DISPLAY 'WRKRTS-IDENTIFICATION-NBR <'                    P442288 
07607                                WRKRTS-IDENTIFICATION-NBR '>'      P442288 
07608          DISPLAY ' CHG TYPE <' WRKRTS-TYPE-OF-CHANGE-FLAG '>'     P442288 
07609          DISPLAY ' APP= $<' WRKRTS-APPORTIONED-FEE '>'            P442288 
07610          DISPLAY 'WRKRTS-DOCNO <' WRKRTS-DOCNO '>'                   CL**4
07610          DISPLAY 'WRKRTS-PROGRAM-CHG-FLAG <'                         CL**4
0 610                        WRKRTS-PROGRAM-CHG-FLAG '>'                   CL**4
07611          DISPLAY 'WRKRTS-REPLACED-DOCNO <' WRKRTS-REPLACED-DOCNO  P442288 
07611                        WRKRTS-REPLACED-DOCNO '>'                  P442288 
               DISPLAY 'WRKRTS-CNTY-ADD-ON-FEE <'                               
                             WRKRTS-CNTY-ADD-ON-FEE '>'                         
07589          DISPLAY '==============================================='P442288 
 7612      END-IF.                                                      P442288 
07613                                                                   P442288 
07614 *---------------------------------------------------------------* P442288 
07615 *    SYSTEM CHANGED:  ALL COUNTY NUMBERS REFLECT THE            * P442288 
07616 *    COMPTROLLER COUNTY NUMBERING SYSTEM.                       * P442288 
07617 *----                                                           * P442288 
07618 *    IF WRKRTS-FILE-STATUS = ZEROS                              * P442288 
07619 *        MOVE COMPT-COUNTY-NUMBER (WRKRTS-COUNTY)               * P442288 
07620 *                                  TO WS-COMPT-COUNTY           * P442288 
07621 *        IF TX-HWY-COUNTY-NUMBER (WRKRTS-COUNTY)                * P442288 
07622 *            NOT EQUAL WRKRTS-COUNTY                            * P442288 
07623 *            DISPLAY ' TX COMPT COUNTY TABLE OUT OF SYNC AT TX' * P442288 
07624 *                ' COUNTY ' WRKRTS-COUNTY ' NOT = # '           * P442288 
07625 *                TX-HWY-COUNTY-NUMBER (WRKRTS-COUNTY) ' '       * P442288 
07626 *                COUNTY-NAME (WRKRTS-COUNTY).                   * P442288 
07627 *---------------------------------------------------------------* P442288 
07628      IF WRKRTS-FILE-STATUS = ZEROS                                P442288 
                                                                                
      D    DISPLAY '!TC010 - INSIDE Z-2-READ-IRP-RTS-FILE'                      
                                                                                
07629          MOVE WRKRTS-COUNTY        TO WS-COMPT-COUNTY.            P442288 
07630                                                                   P442288 
07631      MOVE WRKRTS-DOCNO                     TO WS-RTS-DOCNO.       P442288 
07632                                                                   P442288 
07633      IF WS-RTS-DOCNO-NUM NOT NUMERIC                              P442288 
                                                                                
      D    DISPLAY '!TC011 - IF WS-RTS-DOCNO-NUM NOT NUMERIC <'                 
      D                         WS-RTS-DOCNO-NUM '>'                            
                                                                                
07634          MOVE 1                            TO ERROR-PROCESSING-SW P442288 
07635          MOVE WS-RTS-DOCNO                 TO ERROR-MSG-022-DOCNO P442288 
07636          MOVE WRKRTS-CURRENT-TEXAS-LIC-NBR TO ERR-LICN            P442288 
07637          MOVE ERROR-MSG-022-TEXT           TO ERR-ERROR-MESSAGE-1 P442288 
07638          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
07639          MOVE SPACES                       TO ERR-LICN            P442288 
07640          MOVE ZEROS                        TO WS-RTS-DOCNO.       P442288 
07641                                                                   P442288 
07642  Z-2-READ-IRP-RTS-FILE-EXIT.                                      P442288 
07643      EXIT.                                                        P442288 
07644                                                                   P442288 
07645  Z-3-REWRITE-RTS-FILE.                                            P442288 
07646                                                                   P442288 
                                                                                
      D    DISPLAY '!TC230 - Z-3-REWRITE-RTS-FILE'                              
                                                                                
07647      REWRITE WORK-FD-RECORD FROM WRKRTS-INTERFACE-RECORD.         P442288 
07648      PERFORM Z-7-WRKRTS-FILE-STATUS-CK.                           P442288 
07649                                                                   P442288 
07650                                                                   P442288 
07651  Z-4-CONTROL-EDIT-ERROR.                                          P442288 
07652                                                                   P442288 
07653      IF TESTING-SHOW-DISPLAYS                                     P442288 
07654          DISPLAY '@ Z-4--ERROR :'                                 P442288 
07655                  ' ERR SW = ' ERROR-PROCESSING-SW                 P442288 
07656                  ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG          P442288 
07657                  ' UPDT ERR FL = ' RTS-UPDT-ERROR-FLAG            P442288 
07658                  ' ERROR NUMBER = ' ERR-ERROR-NUMBER.             P442288 
07659                                                                   P442288 
07660      MOVE SPACES                   TO ERR-NON-UPD.                P442288 
07661      MOVE SPACES                   TO ERR-MESS-DATA.              P442288 
07662      IF  ERR-LINE-CTR > 50                                        P442288 
07663          MOVE ZERO                 TO ERR-LINE-CTR                P442288 
07664          ADD 1                     TO ERR-PAGE-CTR                P442288 
07665          MOVE WS-CURRENT-DATE-DT   TO ERROR-CURRENT-DT            P442288 
07666          MOVE ERR-PAGE-CTR         TO ERROR-PAGE-NUMBER           P442288 
07667          MOVE 1                    TO PROPER-SPACING              P442288 
07668          MOVE ERROR-HDR-1          TO PROCESS-CONTROL-REP-REC     P442288 
07669          PERFORM Z-41-WRITE-CONTROL-ERROR                         P442288 
07670                                                                   P442288 
07671          MOVE SPACES               TO PROPER-SPACING              P442288 
07672          MOVE ERROR-HDR-1-RUNNING  TO PROCESS-CONTROL-REP-REC     P442288 
07673          PERFORM Z-41-WRITE-CONTROL-ERROR                         P442288 
07674          MOVE SPACES               TO PROCESS-CONTROL-REP-REC     P442288 
07675          PERFORM Z-41-WRITE-CONTROL-ERROR                         P442288 
07676                                                                   P442288 
07677          MOVE SPACES               TO PROPER-SPACING              P442288 
07678          MOVE ERROR-HDR-2          TO PROCESS-CONTROL-REP-REC     P442288 
07679          PERFORM Z-41-WRITE-CONTROL-ERROR                         P442288 
07680          MOVE ZERO                 TO PROPER-SPACING              P442288 
07681          MOVE ERROR-HDR-3          TO PROCESS-CONTROL-REP-REC     P442288 
07682          PERFORM Z-41-WRITE-CONTROL-ERROR                         P442288 
07683          MOVE SPACES               TO PROPER-SPACING              P442288 
07684          MOVE ERROR-HDR-4          TO PROCESS-CONTROL-REP-REC     P442288 
07685          PERFORM Z-41-WRITE-CONTROL-ERROR                         P442288 
07686          MOVE ERROR-HDR-5          TO PROCESS-CONTROL-REP-REC     P442288 
07687          PERFORM Z-41-WRITE-CONTROL-ERROR                         P442288 
07688          MOVE ERROR-HDR-5B         TO PROCESS-CONTROL-REP-REC     P442288 
07689          PERFORM Z-41-WRITE-CONTROL-ERROR                         P442288 
07690          MOVE SPACES               TO PROCESS-CONTROL-REP-REC     P442288 
07691          PERFORM Z-41-WRITE-CONTROL-ERROR.                        P442288 
07692                                                                   P442288 
07693      MOVE SPACES                   TO ERR-KEY.                    P442288 
07694      IF BATCH-CNTL-ERROR                                          P442288 
07695          MOVE BATCH-KEY            TO ERR-MESS-DATA               P442288 
07696          MOVE SPACES               TO ERR-MESS-REGION             P442288 
07697          MOVE BATCH-REGIS-DT          TO ERR-ACCOUNT-DT           P442288 
07698          MOVE BATCH-ACCOUNT-NUMBER TO ERR-ACCOUNT-NUMBER          P442288 
07699          MOVE BATCH-FLEET-NBR      TO ERR-FLEET-NUMBER            P442288 
07700          MOVE BATCH-SUPPLEMENT-NUMBER TO ERR-SUPPLEMENT-NUMBER    P442288 
07701      ELSE                                                         P442288 
07702      IF  SHOW-RGN-IF-KEY-ON-ERROR      OR                         P442288 
07703          RTS-INVALID-UPDATE                                       P442288 
07704          MOVE WS-TRANS-KEY            TO ERR-MESS-DATA            P442288 
07705          MOVE WS-RGN-OFFICE           TO ERR-MESS-REGION          P442288 
07706          MOVE BATCH-REGIS-DT          TO ERR-ACCOUNT-DT           P442288 
07707          MOVE BATCH-ACCOUNT-NUMBER    TO ERR-ACCOUNT-NUMBER       P442288 
07708          MOVE BATCH-FLEET-NBR         TO ERR-FLEET-NUMBER         P442288 
07709          MOVE BATCH-SUPPLEMENT-NUMBER TO ERR-SUPPLEMENT-NUMBER    P442288 
07710          IF ERR-ERROR-NUMBER = 629                                P442288 
07711              IF EMSG29-FIRST-10-O NOT = EMSG29-FIRST-10-N         P442288 
07712                  MOVE '**'            TO ERR-NON-UPD              P442288 
07713              ELSE                                                 P442288 
07714                  MOVE ' +'            TO ERR-NON-UPD              P442288 
07715          ELSE                                                     P442288 
07716          IF  RTS-INVALID-UPDATE                                   P442288 
07717              MOVE '**'                TO ERR-NON-UPD              P442288 
07718          ELSE                                                     P442288 
07719              MOVE SPACES              TO ERR-NON-UPD.             P442288 
07720                                                                   P442288 
07721                                                                   P442288 
07722      MOVE SPACES                   TO PROPER-SPACING.             P442288 
07723      MOVE ERROR-DET-1              TO PROCESS-CONTROL-REP-REC     P442288 
07724                                       WS-TRANS-DIAG.              P442288 
07725      PERFORM Z-41-WRITE-CONTROL-ERROR.                            P442288 
07726      ADD 1                          TO ERR-LINE-CTR.              P442288 
07727                                                                   P442288 
07728  Z-41-WRITE-CONTROL-ERROR.                                        P442288 
07729                                                                   P442288 
07730      EVALUATE  PROPER-SPACING                                     P442288 
07731          WHEN '1'                                                 P442288 
07732            WRITE PROCESS-CONTROL-REP-REC                          P442288 
07733               AFTER ADVANCING PAGE                                P442288 
07734          WHEN '0'                                                 P442288 
07735            WRITE PROCESS-CONTROL-REP-REC                          P442288 
07736               AFTER ADVANCING 2 LINES                             P442288 
07737          WHEN OTHER                                               P442288 
07738            WRITE PROCESS-CONTROL-REP-REC                          P442288 
07739               AFTER ADVANCING 1 LINE                              P442288 
07740      END-EVALUATE.                                                P442288 
07741                                                                   P442288 
07742                                                                   P442288 
07743                                                                   P442288 
07744  Z-5-WRITE-UPDATE-REPORT.                                         P442288 
07745                                                                   P442288 
07746      IF TESTING-SHOW-DISPLAYS                                     P442288 
07747          DISPLAY '@ Z-5--UPDRPT:'                                 P442288 
07748                  ' ERR SW = ' ERROR-PROCESSING-SW                 P442288 
07749                  ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG          P442288 
07750                  ' UPDT ERR FL = ' RTS-UPDT-ERROR-FLAG            P442288 
07751                  ' ERROR NUMBER = ' ERR-ERROR-NUMBER.             P442288 
07752                                                                   P442288 
07753      MOVE SPACES                   TO ERR-NON-UPD.                P442288 
07754      MOVE SPACES                   TO ERR-MESS-DATA.              P442288 
07755      IF  UPD-LINE-CTR > 50                                        P442288 
07756          MOVE ZERO                 TO UPD-LINE-CTR                P442288 
07757          ADD 1                     TO UPD-PAGE-CTR                P442288 
07758          MOVE WS-CURRENT-DATE-DT   TO UPDAT-CURRENT-DT            P442288 
07759          MOVE UPD-PAGE-CTR         TO UPDAT-PAGE-NUMBER           P442288 
07760          MOVE 1                    TO PROPER-SPACING              P442288 
07761          MOVE UPDAT-HDR-1          TO PROCESS-UPDATE-REP-REC      P442288 
07762          PERFORM Z-51-WRITE-UPDATE-LINE                           P442288 
07763                                                                   P442288 
07764          MOVE SPACES               TO PROPER-SPACING              P442288 
07765          MOVE ERROR-HDR-1-RUNNING  TO PROCESS-UPDATE-REP-REC      P442288 
07766          PERFORM Z-51-WRITE-UPDATE-LINE                           P442288 
07767          MOVE SPACES               TO PROCESS-UPDATE-REP-REC      P442288 
07768          PERFORM Z-51-WRITE-UPDATE-LINE                           P442288 
07769                                                                   P442288 
07770          MOVE SPACES               TO PROPER-SPACING              P442288 
07771          MOVE UPDAT-HDR-2          TO PROCESS-UPDATE-REP-REC      P442288 
07772          PERFORM Z-51-WRITE-UPDATE-LINE                           P442288 
07773          MOVE ZERO                 TO PROPER-SPACING              P442288 
07774          MOVE ERROR-HDR-3          TO PROCESS-UPDATE-REP-REC      P442288 
07775          PERFORM Z-51-WRITE-UPDATE-LINE                           P442288 
07776          MOVE SPACES               TO PROPER-SPACING              P442288 
07777          MOVE ERROR-HDR-4          TO PROCESS-UPDATE-REP-REC      P442288 
07778          PERFORM Z-51-WRITE-UPDATE-LINE                           P442288 
07779          MOVE ERROR-HDR-5          TO PROCESS-UPDATE-REP-REC      P442288 
07780          PERFORM Z-51-WRITE-UPDATE-LINE                           P442288 
07781          MOVE SPACES               TO PROCESS-UPDATE-REP-REC      P442288 
07782          PERFORM Z-51-WRITE-UPDATE-LINE.                          P442288 
07783                                                                   P442288 
07784      MOVE SPACES                   TO ERR-KEY.                    P442288 
07785      IF BATCH-CNTL-ERROR                                          P442288 
07786          MOVE BATCH-KEY            TO ERR-MESS-DATA               P442288 
07787          MOVE SPACES               TO ERR-MESS-REGION             P442288 
07788          MOVE BATCH-REGIS-DT          TO ERR-ACCOUNT-DT           P442288 
07789          MOVE BATCH-ACCOUNT-NUMBER TO ERR-ACCOUNT-NUMBER          P442288 
07790          MOVE BATCH-FLEET-NBR      TO ERR-FLEET-NUMBER            P442288 
07791          MOVE BATCH-SUPPLEMENT-NUMBER TO ERR-SUPPLEMENT-NUMBER    P442288 
07792      ELSE                                                         P442288 
07793      IF  SHOW-RGN-IF-KEY-ON-ERROR      OR                         P442288 
07794          RTS-INVALID-UPDATE                                       P442288 
07795          MOVE WS-TRANS-KEY            TO ERR-MESS-DATA            P442288 
07796          MOVE WS-RGN-OFFICE           TO ERR-MESS-REGION          P442288 
07797          MOVE BATCH-REGIS-DT          TO ERR-ACCOUNT-DT           P442288 
07798          MOVE BATCH-ACCOUNT-NUMBER    TO ERR-ACCOUNT-NUMBER       P442288 
07799          MOVE BATCH-FLEET-NBR         TO ERR-FLEET-NUMBER         P442288 
07800          MOVE BATCH-SUPPLEMENT-NUMBER TO ERR-SUPPLEMENT-NUMBER    P442288 
07801          IF ERR-ERROR-NUMBER = 629                                P442288 
07802              IF EMSG29-FIRST-10-O NOT = EMSG29-FIRST-10-N         P442288 
07803                  MOVE '??'            TO ERR-NON-UPD              P442288 
07804              ELSE                                                 P442288 
07805                  MOVE '  '            TO ERR-NON-UPD              P442288 
07806          ELSE                                                     P442288 
07807          IF  RTS-REDUNDANT-UPD                                    P442288 
07808              MOVE '  '                TO ERR-NON-UPD              P442288 
07809          ELSE                                                     P442288 
07810          IF  RTS-INVALID-UPDATE                                   P442288 
07811              MOVE '**'                TO ERR-NON-UPD              P442288 
07812          ELSE                                                     P442288 
07813              MOVE SPACES              TO ERR-NON-UPD.             P442288 
07814                                                                   P442288 
07815                                                                   P442288 
07816      MOVE SPACES                   TO PROPER-SPACING.             P442288 
07817      MOVE ERROR-DET-1              TO PROCESS-UPDATE-REP-REC      P442288 
07818                                       WS-TRANS-DIAG.              P442288 
07819      PERFORM Z-51-WRITE-UPDATE-LINE.                              P442288 
07820      ADD 1                          TO UPD-LINE-CTR.              P442288 
07821                                                                   P442288 
07822  Z-51-WRITE-UPDATE-LINE.                                          P442288 
07823                                                                   P442288 
07824      EVALUATE  PROPER-SPACING                                     P442288 
07825          WHEN '1'                                                 P442288 
07826            WRITE PROCESS-UPDATE-REP-REC                           P442288 
07827               AFTER ADVANCING PAGE                                P442288 
07828          WHEN '0'                                                 P442288 
07829            WRITE PROCESS-UPDATE-REP-REC                           P442288 
07830               AFTER ADVANCING 2 LINES                             P442288 
07831          WHEN OTHER                                               P442288 
07832            WRITE PROCESS-UPDATE-REP-REC                           P442288 
07833               AFTER ADVANCING 1 LINE                              P442288 
07834      END-EVALUATE.                                                P442288 
07835                                                                   P442288 
07836  Z-6-VSAM-FILE-STATUS-CK.                                         P442288 
07837                                                                   P442288 
                                                                                
      D        DISPLAY 'BTCH-KEY <' BTCH-KEY '>'                                
      D        DISPLAY 'BATCH-FILE-STATUS <' BATCH-FILE-STATUS '>'              
                                                                                
07838          IF (BATCH-FILE-STATUS = 00 OR 97)                        P442288 
07839             OR (BATCH-FILE-STATUS = 10)                           P442288 
07840              NEXT SENTENCE                                        P442288 
07841          ELSE                                                     P442288 
07842          IF BATCH-FILE-STATUS = 93                                P442288 
07843              MOVE 2 TO ERROR-PROCESSING-SW                        P442288 
07844              MOVE 'BATCH'            TO ERROR-MSG-093-FILE        P442288 
07845              MOVE ERROR-MSG-093-TEXT TO ERR-ERROR-MESSAGE-1       P442288 
07846              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07847              MOVE ERROR-MSG-093B-TEXT TO ERR-ERROR-MESSAGE-1      P442288 
07848              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07849              MOVE 16                 TO WS-RETURN-CODE            P442288 
07850              PERFORM Z-Z-TERMINATION                              P442288 
07851          ELSE                                                     P442288 
07852              MOVE BATCH-FILE-STATUS TO EMT-BTCH-ERR-CODE          P442288 
07853              MOVE 2 TO ERROR-PROCESSING-SW                        P442288 
07854              MOVE ERROR-MSG-004-TEXT TO ERR-ERROR-MESSAGE-1       P442288 
07855              MOVE BATCH-FILE-STATUS  TO EMT-BTCH-ERR-CODE         P442288 
07856              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07857              PERFORM Z-Z-TERMINATION.                             P442288 
07858                                                                   P442288 
07859          IF (RESTART-STATUS = 00 OR 97)                           P442288 
07860             OR (RESTART-STATUS = 10)                              P442288 
07861              NEXT SENTENCE                                        P442288 
07862          ELSE                                                     P442288 
07863          IF RESTART-STATUS = 93                                   P442288 
07864              MOVE 2 TO ERROR-PROCESSING-SW                        P442288 
07865              MOVE 'RESTART'          TO ERROR-MSG-093-FILE        P442288 
07866              MOVE ERROR-MSG-093-TEXT TO ERR-ERROR-MESSAGE-1       P442288 
07867              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07868              MOVE ERROR-MSG-093B-TEXT TO ERR-ERROR-MESSAGE-1      P442288 
07869              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07870              MOVE 16                 TO WS-RETURN-CODE            P442288 
07871              PERFORM Z-Z-TERMINATION                              P442288 
07872          ELSE                                                     P442288 
07873              MOVE 'RESTART'          TO ERROR-MSG-033-FILE        P442288 
07874              MOVE 'READ'             TO ERROR-MSG-033-FUNC        P442288 
07875              MOVE RESTART-STATUS     TO ERROR-MSG-033-STATUS      P442288 
07876              MOVE ERROR-MSG-033-TEXT TO ERR-ERROR-MESSAGE-1       P442288 
07877              MOVE 2 TO ERROR-PROCESSING-SW                        P442288 
07878              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07879              MOVE 16                 TO WS-RETURN-CODE            P442288 
07880              PERFORM Z-Z-TERMINATION.                             P442288 
07881                                                                   P442288 
07882  Z-7-WRKRTS-FILE-STATUS-CK.                                       P442288 
07883                                                                   P442288 
07884          IF (WRKRTS-FILE-STATUS = 00 OR 97)                       P442288 
07885             OR (WRKRTS-FILE-STATUS = 10)                          P442288 
07886              NEXT SENTENCE                                        P442288 
07887          ELSE                                                     P442288 
07888          IF WRKRTS-FILE-STATUS = 93                               P442288 
07889              MOVE 2                  TO ERROR-PROCESSING-SW       P442288 
07890              MOVE 'INTERFACE'        TO ERROR-MSG-093-FILE        P442288 
07891              MOVE ERROR-MSG-093-TEXT TO ERR-ERROR-MESSAGE-1       P442288 
07892              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07893              MOVE ERROR-MSG-093B-TEXT TO ERR-ERROR-MESSAGE-1      P442288 
07894              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07895              MOVE 16                 TO WS-RETURN-CODE            P442288 
07896              PERFORM Z-Z-TERMINATION                              P442288 
07897          ELSE                                                     P442288 
07898              MOVE WRKRTS-FILE-STATUS TO EMT-ERR-IRPRTS            P442288 
07899              MOVE 2                  TO ERROR-PROCESSING-SW       P442288 
07900              MOVE ERROR-MSG-005-TEXT TO ERR-ERROR-MESSAGE-1       P442288 
07901              PERFORM Z-4-CONTROL-EDIT-ERROR                       P442288 
07902              PERFORM Z-Z-TERMINATION.                             P442288 
07903                                                                   P442288 
07904    EJECT                                                          P442288 
07905                                                                   P442288 
07906  Z-9-GEN-RTS-TRANS.                                               P442288 
07907 *---------------------------------------------------------------*    CL*16
07908 *       THESE ARE BOGUS VEHICLE TRANSACTIONS REFLECTING         *    CL*16
07909 *       NETTED AUDIT OR BILL CORRECTION SUPPLEMENTS             *    CL*16
07910 *---------------------------------------------------------------*    CL*16
07911         IF WS-TRANSCD = 'XIRRNW' AND                                 CL*16
07912            WS-VIN = 'KORCTSUMMARYNOVIN'                              CL*16
                                                                                
      D       DISPLAY '!TC222 - IF WS-TRANSCD = XIRRNW <'                       
      D                            WS-TRANSCD '>'                               
      D       DISPLAY 'AND WS-VIN = KORCTSUMMARYNOVIN <'                   CL*16
      D                    WS-VIN '>'                                      CL*16
                                                                                
07913          MOVE ALL '7'                  TO WS-RTS-DOCNO               CL*16
07914          MOVE 'NETTED BILL CORRECTION' TO WS-CUST-NAME               CL*16
07915          MOVE 'KORECT'                 TO WS-REGIS-RTS-REGPLTNO      CL*16
07916          MOVE 99                       TO WS-REGCLASSCD              CL*16
07917          MOVE ZEROS                    TO WS-OWNRID                  CL*16
07918          MOVE 'NOVINCORRECTION'        TO WS-VIN                     CL*16
07919          MOVE 'KORRECT'                TO WS-VEHMK                   CL*16
07920          MOVE 1956                     TO WS-VEHMODLYR.              CL*16
07921         IF WS-TRANSCD = 'XIRAUD'                                     CL*12
                                                                                
      D       DISPLAY '!TC223 - IF WS-TRANSCD = XIRAUD <'                       
      D                            WS-TRANSCD '>'                               
                                                                                
07922          MOVE ALL '8'                  TO WS-RTS-DOCNO               CL*12
07923          MOVE 'NETTED AUDIT SUPP'      TO WS-CUST-NAME               CL*16
07924          MOVE 'AUDITX'                 TO WS-REGIS-RTS-REGPLTNO      CL*12
07925          MOVE 99                       TO WS-REGCLASSCD              CL*12
07926          MOVE ZEROS                    TO WS-OWNRID                  CL*12
07927          MOVE 'NOVINAUDIT'             TO WS-VIN                     CL*12
07928          MOVE 'AUDITMK'                TO WS-VEHMK                   CL*12
07929          MOVE 1946                     TO WS-VEHMODLYR.              CL*12
07930                                                                      CL*12
07931         IF TESTING-SHOW-DISPLAYS                                  P442288 
07932             DISPLAY '@ Z-9-GEN-RTS-TRANS: FOR TRANS ' WS-TRANSCD  P442288 
07933             DISPLAY ' Z9-TRANS KEYS: '                            P442288 
07934                     ' OFCISSNO='  WS-OFCISSUANCENO                P442288 
07935                     ' TRANSWSID=' WS-TRANSWSID                    P442288 
07936                     ' AMDATE='    WS-TRANSAMDATE                  P442288 
07937                     ' TIME='      WS-TRANSTIME                    P442288 
07938                     ' OFCISSCD='  WS-OFCISSUANCECD                P442288 
07939             DISPLAY ' Z9-VEHL  DATA: ' WS-VIN ' ' WS-RTS-DOCNO    P442288 
07940                     ' ' WS-VEHMODLYR                              P442288 
07941                     ' ' WS-VEHMK  ' CLASS ' WS-REGCLASSCD         P442288 
07942             DISPLAY ' Z9-REG   DATA: ' WS-RTS-DOCNO               P442288 
07943                     ' ' WS-REGIS-RTS-REGPLTNO                        CL**9
07944                     ' ' WS-REGEFFDATE ' - ' WRKRTS-REG-EXP-DT     P442288 
07945             DISPLAY ' Z9-OWNR  DATA: ' WS-OWNRID ' ' WS-CUST-NAME P442288 
07946         END-IF.                                                   P442288 
07947                                                                   P442288 
                                                                                
      D       DISPLAY '!TC224 - INSERT INTO RTS_TRANS'                          
      D       IF WS-REGCLASSCD <= ZEROES                                        
      D           MOVE ZEROES TO WS-REGCLASSCD                                  
      D       END-IF                                                            
                                                                                
07948 *---------------------------------------------------------------* P442288 
07949 *   REMOVED  EXPDATE  = :WS-EXPDATE-N  VIA RTS DIRECTION (SM)   * P442288 
07950 *---------------------------------------------------------------* P442288 
07951   EXEC ADABAS                                                     P442288 
07952       INSERT                                                      P442288 
07953       INTO  RTS_TRANS     RTS-TRANS                               P442288 
07954       SET   TRANSCD       = :WS-TRANSCD                           P442288 
07955             OFCISSUANCENO = :WS-OFCISSUANCENO                     P442288 
07956             OFCISSUANCECD = :WS-OFCISSUANCECD                     P442288 
07957             TRANSWSID     = :WS-TRANSWSID                         P442288 
07958             TRANSAMDATE   = :WS-TRANSAMDATE                       P442288 
07959             TRANSTIME     = :WS-TRANSTIME                         P442288 
07960             EFFDATE       = :WS-REGEFFDATE                        P442288 
07961             DOCNO         = :WS-RTS-DOCNO                         P442288 
07962             CUSTNAME(1)   = :WS-CUST-NAME                         P442288 
07963             REGPLTNO      = :WS-REGIS-RTS-REGPLTNO                   CL**9
07964             REGCLASSCD    = :WS-REGCLASSCD                        P442288 
07965             OWNRID        = :WS-OWNRID                            P442288 
07966             VIN           = :WS-VIN                               P442288 
07967             VEHMK         = :WS-VEHMK                             P442288 
07968             VEHMODLYR     = :WS-VEHMODLYR                         P442288 
07969             VERSIONCD     = :C-TRANS-VERSIONCD                    P442288 
07970             BSNDATE       = :WS-TRANSAMDATE                       P442288 
07971             BSNDATETOTALAMT = :C-TRANS-EMPTY-ZERO                 P442288 
07972             VOIDTRANSCD   = :C-TRANS-EMPTY-SPACES                 P442288 
07973          OPTIONS PASSWORD = :WS-SCRT                              P442288 
07974             PREFIX        =  RTS-TRANS-                           P442288 
07975       END-EXEC                                                    P442288 
07976       .                                                           P442288 
07977                                                                   P442288 
07978      IF TESTING-SHOW-DISPLAYS                                     P442288 
07979           DISPLAY '@ Z-9-GEN-RTS-TRANS: '                         P442288 
07980                  'RESP=' SQLRSP ', QTY=' SQLQTY ', ISN=' SQLISN.  P442288 
07981                                                                   P442288 
07982      IF RESPONSE-CODE OF RTS-TRANS > ZERO                         P442288 
      D    DISPLAY 'ERROR ENTRY INTO RTS-TRANS ~~~~~~~~~~~~~~~~~~~~'            
      D    DISPLAY 'WS-TRANSCD <' WS-TRANSCD '>'                        P442288 
      D    DISPLAY 'WS-OFCISSUANCENO <' WS-OFCISSUANCENO '>'            P442288 
      D    DISPLAY 'WS-OFCISSUANCECD <' WS-OFCISSUANCECD '>'            P442288 
      D    DISPLAY 'WS-TRANSWSID <' WS-TRANSWSID '>'                    P442288 
      D    DISPLAY 'WS-TRANSAMDATE <' WS-TRANSAMDATE '>'                P442288 
      D    DISPLAY 'WS-TRANSTIME <' WS-TRANSTIME '>'                    P442288 
      D    DISPLAY 'WS-REGEFFDATE <' WS-REGEFFDATE '>'                  P442288 
      D    DISPLAY 'WS-RTS-DOCNO <' WS-RTS-DOCNO '>'                    P442288 
      D    DISPLAY 'WS-CUST-NAME <' WS-CUST-NAME '>'                    P442288 
      D    DISPLAY 'WS-REGIS-RTS-REGPLTNO <' WS-REGIS-RTS-REGPLTNO '>'     CL**9
      D    DISPLAY 'WS-REGCLASSCD <' WS-REGCLASSCD '>'                  P442288 
      D    DISPLAY 'WS-OWNRID <' WS-OWNRID '>'                          P442288 
      D    DISPLAY 'WS-VIN <' WS-VIN '>'                                P442288 
      D    DISPLAY 'WS-VEHMK <' WS-VEHMK '>'                            P442288 
      D    DISPLAY 'WS-VEHMODLYR <' WS-VEHMODLYR '>'                    P442288 
      D    DISPLAY 'C-TRANS-VERSIONCD <' C-TRANS-VERSIONCD  '>'         P442288 
      D    DISPLAY 'WS-TRANSAMDATE <' WS-TRANSAMDATE  '>'               P442288 
      D    DISPLAY 'C-TRANS-EMPTY-ZERO <' C-TRANS-EMPTY-ZERO '>'        P442288 
      D    DISPLAY 'C-TRANS-EMPTY-SPACES <' C-TRANS-EMPTY-SPACES '>'    P442288 
      D    DISPLAY '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'            
07983          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
07984          MOVE 'RTS_TRANS     '     TO ERROR-MSG-014-FILE          P442288 
07985          MOVE RESPONSE-CODE OF RTS-TRANS TO ERROR-MSG-014-ERRNUMB P442288 
07986          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
07987          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
07988          MOVE 20                   TO WS-RETURN-CODE              P442288 
07989          PERFORM Z-Z-TERMINATION.                                 P442288 
07990                                                                   P442288 
07991      EJECT                                                        P442288 
07992                                                                   P442288 
07993 ******************************************************************P442288 
07994  Z-10-ARCHIVE-RTS-RECORDS.                                        P442288 
07995                                                                   P442288 
07996 ******************************************************************P442288 
07997 * COPY PROCESSED RTS INTERFACE RECORD TO ARCHIVE & DELETE IT     *P442288 
07998 ******************************************************************P442288 
07999                                                                   P442288 
                                                                                
      D    DISPLAY '!TC195 - Z-10-ARCHIVE-RTS-RECORDS'                          
                                                                                
08000      IF TESTING-SHOW-DISPLAYS                                     P442288 
08001           DISPLAY '@ Z-10-ARCHIVE DELETE: '                       P442288 
08002              ' RTS-KEY====>' WORK-FD-KEY.                         P442288 
08003      MOVE WS-CRNT-DATE-DT          TO WRKRTS-LAST-CHG-DT.         P442288 
08004      MOVE WS-TRANSAMDATE           TO WRKRTS-RTS-TRAN-DATE.       P442288 
08005      MOVE WS-TRANSTIME             TO WRKRTS-RTS-TRAN-TIME.       P442288 
08006      MOVE WS-OFCISSUANCENO         TO WRKRTS-RTS-OFCISSUANCENO.   P442288 
08007      MOVE WS-TRANSWSID             TO WRKRTS-RTS-TRANSWSID.       P442288 
08008      WRITE  WRKRTS-ARCHIVE-RECORD  FROM WRKRTS-INTERFACE-RECORD.  P442288 
08009      DELETE WORK-FD-INTERFACE-FILE.                               P442288 
08010      PERFORM    Z-7-WRKRTS-FILE-STATUS-CK.                        P442288 
08011                                                                   P442288 
08012  Z-11-COMMIT-ADABAS.                                              P442288 
08013                                                                   P442288 
08014   EXEC ADABAS                                                     P442288 
08015       COMMIT WORK                                                 P442288 
08016       END-EXEC                                                    P442288 
08017       .                                                           P442288 
08018                                                                   P442288 
08019      IF SQLRSP > ZERO                                             P442288 
08020          MOVE 3                    TO ERROR-PROCESSING-SW         P442288 
08021          MOVE ' COMMIT WORK  '     TO ERROR-MSG-014-FILE          P442288 
08022          MOVE SQLRSP                    TO ERROR-MSG-014-ERRNUMB  P442288 
08023          MOVE ERROR-MSG-014-TEXT        TO ERR-ERROR-MESSAGE-1    P442288 
08024          PERFORM Z-4-CONTROL-EDIT-ERROR                           P442288 
08025          PERFORM Z-Z-TERMINATION.                                 P442288 
08026                                                                   P442288 
08027      MOVE WS-TRANSTIME                TO RESTART-UNIQUE-SEQ.      P442288 
08028                                                                   P442288 
08029      IF TESTING-SHOW-DISPLAYS                                     P442288 
08030          DISPLAY 'NOW ATTEMPT TO UPDATE RESTART ' RESTART-IMAGE   P442288 
08031          IF RESTART-DIAG-READ-ERROR EQUAL 'Y' OR                  P442288 
08032             RESTART-STATUS NOT = ZEROS                            P442288 
08033              DISPLAY 'ATTEMPTING "WRITE" ' RESTART-IMAGE          P442288 
08034          ELSE                                                     P442288 
08035              DISPLAY 'ATTEMPTING "RE-WRITE" ' INITIAL-IMAGE.      P442288 
08036                                                                   P442288 
08037      IF RESTART-DIAG-READ-ERROR EQUAL 'Y'                         P442288 
08038         OR RESTART-STATUS NOT = ZEROS                             P442288 
08039          MOVE 1                       TO RESTART-REC-NUM          P442288 
08040          MOVE 'N'                     TO RESTART-DIAG-READ-ERROR  P442288 
08041          WRITE INTERFACE-RESTART-VSAM-REC FROM RESTART-IMAGE      P442288 
08042              INVALID KEY                                          P442288 
08043                 MOVE 'Y'              TO RESTART-DIAG-READ-ERROR  P442288 
08044      ELSE                                                         P442288 
08045          MOVE 'N'                     TO RESTART-DIAG-READ-ERROR  P442288 
08046          REWRITE INTERFACE-RESTART-VSAM-REC FROM RESTART-IMAGE    P442288 
08047              INVALID KEY                                          P442288 
08048                 MOVE 'Y'              TO RESTART-DIAG-READ-ERROR  P442288 
08049                 DISPLAY RESTART-REC-NUM                           P442288 
08050                     ' NOT UPDATED ON INTERFACE-RESTART FILE'.     P442288 
08051                                                                   P442288 
08052      IF TESTING-SHOW-DISPLAYS                                     P442288 
08053          DISPLAY RESTART-STATUS  '=RESTART STATUS AFTER WRITE'    P442288 
08054                  ', INTERNAL ERR FLAG:'  RESTART-DIAG-READ-ERROR. P442288 
08055                                                                   P442288 
08056      EJECT                                                        P442288 
08057                                                                   P442288 
08058                                                                   P442288 
08059  Z-V-WRITE-MVUPDT-REPORT.                                         P442288 
08060                                                                   P442288 
08061      IF TESTING-SHOW-DISPLAYS                                     P442288 
08062          DISPLAY '@ Z-V--UPDRPT:'                                 P442288 
08063                  ' ERR SW = ' ERROR-PROCESSING-SW                 P442288 
08064                  ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG          P442288 
08065                  ' UPDT ERR FL = ' RTS-UPDT-ERROR-FLAG            P442288 
08066                  ' ERROR NUMBER = ' ERR-ERROR-NUMBER.             P442288 
08067                                                                   P442288 
08068      MOVE SPACES                   TO ERR-NON-UPD.                P442288 
08069      MOVE SPACES                   TO ERR-MESS-DATA.              P442288 
08070      IF  MVU-LINE-CTR > 50                                        P442288 
08071          MOVE ZERO                 TO MVU-LINE-CTR                P442288 
08072          ADD 1                     TO MVU-PAGE-CTR                P442288 
08073          MOVE WS-CURRENT-DATE-DT   TO MVUPD-CURRENT-DT            P442288 
08074          MOVE MVU-PAGE-CTR         TO MVUPD-PAGE-NUMBER           P442288 
08075          MOVE 1                    TO PROPER-SPACING              P442288 
08076          MOVE MVUPD-HDR-1          TO PROCESS-MVUPDT-REP-REC      P442288 
08077          PERFORM Z-V1-WRITE-MVUPDT-LINE                           P442288 
08078                                                                   P442288 
08079          MOVE SPACES               TO PROPER-SPACING              P442288 
08080          MOVE ERROR-HDR-1-RUNNING  TO PROCESS-MVUPDT-REP-REC      P442288 
08081          PERFORM Z-V1-WRITE-MVUPDT-LINE                           P442288 
08082          MOVE SPACES               TO PROCESS-MVUPDT-REP-REC      P442288 
08083          PERFORM Z-V1-WRITE-MVUPDT-LINE                           P442288 
08084                                                                   P442288 
08085          MOVE SPACES               TO PROPER-SPACING              P442288 
08086          MOVE MVUPD-HDR-2          TO PROCESS-MVUPDT-REP-REC      P442288 
08087          PERFORM Z-V1-WRITE-MVUPDT-LINE                           P442288 
08088          MOVE ZERO                 TO PROPER-SPACING              P442288 
08089          MOVE ERROR-HDR-3          TO PROCESS-MVUPDT-REP-REC      P442288 
08090          PERFORM Z-V1-WRITE-MVUPDT-LINE                           P442288 
08091          MOVE SPACES               TO PROPER-SPACING              P442288 
08092          MOVE ERROR-HDR-4          TO PROCESS-MVUPDT-REP-REC      P442288 
08093          PERFORM Z-V1-WRITE-MVUPDT-LINE                           P442288 
08094          MOVE ERROR-HDR-5          TO PROCESS-MVUPDT-REP-REC      P442288 
08095          PERFORM Z-V1-WRITE-MVUPDT-LINE                           P442288 
08096          MOVE SPACES               TO PROCESS-MVUPDT-REP-REC      P442288 
08097          PERFORM Z-V1-WRITE-MVUPDT-LINE.                          P442288 
08098                                                                   P442288 
08099      MOVE SPACES                   TO ERR-KEY.                    P442288 
08100      IF BATCH-CNTL-ERROR                                          P442288 
08101          MOVE BATCH-KEY            TO ERR-MESS-DATA               P442288 
08102          MOVE SPACES               TO ERR-MESS-REGION             P442288 
08103          MOVE BATCH-REGIS-DT          TO ERR-ACCOUNT-DT           P442288 
08104          MOVE BATCH-ACCOUNT-NUMBER TO ERR-ACCOUNT-NUMBER          P442288 
08105          MOVE BATCH-FLEET-NBR      TO ERR-FLEET-NUMBER            P442288 
08106          MOVE BATCH-SUPPLEMENT-NUMBER TO ERR-SUPPLEMENT-NUMBER    P442288 
08107      ELSE                                                         P442288 
08108      IF  SHOW-RGN-IF-KEY-ON-ERROR      OR                         P442288 
08109          RTS-INVALID-UPDATE                                       P442288 
08110          MOVE WS-TRANS-KEY            TO ERR-MESS-DATA            P442288 
08111          MOVE WS-RGN-OFFICE           TO ERR-MESS-REGION          P442288 
08112          MOVE BATCH-REGIS-DT          TO ERR-ACCOUNT-DT           P442288 
08113          MOVE BATCH-ACCOUNT-NUMBER    TO ERR-ACCOUNT-NUMBER       P442288 
08114          MOVE BATCH-FLEET-NBR         TO ERR-FLEET-NUMBER         P442288 
08115          MOVE BATCH-SUPPLEMENT-NUMBER TO ERR-SUPPLEMENT-NUMBER    P442288 
08116          IF  RTS-INVALID-UPDATE                                   P442288 
08117              MOVE '**'                TO ERR-NON-UPD              P442288 
08118          ELSE                                                     P442288 
08119              MOVE SPACES              TO ERR-NON-UPD.             P442288 
08120                                                                   P442288 
08121                                                                   P442288 
08122      MOVE SPACES                   TO PROPER-SPACING.             P442288 
08123      MOVE ERROR-DET-1              TO PROCESS-MVUPDT-REP-REC      P442288 
08124                                       WS-TRANS-DIAG.              P442288 
08125      PERFORM Z-V1-WRITE-MVUPDT-LINE.                              P442288 
08126      ADD 1                          TO MVU-LINE-CTR.              P442288 
08127                                                                   P442288 
08128  Z-V1-WRITE-MVUPDT-LINE.                                          P442288 
08129                                                                   P442288 
08130      EVALUATE  PROPER-SPACING                                     P442288 
08131          WHEN '1'                                                 P442288 
08132            WRITE PROCESS-MVUPDT-REP-REC                           P442288 
08133               AFTER ADVANCING PAGE                                P442288 
08134          WHEN '0'                                                 P442288 
08135            WRITE PROCESS-MVUPDT-REP-REC                           P442288 
08136               AFTER ADVANCING 2 LINES                             P442288 
08137          WHEN OTHER                                               P442288 
08138            WRITE PROCESS-MVUPDT-REP-REC                           P442288 
08139               AFTER ADVANCING 1 LINE                              P442288 
08140      END-EVALUATE.                                                P442288 
08141                                                                   P442288 
08142      EJECT                                                        P442288 
08143                                                                   P442288 
08144                                                                   P442288 
08145  Z-W-WRITE-MSGOUT-REPORT.                                         P442288 
08146                                                                   P442288 
08147      IF TESTING-SHOW-DISPLAYS                                     P442288 
08148          DISPLAY '@ Z-W--UPDRPT:'                                 P442288 
08149                  ' ERR SW = ' ERROR-PROCESSING-SW                 P442288 
08150                  ' TRANS ERR FL = ' RTS-TRANS-ERROR-FLAG          P442288 
08151                  ' UPDT ERR FL = ' RTS-UPDT-ERROR-FLAG            P442288 
08152                  ' ERROR NUMBER = ' ERR-ERROR-NUMBER.             P442288 
08153                                                                   P442288 
08154      MOVE SPACES                   TO ERR-NON-UPD.                P442288 
08155      MOVE SPACES                   TO ERR-MESS-DATA.              P442288 
08156      IF  MSG-LINE-CTR > 50                                        P442288 
08157          MOVE ZERO                 TO MSG-LINE-CTR                P442288 
08158          ADD 1                     TO MSG-PAGE-CTR                P442288 
08159          MOVE WS-CURRENT-DATE-DT   TO MSGOUT-CURRENT-DT           P442288 
08160          MOVE MSG-PAGE-CTR         TO MSGOUT-PAGE-NUMBER          P442288 
08161          MOVE 1                    TO PROPER-SPACING              P442288 
08162          MOVE MSGOUT-HDR-1         TO PROCESS-MSGOUT-REP-REC      P442288 
08163          PERFORM Z-W1-WRITE-MSGOUT-LINE                           P442288 
08164                                                                   P442288 
08165          MOVE SPACES               TO PROPER-SPACING              P442288 
08166          MOVE ERROR-HDR-1-RUNNING  TO PROCESS-MSGOUT-REP-REC      P442288 
08167          PERFORM Z-W1-WRITE-MSGOUT-LINE                           P442288 
08168          MOVE SPACES               TO PROCESS-MSGOUT-REP-REC      P442288 
08169          PERFORM Z-W1-WRITE-MSGOUT-LINE                           P442288 
08170                                                                   P442288 
08171          MOVE SPACES               TO PROPER-SPACING              P442288 
08172          MOVE MSGOUT-HDR-2         TO PROCESS-MSGOUT-REP-REC      P442288 
08173          PERFORM Z-W1-WRITE-MSGOUT-LINE                           P442288 
08174          MOVE ZERO                 TO PROPER-SPACING              P442288 
08175          MOVE ERROR-HDR-3          TO PROCESS-MSGOUT-REP-REC      P442288 
08176          PERFORM Z-W1-WRITE-MSGOUT-LINE                           P442288 
08177          MOVE SPACES               TO PROPER-SPACING              P442288 
08178          MOVE ERROR-HDR-4          TO PROCESS-MSGOUT-REP-REC      P442288 
08179          PERFORM Z-W1-WRITE-MSGOUT-LINE                           P442288 
08180          MOVE ERROR-HDR-5          TO PROCESS-MSGOUT-REP-REC      P442288 
08181          PERFORM Z-W1-WRITE-MSGOUT-LINE                           P442288 
08182          MOVE SPACES               TO PROCESS-MSGOUT-REP-REC      P442288 
08183          PERFORM Z-W1-WRITE-MSGOUT-LINE.                          P442288 
08184                                                                   P442288 
08185      MOVE SPACES                   TO ERR-KEY.                    P442288 
08186      IF BATCH-CNTL-ERROR                                          P442288 
08187          MOVE BATCH-KEY            TO ERR-MESS-DATA               P442288 
08188          MOVE SPACES               TO ERR-MESS-REGION             P442288 
08189          MOVE BATCH-REGIS-DT       TO ERR-ACCOUNT-DT              P442288 
08190          MOVE BATCH-ACCOUNT-NUMBER TO ERR-ACCOUNT-NUMBER          P442288 
08191          MOVE BATCH-FLEET-NBR      TO ERR-FLEET-NUMBER            P442288 
08192          MOVE BATCH-SUPPLEMENT-NUMBER TO ERR-SUPPLEMENT-NUMBER    P442288 
08193      ELSE                                                         P442288 
08194      IF  SHOW-RGN-IF-KEY-ON-ERROR      OR                         P442288 
08195          RTS-INVALID-UPDATE                                       P442288 
08196          MOVE WS-TRANS-KEY            TO ERR-MESS-DATA            P442288 
08197          MOVE WS-RGN-OFFICE           TO ERR-MESS-REGION          P442288 
08198          MOVE BATCH-REGIS-DT          TO ERR-ACCOUNT-DT           P442288 
08199          MOVE BATCH-ACCOUNT-NUMBER    TO ERR-ACCOUNT-NUMBER       P442288 
08200          MOVE BATCH-FLEET-NBR         TO ERR-FLEET-NUMBER         P442288 
08201          MOVE BATCH-SUPPLEMENT-NUMBER TO ERR-SUPPLEMENT-NUMBER    P442288 
08202          IF  RTS-INVALID-UPDATE                                   P442288 
08203              MOVE '**'                TO ERR-NON-UPD              P442288 
08204          ELSE                                                     P442288 
08205              MOVE SPACES              TO ERR-NON-UPD.             P442288 
08206                                                                   P442288 
08207                                                                   P442288 
08208      MOVE SPACES                   TO PROPER-SPACING.             P442288 
08209      MOVE ERROR-DET-1              TO PROCESS-MSGOUT-REP-REC      P442288 
08210                                       WS-TRANS-DIAG.              P442288 
08211      PERFORM Z-W1-WRITE-MSGOUT-LINE.                              P442288 
08212      ADD 1                          TO MSG-LINE-CTR.              P442288 
08213                                                                   P442288 
08214  Z-W1-WRITE-MSGOUT-LINE.                                          P442288 
08215                                                                   P442288 
08216      EVALUATE  PROPER-SPACING                                     P442288 
08217          WHEN '1'                                                 P442288 
08218            WRITE PROCESS-MSGOUT-REP-REC                           P442288 
08219               AFTER ADVANCING PAGE                                P442288 
08220          WHEN '0'                                                 P442288 
08221            WRITE PROCESS-MSGOUT-REP-REC                           P442288 
08222               AFTER ADVANCING 2 LINES                             P442288 
08223          WHEN OTHER                                               P442288 
08224            WRITE PROCESS-MSGOUT-REP-REC                           P442288 
08225               AFTER ADVANCING 1 LINE                              P442288 
08226      END-EVALUATE.                                                P442288 
08227                                                                   P442288 
08228      EJECT                                                        P442288 
08229                                                                   P442288 
08230  Z-Z-TERMINATION.                                                 P442288 
08231                                                                   P442288 
08232      IF TESTING-SHOW-DISPLAYS                                     P442288 
08233          DISPLAY ' Z-Z-TERMINATION WILL NOW CLOSE ALL FILES'.     P442288 
                                                                                
08234      CLOSE       PROCESS-CONTROL-REPORT.                          P44228  
08235      CLOSE       PROCESS-UPDATE-REPORT.                           P44228  
08236      CLOSE       PROCESS-MSGOUT-REPORT.                           P44228  
08237      CLOSE       TRANSMITTAL-RPT-FILE.                            P44228  
08238      CLOSE       UPDATES-ARCHIVE-FILE.                            P44228  
08239      CLOSE       IRP-BATCH-FILE.                                  P44228  
08240      CLOSE       WORK-FD-INTERFACE-FILE.                          P44228  
08241      CLOSE       INTERFACE-RESTART.                               P44228  
08242                                                                   P442288 
08243       EXEC ADABAS                                                 P442288 
08244           DBCLOSE                                                 P442288 
08245       END-EXEC                                                    P442288 
08246 ***** IF WS-RETURN-CODE > ZERO                                       CL*22
08247                                                                   P442288 
08248      IF WS-RETURN-CODE > ZERO                                     P442288 
08249          IF WS-RETURN-CODE > 16                                   P442288 
08250              CALL 'ILBOABN0'                                      P442288 
08251          ELSE                                                     P442288 
08252              MOVE WS-RETURN-CODE TO RETURN-CODE                   P442288 
08253          END-IF                                                      CL*22
08254      END-IF.                                                      P442288 
08255      GOBACK.                                                         CL*22
08255 *    STOP RUN.                                                       CL*22
08256 ***** GOBACK.                                                        CL*22
08257 ***** EJECT                                                          CL*22
08258                                                                   P442288 
08259 *---------------------------------------------------------------* P442288 
08260 *   THAT'S ALL FOLKS                                            * P442288 
08261 *---------------------------------------------------------------* P442288 
08262                                                                   P442288 
