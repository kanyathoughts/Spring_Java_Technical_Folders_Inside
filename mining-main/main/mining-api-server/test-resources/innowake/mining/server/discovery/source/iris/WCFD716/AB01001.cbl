       IDENTIFICATION DIVISION.
       PROGRAM-ID. AB01001.
      *@COMPILE PARAMETERS
      *@PROGTYPE=BATCH
      *@DB=YES
      *@URT=UAB01001
      *@SPP=YES
      *@MQ=NO
      *@PHASE=AB01001
      *@LNKMODE=RMODE=24,AMODE=31
      *@COBSUBR=NO
      *@SQL=NO
      *@COMPILE PARAMETERS END
      *  THIS PROGRAM IS LE
      *  THIS PROGRAM USES URT - UAB01001
      *AUTHOR.       DOUG RANK
      *DATE WRITTEN. OCTOBER 2009.
      *****************************************************************
      * MAINTENANCE LOG:
      * ----------------
      *****************************************************************
      * 04/01/11 PWW - TICKET RWIS-8FLJU5 - REMOVE 80-0237825 FROM IOR*
      *                LIST.  (110401)                                *
      * 08/02/10 DCR - TICKET RWIS-87XKXK - ADD 04-3349012 TO IOR LIST*
      *                (100802)                                       *
      * 06/30/10 DCR - TICKET DNIF-86VRQT - CHANGE IOR LIST (100630)  *
      * 06/10/10 DCR - TICKET DRAK-869R2U - ADD VALID IOR (100610)    *
      *****************************************************************
      *  AB01001 - CREATE FCX TRANS '017' AND '018' FOR SIEMENS       *
      *****************************************************************
      * THIS PROGRAM WILL READ SDH RECORDS WITH ACTUAL SEND DATE EQUAL
      * TO CURRENT DATE.  IF THE IOR OR CONSIGNEE MATCHES SIEMENS
      * LIST A '017' AND '018' RECORD WILL BE WRITTEN TO FCX.  AB01002
      * WILL PROCESS '017' RECORDS AND AB01003 WILL PROCESS '018'
      * RECORDS.
      ****************************************************************
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       SPECIAL-NAMES.
170113     UPSI-0 ON IS RERUN-BY-ESM
           UPSI-6 ON IS RERUN-BY-TRANS-DATE
           UPSI-7 ON IS TEST-RUN.

       DATACOM SECTION.
           ID-AREA IS ID-AREA-IDENTIFIER
           PRINT NOGEN.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

170113     SELECT IORFILE ASSIGN SYS011-UR-2501-S-IORFILE.

       DATA DIVISION.
       FILE SECTION.

170113 FD  IORFILE
170113     LABEL RECORDS ARE OMITTED
170113     BLOCK CONTAINS 0 RECORDS.
170113 01  IORFILE-REC.
170113     05  IORFILE-IOR          PIC X(10).
170113     05  FILLER               PIC X(70).

       WORKING-STORAGE SECTION.
       01  ID-AREA-IDENTIFIER          PIC X(32)   VALUE 'AB90'.

       01  WS-DV-STARTS                PIC X(32)   VALUE
           '***** DATAVIEWS START HERE ***'.

          DATA-VIEW FCX-WHOLE-REC   PREFIX IS 'FCX-'.
          DATA-VIEW FC2-WHOLE-REC   PREFIX IS 'FC2-'
                    DD NAME IS FCX-WHOLE-REC.
          DATA-VIEW SDH-AB01001-DVW PREFIX IS 'SDH-'.
          DATA-VIEW TRI-AB01001-DVW PREFIX IS 'TRI-'.

       01  FILLER                      PIC X(60)   VALUE
           '*****  PROGRAM AB90 WORKING-STORAGE STARTS HERE  *****'.

       01  WS-INPUT-DT-CARD.
           05  WS-INPUT-START-DATE     PIC X(08) VALUE ZEROES.
           05  WS-INPUT-END-DATE       PIC X(08) VALUE ZEROES.
           05  FILLER                  PIC X(64) VALUE SPACES.

170103 01  TSUB                        PIC 9(03) VALUE ZEROS.
170113 01  END-OF-FILE-SW              PIC X(01) VALUE 'N'.
170113 01  WS-CARD-EOF-SW              PIC X(01) VALUE 'N'.
170113 01  WS-LOOP-DONE-SW             PIC X(01) VALUE 'N'.
170113 01  WS-CARD-RECORD.
170113     05  WS-CARD-ESM             PIC X(11) VALUE SPACES.
170113     05  FILLER                  PIC X(69) VALUE SPACES.

       01  WS-NON-INIT.
           05  WS-FCX-COUNT            PIC 9(09) VALUE ZEROS.
170113     05  WS-CHECK-IOR            PIC X(10) VALUE SPACES.
           05  WS-IOR-SW               PIC X(01) VALUE SPACES.
           05  WS-IOR-1.
               10  WS-IOR-BASE         PIC X(10).
                   88  VALID-IOR-BASE            VALUES
100802                                                  '04-3349012'
100630                                                  '13-2762488'
100630                                                  '13-3987280'
100630                                                  '22-2102429'
100630                                                  '22-2417778'
100630                                                  '25-1563806'
100630                                                  '39-1280256'
170113                                                  '81-3283763'
100630                                                  '95-2802182'.
110401*                                                 '80-0237825'
               10  WS-IOR-SUFF         PIC X(03) VALUE SPACES.
           05  WS-IOR-2.
               10  WS-IOR-BASE-2       PIC X(12).
                   88  VALID-IOR-BASE-2          VALUES
100630                                                  '024601-00443'.
               10  FILLER              PIC X(01) VALUE SPACES.

170113 01  WS-TABLE-OF-IOR-NOS.
170113     05  WS-TAB-HOLDER           OCCURS 20 TIMES.
170113         10  WS-TAB-IOR-NO       PIC  X(10).

      **************************************************************
      * THE FOLLOWING ARE FOR THE CEE DATE ROUTINES FOR CURRENT    *
      *     GREGARIAN, JULIAN AND TIME.                            *
      **************************************************************
       01  WS-CURRENT-INTEGER-DATE     PIC S9(09) COMP VALUE ZERO.

       01  WS-CURRENT-SECONDS          COMP-2.

       01  WS-FC-RETURN-AREA.
           10 WS-FC-SEV                PIC S9(04) COMP VALUE ZEROES.
           10 WS-FC-MSG                PIC S9(04) COMP VALUE ZEROES.
           10 WS-FC-CTW                PIC X(01) VALUE SPACES.
           10 WS-FC-FAC                PIC X(03) VALUE SPACES.
           10 WS-I-S-INF               PIC X(04) VALUE SPACES.

       01  WS-CURRENT-DATE-INFO.
           10  WS-CURRENT-DATE         PIC  9(08).
           10  WS-CURRENT-DATE-X       REDEFINES  WS-CURRENT-DATE.
               15  WS-CURRENT-DATE-CC  PIC  9(02).
               15  WS-CURRENT-DATE-6.
                   20  WS-CURRENT-DATE-YY
                                       PIC  9(02).
                   20  WS-CURRENT-DATE-MM
                                       PIC  9(02).
                   20  WS-CURRENT-DATE-DD
                                       PIC  9(02).
           10  WS-CURRENT-TIME         PIC  9(09).
           10  WS-CURRENT-TIME-X       REDEFINES WS-CURRENT-TIME.
               15  WS-CURRENT-TIME-6.
                   20  WS-CURRENT-TIME-HR
                                       PIC  9(02).
                   20  WS-CURRENT-TIME-MN
                                       PIC  9(02).
                   20  WS-CURRENT-TIME-SC
                                       PIC  9(02).
               15  WS-CURRENT-TIME-MS  PIC  9(03).

-INC CZA33CR                                                                   %

           EJECT

       LINKAGE SECTION.

       PROCEDURE DIVISION.

       0000-MAIN.
           MOVE 'AB90'              TO CALL-ZAX33-PROGRAM

           CALL 'CEELOCT2'       USING WS-CURRENT-INTEGER-DATE,
                                       WS-CURRENT-SECONDS,
                                       WS-CURRENT-DATE-INFO,
                                       WS-FC-RETURN-AREA

           DO 0100-OPEN-FILES

170113     MOVE SPACES              TO WS-TABLE-OF-IOR-NOS
170113     DO 0300-READ-IORFILE

           IF RERUN-BY-TRANS-DATE
               ACCEPT WS-INPUT-DT-CARD FROM SYSIPT
               DISPLAY 'DATE RUN FROM ' WS-INPUT-START-DATE
                       ' TO ' WS-INPUT-END-DATE
               DO 0600-READ-SDH-DATE
           ELSE
170113         IF RERUN-BY-ESM
170113             DISPLAY 'READ BY ESM'
170113             DO 0500-ENTRY-LOOP
170113         ELSE
                   DO 0700-READ-SDH
170113         ENDIF
           ENDIF

           DISPLAY 'FCX RECORDS WRITTEN ' WS-FCX-COUNT

           DO 0200-CLOSE-FILES

           MOVE +00000                 TO RETURN-CODE
           GOBACK
       EJECT
      /
      ******************************************************************
      * 0100-OPEN-FILES WILL OPEN ALL FILES AND THE DATABASE FOR USE.  *
      ******************************************************************
       0100-OPEN-FILES.

170113     OPEN INPUT IORFILE

           OPEN

           IF UTILITY-RA-STATUS-CODE NOT EQUAL SPACES
               MOVE 'ALL'              TO CALL-ZAX33-TABLE
               MOVE 'OPEN 0100-'       TO CALL-ZAX33-FUNCTION
               MOVE SPACES             TO CALL-ZAX33-KEY-VAL
               MOVE UTILITY-RA-STATUS-CODE
                                       TO CALL-ZAX33-P-RET-CODE
               MOVE UTILITY-RA-RESERV-BYTE
                                       TO CALL-ZAX33-S-RET-CODE
               DO 9999-ABEND-RTN
           ENDIF

170113 0150-SEARCH-TABLE.
170113     MOVE ZEROES                        TO TSUB
170113     MOVE SPACES                        TO WS-IOR-SW
170113                                           WS-LOOP-DONE-SW
170113     LOOP
170113     UNTIL WS-LOOP-DONE-SW               = 'Y'
170113         ADD 1                          TO TSUB
170113         IF TSUB                         > 20
170113             MOVE 'Y'                   TO WS-LOOP-DONE-SW
170113         ELSE
170113             IF WS-TAB-IOR-NO (TSUB)     = SPACES
170113                 MOVE 'Y'               TO WS-LOOP-DONE-SW
170113             ELSE
170113                 IF WS-TAB-IOR-NO (TSUB) = WS-CHECK-IOR
170113                     MOVE 'Y'           TO WS-IOR-SW
170113                                           WS-LOOP-DONE-SW
170113                 ENDIF
170113             ENDIF
170113         ENDIF
170113     ENDLOOP
      ******************************************************************
      * 0200-CLOSE-FILES WILL CLOSE ALL FILES AND THE DATABASE         *
      ******************************************************************
       0200-CLOSE-FILES.

170113     CLOSE IORFILE

           CLOSE

           IF UTILITY-RA-STATUS-CODE NOT EQUAL SPACES
               MOVE 'ALL'              TO CALL-ZAX33-TABLE
               MOVE 'CLOSE 0200'       TO CALL-ZAX33-FUNCTION
               MOVE SPACES             TO CALL-ZAX33-KEY-VAL
               MOVE UTILITY-RA-STATUS-CODE
                                       TO CALL-ZAX33-P-RET-CODE
               MOVE UTILITY-RA-RESERV-BYTE
                                       TO CALL-ZAX33-S-RET-CODE
               DO 9999-ABEND-RTN
           ENDIF

170113 0300-READ-IORFILE.
170113     LOOP
170113         READ IORFILE
170113         IF END
170113             MOVE 'Y'            TO END-OF-FILE-SW
170113         ENDIF
170113     UNTIL END-OF-FILE-SW         = 'Y'
170113         ADD 1                   TO TSUB
170113         IF TSUB                  > 20
170113             DISPLAY '****** AB01000 ERROR ******' UPON CONSOLE
170113             DISPLAY 'ONLY 20 IOR NUMBERS ALLOWED' UPON CONSOLE
170113             CLOSE IORFILE
170113             CLOSE
170113             MOVE  +17           TO RETURN-CODE
170113             GOBACK
170113         ELSE
170113             MOVE IORFILE-IOR    TO WS-TAB-IOR-NO (TSUB)
170113             DISPLAY WS-TAB-IOR-NO (TSUB) '-' TSUB
170113         ENDIF
170113     ENDLOOP

170113 0500-ENTRY-LOOP.
170113     LOOP
170113         ACCEPT WS-CARD-RECORD FROM SYSIPT
170113         IF WS-CARD-ESM           = '99999999999'
170113             MOVE 'Y'            TO WS-CARD-EOF-SW
170113         ENDIF
170113     UNTIL WS-CARD-EOF-SW         = 'Y'
170113         DISPLAY WS-CARD-ESM
170113         DO 0550-READ-SDH-ESM
170113     ENDLOOP

170113***************************************************************
170113* 0550-READ-SDH-ESM.                                          *
170113***************************************************************
170113 0550-READ-SDH-ESM.
170113     FOR EACH SDH-AB01001-DVW
170113     WHERE (SDH-ENTRY-SUMMARY-NO     = WS-CARD-ESM AND
170113            SDH-DELETE-DATE          = 0)
170113
170113         MOVE SPACES             TO WS-IOR-SW
170113         MOVE SDH-IMPORTER-NO    TO WS-IOR-1
170113                                    WS-IOR-2
170113                                    WS-CHECK-IOR
170113         DO 0150-SEARCH-TABLE

170113         IF VALID-IOR-BASE-2
170113             MOVE 'Y'            TO WS-IOR-SW
170113         ENDIF

170113         MOVE SDH-CONSIGNEE-NO   TO WS-IOR-1
170113                                    WS-IOR-2
170113                                    WS-CHECK-IOR
170113         IF WS-IOR-SW         NOT = 'Y'
170113             DO 0150-SEARCH-TABLE
170113         ENDIF

170113         IF VALID-IOR-BASE-2
170113             MOVE 'Y'            TO WS-IOR-SW
170113         ENDIF

170113         IF WS-IOR-SW = 'Y'
170113             DO 6000-GET-TRI
170113             IF TRI-IMPORTER-NO = SDH-IMPORTER-NO  AND
170113                TRI-CONS-NO     = SDH-CONSIGNEE-NO
170113             ELSE
170113                 DO 6100-CHECK-TRI
170113             ENDIF
170113         ENDIF

170113         IF WS-IOR-SW = 'Y'
170113             IF NOT TEST-RUN
170113                 DO 0800-CHECK-FCX
170113             ENDIF
170113             DO 0900-WRITE-FCX
170113         ENDIF

170113     WHEN NONE
170113         DISPLAY 'NO SDH RECORD FOR ESM-' WS-CARD-ESM

170113     WHEN ERROR
170113         MOVE SDH-AB01001-DVW-RA-STATUS-CODE
170113                                 TO CALL-ZAX33-P-RET-CODE
170113         MOVE SDH-AB01001-DVW-RA-UPDATE
170113                                 TO CALL-ZAX33-S-RET-CODE
170113         MOVE '0550-  '          TO CALL-ZAX33-FUNCTION
170113         MOVE 'SDH'              TO CALL-ZAX33-TABLE
170113         MOVE WS-CARD-ESM        TO CALL-ZAX33-KEY-VAL
170113         DO 9999-ABEND-RTN
170113     ENDFOR
       EJECT
      /
      ***************************************************************
      * 0600-READ-SDH-DATE.                                         *
      ***************************************************************
       0600-READ-SDH-DATE.

           FOR EACH SDH-AB01001-DVW
           WHERE (SDH-ACTUAL-SEND-DATE NOT < WS-INPUT-START-DATE  AND
                  SDH-ACTUAL-SEND-DATE NOT > WS-INPUT-END-DATE    AND
                  SDH-DELETE-DATE          = 0)

               MOVE SPACES             TO WS-IOR-SW
               MOVE SDH-IMPORTER-NO    TO WS-IOR-1
                                          WS-IOR-2
170113                                    WS-CHECK-IOR
170113         DO 0150-SEARCH-TABLE

170113         IF VALID-IOR-BASE-2
                   MOVE 'Y'            TO WS-IOR-SW
               ENDIF

               MOVE SDH-CONSIGNEE-NO   TO WS-IOR-1
                                          WS-IOR-2
170113                                    WS-CHECK-IOR
170113         IF WS-IOR-SW         NOT = 'Y'
170113             DO 0150-SEARCH-TABLE
170113         ENDIF

               IF VALID-IOR-BASE-2
                   MOVE 'Y'            TO WS-IOR-SW
               ENDIF

091113         IF WS-IOR-SW = 'Y'
091113             DO 6000-GET-TRI
091113             IF TRI-IMPORTER-NO = SDH-IMPORTER-NO  AND
091113                TRI-CONS-NO     = SDH-CONSIGNEE-NO
091113             ELSE
091113                 DO 6100-CHECK-TRI
091113             ENDIF
091113         ENDIF

               IF WS-IOR-SW = 'Y'
170113             IF NOT TEST-RUN
                       DO 0800-CHECK-FCX
170113             ENDIF
170113             DO 0900-WRITE-FCX
               ENDIF

           WHEN NONE

           WHEN ERROR
               MOVE SDH-AB01001-DVW-RA-STATUS-CODE
                                       TO CALL-ZAX33-P-RET-CODE
               MOVE SDH-AB01001-DVW-RA-UPDATE
                                       TO CALL-ZAX33-S-RET-CODE
               MOVE '0600-  '          TO CALL-ZAX33-FUNCTION
               MOVE 'SDH'              TO CALL-ZAX33-TABLE
               MOVE WS-INPUT-DT-CARD   TO CALL-ZAX33-KEY-VAL
               DO 9999-ABEND-RTN
           ENDFOR
       EJECT
      /
      ***************************************************************
      * 0700-READ-SDH                                               *
      ***************************************************************
       0700-READ-SDH.

           FOR EACH SDH-AB01001-DVW
           WHERE (SDH-ACTUAL-SEND-DATE = WS-CURRENT-DATE  AND
                  SDH-DELETE-DATE      = 0)

               MOVE SPACES             TO WS-IOR-SW
               MOVE SDH-IMPORTER-NO    TO WS-IOR-1
                                          WS-IOR-2
170113                                    WS-CHECK-IOR
170113         DO 0150-SEARCH-TABLE

               IF VALID-IOR-BASE-2
                   MOVE 'Y'            TO WS-IOR-SW
               ENDIF

               MOVE SDH-CONSIGNEE-NO   TO WS-IOR-1
                                          WS-IOR-2
170113                                    WS-CHECK-IOR
170113         IF WS-IOR-SW         NOT = 'Y'
170113             DO 0150-SEARCH-TABLE
170113         ENDIF

               IF VALID-IOR-BASE-2
                   MOVE 'Y'            TO WS-IOR-SW
               ENDIF


091113         IF WS-IOR-SW = 'Y'
091113             DO 6000-GET-TRI
091113             IF TRI-IMPORTER-NO = SDH-IMPORTER-NO  AND
091113                TRI-CONS-NO     = SDH-CONSIGNEE-NO
091113             ELSE
091113                 DO 6100-CHECK-TRI
091113             ENDIF
091113         ENDIF

               IF WS-IOR-SW = 'Y'
170113             IF NOT TEST-RUN
                       DO 0800-CHECK-FCX
170113             ENDIF
170113             DO 0900-WRITE-FCX
               ENDIF

           WHEN NONE

           WHEN ERROR
               MOVE SDH-AB01001-DVW-RA-STATUS-CODE
                                       TO CALL-ZAX33-P-RET-CODE
               MOVE SDH-AB01001-DVW-RA-UPDATE
                                       TO CALL-ZAX33-S-RET-CODE
               MOVE '0700-  '          TO CALL-ZAX33-FUNCTION
               MOVE 'SDH'              TO CALL-ZAX33-TABLE
               MOVE WS-CURRENT-DATE    TO CALL-ZAX33-KEY-VAL
               DO 9999-ABEND-RTN
           ENDFOR
       EJECT
      /
      ***************************************************************
      * 0800-CHECK-FCX                                              *
      ***************************************************************
       0800-CHECK-FCX.

           FOR EACH FCX-WHOLE-REC
           WHERE (FCX-KEY-ESM       = SDH-ENTRY-SUMMARY-NO AND
                 (FCX-TRANSMIT-TYPE = '017'                OR
                  FCX-TRANSMIT-TYPE = '018'))
           HOLD RECORDS

               DELETE FCX-WHOLE-REC

               IF FCX-WHOLE-REC-RA-STATUS-CODE NOT = SPACES
                   MOVE 'FCX'          TO CALL-ZAX33-TABLE
                   MOVE '0800 DELETE'  TO CALL-ZAX33-FUNCTION
                   MOVE FCX-KEY-ESM    TO CALL-ZAX33-KEY-VAL
                   MOVE FCX-WHOLE-REC-RA-STATUS-CODE
                                       TO CALL-ZAX33-P-RET-CODE
                   MOVE FCX-WHOLE-REC-RA-UPDATE
                                       TO CALL-ZAX33-S-RET-CODE
                   FREE SET FCX-WHOLE-REC
               ENDIF

           WHEN ERROR
               MOVE FCX-WHOLE-REC-RA-STATUS-CODE
                                       TO CALL-ZAX33-P-RET-CODE
               MOVE FCX-WHOLE-REC-RA-UPDATE
                                       TO CALL-ZAX33-S-RET-CODE
               MOVE '0800-  '          TO CALL-ZAX33-FUNCTION
               MOVE 'FCX'              TO CALL-ZAX33-TABLE
               MOVE SDH-ENTRY-SUMMARY-NO
                                       TO CALL-ZAX33-KEY-VAL
               DO 9999-ABEND-RTN
           ENDFOR
       EJECT
      /
      *****************************************************************
      * 0900-WRITE-FCX  -                                             *
      *****************************************************************
       0900-WRITE-FCX.

           INITIALIZE FCX-WHOLE-REC-WORKAREA

           MOVE '017'                  TO FCX-TRANSMIT-TYPE
           MOVE SDH-IMPORTER-NO        TO FCX-IMPORTER-OF-REC-NO
           MOVE SDH-ENTRY-SUMMARY-NO   TO FCX-KEY-ESM
           MOVE 000438431              TO FCX-PBW-ID
           MOVE 'AB01001 '             TO FCX-CREATED-PGM-ID
           MOVE ZEROES                 TO FCX-TRANSMIT-DATE
           MOVE WS-CURRENT-DATE        TO FCX-CREATE-DATE
           MOVE WS-CURRENT-TIME-6      TO FCX-CREATE-TIME
           MOVE 'BCH'                  TO FCX-OPID

           ADD 1                       TO WS-FCX-COUNT

170113     IF NOT TEST-RUN
               WRITE FCX-WHOLE-REC

               IF FCX-WHOLE-REC-RA-STATUS-CODE GREATER THAN SPACES
                   MOVE 'FCX'              TO CALL-ZAX33-TABLE
                   MOVE '0900 AD1'         TO CALL-ZAX33-FUNCTION
                   MOVE FCX-KEY-ESM        TO CALL-ZAX33-KEY-VAL
                   MOVE FCX-WHOLE-REC-RA-STATUS-CODE
                                           TO CALL-ZAX33-P-RET-CODE
                   MOVE FCX-WHOLE-REC-RA-UPDATE
                                           TO CALL-ZAX33-S-RET-CODE
                   FREE SET FCX-WHOLE-REC
                   MOVE 'AB90'             TO CALL-ZAX33-PROGRAM
                   DO 9999-ABEND-RTN
               ENDIF
170113     ELSE
170113         DISPLAY FCX-KEY-FCX '-'
170113                 FCX-IMPORTER-OF-REC-NO '-'
170113                 FCX-KEY-ESM '-'
170113                 FCX-PBW-ID
170113     ENDIF

           MOVE '018'                  TO FCX-TRANSMIT-TYPE

           ADD 1                       TO WS-FCX-COUNT

170113     IF NOT TEST-RUN
               WRITE FCX-WHOLE-REC

               IF FCX-WHOLE-REC-RA-STATUS-CODE GREATER THAN SPACES
                   MOVE 'FCX'              TO CALL-ZAX33-TABLE
                   MOVE '0900-AD2'         TO CALL-ZAX33-FUNCTION
                   MOVE FCX-KEY-ESM        TO CALL-ZAX33-KEY-VAL
                   MOVE FCX-WHOLE-REC-RA-STATUS-CODE
                                           TO CALL-ZAX33-P-RET-CODE
                   MOVE FCX-WHOLE-REC-RA-UPDATE
                                           TO CALL-ZAX33-S-RET-CODE
                   FREE SET FCX-WHOLE-REC
                   MOVE 'AB90'             TO CALL-ZAX33-PROGRAM
                   DO 9999-ABEND-RTN
               ENDIF
170113     ELSE
170113         DISPLAY FCX-KEY-FCX '-'
170113                 FCX-IMPORTER-OF-REC-NO '-'
170113                 FCX-KEY-ESM '-'
170113                 FCX-PBW-ID
170113         DISPLAY '--------------------------------------------'
170113     ENDIF

      ***************************************************************
      * 6000-GET-TRI.                                               *
      ***************************************************************
       6000-GET-TRI.

           FOR EACH TRI-AB01001-DVW
           WHERE (TRI-KEY-ESM = SDH-ENTRY-SUMMARY-NO)


           WHEN NONE

               INITIALIZE TRI-AB01001-DVW-WORKAREA

           WHEN ERROR
               MOVE TRI-AB01001-DVW-RA-STATUS-CODE
                                       TO CALL-ZAX33-P-RET-CODE
               MOVE TRI-AB01001-DVW-RA-UPDATE
                                       TO CALL-ZAX33-S-RET-CODE
               MOVE '6000-  '          TO CALL-ZAX33-FUNCTION
               MOVE 'TRI'              TO CALL-ZAX33-TABLE
               MOVE SDH-ENTRY-SUMMARY-NO
                                       TO CALL-ZAX33-KEY-VAL
               DO 9999-ABEND-RTN
           ENDFOR
       EJECT


091113 6100-CHECK-TRI.

           MOVE SPACES                 TO WS-IOR-SW
           MOVE TRI-IMPORTER-NO        TO WS-IOR-1
                                          WS-IOR-2
170113                                    WS-CHECK-IOR
170113     DO 0150-SEARCH-TABLE

           IF VALID-IOR-BASE-2
               MOVE 'Y'                TO WS-IOR-SW
           ENDIF

           MOVE TRI-CONS-NO            TO WS-IOR-1
                                          WS-IOR-2
170113                                    WS-CHECK-IOR
170113     IF WS-IOR-SW             NOT = 'Y'
170113         DO 0150-SEARCH-TABLE
170113     ENDIF

           IF VALID-IOR-BASE-2
               MOVE 'Y'                TO WS-IOR-SW
           ENDIF
      /
      *****************************************************************
      * 9999-ABEND-RTN                                                *
      *****************************************************************
       9999-ABEND-RTN.

           FREE SET FCX-WHOLE-REC
           FREE SET SDH-AB01001-DVW

           CALL 'ZA33BL' USING CALL-ZAX33-DATA
           GOBACK

      *
      *---------------------------------------------------------------*
      *------------------   END  OF  PROGRAM   -----------------------*
      *---------------------------------------------------------------*
