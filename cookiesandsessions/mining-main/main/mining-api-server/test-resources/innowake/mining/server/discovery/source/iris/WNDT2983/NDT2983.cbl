       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.
      *
           SAMPL04.
      *
       AUTHOR.
      *

      *
       INSTALLATION.
      *
      *
       DATE-WRITTEN.
      *
           1983 SEPTEMBER 20.
      *
       DATE-COMPILED.
      *
       SECURITY.
           OFFICIAL USE ONLY.

       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
RC0129 SOURCE-COMPUTER.    UNISYS-2200.
RC0129 OBJECT-COMPUTER.    UNISYS-2200.

RC0129 SPECIAL-NAMES.
RC0129     CONSOLE IS CONSOLE
           PRINTER IS PRINTER.

      *

       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      *
DG0193******************************************************************
DG0193*             CCA0130 IS A DISC FILE FOR PRODUCTION              *
DG0193*                                                                *
DG0193******************************************************************
      *
           SELECT PRESORT-FILE
               ASSIGN TO DISC CCA0121.


           SELECT IDRS-CASE-ASGMT-LISTINGS
               ASSIGN TO PRINTER CCA0141.


      /
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD  PRESORT-FILE
           LABEL RECORDS OMITTED.
      *
       01  PF-RECORD                                    PIC X(110).
       
       FD  IDRS-CASE-ASGMT-LISTINGS
           LABEL RECORDS STANDARD
           REPORT IS IDRS-CASE-ASSIGNMENT-REC.
      *
      *    -------------------------------------------------------------


      /
       WORKING-STORAGE SECTION.
      *
       01  TOTAL-NUM-OF-RECS                      PIC 9(5).
       01  TOTAL-NUM-OF-RECORDS                   PIC 9(5)
                                                  BINARY VALUE ZERO.



DB0398 01  SC-CODE-HOLD                            PIC 99.





RC1211 01  BUR-CASE-CTRL-ACTION-CNT                     PIC 9(5)
RC1211                                                  BINARY
RC1211                                                  VALUE ZERO.

       01  TOTAL-IDRS-CASE-ASSIGNMENTS                  PIC 9(5)
                                                        BINARY
                                                        VALUE ZERO.
       01  TOTAL-INPUT-DATA-RECORDS-CNT                 PIC 9(5)
                                                        BINARY
                                                        VALUE ZERO.
       01  TOTAL-PROCESSD-INFO-RECS-CNT                 PIC 9(5)
                                                        BINARY
                                                        VALUE ZERO.
       01  ASSIGNEE-EMPLEE-NUM-COUNT                    PIC 9(5)
                                                        BINARY
                                                        VALUE ZERO.
       01  EDITED-ASSIGNEE-COUNT                        PIC 9(5)
                                                        BINARY
                                                        VALUE ZERO.
       01  CCA0130-SW                                   PIC X(3)
                                                        VALUE 'NO '.
      /
       01  OPERATOR-REPLY                               PIC X(1).

       01  OPERATOR-QUERY1.
           02  FILLER                                PIC X(47) VALUE
               'DO YOU WISH TO CONTINUE?  RESPOND Y TO CONTINUE'.
           02  FILLER                                PIC X(85)
                                                     VALUE SPACES.

       01  OPERATOR-QUERY2.
           02  FILLER                                PIC X(44) VALUE
               '                                  N TO ABORT'.
           02  FILLER                                PIC X(88)
                                                     VALUE SPACES.

       01  INTERNAL-BAL-ERR-MSG1.
           02  FILLER                                PIC X(45) VALUE
               'INTERNAL BALANCING DETECTED AN OUT OF BALANCE'.
           02  FILLER                                PIC X(87)
                                                        VALUE SPACES.

       01  INTERNAL-BAL-ERR-MSG2.
           02  FILLER                              PIC X(22) VALUE
               'CONDITION IN RUN SAMPL04'.
           02  FILLER                              PIC X(110)
                                                   VALUE SPACES.

       01  INTERNAL-BAL-ERR-MSG3.
           02  FILLER                              PIC X(17) VALUE
               'CCA0140 EXPECTED'.
           02  EXPECTED-CNT                        PIC Z,ZZZ,ZZ9.
           02  FILLER                              PIC X(11) VALUE
               ' PROCESSED '.
           02  PROCESSED-CNT                       PIC Z,ZZZ,ZZ9.
           02  FILLER                              PIC X(88)
                                                   VALUE SPACES.

      /
       01  CASE-CONTROL-ACTION-FILE-IND                 PIC A(4).
DB0395     88  UNDERREPORTER-DATA                       VALUES 'UA  '.
RC1201     88  BUR-DATA                                 VALUES 'BUR '.
VM9805     88  SC-CARD-FILE                             VALUE  'SCC '.
       01  SC-CARD-FILE-EOF-INDICATOR                   PIC X(3).
DB0395 01  UND-ACTIVITY-EOF-IND                         PIC X(3).
RC1201 01  BUR-ACTIVITY-EOF-IND                         PIC X(3).

       01  PREVIOUS-RECORD-TYPE                         PIC X
                                                        VALUE SPACE.
           88  PREVIOUS-RECORD-HEADER                   VALUE 'H'.
           88  PREVIOUS-RECORD-TEXT                     VALUE 'T'.

       01  CURRENT-RECORD-TYPE                          PIC X
                                                        VALUE SPACE.
           88  CURRENT-RECORD-HEADER                    VALUE 'H'.
           88  CURRENT-RECORD-TEXT                      VALUE 'T'.

       01  FIRST-RECORD-IND                             PIC 9
                                                        VALUE ZERO.
           88  FIRST-RECORD                             VALUE ZERO.
           88  NOT-FIRST-RECORD                         VALUE 1.

       01  CURRENT-RECORD                               PIC X(80)
                                                        VALUE SPACES.
           88  INPUT-RECORD-HEADER
                                      VALUES 'IDRS ADJ CONTROL HEADER '
                                             'IDRS CASE CTRL ACTY HDR '.

       01  END-OF-PRESORT-FILE                          PIC X
                                                        VALUE '0'.
           88  DATA-RECORD                              VALUE '0'.



RC1104* Note(s): File defintion of CASE-CTRL-ACTION-DATA-CRD-FL and UND-CASE-CTRL-ACTION-FILE
       01  CCDCF-RECORD                                PIC X(80).

       01  CCDCF-HEADER REDEFINES
           CCDCF-RECORD.
           02  CCDCF-H-ID                              PIC A(23).
           02  FILLER                                   PIC A.
           02  CCDCF-H-CASE-STATUS-CD                  PIC A.
           02  FILLER                                   PIC A.
           02  CCDCF-H-DO-SC-SUBSTATN-NMBR.
               03  CCDCF-H-DSSN-CODE                   PIC XXX.
               03  CCDCF-H-DSSN-EMPLOYEE-ID            PIC X(5).
           02  FILLER                                   PIC A.
           02  CCDCF-H-DO-SC-CD                        PIC XX.
           02  FILLER                                  PIC A(42).
           02  CCDCF-H-CASE-CTRL-ACTION-CD             PIC X.

       01  CCDCF-TEXT REDEFINES
           CCDCF-RECORD.
           02  CCDCF-T-CASE-CONTROL-NUMBER.
               03  CCDCF-T-CCN-YR-COUNT                PIC 9.
               03  CCDCF-T-CCN-JULIAN-DAY              PIC 9(3).
               03  CCDCF-T-CCN-SYSTEMS-CODE            PIC X.
               03  CCDCF-T-CCN-SERIAL-NUMBER           PIC X(5).
           02  CCDCF-T-DAT                             PIC X(9).
           02  CCDCF-T-XYZ-CLASS-CD                    PIC X.
           02  CCDCF-T-XYZ-PRD.
               03  CCDCF-T-TP-YEAR                     PIC 9(4).
               03  CCDCF-T-TP-MONTH                    PIC 99.
           02  CCDCF-T-FILE-LOC-CD                     PIC XX.
           02  CCDCF-T-IRS-RCVD-DATE.
               03  CCDCF-T-IRD-CENT-YEAR.
                   04  CCDCF-T-IRD-CENT                PIC 99.
                   04  CCDCF-T-IRD-YEAR                PIC 99.
               03  CCDCF-T-IRD-MONTH                   PIC 99.
               03  CCDCF-T-IRD-DAY-OF-MONTH            PIC 99.

           02  CCDCF-T-PRIORITY-CODE                   PIC X.
DBY2K      02  CCDCF-T-CASE-CONTROL-CAT-CD.
DBY2K          03  CONTROL-CATEGORY          PIC X(4).
DBY2K          03  FILLER                    PIC X(8).
           02  CCDCF-T-MFT-CD                          PIC 99.
           02  CCDCF-T-REFUND-CP-36-INDCTR             PIC X.
           02  CCDCF-T-EMPLEE-PLAN-NUM                 PIC X(3).
           02  FILLER                                  PIC X(4).
           02  CCDCF-T-ALPHA-FREEZE-CODES              PIC X(11).
           02  FILLER                                  PIC X.
           02  CCDCF-T-AM-CATEGORY-CD                  PIC 99.
           02  CCDCF-T-VALIDITY-DIGIT                  PIC X.
           02  CCDCF-T-NAME-CTRL                       PIC X(4).
           02  CCDCF-T-FOLLOW-UP-CD                    PIC X.
           02  FILLER                                  PIC X.





       01  CASE-CONTROL-ACTION-INFO.
           02  CCAI-HEADER.
               03  CCAI-H-DATA-SOURCE                   PIC A.
               03  CCAI-H-SIGNIFICANCE-IND              PIC 9.
               03  CCAI-H-CASE-CTRL-ACTION-CODE         PIC X.
               03  CCAI-H-CS-CTRL-ACTION-CD-NMR
                   REDEFINES CCAI-H-CASE-CTRL-ACTION-CODE    PIC 9.
               03  CCAI-H-STATUS-CD                     PIC X.
                   03  CCAI-H-STATUS-CD-ALPHABETIC
                       REDEFINES CCAI-H-STATUS-CD       PIC X.

       01  CASE-ASSIGNMENT-REPORT-ERROR                 PIC X
                                                        VALUE '0'.
       01  PRIOR-ASSIGNEE-EMPLEE-NUM                    PIC X(10)
                                                        VALUE SPACES.
       01  PRIOR-FILE-IDENTIFIER                        PIC X(8)
                                                        VALUE SPACES.
       01  CONTINUATION-LITERAL                         PIC X(9)
                                                        VALUE SPACES.

       01  IDRS-CASE-ASGMT-LIST-RECORD.
           02  CASE-ASSIGNMENT-HEADER-INFO.
               03  FILE-IDENTIFIER                      PIC A(4).
               03  ACTION-TYPE                          PIC X(12).
               03  EDITED-ASSIGNEE-EMPLEE-NUM.
                   04  DO-SC-CD                         PIC XX.
                   04  FILLER                           PIC A
                                                        VALUE SPACE.
                   04  DO-SC-SUBSTATION-CODE            PIC XXX.
                   04  FILLER                           PIC A
                                                        VALUE SPACE.
                   04  DO-SC-SUBSTATION-EMPLOYEE-ID     PIC X(5).
               03  CASE-STATUS-CD                       PIC X.
               03  HEADER-ERROR-REASON-CODES.
                   04  ACTION-CODE-ERROR-REASON         PIC X.
                   04  STATUS-CODE-ERROR-REASON         PIC X.
                   04  ASSIGNEE-EMPL-NUM-ERR-REASON     PIC X.
                   04  HEADER-SEQUENCE-ERROR-REASON     PIC X.
           02  EDITED-IDRS-CASE-ASGMT-LINE.
               03  CASE-CONTROL-NUMBER.
                   04  YR-COUNT                         PIC X.
                   04  JULIAN-DAY                       PIC X(3).
                   04  SYSTEMS-CODE-OF-CASE-CTRL-NO     PIC X.
                   04  SERIAL-NUMBER                    PIC X(5).
               03  XTIN                                 PIC X(9).
               03  VALIDITY-DIGIT                       PIC X.
               03  SYSTEMS-CODE                         PIC X.
               03  EMPLEE-PLAN-NUM                      PIC X(3).
               03  XMFT-CD                              PIC XX.
               03  XTAX-PRD.
                   04  CENT                             PIC XX.
                   04  YEAR.
                       05  DECADE-COUNT                 PIC X.
                       05  YR-COUNT                     PIC X.
                   04  MONTH                            PIC XX.
               03  NAME-CTRL                            PIC X(4).
               03  CASE-CONTROL-CATEGORY-CODE           PIC X(12).
               03  TEXT-ERROR-REASON-CODES.
                   04  SPECIFIC-TEXT-HEADER-ERR-CD      PIC X.
                   04  CASE-CTRL-NUM-ERROR-REASON       PIC X.
                   04  SYSTEMS-CD-VAL-DIGIT-ERR-CD      PIC X.
                   04  DAT-ERROR-REASON                 PIC X.
                   04  EMPLEE-PLAN-NUM-ERROR-REASON     PIC X.
                   04  XYZ-PRD-ERROR-REASON             PIC X.
                   04  MFT-CD-ERROR-REASON              PIC X.
                   04  CASE-CTRL-CAT-CD-ERR-REASON      PIC X.
                   04  NAME-CTRL-ERROR-REASON           PIC X.
                   04  IRS-RCVD-DATE-ERROR-REASON       PIC X.
               03  ICALR-EICAL-RLS-T-SRT-SEQ-NM         PIC 9(6).
           02  REPORT-BREAKS.
               03  FILE-IDENTIFICATION                  PIC 9.
               03  ACTION-CODE                          PIC 9.
               03  ASSIGNEE-EMPLEE-NUM                  PIC 9(10).
               03  REPORT-MFT-CD                        PIC XX.
               03  EDITED-CASE-STATUS-CD                PIC X.
           02  FILLER                                   PIC XX.
       01  EDIT-CASE-ASGMT-LIST-FRST-SW                 PIC X(3)
                                                        VALUE 'ON '.



      /
      *FOLLOWING COPY MUST BE KEPT.  IT CORRELATES WITH REPORT SECTION.



       
       PROCEDURE DIVISION.
      *
       DECLARATIVES.
      *
       HEADING-PRINT SECTION.
      *
           USE BEFORE REPORTING IDRS-ASSGNMNT-LIST-TITLE.
      *
       PRINT-HEADING.
      *
       SUPPRESS PRINTING
           IF (FIRST-RECORD)
             AND (PAGE-COUNTER OF IDRS-CASE-ASSIGNMENT-REC = 1)
      *        THEN
                 MOVE ZERO TO PAGE-COUNTER OF IDRS-CASE-ASSIGNMENT-REC
                 MOVE 1 TO FIRST-RECORD-IND
                 SUPPRESS PRINTING
               ELSE
                 IF PRIOR-FILE-IDENTIFIER NOT EQUAL FILE-IDENTIFIER
                   THEN
                     MOVE 1 TO PAGE-COUNTER OF IDRS-CASE-ASSIGNMENT-REC.
      /
       DETAIL-PRINT SECTION.
      *
           USE BEFORE REPORTING ED-IDRS-CASE-ASGMT-LINE.
      *
       DP-ASSIGNEE-EMPLEE-NUM-COUNT.

           ADD 1 TO ASSIGNEE-EMPLEE-NUM-COUNT.
      /
       FOOTING-PRINT SECTION.
      *
           USE BEFORE REPORTING ASGNEE-FILE-TOT-CASES-LN.
      *
       PRINT-FOOTING.
      *
           IF (EDITED-ASSIGNEE-EMPLEE-NUM = PRIOR-ASSIGNEE-EMPLEE-NUM)
             AND (DATA-RECORD)
      *        THEN
                 MOVE 'CONTINUED' TO CONTINUATION-LITERAL
                 SUPPRESS PRINTING
               ELSE
                 MOVE EDITED-ASSIGNEE-EMPLEE-NUM TO
                   PRIOR-ASSIGNEE-EMPLEE-NUM.
      *    ENDIF

      /
       REPORT-ASSIGNMENT-ERROR SECTION.
      *
           USE BEFORE REPORTING IDRS-CASE-ERROR-LINE.
      *
       PRINT-ERROR-LINE.
      *
           IF CASE-ASSIGNMENT-REPORT-ERROR = ZERO
      *      THEN
               SUPPRESS PRINTING.
      *    ENDIF
      *
       END DECLARATIVES.
      *
       MAIN-PROGRAM SECTION.

       CK-REFRM-CASE-CTRL-ACTIONS.
      *
           OPEN INPUT PRESORT-FILE.
           OPEN OUTPUT IDRS-CASE-ASGMT-LISTINGS.

           MOVE ZERO TO FIRST-RECORD-IND.
           INITIATE IDRS-CASE-ASSIGNMENT-REC.
           PERFORM GENERATE-IDRS-ASGMT-LISTINGS
             UNTIL END-OF-PRESORT-FILE = '1'.
           TERMINATE IDRS-CASE-ASSIGNMENT-REC.

           CLOSE IDRS-CASE-ASGMT-LISTINGS.

           STOP RUN.
      /


      /
       GENERATE-IDRS-ASGMT-LISTINGS.
      *
           READ PRESORT-FILE INTO IDRS-CASE-ASGMT-LIST-RECORD
             AT END
               MOVE '1' TO END-OF-PRESORT-FILE.
           IF DATA-RECORD
      *      THEN        
                GENERATE ED-IDRS-CASE-ASGMT-LINE.
      /


