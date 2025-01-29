      ******************************************************************00010027
      *  PTSVSM01                                                      *00020027
      ******************************************************************00030027
      *      VSAM RECORD FORMAT FOR THE PATIENT TRACKING SYSTEM        *00040027
      *      REGISTRATION AND ADMISSION RECORD.                        *00050027
      *      COPYBOOK:  PTSVSM01 .  LENGTH = 650.                      *00060027
      ******************************************************************00070027
      *      THIS COPYBOOK HAD FIELDS ADDED AND THE TOTAL LENGTH       *00080027
      *      INCREASED TO 650 ON 5/22/96.  THIS ALLOWS FOR FUTURE      *00090027
      *      ADDITION OF FIELDS UP TO A LENGTH OF 88 WHICH IS THE      *00100027
      *      NEW LENGTH OF THE FILLER FIELD.                           *00110027
      ******************************************************************00120027
      ******************************************************************00130027
      *      VSAM RECORD FORMAT FOR THE PATIENT TRACKING SYSTEM        *00140027
      *      REGISTRATION AND ADMISSION RECORD.                        *00150027
      *      COPYBOOK:  PTSVSM01 .  LENGTH = 650.                      *00160027
      *               ADDED 6 DATE & TIME FIELDS.                      *00170027
      *               CHANGED KEY DATE FOR Y2K - 12/13/1999 DSAMOC     *00180027
      *      ADDED WS-VISIT-DISCH-BY-RPT-USER  - 10/31/2000 DSAMOC     *00181028
      *      ADDED WS-VISIT-DISCH-BY-RPT-DATE  - 11/08/2000 DSAMOC     *00182029
      ******************************************************************00190027
      *                                                                 00200027
       01  WS-VSAM01-REG-ADM-RECORD.                                    00210027
      ***************************************************************   00220027
      *      ELEMENTS USED TO IDENTIFY VSAM KEYS.                   *   00230027
      ***************************************************************   00240027
           03  WS-VSAM01-KEYS.                                          00250027
               05  WS-REG-ADM-DATE.                                     00260027
                   10  WS-REG-ADM-DATE-YY.                              00270027
                       15  WS-REG-ADM-DATE-Y1  PIC X.                   00280027
                       15  WS-REG-ADM-DATE-Y2  PIC X.                   00290027
                   10  WS-REG-ADM-DATE-M1      PIC X.                   00300027
                   10  WS-REG-ADM-DATE-M2      PIC X.                   00310027
                   10  WS-REG-ADM-DATE-D1      PIC X.                   00320027
                   10  WS-REG-ADM-DATE-D2      PIC X.                   00330027
               05  WS-REG-ADM-TIME         PIC S9(4) COMP-3.            00340027
               05  WS-EVENT-RECORD-ID      PIC X(4).                    00350027
                     88  ER-REGISTRATIONS       VALUE '0201'.           00360027
                     88  ER-OUTPAT-REG          VALUE '0201'.           00370027
                     88  VISIT-BED-ASSIGNED     VALUE '0201'.           00380027
                     88  TRAUMA-REGISTRATION    VALUE '0202'.           00390027
                     88  ANCILL-REGISTRATION    VALUE '0203'.           00400027
                     88  ORTHO-REGISTRATION     VALUE '0204'.           00410027
                     88  ER-RE-REGISTRATION     VALUE '0207'.           00420027
                     88  OP-RE-REGISTRATION     VALUE '0209'.           00430027
                     88  ER-ADMISSIONS          VALUE '0301'.           00440027
                     88  OP-ADMISSIONS          VALUE '0302'.           00450027
                     88  PRE-ADMITS             VALUE '0303'.           00460027
                     88  NEWBORN-ADMITS         VALUE '0304'.           00470027
                     88  CANCEL-ADMISSION       VALUE '0391'.           00480027
                     88  ER-IP-BED-ISSUE        VALUE '0400'.           00490027
                     88  ASSIGN-ER-BED          VALUE '0401'.           00500027
                     88  ASSIGN-OP-BED          VALUE '0402'.           00510027
                     88  PT-EA-UPGRADE          VALUE '0403'.           00520027
                     88  REQUEST-IP-BED         VALUE '0405'.           00530027
                     88  PT-IA-UPGRADE          VALUE '0503'.           00540027
                     88  BED-TIME-CHANGE        VALUE '0505'.           00550027
                     88  BED-TRANSFER           VALUE '0507'.           00560027
                     88  ER-BED-CANCEL          VALUE '0591'.           00570027
                     88  INPAT-DISCHARGE        VALUE '0601'.           00580027
                     88  RELEASE-OP-BED         VALUE '0603'.           00590027
                     88  WAITROOM-DISCHARGE     VALUE '0604'.           00600027
                     88  SHORTSTAY-DISCHARGE    VALUE '0605'.           00610027
                     88  BED-RELEASE-CANCEL     VALUE '0691'.           00620027
                     88  DISCHARGE-CANCEL       VALUE '0692'.           00630027
                     88  ER-VISIT-REVISE        VALUE '0701'.           00640027
                     88  IP-REVISION            VALUE '0702'.           00650027
               05  WS-PAT-ACCT-NO          PIC S9(12) COMP-3.           00660027
               05  WS-PAT-CASE-NO          PIC S9(12) COMP-3.           00670027
               05  WS-PAT-VISIT-NO         PIC S9(12) COMP-3.           00680027
           03  WS-VSAM01-REC.                                           00690027
               05  WS-VSAM01-PATIENT-TYPE   PIC X(01).                  00700027
                   88  INPATIENT                                        00710027
                         VALUES 'A', 'B', 'I', 'J', 'M', 'P', 'S', 'W'. 00720027
                   88  NEWBORN-MOTHER  VALUE 'A'.                       00730027
                   88  NEWBORN         VALUE 'B'.                       00740027
                   88  INTENSIVE-CARE  VALUE 'I'.                       00750027
                   88  PRIVATE-JUSTFD  VALUE 'J'.                       00760027
                   88  CORONARY-CARE   VALUE 'M'.                       00770027
                   88  PRIVATE         VALUE 'P'.                       00780027
                   88  SEMI-PRIVATE    VALUE 'S'.                       00790027
                   88  WARD-PT         VALUE 'W'.                       00800027
                   88  OUTPATIENT   VALUES 'C', 'D', 'E', 'H', 'R'.     00810027
                   88  CLINIC-PT    VALUE 'C'.                          00820027
                   88  RECURRING-PT VALUE 'D'.                          00830027
                   88  EMERGENCY-PT VALUE 'E'.                          00840027
                   88  HOME-HEALTH  VALUE 'H'.                          00850027
                   88  REFERRAL-PT  VALUE 'R'.                          00860027
               05  WS-MED-REC-NO            PIC X(12).                  00870027
               05  WS-PATIENT-NAME          PIC X(25).                  00880027
               05  WS-PT-BIRTHDATE          PIC X(08).                  00890027
               05  WS-PT-CURR-AGE           PIC S9(3) COMP-3.           00900027
               05  WS-PAT-SEX               PIC X(01).                  00910027
               05  WS-PAT-TYPE              PIC X(02).                  00920027
               05  WS-PAT-FIN-CLASS         PIC X(02).                  00930027
      *---------------------------------------------*                   00940027
      *---CHANGE TO REAL LENGTH OF INSURANCE PLAN---*                   00950027
      *---NOT ORIG SET UP TO COLLECT 4 INS, JUST 1--*                   00960027
      *---ALSO IS NOT SET UP TO CHECK INS PRIORITY--*                   00970027
      *---------------------------------------------*                   00980027
MOC            05  WS-INSUR-PLAN1           PIC X(03).                  00990027
MOC            05   FILLER                  PIC X(02).                  01000027
MOC            05   FILLER                  PIC X(06).                  01010027
MOC            05  WS-ED-BED-PLACEMENT-DT   PIC X(06).                  01020027
MOC            05  WS-ED-BED-PLACEMENT-TM   PIC S9(4) COMP-3.           01030027
               05  WS-CASE-CREATE-DATE      PIC X(06).                  01040027
               05  WS-CASE-CREATE-TIME      PIC S9(4) COMP-3.           01050027
               05  WS-OPERATOR-ID           PIC X(06).                  01060027
               05  WS-OPERATOR-TERM-ID      PIC X(04).                  01070027
               05  WS-CASE-HOSP-SERVICE     PIC X(03).                  01080027
               05  WS-CASE-NURS-STATION     PIC X(04).                  01090027
               05  WS-CASE-BED-NO           PIC X(06).                  01100027
               05  WS-CASE-ATN-DR-NO        PIC X(06).                  01110027
               05  WS-CASE-ATN-DR-NAME      PIC X(25).                  01120027
               05  WS-CASE-ADM-DR-NO        PIC X(06).                  01130027
               05  WS-CASE-ADM-REASON       PIC X(02).                  01140027
               05  WS-CASE-PT-CLIN-CD       PIC X(13).                  01150027
               05  WS-CASE-OP-IN-BED-IND    PIC X(01).                  01160027
               05  WS-CASE-OP-IN-BED-VST    PIC S9(12) COMP-3.          01170027
               05  WS-CASE-DISC-DATE        PIC X(06).                  01180027
               05  WS-CASE-DISC-TIME        PIC S9(4) COMP-3.           01190027
               05  WS-CASE-DISC-DISP        PIC X(03).                  01200027
               05  WS-VISIT-REF-DR-NO       PIC X(06).                  01210027
               05  WS-VISIT-ADM-DR-NAME     PIC X(25).                  01220027
               05  WS-VISIT-DX-CODE         PIC X(07).                  01230027
               05  WS-VISIT-DX-NAME         PIC X(30).                  01240027
               05  WS-VISIT-PROC-CODE       PIC X(07).                  01250027
               05  WS-VISIT-PROC-NAME       PIC X(20).                  01260027
               05  WS-VISIT-TRIAGE-DATE     PIC X(06).                  01270027
               05  WS-VISIT-TRIAGE-TIME     PIC S9(4) COMP-3.           01280027
               05  WS-VISIT-TRIAGE-CATG     PIC X(01).                  01290027
               05  WS-VISIT-ADM-DX-CD       PIC X(07).                  01300027
               05  WS-VISIT-ADM-SOURCE      PIC X(02).                  01310027
               05  WS-VISIT-TYPE            PIC S9(2) COMP-3.           01320027
               05  WS-VISIT-CREATE-DATE     PIC X(06).                  01330027
               05  WS-VISIT-CREATE-TIME     PIC S9(4) COMP-3.           01340027
               05  WS-VISIT-HOSP-SERV       PIC X(03).                  01350027
               05  WS-VISIT-ATN-DR-NO       PIC X(06).                  01360027
               05  WS-VISIT-ATN-DR-NAME     PIC X(25).                  01370027
               05  WS-VISIT-CLINIC-CD       PIC X(13).                  01380027
               05  WS-VISIT-CLINIC-DEPT     PIC X(03).                  01390027
               05  WS-VISIT-CLIN-CLINIC     PIC X(04).                  01400027
               05  WS-VISIT-FOLLOWUP-REFRL  PIC X(06).                  01410027
               05  WS-VISIT-OP-BED-OCC-DATE     PIC X(06).              01420027
               05  WS-VISIT-OP-BED-OCC-TIME     PIC S9(4) COMP-3.       01430027
               05  WS-VISIT-OP-BED-RLS-DATE     PIC X(06).              01440027
               05  WS-VISIT-OP-BED-RLS-TIME     PIC S9(4) COMP-3.       01450027
               05  WS-PT-STATUS             PIC X(02).                  01460027
               05  WS-EXPIRED-IND           PIC X(01).                  01470027
               05  WS-CASE-CHIEF-COMPLAINT  PIC X(30).                  01480027
               05  WS-VISIT-DISCH-DISP      PIC X(30).                  01490027
               05  WS-CASE-ADMIT-DATE       PIC X(06).                  01500027
               05  WS-CASE-ADMIT-TIME       PIC S9(4) COMP-3.           01510027
               05  WS-CASE-OBGYN-IND        PIC X(01).                  01520027
MOC   **-THE FOLLOWING FIELD NOT CURRENT BEING USED ON INVISION-**      01530027
MOC   *MOC     05  WS-VISIT-EDDR-CA-STATUS  PIC X(08).                  01540027
MOC            05   FILLER                  PIC X(08).                  01550027
               05  WS-VISIT-PT-ADMIT-YN     PIC X(01).                  01560027
               05  WS-VISIT-TRAUMA-YN       PIC X(01).                  01570027
               05  WS-VISIT-TRAUMA-TYPE     PIC X(01).                  01580027
               05  WS-VISIT-INJURY-AREA     PIC X(27).                  01590027
               05  WS-VISIT-INJRY-DTE-TIME  PIC X(10).                  01600027
MOC   **-THE FOLLOWING FIELD NOT CURRENT BEING USED ON INVISION-**      01610027
MOC   *MOC     05  WS-VISIT-NEUO-CA-STATUS  PIC X(08).                  01620027
001   *MOC     05   FILLER                  PIC X(08).                  01630029
001            05   FILLER                  PIC X(02).                  01631029
000            05   WS-VISIT-DISCH-BY-RPT-DATE PIC X(06).               01632029
MOC   **-THE FOLLOWING FIELD NOT CURRENT BEING USED ON INVISION-**      01640027
MOC   *MOC     05  WS-VISIT-TRAUMA-CRITERIA PIC X(06).                  01650027
000            05   WS-VISIT-DISCH-BY-RPT-USER PIC X(06).               01660029
MOC   **-THE FOLLOWING FIELD NOT CURRENT BEING USED ON INVISION-**      01670027
MOC   *MOC     05  WS-VISIT-AMBULANCE-RPTNO PIC X(10).                  01680027
MOC            05  WS-PATIENT-PHONE-NO      PIC X(10).                  01690027
               05  WS-VISIT-HOW-ARRIVED     PIC X(30).                  01700027
               05  WS-VISIT-DATE            PIC X(06).                  01710027
               05  WS-VISIT-TIME            PIC S9(4) COMP-3.           01720027
      ************************************************************      01730027
      *  6 NEW FIELDS ADDED TO COLLECT E/R DATE/TIMES 10/24/97   *      01740027
      *  NOTE:  WS-ER-IP-ADM-PROCESS-DATE & TIME ARE SIMPLY THE  *      01750027
      *         SYSTEM DATE/TIME OF WHEN EVENT WAS PROCESSED,    *      01760027
      *         ESPECIALLY PERTINENT TO '400' & '503' EVENTS     *      01770027
      ************************************************************      01780027
               05  WS-ER-DISCH-TO-EDAM-DATE  PIC X(06).                 01790027
               05  WS-ER-DISCH-TO-EDAM-TIME  PIC S9(4) COMP-3.          01800027
               05  WS-REQUEST-IP-ADM-DATE    PIC X(06).                 01810027
               05  WS-REQUEST-IP-ADM-TIME    PIC X(04).                 01820027
               05  WS-ER-IP-ADM-PROCESS-DATE PIC X(06).                 01830027
               05  WS-ER-IP-ADM-PROCESS-TIME PIC S9(4) COMP-3.          01840027
      ************************************************************      01850027
      *         ADDED PCP NAME            04/30/98 DSAMOC        *      01860027
      ************************************************************      01870027
               05  WS-PCP-NAME               PIC X(17).                 01880027
               05   FILLER                   PIC X(06).                 01890027
      *************************************************************     01900027
      *------------------E N D   O F   L A Y O U T----------------*     01910027
      *************************************************************     01920027
