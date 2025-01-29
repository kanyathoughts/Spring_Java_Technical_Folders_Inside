****************************************************************************
******** MMRS-M01  mainframe modernization reference system
****************************************************************************
       01  MMRS-COMMAREA.
           05  MMRS-COMMAREA-ALL           PIC X(250).
           05  REDEFINES MMRS-COMMAREA-ALL.
             10 MMRS-LOGIN                 PIC X(2).
                88 MMRS-LOGIN-OK           VALUE 'OK'.
                88 MMRS-LOGIN-NOK          VALUE '--'.
             10 MMRS-CLEAR                 PIC X(2).
                88 MMRS-CLEAR-YES          VALUE 'CL'.
                88 MMRS-CLEAR-NO           VALUE '--'.
             10 MMRS-COUNTER-D1            PIC 9(4).
             10 MMRS-COUNTER-Z3            PIC 9(4).

       01  MMRS-COMMON-TEXT-FIELDS.
           05  MMRS-ASK-FOR-LOGIN          PIC X(79) VALUE
           'Please enter login credentials '.
           05  MMRS-FEEDBACK-OK            PIC X(79) VALUE
           'Login OK'.
           05  MMRS-FEEDBACK-NOK           PIC X(79) VALUE
           'Login not successful'.
           05  MMRS-FEEDBACK-END           PIC X(79) VALUE
           'Application ended'.
           05  MMRS-LOGIN-USER-NOK         PIC X(79) VALUE
           'invalid user id '.
           05  MMRS-LOGIN-PASS-NOK         PIC X(79) VALUE
           'invalid password '.

       01  MMRS-COMMON-COBOL-DATE-FIELDS.
           10  MMRS-CURRENT-DATE-JHJJMMDD.
               15  MMRS-CURRENT-YEAR-JH    PIC  9(2).
               15  MMRS-CURRENT-YEAR-JJ    PIC  9(2).
               15  MMRS-CURRENT-MONTH      PIC  9(2).
               15  MMRS-CURRENT-DAY        PIC  9(2).
           10  MMRS-CURRENT-TIME-HHMMSSMS.
               15  MMRS-CURRENT-HOUR       PIC  9(2).
               15  MMRS-CURRENT-MINUTE     PIC  9(2).
               15  MMRS-CURRENT-SECOND     PIC  9(2).
               15  MMRS-CURRENT-MS         PIC  9(2).
           10  MMRS-DIFF-GMT-FIELDS.
               15  MMRS-DIFF-GMT-NUM       PIC S9(4).

       01  MMRS-COMMON-DISP-DATE-FIELDS.
           10  MMRS-DISP-DATE-JHJJMMDD.
               15  MMRS-CURRENT-DAY        PIC  9(2).
               15  FILLER                  PIC  X(1).
               15  MMRS-CURRENT-MONTH      PIC  9(2).
               15  FILLER                  PIC  X(1).
               15  MMRS-CURRENT-YEAR-JH    PIC  9(2).
               15  MMRS-CURRENT-YEAR-JJ    PIC  9(2).
           10  MMRS-DISP-TIME-HHMMSS.
               15  MMRS-CURRENT-HOUR       PIC  9(2).
               15  FILLER                  PIC  X(1).
               15  MMRS-CURRENT-MINUTE     PIC  9(2).
               15  FILLER                  PIC  X(1).
               15  MMRS-CURRENT-SECOND     PIC  9(2).

      *01  MMRS-COMMON-DISP-DATE-FIELDS.
      *    10  MMRS-DISP-DATE-JHJJMMDD.
      *        15  MMRS-DISP-DAY        PIC  9(2).
      *        15  FILLER               PIC  X(1).
      *        15  MMRS-DISP-MONTH      PIC  9(2).
      *        15  FILLER               PIC  X(1).
      *        15  MMRS-DISP-YEAR-JH    PIC  9(2).
      *        15  MMRS-DISP-YEAR-JJ    PIC  9(2).
      *    10  MMRS-DISP-TIME-HHMMSS.
      *        15  MMRS-DISP-HOUR       PIC  9(2).
      *        15  FILLER               PIC  X(1).
      *        15  MMRS-DISP-MINUTE     PIC  9(2).
      *        15  FILLER               PIC  X(1).
      *        15  MMRS-DISP-SECOND     PIC  9(2).
