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

       COPY MMRS710E.
       COPY MMRS710F.


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
