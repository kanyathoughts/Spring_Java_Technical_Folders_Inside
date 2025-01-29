****************************************************************************
******** MMRS-M01  mainframe modernization reference system
****************************************************************************
*******  show hex values of string (max. 33000 Byte long)
**********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS71Z1.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       77  MY-PROGRAM-NAME        PIC X(8) VALUE 'MMRS71Z1'.
      *
      * Show Hex Values **********************************************
       01  I1      PIC S9(9) COMP.
       01  I1-DISP      PIC 9(9).
       01  I2      PIC S9(9) COMP.
       01  I2-DISP      PIC 9(9).
       01  MY-HEX-ZONE      PIC S9(4) COMP.
       01  MY-HEX-DIGIT     PIC S9(4) COMP.
       01  WPLAN-CTR-BKPI     PIC S9(4) COMP.
       01  WCTR-BKPI     PIC S9(4) COMP.
       01  C     PIC S9(4) COMP.
       01  D     PIC S9(4) COMP.
       01  E     PIC S9(4) COMP.
       01  F     PIC S9(4) COMP.

       01  MY-HEX-CONV.
           05 FILLER-1          PIC X(1) VALUE LOW-VALUES.
           05 MY-HEX-CONV-CHAR  PIC X(1).
       01  MY-HEX-CONV-NUM   REDEFINES MY-HEX-CONV PIC 9(4) COMP.

       01  MY-HEX-SHOW-CHARS PIC X(16) VALUE '0123456789ABCDEF'.
       01  REDEFINES MY-HEX-SHOW-CHARS.
              05 MY-HEX-SHOW-CHAR PIC X(1) OCCURS 16.

       LINKAGE SECTION.
       01  MY-HEX-ORIGIN-LEN     PIC 9(5).
       01  MY-HEX-ORIGIN         PIC X(33000).
       01  MY-HEX-ORIGIN-R       REDEFINES MY-HEX-ORIGIN.
           05 MY-HEX-ORIGIN-BYTE PIC X(1) OCCURS 33000.

       01  MY-HEX-CONV-RESULT.
           05 MY-HEX-CONV-RESULT-CHARS OCCURS 33000.
                10 MY-HEX-CONV-RESULT-ZONE  PIC X(1).
                10 MY-HEX-CONV-RESULT-DIGIT PIC X(1).
      *    ************************************************************
       PROCEDURE DIVISION USING MY-HEX-ORIGIN-LEN, MY-HEX-ORIGIN,
                                MY-HEX-CONV-RESULT.
      *    ************************************************************
      *
           COMPUTE C D = C + 1.
           SUBTRACT 1 FROM D
           ADD 1 TO WPLAN-CTR-BKPI WCTR-BKPI.
           ADD E To 1 Giving E.
           IF  MY-HEX-ORIGIN-LEN > 0
           AND MY-HEX-ORIGIN-LEN < LENGTH OF MY-HEX-ORIGIN
             MOVE MY-HEX-ORIGIN-LEN  TO I2
             SUBTRACT 1   FROM I2
             MOVE 1   TO I1
             PERFORM SHOW-DATA-HEX-CHARACTER I2 TIMES
           END-IF
           .
      *    ************************************************************
      *
       SHOW-DATA-HEX-CHARACTER SECTION.
           MOVE MY-HEX-ORIGIN-BYTE(I1) TO MY-HEX-CONV-CHAR
           DIVIDE MY-HEX-CONV-NUM BY 16
              GIVING MY-HEX-ZONE
              REMAINDER MY-HEX-DIGIT
           MOVE MY-HEX-SHOW-CHAR(MY-HEX-ZONE + 1)
             TO MY-HEX-CONV-RESULT-ZONE(I1)
           MOVE MY-HEX-SHOW-CHAR(MY-HEX-DIGIT + 1)
             TO MY-HEX-CONV-RESULT-DIGIT(I1)
           ADD 1 TO I1
           .
      *    ************************************************************
       END PROGRAM "MMRS71Z1".
