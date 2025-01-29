****************************************************************************
******** MMRS-M01  mainframe modernization reference system
****************************************************************************
*******  show hex values of string (max. 33000 Byte long)
**********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMABCD.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       77  MY-PROGRAM-NAME        PIC X(8) VALUE 'MMABCD'.
      *
      * Show Hex Values **********************************************
       01  MY-HEX-SHOW-CHARS PIC X(16) VALUE '0123456789ABCDEF'.
       01  REDEFINES MY-HEX-SHOW-CHARS.
              05 MY-HEX-SHOW-CHAR PIC X(1) OCCURS 16.
              
       01  DP004-DB2-ERROR-FIELDS.
           05  DP004-FORMATTED-MESSAGE.
               10  FILLER                PIC S9(04)  VALUE +900
                                                     COMP SYNC.
               10  DP004-MESSAGE-LINE    OCCURS 12 TIMES
                                         INDEXED BY DP004-MSG-IDX
                                         PIC  X(75).

      *    ************************************************************
       PROCEDURE DIVISION USING MY-HEX-ORIGIN-LEN, MY-HEX-ORIGIN,
                                MY-HEX-CONV-RESULT.
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
       END PROGRAM "MMABCD".
