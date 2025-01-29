****************************************************************************
******** MMRS-M01  mainframe modernization reference system
****************************************************************************
******* MMRS71D1  add value in working storage section and pass it to caller
**********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS71D1.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS71D1:'.
           COPY MMRS710A.
      *    ************************************************************
       01  MMRS71Z3               PIC X(08) VALUE 'MMRS71Z3'.
       01  MY-COUNTER             PIC 9(04) VALUE ZERO.
      *    ************************************************************
           COPY MMRS710D.
      *    ************************************************************
       LINKAGE SECTION.
       01  DFHCOMMAREA               PIC X(250).
      *    ************************************************************
       PROCEDURE DIVISION.
           EXEC CICS ASKTIME END-EXEC
           IF  EIBCALEN > 0
             MOVE DFHCOMMAREA   TO MMRS-COMMAREA
             ADD  1             TO MY-COUNTER
             MOVE MY-COUNTER    TO MMRS-COUNTER-D1
             MOVE MMRS-COMMAREA TO DFHCOMMAREA
           ELSE
             ADD  1             TO MY-COUNTER
             MOVE MY-COUNTER    TO MMRS-COUNTER-D1
           END-IF
           EXEC CICS ASKTIME END-EXEC
           CALL MMRS71Z3 USING MMRS-COMMAREA
           EXEC CICS ASKTIME END-EXEC
           CALL MMRS71Z3 USING MMRS-COMMAREA
           EXEC CICS ASKTIME END-EXEC
           CALL MMRS71Z3 USING MMRS-COMMAREA
           EXEC CICS ASKTIME END-EXEC
           CALL MMRS71Z3 USING MMRS-COMMAREA
           EXEC CICS ASKTIME END-EXEC
           CALL MMRS71Z3 USING MMRS-COMMAREA
           EXEC CICS ASKTIME END-EXEC
           MOVE MMRS-COMMAREA TO DFHCOMMAREA
           EXEC CICS ASKTIME END-EXEC
           GOBACK
      *    EXEC CICS RETURN  END-EXEC
           .
      *    --------------------------------------------------
