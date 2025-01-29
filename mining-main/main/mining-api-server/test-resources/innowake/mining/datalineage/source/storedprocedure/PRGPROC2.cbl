       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETPRML.
       AUTHOR. EXAMPLE.
       DATE-WRITTEN.   03/25/98.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       
       WORKING-STORAGE SECTION.
       
      *     EXEC SQL INCLUDE SQLCA END-EXEC.
      ***************************************************
      *   DECLARE A HOST VARIABLE TO HOLD INPUT SCHEMA
      ***************************************************
       01  INSCHEMA PIC X(8).
       
      ***************************************************
      *   DECLARE CURSOR FOR RETURNING RESULT SETS
      ***************************************************
      *
           EXEC SQL DECLARE C1 CURSOR WITH RETURN FOR
             SELECT NAME FROM SYSIBM.SYSTABLES WHERE CREATOR=:INSCHEMA
           END-EXEC.
      *
       LINKAGE SECTION.
      *******************************************************
      *   DECLARE THE OUTPUT PARAMETERS FOR THE PROCEDURE
      *******************************************************
       01  RESULT PIC X(80).
      ***************************************************
      *   DECLARE THE INPUT PARAMETERS FOR THE PROCEDURE
      ***************************************************
       01  PUBLICATION  PIC X(8).
       01  ARTICLE      PIC X(8).
       01  ISSUE        PIC S9(4) USAGE BINARY.
       01  COUNTP       PIC S9(4) USAGE BINARY.
       01  OTHERP       PIC X(8).

       PROCEDURE DIVISION USING RESULT, PUBLICATION, ARTICLE,
                 ISSUE, COUNTP, OTHERP.

            GOBACK.