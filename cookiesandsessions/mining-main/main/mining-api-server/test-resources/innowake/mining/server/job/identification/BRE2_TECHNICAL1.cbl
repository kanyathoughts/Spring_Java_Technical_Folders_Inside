       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRE2_TECHNICAL1.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TESTFILE-FILE
                  ASSIGN       TO TESTFILE
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS TESTFILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  TESTFILE-FILE.
       01  TESTFILE-RECORD.
           05  TESTFILE-ID        PIC X(6).
           05  TESTFILE-DATA      PIC X(74).
       
       WORKING-STORAGE SECTION.
       
      *****************************************************************
       01  TESTFILE-STATUS.
           05  TESTFILE-STAT1      PIC X.
           05  TESTFILE-STAT2      PIC X.
       
      *****************************************************************
       PROCEDURE DIVISION.
       
      **   TRY A WRITE BEFORE AN OPEN AND DISPLAY STATUS ON NON-ZERO CONDITION...
           WRITE TESTFILE-RECORD
       
      ** START OF TECHNICAL RULE:
           IF  TESTFILE-STATUS NOT = '00'
               DISPLAY 'OOPS THIS WRITE DID NOT WORK'
			   IF  TESTFILE-ID NOT = '00'
                   DISPLAY 'OOPS THIS WRITE DID NOT WORK'
               END-IF
           END-IF
      ** END OF TECHNICAL RULE:
           EXIT.
