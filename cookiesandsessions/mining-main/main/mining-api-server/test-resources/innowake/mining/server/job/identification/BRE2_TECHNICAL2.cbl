       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRE2_TECHNICAL2.
        
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT IFILE ASSIGN TO DYNAMIC WS-FNAME
              FILE STATUS IS FILE-STAT.
        
       DATA DIVISION.
       FILE SECTION.
       FD IFILE
           RECORD CONTAINS 88 CHARACTERS.
       01 STUDENT-INFO.
          05  STUDENT-NAME OCCURS 4 TIMES.
              10  STDNT-NAME  PIC X(15).
              10  STDNT-IDNO  PIC X(7).
        
       WORKING-STORAGE SECTION.
       01 I                   PIC  9.
       77 WS-FNAME            PIC X(30).
       77 FILE-STAT           PIC XX.
        
       PROCEDURE DIVISION.
        
          DISPLAY "FILENAME? ".
          ACCEPT WS-FNAME.
          OPEN INPUT IFILE.
        
      ** START OF TECHNICAL RULE:
          IF (FILE-STAT = "35") THEN
             DISPLAY "ERROR: FILE " WS-FNAME(1:20) " DOES NOT EXIST"
          END-IF.
      ** END OF TECHNICAL RULE
          CLOSE IFILE.
          STOP RUN.
