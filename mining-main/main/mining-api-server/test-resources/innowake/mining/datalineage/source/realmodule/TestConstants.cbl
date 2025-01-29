       IDENTIFICATION DIVISION.
       PROGRAM-ID.     SAMPLE01.
       AUTHOR.         RAHUL SINGH.
       DATE-WRITTEN.   JULY, 2023.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  NEW-RECORD.
           05  NEW-CUR-DOC-NO                  PIC X(8).
           05  EDR-CUR-DOC-NO                  PIC X(8).

       01  WS-SORT-RECORD.
           05  WS-SORT-CONTROL-KEY.
               10  WS-SORT-NEW-DOC-NO            PIC X(8).
               10  WS-SORT-CUR-DOC-SUFFIX        PIC X(8).
			   10  WS-SORT-CUR-DOC-PREFIX        PIC X(8).

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       0000-MAIN-LINE.

           IF  NEW-CUR-DOC-NO   =  SPACES
               MOVE HIGH-VALUES        TO WS-SORT-NEW-DOC-NO
               PERFORM 2100-ACCT-EVENT
           ELSE
               MOVE EDR-CUR-DOC-NO     TO WS-SORT-NEW-DOC-NO.

           GOBACK.

       2100-ACCT-EVENT.

           MOVE HIGH-VALUES            TO WS-SORT-CUR-DOC-SUFFIX
		                                  WS-SORT-CUR-DOC-PREFIX.
