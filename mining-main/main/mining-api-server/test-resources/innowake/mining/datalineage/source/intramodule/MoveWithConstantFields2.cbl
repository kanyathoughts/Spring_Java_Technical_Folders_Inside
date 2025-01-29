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

       01  WS-SORT-RECORD.
           05  WS-SORT-CONTROL-KEY.
               10  WS-SORT-NEW-DOC-NO            PIC X(8).
               10  WS-SORT-CUR-DOC-SUFFIX        PIC X(8).
               10  WS-SORT-CUR-DOC-PREFIX        PIC X(8).

       01 WS-SPACE-FLG                 PIC X(01) VALUE 'N'.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

       0000-MAIN-LINE.

           IF  NEW-CUR-DOC-NO   =  SPACE
               MOVE LOW-VALUES        TO WS-SORT-NEW-DOC-NO
               MOVE TRUE TO WS-SPACE-FLG.
               PERFORM 2100-ACCT-EVENT
           ELSE
               MOVE ZERO     TO WS-SORT-NEW-DOC-NO.
               MOVE FALSE TO WS-SPACE-FLG.

           GOBACK.

       2100-ACCT-EVENT.

           MOVE ZEROS            TO WS-SORT-CUR-DOC-SUFFIX
		                                  WS-SORT-CUR-DOC-PREFIX.
