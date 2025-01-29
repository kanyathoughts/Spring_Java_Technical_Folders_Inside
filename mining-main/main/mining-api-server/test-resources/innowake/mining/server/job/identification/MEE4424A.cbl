       IDENTIFICATION DIVISION.
       PROGRAM-ID.    M4424VA.
       AUTHOR.        INNOWAKE.
       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       INPUT-OUTPUT    SECTION.
       FILE-CONTROL.
           SELECT VSAM-FILE
             ASSIGN TO VSAMREF1
             ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC
             RECORD KEY IS CMF-KEY9
             FILE STATUS IS VSAM-STATUS.
           SELECT VSAM-FILE-OTHER
             ASSIGN TO VSAMREF1
             ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC
             RECORD KEY IS CMF-KEY9-OTHER
             FILE STATUS IS VSAM-STATUS.
           SELECT VSAM-FILE-SEQ
             ASSIGN TO VSAMREF1
             ORGANIZATION IS INDEXED
             ACCESS IS SEQUENTIAL
             RECORD KEY IS CMF-KEY9-SEQ
             FILE STATUS IS VSAM-STATUS.

       DATA            DIVISION.
       FILE SECTION.

       FD VSAM-FILE.
       01  VSAM-RECORD.
           05 CMF-KEY9.
              10  CMF-NUMBER            PIC X(9).
              10  CMF-SEQ               PIC X.
           05 CMF-REST                  PIC X(90).
       FD VSAM-FILE-OTHER.
       01  VSAM-RECORD-OTHER.
           05 CMF-KEY9-OTHER.
              10  CMF-NUMBER-OTHER     PIC X(9).
              10  CMF-SEQ-OTHER        PIC X.
           05 CMF-REST-OTHER           PIC X(90).
       FD VSAM-FILE-SEQ.
       01  VSAM-RECORD-SEQ.
           05 CMF-KEY9-SEQ.
              10  CMF-NUMBER-SEQ        PIC X(9).
              10  CMF-SEQ-SEQ           PIC X.
           05 CMF-REST-SEQ              PIC X(90).

       WORKING-STORAGE SECTION.
       01 VSAM-STATUS PIC X(2).

      ************************************************************
      *                    PROCEDURE DIVISION                    *
      ************************************************************
       PROCEDURE DIVISION.
       DECLARATIVES.
       DECL-INPUT SECTION.
           USE AFTER ERROR PROCEDURE ON INPUT.
       DECL-INPUT-P.
           DISPLAY 'INPUT PROCEDURE ' VSAM-STATUS.
       DECL-INPUT-EXIT.
           EXIT.
       DECL-OUTPUT SECTION.
           USE AFTER ERROR PROCEDURE ON OUTPUT.
       DECL-OUTPUT-P.
           DISPLAY 'OUTPUT PROCEDURE ' VSAM-STATUS.
       DECL-OUTPUT-EXIT.
           EXIT.
       DECL-IO SECTION.
           USE AFTER ERROR PROCEDURE ON I-O.
       DECL-IO-P.
           DISPLAY 'IO PROCEDURE ' VSAM-STATUS.
       DECL-IO-EXIT.
           EXIT.
       DECL-EXTEND SECTION.
           USE AFTER ERROR PROCEDURE ON EXTEND.
       DECL-EXTEND-P.
           DISPLAY 'EXTEND PROCEDURE ' VSAM-STATUS.
       DECL-EXTEND-EXIT.
           EXIT.
       END DECLARATIVES.
       BEGIN SECTION.
       BEGIN-P1.

           OPEN OUTPUT VSAM-FILE.
           MOVE '1234567890TEST' TO VSAM-RECORD.
           WRITE VSAM-RECORD.
           CLOSE VSAM-FILE.

           DISPLAY 'INPUT'.
           DISPLAY '-----------------'.
           OPEN INPUT VSAM-FILE.
           DISPLAY 'OPEN ' VSAM-STATUS.
           OPEN INPUT VSAM-FILE.
           DISPLAY 'OPEN ' VSAM-STATUS.
           CLOSE VSAM-FILE.
           DISPLAY 'CLOSE ' VSAM-STATUS.
           CLOSE VSAM-FILE.
           DISPLAY 'CLOSE ' VSAM-STATUS.

           DISPLAY '-----------------'.
           OPEN INPUT VSAM-FILE.
           DISPLAY 'OPEN ' VSAM-STATUS.
           MOVE '1234567891' TO VSAM-RECORD.
           START VSAM-FILE.
           DISPLAY 'START ' VSAM-STATUS.
           CLOSE VSAM-FILE.
           DISPLAY 'CLOSE ' VSAM-STATUS.


           DISPLAY '-----------------'.
           DISPLAY 'OUTPUT'.
           DISPLAY '-----------------'.
           OPEN OUTPUT VSAM-FILE.
           DISPLAY 'OPEN ' VSAM-STATUS.
           OPEN OUTPUT VSAM-FILE.
           DISPLAY 'OPEN ' VSAM-STATUS.
           CLOSE VSAM-FILE.
           DISPLAY 'CLOSE ' VSAM-STATUS.
           CLOSE VSAM-FILE.
           DISPLAY 'CLOSE ' VSAM-STATUS.


           DISPLAY '-----------------'.
           DISPLAY 'I-O'.
           DISPLAY '-----------------'.
           OPEN I-O VSAM-FILE.
           DISPLAY 'OPEN ' VSAM-STATUS.
           OPEN I-O VSAM-FILE.
           DISPLAY 'OPEN ' VSAM-STATUS.
           CLOSE VSAM-FILE.
           DISPLAY 'CLOSE ' VSAM-STATUS.
           CLOSE VSAM-FILE.
           DISPLAY 'CLOSE ' VSAM-STATUS.

           DISPLAY '-----------------'.
           OPEN I-O VSAM-FILE.
           DISPLAY 'OPEN ' VSAM-STATUS.
           MOVE '1234567891' TO VSAM-RECORD.
           START VSAM-FILE.
           DISPLAY 'START ' VSAM-STATUS.
           CLOSE VSAM-FILE.
           DISPLAY 'CLOSE ' VSAM-STATUS.


           DISPLAY '-----------------'.
           DISPLAY 'EXTEND'.
           DISPLAY '-----------------'.
           OPEN EXTEND VSAM-FILE-SEQ.
           DISPLAY 'OPEN ' VSAM-STATUS.
           OPEN EXTEND VSAM-FILE-SEQ.
           DISPLAY 'OPEN ' VSAM-STATUS.
           CLOSE VSAM-FILE-SEQ.
           DISPLAY 'CLOSE ' VSAM-STATUS.
           CLOSE VSAM-FILE-SEQ.
           DISPLAY 'CLOSE ' VSAM-STATUS.

           DISPLAY '-----------------'.
           OPEN EXTEND VSAM-FILE-SEQ.
           DISPLAY 'OPEN ' VSAM-STATUS.
           MOVE '1234567890' TO VSAM-RECORD-SEQ.
           WRITE VSAM-RECORD-SEQ.
           DISPLAY 'WRITE ' VSAM-STATUS.
           CLOSE VSAM-FILE-SEQ.
           DISPLAY 'CLOSE ' VSAM-STATUS.

           GOBACK.
