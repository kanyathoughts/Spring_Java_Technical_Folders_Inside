       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CICSPGM.cbl.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYFXOUT      ASSIGN MYFXOUT
                               FILE STATUS  IS MYFXOUT-STATUS.
           SELECT MYFBIN       ASSIGN MYFBIN
                               FILE STATUS  IS MYFBOUT-STATUS.
           SELECT MYFBIO       ASSIGN MYFBIO
                               FILE STATUS  IS MYFBOUT-STATUS.
           SELECT MYEXTEND     ASSIGN MYEXTEND
                               FILE STATUS  IS MYFBOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  MYFXOUT                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS.
       FD  MYFBIN                 RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       FD  MYFBIO                 RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       FD  MYEXTEND               RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       WORKING-STORAGE SECTION.
       01  FREAD PIC X(10) VALUE 'FILEREAD'.
       01  FREADNEXT PIC   X(10) VALUE 'READNEXTF'.
       01  FREADPREV PIC   X(10) VALUE 'READPREVF'.
       01  FSTARTBR  PIC   X(10) VALUE 'STARTBRF'.
       
       01 WS-VSAM-TABLE.
          02 WS-VSAM-TABLE-KEY.
             03 WS-TABLE-NAME PIC X(8).
             03 WS-TABLE-TYPE PIC X(1).
             03 WS-ENCODE-VALUE PIC X(30).
          02 WS-DECODE-VALUE PIC X(80).
          02 FILLER          PIC X(14).
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       DO-ALL-OPEN-OUTPUT SECTION.
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
             OPEN OUTPUT MYFXOUT
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN INPUT MYFBIN
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN I-O MYFBIO
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN EXTEND MYEXTEND
           END-IF
           DISPLAY " FSTARTBR IS NOW: " FSTARTBR.
           DISPLAY " FREADPREV IS NOW: " FREADPREV.
           DISPLAY " WS-VSAM-TABLE-KEY IS NOW: " WS-VSAM-TABLE-KEY.
           DISPLAY " WS-VSAM-TABLE IS NOW: " WS-VSAM-TABLE.
           DISPLAY " FREAD IS NOW: " FREAD.
           DISPLAY " FREADNEXTIS NOW: " FREADNEXT.
           EXEC CICS READ
                FILE(FREAD)
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
           EXEC CICS WRITE
                FILE('FILEWRITE')
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
           EXEC CICS READNEXT
                FILE(FREADNEXT)
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
           EXEC CICS READPREV
                FILE(FREADPREV)
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
           EXEC CICS STARTBR
                FILE(FSTARTBR)
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
           EXEC CICS ENDBR
                FILE("ENDBRF")
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
           EXEC CICS RESETBR
                FILE("RESETBRF")
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
           EXEC CICS REWRITE
                FILE('REWRITEF')
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
           EXEC CICS DELETE
                FILE('DELETEF')
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
           EXEC CICS UNLOCK
                FILE('UNLOCKF')
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
       
