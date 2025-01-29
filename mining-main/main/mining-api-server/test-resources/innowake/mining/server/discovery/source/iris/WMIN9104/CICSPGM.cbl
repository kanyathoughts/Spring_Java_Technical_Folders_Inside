       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSPGM.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-INPUT.
           05 WS-TRAN-ID      PIC X(04).
           05 WS-MESSAGE-I    PIC X(70).
       01 WS-OUTPUT.
           05 WS-TEXT         PIC X(08).
           05 WS-MESSAGE-O    PIC X(70).

       01 WS-MSG-LENGTH       PIC S9(4) COMP.

       01 WS-PROGRAMS.
           05 FILLER          PIC X(08) VALUE 'CICSPGM1'.
           05 FILLER          PIC X(08) VALUE 'CICSPGM2'.
       01 FILLER REDEFINES WS-PROGRAMS.
           05 WS-PROGRAM      PIC X(08) OCCURS 2 TIMES.
       01 WS-PROGRAMSA.
           05 FILLERA          PIC X(08) OCCURS 1 TIMES.
           05 FILLERA          PIC X(08) OCCURS 1 TIMES.
       01 FILLERA REDEFINES WS-PROGRAMSA.
           05 WS-PROGRAMA      PIC X(08) OCCURS 2 TIMES.
       01  PGM-INDX           PIC S9  VALUE ZERO.
       01  WS-RESP            PIC S9(08) COMP.

       01 COMM-AREA           PIC X(100).

       LINKAGE SECTION.

       01 DFHCOMMAREA         PIC X(100).

       PROCEDURE DIVISION.

           MOVE DFHCOMMAREA   TO COMM-AREA
           MOVE 74         TO WS-MSG-LENGTH.
           EXEC CICS RECEIVE
                     INTO(WS-INPUT)
                     LENGTH(WS-MSG-LENGTH)
           END-EXEC.

           MOVE MS-MESSAGE-I    TO WS-MESSAGE-O.
           MOVE 'OUTPUT: '      TO WS-TEXT.
           MOVE 78              TO WS-MSG-LENGTH.

           EXEC CICS SEND
                     FROM(WS-OUTPUT)
                     LENGTH(WS-MSG-LENGTH)
                     ERASE
           END-EXEC.

           MOVE +1             TO PGM-INDX

           EXEC CICS XCTL
                     PROGRAM (WS-PROGRAM (PGM-INDX))
                     COMMAREA (COMM-AREA)
                     LENGTH (LENGTH OF COMM-AREA)
                     RESP(WS-RESP)
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.
           
           EXEC CICS XCTL
                     PROGRAM (WS-PROGRAMA (PGM-INDX))
                     COMMAREA (COMM-AREA)
                     LENGTH (LENGTH OF COMM-AREA)
                     RESP(WS-RESP)
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.

           GOBACK.
