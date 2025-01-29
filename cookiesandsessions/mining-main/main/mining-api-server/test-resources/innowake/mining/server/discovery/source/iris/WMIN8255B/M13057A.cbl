       IDENTIFICATION DIVISION.
       PROGRAM-ID.  M13057A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

        01 OUT-REC PIC X(300).

      * DL/I FUNCTION CODES
        01 DLI-FUNCTIONS.
           05 DLI-GU          PIC X(4) VALUE "GU  ".
           05 DLI-GHU         PIC X(4) VALUE "GHU ".
           05 DLI-GN          PIC X(4) VALUE "GN  ".
           05 DLI-GHN         PIC X(4) VALUE "GHN ".
           05 DLI-GNP         PIC X(4) VALUE "GNP ".
           05 DLI-GHNP        PIC X(4) VALUE "GHNP".
           05 DLI-ISRT        PIC X(4) VALUE "ISRT".
           05 DLI-DLET        PIC X(4) VALUE "DLET".
           05 DLI-REPL        PIC X(4) VALUE "REPL".
           05 DLI-CHKP        PIC X(4) VALUE "CHKP".
           05 DLI-XRST        PIC X(4) VALUE "XRST".
           05 DLI-PCB         PIC X(4) VALUE "PCB ".

      * DATA AREA FOR IO AREA
        01 IO-AREA            PIC X(1000) VALUE SPACE.
        01 CALL-NAME          PIC X(8) VALUE SPACE.

       01 SCHOOL-SSA-UQ.
           05 SEGMENT-NAME    PIC X(009) VALUE 'SCHOOL'.
           05 FILLER          PIC X(991) VALUE SPACE.

       01 STATE-SSA-UQ.
           05 SEGMENT-NAME    PIC X(009) VALUE 'STATE'.
           05 FILLER          PIC X(991) VALUE SPACE.

       01 CITY-SSA-QUALIFIED.
           05 FILLER          PIC X(019) VALUE 'CITY    (CITYNM   ='.
           05 CITY-NAME       PIC X(020) VALUE SPACE.
           05 FILLER          PIC X(001) VALUE ')'.
           05 FILLER          PIC X(958) VALUE SPACE.


       LINKAGE SECTION.

        01 DBPCB1.
           02 DBD-NAME1        PIC  X(8).
           02 SEG-LEVEL1       PIC  X(2).
           02 DBSTATUS1        PIC  X(2) VALUE SPACE.
           02 PROC-OPTIONS1    PIC  X(4).
           02 FILLER          PIC  S9(5) COMP.
           02 SEG-NAME-FB1     PIC  X(8).
           02 LENGTH-FB-KEY1   PIC  S9(5) COMP.
           02 NUMB-SENS-SEGS1  PIC  S9(5) COMP.
           02 KEY-FB-AREA1     PIC  X(80).

       PROCEDURE DIVISION USING DBPCB1.

           PERFORM GU-CALL-1.
           PERFORM GU-CALL-2.
           PERFORM GU-CALL-3.

           GOBACK.

        GU-CALL-1.
           MOVE "SAN ANTONIO" TO CITY-NAME.
           MOVE "GU" TO CALL-NAME.
           CALL "CEETDLI" USING DLI-GU DBPCB1 IO-AREA
                                   STATE-SSA-UQ
                                   CITY-SSA-QUALIFIED
                                   SCHOOL-SSA-UQ.
           PERFORM REPORT-RESULT.
           PERFORM GN-SCHOOL UNTIL DBSTATUS1 NOT EQUAL SPACE.

       GU-CALL-2.
           MOVE "AUSTIN" TO CITY-NAME.
           MOVE "GU" TO CALL-NAME.
           CALL "CEETDLI" USING DLI-GU DBPCB1 IO-AREA
                                   STATE-SSA-UQ
                                   CITY-SSA-QUALIFIED
                                   SCHOOL-SSA-UQ.
           PERFORM REPORT-RESULT.
           PERFORM GN-SCHOOL UNTIL DBSTATUS1 NOT EQUAL SPACE.


       GU-CALL-3.
           MOVE "HOUSTON" TO CITY-NAME.
           MOVE "GU" TO CALL-NAME.
           CALL "CEETDLI" USING DLI-GU DBPCB1 IO-AREA
                                   STATE-SSA-UQ
                                   CITY-SSA-QUALIFIED
                                   SCHOOL-SSA-UQ.
           PERFORM REPORT-RESULT.
           PERFORM GN-SCHOOL UNTIL DBSTATUS1 NOT EQUAL SPACE.

       GN-SCHOOL.
           MOVE "GN" TO CALL-NAME.
           CALL "CEETDLI" USING DLI-GN DBPCB1 IO-AREA
                                   STATE-SSA-UQ
                                   CITY-SSA-QUALIFIED
                                   SCHOOL-SSA-UQ.
           PERFORM REPORT-RESULT.

       REPORT-RESULT.
           STRING "CALL: '" DELIMITED BY SIZE
                  CALL-NAME DELIMITED BY SIZE
                  "' STATUS-CODE: '" DELIMITED BY SIZE
                  DBSTATUS1 DELIMITED BY SIZE
                  "' IO-AREA: '" DELIMITED BY SIZE
                  IO-AREA DELIMITED BY SIZE
                  "'" DELIMITED BY SIZE
             INTO OUT-REC.
           MOVE SPACES TO IO-AREA.

