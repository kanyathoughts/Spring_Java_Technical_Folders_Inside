       IDENTIFICATION DIVISION.
       PROGRAM-ID.     L13057A.
       AUTHOR          INNOWAKE.
       ENVIRONMENT     DIVISION.
       INPUT-OUTPUT    SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
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

        01 STATE-SEGMENT.
           05 STATENM         PIC X(10).
           05 ST-POP          PIC X(05).
           05 CAPITOL         PIC X(05).

        01 CITY-SEGMENT.
           05 CITYNM          PIC X(20).
           05 POP             PIC X(05).
           05 ZIPCODE         PIC X(05).

        01 SCHOOL-SEGMENT.
           05 SCHOOLNM        PIC X(20).
           05 SCH-ADDR        PIC X(05).

        01 STUDENT-SEGMENT.
           05 LASTNM          PIC X(10).
           05 FIRSTNM         PIC X(20).

        01 PET-SEGMENT.
           05 PETNM           PIC X(10).
           05 SPECIES         PIC X(10).

        01 TRICK-SEGMENT.
           05 TRICKNM         PIC X(10).
           05 TRICKDESC       PIC X(20).

        01 CLASSRM-SEGMENT.
           05 ROOMNUM         PIC X(05).
           05 BLDINGNM        PIC X(10).

        01 POSTER-SEGMENT.
           05 MOTTEXT         PIC X(05).
           05 ANIMAL          PIC X(10).

        01 ARTIST-SEGMENT.
           05 ARTISTNM        PIC X(05).

        01 PARK-SEGMENT.
           05 PARKNM          PIC X(20).
           05 PARK-ADDR       PIC X(05).

        01 STATE-UNQ.
           05 SEGMENT-NAME    PIC X(005) VALUE "STATE".
           05 FILLER          PIC X(995) VALUE SPACE.

        01 CITY-UNQ.
           05 CITY-NAME       PIC X(004) VALUE "CITY".
           05 FILLER          PIC X(996) VALUE SPACE.

        01 SCHOOL-UNQ.
           05 SCHOOL-NAME     PIC X(006) VALUE "SCHOOL".
           05 FILLER          PIC X(994) VALUE SPACE.

        01 STUDENT-UNQ.
           05 STUDENT-NAME    PIC X(007) VALUE "STUDENT".
           05 FILLER          PIC X(993) VALUE SPACE.

        01 PET-UNQ.
           05 PET-NAME        PIC X(003) VALUE "PET".
           05 FILLER          PIC X(997) VALUE SPACE.

        01 TRICK-UNQ.
           05 PET-NAME        PIC X(005) VALUE "TRICK".
           05 FILLER          PIC X(995) VALUE SPACE.

        01 CLASSRM-UNQ.
           05 CLASSRM-NAME    PIC X(007) VALUE "CLASSRM".
           05 FILLER          PIC X(993) VALUE SPACE.

        01 POSTER-UNQ.
           05 POSTER-NAME     PIC X(006) VALUE "POSTER".
           05 FILLER          PIC X(994) VALUE SPACE.

        01 ARTIST-UNQ.
           05 ARTIST-NAME     PIC X(006) VALUE "ARTIST".
           05 FILLER          PIC X(994) VALUE SPACE.

        01 PARK-UNQ.
           05 PARK-NAME       PIC X(004) VALUE "PARK".
           05 FILLER          PIC X(996) VALUE SPACE.

        01 STATE-SSA-Q.
           05 FILLER          PIC X(009)  VALUE "STATE   (".
           05 FILLER          PIC X(010)  VALUE "STATENM  =".
           05 STATKEY         PIC X(010).
           05 FILLER          PIC X(001)  VALUE ")".
           05 FILLER          PIC X(973) VALUE SPACE.

        01 CITY-SSA-Q.
           05 FILLER          PIC X(009)  VALUE "CITY    (".
           05 FILLER          PIC X(010)  VALUE "CITYNM   =".
           05 CITKEY          PIC X(020).
           05 FILLER          PIC X(001)  VALUE ")".
           05 FILLER          PIC X(963) VALUE SPACE.

        01 SCHOOL-SSA-Q.
           05 FILLER          PIC X(009)  VALUE "SCHOOL  (".
           05 FILLER          PIC X(010)  VALUE "SCHOOLNM =".
           05 SCHKEY          PIC X(020).
           05 FILLER          PIC X(001)  VALUE ")".
           05 FILLER          PIC X(963) VALUE SPACE.

        01 STUDENT-SSA-Q.
           05 FILLER          PIC X(009)  VALUE "STUDENT (".
           05 FILLER          PIC X(010)  VALUE "LASTNM   =".
           05 STUKEY          PIC X(010).
           05 FILLER          PIC X(001)  VALUE ")".
           05 FILLER          PIC X(973) VALUE SPACE.

        01 PET-SSA-Q.
           05 FILLER          PIC X(009)  VALUE "PET     (".
           05 FILLER          PIC X(010)  VALUE "PETNM    =".
           05 PETKEY          PIC X(010).
           05 FILLER          PIC X(001)  VALUE ")".
           05 FILLER          PIC X(973) VALUE SPACE.

        01 CLASSRM-SSA-Q.
           05 FILLER          PIC X(009)  VALUE "CLASSRM (".
           05 FILLER          PIC X(010)  VALUE "ROOMNUM  =".
           05 CLSKEY          PIC X(005).
           05 FILLER          PIC X(001)  VALUE ")".
           05 FILLER          PIC X(978) VALUE SPACE.

        01 POSTER-SSA-Q.
           05 FILLER          PIC X(009)  VALUE "POSTER  (".
           05 FILLER          PIC X(010)  VALUE "MOTTEXT  =".
           05 POSTER-TEXT     PIC X(005).
           05 FILLER          PIC X(001)  VALUE "&".
           05 FILLER          PIC X(010)  VALUE "ANIMAL   =".
           05 POSTER-ANIMAL   PIC X(010).
           05 FILLER          PIC X(001)  VALUE ")".
           05 FILLER          PIC X(954) VALUE SPACE.

        01 SEGMENT-I-O-AREA   PIC X(1000).

        01 STATUS-MSG.
           05 FILLER          PIC X(001) VALUE X'15'.
           05 FILLER          PIC X(015) VALUE "*************  ".
           05 MSG-FUNCTION    PIC X(016).
           05 FILLER          PIC X(013) VALUE "Status Code='".
           05 MSG-STATUS      PIC X(002).
           05 FILLER          PIC X(001) VALUE "'".
           05 MSG-VALUE1      PIC X(016) VALUE SPACES.
           05 MSG-VALUE2      PIC X(016) VALUE SPACES.

        01 CITY-ID            PIC X(002).
        01 ARTIST-CNT         PIC 9(002) VALUE 0.
        01 SCHOOL-CNT         PIC 9(001) VALUE 0.
        01 PET-CNT            PIC 9(002) VALUE 0.

       LINKAGE SECTION.

        01 DBBS-PCB-MASK.
           05 IPCB-DBD-NAME          PIC X(8).
           05 IPCB-SEGMENT-LEVEL     PIC XX.
           05 IPCB-STATUS-CODE       PIC XX.
           05 IPCB-PROC-OPTIONS      PIC X(4).
           05 FILLER                 PIC S9(5) COMP.
           05 IPCB-SEGMENT-NAME      PIC X(8).
           05 IPCB-KEY-LENGTH        PIC S9(5) COMP.
           05 IPCB-NUMB-SENS-SEGS    PIC S9(5) COMP.
           05 IPCB-KEY               PIC X(80).

        01 IO-PCB.
           05 LOGICAL-TERMINAL-NAME          PIC XXXXXXXX.
           05 RESERVED1                      PIC XX.
           05 STATUS-CODE                    PIC XX.
           05 LOCAL-DATE                     PIC S9(9) COMP-5.
           05 LOCAL-TIME                     PIC S9(9) COMP-5.
           05 INPUT-MESSAGE-SEQUENCE-NUMBER  PIC S9(9) COMP-5.
           05 MESSAGE-OUTPUT-DESCRIPTOR-NAME PIC XXXXXXXX.
           05 USER-ID                        PIC XXXXXXXX.
           05 GROUP-NAME                     PIC XXXXXXXX.
           05 INTERNAL-TIME-STAMP            PIC XXXXXXXXXXXX.
           05 USER-ID-INDICATOR              PIC X.
           05 RESERVED2                      PIC XXX.

       PROCEDURE DIVISION USING IO-PCB DBBS-PCB-MASK.

        MAIN-FLOW.
      *
      * A 3-char prefix is used for a number non-sequence fields:
      *    1st char: CITY #
      *    2nd char: STATE (H for Hawaii, or T for Texas)
      *    3rd char: SCHOOL #
      *
      * HAWAII HONOLULU
      *
           MOVE "HAWAII" TO STATENM.
           MOVE "12345" TO ST-POP.
           MOVE "HNL" TO CAPITOL.

           MOVE "1H" TO CITY-ID.
           MOVE "HONOLULU" TO CITYNM.
           MOVE "12345" TO POP.
           MOVE "96815" TO ZIPCODE.

           PERFORM INSERT-1-STATE.
           PERFORM INSERT-1-CITY.
           PERFORM INSERT-CITY-ENTITIES.

      *
      * HAWAII KAILUA
      *
           MOVE "2H" TO CITY-ID.
           MOVE "KAILUA" TO CITYNM.
           MOVE "12345" TO POP.
           MOVE "96734" TO ZIPCODE.

           PERFORM INSERT-1-CITY.

      *
      * HAWAII Waipahu
      *
           MOVE "3H" TO CITY-ID.
           MOVE "Waipahu" TO CITYNM.
           MOVE "12345" TO POP.
           MOVE "96797" TO ZIPCODE.

           PERFORM INSERT-1-CITY.

      *
      * TEXAS AUSTIN
      *
           MOVE "TEXAS" TO STATENM.
           MOVE "12346" TO ST-POP.
           MOVE "AUS" TO CAPITOL.

           MOVE "3T" TO CITY-ID.
           MOVE "AUSTIN" TO CITYNM.
           MOVE "12345" TO POP.
           MOVE "78701" TO ZIPCODE.

           PERFORM INSERT-1-STATE.
           PERFORM INSERT-1-CITY.
           PERFORM INSERT-3-SCHOOL.
           PERFORM INSERT-4-SCHOOL.
           PERFORM INSERT-5-SCHOOL.

      *
      * TEXAS HOUSTON
      *
           MOVE "4T" TO CITY-ID.
           MOVE "HOUSTON" TO CITYNM.
           MOVE "12345" TO POP.
           MOVE "77060" TO ZIPCODE.

           PERFORM INSERT-1-CITY.
           PERFORM INSERT-CITY-ENTITIES.

      *
      * TEXAS SAN ANTONIO
      *
           MOVE "5T" TO CITY-ID.
           MOVE "SAN ANTONIO" TO CITYNM.
           MOVE "12345" TO POP.
           MOVE "78516" TO ZIPCODE.

           PERFORM INSERT-1-CITY.
           PERFORM INSERT-CITY-ENTITIES.

           GOBACK.

        INSERT-CITY-ENTITIES.
           PERFORM INSERT-1-SCHOOL.
           PERFORM INSERT-1-STUDENT.
           PERFORM INSERT-2-STUDENT.
           PERFORM INSERT-2-SCHOOL.
           PERFORM INSERT-1-STUDENT.


        INSERT-1-STATE.
           MOVE SPACES TO SEGMENT-I-O-AREA.
           MOVE STATE-SEGMENT TO SEGMENT-I-O-AREA.
           CALL "CBLTDLI" USING
             DLI-ISRT
             DBBS-PCB-MASK
             SEGMENT-I-O-AREA
             STATE-UNQ.

           MOVE "ISRT-1-STATE"    TO MSG-FUNCTION.
           MOVE STATENM             TO MSG-VALUE1.
           MOVE CAPITOL             TO MSG-VALUE2.
           PERFORM REPORT-RESULT.

       INSERT-1-CITY.
           MOVE STATENM TO STATKEY OF STATE-SSA-Q.

           MOVE SPACES TO SEGMENT-I-O-AREA.
           MOVE CITY-SEGMENT TO SEGMENT-I-O-AREA.
           CALL "CBLTDLI" USING
             DLI-ISRT
             DBBS-PCB-MASK
             SEGMENT-I-O-AREA
             STATE-SSA-Q
             CITY-UNQ.

           MOVE "ISRT-1-CITY"       TO MSG-FUNCTION.
           MOVE CITYNM              TO MSG-VALUE1.
           MOVE ZIPCODE             TO MSG-VALUE2.
           PERFORM REPORT-RESULT.

       INSERT-1-SCHOOL.
           COMPUTE SCHOOL-CNT = SCHOOL-CNT + 1.
           MOVE "KAMEHAMEHA" TO SCHOOLNM.
           MOVE SPACES TO SCH-ADDR.
           STRING CITY-ID    DELIMITED BY SIZE
                  SCHOOL-CNT DELIMITED BY SIZE
                  "-1"      DELIMITED BY SIZE
             INTO SCH-ADDR.

           MOVE STATENM TO STATKEY OF STATE-SSA-Q.
           MOVE CITYNM TO CITKEY OF CITY-SSA-Q.

           MOVE SPACES TO SEGMENT-I-O-AREA.
           MOVE SCHOOL-SEGMENT TO SEGMENT-I-O-AREA.
           CALL "CBLTDLI" USING
             DLI-ISRT
             DBBS-PCB-MASK
             SEGMENT-I-O-AREA
             STATE-SSA-Q
             CITY-SSA-Q
             SCHOOL-UNQ.

           MOVE "ISRT-1-SCHOOL"   TO MSG-FUNCTION.
           MOVE SCHOOLNM            TO MSG-VALUE1.
           MOVE SCH-ADDR            TO MSG-VALUE2.
           PERFORM REPORT-RESULT.

       INSERT-2-SCHOOL.
           COMPUTE SCHOOL-CNT = SCHOOL-CNT + 1.
           MOVE "KAMEHAMEHA" TO SCHOOLNM.
           MOVE SPACES TO SCH-ADDR.
           STRING CITY-ID    DELIMITED BY SIZE
                  SCHOOL-CNT DELIMITED BY SIZE
                  "-2"       DELIMITED BY SIZE
             INTO SCH-ADDR.

           MOVE STATENM TO STATKEY OF STATE-SSA-Q.
           MOVE CITYNM TO CITKEY OF CITY-SSA-Q.

           MOVE SPACES TO SEGMENT-I-O-AREA.
           MOVE SCHOOL-SEGMENT TO SEGMENT-I-O-AREA.
           CALL "CBLTDLI" USING
             DLI-ISRT
             DBBS-PCB-MASK
             SEGMENT-I-O-AREA
             STATE-SSA-Q
             CITY-SSA-Q
             SCHOOL-UNQ.

           MOVE "ISRT-2-SCHOOL"   TO MSG-FUNCTION.
           MOVE SCHOOLNM            TO MSG-VALUE1.
           MOVE SCH-ADDR            TO MSG-VALUE2.
           PERFORM REPORT-RESULT.

       INSERT-3-SCHOOL.
           COMPUTE SCHOOL-CNT = SCHOOL-CNT + 1.
           MOVE "BRIGHT HORIZONS" TO SCHOOLNM.
           MOVE SPACES TO SCH-ADDR.
           STRING CITY-ID    DELIMITED BY SIZE
                  SCHOOL-CNT DELIMITED BY SIZE
                  "-3"       DELIMITED BY SIZE
             INTO SCH-ADDR.

           MOVE STATENM TO STATKEY OF STATE-SSA-Q.
           MOVE CITYNM TO CITKEY OF CITY-SSA-Q.

           MOVE SPACES TO SEGMENT-I-O-AREA.
           MOVE SCHOOL-SEGMENT TO SEGMENT-I-O-AREA.
           CALL "CBLTDLI" USING
             DLI-ISRT
             DBBS-PCB-MASK
             SEGMENT-I-O-AREA
             STATE-SSA-Q
             CITY-SSA-Q
             SCHOOL-UNQ.

           MOVE "ISRT-3-SCHOOL"   TO MSG-FUNCTION.
           MOVE SCHOOLNM            TO MSG-VALUE1.
           MOVE SCH-ADDR            TO MSG-VALUE2.
           PERFORM REPORT-RESULT.

       INSERT-4-SCHOOL.
           COMPUTE SCHOOL-CNT = SCHOOL-CNT + 1.
           MOVE "BRIGHT HORIZONS" TO SCHOOLNM.
           MOVE SPACES TO SCH-ADDR.
           STRING CITY-ID    DELIMITED BY SIZE
                  SCHOOL-CNT DELIMITED BY SIZE
                  "-4"       DELIMITED BY SIZE
             INTO SCH-ADDR.

           MOVE STATENM TO STATKEY OF STATE-SSA-Q.
           MOVE CITYNM TO CITKEY OF CITY-SSA-Q.

           MOVE SPACES TO SEGMENT-I-O-AREA.
           MOVE SCHOOL-SEGMENT TO SEGMENT-I-O-AREA.
           CALL "CBLTDLI" USING
             DLI-ISRT
             DBBS-PCB-MASK
             SEGMENT-I-O-AREA
             STATE-SSA-Q
             CITY-SSA-Q
             SCHOOL-UNQ.

           MOVE "ISRT-4-SCHOOL"   TO MSG-FUNCTION.
           MOVE SCHOOLNM            TO MSG-VALUE1.
           MOVE SCH-ADDR            TO MSG-VALUE2.
           PERFORM REPORT-RESULT.

       INSERT-5-SCHOOL.
           COMPUTE SCHOOL-CNT = SCHOOL-CNT + 1.
           MOVE "BRIGHT HORIZONS" TO SCHOOLNM.
           MOVE SPACES TO SCH-ADDR.
           STRING CITY-ID    DELIMITED BY SIZE
                  SCHOOL-CNT DELIMITED BY SIZE
                  "-5"       DELIMITED BY SIZE
             INTO SCH-ADDR.

           MOVE STATENM TO STATKEY OF STATE-SSA-Q.
           MOVE CITYNM TO CITKEY OF CITY-SSA-Q.

           MOVE SPACES TO SEGMENT-I-O-AREA.
           MOVE SCHOOL-SEGMENT TO SEGMENT-I-O-AREA.
           CALL "CBLTDLI" USING
             DLI-ISRT
             DBBS-PCB-MASK
             SEGMENT-I-O-AREA
             STATE-SSA-Q
             CITY-SSA-Q
             SCHOOL-UNQ.

           MOVE "ISRT-5-SCHOOL"   TO MSG-FUNCTION.
           MOVE SCHOOLNM            TO MSG-VALUE1.
           MOVE SCH-ADDR            TO MSG-VALUE2.
           PERFORM REPORT-RESULT.

       INSERT-1-STUDENT.
           MOVE "PEARSON " TO LASTNM.
           MOVE SPACES TO FIRSTNM.
           STRING CITY-ID    DELIMITED BY SIZE
                  SCHOOL-CNT DELIMITED BY SIZE
                  "-SONOMA"  DELIMITED BY SIZE
             INTO FIRSTNM.

           MOVE STATENM TO STATKEY OF STATE-SSA-Q.
           MOVE CITYNM TO CITKEY OF CITY-SSA-Q.
           MOVE "KAMEHAMEHA" TO SCHKEY OF SCHOOL-SSA-Q.

           MOVE SPACES TO SEGMENT-I-O-AREA.
           MOVE STUDENT-SEGMENT TO SEGMENT-I-O-AREA.
           CALL "CBLTDLI" USING
             DLI-ISRT
             DBBS-PCB-MASK
             SEGMENT-I-O-AREA
             STATE-SSA-Q
             CITY-SSA-Q
             SCHOOL-SSA-Q
             STUDENT-UNQ.

           MOVE "ISRT-1-STUDENT"  TO MSG-FUNCTION.
           MOVE LASTNM              TO MSG-VALUE1.
           MOVE FIRSTNM             TO MSG-VALUE2.
           PERFORM REPORT-RESULT.

       INSERT-2-STUDENT.
           MOVE "PEARSON" TO LASTNM.
           MOVE SPACES TO FIRSTNM.
           STRING CITY-ID    DELIMITED BY SIZE
                  SCHOOL-CNT DELIMITED BY SIZE
                  "-KANE"    DELIMITED BY SIZE
             INTO FIRSTNM.

           MOVE STATENM TO STATKEY OF STATE-SSA-Q.
           MOVE CITYNM TO CITKEY OF CITY-SSA-Q.
           MOVE "KAMEHAMEHA " TO SCHKEY OF SCHOOL-SSA-Q.

           MOVE SPACES TO SEGMENT-I-O-AREA.
           MOVE STUDENT-SEGMENT TO SEGMENT-I-O-AREA.
           CALL "CBLTDLI" USING
             DLI-ISRT
             DBBS-PCB-MASK
             SEGMENT-I-O-AREA
             STATE-SSA-Q
             CITY-SSA-Q
             SCHOOL-SSA-Q
             STUDENT-UNQ.

           MOVE "ISRT-2-STUDENT"  TO MSG-FUNCTION.
           MOVE LASTNM              TO MSG-VALUE1.
           MOVE FIRSTNM             TO MSG-VALUE2.
           PERFORM REPORT-RESULT.


       REPORT-RESULT.
           DISPLAY MSG-FUNCTION " '" IPCB-STATUS-CODE "'"
                   IPCB-SEGMENT-LEVEL " "
                   IPCB-SEGMENT-NAME " "
                   IPCB-KEY(1:IPCB-KEY-LENGTH)
           MOVE SPACES TO MSG-VALUE1.
           MOVE SPACES TO MSG-VALUE2.
