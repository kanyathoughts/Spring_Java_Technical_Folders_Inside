******** A
       IDENTIFICATION DIVISION.
       PROGRAM-ID. A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DCL.
           10 COORDINATOR-TYPE     PIC X(1).
           10 ANNUAL-DELIV-FEE     PIC S9(4) USAGE COMP.
           10 FREE-DELIVERY-IND    PIC X(1).
        01  W-SUBJECT-ID           PIC S9(9) COMP.
        01  W-USER-ID              PIC X(10).
        01  W-TRMID                PIC 9(5).
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       0000-INITIALIZE.
           EXEC SQL
               INSERT INTO TABLE1
                   (TERMINAL_ID,
                    USER_ID,
                    SUBJECT_ID
                    )
                     VALUES(:W-TRMID,
                            :W-USER-ID,
                            :W-SUBJECT-ID
                            )
           END-EXEC.
