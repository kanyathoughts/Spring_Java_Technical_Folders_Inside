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
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       0000-INITIALIZE.
           EXEC SQL
               DELETE FROM
                        TABLE1
                      WHERE
                        SUBJ_ID = :W-SUBJECT-ID
                        AND CRT_TS = :DCL.FREE-DELIVERY-IND
            END-EXEC


