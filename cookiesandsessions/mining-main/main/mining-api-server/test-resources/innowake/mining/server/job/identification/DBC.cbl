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
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       0000-INITIALIZE.
           EXEC SQL
               UPDATE TABLE1
                     SET
                      UPDATED_BY = :W-USER-ID,
                      COORDINATOR_TYPE = :DCL.COORDINATOR-TYPE,
                      ANNUAL_DELIV_FEE = :DCL.ANNUAL-DELIV-FEE,
                      FREE_DELIVERY_IND = :DCL.FREE-DELIVERY-IND
                  WHERE SUBJECT_ID = :W-SUBJECT-ID
            END-EXEC

