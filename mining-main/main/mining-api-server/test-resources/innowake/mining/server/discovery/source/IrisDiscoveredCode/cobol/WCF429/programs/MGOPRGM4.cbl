       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOPRGM3.
      * COMMENT SECTION
      * COMMENT SECTION
      * COMMENT SECTION
      * COMMENT SECTION
      * COMMENT SECTION
      * COMMENT SECTION
      * COMMENT SECTION
      * COMMENT SECTION
      * COMMENT SECTION
      * COMMENT SECTION
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  REC.
       COPY MGOCOPY3.

       PROCEDURE DIVISION.
              EXEC CICS                   SEND
                MAP     ('MGOMAP3')
                FROM    (MAP-OUTPUT-AREA)
                CURSOR
                DATAONLY
              END-EXEC.
            STOP RUN.
