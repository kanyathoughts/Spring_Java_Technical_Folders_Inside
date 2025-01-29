       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOPRGM3.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  REC.       *> some comment
                      *> another comment
       PROCEDURE DIVISION.
              EXEC CICS                   SEND
                MAP     ('MGOMAP3')
                FROM    (MAP-OUTPUT-AREA)
                CURSOR
                DATAONLY
              END-EXEC.
            STOP RUN.
