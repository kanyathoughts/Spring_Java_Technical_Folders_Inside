      ****************************************************************
      *                                                              *
      *    THIS IS PROBABLY NOT WORKING COBOL                        *
      *    THIS WAS WRITTEN ONLY AS A TEST FOR COPYBOOKS THAT        *
      *    INCLUDE SQL CODE TO SEE IF DISCOVERY CAPTURES THIS        *
      *    CORRECTLY.                                                *
      *                                                              *
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOPRGM1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  REC.

       PROCEDURE DIVISION.
               EXEC SQL
                   DECLARE TEST1.DEPT TABLE
                    (DEPTNO    CHAR(3)           NOT NULL,
                     DEPTNAME  VARCHAR(36)       NOT NULL,
                     MGRNO     CHAR(6)                   ,
                     ADMRDEPT  CHAR(3)           NOT NULL,
                     LOCATION  CHAR(16)                  )
               END-EXEC.
        STOP RUN.