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
	             DECLARE   TEST1.DEPT1    CURSOR FOR
	               SELECT    KSDS_PRIMARY_INDEX
	               FROM      TEST1.DEPT AS VSAMK
	               ORDER BY  VSAMK.KSDS_PRIMARY_INDEX ASC
	               FOR       FETCH ONLY
	           END-EXEC.
	            EXEC SQL
	             CREATE VIEW view_name AS
                 SELECT column1, column2,
                 FROM table_name
                 WHERE condition;
	           END-EXEC.
        STOP RUN.