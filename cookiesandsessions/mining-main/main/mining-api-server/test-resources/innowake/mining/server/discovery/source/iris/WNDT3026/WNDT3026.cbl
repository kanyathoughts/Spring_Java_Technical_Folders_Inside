       IDENTIFICATION DIVISION.
       PROGRAM-ID. WNDT3026.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       BEGIN.
          EXEC SQL
            DECLARE C_NAME CURSOR WITH RETURN FOR  SELECT ID1, VALUE1
            FROM TABLE_A
          END-EXEC.
          EXEC SQL
            DECLARE C_NAME CURSOR WITHOUT RETURN FOR 
            SELECT ID1, VALUE1 FROM TABLE_A
          END-EXEC.
          EXEC SQL
            DECLARE C_MASTER CURSOR WITH HOLD FOR SELECT CUSTOMER_NUM
            FROM CUSTOMER WHERE CITY = 'PITTSBURGH'
          END-EXEC.
          EXEC SQL
            DECLARE C_MASTER CURSOR WITHOUT HOLD FOR SELECT
            CUSTOMER_NUM FROM CUSTOMER WHERE CITY = 'PITTSBURGH'
          END-EXEC.
       GOBACK.
