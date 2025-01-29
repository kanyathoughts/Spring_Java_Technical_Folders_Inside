       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN12259.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
            EXEC SQL
               INSERT INTO TABLE_A
                (SELECT :T2-TXN-ACTVTY-ID
                   , :T2-EVENT-TYPE
                   , A.TXN_FND_ID
                   , A.TXN_SRC_ID
                   , :T2-AMOUNT-TYPE
                   , A.TXN_PC
                   , :T2-XCHNG-TYPE :T2-XCHNG-TYPE-NI
                   FROM VDTRACT1 A
                   WHERE A.TXN_CONF_ID = :VDTRACT1-TXN-CONF-ID
                   AND A.PLN_ID = :VDTRACT1-PLN-ID
                   AND A.PART_ID = :VDTRACT1-PART-ID
                   AND A.TXN_SEQ_NO = :VDTRACT1-TXN-SEQ-NO
                   AND A.TXN_LN_USAGE_CD = 'D'
                   AND A.TXN_ID = 815
                   AND A.TXN_SRC_ID IN
                   (SELECT DISTINCT(TXN_SRC_ID)
                   FROM VDTRACT1 B
                   WHERE B.TXN_CONF_ID =
                   :VDTRACT1-TXN-CONF-ID
                   AND B.PLN_ID =
                   :VDTRACT1-PLN-ID
                   AND B.PART_ID =
                   :VDTRACT1-PART-ID
                   AND B.TXN_SEQ_NO =
                   :VDTRACT1-TXN-SEQ-NO
                   AND B.TXN_LN_USAGE_CD = 'D'
                   AND B.TXN_ID = 381))
           END-EXEC
           EXEC SQL
            DECLARE EMPCURS CURSOR FOR
            SELECT LNAME, FNAME, PAYRATE, HOURS
            FROM EMPLOYEE
            WHERE DEPT = :DEPT-NUM
           END-EXEC
           EXEC SQL
               SELECT MAX(COL1)
               INTO :MAX-COL1
               FROM TABLE_B
               WHERE COL2 = (SELECT MIN(COL2) FROM TABLE_C);
           END-EXEC.
           EXEC SQL
               SELECT COUNT(*)
               INTO :ROW_COUNT
               FROM TABLE_A;
           END-EXEC.
