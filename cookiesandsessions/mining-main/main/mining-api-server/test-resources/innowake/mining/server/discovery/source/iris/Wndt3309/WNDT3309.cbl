       IDENTIFICATION DIVISION.
       PROGRAM-ID. WNDT3309.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
             EXEC SQL
                 INSERT INTO TABLE1
                   SELECT  * FROM  TABLE2
                    WHERE FUND.VISTA_FND_ID <>  ''
                   UNION
                   SELECT * FROM  TABLE3
                    WHERE ABC=''
                    ORDER BY 9
             END-EXEC.
       2100-EXIT.
