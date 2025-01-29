#include <stdio.h> 
 
int main()  {  
	int i = 0, j=50, k=0;    
	 
    $char   PROC[80] ="STORE.PROC1", PROC2[80] ="STORE.PROC2";       
    PROC[80] ="STORE.PROC3";
	printf(" C statement before Exec Sql statements");
	EXEC SQL INSERT INTO UTT_ADT_HEAD (CRT_TS) SELECT CURRENT TIMESTAMP FROM UTT_OA  OAGT ,UTT_MKTR MRKR WHERE OAGT.CNTR_NUM = :WS-CHLD-CON AND OAGT.MKTR_FOCAL_SUBJ_ID = MRKR.SUBJ_ID;
  	EXEC SQL PREPARE mystmt FROM :prep_stmt; 
    EXEC SQL OPEN mycursor; 
    EXEC SQL WHENEVER NOT FOUND GOTO out;      
    EXEC SQL DECLARE DSN8C10.EMP TABLE 
            EMPNO     CHAR(6)     NOT NULL, 
            FIRSTNME  VARCHAR(12) NOT NULL,
            MIDINIT   CHAR(1)     NOT NULL,
            LASTNAME  VARCHAR(15) NOT NULL,
            WORKDEPT  CHAR(3)             ,
            PHONENO   CHAR(4)             ,
            HIREDATE  DATE                ,
            JOB       CHAR(8)             ,
            EDLEVEL   SMALLINT            ,
            SEX       CHAR(1)             ,
            BIRTHDATE DATE                ,
            SALARY    DECIMAL(9,2)        ,
            BONUS     DECIMAL(9,2)        ,
            COMM      DECIMAL(9,2)        );
     EXEC SQL
        select
        ALPHA_SHORT
        INTO
        :ALPHA-SHORT
        FROM TPL.IW_SQL_TEST;
     printf(" C statement between Exec Sql statements");
     EXEC SQL EXECUTE
     BEGIN
       INSERT INTO TPL.IW_SQL_TEST (ALPHA_SHORT)
         VALUES (:ALPHA-SHORT);
     EXEC SQL
        CALL TPL.ME10077 (:ALPHASHORT, 'HELLO OO', -1, 5, 'HELLO OO');
     EXEC SQL
     CALL :PROC (:VAR1, :VAR2, 'A CONSTANT', 123.45);
     EXEC SQL
     WITH DATA_1 AS (SELECT * FROM
     TPL.IW_SQL_TEST)
     SELECT ALPHA_SHORT FROM DATA_1 INTO :ALPHA;
   printf(" C statement after Exec Sql statements");
   $PREPARE STMT1 FROM STATEMENT;
   EXEC SQL
        SELECT
        ALPHA_SHORT
        INTO 
        $ALPHA-SHORT 
        FROM TPL.IW_SQL_TEST;
   EXEC SQL SELECT * FROM TABLE1;
   EXEC SQL BEGIN DECLARE SECTION;
    $int    hostint;
    $double hostdbl;
    $char   PROC[80] ="sdfg1";  
   EXEC SQL END DECLARE SECTION; 
	return 0;	
}