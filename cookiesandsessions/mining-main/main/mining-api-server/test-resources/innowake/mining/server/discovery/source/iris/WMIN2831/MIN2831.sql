WITH CTE1_HIST                                                                  
      ( column1                                                           
       ,column2                                                                                                                                                           
      )                                                                         
       AS                                                                       
      (                                                                         
       SELECT TWH.column1                                                 
             ,TWH.RBLNC_NO AS column2                                                                                                                                         
       FROM VT_ADV_RB_CTE1_HIST_ST_H TWH                                              
       INNER JOIN CTE1_HIST                                                           
          ON CTE1_HIST.column1 = TWH.column1                              
         AND CTE1_HIST.column2 = TWH.RBLNC_NO                                    
      )                                                                         
      ,                                                                         
      ADD_INFO                                                                  
      (    column1                                                                                                            
          ,SAG_STATUS_CD                                                        
          ,IMPLM_BGN_DT                                                         
      )                                                                         
       AS                                                                       
      (                                                                         
          SELECT                                                                
                 CTE1_HIST.column1                                                               
                ,STS.SAG_STATUS_CD                                              
                ,SAD.IMPLM_BGN_DT                                               
            FROM CTE1_HIST                                                            
            INNER JOIN VT_ADVC_PROF_RBLNC RBL                                   
               ON CTE1_HIST.column1           = RBL.column1                                      
            WHERE STS.SAG_STATUS_END_TS       > CURRENT TIMESTAMP               
       ) SELECT                                                                 
               CTE1_HIST.coulmn1                                                
              ,CTE1_HIST.coulmn2                                                                                                
              ,CTE1_HIST.LST_UPDTD_USER_ID                                            
              ,TS.DAYNAME                  ||' '||                              
               TS.MONTHNAME                ||' '||                              
               SUBSTR(TS.RBLDAY,1,2)       ||' '||                              
               TS.RBLTIME                  ||' '||                              
               'EST'                       ||' '||                              
               TS.RBLYEAR AS LST_UPDTD_TS                                                                                  
              ,CTE1_HIST.VALIDT_PRO_CMPL_FL                                           
         FROM CTE1_HIST                                                               
         INNER JOIN RBL                                                         
            ON CTE1_HIST.coulmn1 = RBL.coulmn1                            
         INNER JOIN TS                                                          
            ON TS.coulmn1 = CTE1_HIST.coulmn1                             
         INNER JOIN ADD_INFO                                                    
            ON ADD_INFO.coulmn1 = CTE1_HIST.coulmn1                       
         FOR READ ONLY WITH UR; 
SELECT ID INTO new_id 
    FROM FINAL TABLE (INSERT INTO company_b 
                      VALUES(default, NAME, DEPARTMENT, 
                             JOB, YEARS, SALARY, benefits, ID));

INSERT INTO temp_employee SELECT * FROM employee;
  DECLARE c1 CURSOR FOR 
    SELECT CAST(salary AS DOUBLE) FROM staff 
    ORDER BY salary;
    
SELECT COUNT (*) INTO v_numRecords FROM staff;

SELECT ID INTO new_id 
    FROM FINAL TABLE (INSERT INTO company_b 
                      VALUES(default, NAME, DEPARTMENT, 
                             JOB, YEARS, SALARY, benefits, ID));
WITH cte_film AS (
    SELECT 
        film_id, 
        title,
        (CASE 
            WHEN length < 30 
                THEN 'Short'
            WHEN length >= 30 AND length < 90 
                THEN 'Medium'
            WHEN length >=  90 
                THEN 'Long'
        END) length    
    FROM
        film
)SELECT
    film_id,
    title,
    length
FROM 
    cte_film
WHERE
    length = 'Long'
ORDER BY 
    title;
    CREATE PROCEDURE name
    EXTERNAL NAME bla LANGUAGE COBOL;