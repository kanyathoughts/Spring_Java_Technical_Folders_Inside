SELECT name, job, CAST salary AS DOUBLE 
    FROM(SELECT name, job, CAST salary AS DOUBLE FROM Table1);
    
SELECT COUNT * INTO v_numRecords FROM staff;
    
SET stmt = INSERT INTO ||new_name || ' ' ||
     SELECT empno, firstnme, midinit, lastname, salary ||
     'FROM employee '||
     'WHERE workdept' =? ;
SELECT ID INTO new_id 
    FROM FINAL TABLE (INSERT INTO company_b 
                      VALUES(default, NAME, DEPARTMENT, 
                             JOB, YEARS, SALARY, benefits, ID));
    
