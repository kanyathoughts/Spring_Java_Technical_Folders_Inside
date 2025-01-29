SELECT ID, OLD_SALARY, SALARY into id, old_salary, salary
    FROM FINAL TABLE (UPDATE company_b INCLUDE (OLD_SALARY DECIMAL(7,2)) 
                      SET OLD_SALARY = SALARY, 
                        SALARY = SALARY * 1.05 
                      WHERE ID = new_id);
DELETE FROM (SELECT * FROM company_b 
               ORDER BY SALARY DESC FETCH FIRST ROW ONLY);
               
SELECT ID INTO new_id 
    FROM FINAL TABLE (INSERT INTO company_b 
                      VALUES(default, NAME, DEPARTMENT, 
                             JOB, YEARS, SALARY, benefits, ID));

INSERT INTO temp_employee SELECT * FROM employee;

EXEC SQL UPDATE (SELECT MAX(YEARS) OVER() AS max_years,
                          YEARS,
                          SALARY
                   FROM company_b)
                  SET SALARY = SALARY + 10000
                  WHERE max_years = YEARS;
                  
EXEC SQL DELETE FROM (SELECT * FROM company_b ORDER BY SALARY DESC FETCH FIRST ROW ONLY);

  
SELECT ID, OLD_SALARY, SALARY into :id, :old_salary, :salary
             FROM FINAL TABLE (UPDATE company_b INCLUDE (OLD_SALARY DECIMAL(7,2))
                               SET OLD_SALARY = SALARY,
                                   SALARY = SALARY * 1.05
                               WHERE ID = :new_id);
    EMB_SQL_CHECK("Select from Update");