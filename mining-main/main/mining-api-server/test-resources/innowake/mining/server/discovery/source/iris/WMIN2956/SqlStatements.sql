CREATE PROCEDURE MAX_SALARY (OUT maxSalary DOUBLE)
LANGUAGE SQL 
READS SQL DATA
BEGIN
  SELECT MAX(salary) INTO maxSalary FROM staff;
  DELETE FROM staff
     WHERE WORKDEPT = 'D11';
INSERT INTO staff 
        (SELECT * FROM REMOTESYS.TESTSCHEMA.SALES WHERE SALES_DATE = CURRENT DATE - 1 DAY);
UPDATE staff
  SET DEPTNO =
        (SELECT WORKDEPT FROM EMPLOYEE
           WHERE PROJECT.RESPEMP = EMPLOYEE.EMPNO)
  WHERE RESPEMP='000030';
END @
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
    