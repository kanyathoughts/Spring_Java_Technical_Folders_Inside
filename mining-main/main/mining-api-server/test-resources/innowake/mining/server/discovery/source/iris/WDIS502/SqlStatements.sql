
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


CREATE PROCEDURE OUT_MEDIAN (OUT medianSalary DOUBLE, OUT maxSalary DOUBLE)
DYNAMIC RESULT SETS 0
LANGUAGE SQL 
MODIFIES SQL DATA
BEGIN 

  DECLARE v_numRecords INT DEFAULT 1;
  DECLARE v_counter INT DEFAULT 0;
  DECLARE v_mod INT DEFAULT 0;
  DECLARE v_salary1 DOUBLE DEFAULT 0;
  DECLARE v_salary2 DOUBLE DEFAULT 0;
 
  DECLARE c1 CURSOR FOR 
    SELECT CAST(salary AS DOUBLE) FROM staff 
    ORDER BY salary;

  SELECT COUNT(*) INTO v_numRecords FROM staff;

  SET v_mod = MOD(v_numRecords, 2);
  OPEN c1;  

  CASE v_mod
    WHEN 0 THEN
      WHILE v_counter < (v_numRecords / 2 + 1) DO
        SET v_salary1 = v_salary2;
        FETCH c1 INTO v_salary2;
        SET v_counter = v_counter + 1;
      END WHILE;
      SET medianSalary = (v_salary1 + v_salary2)/2;
    WHEN 1 THEN
      WHILE v_counter < (v_numRecords / 2 + 1) DO
        FETCH c1 INTO medianSalary;
        SET v_counter = v_counter + 1;
      END WHILE;
  END CASE;
  
  CLOSE c1;

  CALL MAX_SALARY(maxSalary);

END @


CREATE PROCEDURE OUT_AVERAGE (OUT averageSalary DOUBLE, OUT medianSalary DOUBLE, OUT maxSalary DOUBLE)
DYNAMIC RESULT SETS 2
LANGUAGE SQL 
MODIFIES SQL DATA
BEGIN 

  DECLARE r1 CURSOR WITH RETURN TO CLIENT FOR
    SELECT name, job, CAST(salary AS DOUBLE)
    FROM staff
    WHERE salary > averageSalary
    ORDER BY name ASC;
    
  DECLARE r2 CURSOR WITH RETURN TO CLIENT FOR
    SELECT name, job, CAST(salary AS DOUBLE)
    FROM staff
    WHERE salary < averageSalary
    ORDER BY name ASC; 

  SELECT AVG(salary) INTO averageSalary FROM staff;
  CALL OUT_MEDIAN(medianSalary, maxSalary); 

  -- open the cursors to return result sets
  OPEN r1;

  OPEN r2;

END @

CREATE PROCEDURE update_salary_if
    (IN employee_number CHAR(6), IN rating SMALLINT)
    LANGUAGE SQL
    BEGIN
      DECLARE SQLSTATE CHAR(5);
      DECLARE not_found CONDITION FOR SQLSTATE '02000';
      DECLARE EXIT HANDLER FOR not_found
         SIGNAL SQLSTATE '20000' SET MESSAGE_TEXT = 'Employee not found';

      IF (rating = 1)
        THEN UPDATE employee
          SET salary = salary * 1.10, bonus = 1000
          WHERE empno = employee_number;
      ELSEIF (rating = 2)
        THEN UPDATE employee
          SET salary = salary * 1.05, bonus = 500
          WHERE empno = employee_number;
      ELSE UPDATE employee
          SET salary = salary * 1.03, bonus = 0
          WHERE empno = employee_number;
      END IF;
    END @