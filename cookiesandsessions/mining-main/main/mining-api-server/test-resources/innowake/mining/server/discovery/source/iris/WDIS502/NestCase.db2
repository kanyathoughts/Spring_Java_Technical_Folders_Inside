CREATE PROCEDURE bump_salary (IN deptnumber SMALLINT) 
LANGUAGE SQL 
BEGIN 
   DECLARE SQLSTATE CHAR(5);
   DECLARE v_salary DOUBLE;
   DECLARE v_id SMALLINT;
   DECLARE v_years SMALLINT;
   DECLARE at_end INT DEFAULT 0;
   DECLARE not_found CONDITION FOR SQLSTATE '02000';

   DECLARE C1 CURSOR FOR
     SELECT id, CAST(salary AS DOUBLE), years 
     FROM staff 
     WHERE dept = deptnumber;
   DECLARE CONTINUE HANDLER FOR not_found 
     SET at_end = 1;

   OPEN C1;
   FETCH C1 INTO v_id, v_salary, v_years;
   WHILE at_end = 0 DO
     CASE 
       WHEN (v_salary < 15000 * v_years)
         THEN CASE
           WHEN (15500*v_years > 99000)
             THEN UPDATE staff 
               SET salary = 99000 
               WHERE id = v_id;
           ELSE UPDATE staff
               SET salary = 15500* v_years
               WHERE id = v_id;
         END CASE;
       WHEN (v_salary < 30000 * v_years)
         THEN CASE 
           WHEN (v_salary < 20000 * v_years)
             THEN CASE
               WHEN (20000*v_years > 99000)
                 THEN UPDATE staff
                   SET salary = 99000
                   WHERE id = v_id;
               ELSE UPDATE staff 
                 SET salary = 20000 * v_years 
                 WHERE id = v_id;
              END CASE;
           ELSE UPDATE staff 
             SET salary = v_salary * 1.10 
             WHERE id = v_id;
         END CASE;
       ELSE UPDATE staff 
         SET job = 'PREZ' 
         WHERE id = v_id;
     END CASE;
     FETCH C1 INTO v_id, v_salary, v_years;
   END WHILE;
   CLOSE C1;
END @