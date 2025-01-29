-- Create procedure to show case the stack functionalities.
CREATE PROCEDURE use_stack(INOUT  s int_stack,
                           OUT val1 INTEGER,
                           OUT val2 INTEGER)
BEGIN
  CALL push(s, 100);
  CALL push(s, 200);
  CALL push(s, 300);
  CALL push(s, 400);

  CALL pop(s, val1);
  CALL top(s, val2);

  CALL stack_2_resultset(s);
END @