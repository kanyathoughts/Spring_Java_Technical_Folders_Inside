// Header file for input output functions

EXEC SQL BEGIN DECLARE SECTION;
EXEC SQL DECLARE CURSORNAME CURSOR WITH RETURN FOR
    select * from cars;
EXEC SQL END DECLARE SECTION;// main function -
// where the execution of program begins
int main()
{
    int marks;  
    EXEC SQL SELECT MARKS FROM STUDENT WHERE STUDENT_ID=1; //select marks from student where student_id=1; //SELECT MARKS FROM STUDENT WHERE STUDENT_ID=1;
    printf("The marks of the student is : %d", marks);
  
    //EXEC SQL select scores from student1 where student_id=2; //SELECT SCORES FROM STUDENT WHERE STUDENT_ID=2;    return 0;
} 