       01  EMPLOYEE-RECORD.
           05  EMPLOYEE-ID       PIC X(5).
           05  EMPLOYEE-NAME     PIC X(30).
           05  EMPLOYEE-SALARY   PIC 9(6)V99.
           05  EMPLOYEE-DEPT     PIC X(3).

       01  OTHER-VARIABLES.
           05  ADDRESS           PIC X(50).
           05  PHONE-NUMBER      PIC X(12).
           05  EMAIL             PIC X(30).
           05  JOB-TITLE         PIC X(20).
           05  EMPLOYEE-TYPE     PIC X.
           05  HIRE-DATE         PIC 9(8).
           05  BONUS             PIC 9(5)V99.
           05  TAX-RATE          PIC 9(3)V9(2).
           05  BENEFITS-CODE     PIC X(3).
           05  EMPLOYEE-STATUS   PIC X.
           05  YEARS-WITH-COMPANY PIC 9(2).
           05  RETIREMENT-PLAN   PIC X.
           05  STOCK-OPTIONS     PIC 9(6).
           05  VACATION-DAYS     PIC 9(3).
           05  SICK-DAYS         PIC 9(3).
           05  MANAGER-APPROVAL  PIC X.
           05  HR-APPROVAL       PIC X.

       01  MANAGER-SALARY REDEFINES
           EMPLOYEE-SALARY PIC 9(6)V99.
       01  MANAGER-DEPT  REDEFINES
           EMPLOYEE-DEPT PIC X(3).
