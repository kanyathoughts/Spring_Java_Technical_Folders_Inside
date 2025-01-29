void foo() {

 int a, b, c, d, e, n; // 1, function declaration
    if (a == b) { // 2, if
      fun1();
    } else if (a == 0 // 3, if
      && b == c) { // 4, && operator
      if (c == -1) { 
      for(int n=0 ;n<=5;n++)
  		{
    		printf("Hello");
  		}// 5, if
        fun2();
      }
    } else if (a == c // 6, if
      || a == d) { // 7, || operator
      fun3();
    } else if (d == e) { 
            do
        {
            printf("*");
            j++;
        }while(j <= i);// 8, if
    } else {
      switch(n) { // 10, switch
        case 1:
          fun1();
          break;
        case 2:
          fun2();
          break;
        default:
          break;
      }
    }
    a = a > 0 ? b : c; // 11, ternary operator
  }