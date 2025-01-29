  void foo() { 
  
   int a, b, c, d, n;
    if (a == 1) { 
     for (int i=1; i<6; i++)
  {
    // Iterate the loop for 8 times
    for (int j=1; j<9; j++)
    {
      // Print the value of i and j when both are equal
      if (i != j && i < j)
         printf("%d, %d\n",i ,j);
    }
  }
    } else if (a == b 
      && a == c) { 
      if (c == 2) { 
        fun2();
      }
    } else if (a == d) { 
		printf("Shazam");
    } else {
    while (n <= 3)
  {
    // Print the message based on the value of n
    if( n == 1 && n2 <4)
      printf( "Good Morning\n");
    else if( n == 2)
      printf( "Good Afternoon\n");
    else
      printf( "Good Evening\n");

    //Increment the variable
    n++;
      switch(n) {
        case 1: // 8, case
          fun1();
          break;
        case 2: // 9, case
          fun2();
          break;
        case 3: // 10, case
          fun3();
          break;
        default:
          break;
      }
      }
    }
    d = a < 0 ? -1 : 1; 
  }
}