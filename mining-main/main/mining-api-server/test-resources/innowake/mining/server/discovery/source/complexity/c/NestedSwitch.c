void foo( int a , int b) {
switch(a) {

   case 'A': 
      printf("This A is part of outer switch" );
		
      switch(b) {
         case 'A':
            printf("This A is part of inner switch" );
            break;
         case 'B': /* case code */
      }
	  
      break;
   case 'B': /* case code */
}
}