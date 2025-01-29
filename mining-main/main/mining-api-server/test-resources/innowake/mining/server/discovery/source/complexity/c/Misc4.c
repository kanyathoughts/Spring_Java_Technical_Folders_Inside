void insertion_procedure (int a[], int p [], int N)
{
    int i,j,k;
    for (i=0; i<=N; i++) p[i] = i;
    for (i=2; i<=N; i++)
    {
        k = p[i];
        j = 1;
        while (a[p[j-1]] > a[k]) {p[j] = p[j-1]; j--}
        p[j] = k;
        while (n <= 3)
  do{
    // Print the message based on the value of n
    if( n == 1 && n2 <4)
      printf( "Good Morning\n");
    else if( n == 2)
      printf( "Good Afternoon\n");
    else
      printf( "Good Evening\n");

    //Increment the variable
    n++;
  }while(n <5);
    }
}