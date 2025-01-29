int main()   
{  
  int i, j, k;    
  printf("Hello World");
  goto label1;
  if(i>j) {
  printf("%d is greater",i);
  }
  else {
  printf("%d is greater",j);
  }
  label1: sum(i,j);
}  

void sum(int i, int j) {
  printf("sum of both is %d",i+j);
}