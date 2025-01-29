#include<stdio.h>

 //functions declaration
 int add(int n1, int n2);
 int subtract(int n1, int n2);
 int multiply(int n1, int n2);
 int divide(int n1, int n2);

 //function to add two integer numbers
 int add(int n1, int n2)
 {
   int result;
   result = n1 + n2;
   return result;
 }

 //function to subtract two integer numbers
 int subtract(int n1, int n2)
 {
   int result;
   result = n1 - n2;
   return result;
 }

 //function to multiply two integer numbers
 int multiply(int n1, int n2)
 {
   int result;
   result = n1 * n2;
   return result;
 }

 //function to divide two integer numbers
 int divide(int n1, int n2)
 {
   int result;
   result = n1 / n2;
   return result;
 }
 
 int main () {
 	printf("Hello World");
 }