#include <stdio.h>                

void functionC(double);             

void functionC(double c_temp) {     
   double f_temp = (c_temp * CONV + OFFSET);
   printf("%5.2f Celsius is %5.2f Fahrenheit\n",c_temp, f_temp);
}