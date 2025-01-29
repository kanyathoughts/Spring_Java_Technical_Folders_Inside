#include<stdio.h>
 
int main()
{
    int ch;
 
    printf("Enter 4 to exit\n");
 
    while((ch=getchar())!=4)
    {
        switch(ch)
        {
            case 1:
                printf("Inside the case 1\n");
                break;
            case 2:
                printf("Inside the case 2\n");
                break;
            case 3:
                printf("Inside the case 3\n");
                break;
            default:
                printf("You entered %d but still inside the switch\n",ch);
        }
        printf("Enter 4 to exit\n");
    }
    printf("Out of While loop\n");
    return 0;
}