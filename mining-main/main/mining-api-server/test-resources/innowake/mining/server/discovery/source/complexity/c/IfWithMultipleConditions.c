#include <stdio.h>
int main(void)
{
    char cData; //character variable
    
    /*Get value from the user*/
    printf("Enter any character: ");
    scanf("%c", &cData);
    
    //check range for alphabate
    if((cData >= 'a' && cData <= 'z') || (cData >= 'A' && cData <= 'Z'))
    {
        printf("\n It is an aplhabate.\n\n");
    }
    else
    {
        printf("\n It is not an aplhabate.\n\n");
    }
    return 0;
}