int main()
{
    int start = 1, end = 10;
    int curr = start;
    print_line :
        printf("%d ", curr);
        if(curr<end)
        {
            curr++;
            goto print_line;
        }
    return 0;
}