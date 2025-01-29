#include <stdio.h>

unsigned int my_func ( char *my_field )
{
    printf("hello\n");    
    return(0);
}

unsigned int tcp_ip_timeouts  ( int  *timeouts ) {};
int tcp_ip_connect   ( char *serverid, int portnum ) {};
int tcp_ip_sending   ( char *outbuf, int length ) {};
unsigned int tcp_ip_reading   ( char *outbuf, int length, int *bytes ) {};
int tcp_ip_send_read ( char *outbuf, int outlen, char *inbuf, int inlen, int *bytes ) {};
unsigned int queue_read(char *inbuf, int inlen) {};
long tcp_ip_terminate() {};
static void  timo_trap() {};
