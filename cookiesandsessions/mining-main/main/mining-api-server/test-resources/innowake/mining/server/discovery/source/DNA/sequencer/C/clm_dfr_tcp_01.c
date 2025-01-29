#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <descrip.h>
#include <starlet.h>
#include <iodef.h>
#include <efndef.h>
#include <lib$routines.h>
#include <ssdef.h>
#include <ctype.h>
#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#include "multinet_root:[multinet.include.sys]ioctl.h"
#include "multinet_root:[multinet.include.netinet]in.h"
#include "multinet_root:[multinet.include]netdb.h"
#include "multinet_root:[multinet.include]errno.h"


unsigned int tcp_ip_timeouts  ( int  *timeouts );
unsigned int tcp_ip_connect   ( char *serverid, int portnum );
unsigned int tcp_ip_sending   ( char *outbuf, int length );
unsigned int tcp_ip_reading   ( char *outbuf, int length, int *bytes );
unsigned int tcp_ip_send_read ( char *outbuf, int outlen,
                                char *inbuf, int inlen, int *bytes );
unsigned int queue_read(char *inbuf, int inlen);
unsigned int tcp_ip_terminate();
static void  timo_trap();

struct iosb_struct
{
    short unsigned  status;
    short unsigned  byte_count;
    long            dev_data;
};

__int64 sock_efn;
int func, s, n, e, timer_id;
long efn_poll;
struct sockaddr_in sin;
struct hostent *hp;
struct servent *sp;
static struct iosb_struct iosb;
int timo[2] = {0,0};

unsigned int tcp_ip_connect ( char *serverid, int portnum )
{
	/***
	 ***  Call "Tcp_Ip_Connect" Using
	 ***	By Reference "DEVMC400.HealthNet.Com_"  ( _=C Low-Value...! )
	 ***	By Value 3005
	 ***	Giving Return_Status.
	 ***/

        /* Poll the logical which specifies the timeout value to set on
           socket */
        static char eqv[255+1];
	static struct dsc$descriptor_s eqv_d={0,DSC$K_DTYPE_T,DSC$K_CLASS_S,eqv};

        static const $DESCRIPTOR (log_d, "DOFR_SERVER_TIMEOUT");
	static const $DESCRIPTOR (tbl_d, "LNM$FILE_DEV");
	eqv_d.dsc$w_length = sizeof (eqv) - 1;

	/* set the timer with the timeout value */
	/* default to ten seconds if logical is not defined */
        if  (lib$get_logical(&log_d,&eqv_d,&eqv_d.dsc$w_length,
            &tbl_d,0,0,0,0) == SS$_NOLOGNAM)
        {   timo[0] = -10000000*10;          }
        else
        {   timo[0] = -10000000 *atoi(eqv); }
        timo[1] = -1;

	/* Allocate a local event flag for the new socket, */
        /* if we don't already have one */
        if (!sock_efn) {e = lib$get_ef(&sock_efn);}

	/***  Get the IP address for the desired host. ***/
        hp = gethostbyname(serverid);
	if (hp == NULL) {
	        /***  %SYSTEM-F-NOSUCHNODE, remote node is unknown  ***/
/***		fprintf(stderr, "gethostbyname: ServerId UnKnown\n");	***/
		return (0x0000028C);
	}

	/*** Create an IP-family socket to make the connection ***/
	s = socket(hp->h_addrtype, SOCK_STREAM, 0);
	if  (s < 0) {return (vmserrno);}

	/*** Create a "sockaddr_in" structure with remote IP address
             to connect to, along with the remote TCP port number.   ***/

	sin.sin_family = hp->h_addrtype;
	memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);
	sin.sin_port = htons(portnum);

	/*** Connect to that address... ***/
	if (connect(s, &sin, sizeof (sin)) < 0) {
		return (vmserrno);
	}
/*      set socket to non-blocking */
/*      func = IO$_READVBLK|IO$M_TIMED; */
	func = IO$_READVBLK;
        e = socket_ioctl (s, FIONBIO, (int *)1);

	return 1;
}

unsigned int tcp_ip_sending ( char *outbuf, int outlen )
{
	/***
	 ***  Call "Tcp_Ip_Sending" Using
	 ***	By Reference Sending_Data
	 ***	By Value 1024              ( You MUST specify length...! )
	 ***	Giving Return_Status.
	 ***/

	e = socket_write(s, outbuf, outlen);
	if (e < 0) {return (vmserrno);}
	return 1;
}

unsigned int tcp_ip_reading ( char *inbuf, int inlen, int *bytes )
{
	/***
	 ***  Call "Tcp_Ip_Reading" Using
	 ***	By Reference Receive_Buffer
	 ***	By Value 1024              ( MUST specify MAX length...! )
	 ***	By Reference Bytes_Read    ( How much did we acually get )
	 ***	Giving Return_Status.
	 ***/

	e = queue_read(inbuf,inlen);
        *bytes = iosb.byte_count;
        return e;
}


unsigned int tcp_ip_send_read ( char *outbuf, int outlen,
                                char *inbuf, int inlen, int *bytes )
{
	/***
	 ***  Call "Tcp_Ip_Reading" Using
	 ***	By Reference Sending_Data
	 ***	By Value 1024              ( You MUST specify length...! )
	 ***	By Reference Receive_Buffer
	 ***	By Value 1024              ( MUST specify MAX length...! )
	 ***	By Reference Bytes_Read    ( How much did we get )
	 ***	Giving Return_Status.
	 ***/

	e = socket_write(s, outbuf, outlen);
	if (e < 0) {return (vmserrno);}

        e = queue_read(inbuf, inlen);
        *bytes = iosb.byte_count;
	return e;
}

unsigned int queue_read(char *inbuf, int inlen)
{
	e = sys$setimr(sock_efn,timo,0,timer_id,0);
/*      e = sys$qiow(sock_efn,s,func,&iosb,0,0,inbuf,inlen,&timo,0,0,0); */
        e = sys$qio(sock_efn,s,func,&iosb,0,0,inbuf,inlen,0,0,0,0);
        e = sys$waitfr(sock_efn);
        if  (!iosb.byte_count) {return SS$_TIMEOUT;}

	sys$cantim(timer_id,0);
        return e;
}

unsigned int tcp_ip_terminate ( )
{
	/***
	 ***  Call "Tcp_Ip_Terminate"
	 ***	Giving Return_Status.
	 ***/

	e = socket_close(s);
	if (e < 0) {return (vmserrno);}

	return 1;
}


static void timo_trap (s)
{
  if (!	iosb.status) {sys$cancel(s);}
  sys$cantim(timer_id,0);
}

/*******************************************
 ***	CLM_DFR_SOC_01.C Module End...   ***
 *******************************************/
