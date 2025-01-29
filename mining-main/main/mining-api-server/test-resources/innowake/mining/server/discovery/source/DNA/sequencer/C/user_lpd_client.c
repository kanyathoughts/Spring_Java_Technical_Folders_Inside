/* CMS REPLACEMENT HISTORY, Element USER_LPD_CLIENT.C*/
/* *9    21-MAR-2000 12:48:58 GOATLEY "Allow user to specify PASSALL mode"*/
/* *8    20-JAN-2000 16:33:14 LENTZ "4903 - wildcard MULTINET_PRINTER_*_RETAIN_CR_DEFAULT"*/
/* *7    22-MAY-1997 10:14:00 BERGANDI "added logical/parameter to not send LF after FF. CR-3475"*/
/* *6     1-DEC-1996 14:05:55 BERGANDI "corrected accvio. bug in 4.0 Revb B."*/
/* *5    31-OCT-1996 15:51:50 BERGANDI "added code to support file type handling and more parameter support (CR-1494,CR-1888)"*/
/* *4     7-APR-1996 12:50:52 MADISON "support : in parameter specficiations"*/
/* *3    17-MAR-1996 14:16:23 MADISON "some fixes for reading print parameters"*/
/* *2    16-MAR-1996 14:33:26 MADISON "LPD improvements - pass 1"*/
/* *1    15-JUN-1994 13:49:53 PIPER "initial CMS population"*/
/* CMS REPLACEMENT HISTORY, Element USER_LPD_CLIENT.C*/
/*
 *  Copyright (C) 1989, 1990, 1992  TGV, Incorporated
 *  Copyright (C) 2000, Process Software
 *
 *  This is a template lpd symbiont customization image. This allows a
 *  user to customize the way that the LPD protocol symbiont generates
 *  the control file which it sends over the network, and the name and
 *  internet address of the server it connects to.
 *
 *  An exact copy of this code is linked into the MULTINET_LPD_SYMBIONT.EXE
 *  image and invoked when the symbiont needs to generate a CF file.
 *  You can modify this code to change the default generation.
 *  If the sharable image MULTINET:USER_LPD_CLIENT.EXE exists, then the
 *  MULTINET_LPD_SYMBIONT.EXE image will call thies routine in it, instead
 *  of the linked-in routine.
 *
 *  THIS INTERFACE IS SUBJECT TO CHANGE WITHOUT NOTICE
 *
 *  Compile and link:
 *  $ CC USER_LPD_CLIENT
 *  $ LINK/SHARE USER_LPD_CLIENT/OPT
 *
 *  Install by placing USER_LPD_CLIENT.EXE in the MULTINET: directory.
 *  The customized image will be used the next time the symbiont
 *  process is restarted.
 *
 */

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#if defined(__GNUC__) || defined(MULTINET_ALPHA)
#include <vms/smbdef.h>		   /* Include SMB definitions */
#include <vms/descrip.h>
#include <vms/stsdef.h>
#include <vms/lnmdef.h>
#include <vms/ssdef.h>
#else
#include "multinet_root:[multinet.include.vms]smbdef.h"
#include <descrip.h> 
#include <stsdef.h>
#include <lnmdef.h>
#include <ssdef.h>
#endif /*__GNUC__*/

extern char *strchr();

/*
 *	This routine is called to generate a CF file.
 *
 *	Get_Job_Item -- address of a special routine to call to get information
 *			about the job we are formatting this CF file for.
 *			(null terminated).
 *
 *	Context      -- context to pass to Get_Job_Item.
 *
 *	DF_File      -- The UNIX name of the DF file being printed. There is
 *			only one DF file per job!!!
 *
 *	BaseName     -- The base of the VMS filename (without extension) of
 *			the file being printed.
 *
 *	HostName     -- The name of THIS host.
 *
 *	FullName     -- The VMS filename with extension of the file being printed.
 *
 */

char *generate_control_file_v40(Get_Job_Item, Context, Df_File, BaseName, HostName, FullName)
int (*Get_Job_Item) ();
int Context;
char *Df_File, *BaseName, *HostName, *FullName;
{
	char *cp, *cp1;
	char User_Name[64];
	static char Local[2048];
	int Print_Control[2], i;
	char Queue_Name[33];
	char Parameter[256];
    	char Filter_Char;
        char Class[256];
	char Logical_Name[256];
	char temp[256];
	char Parameter1[256];
	char Parameter2[256];
	char Parameter3[256];
	char Parameter4[256];
	char Parameter5[256];
	char Parameter6[256];
	char Parameter7[256];
	char Parameter8[256];
	char Form_Name[256];
	char Note[256];
	char Z_String[4096];


#if 0
	Dprintf("Df_File = %s\n", Df_File) ;
	Dprintf("BaseName = %s\n", BaseName) ;
	Dprintf("HostName = %s\n", HostName) ;
	Dprintf("FullName = (%08X) \n", FullName) ;
#endif
	/*
	 *	Get the necessary job information
	 */

	(*Get_Job_Item) (Context,SMBMSG$K_USER_NAME,User_Name);
	cp = strchr(User_Name,' '); if (cp) *cp = 0;
	(*Get_Job_Item) (Context,SMBMSG$K_PRINT_CONTROL, Print_Control);


	(*Get_Job_Item) (Context,SMBMSG$K_QUEUE,Queue_Name);
	cp = strchr(Queue_Name,' '); if (cp) *cp = 0;

	/*
	 *	Lower case it the way UNIX machines expect.
	 */
	for (cp=User_Name; *cp; cp++)
	    if (isupper(*cp)) *cp = tolower(*cp);


	/*
	 * Setup the -C field
 	 * If a parameter begins with CLASS= then use it as the Class 
	 * field, otherwise use the Host Name
 	 */

    	strcpy(Class, HostName);
    	Filter_Char = (Print_Control[0] & SMBMSG$M_PASSALL) ? 'v' : 'f';

        /*
         *      Set the Filter_Char to use
         */
        sprintf (Logical_Name,
                  "MULTINET_PRINTER_%s_DEFAULT_FILTER",Queue_Name);
        if (get_logical(Logical_Name,temp,256,0)) Filter_Char = *temp;

        if (Print_Control[0] & SMBMSG$M_PASSALL) {
                Filter_Char = 'v' ;
                sprintf (Logical_Name,
                         "MULTINET_PRINTER_%s_PASSALL_FILTER",Queue_Name);
                if (get_logical(Logical_Name,temp,256,0))  {
                        Filter_Char = *temp;
		}
        }

    	for (i = 0; i < 8; i++) {
	    if (!(*Get_Job_Item) (Context,SMBMSG$K_PARAMETER_1+i,Parameter)) continue;
    	    if (Parameter[0] == '\0') continue;
    	    cp1 = cp = strchr(Parameter, '=');
    	    if (cp == 0) cp1 = cp = strchr(Parameter, ':');
    	    if (cp == 0) continue;
    	    while (cp > Parameter && isspace(*(cp-1))) cp--;
    	    *cp = '\0';
    	    for (cp = Parameter; *cp != '\0'; cp++) if (islower(*cp)) *cp = toupper(*cp);
    	    for (cp1++; isspace(*cp1); cp1++);
    	    for (cp = cp1 + strlen(cp1); cp > cp1 && isspace(*(cp-1)); cp--);
    	    *cp = '\0';
    	    if (*cp1 != '\0') {
    	    	if (strcmp(Parameter, "CLASS") == 0) {
    	    	    strcpy(Class, cp1);
    	    	} else if (strcmp(Parameter, "FILTER") == 0) {
    	    	    Filter_Char = *cp1;
    	    	}
    	    }
	}

	Z_String[0] = '\0';
	for (i = 0; i < 8; i++) {
		(*Get_Job_Item) (Context,SMBMSG$K_PARAMETER_1+i,Parameter1+i);
		if (strlen(Parameter1+i) != 0) {
			strcat(Z_String, Parameter1+i);
			strcat(Z_String, ",");
		}
	}

 	(*Get_Job_Item) (Context,SMBMSG$K_FORM_NAME,Form_Name);
	strcat(Z_String, "Form=");
	strcat(Z_String, Form_Name);
	strcat(Z_String, ",");
	
 	(*Get_Job_Item) (Context,SMBMSG$K_NOTE,Note);
	for (i = 0; i < 255; i++) {
		if (Note[i] != ' ') continue;
                Note[i] = '_';
	}
	strcat(Z_String, "Note=");
	strcat(Z_String, Note);

	/*
	 *	Format it, dont include flags to 
 	 * 	print the remote banner if the logical name exists
	 */

	sprintf (Logical_Name, 
		  "MULTINET_PRINTER_%s_SUPPRESS_REMOTE_BANNER",Queue_Name);

#if 0
	Dprintf("HostName = %08X, User_Name = %08X, BaseName = %08X, Class = %08X, Filter_Char = %c, DfFile = %08X, FullName = %08X\n",
		HostName, User_Name, BaseName, Class, Filter_Char, Df_File, FullName) ;
	Dprintf("HostName = %s\n", HostName) ;
	Dprintf("User_Name = %s\n", User_Name) ;
	Dprintf("BaseName = %s\n", BaseName) ;
	Dprintf("Class = %s\n", Class) ;
	Dprintf("Filter_Char = %c\n", Filter_Char) ;
	Dprintf("Df_File = %s\n", Df_File) ;
	Dprintf("FullName = %s\n", FullName) ;
#endif
	if (get_logical(Logical_Name,temp,256,0))  {
		sprintf(Local,
			"H%s\nP%s\n%c%s\nU%s\nTRemote from %s\nN%s\n",
			HostName,
			User_Name,
			Filter_Char,
			Df_File,
			Df_File,
			HostName,
			FullName);
	}
	else  {
		sprintf(Local,
			"H%s\nP%s\nJ%s\nC%s\nL%s\n%c%s\nU%s\nTRemote from%s\nN%s\nZ%s\n",
			HostName,
			User_Name,
			BaseName,
			Class,
			User_Name,
    	    	    	Filter_Char,
			Df_File,
			Df_File,
			HostName,
			FullName,
			Z_String);
	}
#if 0
	Dprintf("Local = |%s|\n", Local) ;
#endif

	return(Local);
}

/*
 *	This routine is called once at the start of each job to determine
 *	the Internet Address of the server to connect to and the name of
 *	the remote printer to use. NORMALLY this information comes out of
 *	the MULTINET_NLPx_REMOTE_PRINTER logical name, but this hook allows
 *	you to customize this using per-job criteria.
 *
 *
 *	Get_Job_Item -- address of a special routine to call to get information
 *			about the job we are processing.
 *			(null terminated)
 *
 *	Context      -- context to pass to Get_Job_Item.
 *
 *	Device	     -- The device name of this printer, eg, NLP3
 *
 *	Default      -- The default configuration. This is a string of the
 *			format:
 *			    "Address=192.0.0.1,Printer=lp"
 *
 *	This routine returns a string of the same format, OR a null pointer
 *	indicating to use the default configuration.
 */

char *get_remote_printer_info_v40(Get_Job_Item, Context, Device, Default)
int (*Get_Job_Item) ();
int Context;
char *Device, *Default;
{
    	char Address[256], Printer[256], RetainCR[2], tmp[256], *cp, *cp1;
    	char QueueName[256], lognam[256], NoFFLF[2], PassAll[2];
    	static char RetBuf[256];
    	int i, Allow_UserSpec;
        int gotit;

    	/*
    	 *  	If MULTINET_PRINTER_queuename_ALLOW_USER_SPEC is defined,
    	 *  	then check to see if the user specified a print destination
    	 *  	in the print parameters.
    	 */
    	Address[0] = '\0';
    	Printer[0] = '\0';
    	RetainCR[0] = '\0';
	NoFFLF[0] = '\0' ;
    	Allow_UserSpec = 0;

    	(*Get_Job_Item)(Context, SMBMSG$K_EXECUTOR_QUEUE, QueueName);
    	sprintf(lognam, "MULTINET_PRINTER_%s_ALLOW_USER_SPEC", QueueName);
    	if (get_logical(lognam,tmp,sizeof(tmp),0)) {
    	    if (islower(tmp[0])) tmp[0] = toupper(tmp[0]);
    	    if (tmp[0] == 'Y' || tmp[0] == 'T' || tmp[0] == '1') Allow_UserSpec = 1;
    	}


        /*
         * Check for Retain_CR logical
         */
        gotit    = 0;
        if (! gotit)
           {
           sprintf (lognam,"MULTINET_PRINTER_%s_RETAIN_CR_DEFAULT",QueueName);
           if (get_logical(lognam,tmp,sizeof(tmp),0))
              gotit    = 1;
           }
        if (! gotit)
           {
           sprintf (lognam,"MULTINET_PRINTER_*_RETAIN_CR_DEFAULT");
           if (get_logical(lognam,tmp,sizeof(tmp),0))
              gotit    = 1;
           }
        if (gotit)
           {
	   if (islower(tmp[0])) tmp[0] = toupper(tmp[0]);
	   if (tmp[0] == 'Y' || tmp[0] == 'T' || tmp[0] == '1')
	      strcpy(RetainCR, "Y");
	   else
	      strcpy(RetainCR, "N");
           }

	/*
	* Check for PASSALL logical
	*/
	sprintf (lognam,"MULTINET_PRINTER_%s_PASSALL_DEFAULT",QueueName);
	if (get_logical(lognam,tmp,sizeof(tmp),0)) {
	    if (islower(tmp[0])) tmp[0] = toupper(tmp[0]);
	    if (tmp[0] == 'Y' || tmp[0] == 'T' || tmp[0] == '1')  {
		strcpy(PassAll, "Y");
	    }
	    else  {
		strcpy(PassAll, "N");
	    }
	}

        /*
         * Check for NoLFAfterFF logical
         */
        gotit    = 0;
        if (! gotit)
           {
           sprintf (lognam,"MULTINET_PRINTER_%s_NO_FFLF_DEFAULT",QueueName);
           if (get_logical(lognam,tmp,sizeof(tmp),0))
              gotit    = 1;
           }
        if (! gotit)
           {
           sprintf (lognam,"MULTINET_PRINTER_*_NO_FFLF_DEFAULT");
           if (get_logical(lognam,tmp,sizeof(tmp),0))
              gotit    = 1;
           }
        if (gotit)
           {
	   if (islower(tmp[0])) tmp[0] = toupper(tmp[0]);
	   if (tmp[0] == 'Y' || tmp[0] == 'T' || tmp[0] == '1')
	      strcpy(NoFFLF, "Y");
	   else
	      strcpy(NoFFLF, "N");
           }


    	for (i = 0; i < 8; i++) {
    	    if (!(*Get_Job_Item)(Context, SMBMSG$K_PARAMETER_1+i, tmp)) continue;
    	    if (tmp[0] == '\0') continue;
    	    cp1 = cp = strchr(tmp, '=');
    	    if (cp == 0) cp1 = cp = strchr(tmp, ':');
    	    if (cp == 0) continue;
    	    while (cp > tmp && isspace(*(cp-1))) cp--;
    	    *cp = '\0';
    	    for (cp = tmp; *cp != '\0'; cp++) if (islower(*cp)) *cp = toupper(*cp);
    	    for (cp1++; isspace(*cp1); cp1++);
    	    for (cp = cp1 + strlen(cp1); cp > cp1 && isspace(*(cp-1)); cp--);
    	    *cp = '\0';
    	    if (*cp1 != '\0') {
    	        if (strcmp(tmp, "ADDRESS") == 0) {
    	        	strcpy(Address, cp1);
    	        } else if (strcmp(tmp, "PRINTER") == 0) {
    	        	strcpy(Printer, cp1);
    	        } else if (strcmp(tmp, "RETAIN_CR") == 0) {
    	        	if (*cp1 == 'y' || *cp1 == 'Y' || *cp1 == '1' ||
    	        	    	*cp1 == 't' || *cp1 == 'T') {
    	        	    strcpy(RetainCR, "Y");
    	        	} else {
    	        	    strcpy(RetainCR, "N");
    	        	}
    	        } else if (strcmp(tmp, "NOFFLF") == 0) {
    	        	if (*cp1 == 'y' || *cp1 == 'Y' || *cp1 == '1' ||
    	        	    	*cp1 == 't' || *cp1 == 'T') {
    	        	    strcpy(NoFFLF, "Y");
    	        	} else {
    	        	    strcpy(NoFFLF, "N");
    	        	}
		} else if (strcmp(tmp, "PASSALL") == 0) {
			if (*cp1 == 'y' || *cp1 == 'Y' || *cp1 == '1' ||
				*cp1 == 't' || *cp1 == 'T') {
			    strcpy(PassAll, "Y");
			} else {
			    strcpy(PassAll, "N");
			}
		}
	    }
    	}
    	/*
    	 *  It's OK for us to start with the default settings
    	 *  and then tack on anything else, since the parsing
    	 *  code in the calling routine will handle having
    	 *  multiple entries of the same parameter (using the
    	 *  one specified last).
    	 */

    	if (Allow_UserSpec && Address[0] != '\0') {
    	    sprintf(RetBuf, "Address=%s", Address);
    	    if (Printer[0] == '\0') strcpy(Printer, "lp");
    	} else strcpy(RetBuf, Default);

    	if (Allow_UserSpec && Printer[0] != 0) {
    	    if (RetBuf[0] != '\0') strcat(RetBuf, ",");
    	    strcat(RetBuf, "Printer=");
    	    strcat(RetBuf, Printer);
    	}
    	if (RetainCR[0] != '\0') {
    	    if (RetBuf[0] != '\0') strcat(RetBuf, ",");
    	    strcat(RetBuf, "Retain_CR=");
    	    strcat(RetBuf, RetainCR);
    	}
    	if (NoFFLF[0] != '\0') {
    	    if (RetBuf[0] != '\0') strcat(RetBuf, ",");
    	    strcat(RetBuf, "NoFFLF=");
    	    strcat(RetBuf, NoFFLF);
    	}
	if (PassAll[0] != '\0') {
	    if (RetBuf[0] != '\0') strcat(RetBuf, ",");
	    strcat(RetBuf, "PASSALL=");
	    strcat(RetBuf, PassAll);
	}
	
	return RetBuf;
}


/*
 *	This routine is called once when the symbiont starts up. It could
 *	be used to PSM$REPLACE any of the symbiont routines. The routines
 *	PSM$K_OUTPUT and PSM$K_JOB_COMPLETION are SACRED and cannot be replaced
 *	in this routine with breaking the MultiNet symbiont.
 *
 *	PSM$REPLACE  -- address of a the VMS PSM$REPLACE routine.
 */

int psm_init_v40(PSM$REPLACE)
int (*PSM$REPLACE) ();
{
#ifdef notdef
	Code = PSM$K_READ;
	(*PSM$REPLACE) (&Code, Someroutine);
#endif /*notdef*/

return 0;
}


/*
 * This routine (psm_max_streams_v40) is called to obtain the number of
 * streams to run on this symbiont, the normal value should be 15
 * and it should never return a number greater than 15
 */

int psm_max_streams_v40()
{

	return 15;
}


struct ITMLST {
    unsigned short bufsiz, itmcod;
    void *bufadr, *retlen;
};

/*
**++
**  ROUTINE:	get_logical
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Translates a logical name.  Can be used to translate
**  search list logicals by passing in a non-zero index value.
**
**  RETURNS:	boolean
**
**  PROTOTYPE:
**
**  	get_logical(char *lognam, char *eqvnam, int eqvsiz, int indx)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	    1:	success
**  	    0:  failure
**
**  SIDE EFFECTS:   	None.
**
**--
*/
int get_logical(char *lognam, char *eqvnam, int eqvsiz, int indx) {

    static $DESCRIPTOR(tabnam,"MULTINET_PRINTER_TABLE");
    static unsigned int attr = LNM$M_CASE_BLIND;
    struct dsc$descriptor lnmdsc;
    unsigned short len;
    struct ITMLST lnmlst[3];
    unsigned int status;

    lnmlst[0].bufsiz = sizeof(indx);
    lnmlst[0].itmcod = LNM$_INDEX;
    lnmlst[0].bufadr = &indx;
    lnmlst[0].retlen = 0;
    lnmlst[1].bufsiz = eqvsiz-1 > 255 ? 255 : eqvsiz-1;
    lnmlst[1].itmcod = LNM$_STRING;
    lnmlst[1].bufadr = eqvnam;
    lnmlst[1].retlen = &len;
    lnmlst[2].itmcod = lnmlst[2].bufsiz = 0;
    lnmdsc.dsc$b_dtype = DSC$K_DTYPE_T;
    lnmdsc.dsc$b_class = DSC$K_CLASS_S;
    lnmdsc.dsc$a_pointer = lognam;
    lnmdsc.dsc$w_length = strlen(lognam);
    status = sys$trnlnm(&attr,&tabnam,&lnmdsc,0,lnmlst);
    if ($VMS_STATUS_SUCCESS(status) && len > 0) {
    	eqvnam[len] = '\0';
    	return 1;
    }

    return 0;

} /* get_logical */
