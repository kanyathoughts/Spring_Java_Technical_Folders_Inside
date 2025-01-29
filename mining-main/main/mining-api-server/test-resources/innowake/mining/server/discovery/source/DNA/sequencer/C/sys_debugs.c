#define IDENT "V1.0-00"

	/*==============================================================*/
	/*								*/
	/*	SUITE	:	"C" service routines			*/
	/*	PROGRAM	:	CORDEBUGS.C				*/
	/*	TITLE	:	Bridge between BASIC and HN_DEBUGS	*/
	/*	AUTHOR	:	Shane Smith				*/
	/*								*/
	/* This contains a BASIC-callable entry point to the HN_DEBUGS	*/
	/* intelligent debug routing routines. This replaces an earlier	*/
	/* version written in BASIC.					*/
	/*								*/
	/* This also contains a BASIC-callable entry point to the	*/
	/* HN_CHECK_DEBUGS function.					*/
	/*==============================================================*/

#include <descrip.h>							/* Descriptor format in C				*/
#include <string.h>							/* 'string' handling functions				*/
#include <str$routines.h>						/* basic-type string handling functions			*/
#include "inc:HN_c_helpers.h"						/* "C" Service Routine header definitions		*/
#include "inc:HN_debugs.h"						/* Pull in the HN_DEBUGS structures and definitions.	*/

	/*==============================================================*/
	/*	ENTRY POINT FUNCTION					*/
	/*								*/
	/*	sys_debugs(	struct dsc$descriptor_s *module,	*/
	/*			struct dsc$descriptor_s *action,	*/
	/*			struct dsc$descriptor_s *action_data )	*/
	/*								*/
	/* This is the main entry point from BASIC. Its arguments are	*/
	/* all BASIC string descriptors, so BASIC doesn't have trouble	*/
	/* getting the data in.						*/
	/*==============================================================*/

short sys_debugs(	struct dsc$descriptor_s *module,		/* The name of the calling function			*/
			struct dsc$descriptor_s *action,		/* The action to perform; see HN_debugs.c		*/
			struct dsc$descriptor_s *action_data )		/* Any data required by the function we're performing	*/
{
extern	cdbg_nonvol_str			 cdbg;				/* Arrange access to main non-volatile storage struct.	*/
	char				 c_module[ CDBG_MODULE_LEN+1 ];	/* A buffer to make a c-type copy of the jobame in.	*/
	char				 c_action;			/* A local copy of the action code, so we can change it	*/
	char				 localbuff[ 500 ];		/* A buffer for non-P debugs action_data		*/
	int				 walker;			/* Used in stripping unwanted nulls from action_data	*/
	int				 dummy;				/* Used to take unused returned values			*/
	short				 state;				/* Used to pass the current debug state to BASIC	*/

	if ( cdbg.dbg == 'Y' ) puts( "=== In sys_debugs.c" );		/* If local debugs are on, keep a commentary		*/

	if ( module == NULL )						/* If we have no module argument, default to UNKNOWN	*/
	{								/*							*/
            *c_module = 'u','n','k','n','o','w','n','\0';		/*							*/
	}								/*							*/
	else								/*							*/
	{								/*							*/
	    memcpy( 	&c_module[0],					/* If we've got one though, copy it into our local	*/
			module->dsc$a_pointer,				/* buffer ...						*/
			module->dsc$w_length );				/*							*/
	    c_module[ module->dsc$w_length ] = '\0';			/* ... so we can add a nul terminator.			*/
	};								/*							*/


	if ( ( action == NULL ) || ( action->dsc$w_length != 1 ) )	/* If there's no action, or it's the wrong length,	*/
	{								/*							*/
	    c_action = 'P';						/* default it to print-line,				*/
	    HN_buf_debugs( "", 0, "{cdbg: action def'd to P}" );	/* and prefix it in the buffer with an error report	*/
	}								/*							*/
	else								/*							*/
	{								/*							*/
	    c_action = *action->dsc$a_pointer;				/* If it's kosher though, use it.			*/
	};								/*							*/


	if ( action_data == NULL )					/* If the action data is useless,			*/
	{								/*							*/
	    HN_debugs( c_module,'P',"{cdbg: Invalid action data}" );	/* drop an error message				*/
	    return ( 0 );						/* and bomb out before we go boom.			*/
	};								/*							*/

	switch( c_action )						/* Farm out the operation to the appropriate functions	*/
	{								/*							*/
									/*							*/
	    case 'P':							/* P = Print a debug string				*/
	    case 'p':							/* ------------------------				*/
		if ( action_data->dsc$w_length != 0 )			/* If there's something to print,			*/
		    HN_buf_debugs( "",					/* use the buffered debug facility to impose the right	*/
				action_data->dsc$w_length,		/* length as given on the string descriptor. This stops	*/
				action_data->dsc$a_pointer );		/* embedded nulls truncating the output prematurely.	*/
		HN_buf_debugs(	c_module, 0, "" );			/* The buffer's set up; Now print it.			*/
		break;							/*							*/
									/*							*/
	    default:							/* Anything else, route through the main HN_DEBUGS	*/
		if ( action_data->dsc$w_length != 0 )			/* If there's something to print,			*/
		{							/*							*/
		    memcpy(	&localbuff[0],				/* copy it into our local buffer ...			*/
				action_data->dsc$a_pointer,		/* 							*/
				action_data->dsc$w_length );		/*							*/
		    for ( walker = 0;					/* From the start of the local buffer			*/
			  walker <= action_data->dsc$w_length;		/* Until the end of the updated bit			*/
			  walker++ )					/* Moving in steps of one				*/
		    {							/*							*/
			if ( localbuff[ walker ] == '\0' )		/* If the char at that location is a nul,		*/
			    localbuff[ walker ] = '.';			/* turn it into a dot					*/
		    };							/*							*/
		};							/*							*/
		localbuff[ action_data->dsc$w_length ] = '\0';		/* ... and add a nul terminator.			*/
		HN_debugs(	c_module,				/*							*/
				c_action,				/* Send the now clean, nul-terminated results through	*/
				&localbuff[0] );			/* the main HN_debugs routine				*/
		break;							/*							*/
									/*							*/
	};								/* End of switch( in_action )				*/

	if ( cdbg.dbg == 'Y' ) puts( "=== Leaving cordebugs.c" );	/*							*/
	return (0);							/* Return success					*/

} /* end of sys_debugs */

	/*==============================================================*/
	/*	ENTRY POINT FUNCTION					*/
	/*								*/
	/*	sys_check_debugs(					*/
	/*		struct dsc$descriptor_s *module,		*/
	/*		struct dsc$descriptor_s *out_destination )	*/
	/*								*/
	/* This is the main entry point from BASIC. Its arguments are	*/
	/* all BASIC string descriptors, so BASIC doesn't have trouble	*/
	/* getting the data in.						*/
	/*								*/
	/* Because C doesn't manage its memory like BASIC, the returned	*/
	/* string is constructed using VMS's str$copy_r routine.	*/
	/*==============================================================*/

short sys_check_debugs(	struct dsc$descriptor_s *module,		/* The name of the calling function			*/
			struct dsc$descriptor_s *out_destination )	/* If debugs are on, this is the output spec.		*/
{
extern	cdbg_nonvol_str			 cdbg;				/* Arrange access to main non-volatile storage struct.	*/
	struct dsc$descriptor_s  	 ddesc;				/* Declare a descriptor for the return dest buffer	*/
	char				 c_module[ CDBG_MODULE_LEN+1 ];	/* A buffer to make a c-type copy of the jobame in.	*/
	char				 dbg_dest[ CDBG_DEST_LEN+1 ];
	int				 returned = 0;
	unsigned int			 unsint;
	short				 destlen;

	if ( cdbg.dbg == 'Y' )
	    printf( "=== In sys_check_debugs" );

	if ( module == NULL )						/* If we have no module argument, default to UNKNOWN	*/
	{								/*							*/
            *c_module = 'u','n','k','n','o','w','n','\0';		/*							*/
	}								/*							*/
	else								/*							*/
	{								/*							*/
	    memcpy( 	&c_module[0],					/* If we've got one though, copy it into our local	*/
			module->dsc$a_pointer,				/* buffer ...						*/
			module->dsc$w_length );				/*							*/
	    c_module[ module->dsc$w_length ] = '\0';			/* ... so we can add a nul terminator.			*/
	};								/*							*/

	if ( cdbg.dbg == 'Y' )
	    printf( "=== Testing %s\n",					/* If local debugs are on, keep a commentary		*/
			c_module );

	returned = HN_check_debugs( c_module, dbg_dest );

	if ( returned != -1 )
	{
	    if ( cdbg.dbg == 'Y' ) puts( "=== No debugs required." );	/* If local debugs are on, keep a commentary		*/
	    return (0);							/* Return zero to the caller.				*/
	};

	if ( cdbg.dbg == 'Y' )
	{
	    printf( "=== Module [%s] debug destination is [%s]\n",
			c_module, dbg_dest );
	    puts( "=== Calling str$copy_r" );				/* If local debugs are on, keep a commentary		*/
	}

	destlen = (short)strlen(dbg_dest);

	unsint = str$copy_r(	out_destination,			/* Copy into this BASIC compatible string		*/
				&destlen,				/* This much data					*/
				dbg_dest );				/* From this location					*/

	if ( cdbg.dbg == 'Y' )
	    printf( "=== Str$copy_r returned %d\n",			/* If local debugs are on, keep a commentary		*/
			unsint );

	return (-1);	

}
