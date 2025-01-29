#define IDENT "V1.1-03"

	/*==============================================================*/
	/*								*/
	/*	PROGRAM	:	HN_DEBUGS.C				*/
	/*	TITLE	:	"C" based debug output routines		*/
	/*	AUTHOR	:	Shane Smith				*/
	/*								*/
	/*	This software was developed by Shane Smith on his	*/
	/*	own time, on his own machine. It is offered for		*/
	/*	unrestricted use by anyone who finds it useful.		*/
	/*	If you require modifications to this code, please	*/
	/*	talk to Shane about it.					*/
	/*								*/
	/*==============================================================*/
	/*								*/
	/* Entry points:						*/
	/*								*/
	/* HN_debugs		Main debug control/output routine.	*/
	/*								*/
	/* HN_buf_debugs	Buffered debug output routines. Allows	*/
	/* HN_buf_int_debugs	a debug line to be composed over	*/
	/*			several calls before being output	*/
	/*			through HN_debugs.			*/
	/* HN_check_debugs	Find out if debugs are on for a given	*/
	/*			module name.				*/
	/*								*/
	/*==============================================================*/
	/*								*/
	/*		Modification History				*/
	/*								*/
	/* Vers.   Who	Date	Reason					*/
	/* ------  --- -------- ---------------------------------------	*/
	/* 1.1-03  SFS 27/02/98	Renamed from csrdebugs to hn_debugs	*/
	/* 1.1-02  SFS 21/02/97	Added quota watch facility		*/
	/* 1.1-01  SFS 17/02/97	Added broadcast output type		*/
	/* 1.1-00  SFS 24/09/96	Add some "/<W>-<O>" options		*/
	/* 1.0-00  SFS 08/03/96	Initial version				*/
	/*==============================================================*/

	/*==============================================================*/
	/*								*/
	/*		STANDARD DEFINITION INCLUDES			*/
	/*								*/
	/*==============================================================*/

#include <stdlib.h>							/* Pull in definitions for standard c functions		*/
#include <stdio.h>							/* 	for standard c I/O functions			*/
#include <string.h>							/* 	for 'string' handling functions			*/
#include <ctype.h>							/* 	for character-type identifier functions		*/
#include <time.h>							/* 	for time handling functions			*/


#if defined(__VMS) || defined(VMS)
#  include <jpidef.h>							/* Definitions for vms-specific getjpi sys call		*/
#  include <brkdef.h>							/* Definitions for vms-specific sys$brkthruw sys call	*/
#  include <starlet.h>							/* Definitions for vms-specific sys calls		*/
#  include <lib$routines.h>						/* Definitions for vms-specific lib calls		*/
#  include <str$routines.h>						/* basic-type string handling functions			*/
#  include <lnmdef.h>							/* Definitions for vms-specific logical translations	*/
#  include <descrip.h>							/* Descriptor format in C				*/
#  include "inc:HN_c_helpers.h"						/* Pull in the HN_ function prototypes used in here.	*/
#  include "inc:HN_tree.h"						/* Pull in defs for the HN_ generic tree functions.	*/
#  include "inc:HN_debugs.h"						/* Pull in the HN_DEBUGS structures and definitions.	*/
#else
#  include "HN_c_helpers.h"						/* Pull in the HN_ function prototypes used in here.	*/
#  include "HN_tree.h"							/* Pull in defs for the HN_ generic tree functions.	*/
#  include "HN_debugs.h"						/* Pull in the HN_DEBUGS structures and definitions.	*/
#endif

static void output_help_page(	void );					/* No options required					*/
static void parse_option_list(	char			*text_list,	/* Pointer to nul-term'd options list, ed "/#/T/F<nul>"	*/
				cdbg_options_str	*options );	/* Pointer to the option flags record to update		*/
static void do_pre_options(	cdbg_event_options_str	*opts,		/* The option flags sub-record which currently applies	*/
				cdbg_resrc_usage_str	*baseline,	/* Baseline figures for resource counts			*/
				char			*copy_to,	/* Optional destination to copy the reslt text to	*/
				int			*result_len );	/* The length of the resulting text			*/
static void new_baseline(	cdbg_event_options_str	*opts,		/* The option flags sub-record which currently applies	*/
				cdbg_resrc_usage_str	*baseline );	/* Baseline figures for resource counts			*/
static void print_simple_file_line( char 		*text_line );	/* The text to output, zero-terminated			*/
static void print_simple_screen_line( char 		*text_line );	/* The text to output, zero-terminated			*/
static void print_simple_broadcast_line( char 		*text_line );	/* The text to output, zero-terminated			*/
static void print_simple_line( 	char 			*text_line );	/* The text to output, zero-terminated			*/
static void print_simple_debug(	char			*in_pretext,	/* Text to put in before the main text.			*/
				char			*in_text );	/* The main text to print to the output media		*/
static void print_debug(	char			*in_text );	/* The main text to print to the output media		*/
static void flush_debugs( 	void );					/* No arguments required				*/
static int init_file_debugs( 	char 			*in_filename );	/* The name of the file we're supposed to output to	*/
static void begin_module_debug(	char			*in_data );	/* Describes the requested output method.		*/
static void end_module_debug( 	char 			*in_data );	
static void resources_used(	cdbg_resrc_usage_str	*ret_used );	/*							*/

#if defined(__DECC) || defined(__DECCXX)
#else
static int fsync( int discarded ) { return( 1 ); };
#endif
	/*==============================================================*/
	/*								*/
	/*	DECLARE NON-VOLATILE (SHARED) DATA STRUCTURES		*/
	/*								*/
	/*==============================================================*/

	cdbg_nonvol_str		cdbg;					/* See struct in HN_debugs.h				*/
	char			cdbg_line[ CDBG_OUTPUT_WIDTH + 1 ];	/* No point in allocating one line buffer per routine	*/

	struct HN_tree_root	cdbg_module_root;			/* Root for a tree of module dests for HN_check_debug	*/
	int			cdbg_module_root_initd = 0;		/* Flag for remembering whether we init'd it yet	*/
	char	cdbg_global_debugs_dest[CDBG_DEST_LEN] = {'\0'};	/* If this gets filled, it's used for all modules which	*/
									/* test for debugs via HN_check_debugs.			*/

	/*==============================================================*/
	/*								*/
	/*	LOCAL SERVICE ROUTINES not callable outside the suite	*/
	/*								*/
	/*==============================================================*/

	/*==============================================================*/
	/*								*/
	/*	output_help_page (local)				*/
	/*								*/
	/*==============================================================*/

static void output_help_page( void )
{
	print_simple_line( "HN_debugs:  ==================================================================== " );
	print_simple_line( "HN_debugs: | Supported HN_debugs destination codes:                             |" );
	print_simple_line( "HN_debugs: |   S:         Output to SYS$OUTPUT (usually the screen).            |" );
	print_simple_line( "HN_debugs: |   D:{device} Output to a specified terminal device.                |" );
	print_simple_line( "HN_debugs: |   F:{file}   Output to a specified filename.                       |" );
	print_simple_line( "HN_debugs: |                                                                    |" );
	print_simple_line( "HN_debugs: | If the specified destination cannot be used for any reason, output |" );
	print_simple_line( "HN_debugs: | defaults to 'FALLBACK.DBG' in the process's default directory.     |" );
	print_simple_line( "HN_debugs: |                                                                    |" );
	print_simple_line( "HN_debugs: | Options can be invoked in either of two formats; /<o> or /<w>-<o>, |" );
	print_simple_line( "HN_debugs: | where <o> is the option letter and <w> is when to apply it. If <w> |" );
	print_simple_line( "HN_debugs: | is not given (or not recognised), the option is assumed to apply   |" );
	print_simple_line( "HN_debugs: | to all calls to HN_debugs.                                         |" );
	print_simple_line( "HN_debugs: |                                                                    |" );
	print_simple_line( "HN_debugs: | Supported values for <o>:                                          |" );
	print_simple_line( "HN_debugs: |   F          Flush output automatically. (Warning: high overhead)  |" );
	print_simple_line( "HN_debugs: |   T          Show timestamps.                                      |" );
	print_simple_line( "HN_debugs: |   R          Show resource usage so far (cpu, IO, pagefaults).     |" );
	print_simple_line( "HN_debugs: |   Z          Zero resource usage counters.                         |" );
	print_simple_line( "HN_debugs: |   Q          Show remaining quotas                                 |" );
	print_simple_line( "HN_debugs: |   W          Watch for low quotas                                  |" );
	print_simple_line( "HN_debugs: |                                                                    |" );
	print_simple_line( "HN_debugs: | Supported values for <w>:                                          |" );
	print_simple_line( "HN_debugs: |   A or none  Option applies to every line.                         |" );
	print_simple_line( "HN_debugs: |   B          Option applies to 'B'egin module calls only.          |" );
	print_simple_line( "HN_debugs: |   P          Option applies to print calls; ie P's, F's and O's.   |" );
	print_simple_line( "HN_debugs: |   E          Option applies to 'E'nd module calls only.            |" );
	print_simple_line( "HN_debugs: |                                                                    |" );
	print_simple_line( "HN_debugs: | Eg's:                                                              |" );
	print_simple_line( "HN_debugs: |  'S:'             Output to the screen (or logfile, if in batch).  |" );
	print_simple_line( "HN_debugs: |  'D:_LTA3:'       Output to terminal LTA3: via broadcast.          |" );
	print_simple_line( "HN_debugs: |  'F:T.DBG'        Output to T.DBG....                              |" );
	print_simple_line( "HN_debugs: |  'F:T.DBG/T'       ... with the time on every line.                |" );
	print_simple_line( "HN_debugs: |  'F:T.DBG/B-T/E-T  ... with the time at the beginning and end of   |" );
	print_simple_line( "HN_debugs: |                        each module's output.                       |" );
	print_simple_line( "HN_debugs: |  'F:T.DBG/E-F'     ... flushing at the end of every module.        |" );
	print_simple_line( "HN_debugs: |                                                                    |" );
	print_simple_line( "HN_debugs: | ------------------------------------------------------------------ |" );
	print_simple_line( "HN_debugs: |                                                                    |" );
	print_simple_line( "HN_debugs: | In HN_debugs output, some character substitiutions are made:       |" );
	print_simple_line( "HN_debugs: |                                                                    |" );
	print_simple_line( "HN_debugs: | '^' Replaces most unprintable characters, control codes etc.       |" );
	print_simple_line( "HN_debugs: | '.' Replaces any <NUL>s HN_debugs can tell from line terminators.  |" );
	print_simple_line( "HN_debugs: |                                                                    |" );
	print_simple_line( "HN_debugs:  ==================================================================== " );
	print_simple_line( "-----------" );

	return;

}
	/*==============================================================*/
	/*								*/
	/*	resources_used (local)					*/
	/*								*/
	/* Fills a resources-used record with process's current stats.	*/
	/*==============================================================*/

static void resources_used(	cdbg_resrc_usage_str	*used )		/*							*/
{									/*							*/
	vms_list_item_str	itemlst[ 5 ];				/* Array of list items for use in the GETJPI call	*/
	int			discval;				/*							*/
	int			retval;					/* To take the return value from GETJPI			*/
	int			bio;					/* Stores bio before adding into dio to get cio.	*/

#if defined(__VMS) || defined(VMS)

	itemlst[ 0 ].buflen	= sizeof( used->cpu );			/* Build a list item requesting the cpu time used	*/
	itemlst[ 0 ].itmcod	= JPI$_CPUTIM;				/*							*/
	itemlst[ 0 ].buffer	= &used->cpu;				/*							*/
	itemlst[ 0 ].retbuf	= &discval;				/*							*/

	itemlst[ 1 ].buflen	= sizeof( used->io );			/* Build a list item requesting the Direct IO used	*/
	itemlst[ 1 ].itmcod	= JPI$_DIRIO;				/*							*/
	itemlst[ 1 ].buffer	= &used->io;				/*	Put it in 'combined io'; buffered IO gets added	*/
	itemlst[ 1 ].retbuf	= &discval;				/*	in later.					*/

	itemlst[ 2 ].buflen	= sizeof( bio );			/* Build a list item requesting the Buffered IO used	*/
	itemlst[ 2 ].itmcod	= JPI$_BUFIO;				/*							*/
	itemlst[ 2 ].buffer	= &bio;					/*							*/
	itemlst[ 2 ].retbuf	= &discval;				/*							*/

	itemlst[ 3 ].buflen	= sizeof( used->pgf );			/* Build a list item requesting the no of page faults	*/
	itemlst[ 3 ].itmcod	= JPI$_PAGEFLTS;			/*							*/
	itemlst[ 3 ].buffer	= &used->pgf;				/*							*/
	itemlst[ 3 ].retbuf	= &discval;				/*							*/

	itemlst[ 4 ].buflen	= 0;					/* Build a terminator list item				*/
	itemlst[ 4 ].itmcod	= 0;					/*							*/
	itemlst[ 4 ].buffer	= &discval;				/*							*/
	itemlst[ 4 ].retbuf	= &discval;				/*							*/

	retval = sys$getjpiw( 0, 0, NULL, &itemlst, 0, 0, 0 );		/* Get the usage information from VMS			*/

	used->io += bio;						/* Add bio to the dio already in cio.			*/

#else

	used->cpu = 0;							/* We can't do this on Unix yet.			*/
	used->io = 0;							/*							*/
	used->pgf = 0;							/*							*/
#endif

	used->secs = time( NULL );					/* Get the current time as a number of seconds		*/

	return;
}
	/*==============================================================*/
	/*								*/
	/*	quotas_available (local)				*/
	/*								*/
	/* Fills a resources-used record with process's current stats.	*/
	/*==============================================================*/

static void quotas_available(	vms_quotas_str		*available )	/*							*/
{									/*							*/
	vms_list_item_str	itemlst[ 7 ];				/* Array of list items for use in the GETJPI call	*/
	int			discval;				/*							*/
	int			retval;					/* To take the return value from GETJPI			*/

	if ( cdbg.dbg == 'Y' ) puts( "### Do qet quotas available" );	/* If local debugs are on, report where we are		*/

#if defined(__VMS) || defined(VMS)

	itemlst[ 0 ].buflen	= sizeof( available->bio );		/* Build a list item requesting the bio available	*/
	itemlst[ 0 ].itmcod	= JPI$_BIOLM;				/*							*/
	itemlst[ 0 ].buffer	= &available->bio;			/*							*/
	itemlst[ 0 ].retbuf	= &discval;				/*							*/

	itemlst[ 1 ].buflen	= sizeof( available->dio );		/* Build a list item requesting the dio available	*/
	itemlst[ 1 ].itmcod	= JPI$_DIOLM;				/*							*/
	itemlst[ 1 ].buffer	= &available->dio;			/*							*/
	itemlst[ 1 ].retbuf	= &discval;				/*							*/

	itemlst[ 2 ].buflen	= sizeof( available->enq );		/* Build a list item requesting the enq available	*/
	itemlst[ 2 ].itmcod	= JPI$_ENQLM;				/*							*/
	itemlst[ 2 ].buffer	= &available->enq;			/*							*/
	itemlst[ 2 ].retbuf	= &discval;				/*							*/

	itemlst[ 3 ].buflen	= sizeof( available->fil );		/* Build a list item requesting the fil cnt available	*/
	itemlst[ 3 ].itmcod	= JPI$_FILLM;				/*							*/
	itemlst[ 3 ].buffer	= &available->fil;			/*							*/
	itemlst[ 3 ].retbuf	= &discval;				/*							*/

	itemlst[ 4 ].buflen	= sizeof( available->ast );		/* Build a list item requesting the ast remaining	*/
	itemlst[ 4 ].itmcod	= JPI$_ASTLM;				/*							*/
	itemlst[ 4 ].buffer	= &available->ast;			/*							*/
	itemlst[ 4 ].retbuf	= &discval;				/*							*/

	itemlst[ 5 ].buflen	= sizeof( available->pgfl );		/* Build a list item requesting the pgfl available	*/
	itemlst[ 5 ].itmcod	= JPI$_PGFLQUOTA;			/*							*/
	itemlst[ 5 ].buffer	= &available->pgfl;			/*							*/
	itemlst[ 5 ].retbuf	= &discval;				/*							*/

	itemlst[ 6 ].buflen	= 0;					/* Build a terminator list item				*/
	itemlst[ 6 ].itmcod	= 0;					/*							*/
	itemlst[ 6 ].buffer	= &discval;				/*							*/
	itemlst[ 6 ].retbuf	= &discval;				/*							*/

	retval = sys$getjpiw( 0, 0, NULL, &itemlst, 0, 0, 0 );		/* Get the usage information from VMS			*/

	if ( cdbg.dbg == 'Y' )
	    printf( "### Quotas: dio %d, bio %d, enq %d, fil %d, ast %d, pgfl %d\n",
			available->dio, available->bio, available->enq,
			available->fil, available->ast, available->pgfl );

#else
	available->bio = available->dio = available->enq		/* Fill the slots with zeroes if we're not on VMS	*/
	    = available->fil = available->ast = available->pgfl = 0;	/*							*/

	if ( cdbg.dbg == 'Y' )
	    puts( "### Quotas not applicable" );

#endif

}

static int percentage( int in_val, int in_whole )
{
	if ( in_whole == 0 )
	    return -1;
	return (int)( (float)in_val / ((float)in_whole / 100.0 ) );

}

	/*==============================================================*/
	/*								*/
	/*	quotas_left (local)					*/
	/*								*/
	/* Fills a resources-used record with process's current stats.	*/
	/*==============================================================*/

static void quotas_left(	vms_quotas_str		*available,	/*							*/
				vms_quotas_str		*left )		/*							*/
{									/*							*/
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
	vms_list_item_str	itemlst[ 7 ];				/* Array of list items for use in the GETJPI call	*/
	int			discval;				/*							*/
	int			retval;					/* To take the return value from GETJPI			*/
	int			bio, dio, enq, fil, ast, pgfl;		/* Temporary storage for raw figures			*/

#if defined(__VMS) || defined(VMS)

	if ( cdbg.dbg == 'Y' ) puts( "### Do qet quotas left" );	/* If local debugs are on, report where we are		*/

	itemlst[ 0 ].buflen	= sizeof( bio );			/* Build a list item requesting the remaining bio	*/
	itemlst[ 0 ].itmcod	= JPI$_BIOCNT;				/*							*/
	itemlst[ 0 ].buffer	= &bio;					/*							*/
	itemlst[ 0 ].retbuf	= &discval;				/*							*/

	itemlst[ 1 ].buflen	= sizeof( dio );			/* Build a list item requesting the dio remaining	*/
	itemlst[ 1 ].itmcod	= JPI$_DIOCNT;				/*							*/
	itemlst[ 1 ].buffer	= &dio;					/*							*/
	itemlst[ 1 ].retbuf	= &discval;				/*							*/

	itemlst[ 2 ].buflen	= sizeof( enq );			/* Build a list item requesting the enq remaining	*/
	itemlst[ 2 ].itmcod	= JPI$_ENQCNT;				/*							*/
	itemlst[ 2 ].buffer	= &enq;					/*							*/
	itemlst[ 2 ].retbuf	= &discval;				/*							*/

	itemlst[ 3 ].buflen	= sizeof( fil );			/* Build a list item requesting the file cnt. remaining	*/
	itemlst[ 3 ].itmcod	= JPI$_FILCNT;				/*							*/
	itemlst[ 3 ].buffer	= &fil;					/*							*/
	itemlst[ 3 ].retbuf	= &discval;				/*							*/

	itemlst[ 4 ].buflen	= sizeof( ast );			/* Build a list item requesting the ASTs remaining	*/
	itemlst[ 4 ].itmcod	= JPI$_ASTCNT;				/*							*/
	itemlst[ 4 ].buffer	= &ast;					/*							*/
	itemlst[ 4 ].retbuf	= &discval;				/*							*/

	itemlst[ 5 ].buflen	= sizeof( pgfl );			/* Build a list item requesting the pgfl USED		*/
	itemlst[ 5 ].itmcod	= JPI$_PAGFILCNT;			/*							*/
	itemlst[ 5 ].buffer	= &pgfl;				/*							*/
	itemlst[ 5 ].retbuf	= &discval;				/*							*/

	itemlst[ 6 ].buflen	= 0;					/* Build a terminator list item				*/
	itemlst[ 6 ].itmcod	= 0;					/*							*/
	itemlst[ 6 ].buffer	= &discval;				/*							*/
	itemlst[ 6 ].retbuf	= &discval;				/*							*/

	retval = sys$getjpiw( 0, 0, NULL, &itemlst, 0, 0, 0 );		/* Get the usage information from VMS			*/

	left->dio  = percentage( dio, available->dio );			/* Turn each figure into a percentage remaining		*/
	left->bio  = percentage( bio, available->bio );			/*							*/
	left->enq  = percentage( enq, available->enq );			/*							*/
	left->fil  = percentage( fil, available->fil );			/*							*/
	left->ast  = percentage( ast, available->ast );			/*							*/
	left->pgfl = percentage( pgfl,available->pgfl );		/*							*/

	if ( cdbg.dbg == 'Y' )
	{								/*							*/
	    printf( "### Quotas left: dio %d/%d(%d), bio %d/%d(%d), enq %d/%d(%d)\n",
			dio, available->dio, left->dio,			/*							*/
			bio, available->bio, left->bio,			/*							*/
			enq, available->enq, left->enq );		/*							*/
	    printf( "### Quotas left: fil %d/%d(%d), ast %d/%d(%d), pgfl %d/%d(%d)\n",
			fil, available->fil, left->fil,			/*							*/
			ast, available->ast, left->ast,			/*							*/
			pgfl, available->pgfl, left->pgfl );		/*							*/
	};
#else
	left->bio = left->dio = left->enq				/* Fill the slots with 100's if we're not on VMS, as	*/
	    = left->fil = left->ast = left->pgfl = 100;			/* we don't want spurious low figures on Un*x		*/
#endif

}

	/*==============================================================*/
	/*								*/
	/*	parse_option_list (Local)				*/
	/*								*/
	/* Take a text options list and set the option flags in a given	*/
	/* record from its contents. Note: settings are additive.	*/
	/*==============================================================*/

static void parse_option_list(	char			*text_list,	/* Pointer to nul-term'd options list, ed "/#/T/F<nul>"	*/
				cdbg_options_str	*options )	/* Pointer to the option flags record to update		*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
	int			 ival;					/* Short-term generic int value holder			*/
	int			 len_text;				/* Length of provided text-form option list		*/
	char			*cptr;					/* Short-term generic pointer-to-char holder		*/
	char			*text_ptr;				/* Internal char pointer for walking along the text.	*/
	cdbg_event_options_str	*opt_type;				/* For selecting the event-options subrecord to update	*/

	text_ptr = text_list;						/* Make a moveable copy of text_list			*/
	len_text = strlen( text_ptr );					/* See how long the string is.				*/

	while ( len_text > 0 )						/* Keep going till we run out of text to parse		*/
	{								/*							*/
	    cptr = (char *)memchr( text_ptr, '/', len_text );		/* Hunt for the next "/"				*/
	    if ( cptr == NULL )						/* If there isn't one					*/
		len_text = 0;						/*    dump out of the loop.				*/
	    else							/* If there is a slash though,				*/
	    {								/*							*/
		if ( cptr[2] == '-' )					/*    A "-" in pos 2 means it's for certain calls only	*/
		{							/*    (Eg "/E-T" for timestamps on end calls only)	*/
		    switch( cptr[1] )					/*      Work out which call types the flag applies to	*/
		    {							/*							*/
			case 'B': case 'b':				/*	  B = "Begin module" calls only			*/
			    opt_type = &options->beg;			/*	       Select the 'begin' subrecord		*/
			    break;					/*							*/
									/*							*/
			case 'P': case 'p':				/*	  P = "Print line" calls only			*/
			    opt_type = &options->prt;			/*		(Not yet fully implemented)		*/
			    break;					/*							*/
									/*							*/
			case 'E': case 'e':				/*	  E = "End module" calls only			*/
			    opt_type = &options->end;			/*							*/
			    break;					/*							*/
									/*							*/
			default:					/*	  Anything else applies to all calls.		*/
			    opt_type = &options->all;			/*							*/
		    };							/*      End of switch (call-type).			*/
		    cptr += 2;						/*      Move the pointer on to the start of the flag	*/
		}							/*							*/
		else							/*    Else (there's no call-type involved)		*/
		{							/*							*/
			opt_type = &options->all;			/*	So set it to happen on all calls		*/
		};							/*    End of if (Call-type involved)			*/
									/*							*/
		switch( cptr[1] )					/*    Act on the letter after the slash (or colon).	*/
		{							/*							*/
		    case 'T': case 't':					/*	T = Print timestamps				*/
			opt_type->inl_set = opt_type->timestamp = 'Y';	/*	    						*/
			break;						/*							*/
									/*							*/
		    case 'F': case 'f':					/*	F = Flush output automatically			*/
			opt_type->inl_set = opt_type->autoflush = 'Y';	/*	    						*/
			break;						/*							*/
									/*							*/
		    case 'Z': case 'z':					/*	Z = Zero resource counters			*/
			opt_type->inl_set = opt_type->peg_figs = 'Y';	/*	    						*/
			break;						/*							*/
									/*							*/
		    case 'R': case 'r':					/*	R = Log resource usage				*/
			opt_type->inl_set = opt_type->showresrc = 'Y';	/*	    						*/
			break;						/*							*/
									/*							*/
		    case 'Q': case 'q':					/*	Q = Show remaining quotas			*/
			opt_type->ownl_set = opt_type->show_quo = 'Y';	/*	    						*/
			break;						/*							*/
									/*							*/
		    case 'W': case 'w':					/*	W = Watch for low quotas			*/
			opt_type->ownl_set = opt_type->watch_quo = 'Y';	/*	    						*/
			break;						/*							*/
									/*							*/
		    case 'H': case 'h':					/*	H = Print help message (First call only)	*/
			cdbg.help = 'Y';				/*							*/
			break;						/*							*/
									/*							*/
		    case '#':						/*	# = switch on HN_debugs debugs.		*/
			cdbg.dbg = 'Y';					/*	    Set the flag,				*/
			puts(   "### Debug debugs are on." );		/*	    and print an announcement			*/
			break;						/*							*/
		};							/*    End of switch (flag type).			*/
		text_ptr = cptr + 2;					/*    Advance past the option we just processed,	*/
		len_text = strlen( text_ptr );				/*    and recalculate the distance to the text end.	*/
	    };								/* End of if ("/" found)				*/
	};								/* End of loop (until end of text)			*/

	return;

} /* End of local void parse_option_list */

	/*==============================================================*/
	/*								*/
	/*	do_pre_options (Local)					*/
	/*								*/
	/*==============================================================*/

static void do_pre_options(	cdbg_event_options_str	*opts,		/* The option flags sub-record which currently applies	*/
				cdbg_resrc_usage_str	*baseline,	/* Baseline figures for resource counts			*/
				char			*copy_to,	/* Destination to copy any resulting text to		*/
				int			*result_len )	/* The length of the resulting text			*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
	cdbg_resrc_usage_str	 used_resrc;				/* To receive the current resource usage.		*/
	char			 buffer[ CDBG_OPT_TEXT_LEN ];		/* Buffer for composing the result text into		*/
	int			 ival;					/* Short-term generic integer value holder		*/
	time_t			 time_secs;				/* For retrieveing the time as a figure in seconds	*/
	char			*cptr;					/* Short-term generic pointer-to-char holder		*/
	char			*next_free = &buffer[0];		/* Points to the next free byte in the buffer.		*/

	buffer[0] = '\0';						/* Init the buffer					*/

	if ( cdbg.dbg == 'Y' ) puts( "### Do Pre Options." );		/* If local debugs are on, report where we are		*/

	if ( opts->timestamp == 'Y' )					/*							*/
	{								/*							*/
	    time_secs = time( NULL );					/* Get the current time details into a struct		*/
	    cptr = ctime( &time_secs );					/* Reformat time for display, and get ctime's buff addr	*/
	    memcpy( buffer, &cptr[ 10 ], 9 );				/* Use the " HH:MM:SS" from "Ddd Mmm DD HH:MM:HH YYYY"	*/
	    next_free = &buffer[ 9 ];					/* And position the next free buffer byte pointer	*/
	    *next_free = '\0';						/*							*/
	};								/*							*/

	if ( opts->showresrc == 'Y' )					/*							*/
	{								/*							*/
	    resources_used( &used_resrc );				/* Get the current stats				*/
	    sprintf( next_free, " %6de%6dc%6dio%6dp\0",			/* Put them in the output buffer, nicely formatted	*/
				used_resrc.secs - baseline->secs,	/*  Elapsed is shown as a 'since last baseline' figure	*/
				used_resrc.cpu  - baseline->cpu,	/*  As is CPU ticks					*/
				used_resrc.io   - baseline->io,		/*  and io, 						*/
				used_resrc.pgf  - baseline->pgf );	/*  And pagefaults					*/
	    next_free = (char *)memchr( buffer, '\0', 80 );		/* Find the nul terminator at the end.			*/
	};								/*							*/

	if ( opts->autoflush == 'Y' )					/* If we're autoflushing for this event type,		*/
	    cdbg.flush_on_exit == 'Y';					/*     set the flag do do it on leaving HN_debugs.	*/

	*result_len = strlen( buffer );					/* Work out how much result test we've built up		*/

	if ( copy_to != NULL )						/* If we're supposed to xerox the data somewhere,	*/
	    memcpy( copy_to, buffer, *result_len + 1 );			/*     do so, even if it's just a terminator.		*/

	return;

} /* End of char *do_pre_options */

	/*==============================================================*/
	/*								*/
	/*		do_line_options (Local)				*/
	/*								*/
	/*==============================================================*/

static void do_line_options(	cdbg_event_options_str	*opts,		/* The option flags sub-record which currently applies	*/
				cdbg_resrc_usage_str	*baseline,	/* Baseline figures for resource counts			*/
				char			*copy_to,	/* Destination to copy any resulting text to		*/
				int			*result_len )	/* The length of the resulting text			*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
	cdbg_resrc_usage_str	 used_resrc;				/* To receive the current resource usage.		*/
	char			 buffer[ CDBG_OPT_TEXT_LEN ];		/* Buffer for composing the result text into		*/
	int			 ival;					/* Short-term generic integer value holder		*/
	time_t			 time_secs;				/* For retrieveing the time as a figure in seconds	*/
	char			*cptr;					/* Short-term generic pointer-to-char holder		*/
	char			*next_free = &buffer[0];		/* Points to the next free byte in the buffer.		*/

	if ( cdbg.dbg == 'Y' ) puts( "### Do Line Options." );		/* If local debugs are on, report where we are		*/

	if ( opts->timestamp == 'Y' )					/*							*/
	{								/*							*/
	    time_secs = time( NULL );					/* Get the current time details into a struct		*/
	    cptr = ctime( &time_secs );					/* Reformat time for display, and get ctime's buff addr	*/
	    memcpy( buffer, &cptr[ 11 ], 9 );				/* Use the " HH:MM:SS" from "Ddd Mmm DD HH:MM:HH YYYY"	*/
	    next_free = &buffer[ 9 ];					/* And position the next free buffer byte pointer	*/
	};								/*							*/

	if ( opts->showresrc == 'Y' )					/*							*/
	{								/*							*/
	    resources_used( &used_resrc );				/* Get the current stats				*/
	    sprintf( next_free, "%de %dc %dio %dp \0",			/* Put them in the output buffer, nicely formatted	*/
				used_resrc.secs - baseline->secs,	/*  Elapsed is shown as a 'since last baseline' figure	*/
				used_resrc.cpu  - baseline->cpu,	/*  As is CPU ticks					*/
				used_resrc.io   - baseline->io,		/*  and io, 						*/
				used_resrc.pgf  - baseline->pgf );	/*  And pagefaults					*/
	    next_free = (char *)memchr( buffer, '\0', 80 );		/* Find the nul terminator at the end.			*/
	};								/*							*/

	if ( opts->autoflush == 'Y' )					/* If we're autoflushing for this event type,		*/
	    cdbg.flush_on_exit == 'Y';					/*     set the flag do do it on leaving HN_debugs.	*/

	if ( next_free != buffer )					/* If we've output some text				*/
	{								/*							*/
	    next_free[-1] = ']';					/* Round it off neatly with a bracket			*/
	    next_free[0] = ' ';						/* and a space						*/
	    next_free++;						/*							*/
	};								/*							*/

	next_free[0] = '\0';						/* Nul terminate the result text			*/
	*result_len = ( next_free - buffer );				/* Work out how much result test we've built up		*/
	memcpy( copy_to, buffer, *result_len + 1 );			/* Xerox to destination even if it's just a terminator.	*/

	return;

} /* End of char *do_line_options */

	/*==============================================================*/
	/*								*/
	/*	do_ownline_options (Local)				*/
	/*								*/
	/*==============================================================*/

static void do_ownline_options(	cdbg_event_options_str	*opts )		/* The option flags sub-record which currently applies	*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
	vms_quotas_str		 left;					/* To receive the current remaining quotas.		*/
	char			 buffer[ CDBG_OPT_TEXT_LEN ];		/* Buffer for composing the result text into		*/
	int			 ival;					/* Short-term generic integer value holder		*/
	char			 print_required;			/* Flag for whether a line needs printing or not	*/
	char			 quotas_read;				/* Flags whether we've already read quotas or not	*/
	char			*cptr;					/* Short-term generic pointer-to-char holder		*/
	char			*next_free = &buffer[0];		/* Points to the next free byte in the buffer.		*/

#define REPORT_IF_LOW( name, value )					\
	if ( value < CDBG_QUOTA_WARN_PRECENT )				\
	{								\
	    if ( cdbg.dbg == 'Y' )					\
		printf( "### %s quota below limit.\n", name );		\
	    sprintf( next_free, "%4s=%3d ", name, value );		\
	    next_free += 9;						\
	    print_required = 'Y';					\
	} else {							\
	    if ( cdbg.dbg == 'Y' )					\
		printf( "### %s quota ok.\n", name );			\
	};						

	if ( cdbg.dbg == 'Y' ) puts( "### Do Own-Line Options." );	/* If local debugs are on, report where we are		*/

	if ( opts->show_quo == 'Y' )					/* If we're printing quotas, do this bit.		*/
	{								/*							*/
	    cdbg.ownl_show_done = 'Y';					/* Remember we've been in here.				*/
	    quotas_left( &cdbg.quotas, &left );				/* Find out what quotas are left for printing		*/
	    quotas_read = 'Y';						/* Remember we've done that				*/
	    sprintf( buffer, "Quotas left: dio =%3d, bio =%3d, enq =%3d, fil =%3d, ast =%3d, pgfl=%3d",	/*			*/
			left.dio, left.bio, left.enq,			/* Fill the buffer with the formatted results		*/
			left.fil, left.ast, left.pgfl );		/*							*/
	    print_simple_debug( NULL, buffer );				/* Print what we've built				*/
	};								/*							*/

	if ( opts->watch_quo == 'Y' )					/* If we're monitoring quotas, do this bit.		*/
	{								/*							*/
	    cdbg.ownl_watch_done = 'Y';					/* Remember we've been in here.				*/
	    print_required = 'N';    					/*							*/
	    next_free = buffer;						/*							*/
	    if ( quotas_read != 'Y' )					/* If we didn't just do so,				*/
		quotas_left( &cdbg.quotas, &left );			/* Find out what quotas are left			*/
	    REPORT_IF_LOW( "dio", left.dio );				/* If less than allowed %age of this one left, print	*/
	    REPORT_IF_LOW( "bio", left.bio );				/*							*/
	    REPORT_IF_LOW( "enq", left.enq );				/*							*/
	    REPORT_IF_LOW( "fil", left.fil );				/*							*/
	    REPORT_IF_LOW( "ast", left.ast );				/*							*/
	    REPORT_IF_LOW( "pgfl", left.pgfl )				/*							*/
	    if ( print_required == 'Y' )				/* If there's something in the text buffer, print it.	*/
	    {								/*							*/
		if ( cdbg.dbg == 'Y' )					/*							*/
		    puts( "### Quota watch line triggered." );		/*							*/
		print_simple_debug( NULL, buffer );			/* Print what we've built				*/
	    } else {							/*							*/
		if ( cdbg.dbg == 'Y' )					/*							*/
		    puts( "### Quota watch line not required." );	/*							*/
	    };								/*							*/
	};								/*							*/

	return;

} /* End of do_ownline_options */

	/*==============================================================*/
	/*								*/
	/*	new_baseline (Local)					*/
	/*								*/
	/*==============================================================*/

static void new_baseline(	cdbg_event_options_str	*opts,		/* The option flags sub-record which currently applies	*/
				cdbg_resrc_usage_str	*baseline )	/* Baseline figures for resource counts			*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/

	if ( cdbg.dbg == 'Y' ) puts( "### Do Post Options." );		/* If local debugs are on, report where we are		*/

	if ( opts->peg_figs == 'Y' )					/* If we're supposed to peg the figures here, do so.	*/
	{								/*							*/
	    resources_used( baseline );					/* Peg the baseline values to the current stats,	*/
									/*							*/
	    if ( cdbg.dbg == 'Y' )					/*							*/
		printf( "### Peg: resrc[%d] %6dc %6dio %6dp\n",		/* Debugs again						*/
				cdbg.nest_level,			/*  Which layer we're in				*/
				baseline->cpu,				/*  CPU peg figures as they are now			*/
				baseline->io,				/*  Same with io					*/
				baseline->pgf );			/*  And pagefaults					*/
	};								/*							*/

	return;

} /* End of static void new_baseline */

	/*==============================================================*/
	/*								*/
	/*	print_simple_file_line (Local)				*/
	/*								*/
	/*==============================================================*/

static void print_simple_file_line( char *text_line )			/* The text to output, zero-terminated			*/
{
extern	cdbg_nonvol_str			 cdbg;				/* Arrange access to main non-volatile storage struct.	*/

	fputs( text_line, cdbg.file_desc );				/* Print to line to the open file, plus an '\n'		*/
	fputc( '\n', cdbg.file_desc );					/*							*/
	fflush( cdbg.file_desc );					/* Flush the C buffer into the RMS buffer		*/
	return;								/*							*/
}
	/*==============================================================*/
	/*								*/
	/*	print_simple_screen_line (Local)			*/
	/*								*/
	/*==============================================================*/

static void print_simple_screen_line( char *text_line )			/* The text to output, zero-terminated			*/
{
extern	cdbg_nonvol_str			 cdbg;				/* Arrange access to main non-volatile storage struct.	*/

	puts( text_line );						/* Print to line to the open file, plus an '\n'		*/
	return;								/*							*/
}

	/*==============================================================*/
	/*								*/
	/*	print_simple_broadcast_line (Local)			*/
	/*								*/
	/*==============================================================*/

static void print_simple_broadcast_line( char *text_line )		/* The text to output, zero-terminated			*/
{
extern	cdbg_nonvol_str			 cdbg;				/* Arrange access to main non-volatile storage struct.	*/

#if defined(__VMS) || defined(VMS)
	int			 stat;					/* An integer to take the return status			*/
	vms_iosb_str		 iosb;					/* For returned IO status blocks from sys calls		*/

	struct dsc$descriptor_s  tdesc;					/* Declare a descriptor for the text			*/
	struct dsc$descriptor_s  ddesc;					/* Declare a descriptor for the destination		*/

	tdesc.dsc$b_class	= DSC$K_CLASS_S;			/* It's a static string, ie it's immobile		*/
	tdesc.dsc$b_dtype	= DSC$K_DTYPE_T;			/* It's a text string					*/
	tdesc.dsc$w_length	= strlen(text_line);			/* How much of the string is occupied			*/
	tdesc.dsc$a_pointer	= text_line;				/* And this is where to find it.			*/

	ddesc.dsc$b_class	= DSC$K_CLASS_S;			/* It's a static string, ie it's immobile		*/
	ddesc.dsc$b_dtype	= DSC$K_DTYPE_T;			/* It's a text string					*/
	ddesc.dsc$w_length	= strlen(cdbg.output.dest);		/* How much of the string is occupied			*/
	ddesc.dsc$a_pointer	= cdbg.output.dest;			/* And this is where to find it.			*/

	stat = sys$brkthruw(	0,					/* No event flag					*/
				&tdesc,					/* Where to find the text to send			*/
				&ddesc,					/* Where to find the destination			*/
				BRK$C_DEVICE,				/* The send type; to a device				*/
				&iosb,					/* The i/o status buffer				*/
				13,					/* Carriage control					*/
				0,					/* No flags						*/
				0,					/* No class requestor					*/
				0,					/* Timeout (would be seconds)				*/
				0, 0 );					/* No ast or ast parameter				*/

	if ( cdbg.dbg == 'Y' )
		printf( "### Bcast result= %d %d\n", stat, iosb.status );

#else
	puts( text_line );						/* Print to line to the open file, plus an '\n'		*/
#endif
	return;								/*							*/
}

	/*==============================================================*/
	/*								*/
	/*	print_simple_line (Local)				*/
	/*								*/
	/*==============================================================*/

static void print_simple_line( char *text_line )			/* The text to output, zero-terminated			*/
{
extern	cdbg_nonvol_str			 cdbg;				/* Arrange access to main non-volatile storage struct.	*/
	char				*bpos;				/* Used to walk the text looking for NPC's		*/

	if ( text_line[ 0 ] != '\0' )					/* If this isn't a nul string we're printing,		*/
	{								/*							*/
	    for ( bpos = text_line; *bpos != '\0'; bpos++ )		/* Go through the buffer until you hit the terminator.	*/
	    {								/*							*/
		if ( isprint( (int)*bpos ) == 0 )			/* If the char at that location is not printable	*/
		    *bpos = '^';					/* turn it into a "^"					*/
	    };								/*							*/
	};								/*							*/

	switch( cdbg.output.media )					/* How we output this line depends on the media in use	*/
	{								/*							*/
	    CDBG_CASE_OUT_FILE						/* Output to a file....					*/
		print_simple_file_line( text_line );			/*							*/
		break;							/*							*/
									/*							*/
	    CDBG_CASE_OUT_SCREEN					/* Output to screen ('SYS$OUTPUT' or 'stdout' on Unix)	*/
		print_simple_screen_line( text_line );			/*							*/
		break;							/*							*/
									/*							*/
	    CDBG_CASE_OUT_BCAST						/* Output via broadcast					*/
		print_simple_broadcast_line( text_line );		/*							*/
		break;							/*							*/
									/*							*/
	    default:							/* Anything else, lose the output.			*/
		break;							/*							*/
	};								/*							*/

	return;								/*							*/

} /* End of internal void print_simple_line */

	/*==============================================================*/
	/*								*/
	/*	print_simple_debug (Local)				*/
	/*								*/
	/*==============================================================*/

static void print_simple_debug(	char			*in_pretext,	/* Text to put in before the main text.			*/
				char			*in_text )	/* The main text to print to the output media		*/
									/* Automatically prefixed with module and indented	*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
extern	char			 cdbg_line[ CDBG_OUTPUT_WIDTH + 1 ];	/* Centrally allocated buffer for composing output in	*/
	char			*next_free;				/* Keeps track of the next free buffer element		*/
	char			*prefix_end;				/* Start of changeable text, in case of wrap-around.	*/
	char			*next_print;				/* Walks along the in_text, as stuff gets output	*/
	int			 ival;					/* Short-term integer value holder			*/
	int			 indent;				/* The number of spaces between module name and ']'	*/
	int			 text_left;				/* The amount of text remaining. Used in wrap around.	*/
	int			 chunk_size;				/* The amount of text being printed in one shot.	*/
	int			 printable_width;			/* The number of columns between ']' and max line width	*/

	memcpy(	cdbg_line,						/* Start the line with the module name			*/
		cdbg.current_module,					/* taken from here					*/
		CDBG_MODULE_LEN );					/* and it's this long, already space padded.		*/
	next_free = ( cdbg_line + CDBG_MODULE_LEN );			/* Next free char is after the module name		*/ 

	if ( cdbg.gbl_opts.all.inl_set == 'Y' )				/* If there are any inline all-call options set,	*/
	{								/*							*/
	    do_pre_options( &cdbg.gbl_opts.all, &cdbg.gbl_opts.resrc,	/*							*/
				next_free, &ival );			/* Do the actions and build the text, putting it in the	*/
	    next_free += ival;						/* output line before the indent. Move the pointer on.	*/
	};								/*							*/

	indent = ( ( cdbg.nest_level * CDBG_INDENT_WIDTH ) + 1 );	/* Work out how far we're indenting this time around	*/
	memset( (void *)next_free, ' ', indent );			/* fill that many characters with spaces		*/
	next_free += indent;						/* and move next_free to the end of the cleared area.	*/
	next_free[ 0 ] = ']';						/* Round off the indent neatly				*/
	next_free[ 1 ] = ' ';						/* (No need to move next_free on, it's not used again.)	*/
	prefix_end = next_free + 2;					/* This is the start of the changeable text area	*/
	printable_width = ( CDBG_OUTPUT_WIDTH				/* Calculate the distance between the ']' and the	*/
				- ( prefix_end - cdbg_line ) );		/* wraparound column.					*/

	if ( NULL != ( next_print = in_pretext ) )			/* Initialise the pointer and length appropriately	*/
	    text_left = strlen( in_pretext );				/*							*/
	else								/*							*/
	    text_left = 0;						/*							*/
									/*							*/
	chunk_size = 0;							/*							*/

	while ( text_left > 0 )						/* Don't do this loop if the condition is zero on entry	*/
	{								/*							*/
	    if ( text_left <= printable_width )				/* If what's left will fit, prepare to fit it.		*/
		chunk_size = text_left;					/*							*/
	    else							/*							*/
		chunk_size = printable_width;				/* If not, prepare to fit what we can.			*/
	    next_free = prefix_end;					/* Whichever, prepare to put it in the right place.	*/
									/*							*/
	    memcpy( (void *)prefix_end,(void *)next_print,chunk_size );	/* Copy as much as we can into the output buffer	*/
									/*							*/
	    if ( 0 != ( text_left -= chunk_size ) )			/* Work out how much is left				*/
	    {								/* If there is some,					*/
	        prefix_end[ chunk_size ] = '\0';			/* make sure it's nul-terminated.			*/
		print_simple_line( cdbg_line );				/* print the full line we just filled			*/
		next_print += chunk_size;				/* move the pointer on to the end of the chunk.		*/
		prefix_end[-1] = '-';					/* And change the ']' to a ']-' to signal a wrap	*/
	    };								/*							*/
	};								/* End of while loop					*/

	next_print = in_text;						/* Start the print pointer at the beginning of the text	*/
	text_left = strlen( in_text );					/* and start with the whole width.			*/
	next_free = prefix_end + chunk_size;

	ival = printable_width - chunk_size;				/*							*/
	if ( text_left <= ival )					/* If what's left will fit, prepare to fit it.		*/
	    chunk_size = text_left;					/*							*/
	else								/*							*/
	    chunk_size = ival;						/* If not, prepare to fit what we can.			*/

	do								/* This loop must complete once; blank lines are OK.	*/
	{								/*							*/
	    memcpy( (void *)next_free,(void *)next_print,chunk_size );	/* Copy the chunk into the output buffer for printing	*/
	    next_free[ chunk_size ] = '\0';				/* and make sure it's nul-terminated.			*/
	    print_simple_line( cdbg_line );				/* Send the buffer contents to the output media.	*/
									/*							*/
	    text_left -= chunk_size;					/* Take the chunk size off the remaining text counter	*/
	    next_print += chunk_size;					/* and move the pointer on the same number of bytes.	*/
	    prefix_end[-1] = '-';					/* Change the prefix's ']' to ']-', to signal wrap.	*/
									/*							*/
	    if ( text_left <= printable_width )				/* If what's left will fit, prepare to fit it.		*/
		chunk_size = text_left;					/*							*/
	    else							/*							*/
		chunk_size = printable_width;				/* If not, prepare to fit what we can.			*/
									/*							*/
	    next_free = prefix_end;					/* Whichever, prepare to put it in the right place.	*/
									/*							*/
	} while ( text_left > 0 );					/* Do it again if there's anything left to print	*/

	if ( cdbg.gbl_opts.all.inl_set == 'Y' )				/* If there are any inline all-call options set,	*/
	    new_baseline( &cdbg.gbl_opts.all,&cdbg.gbl_opts.resrc );	/* Do any post-print operations that are required	*/

	return;								/*							*/

} /* End of local void print_simple_debug */

	/*==============================================================*/
	/*								*/
	/*	print_debug (Local)					*/
	/*								*/
	/*==============================================================*/

static void print_debug(	char			*in_text )	/* The main text to print to the output media		*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
	char			 opt_text[ CDBG_OPT_TEXT_LEN ];		/* Buffer for composing options result text into	*/
	int			 optext_len;				/* Short term integer storage				*/

	if ( cdbg.gbl_opts.prt.inl_set == 'Y' )				/* If there are any inline print-call options set,	*/
	{								/*							*/
	    do_line_options(	&cdbg.gbl_opts.prt,			/*							*/
				&cdbg.resrc[cdbg.nest_level],		/* Use the baselines from the module we're in.		*/
				opt_text, &optext_len );		/* Do the actions and build the text,			*/
	    print_simple_debug( opt_text, in_text );			/* and print the results as a prefix to the main text	*/
	    memset( (void *)opt_text, ' ', optext_len );		/* Replace the prefix text with spaces			*/
	    new_baseline( 	&cdbg.gbl_opts.prt,			/* Reset any baselines as required			*/
				&cdbg.resrc[cdbg.nest_level] );		/*							*/
	}								/*							*/
	else								/* If there are no print-call options,			*/
	{								/*							*/
	    print_simple_debug( NULL, in_text );			/* Print just the main text.				*/
	    optext_len = 0;						/* and setup for no prefix text				*/
	};								/*							*/

	if ( cdbg.gbl_opts.prt.ownl_set == 'Y' )			/* If there are any own-line print-call options set,	*/
	    do_ownline_options(	&cdbg.gbl_opts.prt );			/* Do the actions and print. CALLS PRINT_SIMPLE_DEBUG	*/

	return;
}

	/*==============================================================*/
	/*								*/
	/*	flush_debugs (Local)					*/
	/*								*/
	/*==============================================================*/

static void flush_debugs( void )					/* No options required					*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
	int			 discarded;				/* To take fsync's return value, which we discard.	*/

	if ( cdbg.dbg == 'Y' ) puts( "### Flushing." );			/* If local debugs are on, report where we are		*/

	switch( cdbg.output.media )					/* Only certain medias can be flushed.			*/
	{								/*							*/
	    CDBG_CASE_OUT_FILE						/* Output to a file....					*/
		discarded = fsync( fileno( cdbg.file_desc ) );		/* Flush the RMS buffer					*/
		break;							/*							*/
									/*							*/
	    CDBG_CASE_OUT_SCREEN					/* Output to screen ('SYS$OUTPUT' or 'stdout' on Unix)	*/
	    CDBG_CASE_OUT_BCAST						/* Output via broadcast					*/
	    default:							/* Or anything we don't know about,			*/
		if ( cdbg.dbg == 'Y' ) puts( "### N/A for media." );	/* If local debugs are on, report 'no action'		*/
		break;							/*							*/
	};								/* End of switch()					*/

	return;								/*							*/

} /* End of local void flush_debugs */

	/*==============================================================*/
	/*								*/
	/*	init_file_debugs (Local)				*/
	/*								*/
	/*==============================================================*/

static int init_file_debugs( char *in_filename )			/* The name of the file we're supposed to output to	*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
extern	char			 cdbg_line[ CDBG_OUTPUT_WIDTH + 1 ];	/* Centrally allocated buffer for composing output in	*/

	cdbg.file_desc = fopen( in_filename, "w" );			/* Create an output file and declare write access	*/

	if ( cdbg.file_desc != NULL )					/* If that worked,					*/
	{								/*							*/
	    if ( cdbg.dbg == 'Y' ) puts( "### File opened." );		/* If local debugs are on, report success		*/
	    return ( CDBG_SUCCESS );					/*    return success to say so				*/
	}								/*							*/
	else								/* otherwise						*/
	{								/*							*/
	    if ( cdbg.dbg == 'Y' ) puts( "### File open failed." );	/* If local debugs are on, report success		*/
	    return ( CDBG_FAILURE );					/*    return failure so the fallback gets triggered.	*/
	};

} /* End of local int init_file_debugs */

	/*==============================================================*/
	/*								*/
	/*	begin_module_debug (Local)				*/
	/*								*/
	/*==============================================================*/

static void begin_module_debug(	char	*in_data )			/* Describes the requested output method.		*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
	cdbg_start_data_str	 start_data;				/* Local safe storage for in_data on type 'B' calls	*/
	int			 optext_len;				/* To store the prefix length				*/
	int			 ival;					/* Short-term generic int value holder			*/
	char			*cptr;					/* Short-term generic pointer-to-char holder		*/
	time_t			 time_secs;				/* For retrieveing the time as a figure in seconds	*/
	char			 text_options[ CDBG_DEST_LEN ];		/* Local buffer to copy the options list into		*/
	char			 opt_text[ CDBG_OPT_TEXT_LEN ];		/* Buffer for composing options result text into	*/

	HN_dyn_to_fixed(	&start_data.media,			/* Make a local, formatted copy of in_data mapped as	*/
				CDBG_START_DATA_LEN,			/* a cdbg_start_data_str record.			*/
				in_data );				/*							*/

	cptr = (char *)memchr( start_data.dest, '/', CDBG_DEST_LEN );	/* Look for the first option's '/' (if any)		*/
						  			/*							*/
	if ( cptr != NULL )						/* If we found an option, we have to cut all options	*/
	{								/* out of the output description before we can use it.	*/
	    ival = ( CDBG_DEST_LEN					/* Work out how far from the '/' to the end of string,	*/
			- ( (int)cptr - (int)start_data.dest ) );	/* remainder = ( length - ( '/'addr - startaddr ) )	*/
	    memcpy( (void *)text_options, (void *)cptr, ival );		/* Copy that bit out to the local options buffer	*/
	    cptr[ 0 ] = '\0';						/* replace the '/' in the dest with a nul-terminator	*/
	    text_options[ ival + 1 ] = '\0';				/* and nul-terminate the text options string.		*/
	}								/*							*/
	else								/* Otherwise, there are no options qualifiers.		*/
	{								/*							*/
	    text_options[ 0 ] = text_options[ 1 ] = '\0';		/* Clear the first two chars of options[] for safety.	*/
	};								/*							*/

	if ( cdbg.debugs_active == 'Y' )				/* If this isn't the first call to HN_debugs,		*/
	{								/*							*/
	    ival = memcmp(	cdbg.current_module,			/* Now test to see if we're recursing into the		*/
				cdbg.initial_module,			/* initial module.					*/
				CDBG_MODULE_LEN );			/*							*/
	    if ( ival == 0 )						/* If so, increment the recursion count.		*/
		cdbg.recursion_level++;					/*							*/
									/*							*/
	}								/*							*/
	else
	{								/*							*/
	    cdbg.debugs_active = 'Y';					/* Remember HN_debugs are in use			*/
	    parse_option_list( text_options, &cdbg.gbl_opts );		/* Parse the text options into the global option flags	*/
	    if ( cdbg.dbg == 'Y' )					/* If HN_debugs' own debugs are on, list out the	*/
		printf( "### Destination is {%c}{%c}{%s}\n",		/* destination information we're working with.		*/
		    start_data.media,start_data.delim,start_data.dest);	/*							*/
									/*							*/
	    memcpy(	cdbg.initial_module,				/* Copy the current module name field into the		*/
			cdbg.current_module,				/* initializing module name's field.			*/
			CDBG_MODULE_LEN );				/*							*/
	    cdbg.output			= start_data;			/* Copy the destination description into nonvol storage	*/
	    cdbg.nest_level		= 0;				/* Initialize the module-nesting count			*/
	    cdbg.recursion_level	= 1;				/* and the 'recursion' count for the outer module	*/
									/*							*/
	    switch( cdbg.output.media )					/* The next call depends on the output media requested	*/
	    {								/*							*/
		CDBG_CASE_OUT_FILE					/* Output to a file....					*/
		    if ( cdbg.dbg == 'Y' )				/*							*/
			puts( "### File destination." );		/*							*/
		    ival = init_file_debugs( cdbg.output.dest );	/* This should contain a filename			*/
		    break;						/*							*/
									/*							*/
		CDBG_CASE_OUT_SCREEN					/* Output to screen ('SYS$OUTPUT' or 'stdout' on Unix)	*/
		    if ( cdbg.dbg == 'Y' )				/*							*/
			puts( "### Screen destination." );		/*							*/
		    ival = CDBG_SUCCESS;				/* No special startup required				*/
		    break;						/*							*/
									/*							*/
		CDBG_CASE_OUT_BCAST					/* Output via broadcast					*/
		    if ( cdbg.dbg == 'Y' )				/*							*/
			puts( "### Broadcast destination." );		/*							*/
		    ival = CDBG_SUCCESS;				/* No special startup required				*/
		    break;						/*							*/
									/*							*/
		default:						/* Unknown = force the fallback to trigger		*/
		    ival = CDBG_FAILURE;				/* ---------------------------------------		*/
		    break;						/*							*/
	    };								/*							*/
	    if ( ival != CDBG_SUCCESS )					/* Fallback on a standard filename if the specified	*/
	    {								/* output fails, or cannot be interpreted.		*/
		if ( cdbg.dbg == 'Y' ) puts( "Fallback" );		/* Debugs						*/
		init_file_debugs( CDBG_DEFAULT_FILENAME );		/*							*/
		cdbg.output.media = 'F';				/* Don't forget to override the given media type.	*/
		sprintf( cdbg_line, "(Couldn't use [%s])", in_data );	/* Build an error header into the output buffer		*/
		print_simple_line( cdbg_line );				/* Print an unbuggered line to the debug media		*/
	    };								/*							*/
	    time_secs = time( NULL );					/* Get the current time as a number of seconds		*/
	    cptr = ctime( &time_secs );					/* Reformat time for display, and get ctime's buff addr	*/
	    sprintf( cdbg_line, "HN_debugs %s: Started %s",		/* Build a header into the output buffer		*/
				IDENT, cptr );				/*	give the HN_debugs version no + the full time	*/
	    print_simple_line( cdbg_line );				/* Print an unbuggered line to the debug media		*/

	    if (   ( ival != CDBG_SUCCESS )				/* Drop a help message if the init failed. Ditto if the	*/
	        || (   ( *in_data != 'S' ) && ( *in_data != 's' )	/* original destination code was invalid or one of the	*/
	            && ( *in_data != 'F' ) && ( *in_data != 'f' )	/* backwards-compatibility ones.			*/
	            && ( *in_data != 'D' ) && ( *in_data != 'd' ) )  	/*							*/
		|| ( cdbg.help == 'Y' ) )				/* Or "/H" was specified on the command line.		*/
	    {								/* Print the help text					*/
		output_help_page();					/*							*/
	    };								/*							*/
									/* Remember what resources we've used up to now, so we	*/
	    resources_used( &cdbg.gbl_opts.resrc );			/* can keep pre-debug usage out of the figures.		*/
	    quotas_available( &cdbg.quotas );				/* Retrieve the process's quotas			*/
	};								/*							*/

	cdbg.resrc[cdbg.nest_level+1] = cdbg.gbl_opts.resrc;		/* Set the baseline figures to the global baseline	*/

	if ( cdbg.gbl_opts.beg.inl_set == 'Y' )				/* If there are inline begin-module-call options set,	*/
	{								/*							*/
	    new_baseline( 	&cdbg.gbl_opts.beg,			/* Do any post-print operations that are required	*/
				&cdbg.resrc[cdbg.nest_level+1] );	/*							*/
	    do_line_options(	&cdbg.gbl_opts.beg,			/*							*/
				&cdbg.resrc[cdbg.nest_level+1],		/*							*/
				opt_text, &optext_len );		/* Do the actions and build the text,			*/
	    print_simple_debug( NULL, opt_text );			/* and print the result text on the module intro line	*/
	}								/*							*/
	else								/* If there are no begin-module-call options,		*/
	{								/*							*/
	    print_simple_debug( NULL, "" );				/* Print a blank introductory line to the debug media	*/
	};								/*							*/
									/*							*/
	if ( cdbg.gbl_opts.beg.ownl_set == 'Y' )			/* If there are any own-line print-call options set,	*/
	    do_ownline_options(	&cdbg.gbl_opts.beg );			/* Do the actions and print. CALLS PRINT_SIMPLE_DEBUG	*/

	cdbg.nest_level++;						/* Crank up the nesting count now the intro's done.	*/

	return;								/*							*/

} /* End of local void begin_module_debug */

	/*==============================================================*/
	/*								*/
	/*	end_module_debug (Local)				*/
	/*								*/
	/*==============================================================*/

static void end_module_debug( char *in_data )
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
extern	char			 cdbg_line[ CDBG_OUTPUT_WIDTH + 1 ];	/* Centrally allocated buffer for composing output in	*/
	char			 opt_text[ CDBG_OPT_TEXT_LEN ];		/* Buffer for composing options result text into	*/
	char			*cptr;					/* Short-term generic pointer-to-char holder		*/
	int			 ival;					/* Short-term generic int value holder			*/
	int			 optext_len;				/* For storing the prefix length			*/

	if ( cdbg.gbl_opts.end.ownl_set == 'Y' )			/* If there are any own-line print-call options set,	*/
	    do_ownline_options(	&cdbg.gbl_opts.end );			/* Do the actions and print. CALLS PRINT_SIMPLE_DEBUG	*/
									/* NOTE: Done before the 'e' line, so it looks better.	*/

	if ( cdbg.nest_level >= 1 )					/* Decrement the nesting level, and therefore the	*/
	    cdbg.nest_level--;						/* indentation (guarding against accidental negatives)	*/

	if ( cdbg.gbl_opts.end.inl_set == 'Y' )				/* If there are inline begin-module-call options set,	*/
	{								/*							*/
	    do_line_options(	&cdbg.gbl_opts.end,			/*							*/
				&cdbg.resrc[cdbg.nest_level+1],		/* Use the baselines from the module we're leaving.	*/
				opt_text, &optext_len );		/* Do the actions and build the text,			*/
	    print_simple_debug( NULL, opt_text );			/* and print the result text on the module intro line	*/
	    memset( (void *)opt_text, ' ', optext_len );		/* Replace the prefix text with spaces			*/
	    new_baseline( 	&cdbg.gbl_opts.end,			/* Reset any baselines as required			*/
				&cdbg.resrc[cdbg.nest_level+1] );	/*							*/
	}								/*							*/
	else								/* If there are no begin-module-call options,		*/
	{								/*							*/
	    print_simple_debug( NULL, "" );				/* Print a blank farewell line to the debug media	*/
	};								/*							*/
									/*							*/
	ival = memcmp(	cdbg.current_module,				/* Now test to see if we're de-recursing from the	*/
			cdbg.initial_module,				/* initial module.					*/
			CDBG_MODULE_LEN );				/*							*/
	if ( ival == 0 )						/* If so,						*/
	    cdbg.recursion_level--;					/* decrement the recursion count			*/

	if ( cdbg.recursion_level == 0 )	    	    		/* If it's zero we're out of the program. Shutdown time	*/
	{								/*							*/
	    sprintf( cdbg_line, "HN_debugs: inactive." );		/* Build a header into the output buffer		*/
	    cdbg.debugs_active = ' ';					/* Remember HN_debugs are no longer in use		*/
	    print_simple_line( cdbg_line );				/* Print an unbuggered line to the debug media		*/
	    switch( cdbg.output.media )					/* How we output this line depends on the media in use	*/
	    {								/*							*/
		CDBG_CASE_OUT_FILE					/* Output to a file....					*/
		    fclose( cdbg.file_desc );				/* Close the file					*/
		    break;						/*							*/
									/*							*/
		CDBG_CASE_OUT_SCREEN					/* Output to screen ('SYS$OUTPUT' or 'stdout' on Unix)	*/
		    break;						/* No shutdown procedure required			*/
									/*							*/
		CDBG_CASE_OUT_BCAST					/* Output via broadcast					*/
		    break;						/* No shutdown procedure required			*/
									/*							*/
		default:						/*							*/
		    break;						/*							*/
	    };								/* End of switch()					*/
	};								/*							*/

	return;
}

	/*==============================================================*/
	/*								*/
	/*	ENTRY POINT (EXTERNALLY CALLABLE) FUNCTIONS		*/
	/*								*/
	/*==============================================================*/

	/*==============================================================*/
	/*								*/
	/*	HN_debugs	char	*in_module,			*/
	/*			char	 in_action,			*/
	/*			char	*in_data )			*/
	/*								*/
	/*==============================================================*/

void HN_debugs(			char	*in_module,			/* The name of the calling module			*/
				char	 in_action,			/* Action for HN_debugs to perform			*/
				char	*in_data )			/* Any data required by HN_debugs to do the action	*/
{
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/

	cdbg.ownl_show_done = cdbg.ownl_watch_done = 'N';		/* This is used to stop multiple ownline option calls	*/

	if ( ( in_module != NULL ) && ( strlen( in_module ) > 0 ) )	/* If we have an incoming module name,			*/
	{								/*							*/
	    HN_dyn_to_fixed(	cdbg.current_module,			/* put the module name in the current slot,		*/
				CDBG_MODULE_LEN,			/* space padding it to this length so the output will	*/
				in_module );				/* be nice and neat.					*/
	};								/*							*/

	switch( in_action )						/* Farm out the operation to the appropriate function	*/
	{								/*							*/
	    case 'B':							/* B = Begin debugs for a module			*/
	    case 'b':							/* -----------------------------			*/
		begin_module_debug( in_data );				/* Startup debugs if necessary, and init for module.	*/
		break;							/*							*/
									/*							*/
	    case 'P':							/* P = Print a debug string				*/
	    case 'p':							/* ------------------------				*/
		print_debug( in_data );					/* 							*/
		break;							/*							*/
									/*							*/
	    case 'O':							/* O = Print a debug string with one-line indent	*/
	    case 'o':							/* ---------------------------------------------	*/
		cdbg.nest_level++;					/* Crank up the nesting count temporarily.		*/
		print_debug( in_data );					/* 							*/
		cdbg.nest_level--;					/* And back down now we're done.			*/
		break;							/*							*/
									/*							*/
	    case 'F':							/* F = Flush the debug buffer (if applicable to media)	*/
	    case 'f':							/* --------------------------				*/
		print_debug( in_data );					/*							*/
		cdbg.flush_on_exit = 'Y';				/* Set the flag for a flush before exiting.		*/
		break;							/*							*/
									/*							*/
	    case 'E':							/* E = End debugs for a module				*/
	    case 'e':							/* ---------------------------				*/
		end_module_debug( in_data );				/* Decrease indent and shutdown debugs if necessary.	*/
		break;							/*							*/
									/*							*/
	};								/* End of switch( in_action )				*/

	if ( cdbg.debugs_active == 'Y' )				/* Providing we didn't just shut down the debugs,	*/
	{								/* do any applicable after-call options			*/
	    if ( cdbg.gbl_opts.all.ownl_set == 'Y' )			/* If we're supposed to do any ownline opts every line,	*/
		do_ownline_options( &cdbg.gbl_opts.all );		/* Do the actions and print. CALLS PRINT_SIMPLE_DEBUG	*/
	    if ( cdbg.flush_on_exit == 'Y' )				/* Flush any buffers as required by the media in use.	*/
	    {								/*							*/
		flush_debugs( );					/*							*/
		cdbg.flush_on_exit = 'N';				/*							*/
	    };								/*							*/
	};								/*							*/

	return;								/* Return to sender (she wrote upon it, address unknown)*/

} /* End of entry point void HN_debugs */

	/*==============================================================*/
	/*	ENTRY POINT FUNCTION					*/
	/*								*/
	/*	HN_buf_debugs(	char	*in_module,			*/
	/*			int	 text_length,			*/
	/*			char	*text )				*/
	/*								*/
	/* This externally calleable function allows you to compose a	*/
	/* line of debug output before printing it. With each call to	*/
	/* the routine, *text is appended to the debug buffer. The	*/
	/* buffer is only printed and flushed when an in_caller_name	*/
	/* is provided. For example;					*/
	/*								*/
	/*   HN_buf_debugs( "", 5, "hello" ); Appends "hello" to	*/
	/*   whatever is in the buffer, but does not cause any output.	*/
	/*								*/
	/*   HN_buf_debugs( "test", 5, "hello" ); Appends "hello" to	*/
	/*   whatever is in the buffer and sends the buffer contents to	*/
	/*   the current debug destination. The buffer is then reset	*/
	/*   ready to start again.					*/
	/*								*/
	/* If text is a nul terminated dynamic string, text_length may	*/
	/* be left zero. That will cause this function to work out its	*/
	/* length via strlen().						*/
	/*								*/
	/* NOTE: The debug buffer is 200 characters long, but you are	*/
	/* advised to keep each line under about 120. This function	*/
	/* is protected against overflows causing damage, but there's	*/
	/* no guards against losing data or messed-up output.		*/
	/*==============================================================*/

void HN_buf_debugs(		char	*in_module,			/* The name of the calling module (JOBNAME$ in basic)	*/
				int	 in_text_length,		/* The length of the string to add to the buffer	*/
				char	*in_text )			/* The string to add to the end of the buffer.		*/
{
static	char		cdbg_buf[ CDBG_BUFFER_WIDTH + 1 ];		/* A shared fixed-string buffer for debugs		*/
static	int		cdbg_buf_free;					/* A non-vol index to the next free byte of the buffer	*/
	int		ival;						/* Used as index in a loop which suppresses NPC's	*/

	if ( in_text_length == 0 ) in_text_length = strlen( in_text );	/* If zero length given, it's dynamic; use its STRLEN	*/

	if ( cdbg_buf_free + in_text_length >= CDBG_BUFFER_WIDTH )	/* If the length (given or strlen'd) would give buffer	*/
	    in_text_length = ( CDBG_BUFFER_WIDTH - cdbg_buf_free );	/* overflow, truncate it.				*/

	if ( in_text_length > 0 )					/* If we STILL have stuff to add to the buffer,		*/
	{								/*							*/
	    memcpy( &cdbg_buf[cdbg_buf_free],in_text,in_text_length );	/* Copy the specified number of chars into the buffer	*/
	    for (	ival = cdbg_buf_free;				/* From the start of the updated bit			*/
			ival <= cdbg_buf_free + in_text_length;		/* Until the end of the updated bit			*/
			ival++ )					/* Moving in steps of one				*/
	    {								/*							*/
		if ( cdbg_buf[ ival ] == '\0' )				/* If the char at that location is a nul,		*/
		    cdbg_buf[ ival ] = '.';				/* turn it into a dot					*/
	    };								/*							*/
	    cdbg_buf_free += in_text_length;				/* move our index on to the new next free byte		*/
	};								/*							*/

	if ( ( in_module != NULL ) && ( strlen( in_module ) != 0 ) )	/* If there's a name, this is a print-and-buffer-flush;	*/
	{								/*							*/
	    cdbg_buf[ cdbg_buf_free ] = 0;				/* Make sure there's a null terminator.			*/
	    HN_debugs( in_module, 'P', cdbg_buf );			/* Pass the results to HN_debugs for output		*/
	    cdbg_buf_free = 0;						/* reset the 'next free element' index.			*/
	};	    

	return;

} /* End of HN_buf_debugs */

	/*==============================================================*/
	/*	ENTRY POINT FUNCTION					*/
	/*								*/
	/*	HN_buf_int_debugs(					*/
	/*			char	*in_module,			*/
	/*			int	 in_integer )			*/
	/*								*/
	/* This function works in conjunction with HN_buf_debugs()	*/
	/* (see above). It takes an integer value, and tacks it onto	*/
	/* the HN_buf_debugs buffer in displayable format.		*/
	/*								*/
	/* Please note: since we fill the local buffer from right to	*/
	/* left, and we want a pointer to the leftmost occupied slot	*/
	/* we start at the right and decrement the pointer BEFORE we	*/
	/* fill each slot.						*/
	/*==============================================================*/

void HN_buf_int_debugs( char	*in_module,				/* The name of the calling module (JOBNAME$ in basic)	*/
			 int	 in_integer )				/* The int value to be put (displayably) in the buffer	*/
{
	char		 sign_char;					/* A local char to hold any sign until we need it	*/
	char		 itoc_buf[ 12 ];				/* A local buffer to build the ascii representation in	*/
	int		 itoc_free = 12;				/* We move backwards through the buffer...		*/

	itoc_buf[ 12 ] = '\0';						/* Make sure the last char is a nul terminator		*/

	if ( in_integer < 0 ) 						/* If the integer is negative,				*/
	{								/* we need a '-' sign added to the buffer, but we don't	*/
	    sign_char = '-';						/* know where yet so store it for the moment.		*/
	    in_integer = abs( in_integer );				/* Make the number positive to make the next bit easier	*/
	}								/*							*/
	else
	    sign_char = ' ';						/* It's not negative, so we don't want a sign		*/

	if ( in_integer == 0 )						/* If the number IS zero,				*/
	{								/*							*/
	    itoc_buf[ --itoc_free ] = '0';				/* whack in a zero, move the buffer pointer		*/
	}								/*							*/
	else
	{								/* It's a positive, non-zero number.			*/
	    while ( in_integer > 0 )					/*							*/
	    {								/*							*/
		itoc_buf[ --itoc_free ] =				/* Fill the buffer element				*/
				(char)( '0' + ( in_integer % 10 ));	/*	with the digit representing int mod 10		*/
		in_integer = (int)( in_integer / 10 );			/* Then divide the integer by ten, with truncation	*/
	    };								/* Keep going until we run out of integer		*/
	};								/*							*/

	if ( sign_char != ' ' )						/* If we've a sign to add, add it to the buffer		*/
	    itoc_buf[ --itoc_free ] = sign_char;			/*							*/

	HN_buf_debugs( in_module, 0, &itoc_buf[ itoc_free ] );		/* Add the display number to the buffer via the central	*/
									/* buffer update routine.				*/
	return;

} /* End of void HN_buf_int_debugs */

	/*==============================================================*/
	/*	Function to compare two module names, for use		*/
	/*	as a callback from the generic tree routines.		*/
	/*==============================================================*/

static int	compare_modules( void *ent, void *ex )
{
	int result;

	if ( cdbg.dbg == 'Y' )
		printf( "### ---Tree--- Comparing [%.16s] and [%.16s]\n", (char*)ent, (char*)ex );

	result = strncmp( (char*)ent, (char*)ex, CDBG_MODULE_LEN );
	if ( result < 0 ) result = -1;
	else if ( result > 0 ) result = 1;

	return ( result );
}

	/*==============================================================*/
	/*	Function to compare two module names, for use		*/
	/*	as a callback from the generic tree routines.		*/
	/*==============================================================*/

static void	print_module( void *ent, void *unused )
{
	printf( "   [%.16s][%s]\n", (char*)ent, (char*)ent+CDBG_MODULE_LEN );
}

	/*==============================================================*/
	/*	Function to destroy a node of the module tree		*/
	/*==============================================================*/

static void	kill_module( void *ent )
{
	free( ent );
}

	/*==============================================================*/
	/*								*/
	/*	add_module_to_tree					*/
	/*								*/
	/*==============================================================*/

static void add_module_to_tree( char *module, char *destination )
{
extern	char			 cdbg_global_debugs_dest[CDBG_DEST_LEN];/* If this gets filled, it's used for all modules	*/
extern	struct HN_tree_root	 cdbg_module_root;			/* Root for a tree of module dests for HN_check_debug	*/
extern	cdbg_nonvol_str		 cdbg;					/* Arrange access to main non-volatile storage struct.	*/
	char			*new_entry;				/* A pointer used in reserving space for a new entry	*/
	int			 result;				/* Takes the return value from 'add to tree'		*/

	if ( cdbg.dbg == 'Y' )						/* To program is human, to add debugs devine...		*/
	    printf("### ---Tree--- Entry as given [%s][%s]\n",		/*							*/
			module, destination );				/*							*/

	if ( destination[0] == ' ' )					/* Discard any spaces before the destination		*/ 
	    do { destination++; } while ( destination[0] == ' ' );	/*							*/

	new_entry=(char*)malloc(strlen(destination)+CDBG_MODULE_LEN+1);	/* Then reserve enough space for module and destination	*/

	HN_dyn_to_u_fixed( new_entry, CDBG_MODULE_LEN, module );	/* Copy in, uppercase and space pad the module name	*/
	strcpy( new_entry + CDBG_MODULE_LEN, destination );		/* Tack the destination on after, including terminator	*/

	if ( cdbg.dbg == 'Y' )						/*							*/
	    printf("### ---Tree--- Cleaned up entry [%.16s][%s]\n",	/*							*/
			new_entry, &new_entry[CDBG_MODULE_LEN] );	/*							*/

	if ( ! strncmp( new_entry, "HN_DEBUGS", 9 ) )			/* If it's asking for HN_DEBUGS debugs, switch them on	*/
	{								/*							*/ 
	    cdbg.dbg = 'Y';						/*							*/
	    puts( "### Houston, we have debug debugs." );		/*							*/
	    free( new_entry );						/* Dispose of the now unwanted reserved memory		*/
	}								/*							*/  
	else								/*							*/ 
	if ( ! strncmp( new_entry, "GLOBAL", 6 ) )			/* If the module name is the special value 'GLOBAL',	*/
	{								/*							*/
	    strcpy( cdbg_global_debugs_dest, destination );		/* ...make this the global destination			*/
	    free( new_entry );						/* Dispose of the now unwanted reserved memory		*/
	    if ( cdbg.dbg == 'Y' )					/*							*/
		printf( "### Global destination imposed [%s]\n",	/*							*/
			cdbg_global_debugs_dest );			/*							*/
	}								/*							*/ 
	else								/*							*/
	{								/*							*/
	    if ( cdbg.dbg == 'Y' )					/*							*/
		printf("### ---Tree--- Adding to tree [%.16s][%s]\n",	/*							*/
			new_entry, &new_entry[CDBG_MODULE_LEN] );	/*							*/
	    result = HN_tree_add_entry( &cdbg_module_root,new_entry );	/* For a normal module, just whack it on the tree.	*/
	};								/*							*/
}

	/*==============================================================*/
	/*								*/
	/*	read_debug_dest_file					*/
	/*								*/
	/* Read a named debug settings file, and put its definitions	*/
	/* on the saved settings tree.					*/
	/*								*/
	/*==============================================================*/

static void read_debug_dest_file( char *filename )
{
extern	struct HN_tree_root	cdbg_module_root;			/* Root for a tree of module dests for HN_check_debug	*/

	FILE			*dest_file;				/* For opening the debug destinations file on		*/
	char	 		 dest[150];				/* A buffer for the lines from the file			*/
	char			*line_terminators = "! \n\0";		/* defines comment delims and line terminators 		*/
	char			*scan_ptr;				/* Used in finding the end of a destination		*/
	char			*destin;
	char			*module;

	HN_tree_init_root(	&cdbg_module_root,			/* Init this tree root					*/
				"Module debug cache",			/* using this name					*/
				compare_modules,			/* this routine to compare entries			*/
				print_module,				/* this one to print an entry				*/
				kill_module,				/* and this one to destroy it.				*/
				0 );					/* And no, we don't do duplicates			*/
	    
	if ( ! ( dest_file = fopen( filename, "r" ) ) )			/* Open the file readonly, and if it fails...		*/
	{								/*							*/
	    if ( cdbg.dbg == 'Y' )					/*							*/
		printf( "Unable to open %s\n", filename );		/* Report the problem					*/
	    return;							/* and assume there's no file, ergo no debug defs.	*/
	};								/*							*/

	do								/* Loop through reading lines from the file		*/
	{								/*							*/
	    if ( fgets( dest, 150, dest_file ) == NULL ) break;		/* If we've got an end of file back exit the loop	*/
	    if ( dest[0] == '%' ) break;				/* Allow % on line to terminate file. Added for	AIX.	*/
									/*							*/
	    scan_ptr = strpbrk( dest, "\n\r!" );			/* Find the first line candidate			*/
	    if ( scan_ptr != NULL ) *scan_ptr = '\0';			/* Make it a nul, to give a clean username.		*/
									/*							*/
	    module = strtok( dest, "=" );				/* Format is 'module = destination', so split the line	*/
	    destin = strtok( NULL, "\n" );				/* by inserting nuls and get pointers to the chunks.	*/
									/*							*/
	    if ( module != NULL )					/* If we've got something to work with, store it.	*/
	    {								/*							*/
		if ( destin == NULL ) destin = "\0";			/* If there was no destination on the line, dummy it.	*/
		add_module_to_tree( module, destin );			/* Add the module and its output spec to the tree	*/
	    };								/*							*
									/*							*/
	} while (-1);							/* Infinite loop until the break triggers		*/
	    
	if ( cdbg.dbg == 'Y' )						/*							*/
	    puts( "### Finished reading file." );			/*							*/
}

	/*==============================================================*/
	/*	ENTRY POINT FUNCTION					*/
	/*								*/
	/*	dbg = HN_check_debugs(					*/
	/*			char	*in_module,			*/
	/*			char	*out_destination )		*/
	/*								*/
	/* This function decides whether debugs are required or not.	*/
	/* Its return value is -1 for debugs on, and 0 for debugs off.	*/
	/* It also returns the output destination for the calling	*/
	/* program to use when starting the debugs up.			*/
	/*								*/
	/* NOTE: out_destination should be a pointer to an array of	*/
	/* char at least [CDBG_DEST_LEN] characters long, reserved by	*/
	/* the calling program.						*/
	/*								*/
	/* Simple example (not for use in performance-critical stuff):	*/
	/*								*/
	/*	int  dbg = 0;						*/
	/*	char dbg_dest[CDBG_DEST_LEN];				*/
	/*								*/
	/*	dbg = cst_check_debugs( jobname, dbg_dest );		*/
	/*	if ( dbg == -1 )					*/
	/*	    HN_debugs( jobname, 'B', dbg_dest );		*/
	/*								*/
	/*==============================================================*/

int HN_check_debugs(	char	*in_module,				/* The jobname of the calling module			*/
			char	*out_destination )			/* The returned output specification, if available.	*/
{
extern	char			 cdbg_global_debugs_dest[CDBG_DEST_LEN];/* If this gets filled, it's used for all modules	*/
extern	struct HN_tree_root	 cdbg_module_root;			/* Root for a tree of module dests for HN_check_debug	*/
extern	int			 cdbg_module_root_initd;		/* Flag for remembering whether we init'd it yet	*/
	char			 local_module[ CDBG_MODULE_LEN ];	/* Workspace for building a clean module name in	*/
	char			*found_module;				/* Pointer forfinding a module on the tree		*/
	int			 dupes;					/* Discarded 'no of duplicates' count			*/
	int			 result;				/* Returned flag for if hte entry was on the tree.	*/

	if ( cdbg_module_root_initd != 1 )				/* If our historical tree isn't init'd yet, do it now.	*/
	{								/*							*/
	    cdbg_module_root_initd = 1;					/* Remember we've done this bit now.			*/
	    read_debug_dest_file( "HN_DEBUGS.DAT" );			/* Load it initially from the master debug datafile	*/
	    if ( cdbg.dbg == 'Y' )					/*							*/
		HN_tree_print( &cdbg_module_root, 1 );			/*							*/
	};								/*							*/

	if ( cdbg.dbg == 'Y' )						/*							*/
	    printf( "### Check debugs for [%s]\n", in_module );		/*							*/

	if ( cdbg_global_debugs_dest[0] != '\0' )			/* If there's a global destination set, use it.		*/
	{								/*							*/
	    if ( cdbg.dbg == 'Y' )					/*							*/
		puts( "### Global destination active." );		/*							*/
	    strcpy( out_destination, cdbg_global_debugs_dest );		/*							*/
	}								/*							*/
	else								/* Otherwise look for a local one.			*/
	{								/*							*/
	    HN_dyn_to_u_fixed(local_module,CDBG_MODULE_LEN,in_module);	/* Copy in, uppercase and space pad the module name	*/

	    found_module = (char*)HN_tree_find_entry(			/* See if we've encountered this module before.		*/
						&cdbg_module_root,	/* Search the tree anchored by this root		*/
						local_module,		/* for this module					*/
						&dupes,			/* return the number of duplicates here,		*/
						&result );		/* and the found (0)/not found (-1) flag here.		*/

	    if ( result == 0 )						/* If it's found,					*/
	    {								/*							*/
		if ( cdbg.dbg == 'Y' )					/*							*/
		    printf( "### Found on tree: [%s]\n",found_module );	/*							*/
		strcpy(out_destination,&found_module[CDBG_MODULE_LEN]);	/* Copy the destination part into the return buffer	*/
	    }								/*							*/
	    else							/*							*/
	    {								/*							*/
		add_module_to_tree( in_module, "" );			/* Add the module and a blank output spec to the tree	*/
		strcpy( out_destination, "" );				/* Copy a blank destination into the return buffer	*/
	    };								/*							*/
	};

	if ( out_destination[0] == '\0' )				/* If there's no destination, debugs aren't on for	*/
	{								/* the module. Return 0 for 'no translation, no debugs'	*/
	    if ( cdbg.dbg == 'Y' )					/*							*/
		puts( "### Null destination; don't debug." );		/*							*/
	    return ( 0 );						/*							*/
	}								/*							*/
	else								/*							*/
	{								/*							*/
	    if ( cdbg.dbg == 'Y' )					/*							*/
		printf( "### Dest [%s](%i)(%i) found; debugs on.\n",	/*							*/
			out_destination, strlen( out_destination ),	/*							*/
			(int)out_destination[0] );			/*							*/
	    return ( -1 );						/* If there _is_ one, debugs are on.			*/
	}								/*							*/

} /* End of int HN_check_debugs */


#ifdef DEBUG
	/*==============================================================*/
	/*								*/
	/*	Test routine. If you want to test HN_debugs,		*/
	/*	compile with /DEFINE=DEBUG to include the		*/
	/*	following mainline.					*/
	/*								*/
	/*==============================================================*/

int main()
{
#	define	jobname "TEST_PROG"
#	define	job_two "INNER_JOB"
	int	dbg = 0;
	char	out_dest[CDBG_DEST_LEN];

	dbg = HN_check_debugs( jobname, out_dest );

	if ( dbg )
	{
	    HN_debugs( jobname, 'B', out_dest );
	    HN_debugs( jobname, 'P', "First output line." );
	    HN_debugs( jobname, 'P', "Second output line." );
	    HN_buf_debugs( "", 0, "First part of buffered line ->" );
	    HN_buf_debugs( jobname, 0, "<- Second part of buffered line" );
	    HN_debugs( jobname, 'P', "Another output line." );
	    HN_debugs( job_two, 'B', out_dest );
	    HN_debugs( job_two, 'P', "In second layer." );
	    HN_buf_debugs( "", 0, "Let's try an integer ->" );
	    HN_buf_int_debugs( "", (int)10101 );
	    HN_buf_debugs( job_two, 0, "<-" );
	    HN_debugs( job_two, 'P', "Leaving second layer." );
	    HN_debugs( job_two, 'E', "" );
	    HN_debugs( jobname, 'E', "" );
	}
	
}
#endif
