#ifndef __HN_DEBUGS
#define __HN_DEBUGS

#include <stdio.h>							/* Pull in defs for standard c I/O functions		*/
#include <time.h>							/* 	for time handling functions			*/

	/*==============================================================*/
	/*								*/
	/*	SUITE	:	"C" service routines			*/
	/*	PROGRAM	:	HN_DEBUGS.H				*/
	/*	TITLE	:	"C" debug output routines definitions	*/
	/*	AUTHOR	:	Shane Smith				*/
	/*								*/
	/*==============================================================*/

#define CDBG_TIMESTAMP_FLAG	1					/* Identifies a 'timestamp required' flag		*/
#define CDBG_DATESTAMP_FLAG	2					/* Identifies a 'datestamp required' flag		*/

#define CDBG_SUCCESS		0					/* Internal success status code				*/
#define CDBG_FAILURE		-1					/* Internal failure status code				*/

#define CDBG_DEFAULT_FILENAME	"FALLBACK.DBG"				/* The filespec used when no other is given		*/
#define CDBG_OUTPUT_WIDTH	132					/* The maximum output line width			*/
#define CDBG_BUFFER_WIDTH	500					/* The maximum width of a buffered debug can be		*/
#define CDBG_INDENT_WIDTH	2					/* The number of spaces to indent for one nesting level	*/
#define CDBG_OPT_TEXT_LEN	80					/* Length of longest likely options result string.	*/
#define CDBG_MAX_NEST		50					/* Maximum nesting level expected			*/
#define CDBG_QUOTA_WARN_PRECENT 10					/* How low a quota has to go to ring alarms		*/
#define CDBG_MODULE_LEN 	16					/* The maximum stored length of a module name		*/
/*#define CDBG_DEST_LEN		126*/					/* Max destination length (now defined in HN_defs.h)	*/
#define CDBG_START_DATA_LEN	128					/* Length of the whole start_data structure		*/

#define CDBG_CASE_OUT_SCREEN	case 'S': case 's': case 'P': case 'p':	/* The CASE condition for identifying screen output	*/
#define CDBG_CASE_OUT_FILE	case 'F': case 'f':			/* The CASE condition for file output			*/
#define CDBG_CASE_OUT_BCAST	case 'D': case 'd':			/* The CASE condition for broadcast output (VMS only)	*/


typedef struct								/* Struct for resources used statistics			*/
{									/* ------------------------------------			*/
	int		 cpu;						/* Cpu usage, in ticks					*/
	int		 io;						/* Combined Direct and Buffered IO so far		*/
	int		 pgf;						/* Pagefaults						*/
	time_t		 secs;						/* Seconds elapsed					*/
									/*							*/
} cdbg_resrc_usage_str;							/*							*/

typedef struct								/* Struct for VMS quota figures				*/
{									/* ----------------------------				*/
	int		bio;						/* Buffered I/O count limit				*/
	int		dio;						/* Direct I/O count limit				*/
	int		enq;						/* Enqueue limit (for locks)				*/
	int		fil;						/* File open limit					*/
	int		ast;						/* Active AST limit					*/
	int		pgfl;						/* Page file usage limit				*/
} vms_quotas_str;							/*							*/

typedef struct								/* Struct for creating VMS item lists 			*/
{									/* ----------------------------------			*/
	short		 buflen;					/* Word: length of available return buffer.		*/
	short		 itmcod;					/* Word: item code for operation to perform.		*/
	void		*buffer;					/* Pointer to whatever: Address of return buffer.	*/
	int		*retbuf;					/* Pointer to int: Address to put length of ret'd data.	*/
									/*							*/
} vms_list_item_str;							/*							*/

typedef struct								/* Digital VMS I/O Status Block, used in system calls	*/
{									/*							*/
	short		status;						/* Success/Error code for the IO operation		*/
	short		byte_count;					/* Number of bytes returned				*/
	long		dev_specific_info;				/* Contents depend on what the call was doing		*/
} vms_iosb_str;								/*							*/

typedef struct								/* The structure of an event options record		*/
{									/* ----------------------------------------		*/
	char			 inl_set;				/* Y  = At least one inline option is set		*/
	char			 timestamp;				/* Y  = Timestamps required				*/
	char			 autoflush;				/* Y  = Autoflush required				*/
	char			 showresrc;				/* Y  = Show resources used				*/
	char			 peg_figs;				/* Y  = Peg baseline figures (resources, times etc)	*/
									/*							*/
	char			 ownl_set;				/* Y  = At least one own-line option is set		*/
	char			 show_quo;				/* Y  = Print remaining quotas				*/
	char			 watch_quo;				/* Y  = Watch for low quotas				*/
									/*							*/
} cdbg_event_options_str;						/*							*/

typedef struct								/* The structure of an options anchor record		*/
{									/* -----------------------------------------		*/
	char			 module[ CDBG_MODULE_LEN ];		/*							*/
	cdbg_event_options_str	 all;					/* Events applying to all calls				*/
	cdbg_event_options_str	 beg;					/* Events applying to "B" action calls			*/
	cdbg_event_options_str	 prt;					/* Events applying to "P" action calls			*/
	cdbg_event_options_str	 end;					/* Events applying to "E" action calls			*/
	cdbg_resrc_usage_str	 resrc;					/* Resource baseline for this opt record's nest level	*/
	void			*pre;					/* Place-holder pointer, for later expansion.		*/
	void			*post;					/* Place-holder pointer, for later expansion.		*/
									/*							*/
} cdbg_options_str;							/*							*/

typedef struct								/* The structure of the data for "B"egin debugs		*/
{									/* --------------------------------------------		*/
	char			 media;					/* What output media to use, eg "F" for file		*/
	char			 delim;					/* Any delimiter, personal preference.			*/
	char			 dest[ CDBG_DEST_LEN ];			/* Any data required by the media, eg the filename	*/
									/*							*/
} cdbg_start_data_str;							/*							*/

typedef struct								/* The structure holding internal, non-volatile data	*/
{									/* -------------------------------------------------	*/
	unsigned int		 nest_level;				/* How many layers of modules deep we are.		*/
	unsigned int		 recursion_level;			/* How many of those layers are the initial module.	*/
	char			 dbg;					/* Set to 'Y' if HN_debugs debugs are on		*/
	char			 help;					/* Set to 'Y' if "/H" used. Only works on first call.	*/
	char			 flush_on_exit;				/* Set to 'Y' if flush required on leaving HN_debugs	*/
	char			 debugs_active;				/* Has debug output already started (Y) or not (N)	*/
	char			 initial_module[ CDBG_MODULE_LEN ];	/* The name of the outermost debug-generating module	*/
	char			 current_module[ CDBG_MODULE_LEN ];	/* The name of the innermost debug-generating module	*/
	cdbg_start_data_str	 output;				/* Description of current output destination		*/
	vms_quotas_str		 quotas;				/* Process quotas					*/
	cdbg_resrc_usage_str	 resrc[ CDBG_MAX_NEST ];		/* Stores resource usage by nesting level.		*/
	FILE			*file_desc;				/* Current output file channel no (for out type 'F')	*/
									/*							*/
	cdbg_options_str	 gbl_opts;				/* Options to use on all calls				*/
	char			 ownl_show_done;			/* Set to 'Y' if ownline options already actioned	*/
	char			 ownl_watch_done;			/* Set to 'Y' if ownline options already actioned	*/
									/*							*/
} cdbg_nonvol_str;							/*							*/

#endif
