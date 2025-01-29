/*
 This program, and all the routines referenced herein, are the 
 proprietary properties and trade secrets of HEALTH NET,
 Except as provided for by license agreement, this program 
 shall not be duplicated, used or disclosed without written  
 consent, signed by an officer of HEALTH NET, INC.
 
 Start Documentation
 
	SYSTEM:  Health Net Utilities
	PROGRAM: sys_array_desc.c
	TITLE:   HN_DATE_CONVERSION
	DATE:    12/23/97
	AUTHOR:  SFS 
 
	OVERVIEW:

	This function takes information on the shape of an array of fixed
	length strings, and returns a descriptor for the array which can
	be used by either VAX BASIC or the Alpha's DEC BASIC as appropriate.

	It is used to pass COBOL arrays to BASIC subroutines, which is
	otherwise not an easy task.

	Unfortunately, VAX BASIC and DEC BASIC use two different descriptor
	classes for passing string arrays, which is what caused this
	routine to be written. C's convenient conditional compilation
	clause and its pre-defined __ALPHA compiler constant make it really
	easy to build both VAX specific and Alpha specific code from the
	same source.

  CALLING STRUCTURE:

	SYS_ARRAY_DESC(	LONG BY REF,	Number of elements in array
			WORD BY REF,	Length of each element
			LONG BY VALUE,	Address of the first element
			DESC BY REF )	Address of the buffer to put the
					descriptor in.

  WARNINGS:

	This function makes the following assumptions:

	1) The array is of fixed length strings.
	2) The array only has one dimension (ie it's a list, not a table).
	3) The first array element is one, as per COBOL standard practice.

  End Documentation
*/

#include "inc:hn_c_helpers.h"	/* Our debug output routines		*/
#include <descrip.h>		/* DEC's descriptor declarations	*/

	struct	bounds_block	/* And a bit DEC don't provide		*/
	{
	    long			 dsc$l_l;
	    long			 dsc$l_u;
	};

#if defined(__ALPHA) || defined(__ia64)	/* If we're on an Alpha, do this:	*/
	struct	strides_block	/* Another bit missing from descrip.h	*/
	{
	    char			 *dsc$a_a0;
	    unsigned long		 dsc$l_s;
	};

	typedef struct		/* Bolt the three together for use	*/
	{
	    struct dsc$descriptor_nca	 main;
	    struct strides_block	 stride;
	    struct bounds_block		 bounds;
	} array_desc;
#else

	struct	multi_block	/* Another bit missing from descrip.h	*/
	{
	    char			*dsc$a_a0;
	    long			 dsc$l_m;
	};

	typedef struct
	{
	    struct dsc$descriptor_a	 main;
	    struct multi_block		 multi;
	    struct bounds_block		 bounds;
	} array_desc;
#endif

typedef	__int16	wordint;
typedef	__int32	longint;

int sys_dbg_array_desc( array_desc *desc );

/************************************************************************/
/*									*/
/* 	int sys_array_desc(	int,	  Number array elements (inc 0)	*/
/*				wordint,  The length of an element	*/
/*				char*	  Address of the first element	*/
/*				address ) Address of a buffer where the	*/
/*					    resulting array descriptor	*/
/*					    can be put.			*/
/*									*/
/* This is the mainline of this function. See main file header for	*/
/* details of its function.						*/
/************************************************************************/

int sys_array_desc(
	int		*elements,	
	wordint		*ele_len,	
	char		*array_start,
	array_desc	*desc )
{
	/* Declare some workspace for the debug functions		*/
	int	 dbg = 0;			/* 0=off, 1=on		*/
	char	 dest[CDBG_DEST_LEN];		/* Output destination	*/
	char	*jobname = "sys_array_desc";	/* Our function name	*/


	if ( dbg = HN_check_debugs( jobname, dest ) )
	{
	    HN_debugs( jobname, 'B', dest );
	    HN_buf_debugs( "", 0, "Build descriptor for array with " );
	    HN_buf_int_debugs( "", (int)*elements );
	    HN_buf_debugs( "", 0, " elements, " );
	    HN_buf_int_debugs( "", (int)*ele_len );
	    HN_buf_debugs( jobname, 0, " bytes long." );
	    HN_buf_debugs( "", 0, "Element 0 at address " );
	    HN_buf_int_debugs( jobname, (int)array_start );
	}

	/* Populate the elements which are the same regardless of the	*/
	/* platform we're running on.					*/
	/* --dsc$w_length	Size in bytes of each string element	*/
	/* --dsc$b_dtype	Type of data in the strings (text)	*/
	/* --dsc$a_pointer	Address of the first element		*/
	/* --dsc$b_scale	Scale factor (N/A for strings)		*/
	/* --dsc$b_digits	Digits (N/A for strings)		*/
	/* --dsc$b_dimct	Number of dimensions (we only support 1)*/
	/* --dsc$l_arsize	Total size of array in bytes		*/
	/* --dsc$v_fl_binscale	Is 'scale' in binary (N/A for strings)	*/
	/* --dsc$v_fl_redim	Can the array be redimensioned		*/
	/* --dsc$l_l		Lowest element no (Always 1 for COBOL)	*/
	/* --dsc$l_u		Highest element no			*/

	desc->main.dsc$w_length		= *ele_len;
	desc->main.dsc$b_dtype		= DSC$K_DTYPE_T;
	desc->main.dsc$a_pointer	= array_start;
	desc->main.dsc$b_scale		= 0;
	desc->main.dsc$b_digits		= 0;
	desc->main.dsc$b_dimct		= 1;
	desc->main.dsc$l_arsize		= ( *elements * *ele_len );

	desc->main.dsc$b_aflags.dsc$v_fl_binscale	= 0;
	desc->main.dsc$b_aflags.dsc$v_fl_redim		= 0;

	desc->bounds.dsc$l_l		= 1;
	desc->bounds.dsc$l_u		= *elements;

#if defined(__ALPHA) || defined(__ia64)
	/* Now the Alpha specific fields. This includes the descriptor	*/
	/* class, which defines what other fields there are. Trust me.	*/
	/* --dsc$b_class	Array type; Non Contiguous Array	*/
	/* --dsc$a_a0		Address of first byte of data		*/
	/* --dsc$l_s		Bytes from one element start to the next*/

	desc->main.dsc$b_class		= DSC$K_CLASS_NCA;
	desc->stride.dsc$a_a0		= array_start;
	desc->stride.dsc$l_s		= *ele_len;
#else
	/* If we're on a VAX, set the type-a-descriptor-specific	*/
	/* fields.							*/
	/*								*/
	/* column: If set indicates column-major order (FORTRAN only)	*/
	/* coeff:  If set indicates a multipliers block is present	*/
	/* bounds: If set indicates a bounds block is present		*/
	desc->main.dsc$b_class		= DSC$K_CLASS_A;

	desc->main.dsc$b_aflags.dsc$v_fl_column	= 0;
	desc->main.dsc$b_aflags.dsc$v_fl_coeff	= 1;
	desc->main.dsc$b_aflags.dsc$v_fl_bounds	= 1;

	desc->multi.dsc$a_a0		= array_start;
	desc->multi.dsc$l_m		= *elements;
#endif

	/* If debugs are on, switch them off neatly.			*/

	if ( dbg )
	{
	    sys_dbg_array_desc( desc );
	    HN_debugs( jobname, 'E', "" );
	}

	return ( 1 );
}

/************************************************************************/
/*									*/
/*	int sys_dbg_array_desc( array_desc* )				*/
/*									*/
/* This is primarily an internal debug function, which prints out the	*/
/* contents of the descriptor passed to it by reference. Since it is	*/
/* also has potentially wider uses during debugging, it is left as an	*/
/* externally callable function.					*/
/*									*/
/************************************************************************/

int sys_dbg_array_desc( array_desc *desc )
{
	char	*jobname = "sys_dbg_array_desc";

	HN_debugs( jobname, 'B', "F:NL:" );

	HN_buf_debugs( "", 0, "dsc$w_length         [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$w_length     );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "dsc$b_dtype          [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$b_dtype      );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "dsc$b_class          [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$b_class      );
	HN_buf_debugs( jobname, 0, "]" );


	HN_buf_debugs( "", 0, "dsc$a_pointer        [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$a_pointer    );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "dsc$b_scale          [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$b_scale      );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "dsc$b_dimct          [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$b_dimct      );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "dsc$l_arsize         [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$l_arsize     );
	HN_buf_debugs( jobname, 0, "]" );


	HN_buf_debugs( "", 0, "--.dsc$v_fl_binscale [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$b_aflags.dsc$v_fl_binscale );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "--.dsc$v_fl_redim    [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$b_aflags.dsc$v_fl_redim );
	HN_buf_debugs( jobname, 0, "]" );

#if defined(__ALPHA) || defined(__ia64)

	HN_buf_debugs( "", 0, "stride.dsc$a_a0      [" );
	HN_buf_int_debugs( "", (int)desc->stride.dsc$a_a0 );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "stride.dsc$l_s       [" );
	HN_buf_int_debugs( "", (int)desc->stride.dsc$l_s );
	HN_buf_debugs( jobname, 0, "]" );
#else

	HN_buf_debugs( "", 0, "--.dsc$v_fl_column   [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$b_aflags.dsc$v_fl_column );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "--.dsc$v_fl_coeff    [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$b_aflags.dsc$v_fl_coeff );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "--.dsc$v_fl_bounds   [" );
	HN_buf_int_debugs( "", (int)desc->main.dsc$b_aflags.dsc$v_fl_bounds );
	HN_buf_debugs( jobname, 0, "]" );


	HN_buf_debugs( "", 0, "multi.dsc$a_a0       [" );
	HN_buf_int_debugs( "", (int)desc->multi.dsc$a_a0 );
	HN_buf_debugs( jobname, 0, "]" );

	HN_buf_debugs( "", 0, "multi.dsc$a_m        [" );
	HN_buf_int_debugs( "", (int)desc->multi.dsc$l_m );
	HN_buf_debugs( jobname, 0, "]" );
#endif

	HN_debugs( jobname, 'E', "" );

	return ( 1 );
}
