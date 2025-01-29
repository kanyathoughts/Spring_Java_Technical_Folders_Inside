	/*==============================================================*/
	/*								*/
	/*	SUITE	:	"C" service routines			*/
	/*	PCS ELE	:	HN_TREE					*/
	/*	PROGRAM	:	HN_TREE.C				*/
	/*	TITLE	:	Binary Tree functions			*/
	/*	AUTHOR	:	Shane Smith				*/
	/*								*/
	/* This file contains binary tree handling routines for use	*/
	/* mainly from "C".						*/
	/*								*/
	/*==============================================================*/

#include <ctype.h>							/* Pull in definitions for character type test functs	*/
#include <stdio.h>							/* true, false, io functions				*/
#include <stdlib.h>							/* general utility functions				*/
#include <string.h>							/* "C" format string handling				*/

#if defined(__VMS) || defined(VMS)
#include "inc:HN_tree.h"						/* Public defs for tree functions			*/
#else
#include "HN_tree.h"							/* Public defs for tree functions			*/
#endif

#define ROOT 	struct HN_tree_root
#define NODE 	struct HN_tree_node


#ifdef TEST_MODE
#  define DEBUGS_ON 1
#endif

	/*==============================================================*/
	/*								*/
	/*	Local definitions					*/
	/*								*/
	/*==============================================================*/

	/*==============================================================*/
	/*								*/
	/*	init_node						*/
	/*								*/
	/* Initialise a new node for the tree.				*/
	/*==============================================================*/

static NODE *init_node(	void	*new_entry )				/* The address of the entry to point the node at	*/
{
	NODE	*new_node;						/* Temporary pointer for the newly created node		*/

	new_node = (NODE *)malloc( sizeof( NODE ) );			/* Reserve space for the new node			*/
									/*							*/
	if ( new_node != NULL )						/* If the malloc worked,				*/
	{
	    new_node->entry	= new_entry;				/* Attatch it to the entry provided			*/
	    new_node->lesser	= new_node->equal			/*							*/
				= new_node->greater = NULL;		/* As it's new, there can be no descendants yet		*/
	    new_node->dupes_below = 0;					/* And no duplicates below it.				*/
	};

	return( new_node );
}

	/*==============================================================*/
	/*								*/
	/*	add_node						*/
	/*								*/
	/* RECURSIVE: WATCH YOUR BUTT IN HERE.				*/
	/*==============================================================*/

static NODE *add_node(	
				NODE	 *cnode,			/* The start entry for where to add the new node	*/
				int	(*compare)(void *, void *),	/* Pointer to function to compare 2 entries. (-1,0,1)	*/
				void	 *new_entry,			/* The entry the new node should represent		*/
				int	  keep_dupes,			/* Do we want to keep duplicates 1=y,0=n		*/
				int	 *result )			/* -1=out of memory, 0=success, 1=success(duplicate)	*/
{

	if (!cnode)							/* If there's nothing at the current node		*/
	{
	    cnode = init_node( new_entry );				/* Create a new node and point it at the new entry	*/
	    if ( !cnode )						/*							*/
		*result = -1;						/* Return -1 for memory allocation failure		*/
	    else							/*							*/
	        *result = 0;						/* Return 0 for success					*/
	}
	else
	{
	    int comp;
	    comp = (*compare)( new_entry, cnode->entry );		/* Compare the entries					*/
	    switch (comp)						/*							*/
	    {								/*							*/
		case -1:						/* If the new one's lesser,				*/
		    cnode->lesser = add_node( cnode->lesser,		/* Try and add it on the lesser branch			*/
			compare, new_entry, keep_dupes, result );	/*							*/
		    break;						/*							*/
		case 0:							/* If the existing one's equal				*/
		    if (keep_dupes)					/* And we're keeping duplicates,			*/
		        cnode->equal = add_node( cnode->equal,		/*   try and add it on the equal branch			*/
			    compare, new_entry, keep_dupes, result );	/*							*/
		    if ( *result == 0 ) *result = 1;			/* Kept or not, Override success status to 'duplicate'	*/
		    cnode->dupes_below++;				/* Update the node's dupe count				*/
		    break;						/*							*/
		case 1:							/* If the existing one's greater			*/
		    cnode->greater = add_node( cnode->greater,		/* Try and add it on the greater branch			*/
			compare, new_entry, keep_dupes, result );	/*							*/
		    break;						/*							*/
	    };
	};

	return ( cnode );
}

	/*==============================================================*/
	/*								*/
	/*	find_node						*/
	/*								*/
	/* RECURSIVE: WATCH YOUR BUTT IN HERE.				*/
	/*==============================================================*/

static NODE *find_node(	
				NODE	 *cnode,			/* The start entry for where to a the node		*/
				int	(*compare)(void *, void *),	/* Pointer to function to compare 2 entries. (-1,0,1)	*/
				void	 *template_entry,		/* A template entry, with key fields only filled	*/
				int	 *result )			/* -1=no match, 0=success				*/
{
	if (!cnode)							/* If there's nothing at the current node		*/
	{
	    *result = -1;						/* Set up to return failure.				*/
	    return (NULL);
	}
	else
	{
	    int comp;
	    comp = (*compare)( template_entry, cnode->entry );		/* Compare the entries					*/
	    switch (comp)						/*							*/
	    {								/*							*/
		case -1:						/* If the required one's lesser,			*/
		    cnode = find_node( cnode->lesser,			/* Look down the lesser branch				*/
				compare, template_entry, result );	/*							*/
		    break;						/*							*/
		case 0:							/* If the existing one's equal				*/
		    *result = 0;					/* set up to return success				*/
		    break;						/*							*/
		case 1:							/* If the required one's greater			*/
		    cnode = find_node( cnode->greater,			/* look down the greater branch				*/
				compare, template_entry, result );	/*							*/
		    break;						/*							*/
	    };

	};

	return ( cnode );
}

	/*==============================================================*/
	/*								*/
	/*	tree_walk						*/
	/*								*/
	/* RECURSIVE: WATCH YOUR BUTT IN HERE.				*/
	/* Walk the whole tree, executing a user-defined processing	*/
	/* function on every node.					*/
	/*==============================================================*/

static void tree_walk(		NODE	 *cnode,			/* The cnode of the tree to process			*/
				int	  dupes,			/* 1 = process duplicates, 0 = primary nodes only	*/ 
				void	 *func_data,			/* Arguments for the function (if any) in a struct	*/
				void	(*process)(void *, void *) )	/* The function to process an entry			*/
{
	if (cnode)							/* If there's a node at this point,			*/
	{								/*							*/
	    tree_walk( cnode->lesser, dupes, func_data, process );	/* Process everything off its lesser branch first,	*/
	    (*process)( cnode->entry, NULL );				/* Then process this node				*/
	    if ( ( cnode->equal )					/* Followed by anything down its 'equal to' branch	*/
	    &&   ( dupes == 1 ) )					/* providing we want to process dupes			*/
	    {								/*							*/
		NODE *eqnode = cnode->equal;				/*   (This is linear, so we just loop down until we	*/
		do							/*    reach the end of the branch)			*/
		{							/*							*/
		    (*process)( eqnode->entry, func_data );		/*							*/
		    eqnode = eqnode->equal;				/*							*/
		} while ( eqnode );					/*							*/
	    };								/*							*/
	    tree_walk( cnode->greater, dupes, func_data, process );	/* Finally process everything off its greater branch.	*/
	};								/*							*/
}									/* Game over.						*/

 	/*==============================================================*/
	/*								*/
	/*	tree_destroy						*/
	/*								*/
	/* RECURSIVE: WATCH YOUR BUTT IN HERE.				*/
	/* Walk the whole tree, deleting each node after we've finished	*/
	/* with it.							*/
	/*==============================================================*/

static void tree_destroy(	NODE	 *cnode,			/* The cnode of the tree to bump off			*/
				void	(*destroy)(void*) )		/* Routine to destroy an individual entry		*/
{
	NODE *next = cnode;

	if (cnode)							/* If there's a node at this point, kill everything	*/
	{								/* beneath it before we kill this one.			*/
	    tree_destroy( cnode->lesser, destroy );			/* Process everything off its lesser branch first,	*/
	    tree_destroy( cnode->greater, destroy );			/* Then process everything off its greater branch.	*/
	    do								/* Walk down the duplicates, freeing each one as we go.	*/
	    {								/*							*/
		next = cnode->equal;					/*							*/
		(*destroy)( cnode->entry );				/*							*/
		free( cnode );						/*							*/
		cnode = next;						/*							*/
	    } while ( cnode != NULL );					/* Stop when we run out of duplicates.			*/
	};								/*							*/

}									/* Game over.						*/

	/*==============================================================*/
	/*								*/
	/*	HN_tree_init_root					*/
	/*								*/
	/*==============================================================*/

int HN_tree_init_root(	ROOT		 *root,				/* Take the 'root' record from the caller.		*/
			char		 *name,				/* The name to assign the tree				*/
			int		(*compare)(void *, void *),	/* Routine to compare to entries			*/
			void		(*print)(void *, void *),	/* Routine to print an individual entry			*/
			void		(*destroy)(void*),		/* Routine to destroy an individual entry		*/
			int		  keep_dupes )			/* 0=keep everything, 1=keep first occurances only	*/
{
#ifdef DEBUGS_ON
	printf( "Initializing tree [%s]\n", name );
#endif


	strcpy( root->name, name );					/* Load up the information given onto the root record	*/
	root->node_one	= NULL;						/*							*/
	root->compare	= compare;					/*							*/
	root->print	= print;					/*							*/
	root->destroy	= destroy;					/*							*/
	root->keep_dupes= keep_dupes;					/*							*/
	root->entries	= 0;						/* And we've a couple of our own, too.			*/
	root->unique	= 0;						/*							*/

	return (EXIT_SUCCESS);
}	

	/*==============================================================*/
	/*								*/
	/*	HN_tree_add_entry					*/
	/*								*/
	/*==============================================================*/

int HN_tree_add_entry(	ROOT	*root,					/* Take the 'root' record from the caller.		*/
			void	*entry )				/* And the entry we want to add				*/
{
	int result;

	root->node_one = add_node(					/* Call the recursive add node function			*/
					root->node_one,			/* Passing the first node of the tree			*/
					root->compare,			/* the address of the entry comparison routine		*/
					entry,				/* the new entry itself					*/
					root->keep_dupes,		/* whether the tree stores duplicates or not		*/
					&result );			/* And get back whether it worked or not		*/

	switch (result)
	{
	    case -1:							/* -1 = total failure.					*/
		break;							/* Nothing further we can do.				*/
									/*							*/
	    case 0:							/* 0 = fresh new entry					*/
		root->unique++;						/* increment the unique count, BUT DON'T BREAK.		*/
									/*							*/
	    case 1:							/* 1 = entered, but duplicate				*/
		root->entries++;					/* increment the total entries count			*/
	};

#ifdef DEBUGS_ON
	printf( "Adding " );
	(*root->print)( entry, NULL );
	switch (result)
	{   case -1: printf( " failed" ); break;
	    case 1:  printf( " (duplicate)" );
	    case 0:  printf( " worked. Entry %d.\n", root->entries );
	};
#endif

	return (result);
}

	/*==============================================================*/
	/*								*/
	/*	HN_tree_find_entry					*/
	/*								*/
	/*==============================================================*/

void *HN_tree_find_entry( ROOT		*root,				/* Take the 'root' record from the caller.		*/
			 void		*entry_required,		/* a template for the entry we want to find.		*/
			 int		*dupe_count,			/* If found, this is the entry's dupes count.		*/
			 int		*result )			/* 0=found, -1=not found.				*/
{
	NODE	*node_found;

	node_found = find_node(						/* Call the recursive find node function		*/
					root->node_one,			/* Passing the first node of the tree			*/
					root->compare,			/* the address of the entry comparison routine	*/
					entry_required,			/* the new entry itself					*/
					result );			/* And get back whether it worked or not		*/

#ifdef DEBUGS_ON
	printf( "Looking for " );
	(*root->print)( entry_required, NULL );
	if ( *result < 0 )
	{
	    printf( ": Not found.\n" );
	} else {
	    printf( ": Found " );
	    (*root->print)( node_found->entry, NULL );
	    printf( ", %d dupes.\n", node_found->dupes_below );
	}
#endif

	if ( *result < 0 )
	    return (NULL);
	else
	{
	    *dupe_count	= node_found->dupes_below;
	    return (node_found->entry);
	}
}

	/*==============================================================*/
	/*								*/
	/*	HN_tree_process					*/
	/*								*/
	/*==============================================================*/

void HN_tree_process(	ROOT	 *root,
			int	  dupes,				/* 1=process dupes 0=only process primary nodes		*/
			void	 *func_data,				/* Struct containing data to pass to the function	*/
			void	(*process)(void *, void *) )		/* The function to process an entry			*/
{
	tree_walk( root->node_one, dupes, func_data, process );		/* Apply the processing to all the nodes		*/
}

	/*==============================================================*/
	/*								*/
	/*	HN_tree_print						*/
	/*								*/
	/*==============================================================*/

void HN_tree_print(	ROOT		*root,
			int		 dupes )
{
	printf( "\nContents of tree [%s]:\n", root->name );
	tree_walk( root->node_one, dupes, NULL, root->print );
	printf( "\nentries   [%d]\nunique    [%d]\n\n", root->entries, root->unique );
}

	/*==============================================================*/
	/*								*/
	/*	HN_tree_destroy					*/
	/*								*/
	/*==============================================================*/

void HN_tree_destroy(	ROOT	*root )
{
#ifdef DEBUGS_ON
	printf( "Destroying tree [%s]\n", root->name );
#endif

	tree_destroy( root->node_one, root->destroy );
}

#ifdef TEST_MODE

	/*==============================================================*/
	/*==============================================================*/
	/*								*/
	/*	Debug functions						*/
	/*								*/
	/*==============================================================*/
	/*==============================================================*/

	int dbg = 0;

	/*==============================================================*/
	/*	Sample integer comparison and print routines		*/
	/*==============================================================*/

static int	comp_int( void *ent, void *ex )
{
	if (dbg) printf( "Comparing [%d] and [%d]\n", *((int*)ent), *((int*)ex) );

	if ( *((int*)ent) == *((int*)ex) )
	    return ( 0 );
	else
	    if ( *((int*)ent) > *((int*)ex) )
		return ( 1 );
	    else
		return ( -1 );
}

static void	print_int( void *el, void *unused )
{
	printf( "[%d]", *((int*)el) );
}

	/*==============================================================*/
	/*	Sample string comparison and print routine		*/
	/*==============================================================*/

static int	comp_str( void *ent, void *ex )
{
	int result;

	if (dbg) printf( "Comparing [%s] and [%s]\n", (char*)ent, (char*)ex );

	result = strcmp( (char*)ent, (char*)ex );
	if ( result < 0 ) result = -1;
	else if ( result > 0 ) result = 1;

	return ( result );
}

static void	print_str( void *ent, void *unused )
{
	printf( "[%s]", (char*)ent );
}

static void	kill_null( void *unused ) {}

	/*==============================================================*/
	/*	Mainline testcode					*/
	/*==============================================================*/

int main( int argc, char **argv )
{
	ROOT	 	 i_root, s_root;				/* ROOTs for the test trees				*/
	int		 i;						/* Generic loop variable				*/
	int		 dupes, result;					/* For storing returned (but unused) integer variables	*/
	void		*sscan;						/* For storing returned (but unused) pointer variables	*/
	int		 testint;					/* For passing integer values into find_entry		*/
	int		 ivalue[6] = {12,5,30,16,30,21};		/* Test values for putting into the trees		*/
	char		*svalue[9] = {	"Red","Orange","Yellow",
					"Green","Blue","Yellow",
					"Indigo","Violet","Yellow"};

	while(*++argv)
	{
	    if (!strcmp( *argv, "-debug"))
	    {
		dbg = 1;
		printf( "Debugs on.\n" );
	    }
	    else
	        printf( "Unrecognised argument [%s]\n", *argv );
	};

	HN_tree_init_root( &i_root, "Test Integer", comp_int, print_int, kill_null, 1 );
	for ( i = 0; i < 6; i++ )
	    result = HN_tree_add_entry( &i_root, &ivalue[i] );
	HN_tree_print( &i_root, 1 ); 
	testint = 16;
	sscan = HN_tree_find_entry( &i_root, &testint, &dupes, &result );
	testint = 30;
	sscan = HN_tree_find_entry( &i_root, &testint, &dupes, &result );
	testint = 32;
	sscan = HN_tree_find_entry( &i_root, &testint, &dupes, &result );
	HN_tree_destroy( &i_root );

	printf( "\n\n" );

	HN_tree_init_root( &s_root, "Test String", comp_str, print_str, kill_null, 1 );
	for ( i = 0; i < 9; i++ )
	    result = HN_tree_add_entry( &s_root, svalue[i] );
	HN_tree_print( &s_root, 1 );
	sscan = HN_tree_find_entry( &s_root, "Blue", &dupes, &result );
	sscan = HN_tree_find_entry( &s_root, "Yellow", &dupes, &result );
	sscan = HN_tree_find_entry( &s_root, "Tangerine", &dupes, &result );
	HN_tree_destroy( &s_root );


}
#endif
