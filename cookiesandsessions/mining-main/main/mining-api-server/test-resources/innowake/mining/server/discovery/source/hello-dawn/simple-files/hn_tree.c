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
