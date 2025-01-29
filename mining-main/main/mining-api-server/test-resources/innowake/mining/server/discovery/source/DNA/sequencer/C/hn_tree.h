	/*==============================================================*/
	/*								*/
	/*	SUITE	:	"C" service routines			*/
	/*	PCS ELE	:	HN_TREE					*/
	/*	PROGRAM	:	HN_TREE.H				*/
	/*	TITLE	:	Binary Tree functions			*/
	/*	AUTHOR	:	Shane Smith				*/
	/*								*/
	/*==============================================================*/

struct HN_tree_node							/* Structure definition for a generic tree node		*/
{									/*							*/
	void			*entry;					/* Pointer to the entity the node represents		*/
	struct HN_tree_node	*lesser;				/* Pointer to the next node downscale, or NULL if none	*/
	struct HN_tree_node	*equal;					/* Pointer to the next equal node, or NULL if none	*/
	struct HN_tree_node	*greater;				/* Pointer to the next node upscale, or NULL if none	*/
	int		 	dupes_below;				/* Count of dupes below this node. Is total dupes on	*/
									/* the first dupe entry, or when no dupes being stored.	*/
};									/*							*/

struct HN_tree_root							/* Structure definition for generic tree root		*/
{									/*							*/
	char			 name[64];				/* Space for a 64 byte name for the tree		*/
	struct HN_tree_node	*node_one;				/* Pointer to first node in tree			*/
	int			(*compare)(void *, void *);		/* Pointer to function to compare two entries		*/
	void			(*print)(void *, void *);		/* Pointer to function to print an entry		*/
	void			(*destroy)(void *);			/* Pointer to function to destroy an entry		*/
	int			keep_dupes;				/* Whether we're keeping duplicates or not		*/
	int			entries;				/* Number of entries in the tree			*/
	int			unique;					/* Number of unique entries in the tree			*/
};

#ifdef __cplusplus
   extern "C" {
#endif

int HN_tree_init_root(							/* Routine for initializing a tree root record.		*/
		struct HN_tree_root	 *root,				/* Take the 'root' record from the caller.		*/
		char			 *name,				/* The name to assign the tree				*/
		int			(*compare)(void *, void *),	/* Routine to compare two entries			*/
		void			(*print)(void *, void *),	/* Routine to print an individual entry			*/
		void			(*destroy)(void *),		/* Routine to destroy an individual entry		*/
		int			  keep_dupes );			/* 0=keep everything, 1=keep first occurances only	*/

int HN_tree_add_entry(							/* Routine for adding a node to a tree			*/
		struct HN_tree_root	*root,				/* Take the 'root' record from the caller.		*/
		void			*entry );			/* And the entry we want to add				*/

void HN_tree_print(							/* Routine for printing every node on the tree		*/
		struct HN_tree_root	*root,				/* Take the 'root' record from the caller		*/
		int		 	 dupes );			/* Do we print duplicates (1), or just one of each (0)	*/

void *HN_tree_find_entry(						/* Routine to locate and return pointer to a tree node	*/
		struct HN_tree_root	*root,				/* Take the 'root' record from the caller.		*/
		void			*entry_required,		/* a template for the entry we want to find.		*/
		int			*dupe_count,			/* If found, this is the entry's dupes count.		*/
		int			*result );			/* 0=found, -1=not found.				*/

void HN_tree_process(							/* Routine to run a given function on every tree node	*/
		struct HN_tree_root	 *root,				/* Take the 'root' record from the caller		*/
		int		 	  dupes,			/* 1=process dupes 0=only process primary nodes		*/
		void			 *func_data,			/* Struct containing data to pass to the function	*/
		void			(*process)(void *, void *) );	/* The function to process an entry			*/

void HN_tree_destroy(
		struct HN_tree_root	*root );

#ifdef __cplusplus
   }
#endif

