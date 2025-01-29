************************************************************************
** Object	: DISCNATX
** Function	: external subroutine EXTERNAL-SUBR
**            name of subroutine differs from module name
************************************************************************
*
DEFINE SUBROUTINE EXTERNAL-SUBR
	PRINT 'this is external subroutine EXTERNAL-SUBR in program ' *PROGRAM
	PERFORM INTERNAL-SUBR
	DEFINE SUBROUTINE INTERNAL-SUBR
		PRINT 'this is internal subroutine INTERNAL-SUBR in program ' *PROGRAM
	END-SUBROUTINE
END-SUBROUTINE
*
END