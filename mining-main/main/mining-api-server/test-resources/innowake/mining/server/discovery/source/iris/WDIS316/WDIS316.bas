FUNCTION    LONG    CEPF502AC_Check_Claims				&
		    ( STRING M_SSN, M_MBR, M_CODE, ICIDATE M_DATE )

! End Documentation

	!---------------------------------------------------------------!
	!	COMPILATION OPTIONS					!
	!---------------------------------------------------------------!
	OPTION	TYPE = EXPLICIT						&
	,	SIZE = ( INTEGER LONG, REAL DOUBLE )			&
	,	CONSTANT TYPE = INTEGER
	
	SET NO PROMPT

	!---------------------------------------------------------------!
	!	GET PROGRAM VERSION NUMBER				!
	!---------------------------------------------------------------!
	%IF %VARIANT = 1%
	%THEN
	    %INCLUDE "INC:<Program_Name>.VER"
	%ELSE
	    DECLARE STRING CONSTANT Source_Version = "TEST"
	%END %IF

	!---------------------------------------------------------------!
	!	INCLUDE FILES						!
	!---------------------------------------------------------------!
	%INCLUDE "INC:FOO.INC"
	%INCLUDE "INC:SYS_CHN_01.INC"
	%INCLUDE "INC:ICIDATE.REC"
	%INCLUDE "INC:ICITIME.REC"
	%INCLUDE "INC:ICISTAMP.REC"
	%INCLUDE "INC:PHONE.REC"
	%INCLUDE "INC:NAMEDEF.REC"

	%INCLUDE "INC:CLWIP.MAP"
	%INCLUDE "INC:CLHIST.MAP"

	!---------------------------------------------------------------!
	!	EXTERNAL CONSTANT DECLARATION				!
	!---------------------------------------------------------------!
	EXTERNAL LONG CONSTANT						&
		SS$_NORMAL						&
	,	SS$_ENDOFFILE						&
	,	DSC$K_DTYPE_L						&
	,	DSC$K_DTYPE_W						&
	,	DSC$K_DTYPE_B						&
	,	DSC$K_DTYPE_T						&
	
	!---------------------------------------------------------------!
	!	EXTERNAL FUNCTION DECLARATION				!
	!---------------------------------------------------------------!

	EXTERNAL BYTE	FUNCTION	ICIF001_Assign_Channel		&
	,				ICIF002_Deassign_Channel (BYTE)




	!---------------------------------------------------------------!
	!	EXTERNAL SUBPROGRAM DECLARATION				!
	!---------------------------------------------------------------!
	EXTERNAL SUB							&
		CLDD01_NEW ( STRING, STRING, STRING  )			&

	!---------------------------------------------------------------!
	!	LOCAL CONSTANT DECLARATION				!
	!---------------------------------------------------------------!



	!---------------------------------------------------------------!
	!	LOCAL FUNCTION DECLARATION				!
	!---------------------------------------------------------------!



	!---------------------------------------------------------------!
	!	LOCAL SUBROUTINE DECLARATION				!
	!---------------------------------------------------------------!
	!	Initialization	Get input argument & Init. variables 	&
	!	Open_Files	Open all files				&
	!	Main_Process	Main control logic			&
	!	Wrap_Up		Close all files				&

	!---------------------------------------------------------------!
	!	MAPPING AREA						!
	!---------------------------------------------------------------!

	!---------------------------------------------------------------!
	!	LOCAL VARIABLE DECLARATION				!
	!---------------------------------------------------------------!
	DECLARE STRING							&
		M_KEY							&
	,	M_CMD							&

	DECLARE LONG							&
		HN_STAT							&

	DECLARE	BYTE							&
		CLAIMS_HIST_Open					&
	,	CLAIMS_WIP_Open						&



	!---------------------------------------------------------------!
	!	MAIN LOGIC						!
	!---------------------------------------------------------------!
	WHEN ERROR USE GLOBAL_HANDLER

	    GOSUB Initialization
	    GOSUB Open_Files
	    GOSUB Main_Process
	    GOSUB Wrap_Up

	END WHEN

	EXIT FUNCTION

	!---------------------------------------------------------------!
	!	LOCAL SUBROUTINES					!
	!---------------------------------------------------------------!
Initialization:

	IF M_SSN = "CLOSEIT"
	THEN
	    CALL ICIF002_Deassign_Channel(Claims_WIP) IF CLAIMS_WIP
	    CALL ICIF002_Deassign_Channel(Claims_HIST) IF CLAIMS_HIST
	    EXIT FUNCTION
	END IF
	M_CMD = " "
	CEPF502AC_Check_Claims = False

	RETURN

Open_Files:

	SELECT Claims_WIP
	CASE 0%
	    Claims_WIP = ICIF001_Assign_Channel
	    IF Claims_WIP<> 0
		THEN
		    WHEN ERROR IN
			OPEN "Claims_WIP" FOR INPUT AS FILE #Claims_WIP &
			,	INDEXED FIXED				&
			,	ACCESS MODIFY				&
			,	ALLOW MODIFY				&
			,	MAP CLWIP
			
			CLAIMS_WIP_Open = TRUE
		    USE
			CALL TAC02_NEW (0%, "Error opening Claims_WIP, Error: " &
					+ STR$ (ERR))
			STOP
		    END WHEN
	    ELSE
		CALL TAC02_NEW (0%, "No channel available at the moment")
	    END IF
	CASE ELSE
		CLAIMS_WIP_Open = FALSE
	END SELECT

	SELECT Claims_Hist
	CASE 0%
	    Claims_Hist = ICIF001_Assign_Channel
	    IF Claims_Hist <> 0
		THEN
		    WHEN ERROR IN
			OPEN "Claims_Hist" FOR INPUT AS FILE #Claims_Hist &
			,	INDEXED FIXED				&
			,	ACCESS MODIFY				&
			,	ALLOW MODIFY				&
			,	MAP CLHIST
			
			CLAIMS_HIST_Open = TRUE
		    USE
			CALL TAC02_NEW (0%, "Error opening Claims_Hist, Error: " &
					+ STR$ (ERR))
			STOP
		    END WHEN
	    ELSE
		CALL TAC02_NEW (0%, "No channel available at the moment")
	    END IF
	CASE ELSE
		CLAIMS_HIST_Open = FALSE
	END SELECT

	RETURN	

Main_Process:

	M_KEY = M_SSN + M_MBR
	
	WHEN ERROR IN
	    GET #CLAIMS_WIP, KEY#1 GE M_KEY, REGARDLESS
	    HN_STAT = SS$_NORMAL
	USE
		SELECT Err
		CASE 138, 154
		    SLEEP 2%
		    RETRY
		CASE ELSE
		    HN_STAT = 0
		END SELECT
	END WHEN

	WHILE HN_Stat = SS$_NORMAL					&
			AND M_KEY = CLWIP::SSN + CHR$ ( CLWIP::MBR_TYPE )
	    IF M_DATE = CLWIP::FROM_DATE
	    THEN
		CEPF502AC_Check_Claims = True
		CALL CLDD01_NEW (M_CMD, M_SSN, M_CODE)
		EXIT FUNCTION
	    END IF

	    WHEN ERROR IN
		GET #CLAIMS_WIP,  REGARDLESS
		HN_STAT = SS$_NORMAL
	    USE
		SELECT Err
		CASE 138, 154
		    SLEEP 2%
		    RETRY
		CASE ELSE
		    HN_STAT = 0
		END SELECT
	    END WHEN

	NEXT


	WHEN ERROR IN
	    GET #CLAIMS_HIST, KEY#0 GE M_KEY, REGARDLESS
	    HN_STAT = SS$_NORMAL
	USE
		SELECT Err
		CASE 138, 154
		    SLEEP 2%
		    RETRY
		CASE ELSE
		    HN_STAT = 0
		END SELECT
	END WHEN

	WHILE HN_Stat = SS$_NORMAL					&
			AND M_KEY = CLHIST::SSN + CHR$ ( CLHIST::MBR_TYPE )
	    IF M_DATE = CLHIST::FROM_DATE
	    THEN
		CEPF502AC_Check_Claims = True
		CALL CLDD01_NEW (M_CMD, M_SSN, M_CODE)
		EXIT FUNCTION
	    END IF

	    WHEN ERROR IN
		GET #CLAIMS_HIST,  REGARDLESS
		HN_STAT = SS$_NORMAL
	    USE
		SELECT Err
		CASE 138, 154
		    SLEEP 2%
		    RETRY
		CASE ELSE
		    HN_STAT = 0
		END SELECT
	    END WHEN

	NEXT

	RETURN

Wrap_Up:
	!---------------------------------------------------------------!
	!	Set return condition when use ICMENU			!
	!---------------------------------------------------------------!

	RETURN

	!---------------------------------------------------------------!
	!	LOCAL FUNCTIONS						!
	!---------------------------------------------------------------!

	!---------------------------------------------------------------!
	!	GLOBAL HANDLER						!
	!---------------------------------------------------------------!
	HANDLER GLOBAL_HANDLER
	    PRINT ERT$(ERR) + " on " + ERN$
	    EXIT HANDLER
	END HANDLER

	END FUNCTION
