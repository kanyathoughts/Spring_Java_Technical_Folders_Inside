$! you can set the symbol keep_verify_on to Y before you call this
$! procedure if you want
$   PV = F$ENVIRONMENT("VERIFY_PROCEDURE")
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$!-----------------------------------------------------------------------------!
$!  H E A L T H   N E T   -   Automated Business System.                       !
$!-----------------------------------------------------------------------------!
$! This source code, and all the routines referenced herein, are the           !
$! proprietary properties and trade secrets of HEALTH NET, INC. Except as      !
$! provided for by license agreement, this source code shall not be            !
$! duplicated, used or disclosed without written consent, signed by an officer !
$! of HEALTH NET, INC. 							       !
$!-----------------------------------------------------------------------------!
$! Start Documentation:
$!
$!      SYSTEM:	CAPITATION
$!         JOB:	CAP_BAL_BRX_81
$!        DATE: 06-24-04
$!      AUTHOR:	John Ruffner
$!    FUNCTION:	Balance CAPTMLN to CAP22 to PRVTTLS to CAPTTLS_TOTALS
$!		     or ASCTMLN to ASC22 to ASCTTLS to ASCTTLS_TOTALS
$!
$!    OVERVIEW:	This program runs several times from the command procedure.
$!
$!		It extracts totals from the PRVTTLS files that are converted
$!		from CAPTTLS_TOTALS (PRVNEW)
$!
$!		it extracts totals from CAPTTLS_C22, which
$!		is created from the CAP22 database;
$!
$!		it extracts totals from CAPTTLS_TMLN, which is
$!		created from the CAPTMLN_NEW files;
$!
$!		it extracts totals from CAPTTLS_TOTALS, which is
$!		updated weekly or monthly;
$!
$!		after all the extract files are sorted in the same order,
$!		then the balancing routine runs and compares the extracts;
$!		the result is a report of any discrepancies from any of the
$!		data sources
$!
$!		Note 1:  with CAPTMLN we cannot distinguish properly between
$!		prior and current amounts (e.g. - we cap 100 for Jan but in
$!		Feb we adjust to 120 -- there will be one record for 120
$!		in CAPTMLN_NEW and in Feb it will all go under the current
$!		column) so we will only compare the total of prior and current
$!		to PRVTTLS & CAP22.
$!
$!		Note 2:  the old PRVTTLS was known to be out of balance with 
$!		CAP22, but the new PRVTTLS does not have that problem.
$!
$!		Note 3: we have an option to include or exclude SNA rules.
$!
$!		Note 4:  in 2006 we redesigned the subcap system and modified
$!		this procedure so it can be used to balance the 19 ASCTTLS
$!		files with ASCTMLN and ASC22 following the same steps outlined
$!		above.
$!
$!	See Release 2.0 Balancing TSD for more information.
$!
$!     RESTART:	This procedure can always be restarted from the top,
$!		with no user intervention required (other than fixing
$!		the error); OR, it can be restarted at the same step
$!		where it blew off, also without user intervention.
$!
$! MODIFICATION HISTORY:
$!	MOD:	<DD-MMM-YYYY>     <ISR# MOD# >     <USER-ID>
$!
$! mod#08 12-DEC-2014 C4M (JOHN RUFFNER)	RT 2061065 version 2.08
$!	Make sure we look at the correct asctmln file
$!
$! mod#07 20090605 JLR PID 11385 (CAPTTLS go-live)
$!		CAPTTLS_TOTALS and ASCTTLS_TOTALS will now
$!		be the files of record; PRVTTLS_nn AND ASCTTLS_nn will be
$!		generated from them, so we are removing the code associated
$!		with PRVOLD (which referred to the PRVTTLS format files 
$!		that were updated from TTUP files).
$! 20081201 JLR Remove temporary copy step (step 10)
$! 20081017 JLR for implementation of PID_10333 PRVTTLS Enhancements,
$!		we now include CAPTTLS_TOTALS and the PRVTTLS files 
$!		derived therefrom (PRVNEW); we no longer copy
$!		CAPTTLS_CAP22 to CAPTTLS_TOTALS, and we're moving
$!		the copy of CAPTTLS_CAP22 and CAPTTLS_TMLN to the parallel
$!		testing backup area to CAP_PAR_BUX_01
$! 20080115 JLR i'm adding a temporary step 10 that will copy capttls_c22
$!		to capttls_totals until we implement the jobs that will
$!		perform the weekly/monthly updates -- this way we'll always
$!		have a fresh up-to-date copy waiting to be updated; we'll
$!		also copy capttls_c22 and capttls_tmln to the parallel
$!		backup area -- later this can be put in cap_par_bux_01,
$!		but that job will have to be synchronized to after the
$!		creation of those files
$! 20070809 JLR CAPTTLS_C22 and ASCTTLS_A22 will now be used to refer to 
$!		the files created from CAP22 and ASC22; CAPTTLS_TOTALS
$!		and ASCTTLS_TOTALS will refer to the new versions of the
$!		total files that will be updated weekly or monthly; we
$!		are adding the ability to compare four files instead of three.
$! 20060914 JLR Modified so it can be used to balance ASCTTLS
$! 10-27-05 JLR Production version
$! 06-24-04 JLR	Begun
$!
$! End Documentation
$!
$!------------------------------------------------------------------------------
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! STEP_0  Initialization - Initialize local symbols and logicals !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ INITIALIZATION:
$!
$   ON ERROR     THEN $ GOTO ERROR_CONDITION
$!
$   THIS_COMFILE = F$ENVIRONMENT("PROCEDURE")       ! This job's filespec
$   JOB_NAME	 = F$PARSE(THIS_COMFILE,,,"NAME")   ! Parse out the name
$   PID		 = F$GETJPI("","PID")               ! Handy for temp filenames
$   PRCNAM	 = F$GETJPI("","PRCNAM")
$   NODENAME	 = F$GETJPI("","NODENAME")
$   JOBTYPE	 = F$GETJPI("","JOBTYPE")
$   NODE_VERSION = F$GETJPI("","NODE_VERSION")
$   USERNAME	 = F$GETJPI("","USERNAME")
$   RUN_STATUS	 = "0"
$   CR[0,8]	 = 13
$   LF[0,8]	 = 10
$   CRLF	 = CR + LF  ! Carriage return/linefeed for e-mail text
$   ERR_MSG_BGN	 = "ERROR ENCOUNTERED WHILE RUNNING " + JOB_NAME + CRLF
$   DIRW	 = "DIRECTORY/SIZ=ALL/DATE/WID=(FILE=36,SIZ=8)"
$   HN_MAIL	 = "$EXE:ACE_MAI_ORX_01"
$   @COM:INITIALIZATION
$!
$   SET RMS/QUERY=DISABLE
$   SH RMS
$!
$   SAY  := WRITE SYS$OUTPUT
$   SAY ""
$   SAY ""
$   SAY "                PID=''PID'"
$   SAY "       Process Name=''PRCNAM'"
$   SAY "          Node Name=''NODENAME'"
$   SAY "           Job Type=''JOBTYPE'"
$   SAY "        VMS Version=''NODE_VERSION'"
$   SAY "  Username(account)=''USERNAME'"
$   SAY "     Procedure name=''JOB_NAME'"
$   SAY ""                            
$   SAY "                 P1=''p1'  <- Restart Step"
$   SAY "                 P2=''p2' <- BUSINESS_REGION"
$   SAY "                 P3=''p3' <- BUSINESS_PRODUCT_LINE"
$   SAY "                 P4=''p4'  <- CAP_TYPE"
$   SAY "                 P5=''p5' <- COB$SWITCHES (optional)"
$   SAY ""
$!
$   SET VERIFY
$   BUSINESS_REGION		:== "''p2'"
$   BUSINESS_PRODUCT_LINE	:== "''p3'"
$   CAP_TYPE			:== "''p4'"
$   MAX_ERRORS			:== "000010000"
$!
$   IF P2 .EQS. "" .OR. P3 .EQS. "" .OR. P4 .EQS. ""
$   THEN
$	ERROR_STATUS = "%X00000000"
$	ERR_MSG = ERR_MSG_BGN + -
		"Required parameter(s) is(are) missing. " + CRLF + -
		"Please re-submit this job with required parameter(s)"
$	GOTO ERROR_CONDITION
$   ENDIF
$!
$! get capregn symbols
$! mod#08 - begin
$! define CAPREGN symbols
$   WS_CAP_TYPE_SUBCAP := S
$   IF CAP_TYPE .EQS. WS_CAP_TYPE_SUBCAP
$   THEN
$	@COM:ASC_LOGICALS
$   ENDIF
$! mod#08 - end
$
$   @COM:CAP_UTL_BRX_001.COM     "''P2'" "''P3'" "''P4'"
$!
$!   IF CAPREGN_RUN_STATUS .EQS. "ENDED"
$!   THEN
$!	ERROR_STATUS = "%X00000000"
$!	ERR_MSG = ERR_MSG_BGN + -
!		"HNCAP Batch Cycle has been ended. " + CRLF + -
!		"If you have to rerun, follow the rerun instructions."
$!	GOTO ERROR_CONDITION
$!   ENDIF
$!
$   IF CAP_TYPE .EQS. "S"
$     THEN
$	DEFINE CAPWORK ASCWORK:
$	DEFINE CAP_RPT ASC_RPT:
$	PFX = "ASC"				!prefix
$   ELSE
$	PFX = "CAP"				!prefix
$   ENDIF
$
$   DEFINE PRVNEW CAPWORK:PRVNEW.RPT81
$   DEFINE TMLNEXT CAPWORK:TMLNEXT.RPT81
$   DEFINE C22EXT CAPWORK:C22EXT.RPT81
$   DEFINE CTUEXT CAPWORK:CTUEXT.RPT81
$!
$   DEFINE RPT1 	CAP_RPT:CAP_BAL_BRX_81.RPT
$   DEFINE MSGFILE	CAPWORK:CAP_BAL_BRX_81.MSG
$   DEFINE PRGSTAT	CAPWORK:CAP_BAL_BRX_81.STAT
$!
$   SET NOVERIFY
$   IF  P1 .EQS. "E" THEN EXIT
$   IF  P1 .NES. "" .AND. P1 .NES. "0"  ! Check restart step
$   THEN
$       SEARCH/NOLOG 'THIS_COMFILE' "STEP_''P1':"   ! Verify restart param
$       IF  $STATUS .NE. %X00000001
$       THEN
$           ERROR_STATUS = $STATUS
$           ERR_MSG = ERR_MSG_BGN + -
                "/PARAMETER=''P1' doesn't exist. Please re-submit at step" + -
                CRLF + "indicated in prior E-mail, not the one listed above."
$           GOTO ERROR_CONDITION
$       ENDIF
$       SET VERIFY
$       GOTO STEP_'P1'
$   ENDIF
$!
$   SET VERIFY
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! Step 1A - Extract summarys for all rec_types from 
$!		all newly created PRVTTLS files (currently 20 for CA/OR
$!		including the projected month)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_1A:
$   RUN_STATUS = "1A"
$   SHOW PROCESS/ACC
$
$! output file:
$   DEFINE EXTOUT PRVNEW:
$
$! input files (look at pre-rolled totals)
$! note: we'll deassign the logicals at the end of extract_prvext, primarily
$! because ascttls_totals is defined to ASC_TOTALS
$   DEFINE/PROC ASC_TOTALS	ASC_CHECKPOINT_3:
$   DEFINE/PROC PRV_TOTALS	CAP_CHECKPOINT_3:
$   GOSUB EXTRACT_PRVEXT
$
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! Step 1B - Sort PRVTTLS extract (PRVNEW)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_1B:
$   RUN_STATUS = "1B"
$   SHOW PROCESS/ACC
$
$   DEFINE PRVEXT PRVNEW:
$   GOSUB SORT_PRVEXT
$
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! Step 3 - Extract summarys for all rec_types from 
$!		CAPTTLS_TMLN file
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_3:
$   RUN_STATUS = "3"
$   SHOW PROCESS/ACC
$!
$   IF CAP_TYPE .EQS. "S"
$     THEN
$	DEFINE CAPTTLS_FILE ASCTTLS_TMLN:
$     ELSE
$	DEFINE CAPTTLS_FILE CAPTTLS_TMLN:
$   ENDIF
$   DEFINE EXTOUT TMLNEXT:
$! delete output files in case we need space
$   SET NOON
$	DELETE/LOG EXTOUT:;*
$   SET ON
$! input files:
$   DIRW CAPTTLS_FILE
$!
$! we need about 5000000 blocks for the file, so let's make sure we have 6M
$   SIZE_NEEDED = 6000000
$   FREE_BLKS = F$GETDVI("EXTOUT", "FREEBLOCKS")		!free space
$   SAY "Estimated space needed for TMLNEXT is ''SIZE_NEEDED'; " + -
	"''FREE_BLKS' blocks are available"
$   IF SIZE_NEEDED.GT. FREE_BLKS
$   THEN
$	ERROR_STATUS = "%X00000000"
$       ERR_MSG = ERR_MSG_BGN + "Not enough space for TMLNEXT file."
$       GOTO ERROR_CONDITION
$   ENDIF
$!
$   DIRW/VERSION=5 EXE:CAP_BAL_BRX_81.*
$   DEFINE/USER COB$SWITCHES "2,''P5'"		! 2 = extract from CAPTTLS_FILE
$						! 4 = exclude SNA rules
$
$   RUN/NODEBUG EXE:CAP_BAL_BRX_81.EXE
$   SET NOON
$	SET FILE/TRUNC EXTOUT:;*
$	LOGDIR EXTOUT
$	TYPE PRGSTAT
$	DELETE/LOG PRGSTAT:;*
$   SET ON
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! Step 4 - TMLNEXT is created in the correct order so it doesn't
$!		need to be sorted
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! Step 5 - Extract summarys for all rec_types from 
$!		CAPTTLS_C22 file (CAP22 summary)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_5:
$   RUN_STATUS = "5"
$   SHOW PROCESS/ACC
$!
$   IF CAP_TYPE .EQS. "S"
$     THEN
$	DEFINE CAPTTLS_FILE ASCTTLS_A22:
$     ELSE
$	DEFINE CAPTTLS_FILE CAPTTLS_C22:
$   ENDIF
$   DEFINE EXTOUT C22EXT:
$! delete output files in case we need space
$   SET NOON
$	DELETE/LOG EXTOUT:;*
$   SET ON
$! input files:
$   DIRW CAPTTLS_FILE
$!
$! we need about 5000000 blocks for the file, so let's make sure we have 6M
$   SIZE_NEEDED = 6000000
$   FREE_BLKS = F$GETDVI("EXTOUT", "FREEBLOCKS")		!free space
$   SAY "Estimated size of C22EXT is ''SIZE_NEEDED'; " + -
	"''FREE_BLKS' blocks are available"
$   IF SIZE_NEEDED.GT. FREE_BLKS
$   THEN
$	ERROR_STATUS = "%X00000000"
$       ERR_MSG = ERR_MSG_BGN + "Not enough space for C22EXT file."
$       GOTO ERROR_CONDITION
$   ENDIF
$!
$   DIRW/VERSION=5 EXE:CAP_BAL_BRX_81.*
$   DEFINE/USER COB$SWITCHES "2,''p5'"		! 2 = extract from CAPTTLS_FILE
$						! 4 = exclude SNA rules
$   RUN/NODEBUG EXE:CAP_BAL_BRX_81.EXE
$   SET NOON
$	SET FILE/TRUNC EXTOUT:;*
$	LOGDIR EXTOUT
$	TYPE PRGSTAT
$	DELETE/LOG PRGSTAT:;*
$   SET ON
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! Step 6 - C22EXT is created in the correct order so it doesn't
$!		need to be sorted
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! Step 7 - Extract summarys for all rec_types from 
$!		CAPTTLS_TOTALS file (CAPTTLS updates weekly/monthly)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_7:
$   RUN_STATUS = "7"
$   SHOW PROCESS/ACC
$!
$   IF CAP_TYPE .EQS. "S"
$     THEN
$	DEFINE CAPTTLS_FILE ASCTTLS_TOTALS:
$     ELSE
$	DEFINE CAPTTLS_FILE CAPTTLS_TOTALS:
$   ENDIF
$   DEFINE EXTOUT CTUEXT:
$! delete output files in case we need space
$   SET NOON
$	DELETE/LOG EXTOUT:;*
$   SET ON
$! input files:
$   DIRW CAPTTLS_FILE
$!
$! we need about 5000000 blocks for the file, so let's make sure we have 6M
$   SIZE_NEEDED = 6000000
$   FREE_BLKS = F$GETDVI("EXTOUT", "FREEBLOCKS")		!free space
$   SAY "Estimated size of CTUEXT is ''SIZE_NEEDED'; " + -
	"''FREE_BLKS' blocks are available"
$   IF SIZE_NEEDED.GT. FREE_BLKS
$   THEN
$	ERROR_STATUS = "%X00000000"
$       ERR_MSG = ERR_MSG_BGN + "Not enough space for CTUEXT file."
$       GOTO ERROR_CONDITION
$   ENDIF
$!
$   DIRW/VERSION=5 EXE:CAP_BAL_BRX_81.*
$   DEFINE/USER COB$SWITCHES "2,''p5'"		! 2 = extract from CAPTTLS_FILE
$						! 4 = exclude SNA rules
$   RUN/NODEBUG EXE:CAP_BAL_BRX_81.EXE
$   SET NOON
$	SET FILE/TRUNC EXTOUT:;*
$	LOGDIR EXTOUT
$	TYPE PRGSTAT
$	DELETE/LOG PRGSTAT:;*
$   SET ON
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! Step 8 - CTUEXT is created in the correct order so it doesn't
$!		need to be sorted
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$
$ END_STEP_8:
$
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$! Step 9 - run the balancing program for
$!		CAPTMLN, CAP22, CAPTTLS, PRVNEW;
$!		(or substitute ASCTMLN, ASC22, ASCTTLS, and ASCTTLS_nn
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_9:
$   RUN_STATUS = "9"
$   SHOW PROCESS/ACC
$!
$   SET NOON
$!	DELETE/LOG RPT1:;*
$   SET ON
$!
$! input files:
$   DIRW TMLNEXT:
$   DIRW C22EXT:
$   DIRW CTUEXT:
$   DIRW PRVNEW:
$!
$   DIRW/VERSION=5 EXE:CAP_BAL_BRX_81.*
$
$! switches: 	 3 = balance_files
$!		 5 = skip prvttls cnt errs
$!		 6 = exclude projected month
$!		 7 = exclude zero dollar records
$!		 8 = skip sna count errors
$!		11 = balance prvold
$!		12 = balance cap22/asc22
$!		13 = balance captmln/asctmln
$!		14 = balance capttls_totals
$!		15 = balance prvnew
$   IF CAP_TYPE .EQS. "S"			!currently the same
$     THEN
$!	DEFINE/USER COB$SWITCHES "3,5,8,12,13,14,15"
$	DEFINE/USER COB$SWITCHES "3,12,13,14,15"
$     ELSE
$	DEFINE/USER COB$SWITCHES "3,12,13,14,15"
$   ENDIF
$
$   RUN/NODEBUG EXE:CAP_BAL_BRX_81.EXE
$   SET NOON
$	SHOW SYM CAP_BAL_BRX_81_STATUS
$	SET FILE/TRUNC RPT1:;*
$	LOGDIR RPT1
$	TYPE PRGSTAT
$	DELETE/LOG PRGSTAT:;*
$   SET ON
$
$   GOSUB EMAIL_BALANCE_MSG
$   IF CAP_BAL_BRX_81_STATUS .NES. "I"
$   THEN
$	SAY "THIS JOB WILL ABORT BECAUSE THE REPORT IS OUT OF BALANCE."
$	SAY "PLEASE INVESTIGATE,"
$       ERR_MSG = ERR_MSG_BGN + -
		"PRVTTLS or ASCTTLS/''PFX'TMLN/''PFX'22 out of balance"
$       GOTO ERROR_CONDITION
$   ENDIF
$   GOTO WRAP_UP

$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! SUBROUTINES
$!!	these subroutines used to be called twice (for the old PRVTTLS
$!!	and the new PRVTTLS); now that old PRVTTLS have been eliminated,
$!!	they are called but once)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

$ EXTRACT_PRVEXT:
$   IF CAP_TYPE .EQS. "S"
$     THEN
$	DIRW ASC_TOTALS:ASCTTLS_*.ISM
$     ELSE
$	DIRW PRV_TOTALS:PRVTTLS_*.ISM
$   ENDIF
$
$! delete output files in case we need space
$   SET NOON
$	DELETE/LOG EXTOUT:;*
$   SET ON
$
$! we need 5 to 6 megablocks for the file, plus room for the sorted file
$   SIZE_NEEDED = 12000000
$   FREE_BLKS = F$GETDVI("EXTOUT", "FREEBLOCKS")		!free space
$   SAY "Estimated space needed for PRVEXT is ''SIZE_NEEDED'; " + -
	"''FREE_BLKS' blocks are available"
$   IF SIZE_NEEDED.GT. FREE_BLKS
$   THEN
$	ERROR_STATUS = "%X00000000"
$       ERR_MSG = ERR_MSG_BGN + "Not enough space for PRVEXT file."
$       GOTO ERROR_CONDITION
$   ENDIF
$!
$   DIRW/VERSION=5 EXE:CAP_BAL_BRX_81.*
$   DEFINE/USER COB$SWITCHES "1,''P5'"		! 1 = extract from old format
$						!	PRVTTLS/ASCTTLS
$						! 4 = exclude SNA rules
$   RUN/NODEBUG EXE:CAP_BAL_BRX_81.EXE
$   SET NOON
$	SET FILE/TRUNC EXTOUT:;*
$	LOGDIR EXTOUT
$	TYPE PRGSTAT
$	DELETE/LOG PRGSTAT:;*
$   SET ON
$   DEASSIGN/PROC ASC_TOTALS
$   DEASSIGN/PROC PRV_TOTALS
$   RETURN

$ SORT_PRVEXT:
$   FSP = F$SEARCH("PRVEXT")	!filespec to sort
$   IF FSP .EQS. ""
$   THEN
$	ERROR_STATUS = "%X10018290"
$       ERR_MSG = ERR_MSG_BGN + -
            "PRVEXT doesn't exist."
$       GOTO ERROR_CONDITION
$   ENDIF
$   FNM = F$PARSE(FSP,,,,"SYNTAX_ONLY") - -	!file name for temp sort version
	F$PARSE(FSP,,,"TYPE") - F$PARSE(FSP,,,"VERSION")
$!
$   DEFINE SORTFILE_TMP 'FNM'.TMP
$!
$!  Use Hypersort
$   DEFINE SORTSHR SYS$LIBRARY:HYPERSORT.EXE  
$   SORT/STAT/WORK_FILES=10/KEY=(POS:1,SIZ:40) -
	PRVEXT: -
	SORTFILE_TMP:
$   RENAME/LOG SORTFILE_TMP: PRVEXT:
$   PURGE/LOG PRVEXT
$   RETURN

$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! WRAP_UP -- End                                                 !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ WRAP_UP:
$   SHOW PROCESS/ACC
$   SET  NOVERIFY       ! turn verify off for @com:termination
$   @COM:TERMINATION
$   IF PV THEN SET VERIFY ! restore prior verify setting
$   EXIT 1
$!
$ EMAIL_BALANCE_MSG:
$   SET NOON
$   IF F$SEARCH("MSGFILE:") .NES. "" THEN DELETE/LOG  MSGFILE:;*
$   OPEN/WRITE MF MSGFILE:
$!
$   WRITE MF ""
$   WRITE MF "Hello CAP Team,"
$   WRITE MF ""
$   WRITE MF "The CAP_BAL_BRX_81 report is "
$   IF CAP_BAL_BRX_81_STATUS .EQS. "I"
$   THEN
$	WRITE MF "in balance for this capitation period."
$	WRITE MF "This means ''PFX'TMLN_NEW balances to ''PFX'22."
$   ELSE
$	WRITE MF "NOT in balance for this capitation period."
$	WRITE MF "This means ''PFX'TMLN_NEW does not balance to ''PFX'22."
$!	WRITE MF "''PFX' job stream is being aborted."
$	WRITE MF " "
$	WRITE MF "Please investigate."
$	WRITE MF "Most likely there is a problem in the balancing process."
$      EXIT_STAT :== "E"
$   ENDIF 
$   WRITE MF ""
$   WRITE MF F$FAO("!^")
$   CLOSE MF
$!
$   DIST_LIST = "@ACE_TEAM"
$   IF CAP_BAL_BRX_81_STATUS .EQS. "I"
$   THEN
$      HN_MAIL MSGFILE 'DIST_LIST' "CAP_BAL_BRX_81: ''PFX'22 IS IN BALANCE"
$   ELSE
$      HN_MAIL MSGFILE 'DIST_LIST' "CAP_BAL_BRX_81: ''PFX'22 IS OUT OF BALANCE"
$   ENDIF 
$   DELETE/LOG MSGFILE:;*
$!
$ RETURN
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! ERROR_CONDITION -- Error Handling                              !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ ERROR_CONDITION:
$   IF F$TYPE(ERROR_STATUS) .NES. "INTEGER" -
        THEN ERROR_STATUS = F$INTEGER($STATUS)
$   SHOW SYMBOL ERROR_STATUS
$   ERROR_STATUS = "%X" + F$FAO("!XL", ERROR_STATUS)
$   MESS = F$MESS(ERROR_STATUS)
$   IF "''ERR_MSG_RETURNED'" .NES. "" THEN ERR_MSG = ERR_MSG_RETURNED
$   SHOW SYMBOL MESS
$   SHOW SYMBOL ERR_MSG
$   MAIL_LST = "@OPR_TEAM, @ACE_TEAM"
$   SET  NOVERIFY ! turn verify off for @com:batch_error
$   @COM:BATCH_ERROR 'ERROR_STATUS' 'RUN_STATUS' "''MAIL_LST'" "''ERR_MSG'"
$   IF PV THEN SET VERIFY ! restore prior verify setting
$   EXIT 44
$!
$!-------------------------------------------------------------------------
$!                      End of Job CAP_BAL_BRX_81
$!-------------------------------------------------------------------------
