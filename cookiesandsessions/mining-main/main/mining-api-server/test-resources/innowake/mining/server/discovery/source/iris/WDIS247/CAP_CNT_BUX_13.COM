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
$!         JOB:	CAP_CNT_BUX_13
$!        DATE: 15-May-2017
$!      AUTHOR:	John Ruffner
$!    FUNCTION:	Merge Centene member data feed with ABS member files
$!
$!    OVERVIEW:	This procedure controls the three main functions of the 
$!		program CAP_CNT_BUX_13.EXE
$!		1) read and validate one or more UMVINTF files from Centene
$!		2) update the MBRFLAT file with UMV data
$!			a) read MBRFLAT_OLD from the previous week
$!			b) merge in the updates from UMVINTF
$!			c) write MBRFLAT_NEW which will become MBRFLAT_OLD
$!				the following week
$!		3) merge MBRFLAT_NEW with nine or so ABS member files
$!			a) read one or more ABS files in parallel with 
$!				MBRFLAT_NEW
$!			b) merge the data
$!			c) write out a sequential version of the abs file
$!			d) convert the sequential abs file into an indexed
$!				version in the frozen file area CAP01_ABS_DATA:
$!		
$!		All the functions in CAP_CNT_BUX_13.EXE are controlled by
$!		COB$SWITCHES, and there's a lot of code in the command procedure
$!		to set the switches and figure out which file(s) are being
$!		processed in different steps.
$!		
$!		Step 1:
$!		This step runs CAP_CNT_BUX_13.EXE, but what the program will
$!		do depends on the COB$SWITCHES that have been set.  When this
$!		job runs normally through the SCHEDULER, you'll see in step 1
$!		only switch 1 is set, which causes the program to read UMVINTF
$!		and update MBRFLAT.
$!		However, this step is also used in all the tasks started by
$!		the ICTASK scheduler to run the ABS merge function for one or
$!		more abs files (see below)
$!		
$!		Step 2:
$!		When step 1 processes the umv file, step 2 does nothing;
$!		when step 1 performs the merge function, step 2 takes care of
$!		converting the sequential output file into an indexed file in
$!		the frozen file area.
$!		
$!		Step 3:
$!		When step 1 processes the umv file, step 3 creates a task status
$!		file that will drive the IC scheduler tasks.  For each switch
$!		set in P4, there will be a separate task (each switch controls
$!		one or two abs files).
$!		Step 3 also saves the task status file in cap checkpoint 2 for
$!		use in reruns
$!		
$!		Step 4 restores the task status file from checkpoint 2 (even if
$!		it's not a rerun -- it's just simpler that way)
$!		
$!		Step 5 invokes the IC task scheduler which submits a task
$!		for each item in the task status file (remember each task is
$!		controled by a numeric switch (currently 3 through 11 but we'll
$!		need at least one more when we add Medi-cal).
$!		
$!		Each task runs this same command procedure, but since we can
$!		only pass it one parameter from the task scheduler, we pass
$!		the cob$switch indicating the abs file(s) we want to process.
$!		Then before we even get to step 1, we actually invoke this
$!		command procedure again (with an '@') and pass it the
$!		cob$switch in P3 where this procedure is expecting it.
$!		The inner invocation is the one that will actually merge 
$!		the MBRFLAT file with the ABS file(s) and then convert the 
$!		sequential output to indexed format.
$!		
$!		an example task log would be:
$!			CAP_TSKLOG:SCCB1301CMC6$01C.TSKLOG
$!		
$!     RESTART:	This procedure can always be restarted from the top,
$!		with no user intervention required (other than fixing
$!		the error); OR, it can be restarted at the same step
$!		where it blew off, also without user intervention.
$!
$! MODIFICATION HISTORY:
$!	MOD:	<DD-MMM-YYYY>     <ISR# MOD# >     <USER-ID>
$!	
$! mod#03 -- 20180514 INC2188240 Centene 2.0 C4M-JOHN RUFFNER (ver 2.03)
$!	fixed error introduced with mod#02 (search for mod#03 for more info)
$!	
$! mod#02 -- 20180423 INC2165208 Centene 2.0 C4M-JOHN RUFFNER (ver 2.02)
$!	Adding code to determine if UMV sent us full files (spread out by
$!	business unit), or one consolidated full file, or just a change file
$!	
$! mod#01 -- 20171218 INC1944993 Centene 2.0 C4M-JOHN RUFFNER (ver 2.01)
$!	adding code to override EXE when needed to rerun failed tasks
$!
$! mod#00 -- 20170515 PID 21993/22126 Centene 2.0 C4M-JOHN RUFFNER (ver 2.00)
$!	initial version
$!
$! End Documentation
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! STEP_0  Initialization - Initialize local symbols and logicals !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ INITIALIZATION:
$   IF PV THEN SET VERIFY
$	ON WARNING THEN GOTO ERROR_CONDITION
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$
$   SHOW PROCESS
$   SHOW DEFAULT
$   EID = F$TRNLNM("ENVIRONMENT_ID")
$   THIS_COMFILE = F$ENVIRONMENT("PROCEDURE")       ! This job's filespec
$   JOB_NAME	 = F$PARSE(THIS_COMFILE,,,"NAME")   ! Parse out the name
$   PID		 = F$GETJPI("","PID")               ! Handy for temp filenames
$   PRCNAM	 = F$GETJPI("","PRCNAM")
$   NODENAME	 = F$GETJPI("","NODENAME")
$   JOBTYPE	 = F$GETJPI("","JOBTYPE")
$   NODE_VERSION = F$GETJPI("","NODE_VERSION")
$   USERNAME	 = F$GETJPI("","USERNAME")
$   USERID	 = F$EDIT(F$GETJPI("","USERNAME"), "COLLAPSE")
$   @COM:INITIALIZATION
$
$   SAY  := WRITE SYS$OUTPUT
$   SAY ""
$   SAY ""
$   SAY "  Command procedure=''THIS_COMFILE'"
$   SAY "                PID=''PID'"
$   SAY "       Process Name=''PRCNAM'"
$   SAY "          Node Name=''NODENAME'"
$   SAY "           Job Type=''JOBTYPE'"
$   SAY "        VMS Version=''NODE_VERSION'"
$   SAY "  Username(account)=''USERNAME'"
$   SAY "     Procedure name=''JOB_NAME'"
$   SAY " "
$   SAY " P1 (region/product)                 =''p1'"
$   SAY " P2 (start step)                     =''p2'"
$   SAY " P3 (cob$switches for step 1)        =''p3'"
$   SAY " P4 (cob$switches for step 3)        =''p4'"
$   SAY " P5 (start step override - optional) =''p5'"
$   SAY " P6 (step1 switches override - opt)  =''p6'"
$   SAY " P7 (step3 switches override - opt)  =''p7'"
$! mod#02 begin
$   SAY " P8 (for anything else - opt)        =''p8'"
$! mod#02 end
$   SAY " "
$
$   RUN_STATUS	 = "0"
$   CR[0,8]	 = 13
$   LF[0,8]	 = 10
$   CRLF	 = CR + LF  ! Carriage return/linefeed for e-mail text
$   ERR_MSG_BGN	 = "ERROR ENCOUNTERED WHILE RUNNING " + JOB_NAME + CRLF
$   ERR_MSG	 = ERR_MSG_BGN
$   DIRW	 = "DIRECTORY/SIZ=ALL/DATE/WID=(FILE=36,SIZ=8)"
$   MAIL_LST = "@OPR_TEAM, @ACE_TEAM"
$
$   SET RMS/QUERY=DISABLE
$   SH RMS
$
$!
$   IF PV THEN SET VERIFY
$! constants:
$! mod#02/mod#03 begin
$   CNST0 := 0
$   CNST1 := 1
$   CNST_2SECS = "0:0:2"
$   CNST_30DAYS = "-30-0:0:0"
$   CNST_4DAYS = "-4-0:0:0"
$   CNST_90DAYS = "-90-0:0:0"
$   CNST_ABORT_A := A
$   CNST_ALL_A := A
$   CNST_ALQ := ALQ
$   CNST_AL := AL
$   CNST_BU := BU
$   CNST_CCB13 := CCB13
$   CNST_COMMA = ","
$   CNST_DUMMY_D := D		! used to pass dummy P5, P6, P7 parameters
$   CNST_EXCLM = "!"
$   CNSTE := E
$   CNSTENDED := ENDED
$   CNST_ERR_FNF = "%X10018290"
$   CNST_ERR_NORM = "%X00000000"
$   CNST_SUCCESS = "%X00000001"
$   CNST_FREEBLOCKS := FREEBLOCKS
$   CNST_ICTSK	:= ICTSK
$   CNST_ITERATIONS_LIMIT = 6
$   CNST_PRD := "PRD"
$   CNST_PX = "%X"
$   CNST_XL = "!XL"
$   CNST_INTEGER := INTEGER
$   CNST_NUL = ""
$   CNST_PRIO1	:= 0001
$   CNST_STEP3_ALL_SWITCHES = "2,3,4,5,6,7,8,9,10,11"
$   CNST_SW2	:= 2
$   CNST_SKIP_WAIT := SKIP_WAIT
$   CNST_TEST_WAIT := TEST_WAIT
$   CNST_TRUE	:= T
$   CNST_FALSE	:= F
$   MODE_I	:= INTERACTIVE
$   NLA0	:= _NLA0:
$! mod#02/mod#03 end
$
$   IF P1 .EQS. CNST_NUL
$   THEN
$	ERROR_STATUS = CNST_ERR_NORM
$	ERR_MSG = ERR_MSG_BGN + -
		"Required parameter(s) is(are) missing. " + CRLF + -
		"Please re-submit this job with required parameter(s)"
$	GOTO ERROR_CONDITION
$   ENDIF
$
$!
$   IF F$EXTRACT(0,5,P1) .EQS. CNST_CCB13
$   THEN
$! this procedure was invoked by the ictask scheduler (not the DEC scheduler)
$! so we are inside a task and need to process one abs file, based on the
$! cob$switch -- so we'll just call this procedure again with the parameters
$! to go to step 1
$	@'THIS_COMFILE 'F$EXTRACT(5,5,P1) 1 2,'F$EXTRACT(10,2,P1)
$	GOTO WRAP_UP
$   ENDIF
$
$   REGN_CODE = F$EXTRACT(0,2,P1)
$   PRODUCT_LINE = F$EXTRACT(2,2,P1)
$   CAP_TYPE = F$EXTRACT(4,1,P1)
$   MAX_ERRORS := "000010000"
$
$   @COM:CAP_UTL_BRX_01.COM "''REGN_CODE'" "''PRODUCT_LINE'" "''CAP_TYPE'"
$
$   IF CAPREGN_RUN_STATUS .EQS. CNSTENDED
$   THEN
$	ERROR_STATUS = CNST_ERR_NORM
$	ERR_MSG = ERR_MSG_BGN + -
		"HNCAP Batch Cycle has been ended. " + CRLF + -
		"If you have to rerun, follow the rerun instructions."
$	GOTO ERROR_CONDITION
$   ENDIF
$
$! mod#02 begin
$   IF  P5 .NES. CNST_NUL .AND. P5 .NES. CNST_DUMMY_D THEN -
				P2 = P5		! override start step
$   IF  P6 .NES. CNST_NUL .AND. P5 .NES. CNST_DUMMY_D THEN -
				P3 = P6		! override step 1 switches
$   IF  P7 .NES. CNST_NUL .AND. P5 .NES. CNST_DUMMY_D THEN -
				P4 = P7		! override step 3 switches
$! mod#02 end
$   IF "''P4'" .EQS. CNST_ALL_A
$   THEN
$	P4 = CNST_STEP3_ALL_SWITCHES
$   ENDIF
$
$!
$! we have to unslot CAPREFCOP since want to look at the live ABS files
$   @ICI_MANAGER:SLOT "-CAPREFCOP_''REGN_CODE'_LOGICALS"
$   @ICI_MANAGER:SLOT "CAPACMS_LOGICALS_''REGN_CODE'"
$! in the original version we were looking at tables_master to get rmc
$! information -- now it comes from cntrmcs_file; but i'm leaving this define
$! here anyway because, if we need tables_master in the future, you'll have
$! to decide if you want to look at the live version, or the copy from the 
$! prior week (remember, we haven't extracted and updated tables_master
$! for the current week at the time this job runs)
$! 20171026 - we need the live version with the latest 
$! race/ethnicity/language/codes (define to CAP01_TABLES_MASTER: removed)
$
$! mod#03 begin
$   DEFINE UMVCHG_SAV UMVWORK:UMVINTF*.CHS
$! mod#03 end
$! mod#02 begin
$   DEFINE CCB13_BUPRM_FILE CAPDATA_DSK1:CCB13_BUPRM.TXT
$! mod#02 end
$   DEFINE CCB13_PARM_FILE CAPDATA_DSK1:CCB13.TXT
$   DEFINE CNTRMCS_FILE CAPDATA_DSK1:CNTRMCS.TXT
$   DEFINE UMVINTF_TRIGGER_FILE UMVWORK:UMVINTF.TRIGGER
$   DEFINE UMVINTF_TRIGGER_DONE UMVWORK:UMVINTF.TRIGDONE
$   DEFINE UMVINTF_FILESPEC UMVWORK:UMVINTF*.TXT
$   DEFINE INPUT_LIST_FILE CAPWORK:UMVINPT.SEQ
$   DEFINE MBRFLAT_OLD_FILE UMVWORK:MBRFLAT.'CAPREGN_PREV_CAP_RUN_DATE
$   DEFINE MBRFLAT_NEW_FILE UMVWORK:MBRFLAT.'CAPREGN_CAP_RUN_DATE
$   DEFINE CAPAUDT_FILE UMVWORK:CAPAUDT.SEQ
$   DEFINE ICTSK_PARM_FILE CAPWORK:CCB13_ICTSK.TXT
$   DEFINE TASK_STATUS_FILE CAPWORK:CCB13_TSKSTAT.ISM
$   DEFINE TASK_STATUS_SAVE CAP_CHECKPOINT_2:CCB13_TSKSTAT.ISM
$   DEFINE PRGSTAT CAPWORK:CAP_CNT_BUX_13_'P3'.STAT
$
$! set a flag for each cob$switch; first set all to false, just in case
$! there might be one set already
$   DEFINE COB$SWITCHES "''P3'"
$   SW = 1
$ SWITCH1_LOOP:
$   RUN_SW'SW = CNST_FALSE
$   SW = SW + 1
$   IF SW .GT. 16 THEN GOTO SWITCH1_LOOP_END
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$   GOTO SWITCH1_LOOP

$ SWITCH1_LOOP_END:
$   IF PV THEN SET VERIFY
$   SHOW SYMBOL RUN_SW*
$
$! now change the flag to true for each switch in P3, so we know which steps
$! we're performing (see the list in CAP_REFCOP:CAP_UTL_BUX_13.COB)
$   ELM = 0
$ SWITCH2_LOOP:
$   RUN_SWITCH = F$ELEMENT('ELM,CNST_COMMA,P3)
$   IF RUN_SWITCH .EQS. CNST_COMMA THEN GOTO SWITCH2_LOOP_END
$   RUN_SW'RUN_SWITCH = CNST_TRUE
$   ELM = ELM + 1
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$   GOTO SWITCH2_LOOP

$ SWITCH2_LOOP_END:
$   IF PV THEN SET VERIFY
$   SHOW SYMBOL RUN_SW*
$
$! now use the parameter file to set switches and logicals for all the files
$   SET NOON
$	CLOSE CPF
$   SET ON
$   OPEN/SHARED/READ CPF CCB13_PARM_FILE
$ DEF_LOOP:
$   READ CPF CPR /END=DEF_LOOP_END
$! skip comments:
$   IF F$EXTRACT(0,1,CPR) .EQS. CNST_EXCLM THEN GOTO DEF_LOOP
$   GOSUB PARSE_CPR
$   SHOW SYM CPR_*
$! input and output for the scheduler tasks (note that we are processing
$! two hicssn files, using the same logical names, so we have to use
$! cpr_switch to make the physical name of the output file unique;
$! furthermore, we have to check the run switch so we don't override
$! the definition for HICSSNX_IN_FILE and HICSSNX_OUT_FILE (switch 10) when 
$! we get to switch 11):
$   IF RUN_SW'CPR_SWITCH -
    .OR. F$TRNLNM("''CPR_PREFIX'_IN_FILE") .EQS. CNST_NUL
$   THEN
$	DEFINE/LOG 'CPR_PREFIX'_IN_FILE 'CPR_SRC
$	DEFINE/LOG 'CPR_PREFIX'_OUT_FILE CAPWORK:'CPR_PREFIX'.OUT'CPR_SWITCH'
$   ENDIF
$   DIRW 'CPR_PREFIX'_IN_FILE
$   DIRW 'CPR_FDL'
$   IF F$SEARCH("''CPR_SRC'") .EQS. CNST_NUL -
    .OR. F$SEARCH("''CPR_FDL'") .EQS. CNST_NUL
$   THEN
$	ERROR_STATUS = CNST_ERR_FNF
$       ERR_MSG = ERR_MSG_BGN + "''CPR_SRC' or ''CPR_FDL' missing."
$       GOTO ERROR_CONDITION
$   ENDIF
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$   GOTO DEF_LOOP

$ DEF_LOOP_END:
$   IF PV THEN SET VERIFY
$   CLOSE CPF
$
$   IF  P2 .EQS. CNSTE THEN EXIT
$   IF  P2 .EQS. CNST_NUL THEN P2 = CNST_ICTSK
$   IF  P2 .EQS. CNST0 THEN P2 = CNST1
$   SEARCH/NOLOG 'THIS_COMFILE' "STEP_''P2':"   ! Verify restart param
$   IF  $STATUS .NE. CNST_SUCCESS
$   THEN
$	ERROR_STATUS = $STATUS
$	ERR_MSG = ERR_MSG_BGN + -
		"STEP_''P2' doesn't exist."
$           GOTO ERROR_CONDITION
$   ENDIF
$   GOTO STEP_'P2'

$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 1 - run CAP_UTL_BUX_13 to process files according to the
$!		cob$switches
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_1:
$   RUN_STATUS = "1"
$   SHOW PROCESS/ACC
$
$   IF RUN_SW1
$   THEN
$! process input file from centene 
$! mod#02 begin
$	IF EID .EQS. CNST_PRD .OR. "''P8'" .EQS. CNST_TEST_WAIT
$	THEN
$	    IF "''P8'" .NES. CNST_SKIP_WAIT
$	    THEN
$! wait until the umvintf file(s) is/are loaded into umvwork:
$		GOSUB WAIT_FOR_UMVINTF_FILE
$	    ENDIF
$	ENDIF
$! mod#02 end
$	DIRW UMVINTF_FILESPEC
$	IF F$SEARCH("UMVINTF_FILESPEC") .EQS. CNST_NUL
$	THEN
$		ERROR_STATUS = CNST_ERR_FNF
$       	ERR_MSG = ERR_MSG_BGN + "CENTENE input file(s) missing."
$       	GOTO ERROR_CONDITION
$	ENDIF
$	DIRE/NOHEAD UMVINTF_FILESPEC: /OUT=INPUT_LIST_FILE
$! input_list_file now contains the filespecs for one or more umvintf files
$	LOGDIR INPUT_LIST_FILE
$	TYPE INPUT_LIST_FILE
$	SET NOON
$		PURGE/LOG/BEF='CNST_90DAYS INPUT_LIST_FILE
$	SET ON
$
$	LOGDIR MBRFLAT_OLD_FILE
$	IF F$SEARCH("MBRFLAT_OLD_FILE") .EQS. CNST_NUL
$	THEN
$	    ERROR_STATUS = CNST_ERR_FNF
$           ERR_MSG = ERR_MSG_BGN + "MBRFLAT_OLD_FILE missing."
$           GOTO ERROR_CONDITION
$	ENDIF
$
$	IF F$SEARCH("MBRFLAT_NEW_FILE") .NES. CNST_NUL
$	THEN
$		LOGDIR MBRFLAT_NEW_FILE
$		DELETE/LOG MBRFLAT_NEW_FILE:;*
$	ENDIF
$   ELSE
$! we are going to merge mbrflat with the abs member files
$	LOGDIR MBRFLAT_NEW_FILE
$	IF F$SEARCH("MBRFLAT_NEW_FILE") .EQS. CNST_NUL
$	THEN
$	    ERROR_STATUS = CNST_ERR_FNF
$           ERR_MSG = ERR_MSG_BGN + "MBRFLAT_NEW_FILE missing."
$           GOTO ERROR_CONDITION
$	ENDIF
$   ENDIF
$   LOGDIR GROUP_MASTER
$
$! mod#01
$   OVERRIDE_FILE = F$SEARCH("UMVWORK:CAP_CNT_BUX_13.TMP")
$   IF OVERRIDE_FILE .NES. CNST_NUL
$   THEN
$	CREATE_DATE_VMS = F$FILE(OVERRIDE_FILE,"CDT")
$	CREATE_DATE = F$CVT(CREATE_DATE_VMS)
$	THREE_DAYS_AGO = F$CVT("-3-")
$	IF CREATE_DATE .GTS. THREE_DAYS_AGO
$	THEN
$	    @'OVERRIDE_FILE
$! normally the override file should just redefine EXE and exit, but if for
$! some reason you want it to take over all the functions of this procedure
$! and run the reports, then you can set the EXIT_AFTER_OVERRIDE flag to Y and
$! this procedure will immediately exit
$	    IF "''EXIT_AFTER_OVERRIDE'"
$	    THEN
$		SAY "OVERRIDE FILE REQUESTED EXIT"
$		GOTO WRAP_UP
$	    ENDIF
$	ENDIF
$   ENDIF
$! mod#01 end
$
$   SHOW LOGICAL COB$SWITCHES
$   DIRW/VERSION=5 EXE:CAP_CNT_BUX_13.*
$   RUN/NODEBUG EXE:CAP_CNT_BUX_13.EXE
$   SET NOON
$	TYPE PRGSTAT
$	PURGE PRGSTAT:;*
$   SET ON
$   IF RUN_SW1	! updating mbrflat_file
$   THEN
$! mod#02 begin - make sure there are no trigger files left over
$	IF EID .EQS. CNST_PRD
$	THEN
$		DELETE/LOG UMVINTF_TRIGGER_FILE:;*
$	ENDIF
$! mod#02 end
$	RENAME/LOG UMVINTF_FILESPEC: *.'PID
$	SET FILE/TRUNC MBRFLAT_NEW_FILE
$	LOGDIR MBRFLAT_NEW_FILE
$	SET FILE/TRUNC CAPAUDT_FILE
$	LOGDIR CAPAUDT_FILE
$   ENDIF
$
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 2 -	Convert output file into cap frozen area
$!!			(based on the cob$switches parameter)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_2:
$   RUN_STATUS = "2"
$   SHOW PROCESS/ACC
$
$! convert the output files
$   SET NOON
$	CLOSE CPF
$   SET ON
$   OPEN/SHARED/READ CPF CCB13_PARM_FILE
$ CVT_LOOP:
$   READ CPF CPR /END=CVT_LOOP_END
$   IF F$EXTRACT(0,1,CPR) .EQS. CNST_EXCLM THEN GOTO CVT_LOOP
$   GOSUB PARSE_CPR
$   IF RUN_SW'CPR_SWITCH
$   THEN
$! parse the filename - fdn = directory to name (no extension)
$	@COM:CAP_UTL_BRX_007 PARSE 'CPR_DEST CCB$
$
$	SET FILE/TRUNC 'CPR_PREFIX'_OUT_FILE
$	LOGDIR 'CPR_PREFIX'_OUT_FILE
$	SET NOON
$		PURGE/LOG 'CPR_PREFIX'_OUT_FILE
$		DELETE/LOG 'CCB$FDN'.EXC;*
$	SET ON
$	CONVERT/FAST/STAT/FDL='CPR_FDL 'CPR_PREFIX'_OUT_FILE 'CCB$FDN'.TMP -
		/EXCEPT='CCB$FDN'.EXC
$! for most files if we find duplicate records we abort, because it means
$! there's a logic error, but for DLQMSTR, we expect duplicates and DON'T abort
$! note: before we filtered out unneeded records from mbrtime, we had TONS of
$! duplicates, but now in testing I don't see any; however there's no 
$! guarantee we won't have them in the future, so i'm leaving the file as
$! 'duplicates allowed'
$	IF F$FILE("''CCB$FDN'.EXC","EOF") .NES. 0
$	THEN
$		WRITE SYS$OUTPUT "CONVERT ERRORS: "
$		DIRW 'CCB$FDN'.EXC
$       	ERR_MSG = ERR_MSG_BGN + "convert exceptions."
$		IF CPR_ABORT .EQS. CNST_ABORT_A
$		THEN
$		    GOTO ERROR_CONDITION
$		ENDIF
$	ENDIF
$	RENAME/LOG 'CCB$FDN'.TMP 'CPR_DEST
$	LOGDIR 'CPR_DEST
$	SET NOON
$		PURGE/LOG 'CPR_DEST
$	SET ON
$	IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$   ENDIF
$   GOTO CVT_LOOP

$ CVT_LOOP_END:
$   IF PV THEN SET VERIFY
$   CLOSE CPF
$   IF "''P4'" .EQS. CNST_NUL THEN GOTO END_ICTSK_PROCESSING
$
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 3 -	ICTSK A - create and save task status file
$!!			(based on switches passed in P4)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_3:
$   RUN_STATUS = "3"
$   SHOW PROCESS/ACC
$
$! make a parameter file for input to ACE_UTL_BUX_00
$   SET NOON
$	CLOSE IPF
$   SET ON
$   OPEN/WRITE IPF ICTSK_PARM_FILE
$   ELM = 0
$ ICT_LOOP:
$   ICT_SWITCH = F$ELEMENT('ELM,CNST_COMMA,P4)
$   IF ICT_SWITCH .EQS. CNST_COMMA THEN GOTO ICT_LOOP_END
$! switch two is needed in all tasks to tell cap_cnt_bux_13 to merge abs
$! files, but we don't want to create a separate task for switch 2
$   IF ICT_SWITCH .NES. CNST_SW2 
$   THEN 
$	WRITE IPF CNST_CCB13 + P1 + ICT_SWITCH + CNST_COMMA + CNST_PRIO1
$   ENDIF
$   ELM = ELM + 1
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$   GOTO ICT_LOOP

$ ICT_LOOP_END:
$   IF PV THEN SET VERIFY
$   CLOSE IPF
$   LOGDIR ICTSK_PARM_FILE
$   TYPE ICTSK_PARM_FILE
$   SET NOON
$	PURGE/LOG/KEEP=3 ICTSK_PARM_FILE
$   SET ON
$
$! use existing ACE_UTL_BUD_00 program to generate task status file
$! it also generates task step files which we don't need
$   DEFINE FILES_TO_COPY	ICTSK_PARM_FILE:
$   CREATE/FDL=FDL:SYSTSKS.FDL TASK_STATUS_FILE:
$   RUN/NODEBUG EXE:ACE_UTL_BUD_00
$   LOGDIR TASK_STATUS_FILE
$   BACKUP/LOG/IGNORE=INTERLOCK TASK_STATUS_FILE:; TASK_STATUS_SAVE:;
$   SET NOON
$	PURGE/LOG/KEEP=2 TASK_STATUS_SAVE:
$   SET ON
$
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 4 -	ICTSK B - restore task status file (in case of rerun)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_4:
$   RUN_STATUS = "4"
$   SHOW PROCESS/ACC
$
$   BACKUP/LOG/IGNORE=INTERLOCK TASK_STATUS_SAVE:; TASK_STATUS_FILE:;
$   SET NOON
$	PURGE/LOG/KEEP=3 TASK_STATUS_FILE:
$   SET ON
$
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 5 -	ICTSK C - invoke task scheduler to submit tasks
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_5:
$   RUN_STATUS = "5"
$   SHOW PROCESS/ACC
$
$   LOGDIR TASK_STATUS_FILE
$
$   SHOW LOGICAL WORK
$   DIRW EXE:ICTSKSCH
$! tasks will be submitted to a batch queue with the command to 
$! execute CAP_CNT_BUX_13.COM
$   RUN/NODEB EXE:ICTSKSCH
CAP_CNT_BUX_13,2,1
*,0,0
$
$ END_ICTSK_PROCESSING:
$
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 6 - clean up
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_6:
$   RUN_STATUS = "6"
$   SHOW PROCESS/ACC
$
$   SET NOON
$	DELETE/LOG/BEF='CNST_90DAYS UMVWORK:MBRFLAT*.*;*
$	DELETE/LOG/BEF='CNST_90DAYS UMVWORK:UMVINTF*.*;*
$	DELETE/LOG/BEF='CNST_30DAYS UMVWORK:CAPAUDT*.*;*
$	PURGE/LOG/KEEP=3 WORK:SCCB13*.*
$   SET ON
$
$   GOTO WRAP_UP

$ PARSE_CPR:
$! parse out elements of the ccb13_parm_record
$   CPR_SWITCH = F$ELEMENT(0,CNST_COMMA,CPR)
$   CPR_SRC = F$ELEMENT(1,CNST_COMMA,CPR)
$   CPR_DEST = F$ELEMENT(2,CNST_COMMA,CPR)
$   CPR_PREFIX = F$ELEMENT(3,CNST_COMMA,CPR)
$   CPR_FDL = F$ELEMENT(4,CNST_COMMA,CPR)
$   CPR_ABORT = F$ELEMENT(5,CNST_COMMA,CPR)
$   RETURN

$! mod#02 begin
$!
$ STEP_TEST_WAIT:
$   GOSUB WAIT_FOR_UMVINTF_FILE
$   EXIT

$ WAIT_FOR_UMVINTF_FILE:
$! in case this is a restart, we always have to have a change file, but it
$! may have been renamed, so rename it back, if found
$
$! mod#03 - with mod#02 we introduced a bug whereby on a normal run (first time
$! through) we renamed the saved change file from the previous week back to .txt
$! and since it had no corresponding trigger file, the job failed -- now we'll
$! "archive" any saved change files over four days old, and then if there are
$! any left, we'll rename them to .TXT
$! 
$   SET NOON
$	RENAME/LOG UMVCHG_SAV:;*/BEF='CNST_4DAYS/CREATE UMVWORK:*.ARC;
$	RENAME/LOG UMVCHG_SAV:;* UMVWORK:*.TXT;*
$   SET ON
$! mod#03 end
$
$   ITERATIONS = 0
$   FILES_READY = CNST_FALSE
$
$ WAIT_FOR_UMVINTF_FILES_LOOP:
$   GOSUB WAIT_FOR_UMVINTF_FILES_RTN
$   IF FILES_READY THEN RETURN
$   ITERATIONS = ITERATIONS + 1
$   IF ITERATIONS .GT. CNST_ITERATIONS_LIMIT
$   THEN
$	WRITE SYS$OUTPUT "UMVINTF files not ready; see errors in log"
$	GOTO ERROR_CONDITION
$   ENDIF
$   IF EID .EQS. CNST_PRD
$   THEN
$	WAIT 00:05:00
$   ELSE
$	WAIT 00:00:15
$   ENDIF
$   GOTO WAIT_FOR_UMVINTF_FILES_LOOP

$ WAIT_FOR_UMVINTF_FILES_RTN:
$! wait until the umvintf file(s) is/are loaded into umvwork:
$! first determine if they sent files by business unit (BU)
$   SET NOON
$	PURGE/LOG UMVINTF_FILESPEC:
$	DIRW UMVINTF_TRIGGER_FILE
$	DIRW UMVINTF_FILESPEC
$   SET ON
$   ALCNT = 0	! one full file with all BUs (indicated by AL in the filename)
$   BUCNT = 0	! full files by BU (indicated by BU and not AL in filename)
$   CFCNT = 0	! change files
$ WAIT_LOOP_1:
$   UFL = F$SEARCH("UMVINTF_FILESPEC:",1)
$   IF UFL .EQS. CNST_NUL THEN GOTO WAIT_LOOP_1_END
$   IF F$LOCATE(CNST_AL, UFL) .LT. F$LENGTH(UFL)
$   THEN
$	ALCNT = ALCNT + 1
$   ELSE
$	IF F$LOCATE(CNST_BU, UFL) .LT. F$LENGTH(UFL)
$	THEN
$	   BUCNT = BUCNT + 1
$	ELSE
$	   CFCNT = CFCNT + 1
$	   CFL = UFL
$	ENDIF
$   ENDIF
$   GOTO WAIT_LOOP_1

$ WAIT_LOOP_1_END:
$
$! make sure there are no downloads in progress -- there should be a 
$! trigger file for each umvintf file (EFT team was not able to create one
$! trigger file at the end of the job which would have made things MUCH easier)
$   TRIGCNT = 0
$ WAIT_LOOP_2:
$   TFL = F$SEARCH("UMVINTF_TRIGGER_FILE:;*")
$   IF TFL .EQS. CNST_NUL THEN GOTO WAIT_LOOP_2_END
$   TRIGCNT = TRIGCNT + 1
$   GOTO WAIT_LOOP_2

$ WAIT_LOOP_2_END:
$   UFCNT = BUCNT + CFCNT + ALCNT
$   IF TRIGCNT .GT. UFCNT
$   THEN
$	WRITE SYS$OUTPUT "Too many trigger files"
$	TMPNBR = UFCNT
$	WRITE SYS$OUTPUT "Should be no more than: ''TMPNBR'"
$	GOTO ERROR_CONDITION
$   ENDIF
$   IF TRIGCNT .LT. (UFCNT - 1)
$   THEN
$	WRITE SYS$OUTPUT "Some trigger files are missing"
$	TMPNBR = UFCNT - 1
$	WRITE SYS$OUTPUT "Should be at least: ''TMPNBR'"
$	GOTO ERROR_CONDITION
$   ENDIF
$   IF TRIGCNT .LT. UFCNT
$   THEN
$	WRITE SYS$OUTPUT "Looks like a file is still downloading"
$	TMPNBR = UFCNT
$	WRITE SYS$OUTPUT "Should be: ''TMPNBR' trigger files"
$	RETURN
$   ENDIF
$! all the umfintf files we currently see are fully downloaded (no downloads
$! in progress -- but there could be more files coming)
$
$! there should always be one change file
$    IF CFCNT .LT. 1
$    THEN
$! we have to keep waiting
$	RETURN
$    ENDIF
$    IF CFCNT .GT. 1
$    THEN
$	WRITE SYS$OUTPUT "UMV sent too many change files"
$	GOTO ERROR_CONDITION
$    ENDIF
$! we have one change file, now we can check for full files
$
$   IF BUCNT .GT. 0
$   THEN
$! they did send files by BU; do we have the right number?
$! get the current number of BUs from the business unit parameter file
$	SET NOON
$	    CLOSE BUPFL
$	SET ON
$	DIRW CCB13_BUPRM_FILE
$	TYPE CCB13_BUPRM_FILE
$	OPEN/SHARED/READ BUPFL CCB13_BUPRM_FILE
$	READ BUPFL BUPRC /END=BUPRM_EMPTY
$	CLOSE BUPFL
$	BUNBR = F$INTEGER(BUPRC)
$	IF BUNBR .LE. 0
$	THEN
$	    WRITE SYS$OUTPUT "Number of BUs in CCB13_BUPRM_FILE invalid"
$	    GOTO ERROR_CONDITION
$	ENDIF
$	IF BUNBR .LT. BUCNT
$	THEN
$	    WRITE SYS$OUTPUT "UMV sent too many files: ''BUCNT' vs ''BUNBR'"
$	    GOTO ERROR_CONDITION
$	ENDIF
$	IF BUNBR .GT. BUCNT
$	THEN
$	    WRITE SYS$OUTPUT "Wait for more BU files: ''BUCNT' vs ''BUNBR'"
$	    RETURN
$	ENDIF
$! we have the right number of BU files; make sure they didn't send
$! one combined full file as well
$	IF ALCNT .GT. 0
$	THEN
$	    WRITE SYS$OUTPUT "UMV sent BU files and a combined full file"
$	    GOTO ERROR_CONDITION
$	ENDIF
$! we have the right number of full files
$	GOTO FULL_FILES_SENT
$   ELSE
$! no BU files, see if they sent one combined full file
$	IF ALCNT .GT. 0
$	THEN
$	    IF ALCNT .GT. 1
$	    THEN
$		WRITE SYS$OUTPUT "UMV sent too many combined full files"
$		GOTO ERROR_CONDITION
$	    ENDIF
$! we have one combined full file
$	    GOTO FULL_FILES_SENT
$	ELSE
$! no full files, so we'll process the change file
$	    FILES_READY = CNST_TRUE
$	    RETURN
$	ENDIF
$   ENDIF

$ FULL_FILES_SENT:
$! we have the right number of BU files; rename the change file so it doesn't
$! get processed, and create an empty MBRFLAT_OLD_FILE (we have to wipe out
$! all existing history; the full file(s) will replace everything)
$    RENAME/LOG 'CFL *.CHS;
$    CREATE MBRFLAT_OLD_FILE
$    FILES_READY = CNST_TRUE
$    RETURN


$ WAIT_FOR_UMVINTF_FILE_OLD_CODE:
$! mod#02 end
$! wait until the umvintf file(s) is/are loaded into umvwork:
$   IF F$SEARCH("UMVINTF_TRIGGER_FILE") .NES. CNST_NUL
$   THEN
$! mod#01
$	IF PV THEN SET VERIFY ! restore prior verify setting
$! mod#01 end
$	LOGDIR UMVINTF_TRIGGER_FILE
$	RETURN
$   ENDIF
$   WAIT 'CNST_2SECS
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$   GOTO WAIT_FOR_UMVINTF_FILE

$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! WRAP_UP -- End                                                 !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ WRAP_UP:
$   SHOW PROCESS/ACC
$! reseting the logical names is not necessary during a cap run; but we
$! include it here in case you want to run it interactively during testing
$   @ICI_MANAGER:SLOT "-CAPACMS_LOGICALS_''REGN_CODE'"
$   @ICI_MANAGER:SLOT "CAPREFCOP_''REGN_CODE'_LOGICALS"
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY ! turn verify off for term
$   @COM:TERMINATION
$   IF PV THEN SET VERIFY ! restore prior verify setting
$   EXIT 1

$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! ERROR_CONDITION -- Error Handling                              !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ ERROR_CONDITION:
$   IF F$TYPE(ERROR_STATUS) .NES. CNST_INTEGER -
        THEN ERROR_STATUS = F$INTEGER($STATUS)
$   SHOW SYMBOL ERROR_STATUS
$   ERROR_STAT = CNST_PX + F$FAO(CNST_XL, ERROR_STATUS)
$   MESS = F$MESS(ERROR_STATUS)
$   IF "''ERR_MSG_RETURNED'" .NES. CNST_NUL THEN ERR_MSG = ERR_MSG_RETURNED
$   SHOW SYMBOL MESS
$   SHOW SYMBOL ERR_MSG
$   SHOW SYMBOL ERROR_STAT
$   @ICI_MANAGER:SLOT "-CAPACMS_LOGICALS_''REGN_CODE'"
$   @ICI_MANAGER:SLOT "CAPREFCOP_''REGN_CODE'_LOGICALS"
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY ! turn verify off for err
$   @COM:BATCH_ERROR 'ERROR_STATUS' 'RUN_STATUS' "''MAIL_LST'" "''ERR_MSG'"
$   IF PV THEN SET VERIFY ! restore prior verify setting
$   EXIT 44
$!-------------------------------------------------------------------------
$!                      End of Job CAP_CNT_BUX_13
$!-------------------------------------------------------------------------

