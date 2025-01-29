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
$!      SYSTEM:	CAPITATION (Commercial and Subcap)
$!         JOB:	CAP_EDI_BUX_10
$!        DATE: 17-NOV-2010
$!      AUTHOR:	John Ruffner (C4M)
$!    FUNCTION:	transmit reports via email and/or EDI
$!
$!    OVERVIEW:	This command procedure reads indexed files (based on the
$!		media type) and transmits the reports listed in the files
$!		via email (for media types I%) and via EDI (for media
$!		types E*).  The files are named as follows:
$!			CAP_RPT:REPORT_I%_LIS.ISM
$!			CAP_RPT:REPORT_E%_LIS.ISM
$!			ASC_RPT:REPORT_ASC_I%_LIS.ISM
$!			ASC_RPT:REPORT_ASC_E%_LIS.ISM
$!		You can search the REPT_MEDIA table in TABLES_MASTER
$!		for a complete list of codes in use.
$!		
$!
$!     RESTART:	This procedure can always be restarted from the top,
$!		with no user intervention required (other than fixing
$!		the error); OR, it can NOT be restarted at the same step
$!		where it blew off, also without user intervention.
$!		
$!		Note that once a report is transmitted, the 'copies' field
$!		in the report list files gets updated to -1, so on any
$!		rerun, the reports that have been processed will not be sent
$!		again; to resend reports, run with a p3 parameter of RESEND.
$!		
$! MODIFICATION HISTORY:
$!	MOD:	<DD-MMM-YYYY>     <ISR# MOD# >     <USER-ID>
$!
$! mod#03  18-Jul-2015  H2K  (MOHAMED HASHIM )  TASK0084916
$!	apparently with mod#02 we accidentally changed the report list file
$!	from ACE_RPT:REPORT_%%_LIS.ISM to REPORT_%%_LIS.WKLY; now we are
$!	changing it back
$!         
$! mod#02 8-JUL-2015 C4M (JOHN RUFFNER)	TICKET  INC0000154787 version 1.2
$!	Modify for use with MCD system -- since the input files (like
$!	REPORT_I2_LIS.ISM) specify ACE_RPT, we will change it to MCD_RPT
$!	in this procedure so the EDI process can find the file to transmit
$!	
$! mod#01 27-FEB-2012 C4M (JOHN RUFFNER)	TICKET #1429501
$!	HIPAA 5010 format files should not be sent to DECEDI (they should
$!	be picked up automatically from cap_rpt or spc_rpt)
$!	
$! mod#00 17-NOV-2010     INITIAL CREATION    JOHN RUFFNER (C4M)
$!      PID 10805 (HIPAA 5010 expansion)
$!      Procedure begun, cloned from ACE_EDI_BUM_10, replacing 
$!      ACE_EDI_BUM_10 and ASC_EDI_BUM_10.
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
$   SAY " P1 (start step)      =''p1'"
$   SAY " P2 (region/product)  =''p2'"
$   SAY " P3 (resend flag)     =''p3'"
$   SAY " P4 (resend file)     =''p4'"
$   SAY " P5 (media type)      =''p5'"
$   SAY " "
$
$   RUN_STATUS	 = "0"
$   CR[0,8]	 = 13
$   LF[0,8]	 = 10
$   CRLF	 = CR + LF  ! Carriage return/linefeed for e-mail text
$   ERR_MSG_BGN	 = "ERROR ENCOUNTERED WHILE RUNNING " + JOB_NAME + CRLF
$   ERR_MSG	 = ERR_MSG_BGN
$   DIRW	 = "DIRECTORY/SIZ=ALL/DATE/WID=(FILE=36,SIZ=8)"
$
$   SET RMS/QUERY=DISABLE
$   SH RMS
$
$!
$! constants:
$   CNSTE = "E"
$   CNSTN = "N"
$   CNSTP = "P"
$   CNSTT = "T"
$   CNSTY = "Y"
$   CNSTMCD = "MCD"
$   CNSTPRD = "PRD"
$   CNSTRESEND = "RESEND"
$   CNST_CAP_TYPE_SUBCAP = "S"
$   CNST_CONSOLIDATED = "C"
$   CNST_MEDIA_CLASS_EDI = "E"
$   CNST_MEDIA_TYPE_3041 = "E2"
$   CNST_MEDIA_TYPE_5010 = "E4"
$   CNST0 = 0
$   CNST1 = 1
$   CNST02 = "02"
$   CNSTNEG1 = -1
$   CNST4 = 4
$   CNST11 = "11"
$   CNST42 = "42"
$   SWITCH_271 = "6"
$   SWITCH_CONSOL = ",9"
$   SWITCH_CMR_ACT = ",1"
$   SWITCH_CMR_ELIG = ",2"
$   SWITCH_ASC_ELIG = ",3"
$   
$   IF PV THEN SET VERIFY
$   REGN_CODE = F$EXTRACT(0,2,P2)
$   PRODUCT_LINE = F$EXTRACT(2,2,P2)
$   CAP_TYPE = F$EXTRACT(4,1,P2)
$   MAX_ERRORS := "000010000"
$
$   IF CAP_TYPE .EQS. CNST_CAP_TYPE_SUBCAP
$   THEN
$	DEFINE CAPWORK ASCWORK:
$	DEFINE CAP_RPT ASC_RPT:
$	CNSTETYPE = "ASC_RPT:REPORT_ASC_E%_LIS.ISM"
$	CNSTITYPE = "ASC_RPT:REPORT_ASC_I%_LIS.ISM"
$	CNST_MEDIA_OFFSET = 11
$   ELSE 
$! Mod#03 Start
$       CNSTETYPE = "CAP_RPT:REPORT_E%_LIS.ISM"
$       CNSTITYPE = "CAP_RPT:REPORT_I%_LIS.ISM"
$! Mod#03 End
$       CNST_MEDIA_OFFSET = 7
$   ENDIF
$! we must set the usage_ind in the 5010 271u to T for a test run, and
$! P in production -- we will set that and any other environment
$! dependent variables here -- this was approved by Ken Kohnman of CTO
$   ENV = F$TRNLNM("ENVIRONMENT_ID")
$   IF "''ENV'" .EQS. CNSTPRD
$   THEN
$	USAGE_IND = CNSTP
$   ELSE
$	USAGE_IND = CNSTT
$   ENDIF
$
$   MAIL_SENT	= CNST0
$   TMP1_FNAME	:= CAPWORK:'JOB_NAME'_'PID'.TMP	!Name of email file
$   DEFINE MAILFILE ASCWORK:MHN_EMAIL.TMP
$
$   IF P3 .EQS. CNSTRESEND
$   THEN
$	RESEND = CNSTY
$! mod#02 - remove commented code
$   ELSE
$	RESEND = CNSTN
$   ENDIF
$
$   IF  P1 .EQS. CNSTE THEN EXIT
$   SEARCH/NOLOG 'THIS_COMFILE' "STEP_''P1':"   ! Verify restart param
$   IF  $STATUS .NE. %X00000001
$   THEN
$	ERROR_STATUS = $STATUS
$	ERR_MSG = ERR_MSG_BGN + "STEP_''P1' doesn't exist."
$           GOTO ERROR_CONDITION
$   ENDIF
$   GOTO STEP_'P1'
$
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 10  -- process all "I" type (email) report_list files
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_10:
$   RUN_STATUS = "10"
$   SHOW PROCESS/ACCOUNTING
$
$   IF RESEND .AND. P4 .NES. ""
$   THEN
$	INFILE = "''P4'"
$	GOSUB PROCESS_REPORT_LIST
$	GOTO WRAP_UP
$   ENDIF
$
$ STEP_10_LOOP:
$   INFILE = F$SEARCH("''CNSTITYPE'",1)
$   IF INFILE .EQS. "" THEN GOTO STEP_10_LOOP_END
$   GOSUB PROCESS_REPORT_LIST
$   GOTO STEP_10_LOOP
$ STEP_10_LOOP_END:
$
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 20  -- process all "E" type (EDI) report_list files
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_20:
$   RUN_STATUS = "20"
$   SHOW PROCESS/ACCOUNTING
$
$   IF RESEND .AND. P4 .NES. ""
$   THEN
$	INFILE = "''P4'"
$	GOSUB PROCESS_REPORT_LIST
$	GOTO WRAP_UP
$   ENDIF
$
$ STEP_20_LOOP:
$   INFILE = F$SEARCH("''CNSTETYPE'",2)
$   IF INFILE .EQS. "" THEN GOTO STEP_20_LOOP_END
$   GOSUB PROCESS_REPORT_LIST
$   GOTO STEP_20_LOOP
$ STEP_20_LOOP_END:
$
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 30 -- call CAP_UTL_BRX_008.COM to send email notification.
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_30:
$   RUN_STATUS = "30"
$   SHOW PROCESS/ACCOUNTING
$
$   IF CAP_TYPE .EQS. CNST_CAP_TYPE_SUBCAP
$   THEN
$	SET NOON
$	OPEN/WRITE IT MAILFILE:
$	WRITE IT "****************PROCESSING NOTIFICATION**********************"
$	WRITE IT ""
$	WRITE IT "H5004a42p has been transferred to the MHN system.
$	WRITE IT "Please process this file."
$	WRITE IT ""
$	WRITE IT ""
$	CLOSE IT
$	@COM:CAP_UTL_BRX_008 MAILFILE "File Transfer Notification" 001
$	PURGE/LOG MAILFILE:
$	SET ON
$   ENDIF
$
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! WRAP_UP 
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ WRAP_UP:
$   SHOW PROCESS/ACC
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$   @COM:TERMINATION
$   IF PV THEN SET VERIFY
$   EXIT 1

$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! PROCESS_REPORT_LIST - send each report in the file separately
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ PROCESS_REPORT_LIST:
$   IF P5 .NES. ""
$   THEN
$	MEDIA_TYPE = "''P5'"
$   ELSE
$! use cap_utl_brx_007 to get the filename part of the filespec:
$	@COM:CAP_UTL_BRX_007 PARSE 'INFILE CEB10$
$	MEDIA_TYPE = F$EXTRACT(CNST_MEDIA_OFFSET,2,CEB10$FN)
$   ENDIF
$   MEDIA_CLASS = F$EXTRACT(0,1,MEDIA_TYPE)
$   DEFINE/NOLOG REPORT_LIST 'INFILE
$   OPEN/SHARE=READ/READ/WRITE/ERROR=REPORT_LIST_OPEN_ERR RPTLST REPORT_LIST
$   SAY "Sending reports for media type: ''MEDIA_TYPE'"
$!
$ RPTLST_LOOP:
$   READ/END=END_RPTLST_LOOP/TIME_OUT=255 RPTLST RPTLST_REC
$!
$   RPT_FILE = F$EDIT   (F$EXTRACT(0, 64, RPTLST_REC), "TRIM")
$! mod#02 - for region 02 change ACE_RPT to MCD_RPT
$   IF REGN_CODE .EQS. CNST02
$   THEN
$	RPT_FILE[0,3] := 'CNSTMCD
$   ENDIF
$! mod#02 - end
$   RPT_NAME = F$PARSE  (RPT_FILE,,,"NAME")
$   RPT_ID   = F$EXTRACT(12, 2, RPT_NAME)
$   RPT_EXT  = F$PARSE  (RPT_FILE,,,"TYPE")
$   RPT_LVL  = F$EXTRACT(F$LEN(RPT_EXT) - 1, 1, RPT_EXT) !s=site,p=phy,c=consold
$   RPT_TYPE = F$EXTRACT(1,  3, RPT_EXT)
$ show sym rpt_*
$!   COPIES   = F$CVSI   (0, 16, F$EXTRACT(64, 2, RPTLST_REC))
$   COPIES   = 1
$   IF RESEND .AND. COPIES .LT. CNST1
$   THEN
$	COPIES = CNST1
$   ENDIF
$
$   IF  COPIES .LT. CNST1 THEN $ GOTO RPTLST_LOOP
$   IF  F$SEARCH(RPT_FILE,3) .EQS. ""
$   THEN
$	SAY "File not found, skipping ''RPT_FILE'"
$	GOTO RPTLST_LOOP
$   ENDIF
$   IF F$FILE_ATTRIBUTES("''RPT_FILE'","EOF") .EQ. CNST0 
$   THEN
$       SAY "Zero block file, skipping ''RPT_FILE'"
$       GOTO RPTLST_LOOP
$   ENDIF
$
$   SEND_FILE = RPT_FILE
$   IF MEDIA_CLASS .EQS. CNST_MEDIA_CLASS_EDI
$   THEN
$	IF MEDIA_TYPE .NES. CNST_MEDIA_TYPE_3041 -
	.AND. MEDIA_TYPE .NES. CNST_MEDIA_TYPE_5010 THEN GOTO RPTLST_LOOP
$	IF RPT_ID .NES. CNST11 .AND. RPT_ID .NES. CNST42 THEN GOTO RPTLST_LOOP
$	OUT_FILE = RPT_FILE
$	I = F$LOCATE ("SEQ", OUT_FILE)
$	IF I .EQ. F$LENGTH(OUT_FILE)
$	THEN
$	    ERR_MSG = ERR_MSG + "EDI input must be .SEQ file"
$	    ERROR_STATUS = CNST4
$	    GOTO ERROR_CONDITION
$	ENDIF
$	IF MEDIA_TYPE .EQS. CNST_MEDIA_TYPE_3041
$	THEN
$	    IF CAP_TYPE .EQS. CNST_CAP_TYPE_SUBCAP
$	    THEN
$		ERR_MSG = ERR_MSG + "3041 EDI format cannot be used for subcap"
$		ERROR_STATUS = CNST4
$		GOTO ERROR_CONDITION
$	    ENDIF
$	    OUT_FILE[I,3] := EDI
$	    SEND_FILE = OUT_FILE
$	    DEFINE/NOLOG EDI_INPUT_FILE 'RPT_FILE'
$	    DEFINE/NOLOG EDI_OUTPUT_FILE 'OUT_FILE'
$	    IF RPT_ID .EQS. CNST11
$	    THEN
$		DIRW EXE:ACE_EDI_FMT_01
$		RUN/NODEB EXE:ACE_EDI_FMT_01  !Activity
$	    ENDIF
$	    IF RPT_ID .EQS. CNST42
$	    THEN
$		DIRW EXE:ACE_EDI_FMT_02
$		RUN/NODEB EXE:ACE_EDI_FMT_02  !Eligibility
$	    ENDIF
$	ENDIF
$	IF MEDIA_TYPE .EQS. CNST_MEDIA_TYPE_5010
$	THEN
$	    OUT_FILE[I,3] := E51
$	    SEND_FILE = OUT_FILE
$	    SAVE_FILE = OUT_FILE
$	    SAVE_FILE[I,3] := ESV
$	    SWITCHES = SWITCH_271			! write cap271u
$	    IF RPT_LVL .EQS. CNST_CONSOLIDATED
$	    THEN
$		SWITCHES = SWITCHES + SWITCH_CONSOL	! consolidated report
$	    ENDIF
$	    DEFINE/NOLOG CAP271U_FILE 'SAVE_FILE'
$	    IF RPT_ID .EQS. CNST11
$	    THEN
$		IF CAP_TYPE .EQS. CNST_CAP_TYPE_SUBCAP
$		THEN
$		    ERR_MSG = ERR_MSG + -
			"the subcap system does not produce activity reports"
$		    ERROR_STATUS = CNST4
$		    GOTO ERROR_CONDITION
$		ELSE
$		    SWITCHES = SWITCHES + SWITCH_CMR_ACT ! cmr activity report
$		    DEFINE/NOLOG CAPACTP_FILE 'RPT_FILE'
$		ENDIF
$	    ENDIF
$	    IF RPT_ID .EQS. CNST42
$	    THEN
$		IF CAP_TYPE .EQS. CNST_CAP_TYPE_SUBCAP
$		THEN
$		    SWITCHES = SWITCHES + SWITCH_ASC_ELIG ! asc elig report
$		    DEFINE/NOLOG ASCELTP_FILE 'RPT_FILE'
$		ELSE
$		    SWITCHES = SWITCHES + SWITCH_CMR_ELIG ! cmr elig report
$		    DEFINE/NOLOG CAPELTP_FILE 'RPT_FILE'
$		ENDIF
$	    ENDIF
$	    DEFINE COB$SWITCHES "''SWITCHES'"
$	    DIRW EXE:CAP_EDI_FMT_03
$	    RUN/NODEB EXE:CAP_EDI_FMT_03
$	    COPY/LOG 'SAVE_FILE' 'OUT_FILE'
$	ENDIF
$   ENDIF
$
$! mod#01 - the 5010 EDI format should not go to DECEDI; it should be picked up
$! automatically from the report area
$   IF MEDIA_TYPE .NES. CNST_MEDIA_TYPE_5010
$   THEN
$	SAY "Calling ACE_EDI_BUM_20 to transmit ''SEND_FILE'"
$	@COM:ACE_EDI_BUM_20 'SEND_FILE' 'MEDIA_TYPE'
$   ENDIF
$   IF .NOT. $STATUS
$   THEN
$	GOSUB RPTLST_ERRORS
$   ELSE
$	RPTLST_REC[512,16] = CNSTNEG1
$	WRITE/UPDATE RPTLST RPTLST_REC
$   ENDIF
$!
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$   GOTO RPTLST_LOOP

$ END_RPTLST_LOOP:
$   IF PV THEN SET VERIFY
$   CLOSE/NOLOG RPTLST
$   RETURN

$ REPORT_LIST_OPEN_ERR:
$   ERROR_STATUS = $STATUS
$   ERR_MSG = ERR_MSG + "Can't open ''INFILE'"
$   GOTO ERROR_CONDITION

$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! RPTLST_ERRORS -- E-mail warning message to ACE_TEAM
$!! (it's not likely we could ever get here, but i'll leave steve's
$!! code just in case)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ RPTLST_ERRORS:
$   IF  .NOT. MAIL_SENT		    !Only send 1 E-mail
$   THEN
$	OPEN/WRITE TEMP1 'TMP1_FNAME'
$	WRITE TEMP1 "ACE_TEAM,"
$	WRITE TEMP1 ""
$	WRITE TEMP1 "    Warnings were encountered while attempting to E-mail"
$	WRITE TEMP1 "a few of the reports to commercial ACE providers. THIS IS"
$	WRITE TEMP1 "NOT URGENT and can be investigated on the next business"
$	WRITE TEMP1 "day. Please check the log file PRDLOG:", JOB_NAME, ".LOG"
$	WRITE TEMP1 "to determine if any corrective action needs to be taken."
$	WRITE TEMP1 ""
$       WRITE TEMP1 "    THIS IS NOT A SYSTEM CRASH. PLEASE REMAIN CALM."
$       WRITE TEMP1 "STAY IN YOUR SEAT. THE WORLD IS NOT COMING TO AN END."
$       WRITE TEMP1 "YOU CAN TRUST ME ON THIS ONE. "
$       WRITE TEMP1 ""
$	CLOSE/NOLOG TEMP1
$	MAIL/SUBJ="Some ACE reports were not E-mailed" 'TMP1_FNAME' "@ACE_TEAM"
$	DELETE/LOG 'TMP1_FNAME';*
$	MAIL_SENT = 1
$   ENDIF
$   RETURN 1

$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! ERROR_CONDITION - Abort and Report error. For the third parameter, either
$!   type the Username or the mail distribution list (with a @).
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ ERROR_CONDITION:
$   IF  F$TYPE(ERROR_STATUS) .EQS. "" THEN $ ERROR_STATUS = $STATUS
$   SET NOON
$!
$   CLOSE/NOLOG TEMP1
$   IF  F$SEARCH(TMP1_FNAME) .NES. "" THEN $ DELETE 'TMP1_FNAME';*
$   CLOSE/NOLOG RPTLST
$   CLOSE/NOLOG EDI_FILE
$   MAIL_LST = "@OPR_TEAM, @ACE_TEAM"
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$   SHOW SYMBOL ERROR_STATUS
$   @COM:BATCH_ERROR 'ERROR_STATUS' 'RUN_STATUS' "''MAIL_LST'" "''ERR_MSG'"
$   IF PV THEN SET VERIFY
$   EXIT 44
$!-------------------------------------------------------------------------
$!			End of ACE_EDI_BUM_10.COM 
$!-------------------------------------------------------------------------
