$   PV = F$ENVIRONMENT("VERIFY_PROCEDURE") ! Save verify setting in case this
$   SET NOVERIFY                           ! comfile is '@'ed from another
$!***************************************************************************
$! This source code, and all the routines referenced herein, are            *
$! the proprietary properties and trade secrets of HEALTH NET,INC.          *
$! Except as provided for by license agreement, this source code            *
$! shall not  be  duplicated, used or disclosed without written             *
$! consent, signed by an officer of HEALTH NET,INC.                         *
$! **************************************************************************
$! Start Documentation:
$!
$!      SYSTEM:	CAPITATION
$!         JOB:	CAP_EDI_BUX_04
$!        DATE: 02-NOV-2010
$!      AUTHOR:	John Ruffner (C4M)
$!    FUNCTION: Update payer info in CAPEDIP with data from TR
$!
$!    OVERVIEW:	This job uses a web service to get the latest payer
$!		data from the Transaction Repository and updates the
$!		corresponding record in the CAPEDIP file.  The CAPEDIP
$!		file, in turn, is used to populated the payer fields
$!		in the 5010 271u generic transaction file.
$!		
$!		Note that the web service may be down when we run, so
$!		we'll loop through a few times before we blow off.
$!		if it doesn't come up in time for cap processing,
$!		we can most likely run with the prior week's info.
$!		
$!     RESTART:	This procedure can always be restarted from the top,
$!		with no user intervention required (other than fixing
$!		the error); OR, it can be restarted at the same step
$!		where it blew off, also without user intervention.
$!
$!     CHANGES:
$! MOD  :    <DD-MMM-CCYY>   <ISR# MOD#>         <USER-ID>
$! 01         07-MAY-2010     MOD #01		  INDUJA U (HC8)
$!
$!	      Modified the procedure to include web service calls to
$!	      get the latest Trading partner information from 
$!	      the Transaction Repository and update the CAPEDPR file,
$!	      which is used to populate 
$!
$!            02-NOV-2010     INITIAL CREATION    JOHN RUFFNER (C4M)
$!            PID 10805 (HIPAA 5010 expansion)
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
$   SET NOVERIFY
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
$   SAY " P3 (payor id)        =''p3'"
$   SAY " P4 (region)          =''p4'"
$   SAY " P5 (transaction nbr) =''p5'"
$   SAY " P6 (rerun okay)      =''p6'"
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
$   IF PV THEN SET VERIFY
$   REGN_CODE = F$EXTRACT(0,2,P2)
$   PRODUCT_LINE = F$EXTRACT(2,2,P2)
$   CAP_TYPE = F$EXTRACT(4,1,P2)
$   MAX_ERRORS := "000010000"
$
$! abbreviations:
$!  WBS = webservice
$
$! constants:
$   CNSTE := "E"
$   CNSTENDED := "ENDED"
$   CNSTRUNNING := "RUNNING"
$   CNST_SATURDAY = "Saturday"
$   CNST_SUNDAY = "Sunday"
$   CNST_MONDAY = "Monday"
$   CNST0 = 0
$   CNST1 = 1
$   CNST6 = 6
$   CNST9 = 9
$   CNST20 = 20
$   CNST44 = 44
$   CONST_Y = "Y"
$
$   IF P2 .EQS. "" .OR. P3 .EQS. "" .OR. P4 .EQS. "" .OR. P5 .EQS. ""
$   THEN
$	ERROR_STATUS = "%X00000000"
$	ERR_MSG = ERR_MSG_BGN + -
		"Required parameter(s) is(are) missing. " + CRLF + -
		"Please re-submit this job with required parameter(s)"
$	GOTO ERROR_CONDITION
$   ENDIF
$
$   @COM:CAP_UTL_BRX_01.COM "''REGN_CODE'" "''PRODUCT_LINE'" "''CAP_TYPE'"
$
$   IF CAPREGN_RUN_STATUS .EQS. CNSTRUNNING -
    .AND. P6 .NES. "OKAY"
$   THEN
$	ERROR_STATUS = "%X00000000"
$	ERR_MSG = ERR_MSG_BGN + -
		"HNCAP Batch Cycle is running. " + CRLF + -
		"If you have to run now, pass "OKAY" as P6"
$	GOTO ERROR_CONDITION
$   ENDIF
$
$!
$   PAYOR_ID == P3
$   REGION_CD == P4
$   TRX_NBR == P5
$   DEFINE CAPEDIP_FILE CAPEDIP_EDI_PARMS
$!Mod #01 Begin
$   DEFINE CAPEDPR_FILE CAPEDPR_EDI_PROFILE
$!Mod #01 End
$
$   DEFINE PRGSTAT CAPWORK:CAP_EDI_BUX_04_'P3'.STAT
$
$   IF P1 .EQS. CNSTE THEN EXIT
$   IF P1 .EQS. "0" THEN P1 = "1"
$   SEARCH/NOLOG 'THIS_COMFILE' "STEP_''P1':"   ! Verify restart param
$   IF  $STATUS .NE. %X00000001
$   THEN
$	ERROR_STATUS = $STATUS
$	ERR_MSG = ERR_MSG_BGN + -
		"STEP_''P1' doesn't exist."
$           GOTO ERROR_CONDITION
$   ENDIF
$   GOTO STEP_'P1'

$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 1 -	Update CAPEDIP
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_1:
$   RUN_STATUS = "1"
$   SHOW PROCESS/ACC
$
$   IF F$SEARCH("CAPEDIP_FILE") .EQS. ""
$   THEN
$	SHOW LOGICAL CAPEDIP_FILE
$	CREATE/LOG/FDL=FDL:CAP_CAPEDIP.FDL_ISM CAPEDIP_FILE
$   ENDIF
$
$! we'll keep trying to access the webservice for an hour or so before we abort;
$! note that fatal errors will abort the program and therefore the job,
$! but if we have trouble with the webservice, we'll return a failure symbol
$    TRY_CNT = CNST0
$ TRY_WBS_LOOP:
$   DIRW/VERSION=5 EXE:CAP_EDI_BUX_04.*
$   RUN/NODEBUG EXE:CAP_EDI_BUX_04.EXE
$   IF "''CAP_EDI_BUX_04_SUCCESS'"
$   THEN
$	SET NOON
$	    TYPE PRGSTAT
$	    DELETE/LOG PRGSTAT:;*
$	SET ON
$	GOTO TRY_WBS_LOOP_END
$   ENDIF
$   TRY_CNT = TRY_CNT + CNST1
$   SHOW SYMBOL TRY_CNT
$   IF TRY_CNT .GT. CNST6
$   THEN
$	SET VER
$! we don't want to blow off late at night unless it's saturday or sunday night
$! or sunday or monday morning
$	DAYTIME:="''F$TIME()'"
$	DAY = F$CVTIME(DAYTIME,,"WEEKDAY")
$	HOUR = F$CVTIME(DAYTIME,,"HOUR")
$	IF ((DAY .NES. CNST_SUNDAY .AND. DAY .NES. CNST_SATURDAY) -
	      .AND. HOUR .GT. CNST20) -
	    .OR. ((DAY .NES. CNST_SUNDAY .OR. DAY .NES. CNST_MONDAY) -
	      .AND. HOUR .LT. CNST9)
$	THEN
$		TRY_CNT = CNST0
$		WRITE SYS$OUTPUT -
			"No rush, let the cap team sleep and try again"
$		MAIL NL: "@ACE_TEAM" -
    /SUB="Webservice in CAP_EDI_BUX_04 failing; we'll keep trying till 9:00 am"
$	ELSE
$		ERR_MSG = ERR_MSG + -
			"Webservice in CAP_EDI_BUX_04 keeps failing"
$		ERROR_STATUS = CNST44
$		GOTO ERROR_CONDITION
$	ENDIF
$   ENDIF
$   WAIT 00:10:00
$   SET NOVER
$   GOTO TRY_WBS_LOOP

$ TRY_WBS_LOOP_END:
$   SET VER
$
$!
$!Mod #01 Begin
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 2 -    Update CAPEDPR
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ STEP_2:
$   RUN_STATUS = "2"
$   SHOW PROCESS/ACC
$
$   IF F$SEARCH("CAPEDPR_FILE") .EQS. ""
$   THEN
$       SHOW LOGICAL CAPEDPR_FILE
$       CREATE/LOG/FDL=FDL:CAP_CAPEDPR.FDL_ISM CAPEDPR_FILE
$   ENDIF
$
$! we'll keep trying to access the webservice for an hour or so before we
$! abort;
$! note that fatal errors will abort the program and therefore the job,
$! but if we have trouble with the webservice, we'll return a failure symbol
$    TRY_CNT = CNST0
$ TRY_WBS_LOOP1:
$   DIRW/VERSION=5 EXE:CAP_EDI_BUX_05.*
$   RUN/NODEBUG EXE:CAP_EDI_BUX_05.EXE
$   IF "''CAP_EDI_BUX_05_SUCCESS'" .EQS. CONST_Y
$   THEN
$       SET ON
$       GOTO TRY_WBS_LOOP_END1
$   ENDIF
$   TRY_CNT = TRY_CNT + CNST1
$   SHOW SYMBOL TRY_CNT
$   IF TRY_CNT .GT. CNST6
$   THEN
$       SET VER
$! we don't want to blow off late at night unless it's saturday or sunday night
$! or sunday or monday morning
$       DAYTIME:="''F$TIME()'"
$       DAY = F$CVTIME(DAYTIME,,"WEEKDAY")
$       HOUR = F$CVTIME(DAYTIME,,"HOUR")
$       IF ((DAY .NES. CNST_SUNDAY .AND. DAY .NES. CNST_SATURDAY) -
              .AND. HOUR .GT. CNST20) -
            .OR. ((DAY .NES. CNST_SUNDAY .OR. DAY .NES. CNST_MONDAY) -
              .AND. HOUR .LT. CNST9)
$       THEN
$               TRY_CNT = CNST0
$               WRITE SYS$OUTPUT -
                        "No rush, let the cap team sleep and try again"
$               MAIL NL: "@ACE_TEAM" -
    /SUB="Webservice in CAP_EDI_BUX_05 failing; we'll keep trying till 9:00 am"
$       ELSE
$               ERR_MSG = ERR_MSG + -
                        "Webservice in CAP_EDI_BUX_05 keeps failing"
$               ERROR_STATUS = CNST44
$               GOTO ERROR_CONDITION
$       ENDIF
$   ENDIF
$   WAIT 00:10:00
$   SET NOVER
$   GOTO TRY_WBS_LOOP1

$ TRY_WBS_LOOP_END1:
$   SET VER
$
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! WRAP_UP -- End                                                 !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ WRAP_UP:
$   SHOW PROCESS/ACC
$   SET  NOVERIFY       ! turn verify off for @com:termination
$   @COM:TERMINATION
$   IF PV THEN SET VERIFY ! restore prior verify setting
$   EXIT 1

$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! ERROR_CONDITION -- Error Handling                              !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
$!-------------------------------------------------------------------------
$!                      End of Job CAP_EDI_BUX_04
$!-------------------------------------------------------------------------
