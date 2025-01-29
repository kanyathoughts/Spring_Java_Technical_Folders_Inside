$!
$   PV = F$ENVIRONMENT("VERIFY_PROCEDURE") ! Save verify setting in case this
$   SET NOVERIFY                           ! comfile is '@'ed from another
$!
$!-----------------------------------------------------------------------------!
$!  H E A L T H   N E T   -   Automated Business System.                       !
$!-----------------------------------------------------------------------------!
$!  This software  source  code  module and all components referenced  herein, !
$!  are the proprietary properties and trade secrets of HEALTH NET.  Except as !
$!  provided for by license agreement,  this software source code module shall !
$!  not be duplicated, used or disclosed without written consent, signed by an !
$!  officer of HEALTH NET. COPYRIGHT ) 2002 By HEALTH NET CORPORATION,         !
$!  Woodland Hills, CA All rights reserved.                                    !
$!-----------------------------------------------------------------------------!
$! Start Documentation:
$!
$!      SYSTEM:         CAPITATION
$!      JOB:		CAP_BAL_BRX_00
$!      TITLE:		Extract records from CAPTMLN_NEW_01..04, CAPTTLS_TOTALS,
$!			CAP_PROJECTED_RATES, and
$!			CAPTUHS_TOTALS_UPDATES_HIST files to create the work 
$!			files for CAP Balancing.
$!
$!      DATE:           25-Jun-2003
$!      AUTHOR:		John Wang
$!
$!      OVERVIEW:    
$!
$!			This procedure expects to be executed weekly or monthly.
$!			It is triggered by CAPREGN_REGION_CONTROL file. This 
$!			file will control the CAP month we are processing.
$!
$!	Parameters:
$!		    P1: Restart Step
$!                  P2: BUSINESS_REGION
$!                  P3: BUSINESS_PRODUCT_LINE   
$!                  P4: CAP_TYPE
$!                  P5: Stream Number
$!
$!      MOD:    {DD-MMM-CCYY}   {REF#}          {USER}
$!
$!	20070525	JLR	MOD#03
$!	ACETUHS files were not being concatenated correctly which caused
$!	CAP_BAL_BRX_11 to be out of balance.
$!
$!	01-Jul-2005	JLR
$!	moved ACEELIG creation to a new com file (CAP_RPT_BUX_48.COM) in order
$!	to speed up job stream.
$!
$!	02-Feb-2004	JHW
$!	Insert a new step to create the Ace_Eligibility_Info.
$!	This step moved from CAP_RPT_BUX_46.COM Step 2, due to the
$!	HNCAP re-schedule.  (See HNCAP Scheduler Flow Chart for details)
$!
$! End Documentation
$!
$!------------------------------------------------------------------------------
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                !!
$!! STEP_0  Initialization - Initialize local symbols and logicals !!
$!!                                                                !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ INITIALIZATION:
$!
$   @COM:INITIALIZATION
$!
$   ON ERROR     THEN $ GOTO ERROR_CONDITION
$!
$   THIS_COMFILE = F$ENVIRONMENT("PROCEDURE")       ! This job's filespec
$   JOB_NAME    == F$PARSE(THIS_COMFILE,,,"NAME")   ! Parse out the name
$   PID          = F$GETJPI("","PID")               ! Handy for temp filenames
$   RUN_STATUS   = "0"
$   CR[0,8]      = 13
$   LF[0,8]      = 10
$   CRLF         = CR + LF  ! Carriage return/linefeed for e-mail text
$   ERR_MSG  = "ERROR ENCOUNTERED WHILE RUNNING " + JOB_NAME + CRLF
$!
$   SET RMS/QUERY=DISABLE
$   SHOW RMS_DEFAULT
$!
$   ECHO  := WRITE SYS$OUTPUT
$   ECHO ""
$   ECHO "                PID=''PID'"
$   ECHO "       Process Name=''F$GETJPI("","PRCNAM")'"
$   ECHO "          Node Name=''F$GETJPI("","NODENAME")'"
$   ECHO "           Job Type=''F$GETJPI("","JOBTYPE")'"
$   ECHO "        VMS Version=''F$GETJPI("","NODE_VERSION")'"
$   ECHO "  Username(account)=''F$GETJPI("","USERNAME")'"
$   ECHO "     Procedure name=''JOB_NAME'"
$   ECHO ""                            
$   ECHO "                 P1=''P1'"
$   ECHO "		   P2=''P2'"
$   ECHO "		   P3=''P3'"
$   ECHO "		   P4=''P4'"
$   ECHO "		   P5=''P5'"
$   ECHO ""
$!
$   SET VERIFY
$   BUSINESS_REGION             :== "''P2'"
$   BUSINESS_PRODUCT_LINE       :== "''P3'"
$   CAP_TYPE                    :== "''P4'"
$   MAX_ERRORS			:== "099999999"
$   EMPTY_STREAM_OK		:== "N"
$   EI = F$TRNLNM("ENVIRONMENT_ID")
$   IF (EI .EQS. "DEV" .OR. EI .EQS. "TST") -	!we're not in production
	THEN EMPTY_STREAM_OK	:== "Y"
$!
$ DEFINE CAPTMLN_E_WORK    CAPWORK:CAPTMLN_E_WORK.SEQ
$ DEFINE CAPTMLN_R_WORK    CAPWORK:CAPTMLN_R_WORK.SEQ
$ DEFINE CAPPRRT_WORK      CAPWORK:CAPPRRT_WORK.SEQ
$ DEFINE CAPTUHS_WORK      CAPWORK:CAPTUHS_WORK.SEQ
$ DEFINE ACEELIG_WORK      CAPWORK:ACEELIG_WORK.SEQ
$ DEFINE CAPTTLS_WORK      CAPWORK:CAPTTLS_WORK.SEQ
$ DEFINE CAPTMLN_NEW	   CAPWORK:CAPTMLN_NEW.TMP
$!
$   LOGDIR	CAPREGN_REGION_CONTROL
$   LOGDIR      TABLES_MASTER
$   LOGDIR	CAPTMLN_NEW_01
$   LOGDIR	CAPTMLN_NEW_02
$   LOGDIR	CAPTMLN_NEW_03
$   LOGDIR	CAPTMLN_NEW_04
$   LOGDIR	PRV_TOTALS_PRVTTLS_01
$!  LOGDIR	CAP_TOTALS_UPDATES_HIST
$!  LOGDIR	CAP_PROJECTED_RATES
$   LOGDIR      ACE_ELIGIBILITY_INFO
$!
$!   WRITE SYS$OUTPUT "************************"
$!   WRITE SYS$OUTPUT P1, P2
$!   WRITE SYS$OUTPUT "************************"
$!   WRITE SYS$OUTPUT THIS_COMFILE, "STEP_''P1':"
$!
$   IF  P1 .NES. "" .AND. P1 .NES. "0"  ! Check restart step
$   THEN
$       SEARCH/NOLOG 'THIS_COMFILE' "STEP_''P1':"   ! Verify restart param
$       IF  $STATUS .NE. %X00000001
$       THEN
$           ERROR_STATUS = $STATUS
$           ERR_MSG = ERR_MSG + -
                "/PARAMETER=''P1' doesn't exist. Please re-submit at step" + -
                CRLF + "indicated in prior E-mail, not the one listed above."
$           GOTO ERROR_CONDITION
$       ENDIF
$       SET VERIFY
$       GOTO STEP_'P1'
$   ENDIF
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 1 - (removed ACEELIG build)				  !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_1:
$   SHOW PROCESS/ACCOUNTING
$   RUN_STATUS = "1"
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 2 - Sort/merge/extract CAPTMLN, ACETUHS, PROJECTED RATES !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_2:
$!
$   SHOW PROCESS/ACCOUNTING
$   RUN_STATUS = "2"
$!
$   CAPREGN_CAP_RUN_DATE = ""
$   DELETE/SYM/LOCAL CAPREGN_CAP_RUN_DATE
$   @COM:CAP_UTL_BRX_01.COM 'P2 'P3 'P4
$   ACETUHS_FILE = "ACETUHS_DIR:ACETUHS_" + CAPREGN_CAP_RUN_DATE + ".SEQ"
$!
$ SHOW SYM CAPREGN_PERIOD_END_DATE
$ SHOW SYM CAPREGN_RUN_CYCLE
$ SHOW SYM CAPREGN_CAP_RUN_DATE
$!
$ EXP_DATE = F$EXTRACT (0, 6, CAPREGN_PERIOD_END_DATE)
$ OPEN /WRITE TMP_FILE CAPWORK:CAP_BAL_BRX_00_TMP.SPEC
$ WRITE TMP_FILE " /FIELD=(NAME=CAP_PERIOD, POS:19 ,SIZE:6) "
$ WRITE TMP_FILE " /CONDITION=(NAME=DATE_SEL, TEST=(CAP_PERIOD EQ ''EXP_DATE')) "
$ WRITE TMP_FILE " /INCLUDE=(CONDITION=DATE_SEL) "
$ CLOSE TMP_FILE
$ TYPE CAPWORK:CAP_BAL_BRX_00_TMP.SPEC
$ SORT /SPEC=CAPWORK:CAP_BAL_BRX_00_TMP.SPEC -
       CAPTMLN_NEW_01:;,  -
       CAPTMLN_NEW_02:;,  -
       CAPTMLN_NEW_03:;,  -
       CAPTMLN_NEW_04:;   - 
       CAPTMLN_NEW        /STAT
$!
$ DELETEX/NOCONFIRM/LOG CAPWORK:CAP_BAL_BRX_00_TMP.SPEC;*
$!
$ CREATE CAPWORK:MBRPRRT_BAL_BRX_00.TMP
$!
$! find previous cap month
$   YYYY = F$EXTRACT(0,4,CAPREGN_PERIOD_END_DATE)
$   MM = F$EXTRACT(4,2,CAPREGN_PERIOD_END_DATE)
$   IF MM .EQS. "01"
$     THEN
$	YYYY = YYYY - 1 
$	MBRPRRT_PREV_MONTH = "''YYYY'" + "12"
$     ELSE
$	MBRPRRT_PREV_MONTH = YYYY + MM - 1
$   ENDIF
$
$ GET_MBRPRRT_FILES:
$!
$ CAPPRRT_FILE = F$SEARCH ("CAP_PROJECTED_RATES:MBRPRRT.''MBRPRRT_PREV_MONTH'%%_%%%*")
$ SHOW SYM CAPPRRT_FILE
$ IF CAPPRRT_FILE .NES. ""
$ THEN 
$	IF F$FILE_ATTRIBUTE (CAPPRRT_FILE, "EOF") .NE. 0
$	THEN
$	     APPEND 'CAPPRRT_FILE CAPWORK:MBRPRRT_BAL_BRX_00.TMP
$	ENDIF
$       GOTO GET_MBRPRRT_FILES
$ ENDIF
$!
$! mod#03: this failed to pick up the prior runs for the month:
$! ACETUHS_FILE = F$SEARCH ("ACETUHS_DIR:ACETUHS_''CAPREGN_CAP_RUN_DATE'.SEQ")
$! SHOW SYM ACETUHS_FILE
$! IF ACETUHS_FILE .NES. ""
$! THEN 
$!	IF F$FILE_ATTRIBUTE (ACETUHS_FILE, "EOF") .NE. 0
$!	THEN
$!	     COPY 'ACETUHS_FILE CAPWORK:ACETUHS_BAL_BRX_00.TMP;
$!	ENDIF
$! ENDIF
$!
$! this is a better way:
$ COPY/CONC/LOG ACETUHS_DIR:ACETUHS_%%%%%%%%.SEQ CAPWORK:ACETUHS_BAL_BRX_00.TMP
$
$   DEFINE  CAP_PROJECTED_RATES		CAPWORK:MBRPRRT_BAL_BRX_00.TMP
$   DEFINE  CAP_TOTALS_UPDATES_HIST     CAPWORK:ACETUHS_BAL_BRX_00.TMP
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 3 -- Extract Records from 5 files when it is monthly run.!!
$!!           Only 3 files are extracted, if it is weekly run.    !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_3:
$!
$   RUN_STATUS = "3"
$   SHOW PROCESS/ACC
$   LOGDIR EXE:CAP_BAL_BRX_00.EXE
$   RUN/NODEBUG EXE:CAP_BAL_BRX_00.EXE
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 4 -- Work the work files.                                !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_4:
$!
$   RUN_STATUS = "4"
$   SHOW PROCESS/ACC
$!
$ DELETEX/NOCONFIRM/LOG	CAPWORK:MBRPRRT_BAL_BRX_00.TMP;*
$ DELETEX/NOCONFIRM/LOG CAPWORK:ACETUHS_BAL_BRX_00.TMP;*
$!
$ IF F$SEARCH ("CAPWORK:CAPTMLN_R_WORK.SEQ") .NES. ""
$ THEN 
$      SORT /KEY=(POS:9,SIZ:5)/KEY=(POS:52,SIZ:6)        -
            /KEY=(POS:22,SIZ:8)/KEY=(POS:42,SIZ:9) /STAT -
		CAPWORK:CAPTMLN_R_WORK.SEQ CAPWORK:CAPTMLN_R_WORK.SEQ
$      PURGE CAPWORK:CAPTMLN_R_WORK.SEQ
$ ENDIF
$!
$ IF F$SEARCH ("CAPWORK:CAPTMLN_E_WORK.SEQ") .NES. ""
$ THEN
$      SORT /KEY=(POS:9,SIZ:5)/KEY=(POS:38,SIZ:13) /NODUP /STAT -
	CAPWORK:CAPTMLN_E_WORK.SEQ CAPWORK:CAPTMLN_E_WORK.SEQ
$      PURGE CAPWORK:CAPTMLN_E_WORK.SEQ
$ ENDIF
$!
$ IF F$SEARCH ("CAPWORK:ACEELIG_WORK.SEQ") .NES. ""
$ THEN
$      SORT /STAT CAPWORK:ACEELIG_WORK.SEQ   CAPWORK:ACEELIG_WORK.SEQ
$      PURGE CAPWORK:ACEELIG_WORK.SEQ
$ ENDIF
$!
$ DELETEX/NOCONFIRM/LOG CAPTMLN_NEW:;
$!
$ IF F$SEARCH ("CAPWORK:CAPTTLS_WORK.SEQ") .NES. ""
$ THEN
$      SORT /STAT CAPWORK:CAPTTLS_WORK.SEQ   CAPWORK:CAPTTLS_WORK.SEQ
$      PURGE CAPWORK:CAPTTLS_WORK.SEQ
$ ENDIF
$!
$ SORT /WORKFILES=3 /STAT CAPWORK:CAPPRRT_WORK.SEQ, CAPWORK:CAPTUHS_WORK.SEQ -
                CAPWORK:CAPPRRT_CAPTUHS.SEQ
$      PURGE CAPWORK:CAPPRRT_WORK.SEQ
$      PURGE CAPWORK:CAPTUHS_WORK.SEQ    
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
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! ERROR_CONDITION -- Error Handling                              !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ ERROR_CONDITION:
$   IF F$TYPE(ERROR_STATUS) .NES. "INTEGER" -
        THEN ERROR_STATUS = F$INTEGER($STATUS)
$   ERROR_STATUS[0, 3] = 4              ! Raise severity to "F"
$   MAIL_LST = "@OPR_TEAM, @ACE_TEAM"
$   SET  NOVERIFY ! turn verify off for @com:batch_error
$   @COM:BATCH_ERROR 'ERROR_STATUS' 'RUN_STATUS' "''MAIL_LST'" "''ERR_MSG'"
$   IF PV THEN SET VERIFY ! restore prior verify setting
$   EXIT ERROR_STATUS
$!
$!-------------------------------------------------------------------------
$!                      End of Job
$!-------------------------------------------------------------------------
