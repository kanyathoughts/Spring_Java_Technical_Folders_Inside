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
$!      JOB:		CAP_BAL_BRX_11
$!      TITLE:		Use extract records from CAPTTLS_TOTALS VS
$!			CAP_PROJECTED_RATES + CAPTUHS_TOTALS_UPDATES_HIST files
$!			(CAPTTLS_WORK VS CAPPRRT_CAPTUHS) to create the Balance
$!			report.
$!
$!      DATE:           25-Jun-2003
$!      AUTHOR:		John Wang
$!
$!      OVERVIEW:    
$!
$!			This procedure expects to be executed by weekly
$!			and monthly.
$!
$!	Parameters:
$!		    P1: Restart Step
$!
$!      MOD:    {DD-MMM-CCYY}   {REF#}          {USER}
$!      01      25-FEB-2004                     Steve Leski
$!              Added Region Code, Product line and cap type for new RPTMSTR.
$!
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
$   ON ERROR     THEN $ GOTO ERROR_CONDITION
$!
$   HN_MAIL   := $EXE:ACE_MAI_ORX_01
$   DCL_ABORT  =  196652
$   MSGFILE   := CAPWORK:CAP_BAL_BRX_11.MSG
$   W         := WRITE SYS$OUTPUT
$!
$   THIS_COMFILE = F$ENVIRONMENT("PROCEDURE")       ! This job's filespec
$   JOB_NAME    == F$PARSE(THIS_COMFILE,,,"NAME")   ! Parse out the name
$   PID          = F$GETJPI("","PID")               ! Handy for temp filenames
$   RUN_STATUS   = "0"
$   CR[0,8]      = 13
$   LF[0,8]      = 10
$   CRLF         = CR + LF  ! Carriage return/linefeed for e-mail text
$   ERR_MSG  = "ERROR ENCOUNTERED WHILE RUNNING " + JOB_NAME + CRLF
$   @COM:INITIALIZATION
$! get the run cycle:
$   @COM:CAP_UTL_BRX_01 'P2 'P3 'P4 !should be region,business_type,cap_type
$!
$   SET RMS/QUERY=DISABLE
$   SHOW RMS_DEFAULT
$!
$   MAX_ERRORS			:== "099999999"
$!  change to "N" before release to production:
$   EMPTY_STREAM_OK		:== "Y"
$!
$   DEFINE CAP_BAL_BRX_11_REPORT_FILE   CAP_RPT:CAP_BAL_BRX_11.RPT
$!
$   DEFINE      CAPPRRT_CAPTUHS CAPWORK:CAPPRRT_CAPTUHS.SEQ
$   DEFINE      CAPTTLS_WORK    CAPWORK:CAPTTLS_WORK.SEQ
$!
$   LOGDIR    CAPTTLS_WORK
$   LOGDIR    CAPPRRT_CAPTUHS
$!
$   IF  P1 .EQS. "" THEN P1 = "0"
$   IF  P2 .EQS. "" THEN P2 = "01"
$   IF  P3 .EQS. "" THEN P3 = "CM"
$   IF  P4 .EQS. "" THEN P4 = "C"
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
$   ECHO "                 P2=''P2'"
$   ECHO "                 P3=''P3'"
$   ECHO "                 P4=''P4'"
$   ECHO ""
$!
$   BUSINESS_REGION         := "''P2'"
$   BUSINESS_PRODUCT_LINE   := "''P3'"
$   CAP_TYPE                := "''P4'"
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
$   SET VERIFY
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                               !!
$!! Step 1 -- Use extract records to generate the balance report. !!
$!!                                                               !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ STEP_1:
$!
$   RUN_STATUS = "1"
$   SHOW PROCESS/ACC
$   CAP_BAL_BRX_11_STATUS :== "I"
$   LOGDIR EXE:CAP_BAL_BRX_11.EXE
$   RUN/NODEBUG EXE:CAP_BAL_BRX_11.EXE
$!
$   SHOW SYMBOL CAP_BAL_BRX_11_STATUS
$!
$   GOSUB BALANCE_MSG
$   IF CAP_BAL_BRX_11_STATUS .EQS. "O"
$   THEN
$      W " "
$!      W "THIS JOB WILL BE FORCED TO ABORT BECAUSE THE REPORT OUT OF BALANCE."
$!      W "PLEASE INVESTIGATE, FIX THE ERROR, AND RESTART THE JOB"
$!      W "AS THE BELOW INSTRUCTION."
$!      W " "
$!      GOTO ERROR_CONDITION
$   ENDIF
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                !!
$!! WRAP_UP -- End                                                 !!
$!!                                                                !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ WRAP_UP:
$   SHOW PROCESS/ACC
$   SET  NOVERIFY       ! turn verify off for @com:termination
$   @COM:TERMINATION
$   IF PV THEN SET VERIFY ! restore prior verify setting
$   EXIT 1
$!
$ BALANCE_MSG:
$!
$   SET NOVERIFY
$   SET NOON
$   GOSUB RESET_MSG
$!
$   W ""
$   W "Hello CAP Team,"
$   W ""
$   IF CAP_BAL_BRX_11_STATUS .EQS. "I"
$   THEN
$      W "   The CAP_BAL_BRX_11 report is in balance for this capitation period."
$   ELSE
$      W "   The CAP_BAL_BRX_11 report is not in balance for this capitation period."
$!     W "   CAP job stream is being aborted."
$      W "   Please investigate.
$      EXIT_STAT :== "E"
$   ENDIF 
$   W ""
$   W ""
$   W ""
$   W F$FAO("!^")
$   DEASSIGN SYS$OUTPUT
$!
$   DIST_LIST = "@ACE_TEAM"
$   IF CAP_BAL_BRX_11_STATUS .EQS. "I"
$   THEN
$      HN_MAIL 'MSGFILE' 'DIST_LIST' "CAP_BAL_BRX_11: CAP IS IN BALANCE"
$   ELSE
$      HN_MAIL 'MSGFILE' 'DIST_LIST' "CAP_BAL_BRX_11: CAP IS OUT OF BALANCE"
$   ENDIF 
$!
$!
$ RETURN
$!-----------------------------------------------------------------------------
$! RESET_MSG - Define SYS$OUTPUT to a temp file to capture program messages.
$!----------------------------------------------------------------------------- 
$!
$ RESET_MSG:
$!   
$   IF F$SEARCH("''MSGFILE'") .EQS. "" THEN GOTO SYSOUT
$   DELETE/LOG  'MSGFILE';*
$!
$ SYSOUT:
$   DEFINE/LOG SYS$OUTPUT  'MSGFILE'
$ RETURN
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                !!
$!! ERROR_CONDITION -- Error Handling                              !!
$!!                                                                !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
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
