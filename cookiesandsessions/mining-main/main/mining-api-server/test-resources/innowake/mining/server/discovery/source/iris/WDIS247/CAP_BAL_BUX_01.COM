$!
$   PV = F$ENVIRONMENT("VERIFY_PROCEDURE") ! Save verify setting in case this
$   SET VERIFY                             ! comfile is '@'ed from another
$!
$!-----------------------------------------------------------------------------!
$!  H E A L T H   N E T   -   Automated Business System.                       !
$!-----------------------------------------------------------------------------!
$!  This software  source  code  module and all components referenced  herein, !
$!  are the proprietary properties and trade secrets of HEALTH NET.  Except as !
$!  provided for by license agreement,  this software source code module shall !
$!  not be duplicated, used or disclosed without written consent, signed by an !
$!  officer of HEALTH NET. COPYRIGHT © 2002 By HEALTH NET CORPORATION,         !
$!  Woodland Hills, CA All rights reserved.                                    !
$!-----------------------------------------------------------------------------!
$! Start Documentation:
$!
$!      SYSTEM:         CAPITATION
$! 	JOB:    	CAP_BAL_BUX_01.COM
$! 	TITLE:   	PRVTTLS AUDIT/UPDATE
$!      DATE:           30-Jan-2004
$! 	AUTHOR:  	John Wang
$!
$!     OVERVIEW:	This command procedure cloned from ACE_BAL_BUX_01.COM.
$!			Temporarily, it uses ICPROG_STAT instead of uses
$!			CAPREGN_REGION_CONTROL.  We will need to re-write this
$!			command procedure latter.
$!			
$!
$!
$!      MOD:    {DD-MMM-CCYY}   {REF#}          {USER}
$!
$!		25-FEB-2004			John Wang
$!		Fixed the parameter to prog_date_bom_yyyymmdd.
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
$   THIS_COMFILE = F$ENVIRONMENT("PROCEDURE")       ! This job's filespec
$   JOB_NAME    == F$PARSE(THIS_COMFILE,,,"NAME")   ! Parse out the name
$   PID          = F$GETJPI("","PID")               ! Handy for temp filenames
$   RUN_STATUS   = "0"
$   CR[0,8]      = 13
$   LF[0,8]      = 10
$   CRLF         = CR + LF  ! Carriage return/linefeed for e-mail text
$   ERR_MSG  = "ERROR ENCOUNTERED WHILE RUNNING " + JOB_NAME + CRLF
$   @COM:INITIALIZATION
$!
$   SET NOVERIFY
$!
$   ECHO  := WRITE SYS$OUTPUT
$   ECHO ""
$   ECHO ""
$   ECHO "                PID=''PID'"
$   ECHO "       Process Name=''F$GETJPI("","PRCNAM")'"
$   ECHO "          Node Name=''F$GETJPI("","NODENAME")'"
$   ECHO "           Job Type=''F$GETJPI("","JOBTYPE")'"
$   ECHO "        VMS Version=''F$GETJPI("","NODE_VERSION")'"
$   ECHO "  Username(account)=''F$GETJPI("","USERNAME")'"
$   ECHO "     Procedure name=''JOB_NAME'"
$   ECHO ""                            
$   ECHO "                 P1=''p1'"
$   ECHO "                 P2=''p2'"
$   ECHO "                 P3=''p3'"
$   ECHO "                 P4=''p4'"
$   ECHO "                 P5=''p5'"
$   ECHO ""
$!
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
$!! Step 1 -- @COM:SYS_SCH_BRU_01.COM ACE_ACT_EFF	and	  !!
$!!	      @COM:ACE_BAL_BUX_01.COM 'prog_date_bom_yyyymmdd U	  !!
$!!                                                               !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ STEP_1:
$   RUN_STATUS = "1"
$   SHOW PROCESS/ACC
$!
$ @COM:SYS_SCH_BRU_01.COM ACE_ACT_EFF
$ @COM:ACE_BAL_BUX_01.COM 'prog_date_bom_yyyymmdd' U
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                !!
$!! WRAP_UP -- End                                                 !!
$!!                                                                !!
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
$!!                                                                !!
$!! ERROR_CONDITION -- Error Handling                              !!
$!!                                                                !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ ERROR_CONDITION:
$   IF F$TYPE(ERROR_STATUS) .NES. "INTEGER" -
        THEN ERROR_STATUS = F$INTEGER($STATUS)
$   MAIL_LST = "@OPR_TEAM, @ACE_TEAM"
$   SET  NOVERIFY ! turn verify off for @com:batch_error
$   @COM:BATCH_ERROR 'ERROR_STATUS' 'RUN_STATUS' "''MAIL_LST'" "''ERR_MSG'"
$   IF PV THEN SET VERIFY ! restore prior verify setting
$   EXIT 44
$!
$!-------------------------------------------------------------------------
$!                      End of Job
$!-------------------------------------------------------------------------
