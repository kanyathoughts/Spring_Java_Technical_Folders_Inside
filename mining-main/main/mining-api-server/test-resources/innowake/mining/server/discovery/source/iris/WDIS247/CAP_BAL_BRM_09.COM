$ SAVED_VERIFY=F$VERIFY(0) 
$ SET NOVERIFY
$! This program,  and all the routines referenced  herein,   are the
$! proprietary properties and trade secrets of HEALTH NET. Except as
$! provided for  by license agreement,  this  program  shall not  be
$! duplicated, used or  disclosed  without  written  consent, signed
$! by an officer of HEALTH NET.
$!
$! Start Documentation
$!	SYSTEM:  	CAP
$!      JOB:            CAP 27 To 30 Report Balancing
$! 	TITLE:   	CAP_BAL_BRM_09.COM
$! 	DATE:    	16-SEP-2003.
$! 	AUTHOR:  	JHW/DDB
$!
$!	OVERVIEW:
$!
$!	  This command procedure cloned from ACE_BAL_BRM_09.com (written by MOO)
$!	  This command procedure balances the CAP_RPT_BRM_27 and
$!	  CAP_RPT_BRM_30 reports.  
$!
$!     CHANGES:
$!
$! mod#03 20091005 JLR	TICKET #855851
$!	I don't see how step 3 could ever have worked, but i've broken
$!	it into two steps and fixed it (the problem was that, john wang was
$!	trying to extract the provider id for the ACE_30 report from the 
$!	detail line, whereas the provider id appears on a line above the
$!	detail)
$!	
$! AUG-31-2004	      John Wang
$! Set verify.  Delete old cap work files.
$!
$! JUL-30-2004        John Wang
$! Added new step (step 3) to read cap27 and cap30 reports, then
$! dump the difference by ppg and category.  Append these results to message
$! and send out to user.
$!
$! End Documentation
$!
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "           NODENAME=''F$GETJPI("","NODENAME")'"
$ WRITE SYS$OUTPUT "                PID=''f$getjpi("","pid")'"
$ WRITE SYS$OUTPUT "       Process Name=''f$getjpi("","prcnam")'"
$ WRITE SYS$OUTPUT "  Username(account)=''f$getjpi("","username")'"
$ WRITE SYS$OUTPUT "     Procedure name=''F$ENVIRONMENT("PROCEDURE")'"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT ""
$
$!
$ IF SAVED_VERIFY .EQ. 1 THEN SET VERIFY
$!
$ ON ERROR   THEN GOTO ERROR_CONDITION
$!
$ INITIALIZATION:
$   JOB_NAME :== "CAP_BAL_BRM_09"
$   @COM:INITIALIZATION
$
$   HN_MAIL   := $EXE:ACE_MAI_ORX_01
$   DCL_ABORT  =  196652
$   MSGFILE   := CAPWORK:CAP_BAL_BRM_09.MSG
$   W         := WRITE SYS$OUTPUT
$!
$!  Build Distribution List
$!
$   DIST_ERR  = "@ACE_TEAM"
$   DIST_LIST = "@ACE_TEAM,@OPR_TEAM,@CAP_UNIT"
$!
$   EXIT_STAT :== ""
$!
$   BAL_RPT := "CAP_RPT:CAP_BAL_BRM_09.RPT"
$!
$   @COM:SYS_SCH_BRU_01 "ACE_RPT_EFF"
$   CAP_PER = PROG_DATE_EOM_YYYYMMDD
$   SHOW SYM CAP_PER
$!
$   RPT_27  := ACE_RPT:ACE_RPT_BRM_27.RPT__'CAP_PER'_M_H
$   RPT_30  := ACE_RPT:ACE_RPT_BRM_30.RPT__'CAP_PER'_M_H
$!
$   DEFINE ACE_RPT_BRM_27 'RPT_27'
$   DEFINE ACE_RPT_BRM_30 'RPT_30'
$!
$   DEFINE CAP2730_SEQ_FILE  CAPWORK:CAP2730_SEQ.DAT
$   DEFINE CAP2730_DIF_FILE  CAPWORK:CAP2730_DIF.DAT
$!
$   CAP_BAL_BRM_09_STATUS :== "I"
$!
$   IF P1 .NES. "" THEN GOTO STEP'P1
$!
$!*****************************************************************************
$ STEP1:
$! In step 1 we will see if the BRM_27 and BRM_30 reports for the current cap
$! period have been generated.
$!*****************************************************************************
$!
$   SAVE_STEP_NUMBER := "1"
$   SHOW PROCESS/ACCOUNTING
$   SET VERIFY
$!
$   MSG = "FNR"
$   OPEN/READ/SHARE/ERROR=FILE_NOT_READY  A_RPT 'RPT_27'
$   CLOSE A_RPT 
$   OPEN/READ/SHARE/ERROR=FILE_NOT_READY  A_RPT 'RPT_30'
$   CLOSE A_RPT 
$!
$   GOTO STEP2
$!
$ FILE_NOT_READY:
$   GOTO INFO_MSG_EXIT
$!
$!*****************************************************************************
$ STEP2:
$!  Call CAP_BAL_BRM_09.EXE to do our balancing.
$!*****************************************************************************
$!
$   SAVE_STEP_NUMBER := "2"
$   SHOW PROCESS/ACCOUNTING
$   SET VERIFY
$!
$   RUN EXE:CAP_BAL_BRM_09.EXE
$!
$   SHOW SYMBOL CAP_BAL_BRM_09_STATUS
$!
$!
$!*****************************************************************************
$ STEP3:
$!  step 3 deletes the work files; i'm separating this part from step 4 so
$!  that i can manipulate the work files during testing
$!*****************************************************************************
$!
$   SAVE_STEP_NUMBER := "3"
$   SHOW PROCESS/ACCOUNTING
$!
$   IF  F$SEARCH("CAPWORK:CAP27_WORK.SEQ") .NES. ""
$   THEN DELETE/NOCONFIRM/LOG CAPWORK:CAP27_WORK.SEQ;*
$   ENDIF
$!
$   IF  F$SEARCH("CAPWORK:CAP30_RMC.SEQ") .NES. ""
$   THEN DELETE/NOCONFIRM/LOG CAPWORK:CAP30_RMC.SEQ;*
$   ENDIF
$!
$   IF  F$SEARCH("CAPWORK:CAP2730_SEQ.DAT") .NES. ""
$   THEN DELETE/NOCONFIRM/LOG CAPWORK:CAP2730_SEQ.DAT;*
$   ENDIF
$!
$   IF  F$SEARCH("CAPWORK:CAP2730_DIF.DAT") .NES. ""
$   THEN DELETE/NOCONFIRM/LOG CAPWORK:CAP2730_DIF.DAT;*
$   ENDIF
$!
$!*****************************************************************************
$ STEP4:
$!  If 09 Report out of balance, we need:
$!  1) GOSUB to read ace_bal_09 in tables master to get rmcs
$!  2) GOSUB to read ppg and amount from cap27 report 
$!  3) GOSUB to read ppg, rmc, and amount from cap30 report
$!  4) SORT cap27_work.seq and cap30_rmc.seq to cap2730_seq.dat
$!  5) RUN cap_bal_brm_09A.exe to dump the differences
$!  6) WRITE the differences to message and send out
$!*****************************************************************************
$!
$   SAVE_STEP_NUMBER := "4"
$   SHOW PROCESS/ACCOUNTING
$!
$!  Run only if it is out of balance.
$!
$   IF P2 .EQS. "O" THEN CAP_BAL_BRM_09_STATUS = "O" ! force OOB for testing
$   IF CAP_BAL_BRM_09_STATUS .NES. "I"
$   THEN
$        GOSUB READ_ACEBAL09
$        GOSUB READ_CAP27
$        GOSUB READ_CAP30_RMC
$        SORT CAPWORK:CAP27_WORK.SEQ, CAPWORK:CAP30_RMC.SEQ -
              CAPWORK:CAP2730_SEQ.DAT/STAT
$        LOGDIR EXE:CAP_BAL_BRM_09A.EXE
$        RUN/NODEBUG EXE:CAP_BAL_BRM_09A.EXE
$   ENDIF
$!
$   SET VERIFY
$!
$!  If in balance, only 09 report appends.  If out of balance,
$!  we also append a report of differences.
$   GOSUB BALANCE_MSG
$!
$!------------------------------------------------------------------------------
$! WRAP_UP - exit the balancing with normal status
$!------------------------------------------------------------------------------
$!
$ WRAP_UP:
$   SET VERIFY
$   GOSUB CLEAN_UP
$!
$!   IF EXIT_STAT .NES. ""
$!   THEN
$!       @COM:ACE_OPCOM_MSG.COM "CAP_BAL_BRM_09.COM"
$!	EXIT DCL_ABORT
$!   ENDIF
$!
$   WRITE SYS$OUTPUT "**********************************************"
$   WRITE SYS$OUTPUT "*    The CAP_BAL_BRM_09 Completed Successfully"
$   WRITE SYS$OUTPUT "**********************************************"
$!
$   @COM:TERMINATION
$   EXIT 1

$!
$!-----------------------------------------------------------------------------
$! Read ACE_BAL_09 Records in Tables_Master to get the rmc codes
$! Put these rmc codes to 1 line of record
$!----------------------------------------------------------------------------- 
$!
$READ_ACEBAL09:
$ OPEN/READ  INPUT_FILE  TABLES_MASTER
$ OPEN/WRITE OUTPUT_FILE CAPWORK:ACEBAL09_WORK.SEQ
$ FIRST_REC = "Y"
$ OUT_LINE  = ""
$ READ_AGAIN:
$ READ/END_OF_FILE=EXIT_READ_ACEBAL09 INPUT_FILE DATA_LINE
$
$ IF F$EXTRACT (0, 12, DATA_LINE) .EQS. "ACE_BAL_09  "
$ THEN 
$      IF FIRST_REC .EQS. "N"
$      THEN
$          OUT_LINE = OUT_LINE + F$EXTRACT (12, 3, DATA_LINE) + ";"
$      ENDIF
$      FIRST_REC = "N"
$ ENDIF
$!
$   SET NOVERIFY
$ GOTO READ_AGAIN
$!
$ EXIT_READ_ACEBAL09:
$!
$   SET VERIFY
$ WRITE OUTPUT_FILE OUT_LINE
$ CLOSE INPUT_FILE
$ CLOSE OUTPUT_FILE
$ RETURN
$!
$!-----------------------------------------------------------------------------
$! Read CAP27 Report to generate a sequential file.
$! This sequential file contains ppg and cap amount in each record.
$!----------------------------------------------------------------------------- 
$!
$ READ_CAP27:
$!
$ OPEN/READ  INPUT_FILE  ACE_RPT_BRM_27
$ OPEN/WRITE OUTPUT_FILE CAPWORK:CAP27_WORK.SEQ
$ READ_AGAIN_CAP27:
$ READ/END_OF_FILE=EXIT_READ_CAP27 INPUT_FILE DATA_LINE
$
$ IF F$EXTRACT (127, 1, DATA_LINE) .EQS. "."
$ THEN
$      OUT_LINE = F$EXTRACT (2, 5, DATA_LINE)  + "            " + -
           "                    " + F$EXTRACT (51, 13, DATA_LINE)
$      IF F$EXTRACT (51, 13, DATA_LINE) .NES. "         0.00"
$      THEN
$           WRITE OUTPUT_FILE OUT_LINE
$      ENDIF
$      OUT_LINE = F$EXTRACT (68, 5, DATA_LINE) + "            " + -
           "                    " + F$EXTRACT (117, 13, DATA_LINE)
$      IF F$EXTRACT (117, 13, DATA_LINE) .NES. "         0.00"
$      THEN
$           WRITE OUTPUT_FILE OUT_LINE
$      ENDIF
$ ENDIF
$!
$  SET NOVERIFY
$ GOTO READ_AGAIN_CAP27
$!
$ EXIT_READ_CAP27:
$!
$  SET VERIFY
$ CLOSE INPUT_FILE
$ CLOSE OUTPUT_FILE
$ RETURN
$!
$!-----------------------------------------------------------------------------
$! Read CAP30 Report to generate a sequential file.
$! This sequential file contains ppg, rmc, and cap amount in each record.
$!  (see notes for mod#03)
$!----------------------------------------------------------------------------- 
$!
$ READ_CAP30_RMC:
$!
$ OPEN/READ  RMC_FILE    CAPWORK:ACEBAL09_WORK.SEQ
$ OPEN/READ  INPUT_FILE  ACE_RPT_BRM_30
$ OPEN/WRITE OUTPUT_FILE CAPWORK:CAP30_RMC.SEQ
$!
$ READ/END_OF_FILE=EXIT_RMC_READ RMC_FILE DATA_RMC
$!
$ EXIT_RMC_READ:
$!
$ CLOSE RMC_FILE
$!
$ READ_AGAIN_CAP30:
$ READ/END_OF_FILE=EXIT_READ_CAP30 INPUT_FILE DATA_LINE
$
$ IF F$EXTRACT (2, 3, DATA_LINE) .EQS. "ALL" THEN GOTO EXIT_READ_CAP30
$!
$ IF F$EXTRACT (0, 2, DATA_LINE) .EQS. "  " -
  .AND. F$EXTRACT (2, 1, DATA_LINE) .NES. " " 		! prv id
$ THEN
$          PRV_TYPE_ID = F$EXTRACT (2, 5, DATA_LINE)
$ ENDIF
$ IF F$EXTRACT (128, 1, DATA_LINE) .EQS. "." -
  .AND. F$EXTRACT (7, 5, DATA_LINE) .NES. "TOTAL"
$ THEN
$	RMC_CAT = F$EXTRACT (9, 3, DATA_LINE)
$	OUT_LINE = PRV_TYPE_ID + "                " + -
                 RMC_CAT + "         " + -
                 F$EXTRACT (114, 17, DATA_LINE)
$!
$     IF F$LOCATE (RMC_CAT, DATA_RMC) .NE. F$LENGTH (DATA_RMC)
$     THEN
$          WRITE OUTPUT_FILE OUT_LINE
$     ENDIF
$ ENDIF
$!
$  SET NOVERIFY
$ GOTO READ_AGAIN_CAP30
$!
$ EXIT_READ_CAP30:
$!
$  SET VERIFY
$ CLOSE INPUT_FILE
$ CLOSE OUTPUT_FILE
$ RETURN
$
$!
$!-----------------------------------------------------------------------------
$! RESET_MSG - Define SYS$OUTPUT to a temp file to capture program messages.
$!----------------------------------------------------------------------------- 
$!
$ RESET_MSG:
$!   
$   IF F$SEARCH("''MSGFILE'") .EQS. "" THEN GOTO SYSOUT
$   DELETE/LOG	'MSGFILE';*
$!
$ SYSOUT:
$   DEFINE/LOG SYS$OUTPUT  'MSGFILE'
$   SET NOVERIFY
$ RETURN
$!
$ CLEAN_UP:
$   IF F$SEARCH("''MSGFILE'") .NES. "" THEN DELETE/LOG 'MSGFILE';*
$ RETURN
$!
$!
$ BALANCE_MSG:
$!
$   SET NOON
$   GOSUB RESET_MSG
$!
$   W ""
$   W "Hello CAP Team and OPR Team,
$   W ""
$   IF CAP_BAL_BRM_09_STATUS .EQS. "I"
$   THEN
$      W "   The CAP 27 and 30 reports are in balance for this capitation period."
$   ELSE
$      W "   The CAP 27 and 30 reports are not in balance for this capitation period."
$      W "   Monthend CAP job stream is being aborted."
$      W "   Please investigate.
$      EXIT_STAT :== "E"
$   ENDIF 
$   W ""
$   W ""
$   W ""
$   W F$FAO("!^")
$   DEASSIGN SYS$OUTPUT
$   SET VERIFY
$   IF F$SEARCH("''BAL_RPT'") .NES. "" 
$   THEN 
$      APPEND/LOG 'BAL_RPT' 'MSGFILE'
$   ENDIF
$!
$!  If Status is out of balance, we should append differences between cap27 and cap30
$!  to message.
$!
$   IF CAP_BAL_BRM_09_STATUS .NES. "I"
$   THEN
$      APPEND/LOG CAPWORK:CAP2730_DIF.DAT 'MSGFILE'
$   ENDIF
$!
$ DEASSIGN SYS$OUTPUT
$   SET VERIFY
$!
$   IF CAP_BAL_BRM_09_STATUS .EQS. "I"
$   THEN
$      HN_MAIL 'MSGFILE' 'DIST_LIST' "CAP_BAL_BRM_09: CAP IS IN BALANCE"
$   ELSE
$      HN_MAIL 'MSGFILE' 'DIST_LIST' "CAP_BAL_BRM_09: CAP IS OUT OF BALANCE (program aborted)"
$      EXIT DCL_ABORT
$   ENDIF 
$!
$!
$ RETURN
$!
$!
$ INFO_MSG_EXIT:
$!
$   SET VERIFY
$   SET NOON
$   GOSUB RESET_MSG
$!
$ IF MSG .EQS. "FNR"
$ THEN
$   W "To CAP Oncall,
$   W ""
$   W "   The ACE_RPT_BRM_27 and/or ACE_RPT_BRM_30 reports were not available."
$   W "   This procedure, CAP_BAL_BRM_09.COM, is aborting in order to halt"
$   W "   the job stream, as this balancing job is critical to monthend"
$   W "   processing."
$   W ""
$   w ""
$ ENDIF
$!
$ DEASSIGN SYS$OUTPUT
$$   SET VERIFY
!
$ $STATUS      = DCL_ABORT
$ ERROR_STATUS = DCL_ABORT
$ GOTO INFO_EXIT
$!
$!
$!-----------------------------------------------------------------------------
$! WRITE_TIME_MSG - Write operated submit time message.
$!-----------------------------------------------------------------------------
$!
$ WRITE_TIME_MSG:
$   WRITE SYS$OUTPUT F$FAO						-
	( "Operator submitted CAP_BAL_BRM_09 !AS"		-
	, F$GETJPI( "", "LOGINTIM" )					-
	, 0								-
	)
$ RETURN
$!
$ PROCEDURE_NAME:
$ PROCEDURE_NAME = F$PARSE( F$ENVIRONMENT( "PROCEDURE" ),,,"NAME" )
$ IF F$MODE() .EQS. "BATCH"
$ THEN
$   LOG_FILE	    = F$GETQUI("DISPLAY_JOB","LOG_SPECIFICATION",,"THIS_JOB")
$   PARSED_LOG_FILE = F$PARSE(LOG_FILE,PROCEDURE_NAME,,,"SYNTAX_ONLY")
$   DIR_RESULT	    = F$SEARCH(PARSED_LOG_FILE)
$ ELSE
$   IF F$MODE() .EQS. "OTHER"
$   THEN
$	LOG_FILE	= NSCHED_OUTFILE
$   	PARSED_LOG_FILE = F$PARSE(LOG_FILE,PROCEDURE_NAME,,,"SYNTAX_ONLY")
$   	DIR_RESULT      = F$SEARCH(PARSED_LOG_FILE)
$ 	ELSE
$	    DIR_RESULT  = " - No log, mode is ''F$MODE()'"
$	ENDIF
$ ENDIF
$ RETURN
$
$!
$!------------------------------------------------------------------------------
$! ERROR_CONDITION - exit the Reporting Batch with error status
$!------------------------------------------------------------------------------
$!
$ ERROR_CONDITION: 
$   ERROR_STATUS = '$STATUS'
$!
$   SET VERIFY
$   GOSUB RESET_MSG
$   GOSUB WRITE_TIME_MSG
$   SET NOON
$!
$   GOSUB PROCEDURE_NAME
$   WRITE SYS$OUTPUT F$FAO		-
	("!2(/)On step number !AS"	-
	 + "!/Check the log file !AS"	-
	, F$STRING(SAVE_STEP_NUMBER)	-
	, DIR_RESULT			-
	)
$!
$   WRITE SYS$OUTPUT ""
$!
$ ERROR_EXIT:
$   DEASSIGN SYS$OUTPUT
$   SET VERIFY
$   HN_MAIL 'MSGFILE' 'DIST_ERR'					    -
   "CAP_BAL_BRM_09: Error occurred in CAP_BAL_BRM_09"
$   IF ERROR_STATUS THEN ERROR_STATUS = 44 !Ensure that we exit with an error.
$   EXIT ERROR_STATUS
$!
$!
$!
$ ERR_EXIT:
$! 
$   ERROR_STATUS = '$STATUS'
$   SET VERIFY
$   W ""
$   W ""
$   W F$FAO					        - 
	( "Operator submitted CAP_BAL_BRM_09 !AS"	 	        -
	+ "!/Job failed at !%D "				        -
	, F$GETJPI( "", "LOGINTIM" )					-
	, 0								-
	)
$   WARN_MSG = "NO"
$   GOSUB LOG_FILE_INFO
$   HN_MAIL 'MSGFILE' 'DIST_ERR' -
    "CAP_BAL_BRM_09: Error occurred in CAP_BAL_BRM_09.COM"
$!
$   GOSUB CLEAN_UP
$   IF ERROR_STATUS THEN ERROR_STATUS = 42 !Ensure that we exit with an error.
$   EXIT ERROR_STATUS
$!
$!
$ INFO_EXIT:
$! 
$   ERROR_STATUS = '$STATUS'
$   HN_MAIL 'MSGFILE' 'DIST_ERR' -
    "CAP_BAL_BRM_09: Warning(s) or errors occurred in CAP_BAL_BRM_09.COM"
$!
$   GOSUB CLEAN_UP
$   IF ERROR_STATUS THEN ERROR_STATUS = 42 !Ensure that we exit with an error.
$   EXIT ERROR_STATUS
$!
$!
$!------------------------------------------------------------------------------
$! LOG_FILE_INFO - Will get out location of the log file 
$!------------------------------------------------------------------------------
$!
$ LOG_FILE_INFO:
$   SET NOON
$   PROCEDURE_NAME = F$PARSE( F$ENVIRONMENT( "PROCEDURE" ),,,"NAME" )
$   IF F$MODE() .EQS. "BATCH"
$   THEN
$     LOG_FILE	    = F$GETQUI("DISPLAY_JOB","LOG_SPECIFICATION",,"THIS_JOB")
$     PARSED_LOG_FILE = F$PARSE(LOG_FILE,PROCEDURE_NAME,,,"SYNTAX_ONLY")
$     DIR_RESULT	    = F$SEARCH(PARSED_LOG_FILE)
$   ELSE
$     IF F$MODE() .EQS. "OTHER"
$     THEN
$	  LOG_FILE	= NSCHED_OUTFILE
$   	  PARSED_LOG_FILE = F$PARSE(LOG_FILE,PROCEDURE_NAME,,,"SYNTAX_ONLY")
$   	  DIR_RESULT      = F$SEARCH(PARSED_LOG_FILE)
$     ELSE
$  	  DIR_RESULT  = " - No log, mode is ''F$MODE()'"
$     ENDIF
$   ENDIF
$!
$   IF WARN_MSG .EQS. "YES"
$   THEN
$      W F$FAO						    -
 	   ("!/The log file is !AS"					    -
	   , DIR_RESULT							    -
	   )
$   ELSE
$      W F$FAO						    -
	   ("!2(/)On step number !AS"					    -
	    + "!/Check the log file !AS"				    -
	   , F$STRING(SAVE_STEP_NUMBER)					    -
	   , DIR_RESULT							    -
	   )
$   ENDIF
$!
$!
$   W ""
$!
$   DEASSIGN SYS$OUTPUT
$   SET VERIFY
$!
$ RETURN
$!
$!-------------------------------------------------------------------------
$!                      End of Job CAP_BAL_BRM_09
$!-------------------------------------------------------------------------
