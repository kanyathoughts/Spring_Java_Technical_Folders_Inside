$! Beg Mod #1
$ SAVED_VERIFY=F$VERIFY(0) 
$ SET NOVERIFY
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "                PID=''f$getjpi("","pid")'"
$ WRITE SYS$OUTPUT "       Process Name=''f$getjpi("","prcnam")'"
$ WRITE SYS$OUTPUT "  Username(account)=''f$getjpi("","username")'"
$ WRITE SYS$OUTPUT "     Procedure name=''F$ENVIRONMENT("PROCEDURE")'"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT ""
$ IF SAVED_VERIFY .EQ. 1 THEN SET VERIFY
$!
$!
$ ON ERROR   THEN GOTO ERROR_CONDITION
$!
$!
$! Initialization
$!
$ INITIALIZATION:
$   JOB_NAME :== "CAP_BAL_BRM_08"
$   @COM:INITIALIZATION
$
$   HN_MAIL     := $EXE:ACE_MAI_ORX_01
$   MSGFILE     := CAPWORK:CAP_BAL_BRM_08.MSG
$!
$!  Build DIST_LIST
$!
$   DIST_ERR  = "@ACE_TEAM"
$   DIST_LIST = "@ACE_TEAM,@OPR_TEAM" 
$!
$! Mod #02 Start
$   BAL_MORE_100     = "M"
$! Mod #02 End
$   DEFINE CAP_BAL_BRM_01_REPORT_FILE CAP_RPT:CAP_BAL_BRM_08.RPT
$   BAL_RPT := "CAP_RPT:CAP_BAL_BRM_08.RPT"
$!
$   @COM:SYS_SCH_BRU_01 "ACE_ACT_EFF"
$   @COM:CAP_BAL_BRM_09 "ARTIFACT"
$   CAP_PER = PROG_DATE_EOM_YYYYMMDD
$   SHOW SYM CAP_PER
$!
$   RPT_13A := ACE_RPT:ACE_RPT_BRM_13A.RPT__'CAP_PER'_M_H
$   RPT_15  := ACE_RPT:ACE_RPT_BRM_15.RPT__'CAP_PER'_M_H
$   RPT_20  := ACE_RPT:ACE_RPT_BRM_20.RPT__'CAP_PER'_M_H
$   RPT_21M := ACE_RPT:ACE_RPT_BRM_21M.RPT__'CAP_PER'_M_H
$   RPT_30  := ACE_RPT:ACE_RPT_BRM_30.RPT__'CAP_PER'_M_H
$!
$   DEFINE ACE_RPT_BRM_13A 'RPT_13A'
$   DEFINE ACE_RPT_BRM_15  'RPT_15'
$   DEFINE ACE_RPT_BRM_20  'RPT_20'
$   DEFINE ACE_RPT_BRM_21M 'RPT_21M'
$   DEFINE ACE_RPT_BRM_30  'RPT_30'
$!
$   RPT_DATE =  F$CVTIME(F$FILE_ATTRIBUTES("''RPT_13A'","CDT"),"COMPARISON")
$   RPT_DATE =  F$EXTRACT(0,4,RPT_DATE) + F$EXTRACT(5,2,RPT_DATE) +    -
                F$EXTRACT(8,2,RPT_DATE) 
$!
$!*****************************************************************************
$! STEP 1 - Execute CAP_RPT_BRM_08.EXE
$!*****************************************************************************
$!
$ STEP001:
$   SAVE_STEP_NUMBER := "001"
$   SHOW PROCESS/ACCOUNTING
$   SET VERIFY
$!
$   CAP_BAL_BRM_08_STATUS :== "I"
$!
$   RUN/NODEBUG EXE:CAP_BAL_BRM_08.EXE
$   RUN/NODEBUG EXE:CAP_BAL_BRM_09.EXE
$   RUN/NODEBUG EXE:BASIC_PROGRAM
$!
$   SHOW SYMBOL CAP_BAL_BRM_08_STATUS
$!
$    IF CAP_BAL_BRM_08_STATUS .NES. "I"
$    THEN
$! Mod #02 Start
$      IF CAP_BAL_BRM_08_STATUS .EQS. BAL_MORE_100
$      THEN	
$      	  GOTO ERROR_CONDITION2
$      ELSE	
$         W "   The reports are not in balance for this capitation period."
$         W "   Monthend CAP Job stream is running as the oob is less than 100."
$         W "   Take a look at the log for the out of balance values. "
$! Mod #03 Start
$         GOTO WRAP_UP
$! Mod #03 End
$      ENDIF
$! Mod #02 End
$    ELSE
$      GOTO WRAP_UP
$    ENDIF
$!
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
$ CLEAN_UP:
$   IF F$SEARCH("''MSGFILE'") .NES. "" THEN DELETE/LOG 'MSGFILE';*
$ RETURN
$!
$!-----------------------------------------------------------------------------
$! WRITE_TIME_MSG - Write operated submit time message.
$!-----------------------------------------------------------------------------
$!
$ WRITE_TIME_MSG:
$   WRITE SYS$OUTPUT F$FAO                                              -
        ( "Operator submitted CAP_BAL_BRM_08 !AS"               -
        , F$GETJPI( "", "LOGINTIM" )                                    -
        , 0                                                             -
        )
$ RETURN
$!
$ PROCEDURE_NAME:
$ PROCEDURE_NAME = F$PARSE( F$ENVIRONMENT( "PROCEDURE" ),,,"NAME" )
$ IF F$MODE() .EQS. "BATCH"
$ THEN
$   LOG_FILE        = F$GETQUI("DISPLAY_JOB","LOG_SPECIFICATION",,"THIS_JOB")
$   PARSED_LOG_FILE = F$PARSE(LOG_FILE,PROCEDURE_NAME,,,"SYNTAX_ONLY")
$   DIR_RESULT      = F$SEARCH(PARSED_LOG_FILE)
$ ELSE
$   IF F$MODE() .EQS. "OTHER"
$   THEN
$       LOG_FILE        = NSCHED_OUTFILE
$       PARSED_LOG_FILE = F$PARSE(LOG_FILE,PROCEDURE_NAME,,,"SYNTAX_ONLY")
$       DIR_RESULT      = F$SEARCH(PARSED_LOG_FILE)
$       ELSE
$           DIR_RESULT  = " - No log, mode is ''F$MODE()'"
$       ENDIF
$ ENDIF
$ RETURN
$!
$!------------------------------------------------------------------------------
$! ERROR_CONDITION - exit the Reporting Batch with error status
$!------------------------------------------------------------------------------
$!
$ ERROR_CONDITION: 
$   ERROR_STATUS = '$STATUS'
$!
$   SET NOVERIFY
$   GOSUB RESET_MSG
$   GOSUB WRITE_TIME_MSG
$   SET NOON
$!
$   GOSUB PROCEDURE_NAME
$   WRITE SYS$OUTPUT F$FAO              -
        ("!2(/)On step number !AS"      -
         + "!/Check the log file !AS"   -
        , F$STRING(SAVE_STEP_NUMBER)    -
        , DIR_RESULT                    -
        )
$!
$   WRITE SYS$OUTPUT ""
$!
$ ERROR_EXIT:
$   DEASSIGN SYS$OUTPUT
$   IF ERROR_STATUS THEN ERROR_STATUS = 44 !Ensure that we exit with an error.
$   EXIT ERROR_STATUS
$!
$!
$ ERROR_CONDITION2:
$   ERROR_STATUS = '$STATUS'
$   SET NOVERIFY
$   GOSUB RESET_MSG
$   GOSUB WRITE_TIME_MSG
$   SET NOON
$!
$   IF CAP_BAL_BRM_08_STATUS .NES. "I"
$   THEN
$     WRITE SYS$OUTPUT F$FAO                                                -
        ( "CAP_BAL_BRM_08 - OUT OF BALANCE !/!AS"                           -
        + "!/!AS"                                                           -
        , ""                                                                -
        , ""                                                                -
        )
$   ELSE
$     WRITE SYS$OUTPUT F$FAO                                                -
        ( "CAP_BAL_BRM_08 - PROCESSING ERROR !AS"                           -
         + "!/!AS"                                                          -
        , ""                                                                -
        , ""                                                                -
        )
$   ENDIF
$!
$   GOSUB PROCEDURE_NAME
$   WRITE SYS$OUTPUT F$FAO              -
        ("!/The log file is !AS"        -
        , DIR_RESULT                    -
        )
$   WRITE SYS$OUTPUT ""
$!
$   GOTO ERROR_EXIT2
$!
$ ERROR_EXIT2:
$   SET ON
$   ON ERROR   THEN GOTO ERROR_CONDITION
$      IF CAP_BAL_BRM_08_STATUS .NES. "I"
$      THEN 
$         WRITE SYS$OUTPUT ""
$         WRITE SYS$OUTPUT -
"------------------------------------------------------------------------"
$         WRITE SYS$OUTPUT -
"The CAP System is not in Balance.  Please research the cause of"
$         WRITE SYS$OUTPUT -
"the Out of Balance Condition."
$         WRITE SYS$OUTPUT -
"------------------------------------------------------------------------"
$	  DEASSIGN SYS$OUTPUT
$         APPEND/LOG 'BAL_RPT' 'MSGFILE'
$         HN_MAIL 'MSGFILE' 'DIST_LIST' "CAP_BAL_BRM_08: OUT OF BALANCE (program aborted)"
$         GOTO ERROR_EXIT
$      ELSE
$         HN_MAIL 'MSGFILE' 'DIST_LIST' "CAP_BAL_BRM_08: IN BALANCE"
$      ENDIF
$!
$!------------------------------------------------------------------------------
$! WRAP_UP - exit the balancing with normal status
$!------------------------------------------------------------------------------
$!
$!
$ WRAP_UP:
$   SET NOVERIFY
$   GOSUB CLEAN_UP
$!
$   WRITE SYS$OUTPUT                                                           -
                    "**********************************************************"
$   WRITE SYS$OUTPUT                                                           -
                    "*    The CAP_BAL_BRM_08 Completed Successfully"
$   WRITE SYS$OUTPUT                                                           -
                    "**********************************************************"
$!
$   @COM:TERMINATION
$   EXIT 
$!
$!
