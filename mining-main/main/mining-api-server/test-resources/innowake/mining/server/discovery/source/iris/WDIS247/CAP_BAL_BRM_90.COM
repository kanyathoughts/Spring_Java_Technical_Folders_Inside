$! This program,  and all the routines referenced  herein,   are the
$! proprietary properties and trade secrets of HEALTH NET. Except as
$! provided for  by license agreement,  this  program  shall not  be
$! duplicated, used or  disclosed  without  written  consent, signed
$! by an officer of HEALTH NET.
$!
$! Start Documentation
$!      SYSTEM:         CAP
$!      JOB:            CAP_BAL_BRM_90.COM
$!      TITLE:          CAP Various Report Balancing
$!      DATE:           23-Jun-2003
$!      AUTHOR:         DDB
$!
$!      OVERVIEW:
$!
$!      This command procedure cloned from ACE_BAL_BRX_90.COM (written by MXB). 
$!      This command procedure performs report balancing on between various
$!      reports:
$!          ACE_RPT_BRM_15   ACE_RPT_BRM_20M  ACE_RPT_BRM_20P 
$!          ACE_RPT_BRM_21M  ACE_RPT_BRM_25   ACE_RPT_BRM_27
$!          ACE_RPT_BRM_24M  ACE_RPT_BRM_24P  ***Mod #3 *** Remove ACE_RPT_BRM_28
$!          ACE_RPT_BRM_30   ACE_RPT_BRM_39M  ACE_RPT_BRM_39P                   
$!  
$!
$!      Parameters:
$!          P1 - The step number to re-start this command procedure
$!
$!   02	08-JUL-2014     Deepa D Nair (DXH)      Remedy #HD1960387
$!      Business user authorized a change whereby if the out-of-balance
$!      is < 100, we should send an email msg but not abort the job.
$!      So adding the two conditions oob >100 and oob < 100 and if
$!      oob < 100 the job will not abort
$!
$!	30-JUL-2004	John Wang
$!      Define ERROR_STATUS and ERROR_MSG at right place.
$!
$! End Documentation
$!
$ INITIALIZATION:
$   JOB_NAME :== "CAP_BAL_BRM_90"
$   @COM:INITIALIZATION
$   W        := WRITE SYS$OUTPUT
$!
$!
$ SAVED_VERIFY=F$VERIFY(0) 
$ SET NOVERIFY
$ W ""
$ W ""
$ W "           NODENAME=''F$GETJPI("","NODENAME")'"
$ W "                PID=''f$getjpi("","pid")'"
$ W "       Process Name=''f$getjpi("","prcnam")'"
$ W "  Username(account)=''f$getjpi("","username")'"
$ W "     Procedure name=''F$ENVIRONMENT("PROCEDURE")'"
$ W ""
$ W ""
$ W "         P1 = ''P1'"
$ W ""
$!
$ IF SAVED_VERIFY .EQ. 1 THEN SET VERIFY
$!
$ ON ERROR   THEN GOTO ERROR_CONDITION
$!
$   HN_MAIL         := $EXE:ACE_MAI_ORX_01
$   DCL_ABORT        =  196652
$   MSGFILE         := CAPWORK:CAP_BAL_BRM_90.MSG
$   SAVE_STEP_NUMBER = "BEFORE STEP NUMBER WAS INITIALIZED."
$   THIS_COM         = F$ENVIRONMENT("PROCEDURE")
$   START_TIME       = F$TIME()
$   MSG              = "FNR"
$   DEFINE  MSGFILE  'MSGFILE'
$! Mod #02 Start
$   BAL_MORE_100     = "M"
$! Mod #02 End	
$! 
$!
$!  Build Distribution List
$!
$   DIST_ERR  = "@ACE_TEAM"
$   DIST_LIST = "@ACE_TEAM,@OPR_TEAM" 
$   EXIT_STAT :== ""
$!
$!
$   @COM:SYS_SCH_BRU_01 "ACE_RPT_EFF"
$   CAP_PER = PROG_DATE_EOM_YYYYMMDD
$   W   "ACE_RPT_EFF IS ''PROG_DATE_EOM_YYYYMMDD'"
$   BAL90_FILE :== CAP_RPT:CAP_BAL_BRM_90.RPT__'CAP_PER'
$!
$   DEFINE BAL_RPT          'BAL90_FILE'
$   DEFINE ACE_RPT_BRM_15    ACE_RPT:ACE_RPT_BRM_15.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_20M   ACE_RPT:ACE_RPT_BRM_20M.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_20P   ACE_RPT:ACE_RPT_BRM_20P.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_21M   ACE_RPT:ACE_RPT_BRM_21M.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_24M   ACE_RPT:ACE_RPT_BRM_24M.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_24P   ACE_RPT:ACE_RPT_BRM_24P.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_25    ACE_RPT:ACE_RPT_BRM_25.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_27    ACE_RPT:ACE_RPT_BRM_27.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_30    ACE_RPT:ACE_RPT_BRM_30.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_39M   ACE_RPT:ACE_RPT_BRM_39M.RPT__'CAP_PER'_M_H
$   DEFINE ACE_RPT_BRM_39P   ACE_RPT:ACE_RPT_BRM_39P.RPT__'CAP_PER'_M_H
$!
$!  PARAMETERS VALIDATION
$!
$   IF "''P1'" .NES. ""
$   THEN
$       P1      = F$EDIT(P1,"UPCASE")
$       SEARCH/NOOUT/NOLOG   'THIS_COM' "STEP''P1':"
$       IF $STATUS .NE.  %X00000001
$       THEN
$           MSG = "BADP1"
$           GOTO ERR_MSG_EXIT
$       ELSE
$            GOTO STEP'P1'
$       ENDIF
$   ENDIF 
$!
$   IF "''P2'" .NES. ""
$   THEN
$      P2      = F$EDIT(P2,"UPCASE")
$      PARAM2 := "''P2'"
$      IF F$SEARCH(F$STRING(PARAM2))  .EQS. ""
$      THEN
$         MSG = "BADP2"
$         GOTO ERR_MSG_EXIT
$      ENDIF
$      DIR 'PARAM2'
$      @ 'PARAM2'
$   ENDIF
$!
$!
$!*****************************************************************************
$ STEP1:
$!
$!  Call CAP_BAL_BRM_90.EXE to do our balancing.
$!*****************************************************************************
$!
$   SAVE_STEP_NUMBER := "1"
$   SHOW PROCESS/ACCOUNTING
$   SET VERIFY
$!
$   CAP_BAL_BRM_90_STATUS :== "I"
$!
$   RUN/NODEBUG  EXE:CAP_BAL_BRM_90.EXE
$!
$   SHOW SYMBOL CAP_BAL_BRM_90_STATUS
$!
$   IF CAP_BAL_BRM_90_STATUS .NES. "I"
$   THEN
$        GOSUB BALANCE_MSG
$   ENDIF
$!
$!------------------------------------------------------------------------------
$! WRAP_UP - exit the balancing with normal status
$!------------------------------------------------------------------------------
$!
$ WRAP_UP:
$    SET NOVERIFY
$!
$   IF EXIT_STAT .NES. ""
$   THEN
$       @COM:ACE_OPCOM_MSG.COM "CAP_BAL_BRM_90.COM"
$       EXIT DCL_ABORT
$   ENDIF
$!
$   W   "**********************************************************"
$   W   "*    The CAP_BAL_BRM_90 Completed Successfully"
$   W   "**********************************************************"
$!
$   @COM:TERMINATION
$   EXIT 1
$!
$!-----------------------------------------------------------------------------
$! RESET_MSG - Define SYS$OUTPUT to a temp file to capture program messages.
$!----------------------------------------------------------------------------- 
$!
$ RESET_MSG:
$!   
$   IF F$SEARCH("''MSGFILE'") .EQS. "" 
$   THEN 
$         GOTO SYSOUT
$   ENDIF
$   DELETE/LOG  'MSGFILE';*
$!
$ SYSOUT:
$   DEFINE/LOG SYS$OUTPUT  'MSGFILE'
$ RETURN
$!
$!
$!
$ BALANCE_MSG:
$!
$   SET NOVERIFY
$   SET NOON
$   ERROR_STATUS = '$STATUS'
$   WARN_MSG = "NO"
$   GOSUB RESET_MSG
$! 
$   W ""
$   W "Hello CAP Team and OPR Team, "
$   W ""
$   IF CAP_BAL_BRM_90_STATUS .EQS. "I"
$   THEN
$      W "   The CAP reports are in balance for this capitation period."
$! Mod #02 Start	
$   ELSE
$      IF CAP_BAL_BRM_90_STATUS .EQS. BAL_MORE_100
$      THEN
$         W "   The reports are not in balance for this capitation period."
$         W "   Monthend CAP job stream is being aborted."
$         W "   Take a look at the log for the out of balance amounts. "
$         EXIT_STAT :== "E"
$      ELSE
$	  W "   The reports are not in balance for this capitation period."
$	  W "	Monthend CAP Job stream is running as the oob is less than 100."
$	  W "   Take a look at the log for the out of balance values. "
$      ENDIF
$! Mod #02 End
$   ENDIF 
$   W F$FAO("!^")
$   DEASSIGN SYS$OUTPUT
$   IF F$SEARCH("''BAL90_FILE'") .NES. "" 
$   THEN 
$      APPEND/LOG 'BAL90_FILE' 'MSGFILE'
$   ENDIF
$!
$   DEASSIGN SYS$OUTPUT
$!
$   IF CAP_BAL_BRM_90_STATUS .EQS. "I"
$   THEN
$      HN_MAIL 'MSGFILE' 'DIST_LIST' "CAP_BAL_BRM_90: CAP IS IN BALANCE"
$   ELSE
$! Mod #02 Start
$      IF CAP_BAL_BRM_90_STATUS .EQS. BAL_MORE_100
$      THEN	
$         HN_MAIL 'MSGFILE' 'DIST_ERR' "CAP_BAL_BRM_90: CAP IS OUT OF BALANCE (program aborted)"
$         GOTO EXIT_STATUS
$      ELSE
$	  HN_MAIL 'MSGFILE' 'DIST_LIST' "CAP_BAL_BRM_90: CAP IS OUT OF BALANCE (program not aborted)"
$      ENDIF	
$! Mod #02 End
$   ENDIF 
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
$   ELSE
$       DIR_RESULT      = " - No log, mode is ''F$MODE()'"
$   ENDIF
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
$!   GOSUB BALANCE_MSG
$   SET NOON
$!
$   GOSUB PROCEDURE_NAME
$   W ""
$!
$ ERR_MSG_EXIT:
$   ERROR_STATUS = '$STATUS'
$   SET NOVERIFY
$   IF MSG .EQS. "BADP1"
$   THEN
$        W "''F$ENVIRONMENT("PROCEDURE")' was executed with"
$        W " an invalid P1 parameter of " + "''P1'" + "."
$        W " "
$   ENDIF
$!
$   IF MSG .EQS. "BADP2"
$   THEN
$       IF F$SEARCH(F$STRING(PARAM2)) .EQS. "" 
$       THEN 
$           W "Could not locate the file " + "''PARAM2'" + " specified as P2."
$           W "Please re-execute with a valid file as P2."
$       ELSE
$           W "Please check the syntax of each line in the file ''PARAM2'."
$           W "''PARAM2'" + " should only contain DEFINE and/or ASSIGN"         
$           W "logical statements.  Re-execute ''F$ENVIRONMENT("PROCEDURE")'"
$           W "when the file has been corrected."
$       ENDIF
$   ENDIF
$   W ""
$   W ""
$   W F$FAO                                             - 
        ( "Operator submitted CAP_BAL_BRM_90 !AS"       -
        + "!/Job failed at !%D "                        -
        , F$GETJPI( "", "LOGINTIM" )                    -
        , 0                                             -
        )
$   WARN_MSG = "NO"
$   GOSUB LOG_FILE_INFO
$   HN_MAIL 'MSGFILE' 'DIST_ERR' -
    "CAP_BAL_BRM_90: Error occurred in CAP_BAL_BRM_90.COM"
$!
$   IF ERROR_STATUS THEN ERROR_STATUS = 42 !Ensure that we exit with an error.
$   EXIT ERROR_STATUS
$!
$!
$ INFO_EXIT:
$! 
$   ERROR_STATUS = '$STATUS'
$   HN_MAIL 'MSGFILE' 'DIST_ERR' -
    "CAP_BAL_BRM_90: Warning(s) or errors occurred in CAP_BAL_BRM_90.COM"
$!
$ EXIT_STATUS:
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
$     LOG_FILE        = F$GETQUI("DISPLAY_JOB","LOG_SPECIFICATION",,"THIS_JOB")
$     PARSED_LOG_FILE = F$PARSE(LOG_FILE,PROCEDURE_NAME,,,"SYNTAX_ONLY")
$     DIR_RESULT      = F$SEARCH(PARSED_LOG_FILE)
$   ELSE
$     IF F$MODE() .EQS. "OTHER"
$     THEN
$         LOG_FILE        = NSCHED_OUTFILE
$         PARSED_LOG_FILE = F$PARSE(LOG_FILE,PROCEDURE_NAME,,,"SYNTAX_ONLY")
$         DIR_RESULT      = F$SEARCH(PARSED_LOG_FILE)
$     ELSE
$         DIR_RESULT  = " - No log, mode is ''F$MODE()'"
$     ENDIF
$   ENDIF
$!
$   IF WARN_MSG .EQS. "YES"
$   THEN
$      W F$FAO ("!/The log file is !AS" , DIR_RESULT)    
$   ELSE
$      W F$FAO ("!2(/)On step number !AS"           -
                + "!/Check the log file !AS"        -
                , F$STRING(SAVE_STEP_NUMBER)        -
                , DIR_RESULT)
$   ENDIF
$!
$!
$   W ""
$!
$   DEASSIGN SYS$OUTPUT
$!
$ RETURN
$!
