$!-----------------------------------------------------------------------------!
$!  H E A L T H   N E T   -   Automated Business System.                       !
$!-----------------------------------------------------------------------------!
$!  This source code  module and all components referenced  herein, are the    !
$!  proprietary properties and trade secrets of HEALTH NET,INC .  Except as    !
$!  provided for by license agreement,  this source code module shall not be   !
$!  duplicated, used or disclosed without written consent, signed by an officer!
$!  of HEALTH NET,INC. 							       !	
$!-----------------------------------------------------------------------------!
$! Start Documentation:
$!
$!	SYSTEM:  	HN COMMERCIAL CAPITATION
$! 	JOB:    	CAP_EXT_BUD_99.COM
$! 	TITLE:   	AHCCCS 820 file load
$! 	DATE:    	20-JUN-2013
$! 	AUTHOR:  	MOHAMED HASHIM (H2K)
$!
$!     OVERVIEW:
$!		The command procedure will run daily and it will search in the
$!		directory SEM_HCFA_DATA for the AHCCCS 820 file. If a file exists
$!		which is not processed by this job then this job will load the
$!		values from the 820 file to the files CAP66_AHCCCS_MBR_PAYMNT_HIST
$!              and CAP67_AHCCCS_ORG_PAYMNT_HIST
$!
$!  Modification History:
$!  ============ ========
$!      MOD:   <DD-MMM-CCYY>    <USER NAME> <USER_ID>   <ISR# PID#>
$!
$!      #01     21-JUL-2015     Prabhakar Jayakumar (MW6)   INC0000188684
$!              Changed file name from AZW_820_ to AZD_820 
$!              Weekly file to Daily file.
$!
$!	00	20-JUN-2013	Mohamed Hashim (H2K)      PID 16230
$!              AZ Medicaid Capitation
$!		Initial Creation                   
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
$ ON ERROR THEN GOTO ERROR_CONDITION
$   SET VERIFY
$!
$   PV = F$ENVIRONMENT("VERIFY_PROCEDURE")
$!
$ INITIALIZATION:
$   ECHO := WRITE SYS$OUTPUT
$   THIS_COMFILE = F$ENVIRONMENT("PROCEDURE")
$   JOB_NAME	== F$PARSE(THIS_COMFILE,,,"NAME")
$   PID          = F$GETJPI("","PID")   ! Handy for temp filenames
$   MAIL_TEMP   := CAP_WRK1:'JOB_NAME'_'PID'_MAIL.TMP
$   WS_NULL        = ""
$   WS_RUNSTAT0  = "0"
$   WS_RUNSTAT1  = "1"
$   WS_RUNSTAT2  = "2"
$   WS_AMPM_IND = "AM"
$   WS_PYMNT_MED = "MED"
$   WS_PYMNT_DEN = "DEN"
$!
$   IF  P1 .EQS. WS_NULL
$   THEN
$	CALL WRITE_STATUS "^''JOB_NAME'"  "AHCCCS Payment Extract"
$   ELSE
$	CALL WRITE_STATUS "RESTARTED"
$   ENDIF
$!
$   RUN_STATUS   = WS_RUNSTAT0 
$   CR[0,8] = 13
$   LF[0,8] = 10
$   CRLF = CR + LF
$   ERR_MSG  = "** ERROR RUNNING THE " + JOB_NAME + " PROCEDURE **"
$   @COM:INITIALIZATION
$   ENVIRONMENT = F$TRNLNM("ENVIRONMENT_ID")
$   SHOW SYM ENVIRONMENT
$!
$       TIME = F$TIME()
$       TIME = F$CVTIME(TIME)
$       CCYY = F$EXTRACT(0,4,TIME)
$       MM   = F$EXTRACT(5,2,TIME)
$       DD   = F$EXTRACT(8,2,TIME)
$       CCYYMMDD = CCYY+MM+DD
$
$   SHOW SYM  TIME
$   SHOW SYM CCYYMMDD
$   DEFINE CAP66_AHCCCS_MBR_PAYMNT_HIST CAPDATA_DSK1:CAP66_AHCCCS_MBR_PAYMNT_HIST.ISM
$   DEFINE CAP67_AHCCCS_ORG_PAYMNT_HIST CAPDATA_DSK1:CAP67_AHCCCS_ORG_PAYMNT_HIST.ISM
$!
$   SET VERIFY
$!
$
$   IF  P1 .NES. WS_NULL
$   THEN
$       IF  P1 .EQS. WS_RUNSTAT0   THEN P1 = WS_RUNSTAT1
$       SEARCH/NOLOG 'THIS_COMFILE' "STEP_''P1':"
$       IF  $STATUS .NE. %X00000001
$       THEN            
$           ERROR_STATUS = $STATUS
$           ERR_MSG = ERR_MSG + CRLF + -
                "/PARAMETER=''P1' doesn't exist. Please re-submit at step" + -
                CRLF + "indicated in prior E-mail, not the one listed above."
$           GOTO ERROR_CONDITION
$       ENDIF
$       GOTO STEP_'P1'
$   ENDIF
$!	
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                          !!
$!! Step 1 -- Search the AHCCCS files				             !!
$!!                                                                          !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ STEP_1:
$   SET NOVERIFY
$   RUN_STATUS = WS_RUNSTAT1
$   SET  VERIFY
$   SHOW PROCESS/ACC
$!
$ SEARCH_FILES:
$! MOD #01 Begins
$   IF  F$SEARCH("SEM_HCFA_DATA:AZD_820__''CCYYMMDD*.IN;*",1) .NES. WS_NULL 
$! MOD #01 Ends
$   THEN 
$! MOD #01 Begins
$	AHCCCS_820_FILE =  F$SEARCH("SEM_HCFA_DATA:AZD_820__''CCYYMMDD*.IN;*")
$! MOD #01 Ends
$	AHCCCS_820_FILE =  "SEM_HCFA_DATA:" + F$EXTRACT(31,31,AHCCCS_820_FILE)
$	SH SYM AHCCCS_820_FILE
$	GOSUB LOAD_FILES
$	GOTO SEARCH_FILES
$   ELSE
$! MOD #01 Begins
$	WRITE SYS$OUTPUT " The file SEM_HCFA_DATA:AZD_820__''CCYYMMDD*.IN not found"	
$! MOD #01 Ends
$       GOTO WRAP_UP
$   ENDIF
$!	
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                           		                                     !!
$!!  MOD #01 Begins
$!! Step 2  Load SEM_HCFA_DATA:AZD_820__ccyymmdd*.in to CAP table !!
$!!  MOD #01 Ends
$!!                                                                          !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ STEP_2:
$ LOAD_FILES:
$   SET NOVERIFY
$   RUN_STATUS = WS_RUNSTAT2
$   CALL WRITE_STATUS "CAP_EXT_BUD_99"  "Load AHCCCS to CAP Database"
$   SET  VERIFY
$   SHOW PROCESS/ACC
$   SH SYM AHCCCS_820_FILE
$   DIR CAP_EXT_BUD_99.EXE;	
$   RUN EXE:CAP_EXT_BUD_99.EXE
$   SH SYM AHCCCS_820_FILE
$   IF AHCCCS_820_FILE .NES. WS_NULL
$   THEN	
$        RENAME/LOG  'AHCCCS_820_FILE *.DONE
$   ENDIF
$   RETURN
$!	
$!	
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                          !!
$!! WRAP_UP -- End of AHCCCS loading  process                                  !!
$!!                                                                          !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ WRAP_UP:
$   SHOW PROCESS/ACC
$   SET  NOVERIFY
$   @COM:TERMINATION
$   CALL WRITE_STATUS "COMPLETED"
$   IF  PV .EQS. "TRUE" THEN SET VERIFY
$   EXIT 1
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                !!
$!! ERROR_CONDITION -- Error Handling                              !!
$!!                                                                !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$! Error_condition - Abort and Report error. For the third parameter, either
$!   type the Username or the mail distribution list (with a @).
$!
$!
$ ERROR_CONDITION:
$   IF  F$TYPE(ERROR_STATUS) .EQS. WS_NULL   THEN ERROR_STATUS = $STATUS
$   TEMP = F$INTEGER(ERROR_STATUS)
$   TEMP[0,3] = 4				    ! Raise severity to "F"
$   ERROR_STATUS = "%X" + F$FAO("!XL", TEMP)
$   SET  NOVERIFY
$   CALL WRITE_STATUS "ABENDED"
$   MAIL_LST = "@OPR_TEAM, @ACE_TEAM"
$   ECHO ERR_MSG
$   @COM:BATCH_ERROR 'ERROR_STATUS' 'RUN_STATUS' "''MAIL_LST'" "''ERR_MSG'"
$   IF  PV THEN SET VERIFY
$   EXIT  ERROR_STATUS
$!
$!
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                !!
$!! WRITE_STATUS -- Display status for status reporting.           !!
$!!                                                                !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ WRITE_STATUS:	    SUBROUTINE
$   SET NOON
$   AM_PM_IND = WS_AMPM_IND
$   DAY  = F$EXTRACT(0, 3, F$EDIT(F$CVTIME(,,"WEEKDAY"), "UPCASE"))
$   TIME = F$EXTRACT(0, 5, F$CVTIME(,,"TIME"))
$   IF  F$EXTRACT(0, 2, TIME) .GTS. "11" - 
	THEN AM_PM_IND = "PM"
$   ECHO "^^", P1, "\", P2, "\", DAY, " ", TIME, AM_PM_IND
$   SET ON
$   EXIT 1
$ ENDSUBROUTINE
$!
$!-------------------------------------------------------------------------
$!			End of CAP_EXT_BUD_99.COM 
$!-------------------------------------------------------------------------
