$!
$	ON ERROR THEN GOTO ERROR_CONDITION
$!
$!******************************************************************************
$! Initialization
$!******************************************************************************
$!
$ STEP_00:
$	STEP = 00
$!
$	DISPLAY = "WRITE SYS$OUTPUT"
$	JOB_NAME :== "CLM_TD_OUX_03_FTP"
$       RUN_STATUS = "INITIALIZATION"
$       USRNME = F$EDIT(F$GETJPI("","USERNAME"),"TRIM")
$       CSV_FILE1 = "''P1'"
$       CSV_FILE2 = "''P2'"
$       SET NOVERIFY
$       @COM:INITIALIZATION
$       @COM:CLM_EXT_SYM_01 "TABLES:CLM_TD_SYMBOLS.TXT" "CLM_TD_OUX_03"
$!
$       ENVIRONMENT = F$TRNLNM("ENVIRONMENT_ID")
$!
$       SET VERIFY
$!
$       PURGE/NOLOG SYS$LOGIN:CLM_TD_OUX_03_FTP.LOG
$ 	SHOW PROCESS/ACCOUNTING
$       TODAY_IS    = F$CVTIME("","COMPARISON") - "-" - "-" - ":"
$       RUN_DATE    = F$EXTRACT(0,8, TODAY_IS)
$       SHOW SYMBOL   RUN_DATE
$       MAIL       :== MAIL
$!
$       IF ENVIRONMENT .EQS. "PRD"
$       THEN
$           USER_LIST = "@CLM_TEAM,@OPR_TEAM"
$           FTP_PATH  = "''FTP_PRD_Folder1'"
$       ELSE
$           USER_LIST = F$GETJPI("","USERNAME")
$           FTP_PATH  = "''FTP_TST_Folder1'"
$       ENDIF
$!
$!******************************************************************************
$! Main Procedure
$!******************************************************************************
$!
$ STEP_10:
$!
$       RUN_STATUS = "STEP_10"
$       STEP = 10
$       DISPLAY "Step 10: Check/display CSV file in the CLM_RPT directory"
$ 	SHOW SYMBOL RUN_DATE
$!
$       IF F$SEARCH("''CSV_FILE1'") .EQS. ""
$       THEN
$          DISPLAY "''CSV_FILE1' file not found."
$          FTP_INPUT_FILE1 = ""
$       ELSE
$          FTP_INPUT_FILE1 = "''CSV_FILE1'"
$       ENDIF
$!
$       IF F$SEARCH("''CSV_FILE2'") .EQS. ""
$       THEN
$          DISPLAY "''CSV_FILE2' file not found."
$          FTP_INPUT_FILE2 = ""
$       ELSE
$          FTP_INPUT_FILE2 = "''CSV_FILE2'"
$       ENDIF
$!
$!******************************************************************************
$! Ftp file into a network folder #1
$!******************************************************************************
$!
$ STEP_20:
$!
$       RUN_STATUS = "STEP_20"
$       STEP = 20
$       DISPLAY "Step 20:  FTP csv file to users network folder #1"
$       !---------------------------------------------------------------------!
$       ! FTP the PA_LIST_usr_yyyymmddtttt.TXT file to the user-PC-directorys !
$       !---------------------------------------------------------------------!
$!
$       SHOW TIME
$       SHOW SYMBOL FTP_INPUT_FILE1
$       SHOW SYMBOL FTP_INPUT_FILE2
$       GOSUB SEND_CSV_REPORT
$!
$!******************************************************************************
$! Ftp file into a network folder #2
$!******************************************************************************
$!
$ STEP_30:
$!
$       RUN_STATUS = "STEP_30"
$       STEP = 30
$       DISPLAY "Step 30:  FTP csv file to users network folder #2"
$       !---------------------------------------------------------------------!
$       ! FTP the PA_LIST_usr_yyyymmddtttt.TXT file to the user-PC-directorys !
$       !---------------------------------------------------------------------!
$       SHOW TIME
$!
$       IF ENVIRONMENT .EQS. "PRD"
$       THEN
$           FTP_PATH  = "''FTP_PRD_Folder2'"
$       ELSE
$           FTP_PATH  = "''FTP_TST_Folder2'"
$       ENDIF
$!
$       SHOW TIME
$       SHOW SYMBOL FTP_INPUT_FILE1
$       SHOW SYMBOL FTP_INPUT_FILE2
$       GOSUB SEND_CSV_REPORT
$!
$!******************************************************************************
$ WRAP_UP:
$!
$      SET NOVERIFY
$      @COM:TERMINATION
$      SET VERIFY
$!
$      EXIT
$!
$!******************************************************************************
$ SEND_CSV_REPORT:
$!
$      SET NOON
$      FTP_STAT == 0
$      COM_FILE = "CLM_RPT:TEMP_CLM_TD_FTP_''USRNME'.COM"
$!   
$      ON WARNING THEN GOTO ERROR_CONDITION
$      OPEN/WRITE FTP_COM      'COM_FILE
$      WRITE FTP_COM "$    ON ERROR THEN GOTO FTP_ERROR_COND"
$      WRITE FTP_COM "$    SHOW TIME"
$      WRITE FTP_COM "$    ftp"
$      WRITE FTP_COM " exit-on-error on"
$      WRITE FTP_COM " verbose on"
$      WRITE FTP_COM " statistics on"
$      WRITE FTP_COM " passive off"
$      WRITE FTP_COM " connect ''FTP_Parm1' "
$      WRITE FTP_COM " login ''FTP_Parm2' "
$      WRITE FTP_COM " password ''FTP_Parm3' "
$      WRITE FTP_COM " cd ""''FTP_PATH'"""
$      IF FTP_INPUT_FILE1 .NES. ""
$      THEN
$         WRITE FTP_COM " put ''FTP_INPUT_FILE1'"
$      ENDIF
$      IF FTP_INPUT_FILE2 .NES. ""
$      THEN
$         WRITE FTP_COM " put ''FTP_INPUT_FILE2'"
$      ENDIF
$      WRITE FTP_COM " dir"
$      WRITE FTP_COM " exit"
$      WRITE FTP_COM "$    SHOW TIME"
$      WRITE FTP_COM "$    EXIT"
$      WRITE FTP_COM "$ FTP_ERROR_COND:"
$      WRITE FTP_COM "$    FTP_STAT == 1"
$      WRITE FTP_COM "$    ERROR_STATUS == $STATUS"
$      WRITE FTP_COM "$    EXIT ERROR_STATUS"
$      CLOSE FTP_COM
$!
$!     TYPE 'COM_FILE;
$      @'COM_FILE
$      DELETE/LOG 'COM_FILE;*
$      SET VERIFY
$!
$      IF FTP_STAT .NES. 0
$      THEN
$           ERR_MSG = " FTP of Prior Auth Report csv file Failed..."
$           GOTO FTP_ERROR
$      ENDIF
$!
$      RETURN
$!
$!******************************************************************************
$ FTP_ERROR:
$!
$      OPEN/WRITE    FTP_ERR    DAT:FTP_TXT.ERR
$      WRITE FTP_ERR " FTP to Send Prior Auth Extract Report has failed" + ''f$time()'
$      WRITE FTP_ERR " Please check CLM_TD_OUX_03_FTP.LOG for Errors "
$      WRITE FTP_ERR " Verify which files from this log need to be re-FTPed. "
$      WRITE FTP_ERR " ''ERR_MSG'"
$      CLOSE FTP_ERR
$!
$      MAIL/subject="FTP Send Prior Auth Extract Rpt Issue" DAT:ftp_txt.err "''USER_LIST'"
$!
$      DISPLAY  "Error sending Prior Auth Extract Report"
$      DISPLAY  "''ERR_MSG'"
$      DISPLAY  "Check Users SYS$LOGIN:CLM_TD_OUX_03_FTP.LOG"
$      DELETE/NOLOG/NOCONFIRM   DAT:FTP_TXT.ERR;*
$!
$      SET NOVERIFY
$      EXIT
$!
$!******************************************************************************
$ ERROR_CONDITION:
$!
$      ERROR_STATUS = $STATUS
$      DISPLAY "BATCH_ERROR - The following error was encountered :"
$      DISPLAY F$MESSAGE( ERROR_STATUS )
$!
$      EXIT
