$!-----------------------------------------------------------------------------!
$!  H E A L T H   N E T   -   Automated Business System.                       !
$!-----------------------------------------------------------------------------!
$! This source code, and all the routines referenced herein, are               !
$! the proprietary properties and trade secrets of HEALTH NET,INC.             !
$! Except as provided for by license agreement, this source code               !
$! shall not  be  duplicated, used or disclosed without written                !
$! consent, signed by an officer of HEALTH NET,INC.                            !
$!-----------------------------------------------------------------------------!
$!
$! Start Documentation
$!      SYSTEM:         Capitation Administration System (CAS)
$!      JOB:            CAP_EXT_BUM_01.COM
$!      TITLE:          UDMS claim reports extraction
$!      DATE:           30-Jan-2003
$!      AUTHOR:         Ken Cheng
$!
$!      OVERVIEW:               REVISED : {DD-MMM-CCYY} BY {User_ID}
$!
$!          This procedure extracts HMO, POS and CSP claim reports 
$!          from UDMS; FTP the reports to CAP NT server 
$!          \\WHSRVSHR004\capdata\cap2000\download directory.
$!
$!
$!      MOD#     DD-MMM-YYYY      USERNAME(VMSID)       ISR #      DESCRIPTION
$!     
$!      #1      07-Feb-2003                                     C6X
$!              Replace logical name ACECOM: to COM:.
$!      #2      10-Feb-2003                                     C6X
$!              Replace incorrect century parameter.
$!      #3      17-Feb-2003                                     C6X
$!              Replace incorrect month date parameter was used.
$!      #4      28-Feb-2003                                     C6X
$!              Insert logic to purge and rename report before
$!              submit UDMS queries.
$!              Replace FTP server from \\WHSRVSHR004\CAPDATA to 
$!              \\wh-fsvs03\data2\common\capdata.
$!	#5	Jan-14-2004
$!		Execute CAPREFCOP_01 to allow this command to reference 
$!		CAP variables.					C6X
$!	#6	Feb-22-2004
$!		Roll back change in step #5 due to enviroment cange.
$!								C6X
$!      #7      Apr-04-2005
$!              Add new target for FTP:RC-app27.
$!                                                              DDB
$!
$!      #8      March, 15, 2006					SSJ
$!		Call CAP_UTL_BRX_06 to get FTP information.
$!              Deleted the lines that had the FTP information.
$!
$!      #09     14-OCT-2014       GAYATHRI K KRISHNAMURTHY(K8D)    14941
$!              14941-17152 SHP Enhancements to ABS 
$!              Modified program to handle medi-cal data. Claims input file
$!              will be split into commercial and medi-cal extracts and FTPed
$!              to their respective HNCAS folders.
$!              step 3 is moved to step 4, now file split will be done in step 3.
$!              
$! End Documentation
$!
$!
$ ON ERROR THEN GOTO MAIL_ACETEAM
$! ON ERROR THEN GOTO ERROR_CONDITION
$ INITIALIZATION:
$!Mod #5 Run CAPREFCOP_01 to set CAP variables
$!Mod #6   CAPREFCOP_01
$!Mod #4 Added HN_MAIL
$   HN_MAIL     := $EXE:ACE_MAI_ORX_01
$   RUN_STATUS  = 0
$   THIS_COM    = F$ENVIRONMENT("PROCEDURE")
$   JOB_NAME    = F$PARSE(THIS_COM,,,"NAME")    !Get this procedure's name
$   CR[0,8]     = 13                            !Carriage return char
$   LF[0,8]     = 10                            !Line feed char
$   FTP_INFO_ERR= "N"
$   FTP_ERR1	= "N/A"
$   FTP_ERR2	= "N/A"
$! MOD #09 BEG
$   FTP_ERR3    = "N/A"
$   FTP_ERR4    = "N/A"
$   WS_NA       = "N/A"
$! MOD #09 END
$   ERR_MSG     = "ERROR RUNNING THE " + JOB_NAME + " PROCEDURE. " - 
                  + "Please contact UDMS support."
$!Mod #4 Replace Batch_Error with HN_Mail when encounter error.
$!   MAIL_LST    = "@ACE_TEAM"
$! MOD #09 BEG
$ TABLE_ID    = "RMC_PROD_DES"
$ WS_Y        = "Y"
$ WS_N        = "N"
$ WS_SPACE    = ""
$! MOD #09 END
$   @COM:INITIALIZATION
$!
$!------------------------------------------------------------------------------
$!  Display the process's PID, name, and other information that may be of some
$!  help if the job fails. 
$!------------------------------------------------------------------------------
$ SAVE_CURRENT_VERIFY = F$VERIFY(0)
$ SET NOVERIFY
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "           NODENAME=''F$GETJPI("","NODENAME")'"
$ WRITE SYS$OUTPUT "                PID=''F$GETJPI("","PID")'"
$ WRITE SYS$OUTPUT "       Process Name=''F$GETJPI("","PRCNAM")'"
$ WRITE SYS$OUTPUT "  Username(account)=''F$GETJPI("","USERNAME")'"
$ WRITE SYS$OUTPUT "     Procedure name=''F$ENVIRONMENT("PROCEDURE")'"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "                 P1=''p1'   <- Restart Step"
$ WRITE SYS$OUTPUT ""
$ IF SAVE_CURRENT_VERIFY .EQ. 1 THEN SET VERIFY
$!
$   SET VERIFY
$! BEG MOD #8 SSJ
$   IF  P1 .NES. "" .AND. P1 .NES. "0"  ! Check restart step
$   THEN
$       SEARCH/NOLOG 'THIS_COM' "STEP_''P1':"   ! Verify restart param
$       IF  $STATUS .NE. %X00000001
$       THEN
$	    P1 = "0" ! if the step number is not found, then start from STEP_0
$       ENDIF
$       GOTO STEP_'P1'
$   ENDIF
$! END MOD #8 SSJ
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                               !!
$!! Step 0 - Purge/Rename HMO, POS and CSP file.                  !!
$!!                                                               !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!Mod #4 start
$ STEP_0:
$  IF F$SEARCH("UDMS$RPT:HMO_CLMS.EXP") .eqs. "" THEN GOTO EndPurgeHMO
$       PURGE UDMS$RPT:HMO_CLMS.EXP
$       REN UDMS$RPT:HMO_CLMS.EXP UDMS$RPT:HMO_CLMS_BAK.EXP
$       PURGE UDMS$RPT:HMO_CLMS_BAK.EXP/keep=3
$  EndPurgeHMO:
$!
$  IF F$SEARCH("UDMS$RPT:POS_CLMS.EXP") .eqs. "" THEN GOTO EndPurgePOS
$       PURGE UDMS$RPT:POS_CLMS.EXP
$       REN UDMS$RPT:POS_CLMS.EXP UDMS$RPT:POS_CLMS_BAK.EXP
$       PURGE UDMS$RPT:POS_CLMS_BAK.EXP/keep=3
$  EndPurgePOS:
$!
$  IF F$SEARCH("UDMS$RPT:CSP_CLMS.EXP") .eqs. "" THEN GOTO EndPurgeCSP
$       PURGE UDMS$RPT:CSP_CLMS.EXP
$       REN UDMS$RPT:CSP_CLMS.EXP UDMS$RPT:CSP_CLMS_BAK.EXP
$       PURGE UDMS$RPT:CSP_CLMS_BAK.EXP/keep=3
$  EndPurgeCSP:
$!Mod #4 end
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                               !!
$!! Step 1 - Extract century, year and month from ACE report date.!!
$!!                                                               !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_1:
$!
$! Obtaining Century, Year and Month from system.
$!--------------------------------------------------- 
$   @COM:SYS_SCH_BRU_01 ace_rpt_eff
$!   Century =F$EXTRACT(0,2,PROG_DATE_YYYYMMDD)
$!   Year = F$EXTRACT(2,2,PROG_DATE_YYYYMMDD)
$!   Month= F$EXTRACT(4,2,PROG_DATE_YYYYMMDD)
$! Mod #3 start
$   Century =F$EXTRACT(0,2,PROG_DATE_EPM_YYYYMMDD)
$   Year = F$EXTRACT(2,2,PROG_DATE_EPM_YYYYMMDD)
$   Month= F$EXTRACT(4,2,PROG_DATE_EPM_YYYYMMDD)
$! Mod #3 end
$   sh sym Century
$   sh sym year
$   sh sym month
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                  !!
$!! Step 2 - USe date parameters from step 1 to build UDMS batch     !!
$!!          command files. Execute batch commands to create reports.!!
$!!                                                                  !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_2:
$   !---------------------------------------------------
$   !HMO_CLMS_NEW
$   !---------------------------------------------------
$! MOD #09 BEG
$   Open/Write O_F ACEWORK:UDMS_Batch_HMO.com
$   write O_F "$UDMS/BATCH/EXIT EXP CLHDSVCPRVGRP6 SELECT HMO_CLMS_NEW GENERATE"
$   write O_F "$ DECK"
$   write O_F Century
$   write O_F Year
$   write O_F Month
$   write O_F "$ EOD"
$   write O_F "$ EXIT"
$   Close O_F
$   TYPE ACEWORK:UDMS_Batch_HMO.com
$   SUBMIT/que=UDMS$BATCH/name=EXP_HMO ACEWORK:UDMS_Batch_HMO.com
$   !---------------------------------------------------
$   !POS_CLMS_NEW
$   !---------------------------------------------------
$   Open/Write O_F ACEWORK:UDMS_Batch_POS.com
$   write O_F "$UDMS/BATCH/EXIT EXP POSHDRSVCPRV6 SELECT POS_CLMS_NEW GENERATE" 
$   write O_F "$ DECK"
$   write O_F Century
$   write O_F Year
$   write O_F Month
$   write O_F "$ EOD"
$   write O_F "$ EXIT"
$   Close O_F
$   TYPE ACEWORK:UDMS_Batch_POS.com
$   SUBMIT/que=UDMS$BATCH/name=EXP_POS ACEWORK:UDMS_Batch_POS.com
$   !---------------------------------------------------
$   !CSP_CLMS_NEW
$   !---------------------------------------------------
$   Open/Write O_F ACEWORK:UDMS_Batch_CSP.com
$   write O_F "$UDMS/BATCH/EXIT EXP CSP_HDSVPRGRP6 SELECT CSP_CLMS_NEW GENERATE"
$   write O_F "$ DECK"
$   write O_F Century
$   write O_F Year
$   write O_F Month
$   write O_F "$ EOD"
$   write O_F "$ EXIT"
$   Close O_F
$   TYPE ACEWORK:UDMS_Batch_CSP.com
$   SUBMIT/que=UDMS$BATCH/name=EXP_CSP ACEWORK:UDMS_Batch_CSP.com
$! MOD #09 END
$
$   SET NOON
$   SYNCHRONIZE/QUEUE=UDMS$BATCH EXP_HMO
$   SYNCHRONIZE/QUEUE=UDMS$BATCH EXP_POS
$   SYNCHRONIZE/QUEUE=UDMS$BATCH EXP_CSP
$   SET ON
$!
$! MOD #09 BEG
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!        
$!! STEP 3 - SPLIT INPUT FILE INTO COMMERCIAL AND MEDI-CAL             !!
$!!          EXTRACTS.                                                 !!
$!!                                                                    !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ STEP_3:
$!
$ DEFINE CLMS_HMO_INPUT_FL  UDMS$RPT:HMO_CLMS.EXP
$ DEFINE CLMS_POS_INPUT_FL  UDMS$RPT:POS_CLMS.EXP
$ DEFINE CLMS_CSP_INPUT_FL  UDMS$RPT:CSP_CLMS.EXP
$ DEFINE TABLES_MASTER CAP01_TABLES_MASTER
$   DEFINE CLMSINHMO CAPWORK:CLMS_IN_HMO.EXP
$   DEFINE CLMSINPOS CAPWORK:CLMS_IN_POS.EXP
$   DEFINE CLMSINCSP CAPWORK:CLMS_IN_CSP.EXP
$!
$!
$ CREATE CAPWORK:CAP_COM_CLMS_HMO.EXP
$ CREATE MCDWORK:CAP_MED_CLMS_HMO.EXP
$ CREATE CAPWORK:CAP_COM_CLMS_POS.EXP
$ CREATE MCDWORK:CAP_MED_CLMS_POS.EXP
$ CREATE CAPWORK:CAP_COM_CLMS_CSP.EXP
$ CREATE MCDWORK:CAP_MED_CLMS_CSP.EXP
$!
$ DEFINE CAP_COMMERCIAL_CLMS_HMO CAPWORK:CAP_COM_CLMS_HMO.EXP
$ DEFINE CAP_MEDICAL_CLMS_HMO    MCDWORK:CAP_MED_CLMS_HMO.EXP
$ DEFINE CAP_COMMERCIAL_CLMS_POS CAPWORK:CAP_COM_CLMS_POS.EXP
$ DEFINE CAP_MEDICAL_CLMS_POS    MCDWORK:CAP_MED_CLMS_POS.EXP
$ DEFINE CAP_COMMERCIAL_CLMS_CSP CAPWORK:CAP_COM_CLMS_CSP.EXP
$ DEFINE CAP_MEDICAL_CLMS_CSP    MCDWORK:CAP_MED_CLMS_CSP.EXP
$!
$ OPEN_TABLES_MASTER:
$   OPEN/READ/SHARED TABLES_MASTER_FILE TABLES_MASTER
$   READ /INDEX=0 /END_OF_FILE=EXIT_TABLES_MASTER /ERROR=TABLE_ERROR -
         /TIME_OUT=0 /MATCH=EQ -
         /KEY= 'TABLE_ID       -
         TABLES_MASTER_FILE TABLE_REC
$
$ SET VERIFY
$ READ_AGAIN:
$
$ READ /END = EXIT_TABLES_MASTER TABLES_MASTER_FILE TABLE_REC
$ CHECK_TABLE_REC:
$   IF TABLE_ID .NES. F$EXTRACT (0,12,TABLE_REC)
$   THEN
$       GOTO EXIT_TABLES_MASTER
$   ENDIF
$   CMR_FL = F$EXTRACT (25,1,TABLE_REC)
$   MCR_FL = F$EXTRACT (27,1,TABLE_REC)
$   CAP_FL = F$EXTRACT (30,1,TABLE_REC)
$ IF (MCR_FL .EQS. WS_Y) .AND. (CAP_FL .EQS. WS_Y) .AND. (CMR_FL .EQS. WS_N)
$ THEN
$   OPEN/WRITE  CAP_LOOKUP_CLMS CAPWORK:CAP_CLMS_LOOKUP.EXP
$   WRITE  CAP_LOOKUP_CLMS    TABLE_REC
$ ENDIF
$ GOTO READ_AGAIN
$!
$ EXIT_TABLES_MASTER:
$!
$    CLOSE TABLES_MASTER_FILE
$!
$ DEFINE CAP_LOOKUP_CLMS_FILE         CAPWORK:CAP_CLMS_LOOKUP.EXP
$! extract commercial and medi-cal claims from clms_hmo input file
$  CREATE CLMSINHMO:
$   DECK
SCRIPT VERSION 001
CLMS_HMO_INPUT_FL
CAP_MEDICAL_CLMS_HMO
CAP_COMMERCIAL_CLMS_HMO
LI
32
CAP_LOOKUP_CLMS_FILE
13
2
S

E
$   EOD
$   DEFINE COB$INPUT CLMSINHMO:
$    CLOSE CAP_LOOKUP_CLMS
$   RUN/NODEB EXE:CAP_UTL_OUX_01
$   LOGDIR CAP_MEDICAL_CLMS_HMO
$   LOGDIR CAP_COMMERCIAL_CLMS_HMO
$!
$! extract commercial and medi-cal claims from clms_pos input file
$  CREATE CLMSINPOS:
$   DECK
SCRIPT VERSION 001
CLMS_POS_INPUT_FL  
CAP_MEDICAL_CLMS_POS    
CAP_COMMERCIAL_CLMS_POS
LI
32
CAP_LOOKUP_CLMS_FILE
13
2
S

E
$   EOD
$   DEFINE COB$INPUT CLMSINPOS:
$   RUN/NODEB EXE:CAP_UTL_OUX_01
$   LOGDIR CAP_MEDICAL_CLMS_POS    
$   LOGDIR CAP_COMMERCIAL_CLMS_POS
$!
$! extract commercial and medi-cal claims from clms_csp input file
$  CREATE CLMSINCSP:
$   DECK
SCRIPT VERSION 001
CLMS_CSP_INPUT_FL  
CAP_MEDICAL_CLMS_CSP    
CAP_COMMERCIAL_CLMS_CSP
LI
32
CAP_LOOKUP_CLMS_FILE
13
2
S

E
$   EOD
$   DEFINE COB$INPUT CLMSINCSP:
$   RUN/NODEB EXE:CAP_UTL_OUX_01
$   LOGDIR CAP_MEDICAL_CLMS_CSP    
$   LOGDIR CAP_COMMERCIAL_CLMS_CSP
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!                                                                    !!
$!! STEP 4 - FTP HMO, POS AND CSP REPORTS TO NT SERVER.                !!
$!!          CLEAN UP TEMPORARY WORKING FILES.                         !! 
$!!                                                                    !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$!
$ STEP_4:
$!
$! MOD #09 END
$! Beg Mod #8 SSJ 3-15-2006
$!
$! 3:Rc-app27, HNCAS server, as of 3-15-2006
$!
$   @COM:CAP_UTL_BRX_06 3
$!
$   IF	FTP_SERVER_NAME  .EQS. "" .OR. FTP_ACCOUNT_NAME .EQS. ""
$   THEN
$	WRITE SYS$OUTPUT "FTP_SERVER_NAME = ", FTP_SERVER_NAME
$	WRITE SYS$OUTPUT "FTP_ACCOUNT_NAME= ", FTP_ACCOUNT_NAME
$	WRITE SYS$OUTPUT "FTP_PASSWORD    = ", FTP_PASSWORD
$	WRITE SYS$OUTPUT "FTP_TOPFOLDER   = ", FTP_TOPFOLDER
$	FTP_INFO_ERR = "Y"
$	FTP_ERR1 = "  Failed to find HNCAS server FTP Information."
$   ELSE
$	HNCAS_SERVER    = FTP_SERVER_NAME
$	HNCAS_ACCOUNT   = FTP_ACCOUNT_NAME
$	HNCAS_PASSWORD  = FTP_PASSWORD
$	HNCAS_TOPFOLDER = FTP_TOPFOLDER
$   ENDIF
$! MOD #09 BEG
$ DEFINE CAPDATA_DSK1 MCDDATA_DSK1
$   @COM:CAP_UTL_BRX_06 3
$!
$   IF  FTP_SERVER_NAME  .EQS. "" .OR. FTP_ACCOUNT_NAME .EQS. ""
$   THEN
$       WRITE SYS$OUTPUT "FTP_SERVER_NAME = ", FTP_SERVER_NAME
$       WRITE SYS$OUTPUT "FTP_ACCOUNT_NAME= ", FTP_ACCOUNT_NAME
$       WRITE SYS$OUTPUT "FTP_PASSWORD    = ", FTP_PASSWORD
$       WRITE SYS$OUTPUT "FTP_TOPFOLDER   = ", FTP_TOPFOLDER
$       FTP_INFO_ERR = "Y"
$       FTP_ERR3 = "  Failed to find HNCAS server FTP Information for Medi-cal."
$   ELSE
$       HNCAS_MCD_SERVER    = FTP_SERVER_NAME
$       HNCAS_MCD_ACCOUNT   = FTP_ACCOUNT_NAME
$       HNCAS_MCD_PASSWORD  = FTP_PASSWORD
$       HNCAS_MCD_TOPFOLDER = FTP_TOPFOLDER
$   ENDIF
$   DEASSIGN CAPDATA_DSK1
$! MOD #09 END
$!
$! 4:WH-FTP01, user data server for CAP2000, as of 3-15-2006
$!
$   @COM:CAP_UTL_BRX_06 4
$!
$   IF	FTP_SERVER_NAME  .EQS. "" .OR. FTP_ACCOUNT_NAME .EQS. ""
$   THEN
$	WRITE SYS$OUTPUT "FTP_SERVER_NAME = ", FTP_SERVER_NAME
$	WRITE SYS$OUTPUT "FTP_ACCOUNT_NAME= ", FTP_ACCOUNT_NAME
$	WRITE SYS$OUTPUT "FTP_PASSWORD    = ", FTP_PASSWORD
$	WRITE SYS$OUTPUT "FTP_TOPFOLDER   = ", FTP_TOPFOLDER
$	FTP_INFO_ERR = "Y"
$	FTP_ERR2 = "  Failed to find CAP2000 user server FTP Information."
$   ELSE
$	USR_SERVER	= FTP_SERVER_NAME
$	USR_ACCOUNT	= FTP_ACCOUNT_NAME
$	USR_PASSWORD    = FTP_PASSWORD
$	USR_TOPFOLDER   = FTP_TOPFOLDER
$   ENDIF
$	
$! MOD #09 BEG
$ DEFINE CAPDATA_DSK1 MCDDATA_DSK1
$!
$   @COM:CAP_UTL_BRX_06 4
$!
$   IF  FTP_SERVER_NAME  .EQS. "" .OR. FTP_ACCOUNT_NAME .EQS. ""
$   THEN
$       WRITE SYS$OUTPUT "FTP_SERVER_NAME = ", FTP_SERVER_NAME
$       WRITE SYS$OUTPUT "FTP_ACCOUNT_NAME= ", FTP_ACCOUNT_NAME
$       WRITE SYS$OUTPUT "FTP_PASSWORD    = ", FTP_PASSWORD
$       WRITE SYS$OUTPUT "FTP_TOPFOLDER   = ", FTP_TOPFOLDER
$       FTP_INFO_ERR = "Y"
$       FTP_ERR4 = "  Failed to find CAP2000 user server FTP Information for Medi-cal."
$   ELSE
$       USR_MCD_SERVER      = FTP_SERVER_NAME
$       USR_MCD_ACCOUNT     = FTP_ACCOUNT_NAME
$       USR_MCD_PASSWORD    = FTP_PASSWORD
$       USR_MCD_TOPFOLDER   = FTP_TOPFOLDER
$   ENDIF
$   DEASSIGN CAPDATA_DSK1
$! MOD #09 END
$   IF  FTP_INFO_ERR THEN GOTO MAIL_ACETEAM_FTP_INFO_ERROR
$!
$! MOD #09 BEG
$   FILE_NAME_1 = "CAPWORK:CAP_COM_CLMS_HMO.EXP"
$   FILE_NAME_2 = "MCDWORK:CAP_MED_CLMS_HMO.EXP"
$  IF (F$SEARCH(FILE_NAME_1) .EQS. "") .AND. (F$SEARCH(FILE_NAME_2) .EQS. "")
$! MOD #09 END
$   THEN
$	GOSUB MAIL_ACETEAM_ONLY
$	GOTO END_FTP_HMO
$   ENDIF
$
$! MOD #09 BEG
$   DIR CAP_COMMERCIAL_CLMS_HMO
$   DIR CAP_MEDICAL_CLMS_HMO
$! MOD #09 END
$
$   OPEN/WRITE Out_FTP CAPWORK:CAP_EXT_BUM_01_USR.FTP
$   Write Out_FTP "$ FTP ''USR_SERVER'"
$   Write Out_FTP "LOGIN ''USR_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''USR_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   Write Out_FTP "cd ''USR_TOPFOLDER' "
$! MOD #09 BEG
$   WRITE OUT_FTP "PUT CAPWORK:CAP_COM_CLMS_HMO.EXP;  HMO_CLMS.EXP"
$! MOD #09 END
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE CAPWORK:CAP_EXT_BUM_01_USR.FTP;
$   @CAPWORK:CAP_EXT_BUM_01_USR.FTP
$   DELETE/NOCONFIRM/LOG CAPWORK:CAP_EXT_BUM_01_USR.FTP;
$!
$   OPEN/WRITE Out_FTP CAPWORK:CAP_EXT_BUM_01_CAS.FTP;
$   Write Out_FTP "$ FTP ''HNCAS_SERVER'"
$   Write Out_FTP "LOGIN ''HNCAS_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''HNCAS_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   Write Out_FTP "cd ''HNCAS_TOPFOLDER' "
$! MOD #09 BEG
$   WRITE OUT_FTP "PUT CAPWORK:CAP_COM_CLMS_HMO.EXP;  HMO_CLMS.EXP"
$! MOD #09 END
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE CAPWORK:CAP_EXT_BUM_01_CAS.FTP;
$   @CAPWORK:CAP_EXT_BUM_01_CAS.FTP
$   IF .NOT. $STATUS THEN GOSUB MAIL_ACETEAM_ONLY
$   DELETE/NOCONFIRM/LOG CAPWORK:CAP_EXT_BUM_01_CAS.FTP;
$!
$! MOD #09 BEG
$   OPEN/WRITE Out_FTP MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP;
$   Write Out_FTP "$ FTP ''HNCAS_MCD_SERVER'"
$   Write Out_FTP "LOGIN ''HNCAS_MCD_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''HNCAS_MCD_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   WRITE OUT_FTP "CD ''HNCAS_MCD_TOPFOLDER' "
$   WRITE OUT_FTP "PUT MCDWORK:CAP_MED_CLMS_HMO.EXP;   HMO_CLMS.EXP"
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP;
$   @MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP
$   IF .NOT. $STATUS THEN GOSUB MAIL_ACETEAM_ONLY
$   DELETE/NOCONFIRM/LOG MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP;
$!
$   OPEN/WRITE Out_FTP MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP;
$   Write Out_FTP "$ FTP ''USR_MCD_SERVER'"
$   Write Out_FTP "LOGIN ''USR_MCD_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''USR_MCD_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   Write Out_FTP "cd ''USR_MCD_TOPFOLDER' "
$   WRITE OUT_FTP "PUT MCDWORK:CAP_MED_CLMS_HMO.EXP;   HMO_CLMS.EXP"
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP;
$   @MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP
$   IF .NOT. $STATUS THEN GOSUB MAIL_ACETEAM_ONLY
$   DELETE/NOCONFIRM/LOG MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP;
$! MOD #09 END
$ END_FTP_HMO:
$!
$! MOD #09 BEG
$   FILE_NAME_1 = "CAPWORK:CAP_COM_CLMS_POS.EXP"
$   FILE_NAME_2 = "MCDWORK:CAP_MED_CLMS_POS.EXP"
$  IF (F$SEARCH(FILE_NAME_1) .EQS. "") .AND. (F$SEARCH(FILE_NAME_2) .EQS. "")
$! MOD #09 END
$   THEN
$	GOSUB MAIL_ACETEAM_ONLY
$	GOTO END_FTP_POS
$   ENDIF
$! MOD #09 BEG
$   DIR CAP_COMMERCIAL_CLMS_POS
$   DIR CAP_MEDICAL_CLMS_POS
$! MOD #09 END
$   OPEN/WRITE Out_FTP CAPWORK:CAP_EXT_BUM_01_USR.FTP
$   Write Out_FTP "$ FTP ''USR_SERVER'"
$   Write Out_FTP "LOGIN ''USR_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''USR_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   Write Out_FTP "cd ''USR_TOPFOLDER' "
$! MOD #09 BEG
$   WRITE OUT_FTP "PUT CAPWORK:CAP_COM_CLMS_POS.EXP;  POS_CLMS.EXP"
$! MOD #09 END
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE CAPWORK:CAP_EXT_BUM_01_USR.FTP;
$   @CAPWORK:CAP_EXT_BUM_01_USR.FTP
$   DELETE/NOCONFIRM/LOG CAPWORK:CAP_EXT_BUM_01_USR.FTP;
$!
$   OPEN/WRITE Out_FTP CAPWORK:CAP_EXT_BUM_01_CAS.FTP;
$   Write Out_FTP "$ FTP ''HNCAS_SERVER'"
$   Write Out_FTP "LOGIN ''HNCAS_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''HNCAS_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   Write Out_FTP "cd ''HNCAS_TOPFOLDER' "
$! MOD #09 BEG
$   WRITE OUT_FTP "PUT CAPWORK:CAP_COM_CLMS_POS.EXP;  POS_CLMS.EXP"
$! MOD #09 END
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE CAPWORK:CAP_EXT_BUM_01_CAS.FTP;
$   @CAPWORK:CAP_EXT_BUM_01_CAS.FTP
$   IF .NOT. $STATUS THEN GOSUB MAIL_ACETEAM_ONLY
$   DELETE/NOCONFIRM/LOG CAPWORK:CAP_EXT_BUM_01_CAS.FTP;
$!
$! MOD #09 BEG
$   OPEN/WRITE Out_FTP MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP;
$   Write Out_FTP "$ FTP ''HNCAS_MCD_SERVER'"
$   Write Out_FTP "LOGIN ''HNCAS_MCD_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''HNCAS_MCD_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   WRITE OUT_FTP "CD ''HNCAS_MCD_TOPFOLDER' "
$   WRITE OUT_FTP "PUT MCDWORK:CAP_MED_CLMS_POS.EXP;  POS_CLMS.EXP"
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP;
$   @MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP
$   IF .NOT. $STATUS THEN GOSUB MAIL_ACETEAM_ONLY
$   DELETE/NOCONFIRM/LOG MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP;
$!
$   OPEN/WRITE Out_FTP MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP;
$   Write Out_FTP "$ FTP ''USR_MCD_SERVER'"
$   Write Out_FTP "LOGIN ''USR_MCD_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''USR_MCD_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   Write Out_FTP "cd ''USR_MCD_TOPFOLDER' "
$   WRITE OUT_FTP "PUT MCDWORK:CAP_MED_CLMS_POS.EXP;  POS_CLMS.EXP"
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP;
$   @MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP
$   IF .NOT. $STATUS THEN GOSUB MAIL_ACETEAM_ONLY
$   DELETE/NOCONFIRM/LOG MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP;
$! MOD #09 END
$!
$ END_FTP_POS:
$!
$! MOD #09 BEG
$   FILE_NAME_1 = "CAPWORK:CAP_COM_CLMS_CSP.EXP"
$   FILE_NAME_2 = "MCDWORK:CAP_MED_CLMS_CSP.EXP"
$  IF (F$SEARCH(FILE_NAME_1) .EQS. "") .AND. (F$SEARCH(FILE_NAME_2) .EQS. "")
$! MOD #09 END
$   THEN
$	GOSUB MAIL_ACETEAM_ONLY
$	GOTO END_FTP_CSP
$   ENDIF
$! MOD #09 BEG
$   DIR CAP_COMMERCIAL_CLMS_CSP
$   DIR CAP_MEDICAL_CLMS_CSP
$! MOD #09 END
$   OPEN/WRITE Out_FTP CAPWORK:CAP_EXT_BUM_01_USR.FTP
$   Write Out_FTP "$ FTP ''USR_SERVER'"
$   Write Out_FTP "LOGIN ''USR_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''USR_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   Write Out_FTP "cd ''USR_TOPFOLDER' "
$! MOD #09 BEG
$   WRITE OUT_FTP "PUT CAPWORK:CAP_COM_CLMS_CSP.EXP;  CSP_CLMS.EXP"
$! MOD #09 END
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE CAPWORK:CAP_EXT_BUM_01_USR.FTP;
$   @CAPWORK:CAP_EXT_BUM_01_USR.FTP
$   DELETE/NOCONFIRM/LOG CAPWORK:CAP_EXT_BUM_01_USR.FTP;
$!
$   OPEN/WRITE Out_FTP CAPWORK:CAP_EXT_BUM_01_CAS.FTP;
$   Write Out_FTP "$ FTP ''HNCAS_SERVER'"
$   Write Out_FTP "LOGIN ''HNCAS_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''HNCAS_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   Write Out_FTP "cd ''HNCAS_TOPFOLDER' "
$! MOD #09 BEG
$   WRITE OUT_FTP "PUT CAPWORK:CAP_COM_CLMS_CSP.EXP;  CSP_CLMS.EXP"
$! MOD #09 END
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE CAPWORK:CAP_EXT_BUM_01_CAS.FTP;
$   @CAPWORK:CAP_EXT_BUM_01_CAS.FTP
$   IF .NOT. $STATUS THEN GOSUB MAIL_ACETEAM_ONLY
$   DELETE/NOCONFIRM/LOG CAPWORK:CAP_EXT_BUM_01_CAS.FTP;
$!
$! MOD #09 BEG
$   OPEN/WRITE Out_FTP MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP;
$   Write Out_FTP "$ FTP ''HNCAS_MCD_SERVER'"
$   Write Out_FTP "LOGIN ''HNCAS_MCD_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''HNCAS_MCD_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   WRITE OUT_FTP "CD ''HNCAS_MCD_TOPFOLDER' "
$   WRITE OUT_FTP "PUT MCDWORK:CAP_MED_CLMS_CSP.EXP;  CSP_CLMS.EXP"
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP;
$   @MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP
$   IF .NOT. $STATUS THEN GOSUB MAIL_ACETEAM_ONLY
$   DELETE/NOCONFIRM/LOG MCDWORK:CAP_EXT_BUM_01_CAS_MCD.FTP;
$!
$   OPEN/WRITE Out_FTP MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP;
$   Write Out_FTP "$ FTP ''USR_MCD_SERVER'"
$   Write Out_FTP "LOGIN ''USR_MCD_ACCOUNT'"
$   Write Out_FTP "PASSWORD ''USR_MCD_PASSWORD'"
$   Write Out_FTP "EXIT-ON-ERROR"
$   Write Out_FTP "cd ''USR_MCD_TOPFOLDER' "
$   WRITE OUT_FTP "PUT MCDWORK:CAP_MED_CLMS_CSP.EXP;  CSP_CLMS.EXP"
$   Write Out_FTP "DIR"
$   Write Out_FTP "EXIT"
$   CLOSE Out_FTP
$   TYPE MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP;
$   @MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP
$   IF .NOT. $STATUS THEN GOSUB MAIL_ACETEAM_ONLY
$   DELETE/NOCONFIRM/LOG MCDWORK:CAP_EXT_BUM_01_USR_MCD.FTP;
$! MOD #09 END
$!
$ END_FTP_CSP:
$!
$!-------------------------------------------------
$   SET NOON
$   delete ACEWORK:udms_batch_HMO.com;*
$   delete ACEWORK:udms_batch_POS.com;*
$   delete ACEWORK:udms_batch_CSP.com;*
$! MOD #09 BEG
$   DELETE CAPWORK:CLMS_IN*.EXP;*
$   DELETE CAPWORK:CAP_CLMS_LOOKUP.EXP;*
$   PURGE CAPWORK:CAP_COM*.EXP
$   PURGE MCDWORK:CAP_MED*.EXP
$! MOD #09 END
$   SET ON
$! END MOD #8
$!
$! Termination
$!
$ WRAP_UP:
$   @COM:TERMINATION
$   EXIT 1
$!
$!Mod #4 Start
$ MAIL_ACETEAM_ONLY:
$   OPEN/WRITE MSG ACEWORK:CAP_EXT_BUM_01.MSG
$   WRITE MSG -
    "*****************************************************************"
$   WRITE MSG " PROCEDURE:"+JOB_NAME
$   WRITE MSG " Failed to generate file:"+File_Name -
    + "! Please contact UDMS support."
$   WRITE MSG -
    "*****************************************************************"
$   CLOSE MSG
$   HN_MAIL ACEWORK:CAP_EXT_BUM_01.MSG @ACE_TEAM
$   DELETE ACEWORK:CAP_EXT_BUM_01.MSG;*
$ RETURN
$!
$ MAIL_ACETEAM:
$   OPEN/WRITE MSG ACEWORK:CAP_EXT_BUM_01.MSG
$   WRITE MSG -
    "*****************************************************************"
$   WRITE MSG ERR_MSG
$   WRITE MSG -
    "*****************************************************************"
$   CLOSE MSG
$   HN_MAIL ACEWORK:CAP_EXT_BUM_01.MSG @ACE_TEAM
$   DELETE ACEWORK:CAP_EXT_BUM_01.MSG;*
$ EXIT
$!Mod #4 end
$! Beg Mod #8
$ MAIL_ACETEAM_FTP_INFO_ERROR:
$   OPEN/WRITE MSG ACEWORK:CAP_EXT_BUM_01.MSG
$   WRITE MSG -
    "*****************************************************************"
$   WRITE MSG "  Error running the ''JOB_NAME' procedure."
$   IF FTP_ERR1 .NES. "N/A" THEN WRITE MSG FTP_ERR1
$   IF FTP_ERR2 .NES. "N/A" THEN WRITE MSG FTP_ERR2
$! MOD #09 BEG
$   IF FTP_ERR3 .NES. WS_NA THEN WRITE MSG FTP_ERR3
$   IF FTP_ERR4 .NES. WS_NA THEN WRITE MSG FTP_ERR3
$! MOD #09 END
$   WRITE MSG -
    "*****************************************************************"
$   CLOSE MSG
$   HN_MAIL ACEWORK:CAP_EXT_BUM_01.MSG @ACE_TEAM
$   DELETE ACEWORK:CAP_EXT_BUM_01.MSG;*
$!
$   EXIT
$!
$! End Mod #8
$!
$! Beg Mod #4
$!
$! Error_condition - Abort and Report error. For the third parameter, either
$!   type the Username or the mail distribution list (with a @).
$!
$! ERROR_CONDITION:
$!   IF F$TYPE(ERROR_STATUS) .EQS. "" THEN ERROR_STATUS = $STATUS
$!   @COM:BATCH_ERROR 'ERROR_STATUS' 'RUN_STATUS' "''MAIL_LST'" "''ERR_MSG'"
$!   IF  ERROR_STATUS THEN ERROR_STATUS = 44     ! Ensures error exit 
$!   EXIT ERROR_STATUS
