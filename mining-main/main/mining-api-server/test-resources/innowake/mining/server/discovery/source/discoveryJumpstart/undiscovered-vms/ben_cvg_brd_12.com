$! This source code , and all the routines referenced  herein, are the    
$! proprietary properties and trade secrets of HEALTH NET, INC. Except as 
$! provided for  by license agreement,  this source code shall not  be   
$! duplicated, used or  disclosed  without  written  consent, signed      
$! by an officer of HEALTH NET, INC.                                      
$!
$! Start Documentation
$!	System  :  Benefits	
$! 	Job     :  BEN_CVG_BRD_12.COM  	
$!	Date    :  08-Aug-2011		
$! 	Author  :  Ramesh masula (mr6)		
$!
$!	Overview: The command procedure takes a back up of of files from 
$!                BEN_WORK: to BENDAT: and maintain 7 versions.     
$!
$!---------------------- modification history ------------------------------
$!
$! mod:  <dd-mmm-yyyy>        <isr>             <author>   <author id>
$! 
$! Mod #1 22-APR-2013      RT # 1704867         S,Parthasarathy (PS2)
$!                         To delete and purge 3 more files from BEN_WORK
$!                         directory.
$!     
$!        08-aug-2011      initial creation     Ramesh Masula  (MR6)
$!                         PID14252 Benefits mapping translation
$!
$!--------------------------------------------------------------------------
$! End Documentation
$!
$!
$  ON ERROR THEN GOTO ERROR_CONDITION
$! Initialization
$  INITIALIZATION:
$   JOB_NAME :== "BEN_CVG_BRD_12"
$   RUN_STATUS = "INITIALIZATION"
$   @COM:INITIALIZATION
$   @COM:OPKD03A.COM
$   THIS_ENVIRONMENT = F$TRNLNM("ENVIRONMENT_ID")
$   SHOW SYMBOL THIS_ENVIRONMENT
$   MAIL_LIST  = "@BEN_TEAM"
$   SET VERIFY
$   SET PREFIX "(!5%T) "
$!
$! Symbol Definition - Begin
$   SYM_SPACE   =   ""
$   SYM_ZERO    =   "0"
$   SYM_THIRTY  =   "30"
$   SYM_STAT_18 =   18
$   SYM_STAT_20 =   "20"
$   SYM_ONE     =   1
$   SYM_THREE   =   3
$   SYM_STAT_40 =   "40"
$! Symbol Definition - End
$   IF P1 .EQS. SYM_SPACE     THEN P1 = SYM_ZERO
$   IF P1 .GTS. SYM_THIRTY   
$    THEN
$       $STATUS = SYM_STAT_18
$       GOTO ERROR_CONDITION
$   ENDIF
$!
$!**************************************************************
$!  COPYING FILES FROM BEN_WORK TO BENDAT
$!**************************************************************
$  STEP10:
$   RUN_STATUS = SYM_STAT_10
$!
$   IF F$SEARCH ("BEN_WORK:CVG_HIST_EXT.SEQ") .NES. SYM_SPACE
$   THEN
$       BACKUP/LOG BEN_WORK:CVG_HIST_EXT.SEQ; BENDAT:;
$   ENDIF
$!
$   IF F$SEARCH ("BEN_WORK:BEN_CVG_NEWLOAD_EXT.SEQ") .NES. SYM_SPACE
$   THEN
$       BACKUP/LOG BEN_WORK:BEN_CVG_NEWLOAD_EXT.SEQ;  BENDAT:;
$   ENDIF
$!
$   IF F$SEARCH ("BEN_WORK:MBMDREFF.DAT") .NES. SYM_SPACE
$   THEN
$       BACKUP/LOG BEN_WORK:MBMDREFF.DAT;  BENDAT:;
$   ENDIF
$!
$   IF F$SEARCH ("BEN_WORK:MBRDREFF.DAT") .NES. SYM_SPACE
$   THEN
$       BACKUP/LOG BEN_WORK:MBRDREFF.DAT; BENDAT:;
$   ENDIF
$!
$   IF F$SEARCH ("BEN_WORK:NEW_ABS2MC400_INTERIM.SEQ") .NES. SYM_SPACE
$   THEN
$       BACKUP/LOG BEN_WORK:BEN_NEW_ABS2MC400_INTERIM.SEQ; BENDAT:;
$   ENDIF
$!
$!
$   IF F$SEARCH ("BEN_WORK:EMAIL_DETAILS.SEQ") .NES. SYM_SPACE
$   THEN
$       BACKUP/LOG BEN_WORK:EMAIL_DETAILS.SEQ; BENDAT:;
$   ENDIF
$!
$!**************************************************************
$!Keep 7 Versions in BENDAT directory
$!**************************************************************
$!
$   PURGE/LOG/KEEP=07 BENDAT:CVG_HIST_EXT.SEQ
$   PURGE/LOG/KEEP=07 BENDAT:BEN_CVG_NEWLOAD_EXT.SEQ
$   PURGE/LOG/KEEP=07 BENDAT:MBMDREFF.DAT
$   PURGE/LOG/KEEP=07 BENDAT:MBRDREFF.DAT    
$   PURGE/LOG/KEEP=07 BENDAT:BEN_NEW_ABS2MC400_INTERIM.SEQ
$   PURGE/LOG/KEEP=07 BENDAT:ABS2MC400.TXT
$   PURGE/LOG/KEEP=07 BENDAT:EMAIL_DETAILS.SEQ
$! Mod #1 Begin
$   DELETE/LOG        BEN_WORK:BEN_CVG_NEWLOAD_EXT.SEQ;*
$   PURGE/LOG/KEEP=07 BEN_WORK:BEN_NEW_ABS2MC400_INTERIM.SEQ
$   DELETE/LOG        BEN_WORK:CVG_HIST_EXT.SEQ;*
$! Mod #1 End         
$!
$!**************************************************************
$!                                     Clean up 		 
$!     Deletes the files on daily basis from ben_work directory
$!**************************************************************
$!
$  IF F$SEARCH ("BEN_WORK:CVG_DISTINCT_EXT.SEQ") .NES. SYM_SPACE
$  THEN
$     DELETE/NOCONFIRM/LOG BEN_WORK:CVG_DISTINCT_EXT.SEQ;*
$  ENDIF
$!
$  IF F$SEARCH ("BEN_WORK:BEN_CVG_NEWLOAD_EXT.ISM") .NES. SYM_SPACE
$  THEN
$     DELETE/NOCONFIRM/LOG BEN_WORK:BEN_CVG_NEWLOAD_EXT.ISM;*
$  ENDIF
$!
$  IF F$SEARCH ("BEN_WORK:BEN_MBMDREFF_IMPORT.ISM") .NES. SYM_SPACE
$  THEN
$     DELETE/NOCONFIRM/LOG BEN_WORK:BEN_MBMDREFF_IMPORT.ISM;*
$  ENDIF
$!
$  IF F$SEARCH ("BEN_WORK:BEN_MBRDREFF_IMPORT.ISM") .NES. SYM_SPACE
$  THEN
$     DELETE/NOCONFIRM/LOG BEN_WORK:BEN_MBRDREFF_IMPORT.ISM;*
$  ENDIF
$!
$  IF F$SEARCH ("BEN_WORK:BEN_NEW_ABS2MC400_INTERIM.ISM") .NES. SYM_SPACE
$  THEN
$     DELETE/NOCONFIRM/LOG BEN_WORK:BEN_NEW_ABS2MC400_INTERIM.ISM;*
$  ENDIF
$!
$  IF F$SEARCH ("BEN_WORK:ABS2MC400.ISM") .NES. SYM_SPACE
$  THEN
$     DELETE/NOCONFIRM/LOG BEN_WORK:ABS2MC400.ISM;*
$  ENDIF
$!
$  IF F$SEARCH ("BEN_WORK:ABS2MC400.TXT") .NES. SYM_SPACE
$  THEN
$     DELETE/NOCONFIRM/LOG BEN_WORK:ABS2MC400.TXT;*
$  ENDIF
$!
$  IF F$SEARCH ("BEN_WORK:EMAIL_DETAILS.SEQ") .NES. SYM_SPACE
$  THEN
$     DELETE/NOCONFIRM/LOG BEN_WORK:EMAIL_DETAILS.SEQ;*
$  ENDIF
$!
$ WRAP_UP:
$   @COM:TERMINATION
$ EXIT 
$!
$ ERROR_CONDITION:
$!
$   ERROR_STATUS = $STATUS
$   ERROR_MSG = F$MESSAGE(ERROR_STATUS)
$   @COM:BATCH_ERROR 'ERROR_STATUS 'RUN_STATUS "''MAIL_LIST'"
$  IF ERROR_STATUS
$  THEN
$!    ENSURE ERROR EXIT
$     EXIT 44 
$  ELSE
$    EXIT ERROR_STATUS
$  ENDIF
$!
$  WRONG_ACCOUNT_MESSAGE:
$!
$   W   "                  -----------------------"
$   W   "                  E    R    R    O    R"
$   W   "                  -----------------------"
$   W   "                   You are in the wrong account!"
$   W   "                   Please login into OPER_CLAIMS"
$   W   ""
$   SET NOVERIFY
$   EXIT 44
$ EXIT_COM:
$       SHOW PROCESS/ACCOUNTING
$       EXIT
