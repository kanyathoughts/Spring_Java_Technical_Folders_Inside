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
$!  officer of HEALTH NET. COPYRIGHT ) 2002 By HEALTH NET CORPORATION,         !
$!  Woodland Hills, CA All rights reserved.                                    !
$!-----------------------------------------------------------------------------!
$!
$! START DOCUMENTATION
$!
$!      SYSTEM:    CAP
$!      JOB:       CAP_ACT_BUX_122  This procedure maintains the cap22 table 
$!                                 CAP22_MBR_TIMELINE in RDB; 
$!                 1. combine 4 input files into one CAPTMLN_MBR_TIMELINE; 
$!                 3. load data from CAPTMLN_MBR_TIMELINE into database
$!
$!		   in spring '04 we deactivated steps 2 & 4 because we found
$!		   its faster to load records with the index in place than
$!		   to drop, load, and rebuild:
$!			2. drop CAP22_MBR_TIMELINE index;
$!			4. create table indexes
$!		   besides, the .RUJ journal file created during the index
$!		   build was so huge (over 4 megablocks), it was threatening
$!		   to corrupt the database -- we tried partitioning the
$!		   index, but there is no way to build in the index in more
$!		   than one step, so no place to put the COMMIT except at
$!		   then end; so really our only choice is to do it this week
$!		   and rebuild the index once a year using a BATCH transaction
$!		   at the same time we perform an archive/purge
$!
$!      TITLE:     CAP22 table maintenance  
$!      DATE:      22-SEP-2003
$!      AUTHOR:    MXF
$!              
$!   
$!      RESTARTS:   If the job aborts in steps 1 or 2 it can be restarted
$!                  from the beginning with no issues. However, should the
$!                  job abort in step 3, a CSR might be required to restore 
$!                  the database, since commits are performed for every 
$!                  100000 records stored. If the job should abort in step
$!                  4, there could be an issue with the rmu load in step 3. 
$!
$!                      Parameters:
$!                                  P1 = Restart Step
$!                                  P6 = Records to Skip
$!
$!      OVERVIEW: As described above.              
$!
$!      REVISED : <DD-MMM-YYYY> BY <PROGRAMMER NAME>
$!
$!	08-25-04 JLR - release 6.0
$!		changed name from CAP_ACT_BUX_22.com to CAP_ACT_BUX_122.com
$!		since NE will require its own procedure; now we'll have to
$!		use the /CORR qualifier with RMU/LOAD since the order of
$!		fields in the record no longer matches the order of columns
$!		in the database
$!
$!	05-18-04 JLR - when we rebuild the index in step 4, it is using a 
$!		journal file over 4M blocks which could corrupt the database;
$!		instead we'll try loading the records with the index in place
$!
$!	09-Jan-2004		JLR
$!	started code to archive and purge old records (later abandoned --
$!	it will be a separate procedure)
$!
$!	29-dec-2003		JLR
$!	In production, the job could not drop the index in step 2, but it
$!	did not blow off; i'll try checking for warning instead of error.
$!
$!..............................................................................
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! STEP_0  Initialization - Initialize local symbols and logicals !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ INITIALIZATION:
$!
$   ON WARNING THEN $ GOTO ERROR_CONDITION
$!
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
$   ECHO "                 P6=''p6'"
$   ECHO ""
$!
$   SET VERIFY
$!
$   BUSINESS_REGION             :== "''p2'"
$   BUSINESS_PRODUCT_LINE       :== "''p3'"
$   CAP_TYPE                    :== "''p4'"
$   CAP_RECORD_SKIP             :== "''p6'"
$!
$   SQL :== $SQL$
$   DEFINE SYSOUT CAPWORK:CAP_ACT_BUX_22.SYSOUT
$   DEFINE SYSERR CAPWORK:CAP_ACT_BUX_22.SYSERR
$!
$!  Check to see if P6 is an integer. P6 must either be spaces or a valid
$!  integer
$!
$   IF  CAP_RECORD_SKIP .EQS. ""
$   THEN
$       CAP_RECORD_SKIP = 0
$   ELSE
$       IF F$TYPE(CAP_RECORD_SKIP) .NES. "INTEGER"
$       THEN 
$           ERROR_STATUS = 44
$           ERR_MSG = ERR_MSG + -
                "/PARAMETER=''P6' Must be SPACES or a valid Integer". 
$           GOTO ERROR_CONDITION
$       ENDIF
$   ENDIF
$!
$   IF  P1 .EQS. "E" THEN EXIT
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
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 1 -    Concatenate CAPTMLN_CAP22_nn files (output from
$!!		CAP_ACT_BUX_50) to a single file (CAPTMLN_MBR_TIMELINE)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_1:
$   RUN_STATUS = "1"
$   SHOW PROCESS/ACC
$!
$   IF F$SEARCH("CAPTMLN_MBR_TIMELINE") .NES. "" 
$   THEN
$       DEL CAPTMLN_MBR_TIMELINE:;*
$   ENDIF
$!
$   IF   F$SEARCH("CAPTMLN_CAP22_01") .EQS. "" -
    .OR. F$SEARCH("CAPTMLN_CAP22_02") .EQS. "" -
    .OR. F$SEARCH("CAPTMLN_CAP22_03") .EQS. "" -
    .OR. F$SEARCH("CAPTMLN_CAP22_04") .EQS. ""
$   THEN
$           ERROR_STATUS = "%X10018290"
$           ERR_MSG = ERR_MSG + -
                "Can't find all four CAPTMLN_CAP22_nn files."
$           GOTO ERROR_CONDITION
$   ENDIF
$!
$   LOGDIR CAPTMLN_CAP22_01
$   LOGDIR CAPTMLN_CAP22_02
$   LOGDIR CAPTMLN_CAP22_03
$   LOGDIR CAPTMLN_CAP22_04
$!
$! 12-29-03 jlr - we used to sort the files together, but there's no reason
$! to sort, so we'll just concatenate (and the only reason to do that is to
$! make a restart easier if it blows off in step 3.)
$!
$   COPY/LOG/CONCATENATE captmln_cap22_01:;,captmln_cap22_02:;,-
         captmln_cap22_03:;,captmln_cap22_04:; CAPTMLN_MBR_TIMELINE
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 2 - Drop the CAP22_PK_SINDEX    (disabled)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_2:
$! we'll try the load without dropping the index:
$   GOTO STEP_3
$!
$   RUN_STATUS := 2
$   SHOW PROCESS/ACC
$!
$   ERR_MSG = ERR_MSG + -
            "/SQL Error occurred when dropping the CAP22_PK_SINDEX index" 
$!
$   IF F$SEARCH("SYSOUT") .NES. "" THEN DELETE/NOLOG SYSOUT:;*
$   IF F$SEARCH("SYSERR") .NES. "" THEN DELETE/NOLOG SYSERR:;*
$   DEFINE/NOLOG SYS$OUTPUT SYSOUT:
$   SQL
    ATTACH 'FILE CAP_DB';
    SET TRANSACTION READ WRITE WAIT 2
        RESERVING CAP22_MBR_TIMELINE FOR EXCLUSIVE WRITE;
    DROP INDEX CAP22_PK_SINDEX;
    COMMIT;
    EXIT
$   DEASSIGN SYS$OUTPUT
$   TYPE SYSOUT
$   SEARCH SYSOUT/OUT=SYSERR "%"
$   IF F$FILE("SYSERR:","EOF") .GT. 0
$   THEN
$	GOTO ERROR_CONDITION
$   ENDIF
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 3 -Using RMU, load the table    
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_3:
$   RUN_STATUS := 3
$   SHOW PROCESS/ACC
$!
$   ERR_MSG = ERR_MSG + -
            "/SQL Error occurred when loading the database" 
$!
$   LOGDIR CAPTMLN_MBR_TIMELINE
$!
$!  Check for Restart
$!
$   IF CAP_RECORD_SKIP .GT. 0 
$   THEN
$      SET NOVERIFY
$      ECHO ""
$      ECHO "******************  RESTART *************************"
$      ECHO "SKIPPING FIRST ", CAP_RECORD_SKIP, " RECORDS"
$      ECHO "*****************************************************"
$      ECHO ""
$      SET VERIFY
$   ENDIF  
$!
$   rmu/load/corr/stat=(on_commit) -
                /commit=100000/log_commits -
                /rms_record_def=(file=sys$input:) -
                /skip='CAP_RECORD_SKIP -
            CAP_DB                              -
            CAP22_MBR_TIMELINE              -
            CAPTMLN_MBR_TIMELINE
DEFINE FIELD CAP22_REGN_CODE DATATYPE IS TEXT SIZE IS 2.
DEFINE FIELD CAP22_PRODUCT_LINE DATATYPE IS TEXT SIZE IS 2.
DEFINE FIELD CAP22_CAP_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_SUBS_SSN DATATYPE IS TEXT SIZE IS 9.
DEFINE FIELD CAP22_MBR_TYPE DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_EXP_DATE DATATYPE IS TEXT SIZE IS 8.
DEFINE FIELD CAP22_PAY_TO_PRV_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_PAY_TO_PRV_ID DATATYPE IS TEXT SIZE IS 6.
DEFINE FIELD CAP22_CONTRACT_NUM DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_COPAY_SCHED DATATYPE IS TEXT SIZE IS 6.
DEFINE FIELD CAP22_RIDER_TYPE DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_RIDER_CODE DATATYPE IS TEXT SIZE IS 3.
DEFINE FIELD CAP22_BENEFIT_ID DATATYPE IS TEXT SIZE IS 8.
DEFINE FIELD CAP22_CARE_TYPE DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_REMIT_CAT DATATYPE IS TEXT SIZE IS 3.
DEFINE FIELD CAP22_LOC DATATYPE IS TEXT SIZE IS 3.
DEFINE FIELD CAP22_GRP_EXCEPTION DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_EFF_DATE DATATYPE IS TEXT SIZE IS 8.
DEFINE FIELD CAP22_CAP_PERIOD_DATE DATATYPE IS TEXT SIZE IS 8.
DEFINE FIELD CAP22_RUN_DATE DATATYPE IS TEXT SIZE IS 8.
DEFINE FIELD CAP22_RATEABLE_GRP_FLAG DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_RATEABLE_PRV_FLAG DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_ELIG_STATUS DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_SLIVER_CODE DATATYPE IS TEXT SIZE IS 2.
DEFINE FIELD CAP22_ACTIVITY_CODE DATATYPE IS TEXT SIZE IS 2.
DEFINE FIELD CAP22_CAP_RETRACTED_FLAG DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_OED_LOCKED_FLAG DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_MED_PRV_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_MED_PRV_ID DATATYPE IS TEXT SIZE IS 6.
DEFINE FIELD CAP22_PHY_ID DATATYPE IS TEXT SIZE IS 6.
DEFINE FIELD CAP22_GRP_ID DATATYPE IS TEXT SIZE IS 10.
DEFINE FIELD CAP22_FNDG_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_PPC_KEY DATATYPE IS TEXT SIZE IS 6.
DEFINE FIELD CAP22_PPC_CODE DATATYPE IS TEXT SIZE IS 3.
DEFINE FIELD CAP22_GRP_RMC DATATYPE IS TEXT SIZE IS 2.
DEFINE FIELD CAP22_GRP_RMC_PROD DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_MBR_GENDER DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_MBR_CONTR_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_MBR_DOB DATATYPE IS TEXT SIZE IS 8.
DEFINE FIELD CAP22_AGE DATATYPE IS SIGNED WORD.
DEFINE FIELD CAP22_AGE_SEX_PROF_FACTOR DATATYPE IS SIGNED LONGWORD SCALE -5.
DEFINE FIELD CAP22_AGE_SEX_HOSP_FACTOR DATATYPE IS SIGNED LONGWORD SCALE -5.
DEFINE FIELD CAP22_AGE_SEX_OVERRIDE_FLAG DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_AGE_SEX_OVERRIDE_ID DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_BASE_RATE DATATYPE IS SIGNED LONGWORD SCALE -4.
DEFINE FIELD CAP22_CALC_METH_CODE DATATYPE IS TEXT SIZE IS 3.
DEFINE FIELD CAP22_PRV_REMIT_RULE_RATE DATATYPE IS SIGNED LONGWORD SCALE -4.
DEFINE FIELD CAP22_PRV_REMIT_RULE_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_MONTHLY_RATE DATATYPE IS SIGNED LONGWORD SCALE -2.
DEFINE FIELD CAP22_DAYS_CAPPED DATATYPE IS SIGNED WORD.
DEFINE FIELD CAP22_PRORATED_PCT DATATYPE IS SIGNED LONGWORD SCALE -5.
DEFINE FIELD CAP22_PRORATED_CAP DATATYPE IS SIGNED LONGWORD SCALE -2.
DEFINE FIELD CAP22_PLAN_CODE DATATYPE IS TEXT SIZE IS 4.
DEFINE RECORD CAP22.
        CAP22_REGN_CODE .
        CAP22_PRODUCT_LINE .
        CAP22_CAP_TYPE .
        CAP22_SUBS_SSN .
        CAP22_MBR_TYPE .
        CAP22_EXP_DATE .
        CAP22_PAY_TO_PRV_TYPE .
        CAP22_PAY_TO_PRV_ID .
	CAP22_CONTRACT_NUM .
	CAP22_COPAY_SCHED .
	CAP22_RIDER_TYPE .
	CAP22_RIDER_CODE .
        CAP22_BENEFIT_ID .
        CAP22_CARE_TYPE .
        CAP22_REMIT_CAT .
        CAP22_LOC .
        CAP22_GRP_EXCEPTION .
        CAP22_EFF_DATE .
        CAP22_CAP_PERIOD_DATE .
        CAP22_RUN_DATE .
        CAP22_RATEABLE_GRP_FLAG .
        CAP22_RATEABLE_PRV_FLAG .
        CAP22_ELIG_STATUS .
        CAP22_SLIVER_CODE .
        CAP22_ACTIVITY_CODE .
        CAP22_CAP_RETRACTED_FLAG .
        CAP22_OED_LOCKED_FLAG.
        CAP22_MED_PRV_TYPE .
        CAP22_MED_PRV_ID .
        CAP22_PHY_ID .
        CAP22_GRP_ID .
        CAP22_FNDG_TYPE .
	CAP22_PPC_KEY .
	CAP22_PPC_CODE .
        CAP22_GRP_RMC .
        CAP22_GRP_RMC_PROD .
        CAP22_MBR_GENDER .
        CAP22_MBR_CONTR_TYPE .
        CAP22_MBR_DOB .
        CAP22_AGE .
        CAP22_AGE_SEX_PROF_FACTOR .
        CAP22_AGE_SEX_HOSP_FACTOR .
        CAP22_AGE_SEX_OVERRIDE_FLAG .
        CAP22_AGE_SEX_OVERRIDE_ID .
        CAP22_BASE_RATE .
        CAP22_CALC_METH_CODE .
        CAP22_PRV_REMIT_RULE_RATE .
        CAP22_PRV_REMIT_RULE_TYPE .
        CAP22_MONTHLY_RATE .
        CAP22_DAYS_CAPPED .
        CAP22_PRORATED_PCT .
        CAP22_PRORATED_CAP .
        CAP22_PLAN_CODE .
END CAP22 RECORD.
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 4 - Create the new Index  (disabled)
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_4:
$! since we're not dropping the index, we don't need to rebuild it
$   GOTO WRAP_UP
$!
$   RUN_STATUS := 4
$   SHOW PROCESS/ACC
$!
$   ERR_MSG = ERR_MSG + -
            "/SQL Error occurred when creating the CAP22_PK_SINDEX index" 
$!
$   IF F$SEARCH("SYSOUT") .NES. "" THEN DELETE/NOLOG SYSOUT:;*
$   IF F$SEARCH("SYSERR") .NES. "" THEN DELETE/NOLOG SYSERR:;*
$   DEFINE/NOLOG SYS$OUTPUT SYSOUT:
$   SQL
    ATTACH 'FILE CAP_DB';

    SET TRANSACTION READ WRITE WAIT 2
        RESERVING CAP22_MBR_TIMELINE FOR EXCLUSIVE WRITE;

    CREATE INDEX CAP22_PK_SINDEX ON CAP22_MBR_TIMELINE
        (CAP22_REGN_CODE ASC,
	 CAP22_PRODUCT_LINE ASC,
	 CAP22_CAP_TYPE ASC,
         CAP22_SUBS_SSN ASC,
         CAP22_MBR_TYPE ASC,
         CAP22_EXP_DATE ASC,
         CAP22_PAY_TO_PRV_TYPE ASC,
	 CAP22_PAY_TO_PRV_ID ASC)
    TYPE IS SORTED
    STORE IN CAP22_I_AREA
    ;
    COMMIT;
    EXIT
$   DEASSIGN SYS$OUTPUT
$   TYPE SYSOUT
$   SEARCH SYSOUT/OUT=SYSERR "%"
$   IF F$FILE("SYSERR:","EOF") .GT. 0
$   THEN
$	GOTO ERROR_CONDITION
$   ENDIF
$   GOTO WRAP_UP
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! WRAP_UP -- End                                                 !!
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
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! ERROR_CONDITION -- Error Handling                              !!
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
