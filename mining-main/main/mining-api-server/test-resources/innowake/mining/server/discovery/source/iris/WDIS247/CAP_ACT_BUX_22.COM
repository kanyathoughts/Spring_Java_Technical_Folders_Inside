$! you can set the symbol keep_verify_on to Y before you call this
$! procedure if you want
$   PV = F$ENVIRONMENT("VERIFY_PROCEDURE")
$   IF .NOT. "''KEEP_VERIFY_ON'" THEN SET NOVERIFY
$!-----------------------------------------------------------------------------!
$!  H E A L T H   N E T   -   Automated Business System.                       !
$!-----------------------------------------------------------------------------!
$! This source code, and all the routines referenced herein, are the           !
$! proprietary properties and trade secrets of HEALTH NET, INC. Except as      !
$! provided for by license agreement, this source code shall not be            !
$! duplicated, used or disclosed without written consent, signed by an officer !
$! of HEALTH NET, INC. 							       !
$!-----------------------------------------------------------------------------!
$! Start Documentation:
$!
$!      SYSTEM:    CAPITATION
$!      JOB:       CAP_ACT_BUX_22
$!      DATE:      22-SEP-2003
$!      AUTHOR:    MXF
$!      FUNCTION:  This procedure loads new records to the 
$!			CAP22_MBR_TIMELINE RDB table
$!      OVERVIEW:  1. combine 4 input files into one sequential file
$!			called CAPTMLN_MBR_TIMELINE; 
$!                 2. we  used to drop CAP22_MBR_TIMELINE index, but rebuilding
$!			the index creates too large of a journal file,
$!			therefore we don't do this anymore
$!                 3. load data from CAPTMLN_MBR_TIMELINE into database
$!			using RMU/LOAD
$!                 4. since we don't drop the index, we don't need to recreate
$!			it (the index is rebuilt yearly during the 
$!			purge/archive process (CAP_PRG_BUY_22)
$!
$!      RESTARTS:   If the job aborts in steps 1 or 2 it can be restarted
$!                  from the beginning with no issues. However, should the
$!                  job abort in step 3, it must be restarted with a P6
$!		    parameter specifying how many records were committed.
$!		    If there is a problem with this, submit a CSR and use
$!		    step 5 of this procedure to delete the loaded records
$!		    and rerun from the start.
$!
$!                      Parameters:
$!                                  P1 = Restart Step
$!                                  P6 = Records to Skip
$!
$!      MOD #     DD-MMM-YYYY      USERNAME(VMSID)       ISR #      DESCRIPTION
$!
$!      MOD #4    24-DEC-2014     DEEPA D NAIR (DXH) -  PID: 14941 - 17286
$!               PID 14941-17286 SHP Enhancements to ABS
$!               Modified to load the new fields into CAP22 table 
$!               as a part of Medi-Cal to ABS coversion project and removed below fields,
$!               CAP22_SUBCAP_QUAL_CODE,CAP22_OED_LOCKED_FLAG,CAP22_COPAY_ADJ_APPLIED,
$!               CAP22_GRPSSNMBR_SERIAL,CAP22_SEGMENT_SERIAL,CAP22_MONTH_SERIAL and CAP22_SLIVER_SERIAL   
$!		
$!	20090302 JLR - per dba roy (dean) collins, add rdm$ parameters
$!		
$!	20070215 JLR - add step 5 to delete records based on run_date,
$!		which we might want to do before a rerun
$!
$!	05-18-04 JLR - when we rebuild the index in step 4, it is using a 
$!		journal file over 4M blocks which could corrupt the database;
$!		instead we'll try loading the records with the index in place
$!
$!..............................................................................
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! STEP_0  Initialization - Initialize local symbols and logicals !!
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
$!
$   @COM:INITIALIZATION
$!
$   BUSINESS_REGION             :== "''p2'"
$   BUSINESS_PRODUCT_LINE       :== "''p3'"
$   CAP_TYPE                    :== "''p4'"
$   RUN_DATE			:== "''P5'"	!used for deleting in step 5
$   CAP_RECORD_SKIP             :== "''p6'"
$!
$   DEFINE RDM$BIND_DAPF_ENABLED 1
$   DEFINE RDM$BIND_DEPTH_BUF_CNT 800
$   DEFINE RDM$BIND_BUFFERS 5000
$   SQL :== $SQL$
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
$
$   @COM:CAP_UTL_BRX_001 'P2 'P3 'P4
$!
$!
$   SET VERIFY
$   DEFINE DELCOM CAPWORK:CAP_ACT_BUX_22_DEL.COM
$
$!  P6 must either be spaces or a valid integer
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
$   SET NOVERIFY
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
$!! Step 1 -    Sort the four CAPTMLN_CAP22_nn files to a single file named
$!!             CAPTMLN_MBR_TIMELINE.
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
$! sort by subs_ssn,mbr_type,prv_type,prv_id
$!
$   Sort /stat -
         /key=(POS:1,SIZ:64) -
         captmln_cap22_01:;,captmln_cap22_02:;,-
         captmln_cap22_03:;,captmln_cap22_04:; CAPTMLN_MBR_TIMELINE
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 2 - Skip dropping the CAP22_PK_SINDEX
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_2:
$! we now run the load without dropping the index:
$   GOTO STEP_3
$
$   RUN_STATUS := 2
$   SHOW PROCESS/ACC
$!
$   ERR_MSG = ERR_MSG + -
            "/SQL Error occurred when dropping the CAP22_PK_SINDEX index" 
$!
$  SQL
    ATTACH 'FILE CAP_DB';
    SET TRANSACTION READ WRITE WAIT 2
        RESERVING CAP22_MBR_TIMELINE FOR EXCLUSIVE WRITE;
    DROP INDEX CAP22_PK_SINDEX;
COMMIT;
EXIT
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
$! Mod #4 begin Added aid_code, aid_ctage and medicare codes
$   rmu/load/stat=(on_commit) -
                /commit=100000/log_commits -
                /rms_record_def=(file=sys$input:) -
		/fields=(CAP22_REGN_CODE, CAP22_PRODUCT_LINE, CAP22_CAP_TYPE, -
			CAP22_SUBS_SSN, CAP22_MBR_TYPE, CAP22_EXP_DATE, -
			CAP22_PAY_TO_PRV_TYPE, CAP22_PAY_TO_PRV_ID, -
			CAP22_PLAN_CODE, CAP22_BENEFIT_ID, CAP22_CARE_TYPE, -
			CAP22_REMIT_CAT, CAP22_LOC, CAP22_GRP_EXCEPTION, -
			CAP22_EFF_DATE, CAP22_CAP_PERIOD_DATE, CAP22_RUN_DATE, -
			CAP22_RATEABLE_GRP_FLAG, CAP22_RATEABLE_PRV_FLAG, -
			CAP22_ELIG_STATUS, CAP22_SLIVER_CODE, -
			CAP22_ACTIVITY_CODE, CAP22_CAP_RETRACTED_FLAG, -
			CAP22_MED_PRV_TYPE, CAP22_MED_PRV_ID, CAP22_PHY_ID, -
			CAP22_GRP_ID, CAP22_FNDG_TYPE, CAP22_GRP_RMC, -
			CAP22_GRP_RMC_PROD, CAP22_COV_CODE, CAP22_MBR_GENDER, -
			CAP22_MBR_CONTR_TYPE, CAP22_MBR_DOB, CAP22_AGE, -
			CAP22_AID_CODE, CAP22_AID_CAT, CAP22_MEDICARE_CODES, -
			CAP22_AGE_SEX_PROF_FACTOR, CAP22_AGE_SEX_HOSP_FACTOR, -
			CAP22_AGE_SEX_OVERRIDE_FLAG, CAP22_AGE_SEX_OVERRIDE_ID, -
			CAP22_BASE_RATE, CAP22_MBR_LEVEL_PREMIUM, -
			CAP22_CALC_METH_CODE, CAP22_PRV_REMIT_RULE_RATE, -
			CAP22_PRV_REMIT_RULE_TYPE, CAP22_MONTHLY_RATE, -
			CAP22_DAYS_CAPPED, CAP22_PRORATED_PCT, -
			CAP22_PRORATED_CAP) -
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
DEFINE FIELD CAP22_PAY_TO_PRV_ID DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_PLAN_CODE DATATYPE IS TEXT SIZE IS 6.
DEFINE FIELD CAP22_BENEFIT_ID DATATYPE IS TEXT SIZE IS 10.
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
DEFINE FIELD CAP22_MED_PRV_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_MED_PRV_ID DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_PHY_ID DATATYPE IS TEXT SIZE IS 6.
DEFINE FIELD CAP22_GRP_ID DATATYPE IS TEXT SIZE IS 8.
DEFINE FIELD CAP22_FNDG_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_GRP_RMC DATATYPE IS TEXT SIZE IS 2.
DEFINE FIELD CAP22_GRP_RMC_PROD DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_COV_CODE DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_MBR_GENDER DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_MBR_CONTR_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_MBR_DOB DATATYPE IS TEXT SIZE IS 8.
DEFINE FIELD CAP22_AGE DATATYPE IS SIGNED WORD.
DEFINE FIELD CAP22_AID_CODE   DATATYPE IS TEXT SIZE IS 2.
DEFINE FIELD CAP22_AID_CAT    DATATYPE IS TEXT SIZE 3.
DEFINE FIELD CAP22_MEDICARE_CODES DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_AGE_SEX_PROF_FACTOR DATATYPE IS SIGNED LONGWORD SCALE -5.
DEFINE FIELD CAP22_AGE_SEX_HOSP_FACTOR DATATYPE IS SIGNED LONGWORD SCALE -5.
DEFINE FIELD CAP22_AGE_SEX_OVERRIDE_FLAG DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_AGE_SEX_OVERRIDE_ID DATATYPE IS TEXT SIZE IS 4.
DEFINE FIELD CAP22_BASE_RATE DATATYPE IS SIGNED LONGWORD SCALE -4.
DEFINE FIELD CAP22_MBR_LEVEL_PREMIUM DATATYPE IS SIGNED LONGWORD SCALE -2.
DEFINE FIELD CAP22_CALC_METH_CODE DATATYPE IS TEXT SIZE IS 3.
DEFINE FIELD CAP22_PRV_REMIT_RULE_RATE DATATYPE IS SIGNED LONGWORD SCALE -4.
DEFINE FIELD CAP22_PRV_REMIT_RULE_TYPE DATATYPE IS TEXT SIZE IS 1.
DEFINE FIELD CAP22_MONTHLY_RATE DATATYPE IS SIGNED LONGWORD SCALE -2.
DEFINE FIELD CAP22_DAYS_CAPPED DATATYPE IS SIGNED WORD.
DEFINE FIELD CAP22_PRORATED_PCT DATATYPE IS SIGNED LONGWORD SCALE -5.
DEFINE FIELD CAP22_PRORATED_CAP DATATYPE IS SIGNED LONGWORD SCALE -2.
DEFINE RECORD CAP22.
        CAP22_REGN_CODE .
        CAP22_PRODUCT_LINE .
        CAP22_CAP_TYPE .
        CAP22_SUBS_SSN .
        CAP22_MBR_TYPE .
        CAP22_EXP_DATE .
        CAP22_PAY_TO_PRV_TYPE .
        CAP22_PAY_TO_PRV_ID .
        CAP22_PLAN_CODE .
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
        CAP22_MED_PRV_TYPE .
        CAP22_MED_PRV_ID .
        CAP22_PHY_ID .
        CAP22_GRP_ID .
        CAP22_FNDG_TYPE .
        CAP22_GRP_RMC .
        CAP22_GRP_RMC_PROD .
        CAP22_COV_CODE .
        CAP22_MBR_GENDER .
        CAP22_MBR_CONTR_TYPE .
        CAP22_MBR_DOB .
        CAP22_AGE .
        CAP22_AID_CODE .
        CAP22_AID_CAT .
        CAP22_MEDICARE_CODES .
        CAP22_AGE_SEX_PROF_FACTOR .
        CAP22_AGE_SEX_HOSP_FACTOR .
        CAP22_AGE_SEX_OVERRIDE_FLAG .
        CAP22_AGE_SEX_OVERRIDE_ID .
        CAP22_BASE_RATE .
        CAP22_MBR_LEVEL_PREMIUM .
        CAP22_CALC_METH_CODE .
        CAP22_PRV_REMIT_RULE_RATE .
        CAP22_PRV_REMIT_RULE_TYPE .
        CAP22_MONTHLY_RATE .
        CAP22_DAYS_CAPPED .
        CAP22_PRORATED_PCT .
        CAP22_PRORATED_CAP .
END CAP22 RECORD.
$!Mod #4 End - Added aid_code, aidcateg and medicare_codes
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 4 - Skip Creating the new Index  
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_4:
$! since we're not dropping the index, we don't need to rebuild it
$   GOTO WRAP_UP
$
$   RUN_STATUS := 4
$   SHOW PROCESS/ACC
$!
$   ERR_MSG = ERR_MSG + -
            "/SQL Error occurred when creating the CAP22_PK_SINDEX index" 
$!
$ SQL
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
$
$   GOTO WRAP_UP
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! Step 5 - Use this step to delete records by run_date before a rerun
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ STEP_5:
$   RUN_STATUS := 5
$   SHOW PROCESS/ACC
$!
$   ERR_MSG = ERR_MSG + -
            "/Error occurred during delete"
$
$   IF RUN_DATE .EQS. "" THEN RUN_DATE = CAPREGN_CAP_RUN_DATE
$! convert to VMS style date
$   M1 = "JAN"
$   M2 = "FEB"
$   M3 = "MAR"
$   M4 = "APR"
$   M5 = "MAY"
$   M6 = "JUN"
$   M7 = "JUL"
$   M8 = "AUG"
$   M9 = "SEP"
$   M10 = "OCT"
$   M11 = "NOV"
$   M12 = "DEC"
$   Y = F$EXTRACT(0,4,RUN_DATE)
$   D = F$EXTRACT(6,2,RUN_DATE)
$   M = F$EXTRACT(4,2,RUN_DATE) + 0  		! force M to be numeric
$   RUN_DATE_VMS = "''D'-" + M'M + "-''Y'"	! VMS style date
$   SHOW SYM RUN_DATE
$   SHOW SYM RUN_DATE_VMS
$
$   OPEN/WRITE DC DELCOM:
$   WRITE DC "$   SQL"
$   WRITE DC "    ATTACH 'FILE CAP_DB';"
$   WRITE DC "    SET TRANSACTION READ WRITE "
$   WRITE DC "	ISOLATION LEVEL READ COMMITTED"
$   WRITE DC "	RESERVING CAP22_MBR_TIMELINE FOR EXCLUSIVE WRITE ;"
$   WRITE DC "    DELETE FROM CAP22_MBR_TIMELINE"
$   WRITE DC "	WHERE CAP22_RUN_DATE = '" + "''RUN_DATE_VMS'" + "';"
$   WRITE DC "    COMMIT;"
$   WRITE DC "    EXIT ;"
$   WRITE DC "$   EXIT
$   CLOSE DC
$   @DELCOM:
$!
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! WRAP_UP -- End                                                 !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ WRAP_UP:
$   SHOW PROCESS/ACC
$   SET  NOVERIFY       ! turn verify off for @com:termination
$   @COM:TERMINATION
$   IF PV THEN SET VERIFY ! restore prior verify setting
$   EXIT 1
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!! ERROR_CONDITION -- Error Handling                              !!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
