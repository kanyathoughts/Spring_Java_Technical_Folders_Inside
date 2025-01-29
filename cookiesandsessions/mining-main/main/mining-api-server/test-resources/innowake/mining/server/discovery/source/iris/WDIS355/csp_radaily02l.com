$ !
$       Cluster = "WHO"
$       Cluster = F$TrnLnm("Environment_Id")
$       Show Sym Cluster
$       If Cluster.Eqs."WHO" Then GoTo Error_Condition
$ !
$ !
$ ! Preliminary setup ...
$ !
$ AREA = "HN_OPER"
$ IF GROUP .EQS. AREA
$   THEN    
$	User_Account = F$GETJPI("","USERNAME")	
$       IF F$EDIT(User_Account,"COLLAPSE,UPCASE") .NES. "OPER_CSP" -
				THEN  GOTO Wrong_Account_Message
$ ENDIF
$ !
$ ON ERROR THEN GOTO ERROR_CONDITION
$!
$! Initialization
$!
$ INITIALIZATION:
$   JOB_NAME :== "CSP_RADAILY02L"
$   @COM:INITIALIZATION
$   SET DEFAULT CSP_RPT
$   TEMP_CLM_WTHLD_HIST := F$TRNLNM("CLM_WTHLD_HIST") + ";"   !Mod #8
$   @COM:OPKD03A.COM            ! read ICSYS_DATE
$! Mod #3 - Begin
$   TIM_START = F$TIME ()
$   WRITE SYS$OUTPUT TIM_START
$   len = F$LENGTH ( TIM_START )
$   ST_TIME = F$EXTRACT(0,11,TIM_START) + ":" + F$EXTRACT(12,len - 12,TIM_START )
$   WRITE SYS$OUTPUT ST_TIME
$! Mod #3 - End
$ !
$!
$      DEFINE REPORT_FILE_NAME_5010  -
                 CSP_RPT_5010:CSP_ACH_BRD_04_'Ach_CYMD.RPT
$      DEFINE HPA835_OUT_5010        -
                  CSP_RPT_5010:CSP_ACH_BRD_04_'Ach_CYMD.DAT
$      DEFINE HPA835_TST_OUT_5010 - 
                  CSP_RPT_5010:CSP_ACH_BRD_04_TST_'Ach_CYMD.DAT
$ exit
