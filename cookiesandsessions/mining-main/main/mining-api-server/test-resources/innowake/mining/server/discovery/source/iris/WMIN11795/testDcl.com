! ON ERROR THEN GOTO ERROR_CONDITION
! INITIALIZATIONS:
!       RUN_STATUS = "INITIALIZATION"
!       JOB_NAME :== "ENID101"
!       @COM:INITIALIZATION
!       MAIL_LIST  = "@CLM_TEAM"
!
!   
!
!
! dEf    OTHER_LOGICAL_NAME_SAME_FILE  PRDIND_LTR_DISK:[DEN_LTRS]DENLTBL.TBL
!        WRITE FTP_COM "$  DEFINE EMAIL_FILENAME WORK:OVERRIDE_FTP_COM_FILE.doc"
