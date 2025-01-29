PROGRAM BEN_PRD_BUD_17
!******************************************************************
! This program,  and all the routines referenced  herein,   are the
! proprietary properties and trade secrets of HEALTH NET INC. Except 
! as provided for by license agreement, this program  shall not   be
! duplicated, used  or  disclosed  without  written consent, signed
! by an officer of HEALTH NET INC.
!******************************************************************
! Start Documentation
!
!       SYSTEM:  ABS CLAIMS
!       PROGRAM: BEN_PRD_BUD_17.BAS
!       TITLE:   BEN_PRD_BUD_17.BAS
!       DATE:    08/04/2011
!       AUTHOR:  Punitha Devaraj(S1T)
!
!   MODIFICATION :
!                 Date (mm/dd/yyyy)     MOD #       ID       Name  
!
!
!   OVERVIEW:
!              The purpose of the oneshot is to create a new ADJRSN file
!              similar to the existing file but with a new record layout
!
! End Documentation

  !---------------------------------------------------------------!
  !       COMPILER OPTIONS                                        !
  !---------------------------------------------------------------!

      OPTION  TYPE = EXPLICIT

  !---------------------------------------------------------------!
  !       INCLUDES, MAPS, ETC                                     !
  !---------------------------------------------------------------!

        %INCLUDE 'INC:ICISTANDARDS.INC'
        %INCLUDE 'INC:ICCHANDEC.INC'
        %INCLUDE 'INC:BASIC-ERROR-CODES.INC'
        %INCLUDE 'INC:ICIDATE.REC'
        %INCLUDE 'INC:ICITIME.REC'
        %INCLUDE 'INC:ICISTAMP.REC'
        %INCLUDE 'INC:COMMON_IO.INC'
        %INCLUDE 'INC:ICPROG_STAT.REC'

    EXTERNAL BYTE FUNCTION ICIF001_Assign_Channel
   !---------------------------------------------------------------!
   !       DECLARATIONS                                            !
   !---------------------------------------------------------------!
	
	DECLARE STRING CONSTANT WS_SPACE      = " "
        DECLARE STRING CONSTANT C_Read        = "Read"
        DECLARE STRING CONSTANT C_Modify      = "MODIFY"
        DECLARE LONG CONSTANT ON_ERROR_NO_BLOW  = 0

        DECLARE LONG  HN_Stat,                      &
                      Stat,                         &
                      Adjrsn_ch,                   &
                      Seq_ch,                       &
                      WS_J

         DECLARE BYTE EoF_Flag

       RECORD AdjRsn

                VARIANT
                  CASE
                        STRING  Key0    = 19                 ! MOD #4

                  CASE
                        STRING  Cd              =  1
                        STRING  Rsn             =  6
                        STRING  Adj_FILL        =  4        ! MOD #4
                        STRING  Plan            =  4
                        ICIDATE Exp_Date
        
                END VARIANT

                ICIDATE Eff_Date
                LONG    Flag
                STRING  Descrip         =  80
                STRING  Category        =  2
!BEGIN ISCEXP
                LONG    Svc_Flag (10)           ! 1 bit / external service      
!END ISCEXP
        END RECORD

        !===============================================================

        DECLARE LONG CONSTANT                                           &
                ! Used in limit                                         &
                AdjRSN$Limit                    = X'0001'               &
                                                                        &
        ,       AdjRSN$No_Male                  = X'0002'               &
        ,       AdjRSN$No_Female                = X'0004'               &
                                                                        &
                ! Report Control                                        &
        ,       AdjRSN$Print_EOB                = X'0010'               &
                                                                        &
        ,       AdjRSN$Payable                  = X'0020'               &
        ,       AdjRSN$SSO_Reduction            = X'0040'               &
                                                                        &
        ,       AdjRSN$Disallowed_UCR           = X'0080'               &
        ,       AdjRSN$Duplicate_Paid           = X'0100'               &
        ,       AdjRSN$Not_Eligible             = X'0200'               &
        ,       AdjRSN$Deductible               = X'0400'               &
        ,       AdjRSN$No_Benefit               = X'0800'               &
        ,       AdjRSN$Discount                 = X'1000'               &
        ,       AdjRSN$Mbr_Exempt               = X'2000'               &
        ,       AdjRSN$Medicare_A_Cutback       = X'4000'               &

        !===============================================================

        %LET %ADJRSN = -1                       ! For conditional includes


        !================================================================

        MAP (ADJRSN)     ADJRSN    ADJRSN
        MAP (ADJRSN1)    ADJRSN   ADJRSN1

        !===============================================================

   !---------------------------------------------------------------!
   !       MAIN PROCESS                                            !
   !---------------------------------------------------------------!
       
   WHEN ERROR USE Global_Handler
       GOSUB OPEN_FILES
       GOSUB PROCESS_FILE
       GOSUB END_OF_PROGRAM
   END WHEN

   !---------------------------------------------------------------!
   !        OPEN FILES						   !
   !---------------------------------------------------------------!

OPEN_FILES:  
    Adjrsn_ch = ICIF001_Assign_Channel
   
      HN_STAT = HN_Open("ADJ_REASON",  &
                C_Read,                          &
                C_Modify,                        &
                Adjrsn_ch,                      &
                ON_ERROR_NO_BLOW)

      IF HN_STAT <> SS$_NORMAL
    THEN
        PRINT " Error in opening ADJRSN  file"
        CALL LIB$STOP (Stat BY VALUE)
    END IF

   Seq_Ch = ICIF001_Assign_Channel
    HN_STAT = HN_CREATE_OPEN_SEQ ("ADJRSN.ISM", &
                                   "FIX"     ,           &
                                   162       ,           &
                                   0         ,           &
                                   0         ,           &
                                   "Write" ,             &
                                   "None" ,              &
                                   Seq_Ch,               &
                                   0                )

    IF HN_STAT <> SS$_NORMAL
      THEN
          PRINT  " Error in opening ADJRSN NEW"
          CALL LIB$STOP (Stat BY VALUE)
    END IF


  RETURN

 
  PROCESS_FILE:

  EoF_Flag = False
  WHILE (EoF_Flag = FALSE)

        HN_STAT = HN_SEQGET (Adjrsn_Ch ,   &
                             Adjrsn,       &
                             "RRL",        &
                             "0"L )

       IF HN_STAT <> SS$_NORMAL
        THEN
           EoF_Flag = TRUE
           ITERATE
        END IF

      ADJRSN1::Cd          = ADJRSN::Cd
      ADJRSN1::Rsn         = ADJRSN::Rsn
      ADJRSN1::Adj_FILL    = WS_SPACE
      ADJRSN1::Plan        = ADJRSN::Plan
      ADJRSN1::Exp_Date    = ADJRSN::Exp_Date
      ADJRSN1::Eff_Date    = ADJRSN::Eff_Date
      ADJRSN1::Flag        = ADJRSN::Flag
      ADJRSN1::Descrip      = ADJRSN::Descrip
      ADJRSN1::Category     = ADJRSN::Category
FOR WS_J=0 TO 10
      ADJRSN1::Svc_Flag(WS_J)     = ADJRSN::Svc_Flag(WS_J)
NEXT WS_J

     Stat = HN_PUT(seq_ch,ADJRSN1,"0"L)

     Next

     stat = HN_CLOSE(adjrsn_ch,"0"L)
     stat = HN_CLOSE(seq_ch,"0"L)

    Return

        !---------------------------------------------------------------!
        !       GLOBAL HANDLER                                          !
        !---------------------------------------------------------------!
        HANDLER Global_Handler
            PRINT ERT$(ERR) + " on " + ERN$
            EXIT HANDLER
        END HANDLER

SUB CLM_INV_OUX_01 (LONG Task_num BY REF, LONG Stat BY REF)
EXIT SUB
            
CALL CLM_INV_OUX_01(0,TEMP)

END_OF_PROGRAM:
END
                                      
