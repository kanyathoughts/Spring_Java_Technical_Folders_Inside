PROGRAM BEN_CON_OUX_01

!     This source code, and all the routines referenced herein, are the
!     proprietary  properties  and  trade  secrets  of  HEALTHNET, INC.
!     Except as  provided for by license agreement,  this source code shall
!     not be duplicated, used  or  disclosed  without  written consent,
!     signed by an officer of HEALTHNET, INC.
!
! Start Documentation
!
!	SYSTEM:  Benefit System
! 	PROGRAM: BEN_CON_OUX_01.BAS
! 	TITLE:   On Demand Default Contract Report Generation
!	ALIAS:	 BEN_CON_OUX_01
! 	DATE:    21-MAR-2012
! 	AUTHOR:  Sathish Kumar Shanmugam(SX2)
!
! 	OVERVIEW:               
!
!             This program generates On Demand Default Contract Report.
!
! MODIFICATION HISTORY:
! ============ ======
! MOD #          DATE           NAME (VMS ID)               ISR #
!                            
!           21-MAR-2012  Sathish Kumar Shanmugam(SX2)    P12346 - AZ Migration  
!	    Initial creation
!			
! End Documentation

	!---------------------------------------------------------------!
	!	COMPILATION OPTIONS					!
	!---------------------------------------------------------------!
	OPTION	TYPE = EXPLICIT						&
	,	SIZE = ( INTEGER LONG, REAL DOUBLE )			&
	,	CONSTANT TYPE = INTEGER
	
	SET NO PROMPT
                                                                       

        COMMON ( COM09P )                                                      &
         WORD   CP.Actual%                      ! Actual data length           &
        ,WORD   CP.Condition                    ! Condition code               &
        ,WORD   CP.Wait.Time%                   ! Time-out on input            &
        ,STRING CP.Timeout.Rtn$         = 2%    ! Time-out return sequence     &
        ,STRING CP.Lname$               = 17%   ! Remember last name           &
        ,STRING CP.Fname$               = 10%   ! Remember first name          &
        ,WORD   CP.Gold.Wait.Time%              ! Time-out on GOLD key         &
        ,STRING CP.Gold.Timeout.Rtn$    = 2%    ! GOLD time-out return seq.    &
        ,STRING Term_Type$              = 5%    ! Terminal type                &
        ,WORD   CP.Gold%                        ! For testing only             &
        ,LONG   PB_Id                           ! Pasteboard Id                &
        ,LONG   DP_Id                           ! Display Id                   &
        ,LONG   Start_Scroll                    ! Start Row of scrolling reg.  &
        ,LONG   End_Scroll                      ! Last Row of scrolling reg.   &
        ,LONG   Prt_Dp_ID                       ! Display Id for print window  &
        ,LONG   Char_Set                        ! Type of characters           &
        ,LONG   Pop_Id                          ! Pop-Up Window ID             &
        ,LONG   KB_Id                           ! Keyboard ID                  &
        ,LONG   MSG_Dp_Id                       ! Message display ID           &
        ,LONG   HLP_Dp_Id                       ! Help display ID
   
        %INCLUDE "SMG$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET"
        %INCLUDE '$SMGDEF'  %FROM %LIBRARY 'SYS$LIBRARY:BASIC$STARLET'

            

 	!----------------------------------------------------------------------!
	!	    Local Declarations					       !
	!----------------------------------------------------------------------!
        !---------------------------------------------------------------!
        !       INCLUDE FILES                                           !
        !---------------------------------------------------------------!
        %INCLUDE "INC:ICISTANDARDS.INC"
        %INCLUDE 'INC:COMMON_IO.INC'
        %INCLUDE "INC:SYS_CHN_01.INC"
        %INCLUDE "INC:ICIDATE.REC"
        %INCLUDE "INC:ICITIME.REC"
        %INCLUDE "INC:ICISTAMP.REC"
        %INCLUDE "INC:KEYCODE.REC"
        %INCLUDE "INC:TAC11.INC"
        %INCLUDE "INC:BASIC-ERROR-CODES.INC"


 	!	EXTERNAL CONSTANT DECLARATION				!
	!---------------------------------------------------------------!
 	EXTERNAL LONG CONSTANT						&
		SS$_NORMAL

	!---------------------------------------------------------------!
	!	EXTERNAL FUNCTION DECLARATION				!
	!---------------------------------------------------------------!

	EXTERNAL BYTE	FUNCTION					&
		Convert_mbrcd_to_rec_type				&
	,	ICIF001_Assign_Channel					&
	,	ICIF002_Deassign_Channel (BYTE)

	EXTERNAL WORD FUNCTION						&
		ICSCRFLD( STRING )

	EXTERNAL LONG FUNCTION						&
		CVT_INTEGER_L( STRING )					&
	,	HN_SUBMIT( STRING, STRING, LONG )

	EXTERNAL STRING FUNCTION					&
		CCYYMMDD						&
	,	CONVERT_CCYYMMDD( ICIDATE )				&
	,	ICTRNLNM_XLATE( STRING )				&
	,	FMT_ICIDATE( ICIDATE, STRING, WORD )			&
	,	FMT_CCYYMMDD( STRING, STRING, WORD )			&
	,	EXIT_PGM_EF( WORD )					&
	, 	ICSCRFLD_GET					  	&
	,	CVT_STRING( LONG, STRING )				&
	,	FMT_SSN( STRING )

	EXTERNAL ICIDATE FUNCTION					&
		CONVERT_ICIDATE( STRING )

	!---------------------------------------------------------------!
	!	EXTERNAL SUBPROGRAM DECLARATION				!
	!---------------------------------------------------------------!
	EXTERNAL SUB							&
		ICSCRGET ( STRING, STRING, STRING, STRING )		&
	,	ICSCRFLD_GET_POS( WORD, WORD, WORD )			&
	,	TAC09( WORD,WORD,WORD,WORD,STRING,STRING,STRING )	&
	,	ICSCRFLD_Erase( WORD )					&
	,	ICSCRFLD_PUT_NEW( WORD, STRING )			&
	,	TAC18_NEW( STRING, STRING, WORD, WORD, WORD, WORD, WORD, WORD )&
	,	HN_FMT_ICIDATE ( ICIDATE, STRING, LONG, STRING )	&
	,	HN_CCYYJJJ ( STRING,STRING )				&
	,	HN_GET_SSN_REF_NUM_XREF					&
		    (STRING,STRING,LONG,STRING,STRING,STRING,LONG,STRING)

	!---------------------------------------------------------------!
	!	LOCAL CONSTANT DECLARATION				!
	!---------------------------------------------------------------!

	DECLARE	STRING CONSTANT						&
	  OK_To_Submit	= "Submit request (Y/N):"			&
	, Chg_Prompt	= "Is this information correct? (Y/N)?"         &
	, Undefined_Func_Msg = "Undefined Function key."		&
	, M1$	= "#,###,###.##"					&
	, M2$	= "#######.##"

 
	!---------------------------------------------------------------!
	!	MAPPING AREA						!
	!---------------------------------------------------------------!

	MAP (CRG_SCREEN)						&
	  WORD	CRG_Prod	 					&
	,	CRG_Plan						&
	,	CRG_RMC	          					&
	,	CRG_Care_Typ						&
	,	CRG_Strt_Loc						&
	,	CRG_End_Loc						&
        ,	CRG_Eff_Date						&
	,	CRG_Thru_Date						&
	,STRING	Sel_Prod	= 8%					&
	,	Sel_Plan	= 4%					&
	,	Sel_RMC 	= 2%					&
	,	Sel_Care_Typ	= 4%					&
	,	Sel_Strt_Loc	= 3%					&
	,	Sel_End_Loc	= 3%		        		&
	,	Sel_Eff_Date	= 8%		    ! CCYYMMDD		&
	,	Sel_Thru_Date	= 8%		    		

	!---------------------------------------------------------------!
	!	LOCAL VARIABLE DECLARATION				!
	!---------------------------------------------------------------!
	DECLARE LONG 							&
		Stat							&
	,	Xref_Stat						&
	,	Done							&
	,	Restart_Entry						&
        ,       Entry_Number

        DECLARE ICIDATE							&
		Current_ICIDATE						&
	,	Temp_Date

        DECLARE STRING                                                  &
                File_Name               ! Use when open a file          &
        ,       Input_Value                                             &
        ,       Param_List                                              &
        ,       Entered_Data                                            &
        ,       Ret_Command                                             &
        ,       Input_Confirm                                           &
        ,       Confirm_Prompt                                          &
        ,       Curr_SSN                                                &
        ,       Date_Today                                              & 
        ,       Up_Arrows                                               &
        ,       Down_Arrows                                             

        DECLARE STRING CONSTANT   WS_SPACES = ""                        &
	,			  WS_VAL_SPACE	= " "			&
	,			  WS_STR_YN	= "YN"                  &
	,			  WS_STR_ON	= " on "	        &	
        ,                         WS_STR_YES    = "Y"                   &
        ,                         WS_STR_NO     = "N"                   &
        ,                         WS_VAL_ZEROES = "00000000"            &
        ,                         WS_VAL_NINES  = "99999999"    
 
        DECLARE BYTE                                                    &
                Input_Loop                                              &
        ,       Back_Fld

	!---------------------------------------------------------------!
	!	MAIN LOGIC						!
	!---------------------------------------------------------------!

	GOSUB Main_Process

	!---------------------------------------------------------------!
	!	LOCAL SUBROUTINES					!
	!---------------------------------------------------------------!

 Main_Process:
        If Input_Loop= -1  
        Then
          GOSUB Exit_para
        End if
	GOSUB Screen_Setup
        GOSUB Restart_From_Top
 
Allow_Changes:
	UNTIL Done
	    SELECT Input_Loop
	    CASE 0%
		GOSUB Enter_Product_ID
	    CASE 1%
		GOSUB Enter_Plan_ID
	    CASE 2%
		GOSUB Enter_RMC
	    CASE 3%
		GOSUB Enter_Type_Of_Care
	    CASE 4%
		GOSUB Enter_Strt_Location
	    CASE 5%
		GOSUB Enter_End_Location
	    CASE 6%
		GOSUB Enter_Eff_Date
	    CASE 7%
		GOSUB Enter_Thru_Date
	    END SELECT
	NEXT
	RETURN IF Input_Loop = -1%
	IF  Restart_Entry
	THEN
	    CALL ICSCRFLD_Erase( -1% )
	    GOSUB Restart_From_Top
            GOSUB Allow_changes
	END IF

        If Input_Loop= -1
        Then
          GOSUB Exit_para
        End if

	Input_Loop = 0%
	WHEN ERROR IN
	    CALL LIB$ERASE_LINE( 23%, 1% )
	    CALL LIB$PUT_SCREEN( Chg_Prompt, 23%,6% )   
	    Input_Confirm = WS_VAL_SPACE
	    CALL TAC09( 0%, 1%, 23%, 45%, WS_STR_YN, WS_SPACES, Input_Confirm )
	    IF Input_Confirm = WS_STR_NO  
	    THEN
		Done = False
		GOSUB Allow_Changes
	    ELSE
		Done = TRUE
	    END IF
	USE
	    Input_Loop = -1%
	END WHEN
	RETURN IF Input_Loop = -1%

!	Submit job
	Param_List  = '"' + TRM$(Sel_Prod) + '","'			    &
		    + TRM$(Sel_Plan) + '","'				    &
		    + TRM$(Sel_RMC) + '","'				    &
		    + TRM$(Sel_Care_Typ) +  '","'     			    &
                    + TRM$(Sel_Strt_Loc) + '","'                            &
                    + TRM$(Sel_End_Loc) +  '","'                            &
                    + TRM$(Sel_Eff_Date) + '","'                            &
                    + TRM$(Sel_Thru_Date)                             

	Stat = HN_Submit( 'COM:BEN_CON_BRX_01.COM'                       &
		,'/LOG=LOG:BEN_CON_BRX_01.LOG/KEEP/NONOTIFY/PARAMETER=(' &
		+ Param_List + ')', Entry_Number )

	SELECT Entry_Number 
	CASE 0
	  CALL TAC02_NEW( 0%, "HN_SUBMIT Failure - Stat: "		    &
		+ NUM1$(Stat) )
	CASE ELSE
	  CALL TAC02_NEW( 0%, "Job BEN_CON_BRX_01.COM successfully submitted.")
 	END SELECT

 Exit_Para:
	EXIT PROGRAM

 Enter_Product_ID:
	Input_Value = ICSCRFLD_GET( CRG_Prod, SEL_Prod )
	SELECT CP.Condition
	CASE NORMAL
	    SEL_Prod = Input_Value
	    Back_Fld = Input_Loop
	    IF	TRM$( SEL_Prod ) <> WS_SPACES
	    THEN
                Input_Loop = Input_Loop + 1%
	    ELSE
		CALL ICSCRFLD_Erase( CRG_Prod )
		Input_Loop = Input_Loop + 1%
	    END IF
	CASE Back_Space, PF2, PF4
	    Done = TRUE
	    Input_Loop = -1%
	CASE ELSE
	    CALL TAC02_NEW( 0%, Undefined_Func_Msg )
	END SELECT
	RETURN

 Enter_Plan_ID:
	Input_Value = ICSCRFLD_GET( CRG_Plan, SEL_Plan )
	SELECT CP.Condition
	CASE NORMAL
	    SEL_Plan = Input_Value
	    IF	TRM$( SEL_Plan ) > WS_SPACES
	    THEN                                   
                Input_Loop = Input_Loop + 1%
	    ELSE
		CALL ICSCRFLD_Erase( CRG_Plan )
		Input_Loop = Input_Loop + 1%
	    END IF
	CASE Back_Space
	    Input_Loop = Input_Loop - 1%
	CASE PF2
	    Done = TRUE
	    Input_Loop = -1%
	CASE PF4
	    Done = TRUE
	    Restart_Entry = TRUE
	CASE ELSE
	    CALL TAC02_NEW( 0%, Undefined_Func_Msg )
	END SELECT
	RETURN

 Enter_RMC:
       Input_Value = ICSCRFLD_GET( CRG_RMC, SEL_RMC )
       SELECT CP.Condition
       CASE NORMAL
           SEL_RMC = Input_Value
           IF  TRM$( SEL_RMC ) > WS_SPACES
           THEN
               Input_Loop = Input_Loop + 1%
           ELSE
               IF SEL_Plan = WS_SPACES and SEL_Prod = WS_SPACES
               THEN            
                CALL ICSCRFLD_Erase( CRG_RMC )
                CALL TAC02_New( 0%,&
                 "Please enter valid value for Product or Plan or RMC")
                Input_Loop = 0%
               ELSE
                CALL ICSCRFLD_Erase( CRG_RMC )
                Input_Loop = Input_Loop + 1%
               END IF
           END IF                                                        
       CASE Back_Space
           Input_Loop = Input_Loop - 1%
       CASE PF2
           Done = TRUE
           Input_Loop = -1%
       CASE PF4
           Done = TRUE
           Restart_Entry = TRUE
       CASE ELSE
           CALL TAC02_NEW( 0%, Undefined_Func_Msg )
       END SELECT
       RETURN

 Enter_Type_Of_Care:
      Input_Value = ICSCRFLD_GET( CRG_Care_Typ, SEL_Care_Typ )
      SELECT CP.Condition
      CASE NORMAL
          SEL_Care_Typ = Input_Value
          IF  TRM$( SEL_Care_Typ ) > WS_SPACES
          THEN              
              Input_Loop = Input_Loop + 1%
          ELSE
              CALL ICSCRFLD_Erase( CRG_Care_Typ )
              Input_Loop = Input_Loop + 1%
          END IF
      CASE Back_Space
          Input_Loop = Input_Loop - 1%
      CASE PF2
          Done = TRUE
          Input_Loop = -1%
      CASE PF4
          Done = TRUE
          Restart_Entry = TRUE
      CASE ELSE
          CALL TAC02_NEW( 0%, Undefined_Func_Msg )
      END SELECT
      RETURN

 Enter_Strt_Location:
     Input_Value = ICSCRFLD_GET( CRG_Strt_Loc, SEL_Strt_Loc )
     SELECT CP.Condition
     CASE NORMAL
         SEL_Strt_Loc = Input_Value
         IF  TRM$( SEL_Strt_Loc ) > WS_SPACES
         THEN
             Input_Loop = Input_Loop + 1%
         ELSE
             CALL ICSCRFLD_Erase( CRG_Strt_Loc)
             Input_Loop = Input_Loop + 1%
         END IF
     CASE Back_Space
         Input_Loop = Input_Loop - 1%
     CASE PF2
         Done = TRUE
         Input_Loop = -1%
     CASE PF4
         Done = TRUE
         Restart_Entry = TRUE
     CASE ELSE
         CALL TAC02_NEW( 0%, Undefined_Func_Msg )
     END SELECT
     RETURN

 Enter_End_Location:
    Input_Value = ICSCRFLD_GET( CRG_End_Loc, SEL_End_Loc )
    SELECT CP.Condition
    CASE NORMAL
        SEL_End_Loc = Input_Value
        IF  TRM$( SEL_End_Loc ) > WS_SPACES
        THEN
            Input_Loop = Input_Loop + 1%
        ELSE
            CALL ICSCRFLD_Erase( CRG_End_Loc)
            Input_Loop = Input_Loop + 1%
        END IF
    CASE Back_Space
        Input_Loop = Input_Loop - 1%
    CASE PF2
        Done = TRUE
        Input_Loop = -1%
    CASE PF4
        Done = TRUE
        Restart_Entry = TRUE
    CASE ELSE
        CALL TAC02_NEW( 0%, Undefined_Func_Msg )
    END SELECT
    RETURN

 
 Enter_Eff_Date:
	Sel_Eff_Date = WS_SPACES IF Sel_Eff_Date = WS_VAL_ZEROES
	Input_Value = ICSCRFLD_GET( CRG_Eff_Date, SEL_Eff_Date )
	SELECT CP.Condition
	CASE NORMAL
	    Sel_Eff_Date = Input_Value
	    Sel_Eff_Date = WS_SPACES IF Input_Value = WS_VAL_ZEROES
	    IF TRM$(Sel_Eff_Date) > WS_SPACES
	    THEN
		Temp_Date = Convert_ICIDATE( Sel_Eff_Date )
		IF Temp_Date::Nbr = 0
		THEN
		    CALL TAC02_New( 0%,"<Start> date is not valid.Reenter date")
                ELSE
                    Input_Loop = Input_Loop + 1%
		END IF
	    ELSE
                CALL TAC02_New( 0%,"Please Enter valid effective date")
	    END IF
	CASE Back_Space
	    Input_Loop = Input_Loop - 1%
	    RETURN
	CASE PF2
	    Done = TRUE
	    Input_Loop = -1%
	    RETURN
	CASE PF4
	    Done = TRUE
	    Restart_Entry = TRUE
	    RETURN
	CASE ELSE
	    CALL TAC02_NEW( 0%, Undefined_Func_Msg )
	END SELECT
        RETURN

 Enter_Thru_Date:
	Sel_Thru_Date = WS_SPACES  	IF Sel_Thru_Date = WS_VAL_ZEROES
	Input_Value = ICSCRFLD_GET( CRG_Thru_Date, SEL_Thru_Date )
	SELECT CP.Condition
	CASE NORMAL
	    Sel_Thru_Date = Input_Value
	    Sel_Thru_Date = WS_SPACES 	IF Input_Value = WS_VAL_ZEROES        &
				OR Input_Value = WS_VAL_NINES
	    IF TRM$(Sel_Thru_Date) > WS_SPACES
	    THEN
		Temp_Date = Convert_ICIDATE( Sel_Thru_Date )
                Done = TRUE
		IF Temp_Date::Nbr = 0
		THEN
		    CALL TAC02_New( 0%, "<End> date is not valid. Reenter date")
                    DONE = False
		END IF
		IF Sel_Thru_Date < Sel_Eff_Date
		THEN
		    CALL TAC02_New( 0%,"<End> date is before <Start> date." )
                    DONE = False
		END IF                                                       
	    ELSE                                                      
                CALL TAC02_New( 0%,"Please Enter valid Thru date" )
	    END IF                           
	CASE Back_Space
             Input_Loop = Input_Loop - 1%
	CASE PF2
	    Done = TRUE
	    Input_Loop = -1%
	CASE PF4
	    Done = TRUE
	    Restart_Entry = TRUE
	CASE ELSE
	    CALL TAC02_NEW( 0%, Undefined_Func_Msg )
	END SELECT
	RETURN

 
 Screen_Setup:
	CALL ICSCRGET ( "BEN", "ICON","BENRPT", "MAIN" )
         CRG_PROD      = ICSCRFLD("SEL.PROD")
         CRG_PLAN      = ICSCRFLD("SEL.PLAN")
         CRG_RMC       = ICSCRFLD("SEL.RMC")
         CRG_CARE_TYP  = ICSCRFLD("SEL.CTYPE")
         CRG_STRT_LOC  = ICSCRFLD("SEL.SLOC")
         CRG_END_LOC   = ICSCRFLD("SEL.ELOC")
         CRG_EFF_DATE  = ICSCRFLD("SEL.EDATE")
         CRG_THRU_DATE = ICSCRFLD("SEL.TDATE")
      	RETURN

 Restart_From_Top:

        Restart_Entry = False
        Done            = 0%
        Input_Loop      = 0%
        Back_Fld        = -1%
        Sel_Prod        = WS_VAL_SPACE
        Sel_Plan        = WS_VAL_SPACE
        Sel_RMC         = WS_VAL_SPACE
        Sel_Care_Typ    = WS_VAL_SPACE
        Sel_Strt_Loc    = WS_VAL_SPACE
        Sel_End_Loc     = WS_VAL_SPACE
        Sel_Eff_Date    = WS_VAL_SPACE
        Sel_Thru_Date   = WS_VAL_SPACE
      RETURN


	!---------------------------------------------------------------!
	!	GLOBAL HANDLER						!
	!---------------------------------------------------------------!
	HANDLER GLOBAL_HANDLER
	    CALL TAC02_NEW( 0%, ERT$(ERR) + WS_STR_ON + ERN$ )
	    EXIT HANDLER
	END HANDLER

	END PROGRAM
