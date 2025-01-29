!*****************************************************************************!
! THIS SOURCE CODE, AND ALL THE ROUTINES REFERENCED HEREIN, ARE THE           !
! PROPRIETARY PROPERTIES AND TRADE SECRETS OF XXXXXXXXXX,INC. EXCEPT AS       !
! PROVIDED FOR BY LICENSE AGREEMENT, THIS SOURCE CODE SHALL NOT BE            !
! DUPLICATED, USED OR DISCLOSED WITHOUT WRITTEN CONSENT, SIGNED               !
! BY AN OFFICER OF XXXXXXXXXX,INC.                                            !
!*****************************************************************************!
!                       START DOCUMENTATION                                   !
!*****************************************************************************!
!                                                                             !
!       SYSTEM:  XXXXXXXXXXXXX                                                !
!       PROGRAM: XXXXXXXXXXXXXXXXXXXXXX                                       !
!       TITLE:   XXXXXXXXXXXXXXXXXXXXXX                                       !
!       DATE:    XXXXXXXXXXX                                                  !
!       AUTHOR:  XXXXXXXXXXXXXXXXXXXXXXXX                                     !
!                                                                             !
! OVERVIEW:                                                                   !
!      Inserting corresponding parent record for the child record in          !
!      XXXXXXXXX file                                                         !
!                                                                             !
!*******************************************************************************
! MOD #      DD-MMM-CCYY   USER NAME(VMS ID)               ISR#
! INITIAL    XXXXXXXXXXX   XXXXXXXXXXXXXXXXXXXXXXXX
! CREATION                                         
!
!*****************************************************************************!
!                      End Documentation
!*****************************************************************************!
!*****************************************************************************!
!                      FILES USED
!*****************************************************************************!
!
! RFILE: DRIVER_IN_FILE
! RDESC: Driver INPUT FILE
!
! MFILE: ADJ_ACCUM
! MDESC: ADJ_ACCUM FILE
!
! MFILE: POS_ADJ_ACCUM
! MDESC: POS_ADJ_ACCUM FILE
!
!*****************************************************************************!
!                       Include Files                                         !
!*****************************************************************************!
        %INCLUDE "INC:XXXXXXXXXXXX.INC"
        %INCLUDE "INC:XXXXXXXXXX.INC"
        %INCLUDE "INC:COMMON_IO.INC"
        %INCLUDE "INC:XXXXXXX.REC"
        %INCLUDE "INC:XXXXXXX.REC"
        %INCLUDE 'INC:XXXXXXXX.REC'
        %INCLUDE "INC:XXXXXXX.REC"
        %INCLUDE "INC:XXXXXXX.MAP"
