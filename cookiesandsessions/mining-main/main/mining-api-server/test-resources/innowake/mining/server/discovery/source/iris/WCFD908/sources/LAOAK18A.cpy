#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK18A__
#define __LAOAK18A__

/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO ABKOMMEN BO=18                           *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 17.05.2001   ! F. P�TSCHKE    ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTAO0A"

typedef struct {
/*---------------------------------------------------------------*
* ABKOMMEN                                                       *
*----------------------------------------------------------------*/
  DCLLA_ABKOMMEN Abk;

} TAbkommen;

#endif

