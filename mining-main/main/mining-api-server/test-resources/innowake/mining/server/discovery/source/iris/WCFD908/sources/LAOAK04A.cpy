#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK04A__
#define __LAOAK04A__
/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO DRUCKOPTIONEN POLICE BO=4                *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 08.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTBN0A"
#include "LADTBD0A"

typedef struct {
/*---------------------------------------------------------------*
* BAUSTEIN NICHT STRENG ZU HISTORISIEREND                        *
*----------------------------------------------------------------*/
  DCLLA_ABKBAUNSH BauNSH;
/*---------------------------------------------------------------*
* BAUSTEIN DRUCKOPTIONEN POLICE                                  *
*----------------------------------------------------------------*/
  DCLLA_ABKBAUDRUCKPOL DruckOpt;
} TDruckOptPolice;

#endif
