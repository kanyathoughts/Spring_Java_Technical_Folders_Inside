#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK17A__
#define __LAOAK17A__

/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO DRUCKOPTIONEN BRIEF BO=17                *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 21.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTDB0A"
#include "LADTDK0A"
#include "LADTDV0A"
#include "LADTDS0A"

#define MAX_KOPIE   6
#define MAX_SCHRIFTST   35

typedef struct {
/*---------------------------------------------------------------*
* BRIEFDATEN                                                     *
*----------------------------------------------------------------*/
  DCLLA_ABKDRPOLBRIEF Brief;
/*---------------------------------------------------------------*
* KOPIEN                                                         *
*----------------------------------------------------------------*/
  struct {
     DCLLA_ABKDRPOLKOPIE Kopie;
/*---------------------------------------------------------------*
* VERSANDWEG                                                     *
*----------------------------------------------------------------*/
     DCLLA_ABKDRPOLVERSWEG VersWeg;
  } Kopien[MAX_KOPIE];
/*---------------------------------------------------------------*
* SCHRIFTSTÜCK                                                   *
*----------------------------------------------------------------*/
  DCLLA_ABKDRPOLSCHRSTK Schriftst[MAX_SCHRIFTST];
} TDruckOptBrief;

#endif
