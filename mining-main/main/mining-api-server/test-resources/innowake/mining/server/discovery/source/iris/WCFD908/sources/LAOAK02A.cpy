#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK02A__
#define __LAOAK02A__

/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO ABRECHNUNGSSTELLE BO=2                   *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 10.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTBN0A"
#include "LADTBA0A"
#include "LADTRE0A"

typedef struct {
  DCLLA_ABKBAUNSH      BauNSH;
  DCLLA_ABKBAUABRECHST AbrechStelle;
  DCLLA_INKGKRECH      GKRech;
} TAbrStelle;

#endif
