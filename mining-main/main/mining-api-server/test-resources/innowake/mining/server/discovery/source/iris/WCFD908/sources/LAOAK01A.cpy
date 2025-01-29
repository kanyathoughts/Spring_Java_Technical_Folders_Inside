#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK01A__
#define __LAOAK01A__

/*---------------------------------------------------------------*
*
* PARAMETER FUER DAS BO BEARBEITUNGSNACHWEIS BO=1
*
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG
*----------------------------------------------------------------*
* 10.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*
* B E A R B E I T U N G S N A C H W E I S
*/

#include "LADTBW0A"

typedef struct {
  DCLLA_ABKBEARBNACHW BearbNw;
} TBearbNw;

#endif
