#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK09A__
#define __LAOAK09A__

/*---------------------------------------------------------------*
*
* PARAMETER FUER DAS BO KOMBIGRUPPENMITGLIEDER BO=9
*
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG
*----------------------------------------------------------------*
* 08.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTMN0A"
#include "LADTMS0A"

typedef struct {
/*---------------------------------------------------------------*
* KOMBIGRUPPENMITGLIED NICHT STRENG ZU HISTORISIEREND
*----------------------------------------------------------------*/
  DCLLA_ABKKMBGRMTGLNSH    KmbgrMtglNSH;
/*---------------------------------------------------------------*
* KOMBIGRUPPENMITGLIED STRENG ZU HISTORISIEREND
*----------------------------------------------------------------*/
  DCLLA_ABKKMBGRMTGLSH KmbgrMtglSH;

} TKombigruppeMtgl;

#endif
