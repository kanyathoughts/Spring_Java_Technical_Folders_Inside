#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK08A__
#define __LAOAK08A__

/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO KOMBIGRUPPE BO=8                         *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 08.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTKN0A"
#include "LADTKS0A"

typedef struct {

/*---------------------------------------------------------------*
* KOMBIGRUPPE NICHT STRENG ZU HISTORISIEREND                     *
*----------------------------------------------------------------*/
    DCLLA_ABKKMBGRNSH KmbgrNSH;
/*---------------------------------------------------------------*
* KOMBIGRUPPE STRENG ZU HISTORISIEREND                           *
*----------------------------------------------------------------*/
    DCLLA_ABKKMBGRSH KmbgrSH;

} TKombigruppe;

#endif

