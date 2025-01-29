#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK06A__
#define __LAOAK06A__

/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO GRUNDDATEN BO=6                          *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 08.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTAS0A"
#include "LADTAN0A"

typedef struct {
/*---------------------------------------------------------------*
* GRUNDDATEN STREND ZU HISTORISIEREND                            *
*----------------------------------------------------------------*/
   DCLLA_ABKOMMENSH AbkSH;
/*---------------------------------------------------------------*
* GRUNDDATEN NICHT STRENG ZU HISTORISIEREND                      *
*----------------------------------------------------------------*/
   DCLLA_ABKOMMENNSH AbkNSH;

} TGrunddaten;

#endif
