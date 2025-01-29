#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK12A__
#define __LAOAK12A__
/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO PRICING BO=12                            *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 10.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTBS0A"
#include "LADTBR0A"
#include "LADTRP1A"

typedef struct {
/*---------------------------------------------------------------*
* BAUSTEIN STRENG ZU HISTORISIEREND                              *
*----------------------------------------------------------------*/
    DCLLA_ABKBAUSH BauSH;
/*---------------------------------------------------------------*
* BAUSTEIN PRICING                                               *
*----------------------------------------------------------------*/
    DCLLA_ABKBAUPRICING Pricing;
/*---------------------------------------------------------------*
* PRICING PARAMETER                                              *
*----------------------------------------------------------------*/
    TPriceKriterien PriceParam;
} TPricing;

#endif
