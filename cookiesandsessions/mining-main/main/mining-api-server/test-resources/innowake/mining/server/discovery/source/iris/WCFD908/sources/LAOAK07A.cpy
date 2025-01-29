#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK07A__
#define __LAOAK07A__

/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO INKASSO BO=7                             *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 08.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTBN0A"
#include "LADTBI0A"
#include "LADTPD0A"
#include "LADTIE0A"
#include "LADTIZ0A"
#include "LADTIG0A"
#include "LADTID1A"
#include "LADTIP0A"

#define  MAX_INKEZZAHLW  15
#define  MAX_INKGKPERSNR  12

typedef struct {
/*---------------------------------------------------------------*
* BAUSTEIN NICHT STRENG ZU HISTORISIEREND                        *
*----------------------------------------------------------------*/
  DCLLA_ABKBAUNSH BauNSH;
/*---------------------------------------------------------------*
* BAUSTEIN INKASSO                                               *
*----------------------------------------------------------------*/
  DCLLA_ABKBAUINK Inkasso;
/*---------------------------------------------------------------*
* PRÄMIENDEPOT                                                   *
*----------------------------------------------------------------*/
  DCLLA_ABKPRAEMDEPOT PraemDepot;
/*---------------------------------------------------------------*
* INKASSO EINZELZAHLER                                           *
*----------------------------------------------------------------*/
  DCLLA_ABKINKEZ InkEZ;
/*---------------------------------------------------------------*
* INKASSO EINZELZAHLER ZAHLWEISE                                 *
*----------------------------------------------------------------*/
  DCLLA_ABKINKEZZAHLW InkEZZahlW[MAX_INKEZZAHLW];
/*---------------------------------------------------------------*
* INKASSO GROßKUNDE                                              *
*----------------------------------------------------------------*/
  DCLLA_ABKINKGK InkGK;
 /*---------------------------------------------------------------*
 * INKASSODRUCKOPTIONEN TRANSFORMIERT                             *
 *----------------------------------------------------------------*/
  TDruckOpt_ID EZDruOpt;
/*---------------------------------------------------------------*
* INKASSO GROßKUNDE PERSONALNUMMER                               *
*----------------------------------------------------------------*/
  DCLLA_ABKINKGKPERSNR InkGKPersNr[MAX_INKGKPERSNR];

} TInkasso;

#endif
