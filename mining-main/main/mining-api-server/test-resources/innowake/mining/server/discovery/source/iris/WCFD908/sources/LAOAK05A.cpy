#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK05A__
#define __LAOAK05A__
/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO GROßKUNDENINKASSO BO=5                   *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 08.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTIA0A"
#include "LADTIB0A"
#include "LADTIS0A"
#include "LADTIO0A"
#include "LADTIT1A"
#include "LADTIN0A"

#define  MAX_GKINKBRIEF  10
#define  MAX_GKINKSCHRIFTST  10

typedef struct {
/*---------------------------------------------------------------*
* GKINKASSO STAND                                                *
*----------------------------------------------------------------*/
    DCLLA_INKGKSTAND GKInkStand;
/*---------------------------------------------------------------*
* GKINKASSO BRIEF                                                *
*----------------------------------------------------------------*/
struct {
    DCLLA_INKGKBRIEF Brief;
/*---------------------------------------------------------------*
* GKINKASSO SCHRIFTSTÜCK                                         *
*----------------------------------------------------------------*/
    DCLLA_INKGKSCHRSTK GKInkSchriftst[MAX_GKINKSCHRIFTST];
} GKInkBrief[MAX_GKINKBRIEF];
/*---------------------------------------------------------------*
* GKINKASSO DRUCKOPTIONEN                                        *
*----------------------------------------------------------------*/
    DCLLA_INKGKDRUCKOPT GKInkDruOpt;
/*---------------------------------------------------------------*
* GKINKASSO DRUCKOPTIONEN CTYPEN                                  *
*----------------------------------------------------------------*/
    TDruckOpt_IT GKInkDruOptTyp;
/*---------------------------------------------------------------*
* GKINKASSO                                                      *
*----------------------------------------------------------------*/
    DCLLA_INKGK GKInk;

} TGKInkasso;

#endif
