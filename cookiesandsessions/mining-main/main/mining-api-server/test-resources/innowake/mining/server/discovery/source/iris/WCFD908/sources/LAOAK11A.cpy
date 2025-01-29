#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LAOAK11A__
#define __LAOAK11A__

/*---------------------------------------------------------------*
*                                                                *
* PARAMETER FUER DAS BO PERSONENKREIS BO=11                      *
*                                                                *
*----------------------------------------------------------------*
* GEAENDERT AM ! VON            ! AENDERUNG                      *
*----------------------------------------------------------------*
* 08.05.2001   ! G. STENZEL     ! ERSTERSTELLUNG                 *
*----------------------------------------------------------------*/

#include "LADTBS0A"
#include "LADTBP0A"
#include "LADTPN0A"
#include "LADTPK0A"

#define MAX_PERSKREIS_KLAUSEL 5

typedef struct {
/*---------------------------------------------------------------*
* BAUSTEIN STRENG ZU HISTORISIEREND                              *
*----------------------------------------------------------------*/
    DCLLA_ABKBAUSH BauSH;
/*---------------------------------------------------------------*
* BAUSTEIN PERSONENKREIS                                         *
*----------------------------------------------------------------*/
    DCLLA_ABKBAUPERSKREIS PersKreis;
/*---------------------------------------------------------------*
* PERSONENKREIS NOTIZEN                                          *
*----------------------------------------------------------------*/
    DCLLA_ABKPERSKRNOTIZ PersNotiz;
/*---------------------------------------------------------------*
* PERSONENKREIS KLAUSELN                                         *
*----------------------------------------------------------------*/
    DCLLA_ABKPERSKREISKL PersKl[MAX_PERSKREIS_KLAUSEL];

} TPersKreis;

#endif

