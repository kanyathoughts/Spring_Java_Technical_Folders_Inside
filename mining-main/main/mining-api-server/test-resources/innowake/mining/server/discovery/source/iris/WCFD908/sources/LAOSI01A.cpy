/*****************************************************************
*                                                                *
*        C - HEADER FUER DAS SSI-A                               *
*                                                                *
******************************************************************
* GEÄNDERT AM  ! VON            ! ÄNDERUNG                       *
*==============+================+================================*
*    05/2001   ! STENZEL        ! ERSTERSTELLUNG                 *
*--------------+----------------+--------------------------------*
*              !                !                                *
*              !                !                                *
*****************************************************************/


#ifndef __LAOSI01A__
#define __LAOSI01A__
#pragma pack(1)

/* UNTERSTÜTZTE SERVICES */

#define TRUE 'T'
#define FALSE 'F'

#define BAUTYP_PRICING       1
#define BAUTYP_INKASSO       2
#define BAUTYP_PERSKREIS     3
#define BAUTYP_DRUCKOPT      4
#define BAUTYP_ABRECHSTELLE  5

#define DRUCKOPT_POLICE       1
#define DRUCKOPT_ABSCHRIFTEN  2
#define DRUCKOPT_URKUNDE      3

#define SERVICE_GIBAbkOMMENSGrunddaten        2101
#define SERVICE_GIBAUSSCHLUSKLAUSELBUZEUZ     2201
#define SERVICE_GIBBavDATEN                   2202
#define SERVICE_GIBPRICINGDATEN               2203
#define SERVICE_GIBINKASSODATEN               2204
#define SERVICE_GIBDRUCKOPTIONEN              2205
#define SERVICE_GIBBEZUGSRECHTDATEN           2206
#define SERVICE_GIBKOMBIGRUPPE                2301

#define MSG_UNKNOWN_METHODE       1
#define MSG_FETCH_BO_LESEN        2
#define MSG_NO_ABKNR              3
#define MSG_NO_KMBGRNR            4
#define MSG_NO_BSABKKZ            5

/* Steuerungsstruktur für das SSI-A */
typedef struct {
    char           szProg[9];                /* 00000 - 00008 */
    char           filler_001[3];                  /* 00009 - 00011 */
    short int      iMethode;                 /* 00012 - 00013 */
    char           filler_002[2];                  /* 00014 - 00015 */
    char           szSicht[9];               /* 00016 - 00024 */
    char           filler_003[3];                  /* 00025 - 00027 */
    char           cMehrDaten;               /* 00028 - 00028 */
    char           filler_004[3];                  /* 00029 - 00031 */
    char           cKeineDaten;              /* 00032 - 00032 */
    char           filler_005[3];                  /* 00033 - 00035 */
/* DZP-Bereich */
    long int       lSatz_Anzahl;             /* 00036 - 00039 */
    long int       lSatz_Aktuell;            /* 00040 - 00043 */
    long int       lSatz_Von_Num;            /* 00044 - 00047 */
    long int       lSatz_Bis_Num;            /* 00048 - 00051 */
    char           szSatz_von_Char[16];      /* 00052 - 00067 */
    char           szSatz_bis_Char[16];      /* 00068 - 00083 */
/* Msgs-Bereich */
    short int      sMsg_Anz;                 /* 00084 - 00085 */
    char           filler_006[2];                  /* 00086 - 00087 */
    struct {
        long int       lMsg_Nr;          /* 00088 - 00091 */
        char           szMeld_System[9];     /* 00092 - 00100 */
        char           filler_007[3];              /* 00101 - 00103 */
        char           szVar1[31];           /* 00104 - 00134 */
        char           filler_008;                 /* 00135 - 00135 */
        char           szVar2[31];           /* 00136 - 00166 */
        char           filler_009;                 /* 00167 - 00167 */
        char           szVar3[31];           /* 00168 - 00198 */
        char           filler_010;                 /* 00199 - 00199 */
        char           cMeld_Typ;            /* 00200 - 00200 */
        char           filler_011[3];              /* 00201 - 00203 */
        char           cMeld_Kat;            /* 00204 - 00204 */
        char           filler_012[3];              /* 00205 - 00207 */
    } Msg[10];                               /* 00088 - 01227 */
/* Zugriffskeys */
    char           Key[60];                  /* 01228 - 01287 */
/* Datenbereich */
    char           Data[31500];              /* 01288 - 32787 */
} TLaosi01a;


/* Schlüsselstrukturen für alle unterstützten Services */
/* Schlüsselstruktur für Service GibAbkommensgrunddaten */
typedef struct {
    short int      iBsAbkKz;
    char           filler_002[2];
    char           szWirkDat[11];
    char           filler_003;
} TAbkKey;

/* Schlüsselstruktur für Service GibAusschlussklausel */
typedef struct {f
    short int      iBsAbkKz;
    char           filler_002[2];
    long int       lKmbgrNr;
    char           szWirkDat[11];
    char           filler_003;
} TAusschlBuzEuzKey;

/* Schlüsselstruktur für Service Preisdaten */
typedef struct {
    short int      iBsAbkKz;
    char           filler_002[2];
    long int       lKmbgrNr;
    char           szWirkDat[11];
    char           filler_003;
    short int      iAnschlVertrKz;
    char           filler_004[3];
} TPricingKey;

/* Schlüsselstruktur für Service GibInkassoDaten */
typedef struct {
    short int      iBsAbkKz;
    char           filler_002[2];
    long int       lKmbgrNr;
} TInkassoKey;

/* Ausgabestruktur für Service gibAbkommensdaten */
typedef struct {
    short int      iRechtKz;
    char           filler_001[2];
    char           szAbkKurzBez[33];
    char           filler_002[3];
    char           cKonsortvertrJN;
    char           filler_003[3];
    char           szStdKorrSpracheKz[5];
    char           filler_004[3];
} TAbkData;


/* Ausgabestruktur für Service gibAusschlussklauselBUZEUZ */
typedef struct {
    short int      iKlauselKz;
    char           filler_001[2];
} TAusschlBuzEuzData;


/* Ausgabestruktur für Service gibBavDaten */
typedef struct {
    short int      iBavArtkz;
    char           filler_001[2];
    short int      iBavPerstypkz;
    char           filler_002[2];
    short int      iPauschalsteuerKz;
    char           filler_003[2];
    char           cAutomUebgangVnJN;
    char           filler_004[3];
    char           cFlexAltersGrenzeJN;
    char           filler_005[3];
} TBavData;


/* Ausgabestruktur für Service gibBezugsrechtDaten */
typedef struct {
    short int      iBezugRechtFormKz;
    char           filler_001[2];
    short int      iStdBezugRechtTodKz;
    char           filler_002[2];
    short int      iBezugRechtVorbehKz;
    char           filler_003[2];
    short int      iLeistEinschBezugKz;
    char           filler_004[2];
    short int      iAuszVerfuegTodKz;
    char           filler_005[2];
} TBezugsRechtData;

/* Ausgabestruktur für gibPricingDaten */
typedef struct {
    short int      iVertragArtKz;
    char           filler_001[2];
    short int      iKostenSystemKz;
    char           filler_002[2];
    short int      iStueckKostenKz;
    char           filler_003[2];
    short int      iWaehVertrKz;
    char           filler_004[2];
} TPricingData;

#endif

