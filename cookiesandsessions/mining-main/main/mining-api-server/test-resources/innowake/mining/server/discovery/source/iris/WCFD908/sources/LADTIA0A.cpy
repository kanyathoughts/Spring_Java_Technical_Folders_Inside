#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LADTIA0A__
#define __LADTIA0A__

/* Datenstruktur für C-Zugriffe für Tabelle LA_INKGKSTAND */
typedef
struct {
  short int     L_LFDNRINKGK;
  char          filler__2[2];
  long int      L_BEARBABKNR;
  short int     L_BSINKGKKZ;
  char          filler__3[2];
  long int      L_ABGANGABKNR;
  short int     L_PARTNROLLEKZ;
  char          filler__4[2];
  short int     L_LFDNRPARTNROLLE;
  char          filler__5[2];
  short int     L_ANFORDARTGKKZ;
  char          filler__6[2];
  short int     L_ABRECHWAEHKZ;
  char          filler__7[2];
  short int     L_TERMININKLAUFKZ;
  char          filler__8[2];
  char          L_DTAJN;
  char          filler__9[3];
  char          L_SOLLSTELLUNGNEGJN;
  char          filler__10[3];
  short int     L_MAHNVERSCHIEBKZ;
  char          filler__11[2];
  short int     L_MAHNVERSCHIEBZEITR;
  char          filler__12[2];
  short int     L_RECHVERSCHIEBKZ;
  char          filler__13[2];
  short int     L_RECHVERSCHIEBZEITR;
  char          filler__14[2];
  short int     L_DATENTRAEGERNR;
  char          filler__15[2];
  short int     L_LSVARTKZ;
  char          filler__16[2];
  short int     L_LSVKONTOHINWEISKZ;
  char          filler__17[2];
  short int     L_ZAHLWEGLSRETOUREKZ;
  char          filler__18[2];
  short int     L_INKSTELLEKZ;
  char          filler__19[2];
  short int     L_FINGIERTERBEGMONAT;
  char          filler__20[2];
  short int     L_ZAHLWEISE;
  char          filler__21[2];
} DCLLA_INKGKSTAND;

#endif

