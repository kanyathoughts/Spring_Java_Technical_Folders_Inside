#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LADTBB0A__
#define __LADTBB0A__

/* Datenstruktur für C-Zugriffe für Tabelle LA_ABKBAV */
typedef
struct {
  long int      L_BEARBABKNR;
  short int     L_BSABKKZ;
  char          filler__2[2];
  short int     L_BAUTYPKZ;
  char          filler__3[2];
  long int      L_LFDNRBAUTYP;
  short int     L_BAUPRICKZ;
  char          filler__4[2];
  long int      L_LFDNRBAUPRIC;
  long int      L_ABGANGABKNR;
  short int     L_BAVARTKZ;
  char          filler__5[2];
  short int     L_BAVPERSTYPKZ;
  char          filler__6[2];
} DCLLA_ABKBAV;

#endif

