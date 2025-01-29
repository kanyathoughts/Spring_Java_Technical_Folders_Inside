#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LADTBW0A__
#define __LADTBW0A__

/* Datenstruktur für C-Zugriffe für Tabelle LA_ABKBEARBNACHW */
typedef
struct {
  long int      L_BEARBABKNR;
  short int     L_BSABKKZ;
  char          filler__2[2];
  char          L_PERSONALNUMMER[7];
  char          filler__3[1];
  short int     L_BEARBARTABKKZ;
  char          filler__4[2];
  short int     L_BEARBTYPABKKZ;
  char          filler__5[2];
  short int     L_BEARBGRUNDKZ;
  char          filler__6[2];
  char          L_BEARBDAT[11];
  char          filler__7[1];
  char          L_WIRKBEGDAT[11];
  char          filler__8[1];
  char          L_WIRKENDDAT[11];
  char          filler__9[1];
  long int      L_ANNULLNR;
  char          L_ANNULLDAT[11];
  char          filler__10[1];

  short int     L_GEVOTYPKZ;
  char          filler__12[2];
  short int     L_GEVOSUBTYPKZ;
  char          filler__13[2];
} DCLLA_ABKBEARBNACHW;

#endif

