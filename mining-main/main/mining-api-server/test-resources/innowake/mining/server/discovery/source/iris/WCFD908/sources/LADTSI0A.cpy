#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LADTSI0A__
#define __LADTSI0A__

/* Datenstruktur für C-Zugriffe für Tabelle LA_SCHNITTST_INK */
typedef
struct {
  short int     L_BESTANDSSCHLUESSEL;
  char          filler__2[2];
  short int     L_DATENTRAEGERNR;
  char          filler__3[2];

  short int     L_LFDNRINKGK;
  char          filler__5[2];
  short int     L_BSINKGKKZ;
  char          filler__6[2];
  short int     L_RECHNR;
  char          filler__7[2];
  
  short int     L_BSABKKZ;
  char          filler__9[2];
  char          L_RECHBEZ[33];
  char          filler__10[3];
} DCLLA_SCHNITTST_INK;

#endif

