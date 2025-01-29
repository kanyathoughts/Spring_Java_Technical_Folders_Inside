#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LADTK20A__
#define __LADTK20A__

/* Datenstruktur für C-Zugriffe für Tabelle LA_ABKKONSFUEHRNSH */
typedef
struct {
  long int      L_BEARBABKNR;
  short int     L_BSABKKZ;
  char          filler__2[2];
  long int      L_ABGANGABKNR;
  long int      L_BILANZBINDENUMMER;
  char          L_BILANZKURZTXT[33];
  char          filler__3[3];
  char          L_BILANZWERTERMITTJN;
  char          filler__4[3];
  char          L_BILANZWMITTEILJN;
  char          filler__5[3];
} DCLLA_ABKKONSFUEHRNSH;

#endif

