#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LADTBK0A__
#define __LADTBK0A__

/* Datenstruktur für C-Zugriffe für Tabelle LA_ABKBAVKRITERIUM */
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
  short int     L_BAVKRITKZ;
  char          filler__5[2];
  long int      L_ABGANGABKNR;
  short int     L_BAVKRITTYPKZ;
  char          filler__6[2];
  short int     L_BAVKRITWERTKZ;
  char          filler__7[2];
  char          L_BAVKRITWERTJN;
  char          filler__8[3];
  struct {
    short          L_BAVKRITFREITEXT_len;
    char           L_BAVKRITFREITEXT_txt[2001];
  } L_BAVKRITFREITEXT;
  char          filler__9[1];
} DCLLA_ABKBAVKRITERIUM;

#endif

