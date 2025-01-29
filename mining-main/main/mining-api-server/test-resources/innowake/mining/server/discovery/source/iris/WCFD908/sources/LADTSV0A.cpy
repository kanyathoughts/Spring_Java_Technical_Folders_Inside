#ifdef __MVS__
#pragma filetag("IBM-273")
#endif

#ifndef __LADTSV0A__
#define __LADTSV0A__

/* Datenstruktur für C-Zugriffe für Tabelle LA_ABKSTDAUSKADRVW */
typedef
struct {
  long int      L_BEARBABKNR;
  short int     L_BSABKKZ;
  char          filler__2[2];
  short int     L_STDAUSKTYPKZ;
  char          filler__3[2];
  long int      L_LFDNRSTDAUSK;
  short int     L_LFDNRADRVWSTDAUSK;
  char          filler__4[2];
  long int      L_ABGANGABKNR;
  short int     L_ADRSTDAUSKKZ;
  char          filler__5[2];
  short int     L_LFDNRADRSTDAUSK;
  char          filler__6[2];
  short int     L_ZWEMPFSTDAUSKKZ;
  char          filler__7[2];
  short int     L_LFDNRZWEMPFSTDAUSK;
  char          filler__8[2];
  short int     L_VERSWEGSTDAUSKKZ;
  char          filler__9[2];
  short int     L_MEDIUMSTDAUSKKZ;
  char          filler__10[2];
  short int     L_DUPLIKATANZSTDAUSK;
  char          filler__11[2];
  char          L_BEGLSCHRSTDAUSKJN;
  char          filler__12[3];
} DCLLA_ABKSTDAUSKADRVW;

#endif

