        01  MAP-INPUT-AREA.
           05  FILLER                          PIC X(12).
           05  MAP-DAT-STR-IN.
               10  MAP-DAT-LEN                 PIC S9(04) COMP.
               10  MAP-DAT-FLD                 PIC X.
               10  FILLER
                   REDEFINES MAP-DAT-FLD.
                   15 MAP-DAT-ATR              PIC X.
               10  MAP-DAT-I                   PIC X(17).
           05  MAP-USR-ID-STR-IN.
               10  MAP-USR-ID-LEN              PIC S9(04) COMP.
               10  MAP-USR-ID-FLD              PIC X.
               10  FILLER
                   REDEFINES MAP-USR-ID-FLD.
                   15 MAP-USR-ID-ATR           PIC X.
               10  MAP-USR-ID-I                PIC X(12).
       01  MAP-OUTPUT-AREA
           REDEFINES MAP-INPUT-AREA.
           10 FILLER                           PIC X(12).
           05  MAP-DAT-STR-OUT.
               10  FILLER                      PIC X(03).
               10  MAP-DAT-O                   PIC X(17).
           05  MAP-USR-ID-STR-OUT.
               10  FILLER                      PIC X(03).
               10  MAP-USR-ID-O                PIC X(12).
